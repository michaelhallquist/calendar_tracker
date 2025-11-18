library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(gt)
library(ggplot2)
library(plotly)
library(purrr)
library(forcats)
library(readr)

# Always pull latest entries via toggl_api.R
source("toggl_api.R")
source("gcal/gcal_rest.R")

local_tz <- "America/New_York"

# Helpers ---------------------------------------------------------------
prepare_toggl <- function(entries_df, local_tz = "America/New_York", week_start = 7) {
  if (is.null(entries_df) || !nrow(entries_df)) return(tibble())
  entries_df %>%
    mutate(
      start_local = with_tz(start_utc, local_tz),
      end_local   = with_tz(end_utc,   local_tz),
      duration_hr = duration_sec / 3600,
      # Prefer task; fall back to description; trim/squish
      task_title  = coalesce(task, description) |> stringr::str_squish(),
      # Optional: normalize repetitive ticket suffixes/IDs, e.g., "Task ABC-123"
      task_title  = stringr::str_replace(task_title, "\\b[A-Z]{2,}-\\d+\\b$", ""),
      # week bucket (Sunday start by default)
      week_start  = floor_date(start_local, "week", week_start = week_start),
      day         = as_date(start_local)
    )
}

categorize_tasks <- function(df, rules = NULL, default = "Uncategorized") {
  if (is.null(df) || !nrow(df)) return(tibble())
  if (is.null(rules) || nrow(rules) == 0) return(df %>% mutate(category = default))
  df %>%
    rowwise() %>%
    mutate(category = {
      txt <- paste(project %||% "", task_title %||% "", sep = " | ")
      hit <- rules$category[stringr::str_detect(txt, rules$pattern)][1]
      ifelse(is.na(hit), default, hit)
    }) %>%
    ungroup()
}

summarize_by_project_task <- function(df) {
  if (is.null(df) || !nrow(df)) return(tibble())
  df %>%
    group_by(project, task_title) %>%
    summarise(
      hours_total = sum(duration_hr, na.rm = TRUE),
      entries     = n(),
      first_start = min(start_local, na.rm = TRUE),
      last_end    = max(end_local,   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(hours_total))
}

read_all_toggl_entries <- function() {
  conn <- init_db_connection(db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  ensure_entries_table(conn)

  start_iso <- "1970-01-01T00:00:00Z"
  end_iso   <- iso_utc(now(tzone = local_tz) + days(1))

  read_entries_range(conn, start_iso, end_iso)
}

# Format datetimes like 13Nov2025 9:00 AM for the GT table
format_compact_datetime <- function(x) {
  if (is.null(x)) return(character())
  out <- rep(NA_character_, length(x))
  valid <- !is.na(x)
  if (any(valid)) {
    dt <- x[valid]
    day_part <- lubridate::day(dt)
    month_part <- format(dt, "%b")
    year_part <- lubridate::year(dt)
    hour24 <- lubridate::hour(dt)
    minute_part <- lubridate::minute(dt)
    hour12 <- ((hour24 + 11) %% 12) + 1
    ampm <- ifelse(hour24 >= 12, "PM", "AM")
    out[valid] <- sprintf(
      "%d%s%d %d:%02d %s",
      day_part, month_part, year_part,
      hour12, minute_part, ampm
    )
  }
  out
}

build_calendar_segments <- function(df, ws, we, local_tz) {
  if (is.null(df) || !nrow(df)) return(tibble())
  df %>%
    filter(!is.na(start_local) & !is.na(end_local)) %>%
    mutate(
      seg_start = pmax(start_local, ws),
      seg_end   = pmin(end_local, we + days(1))
    ) %>%
    filter(seg_end > seg_start) %>%
    rowwise() %>%
    mutate(
      day_list = list(
        seq.Date(
          as_date(floor_date(seg_start, "day")),
          as_date(floor_date(seg_end - seconds(1), "day")),
          by = "day"
        )
      )
    ) %>%
    ungroup() %>%
    tidyr::unnest(day_list, keep_empty = FALSE) %>%
    mutate(
      day_start = as_datetime(day_list, tz = local_tz),
      day_end   = day_start + days(1),
      part_start = pmax(seg_start, day_start),
      part_end   = pmin(seg_end, day_end),
      part_dur_h = as.numeric(difftime(part_end, part_start, units = "hours")),
      dow        = wday(day_start, week_start = 7, label = TRUE, abbr = FALSE)
    ) %>%
    filter(part_end > part_start) %>%
    mutate(
      start_hour = hour(part_start) + minute(part_start)/60 + second(part_start)/3600,
      end_hour   = hour(part_end) + minute(part_end)/60 + second(part_end)/3600
    ) %>%
    mutate(dow = factor(as.character(dow), levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")))
}

calendar_plot_from_segments <- function(segs, ws, we, title_text) {
  if (is.null(segs) || !nrow(segs)) return(NULL)
  segs <- segs %>%
    mutate(
      all_day = dplyr::coalesce(as.logical(all_day), FALSE),
      color_var = ifelse(is.na(color_var) | !nzchar(color_var), "(no group)", color_var),
      tooltip   = ifelse(is.na(tooltip) | !nzchar(tooltip), "(no details)", tooltip)
    )

  timed_hours <- c(segs$start_hour[!segs$all_day], segs$end_hour[!segs$all_day])
  if (length(timed_hours) && all(is.finite(timed_hours))) {
    hour_min <- floor(min(timed_hours, na.rm = TRUE))
    hour_max <- ceiling(max(timed_hours, na.rm = TRUE))
  } else {
    hour_min <- 0
    hour_max <- 24
  }
  hour_min <- max(0, hour_min)
  hour_max <- min(24, hour_max)
  if ((hour_max - hour_min) < 1) {
    hour_max <- min(24, hour_min + 1)
  }
  tick_step <- if ((hour_max - hour_min) > 12) 2 else 1

  has_all_day <- any(segs$all_day)
  if (has_all_day) {
    segs <- segs %>%
      mutate(
        start_hour = ifelse(all_day, -0.45, start_hour),
        end_hour   = ifelse(all_day, -0.05, end_hour)
      )
  }

  lvl <- sort(unique(segs$color_var))
  col_map <- stats::setNames(grDevices::hcl.colors(max(length(lvl), 1), "Dark 3"), lvl)
  dow_labels <- levels(segs$dow)

  segs <- segs %>% mutate(
    dow_num = as.numeric(dow),
    xmin = dow_num - 0.45,
    xmax = dow_num + 0.45,
    ymin = start_hour,
    ymax = end_hour
  )

  p <- plot_ly()
  for (g in lvl) {
    dfg <- segs %>% filter(color_var == g)
    if (!nrow(dfg)) next
    for (i in seq_len(nrow(dfg))) {
      show_leg <- (i == 1)
      p <- p %>% add_polygons(
        x = c(dfg$xmin[i], dfg$xmax[i], dfg$xmax[i], dfg$xmin[i], dfg$xmin[i]),
        y = c(dfg$ymin[i], dfg$ymin[i], dfg$ymax[i], dfg$ymax[i], dfg$ymin[i]),
        name = g,
        legendgroup = g,
        fillcolor = unname(col_map[g]),
        text = rep(dfg$tooltip[i], 5),
        hoverinfo = "text",
        hoveron = "fills",
        line = list(color = "black", width = 0.8),
        showlegend = show_leg,
        inherit = FALSE
      )
    }
  }

  tick_vals <- seq(hour_min, hour_max, by = tick_step)
  tick_text <- sprintf("%02d:00", tick_vals)
  axis_min <- hour_min
  if (has_all_day) {
    tick_vals <- c(-0.25, tick_vals)
    tick_text <- c("All Day", tick_text)
    axis_min <- min(-0.6, hour_min)
  }

  p %>% layout(
    title = title_text,
    legend = list(groupclick = "togglegroup"),
    xaxis = list(
      title = "Day",
      tickmode = "array",
      tickvals = 1:7,
      ticktext = dow_labels,
      range = c(0.5, 7.5)
    ),
    yaxis = list(
      title = "Time of Day",
      autorange = "reversed",
      range = c(hour_max, axis_min),
      tickmode = "array",
      tickvals = tick_vals,
      ticktext = tick_text
    )
  )
}

gt_project_task_table <- function(rollup_df, title = "Time by Project and Task", empty_message = "No data for this week") {
  if (is.null(rollup_df) || !nrow(rollup_df)) return(gt::gt(tibble(msg = empty_message)))
  rollup_df %>%
    mutate(hours_total = round(hours_total, 2)) %>%
    gt(groupname_col = "project") %>%
    tab_header(title = title) %>%
    fmt(
      columns = c(first_start, last_end),
      fns = format_compact_datetime
    ) %>%
    cols_label(
      task_title   = "Task",
      hours_total  = "Hours",
      entries      = "Entries",
      first_start  = "First Start",
      last_end     = "Last End",
    ) %>%
    tab_options(table.font.size = px(14)) %>%
    summary_rows(
      groups = TRUE,
      columns = c(hours_total),
      fns = list(Total = ~sum(., na.rm = TRUE))
    ) %>%
    grand_summary_rows(
      columns = c(hours_total, entries),
      fns = list(Overall = ~sum(., na.rm = TRUE))
    )
}

# Categorization rules (edit as needed)
rules <- tibble::tribble(
  ~pattern,                                   ~category,
  "(?i)grant|proposal|aims|specific aims",    "Grants",
  "(?i)class|lecture|syllabus|grading",       "Teaching",
  "(?i)manuscript|rev(ision)?|proof|submit",  "Writing",
  "(?i)meeting|1:1|check[- ]in|standup",      "Meetings",
  "(?i)code|script|package|bug|issue",        "Coding"
)

# Load time targets mapping (Project_Name + Category + Target_Hours)
time_targets <- readr::read_csv("time_targets.csv", show_col_types = FALSE)
search_all_projects_value <- "__all__"

# UI --------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Toggl Weekly Time"),
  fluidRow(
    column(12,
      div(style = "display:flex; gap:12px; align-items:center; flex-wrap:wrap;",
        actionButton("prev_week", "Previous Week"),
        actionButton("next_week", "Next Week"),
        actionButton("refresh", "Refresh Current Week"),
        strong(textOutput("week_label", container = span))
      )
    )
  ),
  fluidRow(
    column(12,
      tabsetPanel(
        tabPanel("Tracking",
          fluidRow(
            column(6,
              gt_output("project_task_table")
            ),
            column(6,
              plotlyOutput("category_plot", height = "520px")
            )
          )
        ),
        tabPanel("Search",
          fluidRow(
            column(4,
              textInput("search_text", "Search description", placeholder = "Type keywords")
            ),
            column(3,
              selectInput("search_project", "Project", choices = c("All Projects" = search_all_projects_value))
            ),
            column(2,
              dateInput("search_start_date", "Start date", value = Sys.Date() - 30)
            ),
            column(3,
              dateInput("search_end_date", "End date", value = Sys.Date())
            )
          ),
          fluidRow(
            column(12,
              gt_output("search_results_table")
            )
          )
        ),
        tabPanel("Calendar",
          div(style = "margin-bottom:8px;", selectInput("calendar_color", "Color by", choices = c("Project", "Category"), selected = "Project", width = "200px")),
          plotlyOutput("calendar_plot", height = "720px")
        ),
        tabPanel("Gcal",
          div(style = "margin-bottom:8px;", selectInput("gcal_color", "Color by", choices = c("Calendar", "Organizer"), selected = "Calendar", width = "200px")),
          plotlyOutput("gcal_plot", height = "720px")
        )
      )
    )
  )
)

# Server ----------------------------------------------------------------
server <- function(input, output, session) {
  week_offset <- reactiveVal(0L)  # 0 = current week, +n past weeks, -n future weeks

  observeEvent(input$prev_week, ignoreInit = TRUE, {
    week_offset(week_offset() + 1L)
  })
  observeEvent(input$next_week, ignoreInit = TRUE, {
    week_offset(week_offset() - 1L)
  })

  # Pull fresh entries whenever refresh is clicked, week offset changes, or parameters change
  entries_all <- reactive({
    input$refresh  # depend on refresh to re-pull current week
    off <- week_offset()
    off_int <- as.integer(off)
    prev_weeks <- if (off_int >= 0L) off_int else 0L
    next_weeks <- if (off_int <= 0L) as.integer(abs(off_int)) else 0L
    # Expand the fetch window to cover whichever direction the user is exploring
    get_toggl_entries(weeks_before = prev_weeks, weeks_after = next_weeks, force = TRUE)
  })

  toggl_history <- reactive({
    entries_all()  # keep DB fresh when this runs
    read_all_toggl_entries()
  })

  gcal_events_all <- reactive({
    input$refresh
    off <- week_offset()
    off_int <- as.integer(off)
    prev_weeks <- if (off_int >= 0L) off_int else 0L
    next_weeks <- if (off_int <= 0L) as.integer(abs(off_int)) else 0L
    tryCatch(
      get_gcal_events(weeks_before = prev_weeks, weeks_after = next_weeks, local_tz = local_tz),
      error = function(err) {
        warning("Google Calendar fetch failed: ", conditionMessage(err))
        tibble()
      }
    )
  })

  # Prepare + categorize
  prepared <- reactive({
    prepare_toggl(entries_all(), local_tz = local_tz, week_start = 7)
  })

  categorized <- reactive({
    categorize_tasks(prepared(), rules, default = "Other")
  })

  history_prepared <- reactive({
    prepare_toggl(toggl_history(), local_tz = local_tz, week_start = 7)
  })

  observeEvent(history_prepared(), {
    df <- history_prepared()
    choices <- c("All Projects" = search_all_projects_value)
    if (!is.null(df) && nrow(df)) {
      projects <- df %>%
        dplyr::filter(!is.na(project) & nzchar(project)) %>%
        dplyr::pull(project) %>%
        unique() %>%
        sort()
      if (length(projects)) {
        choices <- c(choices, stats::setNames(projects, projects))
      }
    }
    selected <- isolate(input$search_project)
    if (is.null(selected) || !(selected %in% choices)) {
      selected <- search_all_projects_value
    }
    updateSelectInput(session, "search_project", choices = choices, selected = selected)
  })

  observeEvent(history_prepared(), {
    df <- history_prepared()
    if (is.null(df) || !nrow(df)) return()
    min_day <- suppressWarnings(min(df$day, na.rm = TRUE))
    max_day <- suppressWarnings(max(df$day, na.rm = TRUE))
    if (is.na(min_day) || is.na(max_day)) return()
    updateDateInput(session, "search_start_date", min = min_day, max = max_day)
    updateDateInput(session, "search_end_date", min = min_day, max = max_day)
  })

  search_filtered <- reactive({
    df <- history_prepared()
    if (is.null(df) || !nrow(df)) return(tibble())

    start_date <- input$search_start_date
    end_date   <- input$search_end_date
    project_val <- input$search_project
    if (is.null(project_val) || is.na(project_val)) {
      project_val <- search_all_projects_value
    }
    query_text <- input$search_text
    if (is.null(query_text) || !length(query_text)) {
      query_text <- ""
    }
    query_text <- stringr::str_squish(query_text)
    if (!length(query_text) || is.na(query_text)) {
      query_text <- ""
    }

    has_start <- !is.null(start_date) && length(start_date) > 0 && !is.na(start_date)
    has_end   <- !is.null(end_date) && length(end_date) > 0 && !is.na(end_date)

    if (has_start) {
      df <- df %>% filter(day >= as_date(start_date))
    }
    if (has_end) {
      df <- df %>% filter(day <= as_date(end_date))
    }
    if (!identical(project_val, search_all_projects_value)) {
      df <- df %>% filter(project == project_val)
    }
    if (nzchar(query_text)) {
      pattern <- stringr::regex(query_text, ignore_case = TRUE)
      df <- df %>% filter(stringr::str_detect(dplyr::coalesce(description, ""), pattern))
    }
    df
  })

  gcal_prepared <- reactive({
    df <- gcal_events_all()
    if (is.null(df) || !nrow(df)) return(df)
    df %>%
      mutate(
        start_local = with_tz(start_utc, local_tz),
        end_local   = with_tz(end_utc, local_tz),
        week_start  = floor_date(start_local, "week", week_start = 7)
      )
  })

  gcal_selected_week <- reactive({
    df <- gcal_prepared()
    if (is.null(df) || !nrow(df)) return(df)
    rng <- selected_week_range()
    df %>% filter(week_start == rng$ws)
  })

  # Selected week data
  selected_week_df <- reactive({
    df <- categorized()
    if (is.null(df) || !nrow(df)) return(df)
    rng <- selected_week_range()
    df %>% filter(week_start == rng$ws)
  })

  # Selected week range label (always computed from week_offset)
  selected_week_range <- reactive({
    ws <- floor_date(now(tzone = local_tz), unit = "week", week_start = 7) - weeks(week_offset())
    we <- ws + days(6)
    list(ws = ws, we = we, label = paste0(as_date(ws), " — ", as_date(we)))
  })

  output$week_label <- renderText({
    rng <- selected_week_range()
    df <- selected_week_df()
    if (is.null(df) || !nrow(df)) {
      return(paste0("Week of ", rng$label, " (no data)"))
    }
    paste0("Week of ", rng$label)
  })

  # Render GT table
  output$project_task_table <- render_gt({
    rollup <- summarize_by_project_task(selected_week_df())
    rng <- selected_week_range()
    gt_project_task_table(rollup, title = paste0("Toggl — Hours by Project & Task (", rng$label, ")"))
  })

  output$search_results_table <- render_gt({
    df <- search_filtered()
    start_val <- input$search_start_date
    end_val   <- input$search_end_date
    has_start <- !is.null(start_val) && length(start_val) > 0 && !is.na(start_val)
    has_end   <- !is.null(end_val) && length(end_val) > 0 && !is.na(end_val)
    range_label <- paste0(
      if (has_start) start_val else "?",
      " — ",
      if (has_end) end_val else "?"
    )
    rollup <- summarize_by_project_task(df)
    gt_project_task_table(
      rollup,
      title = paste0("Toggl Search — Hours by Project & Task (", range_label, ")"),
      empty_message = "No entries match these filters"
    )
  })

  # Targets plot (interactive with description-level tooltip; targets from CSV)
  output$category_plot <- renderPlotly({
    df <- selected_week_df()
    if (is.null(df) || !nrow(df)) return(NULL)

    # Hours by project in selected week
    project_hours <- df %>%
      group_by(project) %>%
      summarise(hours_spent = sum(duration_hr, na.rm = TRUE), .groups = "drop")

    # Join with time_targets to include targets and categories
    pts <- time_targets %>%
      full_join(project_hours, by = c("Project_Name" = "project")) %>%
      mutate(
        hours_spent  = tidyr::replace_na(hours_spent, 0),
        Target_Hours = tidyr::replace_na(Target_Hours, 0),
        Category     = tidyr::replace_na(Category, "Not in Targets"),
        Project_Name = forcats::fct_reorder(Project_Name, hours_spent)
      ) %>%
      arrange(desc(hours_spent))

    # Tooltip: list descriptions aggregated within each project
    desc_agg <- df %>%
      group_by(project, description) %>%
      summarise(hours = sum(duration_hr, na.rm = TRUE), .groups = "drop") %>%
      arrange(project, desc(hours)) %>%
      group_by(project) %>%
      summarise(
        tooltip_desc = paste0(
          purrr::map2_chr(description, hours, ~ paste0(.x %||% "(no description)", ": ", round(.y, 1), "h")),
          collapse = "<br>"
        ), .groups = "drop"
      )

    plot_df <- pts %>% left_join(desc_agg, by = c("Project_Name" = "project")) %>%
      mutate(
        tooltip = paste0(
          "<b>", as.character(Project_Name), "</b><br>",
          "Spent: ", round(hours_spent, 1), "h<br>",
          "Target: ", round(Target_Hours, 1), "h",
          ifelse(is.na(tooltip_desc), "", paste0("<br><br>", tooltip_desc))
        )
      )

    p <- ggplot(plot_df, aes(x = Project_Name, y = hours_spent, text = tooltip)) +
      geom_col(fill = "steelblue") +
      geom_errorbar(aes(ymin = Target_Hours, ymax = Target_Hours), width = 0.5, linewidth = 0.9, color = "firebrick4") +
      coord_flip() +
      labs(title = paste0("Time Spent vs Targets (", selected_week_range()$label, ")"), x = NULL, y = "Hours") +
      theme_minimal(base_size = 13)

    ggplotly(p, tooltip = "text") %>%
      layout(xaxis = list(title = "Hours"), yaxis = list(title = "Project"))
  })

  # Calendar plot (week grid showing time-of-day blocks)
  output$calendar_plot <- renderPlotly({
    df <- selected_week_df()
    if (is.null(df) || !nrow(df)) return(NULL)
    rng <- selected_week_range()

    seg_ready <- df %>%
      mutate(
        color_var = if (identical(input$calendar_color, "Category")) category else project,
        label_line = dplyr::coalesce(task_title, description),
        label_line = ifelse(!is.na(label_line) & nzchar(label_line), stringr::str_squish(label_line), NA_character_),
        all_day = FALSE
      )

    segs <- build_calendar_segments(seg_ready, rng$ws, rng$we, local_tz) %>%
      mutate(
        tooltip = paste0(
          "<b>", project %||% "(no project)", "</b><br>",
          ifelse(!is.na(label_line) & nzchar(label_line), paste0(label_line, "<br>"), ""),
          format(part_start, "%a %Y-%m-%d %H:%M"), " — ", format(part_end, "%H:%M"), "<br>",
          "Dur: ", round(part_dur_h, 2), "h"
        )
      )

    calendar_plot_from_segments(
      segs,
      rng$ws,
      rng$we,
      paste0("Calendar — ", as_date(rng$ws), " — ", as_date(rng$we))
    )
  })

  output$gcal_plot <- renderPlotly({
    df <- gcal_selected_week()
    if (is.null(df) || !nrow(df)) return(NULL)
    rng <- selected_week_range()

    seg_ready <- df %>%
      mutate(
        color_var = if (identical(input$gcal_color, "Organizer")) organizer else calendar_name,
        label_line = dplyr::coalesce(subject, "(no title)")
      )

    segs <- build_calendar_segments(seg_ready, rng$ws, rng$we, local_tz) %>%
      mutate(
        label_line = ifelse(!is.na(label_line) & nzchar(label_line), stringr::str_squish(label_line), "(no title)"),
        tooltip = ifelse(
          all_day,
          paste0(
            "<b>", label_line, "</b><br>",
            format(part_start, "%a %Y-%m-%d (all day)"), "<br>",
            ifelse(!is.na(location) & nzchar(location), paste0(location, "<br>"), ""),
            ifelse(!is.na(organizer) & nzchar(organizer), paste0("Organizer: ", organizer, "<br>"), ""),
            ifelse(!is.na(attendees) & nzchar(attendees), paste0("Attendees: ", attendees, "<br>"), "")
          ),
          paste0(
            "<b>", label_line, "</b><br>",
            format(part_start, "%a %Y-%m-%d %H:%M"), " — ", format(part_end, "%H:%M"), "<br>",
            ifelse(!is.na(location) & nzchar(location), paste0(location, "<br>"), ""),
            ifelse(!is.na(organizer) & nzchar(organizer), paste0("Organizer: ", organizer, "<br>"), ""),
            ifelse(!is.na(attendees) & nzchar(attendees), paste0("Attendees: ", attendees, "<br>"), ""),
            "Dur: ", round(part_dur_h, 2), "h"
          )
        )
      )

    calendar_plot_from_segments(
      segs,
      rng$ws,
      rng$we,
      paste0("Google Calendar — ", as_date(rng$ws), " — ", as_date(rng$we))
    )
  })
}

# For interactive use via source("toggl_table.R") uncomment below:
# if (interactive()) shinyApp(ui, server)
