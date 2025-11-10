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

gt_project_task_table <- function(rollup_df, title = "Time by Project and Task") {
  if (is.null(rollup_df) || !nrow(rollup_df)) return(gt::gt(tibble(msg = "No data for this week")))
  rollup_df %>%
    mutate(hours_total = round(hours_total, 2)) %>%
    gt(groupname_col = "project") %>%
    tab_header(title = title) %>%
    fmt_datetime(columns = c(first_start, last_end), date_style = 6, time_style = 4) %>%
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
        tabPanel("Calendar",
          div(style = "margin-bottom:8px;", selectInput("calendar_color", "Color by", choices = c("Project", "Category"), selected = "Project", width = "200px")),
          plotlyOutput("calendar_plot", height = "720px")
        )
      )
    )
  )
)

# Server ----------------------------------------------------------------
server <- function(input, output, session) {
  week_offset <- reactiveVal(0L)  # 0 = current week, 1 = previous, etc.

  observeEvent(input$prev_week, ignoreInit = TRUE, {
    week_offset(week_offset() + 1L)
  })
  observeEvent(input$next_week, ignoreInit = TRUE, {
    week_offset(max(0L, week_offset() - 1L))
  })

  # Pull fresh entries whenever refresh is clicked, week offset changes, or parameters change
  entries_all <- reactive({
    input$refresh  # depend on refresh to re-pull current week
    off <- week_offset()
    # Refresh only the selected week window (prev/next triggers reactivity via off)
    get_toggl_entries(weeks_before = off, weeks_after = 0L, force = TRUE)
  })

  # Prepare + categorize
  prepared <- reactive({
    prepare_toggl(entries_all(), local_tz = local_tz, week_start = 7)
  })

  categorized <- reactive({
    categorize_tasks(prepared(), rules, default = "Other")
  })

  # Selected week data
  selected_week_df <- reactive({
    df <- categorized()
    if (is.null(df) || !nrow(df)) return(df)
    week_start_now <- floor_date(now(tzone = local_tz), unit = "week", week_start = 7)
    sel_start <- week_start_now - weeks(week_offset())
    df %>% filter(week_start == sel_start)
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

    # Week bounds in local tz
    ws <- floor_date(now(tzone = local_tz), unit = "week", week_start = 7) - weeks(week_offset())
    we <- ws + days(6)

    # Prepare segments split by day and clamped to week bounds
    segs <- df %>%
      filter(!is.na(start_local) & !is.na(end_local)) %>%
      mutate(
        seg_start = pmax(start_local, ws),
        seg_end   = pmin(end_local,   we + days(1))
      ) %>%
      filter(seg_end > seg_start) %>%
      rowwise() %>%
      mutate(day_list = list(seq.Date(as_date(floor_date(seg_start, "day")), as_date(floor_date(seg_end - seconds(1), "day")), by = "day"))) %>%
      ungroup() %>%
      tidyr::unnest(day_list, keep_empty = FALSE) %>%
      mutate(
        day_start = as_datetime(day_list, tz = local_tz),
        day_end   = day_start + days(1),
        part_start = pmax(seg_start, day_start),
        part_end   = pmin(seg_end,   day_end),
        part_dur_h = as.numeric(difftime(part_end, part_start, units = "hours")),
        dow        = wday(day_start, week_start = 7, label = TRUE, abbr = FALSE)
      ) %>%
      filter(part_end > part_start) %>%
      mutate(
        start_hour = hour(part_start) + minute(part_start)/60 + second(part_start)/3600,
        end_hour   = hour(part_end) + minute(part_end)/60 + second(part_end)/3600,
        color_var  = if (identical(input$calendar_color, "Category")) category else project,
        label_line = dplyr::coalesce(task_title, description),
        label_line = ifelse(!is.na(label_line) & nzchar(label_line), stringr::str_squish(label_line), NA_character_),
        tooltip    = paste0(
          "<b>", project %||% "(no project)", "</b><br>",
          ifelse(!is.na(label_line) & nzchar(label_line), paste0(label_line, "<br>"), ""),
          format(part_start, "%a %Y-%m-%d %H:%M"), " — ", format(part_end, "%H:%M"), "<br>",
          "Dur: ", round(part_dur_h, 2), "h"
        )
      ) %>%
      mutate(dow = factor(as.character(dow), levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")))

    # Dynamic hour bounds based on data (rounded)
    hour_min <- floor(min(c(segs$start_hour, segs$end_hour), na.rm = TRUE))
    hour_max <- ceiling(max(c(segs$start_hour, segs$end_hour), na.rm = TRUE))
    hour_min <- max(0, hour_min)
    hour_max <- min(24, hour_max)
    if ((hour_max - hour_min) < 1) {
      hour_max <- min(24, hour_min + 1)
    }
    tick_step <- if ((hour_max - hour_min) > 12) 2 else 1

    # Build polygons per entry for plotly (hover on fills) with one trace per group
    lvl <- sort(unique(segs$color_var))
    col_map <- stats::setNames(grDevices::hcl.colors(length(lvl), "Dark 3"), lvl)
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

    p %>% layout(
      title = paste0("Calendar — ", as_date(ws), " — ", as_date(we)),
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
        range = c(hour_max, hour_min),
        tickmode = "array",
        tickvals = seq(hour_min, hour_max, by = tick_step),
        ticktext = sprintf("%02d:00", seq(hour_min, hour_max, by = tick_step))
      )
    )
  })
}

# For interactive use via source("toggl_table.R") uncomment below:
# if (interactive()) shinyApp(ui, server)
