# Shared calendar plotting utilities used by both Toggl and Google Calendar views.
# These helpers stay agnostic to the upstream data sources.

build_color_map <- function(levels_vec) {
  lvl <- sort(unique(levels_vec))
  if (!length(lvl)) return(character())
  stats::setNames(grDevices::hcl.colors(max(length(lvl), 1), "Dark 3"), lvl)
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

calendar_plot_from_segments <- function(segs, ws, we, title_text, color_map = NULL) {
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
  if (is.null(color_map) || !length(color_map)) {
    col_map <- build_color_map(lvl)
  } else {
    col_map <- color_map
    missing <- setdiff(lvl, names(col_map))
    if (length(missing)) {
      col_map <- c(col_map, build_color_map(missing))
    }
    col_map <- col_map[lvl]
  }
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
