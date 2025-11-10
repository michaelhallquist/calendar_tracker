# ============================================================
# Toggl Track -> R data.frame via httr2 (Reports API v2)
# Dependencies:
# install.packages(c("httr2", "dplyr", "purrr", "tibble", "lubridate", "readr", "jsonlite"))
# ============================================================

library(httr2)
library(dplyr)
library(purrr)
library(tibble)
library(lubridate)
library(readr)
library(jsonlite)
library(DBI)
library(RSQLite)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- 0) Config ----
api_token <- Sys.getenv("TOGGL_API_TOKEN")  # setenv in .Renviron or Sys.setenv(TOGGL_API_TOKEN="...")
if (identical(api_token, "")) stop("Set TOGGL_API_TOKEN env var to your Toggl Track API token.")

user_agent_str <- "toggl-r-client"  # any identifying string for reports API

weeks_span <- 3L
local_tz   <- "America/New_York"  # your local timezone for defining the window
db_path    <- Sys.getenv("TOGGL_DB_PATH")
if (identical(db_path, "")) {
  db_path <- file.path(getwd(), "toggl_entries.sqlite")
}
db_path <- path.expand(db_path)
# Fallback to tempdir() if target directory is not writable (e.g., shinyapps.io app dir)
try({
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  testfile <- file.path(dirname(db_path), ".db_write_test")
  if (!file.create(testfile)) db_path <<- file.path(tempdir(), "toggl_entries.sqlite")
  if (file.exists(testfile)) unlink(testfile, force = TRUE)
}, silent = TRUE)

# ---- 1) Auth helpers ----
auth_basic <- function(req) req_auth_basic(req, username = api_token, password = "api_token")

resp_ok <- function(resp) {
  resp_check_status(resp)
  resp
}

get_workspace_id <- function() {
  me_url <- "https://api.track.toggl.com/api/v9/me"
  me_resp <- request(me_url) |>
    auth_basic() |>
    req_retry(max_tries = 5, backoff = ~ runif(1, .5, 2)) |>
    req_perform() |>
    resp_ok()
  me <- resp_body_json(me_resp, simplifyVector = TRUE)
  workspace_id <- me$default_workspace_id %||%
    (if (!is.null(me$workspaces) && length(me$workspaces)) me$workspaces[[1]]$id else NULL)
  if (is.null(workspace_id)) stop("No workspace_id found on account.")
  message("Using workspace_id: ", workspace_id)
  workspace_id
}

# ---- 4) Fetch Detailed Report (v2) with pagination ----
# Docs: /reports/api/v2/details
# Important params: workspace_id, since, until, user_agent, page, per_page (<= 200)
reports_base <- "https://api.track.toggl.com/reports/api/v2/details"

fetch_details_page <- function(workspace_id, since, until, page = 1, per_page = 200) {
  req <- request(reports_base) |>
    req_url_query(
      workspace_id = workspace_id,
      since        = as.character(since),
      until        = as.character(until),
      user_agent   = user_agent_str,
      page         = page,
      per_page     = per_page,
      order_desc   = "on",             # newest first (optional)
      without_subtotals = "true"
      # You can add: project_ids, user_ids, client_ids, billable, tag_ids, etc.
    ) |>
    auth_basic() |>
    req_retry(max_tries = 5, backoff = ~ runif(1, .5, 2))
  
  resp <- req_perform(req) |> resp_ok()
  resp_body_json(resp, simplifyVector = TRUE)
}

fetch_all_details <- function(workspace_id, since, until, per_page = 200, max_pages = 1000) {
  page <- 1L
  out  <- list()
  repeat {
    body <- fetch_details_page(workspace_id, since, until, page = page, per_page = per_page)
    items <- body$data
    if (is.null(items) || length(items) == 0) break
    out <- c(out, list(items))
    # stop when we've collected all entries (based on total_count/per_page) or hit empty
    total <- body$total_count %||% NA_integer_
    if (!is.na(total) && page * per_page >= total) break
    page <- page + 1L
    if (page > max_pages) break
  }
  if (length(out)) bind_rows(out) else tibble()
}

read_entries_range <- function(con, start_utc, end_utc) {
  con <- ensure_conn_valid(con)
  ensure_entries_table(con)
  sql <- "SELECT id, description, project, project_id, client, task, user, billable,\n                 start_utc, end_utc, duration_sec, tags, workspace_id, fetched_at\n          FROM toggl_entries\n          WHERE start_utc >= :start_utc AND start_utc < :end_utc\n          ORDER BY start_utc ASC;"
  res <- DBI::dbGetQuery(con, sql, params = list(start_utc = start_utc, end_utc = end_utc))
  if (!nrow(res)) return(tibble())
  res %>%
    as_tibble() %>%
    mutate(
      id           = as.character(id),
      project_id   = suppressWarnings(as.integer(project_id)),
      billable     = suppressWarnings(as.integer(billable)),
      duration_sec = suppressWarnings(as.numeric(duration_sec)),
      start_utc    = suppressWarnings(ymd_hms(start_utc, tz = "UTC", quiet = TRUE)),
      end_utc      = suppressWarnings(ymd_hms(end_utc,   tz = "UTC", quiet = TRUE))
    )
}

# ---- 5) SQLite helpers ----
ensure_entries_table <- function(con) {
  if (!DBI::dbIsValid(con)) stop("Invalid or closed connection to SQLite DB.")
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS toggl_entries (
      id TEXT PRIMARY KEY,
      description TEXT,
      project TEXT,
      project_id INTEGER,
      client TEXT,
      task TEXT,
      user TEXT,
      billable INTEGER,
      start_utc TEXT,
      end_utc TEXT,
      duration_sec REAL,
      tags TEXT,
      workspace_id INTEGER,
      fetched_at TEXT
    );
    "
  )
}

init_db_connection <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  if (!DBI::dbIsValid(con)) {
    stop(sprintf("Failed to open SQLite database at %s", path))
  }
  # Improve reliability and concurrency characteristics
  try(DBI::dbExecute(con, "PRAGMA journal_mode = WAL;"), silent = TRUE)
  try(DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;"), silent = TRUE)
  # Simple liveness check
  ok <- try(DBI::dbGetQuery(con, "SELECT 1;"), silent = TRUE)
  if (inherits(ok, "try-error")) {
    try(DBI::dbDisconnect(con), silent = TRUE)
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
    try(DBI::dbExecute(con, "PRAGMA journal_mode = WAL;"), silent = TRUE)
    try(DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;"), silent = TRUE)
  }
  res <- try(ensure_entries_table(con), silent = TRUE)
  if (inherits(res, "try-error")) {
    try(DBI::dbDisconnect(con), silent = TRUE)
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
    if (!DBI::dbIsValid(con)) {
      stop(sprintf("Failed to open SQLite database at %s on retry", path))
    }
    try(DBI::dbExecute(con, "PRAGMA journal_mode = WAL;"), silent = TRUE)
    try(DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;"), silent = TRUE)
    ensure_entries_table(con)
  }
  con
}

ensure_conn_valid <- function(con) {
  if (!DBI::dbIsValid(con)) {
    # Refresh the global connection if it has gone stale
    conn <<- init_db_connection(db_path)
    return(conn)
  }
  con
}

max_start_from_db <- function(con) {
  con <- ensure_conn_valid(con)
  # Ensure table exists before querying (idempotent)
  try(ensure_entries_table(con), silent = TRUE)
  res <- try(DBI::dbGetQuery(con, "SELECT MAX(start_utc) AS max_start FROM toggl_entries;"), silent = TRUE)
  if (inherits(res, "try-error")) {
    # One more refresh in case the connection closed between checks
    con <- ensure_conn_valid(con)
    res <- DBI::dbGetQuery(con, "SELECT MAX(start_utc) AS max_start FROM toggl_entries;")
  }
  if (!nrow(res)) return(NA_character_)
  res$max_start[[1]]
}

iso_utc <- function(x) {
  if (!length(x)) return(character())
  out <- rep(NA_character_, length(x))
  valid <- !is.na(x)
  if (any(valid)) {
    out[valid] <- format(with_tz(x[valid], "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  }
  out
}

write_entries <- function(con, df) {
  if (!nrow(df)) return(invisible(0L))
  con <- ensure_conn_valid(con)
  payload <- as.data.frame(df)
  insert_stmt <- "
    INSERT INTO toggl_entries (
      id, description, project, project_id, client, task, user, billable,
      start_utc, end_utc, duration_sec, tags, workspace_id, fetched_at
    ) VALUES (
      :id, :description, :project, :project_id, :client, :task, :user, :billable,
      :start_utc, :end_utc, :duration_sec, :tags, :workspace_id, :fetched_at
    )
    ON CONFLICT(id) DO UPDATE SET
      description   = excluded.description,
      project       = excluded.project,
      project_id    = excluded.project_id,
      client        = excluded.client,
      task          = excluded.task,
      user          = excluded.user,
      billable      = excluded.billable,
      start_utc     = excluded.start_utc,
      end_utc       = excluded.end_utc,
      duration_sec  = excluded.duration_sec,
      tags          = excluded.tags,
      workspace_id  = excluded.workspace_id,
      fetched_at    = excluded.fetched_at;
  "
  DBI::dbWithTransaction(con, {
    stmt <- DBI::dbSendStatement(con, insert_stmt)
    # Bind and clear immediately; avoid leaving open results
    tryCatch({
      DBI::dbBind(stmt, payload)
    }, finally = {
      suppressWarnings(try(DBI::dbClearResult(stmt), silent = TRUE))
    })
  })
  invisible(nrow(df))
}

# ---- 5) Tidy columns to a friendly data.frame ----
# The Detailed API returns many fields; we pick common ones. Adjust as desired.
# Typical fields in 'data' rows: id, description, start, end, dur (ms), project, project_id,
# client, user, task, tags (list), billable, currency, workspace_id, etc.
tidy_tags <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0) return(NA_character_)
  paste0(x, collapse = "; ")
}

get_toggl_entries <- function(weeks_before = 3L, weeks_after = 3L, force = FALSE) {
  # Fixed window: [week_start - weeks_before, week_start + weeks(weeks_after + 1) - 1 day]
  now_local <- now(tzone = local_tz)
  week_start_local <- floor_date(now_local, unit = "week", week_start = 7)
  range_start_local <- week_start_local - weeks(weeks_before)
  range_end_local   <- week_start_local + weeks(weeks_after + 1) - days(1)

  # Window as inclusive dates for API
  window_since_date <- as_date(with_tz(range_start_local, "UTC"))
  window_until_date <- as_date(with_tz(range_end_local,   "UTC"))

  conn <- init_db_connection(db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Earliest query date (respect last sync unless forced)
  since_date <- window_since_date
  if (!isTRUE(force)) {
    last_synced_start <- max_start_from_db(conn)
    if (!is.na(last_synced_start)) {
      last_minus_one <- as_date(ymd_hms(last_synced_start, tz = "UTC") - days(1))
      since_date <- max(window_since_date, last_minus_one)
    }
  }

  until_date <- window_until_date

  workspace_id <- get_workspace_id()
  message(sprintf("Pulling Toggl entries from %s to %s (inclusive)", since_date, until_date))

  raw_df <- fetch_all_details(workspace_id, since_date, until_date)

  if (nrow(raw_df)) {
    entries_df <- raw_df %>%
      transmute(
        id            = as.character(.data$id %||% NA_real_),
        description   = .data$description %||% NA_character_,
        project       = .data$project %||% NA_character_,
        project_id    = as.integer(.data$pid %||% NA_integer_),
        client        = .data$client %||% NA_character_,
        task          = .data$task %||% NA_character_,
        user          = .data$user %||% NA_character_,
        billable      = as.integer(.data$billable),
        start_utc     = suppressWarnings(ymd_hms(.data$start, quiet = TRUE, tz = "UTC")),
        end_utc       = suppressWarnings(ymd_hms(.data$end,   quiet = TRUE, tz = "UTC")),
        duration_sec  = if (!is.null(.data$dur)) as.numeric(.data$dur) / 1000 else NA_real_,
        tags          = map_chr(.data$tags, tidy_tags),
        workspace_id  = as.integer(workspace_id)
      ) %>%
      arrange(start_utc)

    fetched_at <- iso_utc(rep(now(tzone = "UTC"), nrow(entries_df)))
    db_entries <- entries_df %>%
      mutate(
        start_utc = iso_utc(start_utc),
        end_utc   = iso_utc(end_utc),
        fetched_at = fetched_at
      )

    rows_written <- write_entries(conn, db_entries)
    message(sprintf("Upserted %s rows into %s", rows_written, db_path))
  } else {
    message("No entries returned by the API. Database left unchanged.")
  }

  # Return all entries in the requested window from the DB
  # Use end-exclusive to include the full last day
  start_iso <- iso_utc(range_start_local)
  end_iso_excl <- iso_utc(range_end_local + days(1))
  df <- read_entries_range(conn, start_iso, end_iso_excl)
  # Optional: save latest pull to a flat file for inspection (use tempdir for hosted envs)
  suppressWarnings(try(readr::write_csv(df, file.path(tempdir(), "toggl_time_entries_latest.csv")), silent = TRUE))
  df
}
