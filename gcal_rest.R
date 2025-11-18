#
# Google Calendar -> SQLite helper functions
#

library(httr2)
library(gargle)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(lubridate)
library(DBI)
library(RSQLite)

`%||%` <- function(a, b) if (!is.null(a)) a else b

gcal_local_tz <- {
  tz <- Sys.getenv("GCAL_LOCAL_TZ", unset = "")
  if (!nzchar(tz)) tz <- Sys.getenv("LOCAL_TZ", unset = "")
  if (!nzchar(tz)) tz <- Sys.getenv("TZ", unset = "")
  if (!nzchar(tz)) tz <- "America/New_York"
  tz
}

.gcal_configure_gargle <- local({
  cache <- Sys.getenv("GARGLE_OAUTH_CACHE", unset = "")
  email <- Sys.getenv("GARGLE_OAUTH_EMAIL", unset = "")
  if (nzchar(cache)) {
    cache <- path.expand(cache)
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
    options(gargle_oauth_cache = cache)
  }
  if (nzchar(email)) {
    # Strip surrounding quotes that are sometimes left in .Renviron
    email <- sub('^\"', "", sub('\"$', "", email))
    options(gargle_oauth_email = email)
  }
  TRUE
})

.gcal_state <- new.env(parent = emptyenv())
gcal_scope <- "https://www.googleapis.com/auth/calendar.readonly"
gcal_base  <- "https://www.googleapis.com/calendar/v3"

gcal_client_config_from_env <- function() {
  json_env <- Sys.getenv("GCAL_CLIENT_JSON", unset = "")
  if (!nzchar(json_env)) {
    stop("Set GCAL_CLIENT_JSON to the JSON contents of your Google OAuth client.")
  }
  parsed <- tryCatch(jsonlite::fromJSON(json_env), error = function(e) NULL)
  if (is.null(parsed)) stop("GCAL_CLIENT_JSON could not be parsed as JSON.")

  candidate <- NULL
  if (!is.null(parsed$installed)) candidate <- parsed$installed
  if (!is.null(parsed$web)) candidate <- parsed$web
  if (is.null(candidate)) stop("GCAL_CLIENT_JSON must contain an 'installed' or 'web' section.")

  id <- candidate$client_id %||% candidate$clientId %||% NULL
  secret <- candidate$client_secret %||% candidate$clientSecret %||% NULL
  redirect_uris <- candidate$redirect_uris %||% candidate$redirectUris %||% NULL

  if (!nzchar(id) || !nzchar(secret)) {
    stop("GCAL_CLIENT_JSON is missing client_id or client_secret entries.")
  }
  list(id = id, secret = secret, redirect_uris = redirect_uris)
}

gcal_client <- function() {
  if (is.null(.gcal_state$client)) {
    cfg <- gcal_client_config_from_env()
    .gcal_state$client <- gargle::gargle_oauth_client(
      name = "gcal_client",
      id = cfg$id,
      secret = cfg$secret,
      redirect_uris = cfg$redirect_uris
    )
  }
  .gcal_state$client
}

gcal_token <- function(scope = gcal_scope) {
  if (!is.null(.gcal_state$token)) {
    return(.gcal_state$token)
  }

  tok <- NULL

  # 1) Try a pre-encoded token saved via gargle::secret_write_rds()
  token_rds_path <- Sys.getenv("GCAL_TOKEN_RDS", unset = ".secrets/gcal-token.rds")
  key_env_name <- "GARGLE_KEY"
  has_key <- gargle::secret_has_key(key_env_name)
  if (nzchar(token_rds_path) && file.exists(token_rds_path) && has_key) {
    message(sprintf("GCAL token: attempting to load %s with key '%s'", token_rds_path, key_env_name))
    tok <- tryCatch(
      gargle::secret_read_rds(token_rds_path, key = key_env_name),
      error = function(e) {
        message(sprintf("GCAL token: failed to read secret RDS (%s)", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(tok)) message("GCAL token: loaded token from secret RDS")
  }

  # 2) Fall back to the standard cache-based fetch;
  #    this will also mint a new token interactively if needed.
  if (is.null(tok)) {
    tok <- gargle::token_fetch(
      scopes = scope,
      client = gcal_client(),
      cache = gargle::gargle_oauth_cache(),
      email = gargle::gargle_oauth_email()
    )
    # If we got a token, persist it for deployments that lack interactive auth
    if (!is.null(tok) && nzchar(token_rds_path) && has_key) {
      dir.create(dirname(token_rds_path), recursive = TRUE, showWarnings = FALSE)
      message(sprintf("GCAL token: writing token to %s with key '%s'", token_rds_path, key_env_name))
      try(
        gargle::secret_write_rds(tok, token_rds_path, key = key_env_name),
        silent = TRUE
      )
    }
  }

  if (is.null(tok)) {
    stop("Google Calendar authentication failed: no token available. Re-authenticate with GCAL_CLIENT_JSON + GARGLE_OAUTH_* settings.")
  }
  if (is.null(tok$credentials$access_token)) {
    stop("Google Calendar authentication failed: access token is missing. Re-authenticate (remove old cache if needed) so gargle can mint a new token.")
  }
  .gcal_state$token <- tok
  tok
}

gcal_auth_req <- function(req, token = NULL) {
  tok <- token %||% gcal_token()
  access_token <- tok$credentials$access_token
  if (is.null(access_token)) {
    stop("Google Calendar authentication failed: access_token is NULL. Re-authenticate to refresh the token cache.")
  }
  req_auth_bearer_token(req, token = access_token)
}

gcal_list_calendars <- function(token = NULL) {
  tok <- token %||% gcal_token()
  req <- request(paste0(gcal_base, "/users/me/calendarList")) |>
    gcal_auth_req(tok) |>
    req_retry(max_tries = 5, backoff = ~ runif(1, 0.5, 2))
  resp <- req_perform(req)
  resp_check_status(resp)
  items <- resp_body_json(resp, simplifyVector = FALSE)$items %||% list()
  map_dfr(items, function(x) {
    tibble(
      id      = x$id %||% NA_character_,
      summary = x$summary %||% NA_character_,
      primary = isTRUE(x$primary)
    )
  })
}

gcal_resolve_calendar <- function(token = NULL, refresh = FALSE) {
  if (!refresh && !is.null(.gcal_state$calendar_info)) {
    return(.gcal_state$calendar_info)
  }
  env_id <- Sys.getenv("GCAL_CALENDAR_ID")
  env_name <- Sys.getenv("GCAL_CALENDAR_NAME")
  if (nzchar(env_id)) {
    info <- list(id = env_id, summary = if (nzchar(env_name)) env_name else env_id)
    .gcal_state$calendar_info <- info
    return(info)
  }
  tok <- token %||% gcal_token()
  cal_tbl <- gcal_list_calendars(tok)
  if (!nrow(cal_tbl)) stop("No calendars available for this account.")
  info <- cal_tbl %>%
    arrange(desc(primary)) %>%
    slice_head(n = 1) %>%
    as.list()
  .gcal_state$calendar_info <- info
  info
}

gcal_fmt_rfc3339 <- function(x) format(with_tz(x, "UTC"), "%Y-%m-%dT%H:%M:%SZ")

gcal_fetch_events_page <- function(calendar_id, time_min, time_max, page_token = NULL, page_size = 2500, token = NULL) {
  tok <- token %||% gcal_token()
  req <- request(paste0(gcal_base, "/calendars/", URLencode(calendar_id, reserved = TRUE), "/events")) |>
    req_url_query(
      singleEvents = "true",
      orderBy      = "startTime",
      timeMin      = gcal_fmt_rfc3339(time_min),
      timeMax      = gcal_fmt_rfc3339(time_max),
      maxResults   = page_size,
      pageToken    = page_token %||% NULL
    ) |>
    gcal_auth_req(tok) |>
    req_retry(max_tries = 5, backoff = ~ runif(1, 0.5, 2))
  resp <- req_perform(req)
  resp_check_status(resp)
  body <- resp_body_json(resp, simplifyVector = FALSE)
  list(
    items = body$items %||% list(),
    nextPageToken = body$nextPageToken %||% NULL
  )
}

gcal_fetch_all_events <- function(calendar_id, time_min, time_max, token = NULL) {
  token <- token %||% gcal_token()
  out <- list()
  page_token <- NULL
  repeat {
    page <- gcal_fetch_events_page(calendar_id, time_min, time_max, page_token, token = token)
    if (length(page$items)) out <- c(out, page$items)
    page_token <- page$nextPageToken
    if (is.null(page_token)) break
  }
  out
}

gcal_collapse_attendees <- function(att) {
  if (is.null(att) || length(att) == 0) return(NA_character_)
  paste0(
    vapply(att, function(a) a$email %||% NA_character_, character(1)),
    collapse = "; "
  )
}

gcal_pick_dt <- function(node, which = c("start", "end"), tz_default = gcal_local_tz) {
  which <- match.arg(which)
  dt <- node[[which]]$dateTime %||% node[[which]]$date
  tz <- node[[which]]$timeZone %||% tz_default
  if (is.null(dt)) return(NA_real_)
  if (grepl("T", dt, fixed = TRUE)) {
    suppressWarnings(ymd_hms(dt, quiet = TRUE, tz = tz))
  } else {
    # All-day events: interpret the date as local midnight, not UTC, to avoid shifting days
    suppressWarnings(as_datetime(ymd(dt, quiet = TRUE), tz = tz_default))
  }
}

gcal_tidy_events <- function(events_list, calendar_info) {
  if (!length(events_list)) return(tibble())
  map_dfr(events_list, function(e) {
    tibble(
      id             = e$id %||% NA_character_,
      calendar_id    = calendar_info$id,
      calendar_name  = calendar_info$summary %||% calendar_info$id,
      status         = e$status %||% NA_character_,
      subject        = e$summary %||% NA_character_,
      description    = e$description %||% NA_character_,
      location       = e$location %||% NA_character_,
      organizer      = e$organizer$email %||% NA_character_,
      attendees      = gcal_collapse_attendees(e$attendees),
      start_dt       = gcal_pick_dt(e, "start"),
      end_dt         = gcal_pick_dt(e, "end"),
      all_day        = ifelse(!is.null(e$start$date) && is.null(e$start$dateTime), TRUE, FALSE),
      created_dt     = suppressWarnings(ymd_hms(e$created %||% NA_character_, quiet = TRUE)),
      updated_dt     = suppressWarnings(ymd_hms(e$updated %||% NA_character_, quiet = TRUE)),
      html_link      = e$htmlLink %||% NA_character_,
      hangout_link   = e$hangoutLink %||% NA_character_,
      recurrence     = paste(e$recurrence %||% character(), collapse = "; "),
      transparency   = e$transparency %||% NA_character_,
      visibility     = e$visibility %||% NA_character_
    )
  }) %>%
    filter(!is.na(start_dt) & !is.na(end_dt)) %>%
    arrange(start_dt)
}

gcal_iso_utc <- function(x) {
  if (!length(x)) return(character())
  out <- rep(NA_character_, length(x))
  valid <- !is.na(x)
  if (any(valid)) {
    out[valid] <- format(with_tz(x[valid], "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  }
  out
}

gcal_db_path <- function() {
  if (exists("db_path", inherits = TRUE)) {
    path <- get("db_path", inherits = TRUE)
    if (!is.null(path) && nzchar(path)) return(path)
  }
  path <- Sys.getenv("TOGGL_DB_PATH")
  if (!nzchar(path)) path <- file.path(getwd(), "toggl_entries.sqlite")
  path.expand(path)
}

gcal_init_db_connection <- function(path = gcal_db_path()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  if (!DBI::dbIsValid(con)) stop(sprintf("Failed to open SQLite database at %s", path))
  try(DBI::dbExecute(con, "PRAGMA journal_mode = WAL;"), silent = TRUE)
  try(DBI::dbExecute(con, "PRAGMA busy_timeout = 5000;"), silent = TRUE)
  con
}

gcal_ensure_events_table <- function(con) {
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS gcal_events (
      id TEXT PRIMARY KEY,
      calendar_id TEXT,
      calendar_name TEXT,
      status TEXT,
      subject TEXT,
      description TEXT,
      location TEXT,
      organizer TEXT,
      attendees TEXT,
      start_utc TEXT,
      end_utc TEXT,
      all_day INTEGER,
      created_utc TEXT,
      updated_utc TEXT,
      html_link TEXT,
      hangout_link TEXT,
      recurrence TEXT,
      transparency TEXT,
      visibility TEXT,
      fetched_at TEXT
    );
    "
  )
}

gcal_write_events <- function(con, events_df) {
  if (!nrow(events_df)) return(invisible(0L))
  gcal_ensure_events_table(con)
  fetched_at <- gcal_iso_utc(rep(now(tzone = "UTC"), nrow(events_df)))
  payload <- events_df %>%
    mutate(
      start_utc   = gcal_iso_utc(start_dt),
      end_utc     = gcal_iso_utc(end_dt),
      created_utc = gcal_iso_utc(created_dt),
      updated_utc = gcal_iso_utc(updated_dt),
      fetched_at  = fetched_at,
      all_day     = as.integer(all_day)
    ) %>%
    select(
      id, calendar_id, calendar_name, status, subject, description, location,
      organizer, attendees, start_utc, end_utc, all_day, created_utc, updated_utc,
      html_link, hangout_link, recurrence, transparency, visibility, fetched_at
    )

  insert_stmt <- "
    INSERT INTO gcal_events (
      id, calendar_id, calendar_name, status, subject, description, location,
      organizer, attendees, start_utc, end_utc, all_day, created_utc, updated_utc,
      html_link, hangout_link, recurrence, transparency, visibility, fetched_at
    ) VALUES (
      :id, :calendar_id, :calendar_name, :status, :subject, :description, :location,
      :organizer, :attendees, :start_utc, :end_utc, :all_day, :created_utc, :updated_utc,
      :html_link, :hangout_link, :recurrence, :transparency, :visibility, :fetched_at
    )
    ON CONFLICT(id) DO UPDATE SET
      calendar_id   = excluded.calendar_id,
      calendar_name = excluded.calendar_name,
      status        = excluded.status,
      subject       = excluded.subject,
      description   = excluded.description,
      location      = excluded.location,
      organizer     = excluded.organizer,
      attendees     = excluded.attendees,
      start_utc     = excluded.start_utc,
      end_utc       = excluded.end_utc,
      all_day       = excluded.all_day,
      created_utc   = excluded.created_utc,
      updated_utc   = excluded.updated_utc,
      html_link     = excluded.html_link,
      hangout_link  = excluded.hangout_link,
      recurrence    = excluded.recurrence,
      transparency  = excluded.transparency,
      visibility    = excluded.visibility,
      fetched_at    = excluded.fetched_at;
  "
  DBI::dbWithTransaction(con, {
    stmt <- DBI::dbSendStatement(con, insert_stmt)
    tryCatch({
      DBI::dbBind(stmt, payload)
    }, finally = {
      suppressWarnings(try(DBI::dbClearResult(stmt), silent = TRUE))
    })
  })
  invisible(nrow(events_df))
}

gcal_read_events_range <- function(con, start_utc, end_utc) {
  gcal_ensure_events_table(con)
  sql <- "
    SELECT id, calendar_id, calendar_name, status, subject, description, location,
           organizer, attendees, start_utc, end_utc, all_day, created_utc, updated_utc,
           html_link, hangout_link, recurrence, transparency, visibility, fetched_at
    FROM gcal_events
    WHERE start_utc >= :start_utc AND start_utc < :end_utc
    ORDER BY start_utc ASC;
  "
  res <- DBI::dbGetQuery(con, sql, params = list(start_utc = start_utc, end_utc = end_utc))
  if (!nrow(res)) return(tibble())
  res %>%
    mutate(
      start_utc = suppressWarnings(ymd_hms(start_utc, tz = "UTC", quiet = TRUE)),
      end_utc   = suppressWarnings(ymd_hms(end_utc,   tz = "UTC", quiet = TRUE)),
      created_utc = suppressWarnings(ymd_hms(created_utc, tz = "UTC", quiet = TRUE)),
      updated_utc = suppressWarnings(ymd_hms(updated_utc, tz = "UTC", quiet = TRUE)),
      all_day = as.logical(all_day)
    )
}

gcal_event_window <- function(weeks_before, weeks_after, local_tz) {
  now_local <- now(tzone = local_tz)
  week_start_local <- floor_date(now_local, unit = "week", week_start = 7)
  list(
    start_local = week_start_local - weeks(weeks_before),
    end_local   = week_start_local + weeks(weeks_after + 1) - days(1)
  )
}

get_gcal_events <- function(weeks_before = 3L,
                            weeks_after = 3L,
                            local_tz = Sys.getenv("GCAL_LOCAL_TZ", "America/New_York"),
                            calendar_id = NULL) {
  calendar_info <- gcal_resolve_calendar()
  if (!is.null(calendar_id)) {
    calendar_info$id <- calendar_id
    calendar_info$summary <- calendar_id
  }
  window <- gcal_event_window(weeks_before, weeks_after, local_tz)
  time_min <- with_tz(window$start_local, "UTC")
  time_max <- with_tz(window$end_local + days(1), "UTC")

  tok <- gcal_token()
  events_raw <- gcal_fetch_all_events(calendar_info$id, time_min, time_max, token = tok)
  events_df <- gcal_tidy_events(events_raw, calendar_info)

  con <- gcal_init_db_connection()
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  if (nrow(events_df)) {
    gcal_write_events(con, events_df)
  }

  start_iso <- gcal_iso_utc(window$start_local)
  end_iso   <- gcal_iso_utc(window$end_local + days(1))
  gcal_read_events_range(con, start_iso, end_iso)
}
