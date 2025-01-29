#' Create a new watcher database
#'
#' Creates and initializes a SQLite database for a new file watcher
#'
#' @param config List containing watcher configuration
#' @return Path to created database
#' @keywords internal
create_watcher_database <- function(config) {
  checkmate::assert_list(config)
  checkmate::assert_string(config$id)

  # Create database path
  db_path <- fs::path(get_vigil_dir(), sprintf("watcher_%s.db", config$id))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWithTransaction(con, {
    # Create tables
    DBI::dbExecute(con, "
      CREATE TABLE config (
        key TEXT PRIMARY KEY,
        value TEXT,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )")

    DBI::dbExecute(con, "
      CREATE TABLE events (
        id INTEGER PRIMARY KEY,
        timestamp TEXT NOT NULL,
        event_type TEXT NOT NULL,
        file_path TEXT NOT NULL,
        callback_output TEXT,
        callback_timestamp TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )")

    DBI::dbExecute(con, "
      CREATE TABLE status (
        key TEXT PRIMARY KEY,
        value TEXT,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )")

    # Write configuration
    config_df <- tibble::tibble(
      key = names(config),
      value = vapply(config, as.character, character(1))
    )
    DBI::dbWriteTable(con, "config", config_df, append = TRUE)

    # Initialize status
    DBI::dbExecute(con,
                   sprintf(
                     "INSERT INTO status (key, value) VALUES ('state', %s)",
                     escape_sql_string("initializing")
                   )
    )
  })

  db_path
}

#' Read watcher configuration from database
#'
#' @param db_path Path to watcher database
#' @return List containing configuration
#' @keywords internal
read_watcher_config <- function(db_path) {
  db_path <- validate_db_path(db_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  config <- DBI::dbGetQuery(con, "SELECT key, value FROM config")
  as.list(tibble::deframe(config))
}

#' Get watcher process information
#'
#' @param db_path Path to watcher database
#' @return List containing process info (pid, state)
#' @keywords internal
get_watcher_process <- function(db_path) {
  db_path <- validate_db_path(db_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  status <- DBI::dbGetQuery(con, "SELECT key, value FROM status")
  status_list <- as.list(tibble::deframe(status))

  list(
    pid = as.integer(status_list$pid),
    state = status_list$state
  )
}

#' Get events for a watcher
#'
#' @param db_path Path to watcher database
#' @param include_output Include callback output in results
#' @return Tibble containing events
#' @keywords internal
get_watcher_events <- function(db_path, include_output = FALSE) {
  db_path <- validate_db_path(db_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  if (include_output) {
    DBI::dbGetQuery(con, "SELECT * FROM events ORDER BY timestamp")
  } else {
    DBI::dbGetQuery(con, "
      SELECT id, timestamp, event_type, file_path, callback_timestamp
      FROM events
      ORDER BY timestamp")
  }
}

#' Clean up a watcher database
#'
#' @param db_path Path to watcher database
#' @param force Whether to force cleanup of persistent watchers
#' @return Logical indicating success
#' @keywords internal
cleanup_watcher_database <- function(db_path, force = FALSE) {
  db_path <- validate_db_path(db_path)

  # Check if this is a persistent watcher
  config <- read_watcher_config(db_path)
  if (isTRUE(config$persistent) && !force) {
    return(FALSE)
  }

  # Update status
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con,
                 sprintf(
                   "INSERT OR REPLACE INTO status (key, value) VALUES ('state', %s)",
                   escape_sql_string("stopped")
                 )
  )

  # Close connection and remove file
  DBI::dbDisconnect(con)
  fs::file_delete(db_path)

  TRUE
}

#' List all watcher databases
#'
#' @param include_stopped Include stopped watchers
#' @return Character vector of database paths
#' @keywords internal
list_watcher_databases <- function(include_stopped = FALSE) {
  vigil_dir <- get_vigil_dir()
  db_files <- fs::dir_ls(vigil_dir, glob = "watcher_*.db")

  if (!include_stopped) {
    # Filter out stopped watchers
    db_files <- db_files[purrr::map_lgl(db_files, function(db) {
      process <- get_watcher_process(db)
      !is.na(process$pid) && process$state != "stopped"
    })]
  }

  db_files
}

#' Check if watcher database exists and is active
#'
#' @param id Watcher identifier
#' @return Logical indicating if watcher is active
#' @keywords internal
is_watcher_active <- function(id) {
  checkmate::assert_string(id)

  db_path <- fs::path(get_vigil_dir(), sprintf("watcher_%s.db", id))
  if (!fs::file_exists(db_path)) {
    return(FALSE)
  }

  process <- get_watcher_process(db_path)
  !is.na(process$pid) && process$state == "running"
}
