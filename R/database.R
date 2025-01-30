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
  tryCatch({
    DBI::dbWithTransaction(con, {
      # Create tables
      DBI::dbExecute(con, "
        CREATE TABLE config (
          key TEXT PRIMARY KEY,
          value TEXT,
          updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )")

      DBI::dbExecute(con, "
        CREATE TABLE active_processes (
          pid INTEGER NOT NULL,
          type TEXT NOT NULL CHECK (type IN ('watcher', 'callback')),
          event_id INTEGER,
          active BOOLEAN DEFAULT TRUE,
          start_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (pid)
        )")

      DBI::dbExecute(con, "
        CREATE TABLE events (
          id INTEGER PRIMARY KEY,
          timestamp TEXT NOT NULL,
          event_type TEXT NOT NULL,
          file_path TEXT NOT NULL,
          callback_source TEXT,
          callback_text TEXT,
          callback_messages TEXT,
          callback_warnings TEXT,
          callback_errors TEXT,
          callback_values TEXT,
          callback_plots TEXT,
          callback_timestamp TEXT,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )")

      # Write configuration
      config_df <- tibble::tibble(
        key = names(config),
        value = vapply(config, as.character, character(1))
      )
      DBI::dbWriteTable(con, "config", config_df, append = TRUE)
    })
  }, finally = {
    DBI::dbDisconnect(con)
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
  tryCatch({
    config <- DBI::dbGetQuery(con, "SELECT key, value FROM config")
    as.list(tibble::deframe(config))
  }, finally = {
    DBI::dbDisconnect(con)
  })
}

#' Get active watcher processes
#'
#' @param db_path Path to watcher database
#' @param type Optional process type filter ("watcher" or "callback")
#' @param active_only Whether to only return active processes
#' @return Tibble containing process info
#' @keywords internal
get_watcher_processes <- function(db_path, type = NULL, active_only = TRUE) {
  db_path <- validate_db_path(db_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  tryCatch({
    query <- "SELECT * FROM active_processes"

    # Add type filter if specified
    if (!is.null(type)) {
      checkmate::assert_choice(type, c("watcher", "callback"))
      query <- paste0(query, sprintf(" WHERE type = '%s'", type))

      # Add active filter if requested
      if (active_only) {
        query <- paste0(query, " AND active = TRUE")
      }
    } else if (active_only) {
      query <- paste0(query, " WHERE active = TRUE")
    }

    query <- paste0(query, " ORDER BY start_time")

    DBI::dbGetQuery(con, query)
  }, finally = {
    DBI::dbDisconnect(con)
  })
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
  tryCatch({
    query <- if (include_output) {
      "SELECT * FROM events ORDER BY timestamp"
    } else {
      "SELECT id, timestamp, event_type, file_path, callback_timestamp
       FROM events
       ORDER BY timestamp"
    }

    DBI::dbGetQuery(con, query)
  }, finally = {
    DBI::dbDisconnect(con)
  })
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

  # Try to delete the database file with one retry
  tryCatch({
    fs::file_delete(db_path)
    TRUE
  }, error = function(e) {
    # Wait and retry once
    Sys.sleep(0.5)
    tryCatch({
      fs::file_delete(db_path)
      TRUE
    }, error = function(e) {
      cli::cli_warn("Failed to delete database file: {db_path}")
      FALSE
    })
  })
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
    # Filter out databases with no active watcher process
    db_files <- db_files[purrr::map_lgl(db_files, function(db) {
      processes <- get_watcher_processes(db, type = "watcher")
      nrow(processes) > 0
    })]
  }

  db_files
}

#' Mark process as inactive in database
#'
#' @param db_path Path to watcher database
#' @param pid Process ID to mark as inactive
#' @return Logical indicating success
#' @keywords internal
mark_process_inactive <- function(db_path, pid) {
  db_path <- validate_db_path(db_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE active_processes SET active = FALSE WHERE pid = ?",
      params = list(pid)
    ) > 0
  }, finally = {
    DBI::dbDisconnect(con)
  })
}

#' Clean up inactive process records
#'
#' @param db_path Path to watcher database
#' @return Number of records cleaned up
#' @keywords internal
cleanup_inactive_processes <- function(db_path) {
  db_path <- validate_db_path(db_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  tryCatch({
    DBI::dbExecute(con, "DELETE FROM active_processes WHERE active = FALSE")
  }, finally = {
    DBI::dbDisconnect(con)
  })
}
