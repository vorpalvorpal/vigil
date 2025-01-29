#' Validate callback specifications
#'
#' @param callback Function, expression, or path to R script
#' @param db_path Path to watcher database for storing callback info
#' @return List containing validated callback info
#' @keywords internal
validate_callback <- function(callback, db_path = NULL) {
  # Handle NULL callback
  if (is.null(callback)) {
    return(list(
      type = "none",
      content = NULL
    ))
  }

  # Get callback content based on type
  callback_content <- if (is.function(callback) || is.language(callback)) {
    # Convert function or expression to string
    deparse(callback, width.cutoff = 500L)
  } else if (is.character(callback) && length(callback) == 1) {
    # Read R script content
    if (!fs::file_exists(callback)) {
      cli::cli_abort("Callback script not found: {callback}")
    }
    if (fs::path_ext(callback) != "R") {
      cli::cli_abort("Callback must be an R script")
    }

    tryCatch({
      content <- readLines(callback)
      cli::cli_alert_warning(c(
        "Script content will be copied to database.",
        "i" = "Changes to original file won't affect the watcher."
      ))
      paste(content, collapse = "\n")
    }, error = function(e) {
      cli::cli_abort(c(
        "Failed to read callback script",
        "x" = e$message
      ))
    })
  } else {
    cli::cli_abort(c(
      "Invalid callback specification",
      "i" = "Must be one of:",
      "*" = "R function or expression",
      "*" = "Path to R script"
    ))
  }

  # Store in database if provided
  if (!is.null(db_path)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    tryCatch({
      DBI::dbWithTransaction(con, {
        DBI::dbExecute(
          con,
          sprintf(
            "INSERT OR REPLACE INTO config (key, value) VALUES ('callback_content', %s)",
            escape_sql_string(callback_content)
          )
        )
      })
    }, error = function(e) {
      cli::cli_abort(c(
        "Failed to store callback in database",
        "x" = e$message
      ))
    }, finally = {
      DBI::dbDisconnect(con)
    })
  }

  list(
    type = if (is.character(callback)) "script" else "expression",
    content = callback_content
  )
}

#' Format timestamp for SQLite storage
#'
#' @param time POSIXt time object (defaults to current time)
#' @return Character string in SQLite timestamp format
#' @keywords internal
format_sql_timestamp <- function(time = Sys.time()) {
  format(time, "%Y-%m-%d %H:%M:%S")
}

#' Validate SQLite database path
#'
#' @param db_path Path to check
#' @param must_exist Whether path must exist
#' @return Normalized path if valid
#' @keywords internal
validate_db_path <- function(db_path, must_exist = TRUE) {
  checkmate::assert_string(db_path)

  # Check extension
  if (fs::path_ext(db_path) != "db") {
    cli::cli_abort("Invalid database extension: must be .db")
  }

  # Normalize path
  db_path <- fs::path_norm(db_path)

  if (must_exist) {
    if (!fs::file_exists(db_path)) {
      cli::cli_abort("Database not found: {db_path}")
    }

    if (!fs::file_access(db_path, mode = "write")) {
      cli::cli_abort("Database is not writable: {db_path}")
    }
  }

  db_path
}

#' Extract watcher ID from database path
#'
#' @param db_path Database path
#' @return Watcher ID string
#' @keywords internal
parse_watcher_id <- function(db_path) {
  checkmate::assert_string(db_path)

  id <- sub("^watcher_(.*)\\.db$", "\\1", basename(db_path))

  if (id == basename(db_path)) {
    cli::cli_abort("Invalid database filename format")
  }

  # Verify ID format (should be UUID)
  if (!grepl("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$",
             id, ignore.case = TRUE)) {
    cli::cli_abort("Invalid watcher ID format")
  }

  id
}

#' Safely escape string for SQL
#'
#' @param str String to escape
#' @return Escaped string safe for SQL
#' @keywords internal
escape_sql_string <- function(str) {
  if (is.null(str)) {
    return("NULL")
  }

  # Handle special cases
  if (identical(str, "")) {
    return("''")
  }

  # Escape single quotes and wrap
  sprintf("'%s'", gsub("'", "''", as.character(str)))
}

#' Get package script path
#'
#' @param script_name Name of script to find
#' @return Absolute path to script
#' @keywords internal
get_script_path <- function(script_name) {
  script_path <- fs::path(
    system.file(package = "vigil"),
    script_name
  )

  if (!fs::file_exists(script_path)) {
    cli::cli_abort("Script not found: {script_name}")
  }

  # Ensure proper permissions on Unix
  if (.Platform$OS.type != "windows") {
    fs::file_chmod(script_path, "0755")
  }

  script_path
}

#' Null coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
