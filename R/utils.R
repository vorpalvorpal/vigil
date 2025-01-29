#' Validate and normalize callback specifications
#'
#' @param callback Function, expression, package function name, or path to R script
#' @param db_path Path to watcher database for storing callback info
#' @return List containing validated callback info
#' @keywords internal
validate_callback <- function(callback, db_path = NULL) {
  # Handle NULL callback
  if (is.null(callback)) {
    return(list(
      type = "none",
      value = NULL
    ))
  }

  # Handle package function specification (e.g. "dplyr::select")
  if (is.character(callback) && length(callback) == 1) {
    if (grepl("::", callback, fixed = TRUE)) {
      parts <- strsplit(callback, "::")[[1]]
      if (length(parts) == 2) {
        if (requireNamespace(parts[1], quietly = TRUE) &&
            exists(parts[2], envir = asNamespace(parts[1]))) {

          # Store in database if provided
          if (!is.null(db_path)) {
            store_callback_info(db_path, "package", callback)
          }

          return(list(
            type = "package",
            value = callback
          ))
        }
      }
      cli::cli_abort("Package function not found: {callback}")
    }

    # Check for R script path
    if (fs::file_exists(callback) && fs::path_ext(callback) == "R") {
      script_path <- fs::path_abs(callback)

      # Verify script is readable
      if (!fs::file_access(script_path, mode = "read")) {
        cli::cli_abort("Callback script is not readable: {script_path}")
      }

      # Store in database if provided
      if (!is.null(db_path)) {
        store_callback_info(db_path, "script", script_path)
      }

      return(list(
        type = "script",
        value = script_path
      ))
    }
  }

  # Handle function or expression
  if (is.function(callback) || is.language(callback)) {
    # Store in database if provided
    if (!is.null(db_path)) {
      store_callback_info(db_path, "expression",
                          deparse(callback, width.cutoff = 500L))
    }

    return(list(
      type = "expression",
      value = callback
    ))
  }

  # Invalid callback
  cli::cli_abort(c(
    "Invalid callback specification",
    "i" = "Must be one of:",
    "*" = "Package function (e.g. 'dplyr::select')",
    "*" = "Path to R script",
    "*" = "Function or expression"
  ))
}

#' Store callback information in database
#' @keywords internal
store_callback_info <- function(db_path, type, value) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(
    con,
    sprintf(
      "INSERT OR REPLACE INTO config (key, value) VALUES ('callback_type', %s)",
      escape_sql_string(type)
    )
  )

  DBI::dbExecute(
    con,
    sprintf(
      "INSERT OR REPLACE INTO config (key, value) VALUES ('callback_value', %s)",
      escape_sql_string(value)
    )
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
    # Verify existence and permissions
    if (!fs::file_exists(db_path)) {
      cli::cli_abort("Database not found: {db_path}")
    }

    if (!fs::file_access(db_path, mode = "write")) {
      cli::cli_abort("Database is not writable: {db_path}")
    }

    # Basic structure check
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))

    required_tables <- c("config", "status", "events")
    existing_tables <- DBI::dbListTables(con)

    missing_tables <- setdiff(required_tables, existing_tables)
    if (length(missing_tables) > 0) {
      cli::cli_abort(c(
        "Invalid database structure",
        "x" = "Missing tables: {toString(missing_tables)}"
      ))
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

#' Prepare environment for callback execution
#'
#' @param db_path Database path
#' @param event_id Event ID
#' @return Environment ready for callback
#' @keywords internal
prepare_callback_env <- function(db_path, event_id) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Get event data
  event <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM events WHERE id = %d",
      as.integer(event_id)
    )
  )

  if (nrow(event) == 0) {
    cli::cli_abort("Event not found: {event_id}")
  }

  # Create clean environment
  env <- new.env(parent = baseenv())

  # Add event data
  env$event <- as.list(event)

  # Add helper functions
  env$format_sql_timestamp <- format_sql_timestamp

  env
}

#' Null coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
