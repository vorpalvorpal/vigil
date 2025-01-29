#' Start a file watcher process
#'
#' @param config List containing watcher configuration
#' @return Process ID or database path depending on configuration
#' @keywords internal
start_watcher <- function(config) {
  checkmate::assert_list(config)
  checkmate::assert_string(config$id)
  checkmate::assert_directory_exists(config$path)

  # Pre-process file_pattern based on platform
  if (!is.null(config$file_pattern)) {
    config$file_pattern <- if (.Platform$OS.type == "windows") {
      convert_regex_to_vbs(config$file_pattern)
    } else {
      escape_file_pattern(config$file_pattern)
    }
  }

  # Create and initialize database
  db_path <- create_watcher_database(config)

  # Get appropriate scripts for platform
  launcher_script <- get_script_path(
    if (.Platform$OS.type == "windows") "launcher.vbs" else "launcher.sh"
  )

  watcher_script <- get_script_path(
    if (.Platform$OS.type == "windows") "watch_files.vbs" else "watch_files.sh"
  )

  # Run launcher and handle results
  args <- c(db_path, watcher_script)
  if (isTRUE(config$wait_for_event)) {
    args <- c(args, "--wait-for-event")
  }

  result <- sys::exec_wait(
    launcher_script,
    args = args,
    std_out = TRUE,
    std_err = TRUE,
    timeout = if (isTRUE(config$wait_for_event)) config$timeout else 15
  )

  # Handle different exit codes
  if (result == 2) {
    # Event written - return path for event retrieval
    return(db_path)
  } else if (result != 0) {
    cleanup_watcher_database(db_path)
    cli::cli_abort(c(
      "Failed to start file watcher",
      "x" = if (isTRUE(config$wait_for_event)) {
        "Timed out waiting for event"
      } else {
        "Watcher process did not initialize"
      }
    ))
  }

  # Normal startup - return PID
  as.integer(result$stdout)
}

#' Convert R regex to VBScript compatible pattern
#'
#' @param file_pattern Character string containing R regular expression
#' @return Character string containing VBScript compatible regular expression
#' @note VBScript has limited regex support. Some R regex features may be removed
#'       or simplified.
#' @keywords internal
convert_regex_to_vbs <- function(file_pattern) {
  checkmate::assert_character(file_pattern, len = 1, null.ok = TRUE)

  if (is.null(file_pattern)) {
    return(".*")  # Default file_pattern
  }

  # Verify input is a valid R regex
  tryCatch(
    grepl(file_pattern, "test string"),
    error = function(e) cli::cli_abort("Invalid regular expression pattern")
  )

  # VBScript specific transformations
  vbs_pattern <- file_pattern |>
    # Convert shorthand character classes
    gsub("\\\\d", "[0-9]", x = _) |>
    gsub("\\\\w", "[A-Za-z0-9_]", x = _) |>
    gsub("\\\\s", "[ \\t\\n\\r]", x = _) |>
    # Remove unsupported lookarounds
    gsub("\\(\\?=[^)]*\\)", "", x = _) |>
    gsub("\\(\\?<=[^)]*\\)", "", x = _) |>
    gsub("\\(\\?![^)]*\\)", "", x = _) |>
    gsub("\\(\\?<![^)]*\\)", "", x = _) |>
    # Convert named groups to regular groups
    gsub("\\(\\?<[^>]+>", "(", x = _)

  if (grepl("\\P{\\w+}|\\p{\\w+}", vbs_pattern)) {
    cli::cli_abort("Unicode character classes are not supported in VBScript regex")
  }

  vbs_pattern
}

#' Escape pattern for shell commands
#'
#' @param file_pattern Character string containing regex pattern
#' @return Character string with shell metacharacters escaped
#' @keywords internal
escape_file_pattern <- function(file_pattern) {
  checkmate::assert_character(file_pattern, len = 1, null.ok = TRUE)

  if (is.null(file_pattern)) {
    return(NULL)
  }

  # Escape single quotes for shell safety (replace ' with '\''')
  gsub("'", "'\\''", file_pattern, fixed = TRUE)
}
