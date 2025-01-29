#' Watch directory for file changes
#'
#' @param path Directory path to watch
#' @param file_pattern Optional file pattern to match (regex)
#' @param recursive Whether to watch subdirectories
#' @param callback Function or path to R script that will be called on file changes.
#'   The callback will receive an event list containing:
#'     - id: Event identifier
#'     - timestamp: When the event occurred
#'     - event_type: "created", "modified", or "deleted"
#'     - file_path: Full path to changed file
#' @param watch_mode The watching mode:
#'   * "continuous" - Watch until explicitly stopped
#'   * "single" - Stop after first event
#'   * "persistent" - Continue watching after R session ends
#' @param change_type Type of changes to watch for:
#'   * "any" - All changes
#'   * "created" - File creation only
#'   * "modified" - File modification only
#'   * "deleted" - File deletion only
#' @return Invisibly returns the watcher ID
#' @export
watch <- function(path,
                  file_pattern = NULL,
                  recursive = FALSE,
                  callback = NULL,
                  watch_mode = "continuous",
                  change_type = "any") {

  # Check platform requirements
  check_platform_requirements()
  if (watch_mode == "persistent") {
    check_persistent_requirements()
  }

  # Basic input validation
  checkmate::assert_directory_exists(path)
  if (!is.null(file_pattern)) {
    checkmate::assert_string(file_pattern)
    # Verify it's a valid regex
    tryCatch(
      grepl(file_pattern, "test string"),
      error = function(e) cli::cli_abort("Invalid regular expression file_pattern")
    )
  }
  checkmate::assert_flag(recursive)
  checkmate::assert_choice(watch_mode, c("single", "continuous", "persistent"))
  checkmate::assert_choice(change_type, c("created", "modified", "deleted", "any"))

  # Create watcher config
  config <- create_watcher_config(
    path = path,
    file_pattern = file_pattern,
    recursive = recursive,
    callback = callback,
    watch_mode = watch_mode,
    change_type = change_type
  )

  # Start watcher process
  pid <- start_watcher(config)

  # Build descriptive messages
  change_msg <- switch(change_type,
                       "any" = "all file changes",
                       "created" = "file creation",
                       "modified" = "file modification",
                       "deleted" = "file deletion"
  )

  duration_msg <- switch(watch_mode,
                         "single" = "until first event",
                         "persistent" = "until explicitly stopped",
                         "continuous" = "until this R session ends"
  )

  cli::cli_alert_success(c(
    "Started watching for {change_msg} at {.path {path}}.",
    "i" = "Watcher will continue {duration_msg}."
  ))

  invisible(config$id)
}

#' List active file watchers
#'
#' Returns information about all currently active file watchers.
#'
#' @return Tibble with columns:
#'   * id - Watcher identifier
#'   * path - Directory being watched
#'   * file_pattern - File pattern (NA if none)
#'   * recursive - Whether watching subdirectories
#'   * persistent - Whether watcher is persistent
#'   * created - When watcher was created
#' @export
list_watchers <- function() {
  # List active databases
  databases <- list_watcher_databases(include_stopped = FALSE)

  if (length(databases) == 0) {
    return(tibble::tibble(
      id = character(),
      path = character(),
      file_pattern = character(),
      recursive = logical(),
      persistent = logical(),
      created = character()
    ))
  }

  # Extract info from each database
  purrr::map_dfr(databases, function(db_path) {
    config <- read_watcher_config(db_path)

    tibble::tibble(
      id = config$id,
      path = config$path,
      file_pattern = if (is.null(config$file_pattern)) NA_character_ else config$file_pattern,
      recursive = as.logical(config$recursive),
      persistent = as.logical(config$persistent),
      created = config$created
    )
  })
}

#' Watch until specific file changes occur
#'
#' Sets up a temporary file watcher that runs until specified changes occur or
#' timeout is reached.
#'
#' @param path Directory path to watch
#' @param file_pattern Optional file pattern to match (regex)
#' @param change_type Type of change to watch for
#' @param timeout Timeout in seconds (NULL means wait indefinitely)
#' @return Tibble with event details or NULL if timeout
#' @export
watch_until <- function(path,
                        file_pattern = NULL,
                        change_type = "any",
                        timeout = NULL) {

  checkmate::assert_directory_exists(path)
  if (!is.null(file_pattern)) {
    checkmate::assert_string(file_pattern)
    tryCatch(
      grepl(file_pattern, "test string"),
      error = function(e) cli::cli_abort("Invalid regular expression file_pattern")
    )
  }
  checkmate::assert_choice(change_type, c("created", "modified", "deleted", "any"))
  checkmate::assert_number(timeout, null.ok = TRUE, lower = 0)

  # Create watcher config with wait_for_event flag
  config <- create_watcher_config(
    path = path,
    file_pattern = file_pattern,
    recursive = FALSE,
    watch_mode = "single",
    change_type = change_type,
    wait_for_event = TRUE,
    timeout = timeout
  )

  tryCatch({
    # Start watcher and wait for event
    db_path <- start_watcher(config)

    # Return events (guaranteed to exist by launcher)
    get_watcher_events(db_path)

  }, finally = {
    if (!is.null(db_path)) {
      cleanup_watcher_database(db_path, force = !config$persistent)
    }
  }, error = function(e) {
    if (!is.null(config$id)) {
      cleanup_watcher_database(
        fs::path(get_vigil_dir(), sprintf("watcher_%s.db", config$id))
      )
    }
    if (grepl("Timed out", e$message)) {
      return(NULL)
    }
    cli::cli_abort(c(
      "Error waiting for events",
      "x" = e$message
    ))
  })
}

#' Kill a file watcher
#'
#' @param id Watcher ID to kill
#' @param timeout Timeout in seconds for kill operation
#' @return Logical indicating if watcher was successfully killed
#' @export
kill_watcher <- function(id, timeout = 5) {
  checkmate::assert_string(id)
  checkmate::assert_number(timeout, lower = 0)

  db_path <- fs::path(get_vigil_dir(), sprintf("watcher_%s.db", id))

  if (!fs::file_exists(db_path)) {
    cli::cli_alert_warning("Watcher {.val {id}} not found")
    return(FALSE)
  }

  success <- kill_watcher_process(db_path, timeout)
  if (success) {
    cli::cli_alert_success("Killed watcher {.val {id}}")
  } else {
    cli::cli_alert_warning("Failed to kill watcher {.val {id}}")
  }

  success
}

#' Kill all file watchers
#'
#' @param timeout Timeout in seconds for each kill operation
#' @return Logical vector indicating success/failure for each watcher
#' @export
kill_all_watchers <- function(timeout = 5) {
  checkmate::assert_number(timeout, lower = 0)

  # Get all active watchers
  watchers <- list_watchers()
  if (nrow(watchers) == 0) {
    cli::cli_alert_info("No active watchers found")
    return(invisible(logical(0)))
  }

  # Kill each watcher
  results <- purrr::map_lgl(watchers$id, ~ kill_watcher(.x, timeout))

  successful <- sum(results)
  total <- length(results)

  if (successful == total) {
    cli::cli_alert_success("Successfully killed all {total} watchers")
  } else {
    cli::cli_alert_warning("Killed {successful} out of {total} watchers")
  }

  invisible(results)
}

#' Create standardized watcher configuration
#' @keywords internal
create_watcher_config <- function(path,
                                  file_pattern = NULL,
                                  recursive = FALSE,
                                  callback = NULL,
                                  watch_mode = "continuous",
                                  change_type = "any",
                                  wait_for_event = FALSE,
                                  timeout = NULL) {
  # Generate unique ID
  id <- uuid::UUIDgenerate()
  # Create temporary database to validate callback
  db_path <- fs::path(get_vigil_dir(), sprintf("watcher_%s.db", id))
  # Process callback with database storage
  validated_callback <- validate_callback(callback, db_path)
  # Build config
  list(
    id = id,
    path = fs::path_norm(path),
    file_pattern = file_pattern,
    recursive = recursive,
    watch_mode = watch_mode,
    change_type = change_type,
    created = format_sql_timestamp(),
    persistent = watch_mode == "persistent",
    wait_for_event = wait_for_event,
    timeout = timeout
  )
}
