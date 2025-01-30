#' Kill a watcher and optionally save its output
#'
#' @param id Watcher identifier
#' @param kill_callbacks Whether to also kill callback processes
#' @param save_output Whether to save watcher output before cleanup
#' @param output_name Optional name for saved output
#' @param timeout Timeout in seconds for kill operations
#' @return Logical indicating if the operation was successful
#' @export
kill_watcher <- function(id,
                         kill_callbacks = TRUE,
                         save_output = FALSE,
                         output_name = NULL,
                         timeout = 5) {

  checkmate::assert_string(id)
  checkmate::assert_flag(kill_callbacks)
  checkmate::assert_flag(save_output)
  checkmate::assert_string(output_name, null.ok = TRUE)
  checkmate::assert_number(timeout, lower = 0)

  db_path <- fs::path(get_vigil_dir(), sprintf("watcher_%s.db", id))

  if (!fs::file_exists(db_path)) {
    cli::cli_alert_warning("Watcher {.val {id}} not found")
    return(FALSE)
  }

  tryCatch({
    # Get process information
    processes <- get_watcher_processes(db_path)
    if (nrow(processes) == 0) {
      cli::cli_alert_warning("No active processes found for watcher {.val {id}}")
      cleanup_watcher_database(db_path)
      return(FALSE)
    }

    # Separate watcher and callback processes
    watcher_procs <- processes[processes$type == "watcher", ]
    callback_procs <- processes[processes$type == "callback", ]

    success <- TRUE

    # Handle callback processes first if requested
    if (kill_callbacks && nrow(callback_procs) > 0) {
      active_callbacks <- callback_procs$pid[purrr::map_lgl(callback_procs$pid,
                                                            function(pid) verify_process_alive(pid))]

      if (length(active_callbacks) > 0) {
        success <- success && kill_processes(active_callbacks, timeout)
        if (!success) {
          cli::cli_alert_warning("Some callback processes could not be killed")
        }
      }
    }

    # Handle watcher processes
    active_watchers <- watcher_procs$pid[purrr::map_lgl(watcher_procs$pid,
                                                        function(pid) verify_process_alive(pid))]

    if (length(active_watchers) > 0) {
      success <- success && kill_processes(active_watchers, timeout)
      if (!success) {
        cli::cli_alert_warning("Some watcher processes could not be killed")
      }
    }

    # Save output if requested
    if (save_output) {
      save_watcher_output(db_path, output_name)
    }

    # Only cleanup database if all processes are dead or if kill_callbacks is TRUE
    if (!kill_callbacks) {
      # Check if any callbacks are still running
      remaining_callbacks <- callback_procs$pid[purrr::map_lgl(callback_procs$pid,
                                                               function(pid) verify_process_alive(pid))]

      if (length(remaining_callbacks) > 0) {
        cli::cli_alert_info(
          "Database retained as {length(remaining_callbacks)} callback processes still running"
        )
        return(success)
      }
    }

    cleanup_watcher_database(db_path)
    success

  }, error = function(e) {
    cli::cli_alert_danger("Error during watcher cleanup: {e$message}")
    FALSE
  })
}

#' Kill all file watchers
#'
#' @param kill_callbacks Whether to also kill callback processes
#' @param save_outputs Whether to save watcher outputs before cleanup
#' @param output_prefix Optional prefix for saved outputs
#' @param timeout Timeout in seconds for each kill operation
#' @return Named logical vector indicating success/failure for each watcher
#' @export
kill_all_watchers <- function(kill_callbacks = TRUE,
                              save_outputs = FALSE,
                              output_prefix = NULL,
                              timeout = 5) {

  checkmate::assert_flag(kill_callbacks)
  checkmate::assert_flag(save_outputs)
  checkmate::assert_string(output_prefix, null.ok = TRUE)
  checkmate::assert_number(timeout, lower = 0)

  # Get all databases with active watchers
  db_files <- list_watcher_databases(include_stopped = FALSE)

  if (length(db_files) == 0) {
    cli::cli_alert_info("No active watchers found")
    return(invisible(logical(0)))
  }

  # Extract IDs from database paths
  watcher_ids <- purrr::map_chr(db_files, parse_watcher_id)
  names(watcher_ids) <- watcher_ids

  # Kill each watcher
  results <- purrr::map_lgl(watcher_ids, function(id) {
    output_name <- if (save_outputs) {
      if (!is.null(output_prefix)) {
        paste0(output_prefix, "_", id)
      } else {
        id
      }
    } else {
      NULL
    }

    cli::cli_alert_info("Killing watcher {.val {id}}")
    kill_watcher(
      id = id,
      kill_callbacks = kill_callbacks,
      save_output = save_outputs,
      output_name = output_name,
      timeout = timeout
    )
  })

  # Summarize results
  successful <- sum(results)
  total <- length(results)

  if (successful == total) {
    cli::cli_alert_success("Successfully killed all {total} watchers")
  } else {
    cli::cli_alert_warning(
      "Killed {successful} out of {total} watchers. Run with higher timeout or check permissions."
    )
  }

  invisible(results)
}

#' Check if a process is alive
#'
#' @param pid Process ID to check
#' @return Logical indicating if process is running
#' @keywords internal
verify_process_alive <- function(pid) {
  checkmate::assert_integerish(pid, len = 1)

  if (.Platform$OS.type == "windows") {
    cmd <- "tasklist"
    args <- c("/FI", sprintf("PID eq %d", pid))
    result <- system2(cmd, args, stdout = TRUE, stderr = TRUE)
    grepl(as.character(pid), paste(result, collapse = "\n"))
  } else {
    # On Unix, kill -0 checks process existence without sending a signal
    system2("kill", c("-0", pid), stdout = FALSE, stderr = FALSE) == 0
  }
}

#' Kill processes with escalating force
#'
#' @param pids Vector of process IDs to kill
#' @param timeout Timeout in seconds
#' @return Logical indicating if all processes were killed
#' @keywords internal
kill_processes <- function(pids, timeout = 5) {
  checkmate::assert_integerish(pids, min.len = 1)
  checkmate::assert_number(timeout, lower = 0)

  if (.Platform$OS.type == "windows") {
    # On Windows, use taskkill
    success <- system2("taskkill", c("/PID", pids), stdout = FALSE, stderr = FALSE) == 0

    if (!success) {
      Sys.sleep(min(1, timeout))
      # Try force kill
      success <- system2("taskkill", c("/F", "/PID", pids),
                         stdout = FALSE, stderr = FALSE) == 0
    }

  } else {
    # On Unix, try SIGTERM first
    system2("kill", as.character(pids), stdout = FALSE, stderr = FALSE)

    # Wait briefly for clean exit
    Sys.sleep(min(1, timeout))

    # Check which processes are still alive
    remaining <- pids[purrr::map_lgl(pids, verify_process_alive)]

    if (length(remaining) > 0) {
      # Force kill remaining processes
      success <- system2("kill", c("-9", remaining),
                         stdout = FALSE, stderr = FALSE) == 0
    } else {
      success <- TRUE
    }
  }

  # Verify all processes are dead
  Sys.sleep(0.1) # Brief pause to let system catch up
  !any(purrr::map_lgl(pids, verify_process_alive))
}

#' Save watcher output to a file
#'
#' @param db_path Path to watcher database
#' @param output_name Optional name for output file
#' @return Path to saved output file
#' @keywords internal
save_watcher_output <- function(db_path, output_name = NULL) {
  checkmate::assert_file_exists(db_path)
  checkmate::assert_string(output_name, null.ok = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  tryCatch({
    # Get all events with their outputs
    events <- DBI::dbGetQuery(con, "SELECT * FROM events")

    # Process serialized data
    events <- dplyr::mutate(
      events,
      dplyr::across(
        dplyr::matches("callback_(values|plots)"),
        ~purrr::map(., function(x) {
          if (!is.na(x) && nchar(x) > 0) {
            tryCatch(
              qs::qdeserialize(qs::base85_decode(x)),
              error = function(e) NULL
            )
          } else {
            NULL
          }
        })
      )
    )

    # Generate output name if not provided
    if (is.null(output_name)) {
      output_name <- format(Sys.time(), "vigil_output_%Y%m%d_%H%M%S")
    }

    # Ensure output directory exists
    output_dir <- fs::path(get_vigil_dir(), "outputs")
    fs::dir_create(output_dir, recurse = TRUE)

    # Save as qs file
    output_path <- fs::path(output_dir, paste0(output_name, ".qs"))
    qs::qsave(events, output_path)

    cli::cli_alert_success("Saved watcher output to {.file {output_path}}")
    invisible(output_path)

  }, finally = {
    DBI::dbDisconnect(con)
  })
}
