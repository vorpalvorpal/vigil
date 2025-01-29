#' Get Vigil User Directory
#'
#' Returns the platform-specific directory where vigil stores its databases and
#' configuration.
#'
#' @return Character string containing the absolute path to the vigil directory
#' @details
#' On Windows: %LOCALAPPDATA%/R/vigil
#' On Unix: ~/.local/share/R/vigil
#' @keywords internal
get_vigil_dir <- function() {
  # Determine base directory
  if (.Platform$OS.type == "windows") {
    base <- Sys.getenv("LOCALAPPDATA", unset = NA)
    if (is.na(base)) {
      cli::cli_abort(c(
        "Could not determine AppData directory",
        "i" = "LOCALAPPDATA environment variable not set",
        "i" = "Ensure Windows environment variables are properly configured"
      ))
    }
  } else {
    base <- "~/.local/share"
  }

  # Create vigil path
  path <- fs::path(base, "R", "vigil")

  # Create directory with error handling
  tryCatch({
    fs::dir_create(path)
  }, error = function(e) {
    cli::cli_abort(c(
      "Could not create vigil directory",
      "x" = e$message,
      "i" = "Check if you have write permissions for {.path {fs::path_dir(path)}}",
      "i" = "Try running with elevated privileges or changing the directory location"
    ))
  })

  # Verify directory is writable
  if (!fs::file_access(path, mode = "write")) {
    cli::cli_abort(c(
      "Vigil directory is not writable",
      "x" = "Cannot write to {.path {path}}",
      "i" = "Check directory permissions",
      "i" = "Current user: {Sys.info()['user']}"
    ))
  }

  # Run database maintenance
  cleanup_old_databases(path)

  fs::path_norm(path)
}

#' Clean up old databases and orphaned resources
#'
#' @param dir Directory to clean
#' @param max_age Maximum age in days before cleanup
#' @keywords internal
cleanup_old_databases <- function(dir, max_age = 7) {
  # Get database files
  db_files <- fs::dir_ls(dir, glob = "watcher_*.db")

  if (length(db_files) == 0) {
    return(invisible())
  }

  # Find old databases
  old_files <- fs::file_info(db_files) |>
    dplyr::filter(
      modification_time < (Sys.time() - max_age * 24 * 60 * 60)
    )

  if (nrow(old_files) > 0) {
    purrr::walk(old_files$path, function(db_path) {
      tryCatch({
        # Check if database belongs to active watcher
        con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        tryCatch({
          status <- DBI::dbGetQuery(
            con,
            "SELECT value FROM status WHERE key IN ('state', 'persistent')"
          )

          # Only clean up if inactive and non-persistent
          if (nrow(status) > 0 &&
              !identical(status$value[status$key == "state"], "running") &&
              !identical(status$value[status$key == "persistent"], "true")) {

            DBI::dbDisconnect(con)
            fs::file_delete(db_path)

            # Clean up associated log files
            id <- sub("watcher_(.*)\\.db$", "\\1", basename(db_path))
            log_files <- fs::dir_ls(
              dir,
              glob = sprintf("*_%s.log", id)
            )
            fs::file_delete(log_files)
          }
        }, finally = {
          DBI::dbDisconnect(con)
        })
      }, error = function(e) {
        cli::cli_warn(c(
          "Could not process database for cleanup",
          "!" = "File: {.path {db_path}}",
          "x" = e$message
        ))
      })
    })
  }
}

#' Verify if a process is a valid vigil watcher
#'
#' @param db_path Path to watcher database
#' @return Boolean indicating if watcher is valid and running
#' @keywords internal
verify_watcher_process <- function(db_path) {
  checkmate::assert_file_exists(db_path)
  id <- parse_watcher_id(db_path)

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))

    # Get watcher status
    status <- DBI::dbGetQuery(
      con,
      "SELECT key, value FROM status WHERE key IN ('pid', 'state', 'persistent')"
    )
    status_list <- as.list(tibble::deframe(status))

    # Safely extract and validate PID
    pid_str <- status_list$pid
    if (is.null(pid_str) || is.na(pid_str)) {
      return(FALSE)
    }

    pid <- suppressWarnings(as.integer(pid_str))
    if (is.na(pid) || pid <= 0) {
      cli::cli_warn("Invalid process ID found in database: {pid_str}")
      return(FALSE)
    }

    # Check if watcher is persistent
    if (identical(status_list$persistent, "true")) {
      if (.Platform$OS.type == "windows") {
        verify_persistent_windows(id)
      } else if (Sys.info()["sysname"] == "Darwin") {
        verify_persistent_macos(id)
      } else {
        verify_persistent_linux(id)
      }
    } else {
      # Check regular process
      if (status_list$state != "running") {
        return(FALSE)
      }

      # Platform-specific process verification
      if (.Platform$OS.type == "windows") {
        verify_windows_process(pid, id)
      } else {
        verify_unix_process(pid, id)
      }
    }
  }, error = function(e) {
    cli::cli_warn(c(
      "Error verifying watcher process",
      "x" = e$message
    ))
    FALSE
  })
}

#' Verify Windows process is a valid watcher
#'
#' @param pid Process ID to verify
#' @param id Watcher ID to check for
#' @return Logical indicating if process is valid watcher
#' @keywords internal
verify_windows_process <- function(pid, id) {
  # Get process command line using wmic
  cmd_info <- tryCatch(
    system2("wmic",
            c("process", "where", sprintf("ProcessId=%d", pid),
              "get", "CommandLine", "/format:csv"),
            stdout = TRUE,
            stderr = TRUE),
    error = function(e) character(0)
  )

  # Check if it's our watcher script
  if (length(cmd_info) < 2 ||
      !any(grepl("watch-files.vbs", cmd_info, fixed = TRUE)) ||
      !any(grepl(id, cmd_info, fixed = TRUE))) {
    return(FALSE)
  }

  TRUE
}

#' Verify Unix process is a valid watcher
#'
#' @param pid Process ID to verify
#' @param id Watcher ID to check for
#' @return Logical indicating if process is valid watcher
#' @keywords internal
verify_unix_process <- function(pid, id) {
  # Get process command line
  cmd_info <- tryCatch(
    system2("ps", c("-p", pid, "-o", "args="), stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )

  # Check if it's our watcher script
  if (length(cmd_info) == 0 ||
      !any(grepl("watch-files.sh", cmd_info, fixed = TRUE)) ||
      !any(grepl(id, cmd_info, fixed = TRUE))) {
    return(FALSE)
  }

  TRUE
}

#' Kill a Windows process and its children
#'
#' @param pid Process ID to kill
#' @param timeout Timeout in seconds
#' @return Boolean indicating success
#' @keywords internal
kill_windows_process <- function(pid, timeout) {
  checkmate::assert_integerish(pid, len = 1, any.missing = FALSE, lower = 1)
  checkmate::assert_number(timeout, lower = 0)

  tryCatch({
    # Get child processes using wmic
    child_pids <- tryCatch({
      cmd_info <- system2("wmic",
                          c("process", "where", sprintf("ParentProcessId=%d", pid),
                            "get", "ProcessId", "/format:csv"),
                          stdout = TRUE,
                          stderr = TRUE)
      # Skip header row and extract PIDs
      if (length(cmd_info) > 1) {
        as.integer(strsplit(cmd_info[-1], ",")[[1]])
      } else {
        integer(0)
      }
    }, error = function(e) integer(0))

    # First try gentle termination
    success <- system2("taskkill", c("/PID", pid), stdout = TRUE, stderr = TRUE) == 0
    if (length(child_pids) > 0) {
      system2("taskkill", sprintf("/PID %d", child_pids), stdout = TRUE, stderr = TRUE)
    }

    if (!success) {
      # If gentle kill failed, try force kill after delay
      Sys.sleep(min(1, timeout))
      success <- system2("taskkill", c("/F", "/PID", pid), stdout = TRUE, stderr = TRUE) == 0
      if (length(child_pids) > 0) {
        system2("taskkill", sprintf("/F /PID %d", child_pids), stdout = TRUE, stderr = TRUE)
      }

      if (!success) {
        cli::cli_warn(c(
          "Failed to forcefully terminate process {pid} and its children",
          "i" = "Process may require elevated privileges to terminate",
          "i" = "Try closing associated applications manually"
        ))
      }
    }
    success
  }, error = function(e) {
    cli::cli_warn(c(
      "Error killing process {pid}",
      "x" = e$message,
      "i" = "Ensure you have appropriate permissions"
    ))
    FALSE
  })
}

#' Kill a Unix process and its children
#'
#' @param pid Process ID to kill
#' @param timeout Timeout in seconds
#' @return Boolean indicating success
#' @keywords internal
kill_unix_process <- function(pid, timeout) {
  checkmate::assert_integerish(pid, len = 1, any.missing = FALSE, lower = 1)
  checkmate::assert_number(timeout, lower = 0)

  tryCatch({
    # Get all child PIDs
    child_pids <- tryCatch(
      system2("pgrep", c("-P", pid), stdout = TRUE, stderr = TRUE),
      error = function(e) character(0)
    )

    child_pids <- as.integer(child_pids)

    # First try SIGTERM on parent and children
    success <- system2("kill", as.character(pid), stdout = TRUE, stderr = TRUE) == 0
    if (length(child_pids) > 0) {
      system2("kill", as.character(child_pids), stdout = TRUE, stderr = TRUE)
    }

    if (!success) {
      # If SIGTERM failed, try SIGKILL after delay
      Sys.sleep(min(1, timeout))
      success <- system2("kill", c("-9", pid), stdout = TRUE, stderr = TRUE) == 0
      if (length(child_pids) > 0) {
        system2("kill", c("-9", as.character(child_pids)), stdout = TRUE, stderr = TRUE)
      }

      if (!success) {
        cli::cli_warn(c(
          "Failed to forcefully terminate process {pid} and its children",
          "i" = "Process may require elevated privileges to terminate",
          "i" = "Try 'sudo kill -9 {pid}' manually"
        ))
      }
    }
    success
  }, error = function(e) {
    cli::cli_warn(c(
      "Error killing process {pid}",
      "x" = e$message,
      "i" = "Ensure you have appropriate permissions"
    ))
    FALSE
  })
}

#' Kill a watcher process and cleanup resources
#'
#' @param db_path Path to watcher database
#' @param timeout Timeout in seconds for kill operation
#' @return Boolean indicating success
#' @keywords internal
kill_watcher_process <- function(db_path, timeout = 5) {
  checkmate::assert_file_exists(db_path)
  checkmate::assert_number(timeout, lower = 0)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  tryCatch({
    # Get watcher status
    status <- DBI::dbGetQuery(
      con,
      "SELECT key, value FROM status WHERE key IN ('pid', 'persistent')"
    )
    status_list <- as.list(tibble::deframe(status))

    # Extract watcher ID from database path
    id <- parse_watcher_id(db_path)

    # Handle persistent watchers
    if (identical(status_list$persistent, "true")) {
      DBI::dbDisconnect(con)  # Close connection before unregistering

      success <- if (.Platform$OS.type == "windows") {
        unregister_persistent_windows(id)
      } else if (Sys.info()["sysname"] == "Darwin") {
        unregister_persistent_macos(id)
      } else {
        unregister_persistent_linux(id)
      }

      if (success) {
        cleanup_watcher_resources(db_path, force = TRUE)
      }

      return(success)
    }

    # Handle regular processes
    if (is.null(status_list$pid)) {
      return(FALSE)
    }

    pid <- as.integer(status_list$pid)

    # Close connection before killing process
    DBI::dbDisconnect(con)

    # Platform-specific process termination
    success <- if (.Platform$OS.type == "windows") {
      kill_windows_process(pid, timeout)
    } else {
      kill_unix_process(pid, timeout)
    }

    if (success) {
      cleanup_watcher_resources(db_path)
    }

    success
  }, error = function(e) {
    cli::cli_warn(c(
      "Error killing watcher process",
      "x" = e$message
    ))
    FALSE
  }, finally = {
    if (!is.null(con) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  })
}

#' Clean up watcher resources
#'
#' @param db_path Path to watcher database
#' @param force Whether to force cleanup of persistent watcher
#' @return Invisible NULL
#' @keywords internal
cleanup_watcher_resources <- function(db_path, force = FALSE) {
  checkmate::assert_file_exists(db_path)

  # Clean up database file
  tryCatch({
    fs::file_delete(db_path)

    # Clean up log files
    id <- parse_watcher_id(db_path)
    log_files <- fs::dir_ls(
      fs::path_dir(db_path),
      glob = sprintf("*_%s.log", id)
    )
    fs::file_delete(log_files)
  }, error = function(e) {
    cli::cli_warn(c(
      "Error cleaning up watcher files",
      "x" = e$message
    ))
  })

  invisible()
}
