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
        "i" = "LOCALAPPDATA environment variable not set"
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
      "i" = "Check if you have write permissions"
    ))
  })

  # Verify directory is writable
  if (!fs::file_access(path, mode = "write")) {
    cli::cli_abort(c(
      "Vigil directory is not writable",
      "i" = "Check permissions for {.path {path}}"
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
        on.exit(DBI::dbDisconnect(con))

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

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))

    # Get watcher status
    status <- DBI::dbGetQuery(
      con,
      "SELECT key, value FROM status WHERE key IN ('pid', 'state', 'persistent')"
    )
    status_list <- as.list(tibble::deframe(status))

    # Check if watcher is persistent
    if (identical(status_list$persistent, "true")) {
      if (.Platform$OS.type == "windows") {
        verify_persistent_windows(sub("watcher_(.*)\\.db$", "\\1", basename(db_path)))
      } else if (Sys.info()["sysname"] == "Darwin") {
        verify_persistent_macos(sub("watcher_(.*)\\.db$", "\\1", basename(db_path)))
      } else {
        verify_persistent_linux(sub("watcher_(.*)\\.db$", "\\1", basename(db_path)))
      }
    } else {
      # Check regular process
      if (is.null(status_list$pid) || status_list$state != "running") {
        return(FALSE)
      }

      pid <- as.integer(status_list$pid)

      # Platform-specific process check
      if (.Platform$OS.type == "windows") {
        # Check if it's a cscript.exe process running our VBS
        cmd_info <- system2("wmic",
                            c("process", "where", sprintf("ProcessId=%d", pid),
                              "get", "CommandLine", "/format:csv"),
                            stdout = TRUE)
        any(grepl("watch-files.vbs", cmd_info, fixed = TRUE))
      } else {
        # On Unix, check if process exists and is a shell script
        cmd <- system2("ps", c("-p", pid, "-o", "command="), stdout = TRUE)
        any(grepl("watch-files.sh", cmd, fixed = TRUE))
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

#' Kill a watcher process
#'
#' @param db_path Path to watcher database
#' @param timeout Timeout in seconds for kill operation
#' @return Boolean indicating success
#' @keywords internal
kill_watcher_process <- function(db_path, timeout = 5) {
  checkmate::assert_file_exists(db_path)
  checkmate::assert_number(timeout, lower = 0)

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))

    # Get watcher status
    status <- DBI::dbGetQuery(
      con,
      "SELECT key, value FROM status WHERE key IN ('pid', 'persistent')"
    )
    status_list <- as.list(tibble::deframe(status))

    # Extract watcher ID from database path
    id <- sub("watcher_(.*)\\.db$", "\\1", basename(db_path))

    # Handle persistent watchers
    if (identical(status_list$persistent, "true")) {
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
  })
}

#' Kill a Windows process
#'
#' @param pid Process ID to kill
#' @param timeout Timeout in seconds
#' @return Boolean indicating success
#' @keywords internal
kill_windows_process <- function(pid, timeout) {
  start_time <- Sys.time()

  # First try gentle termination
  success <- system2("taskkill",
                     c("/PID", pid),
                     stdout = FALSE,
                     stderr = FALSE) == 0

  # If gentle kill failed, try force kill with remaining timeout
  if (!success) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed >= timeout) {
      return(FALSE)
    }

    # Wait briefly before force kill
    Sys.sleep(min(1, timeout - elapsed))

    success <- system2("taskkill",
                       c("/F", "/PID", pid),
                       stdout = FALSE,
                       stderr = FALSE) == 0
  }

  success
}

#' Kill a Unix process
#'
#' @param pid Process ID to kill
#' @param timeout Timeout in seconds
#' @return Boolean indicating success
#' @keywords internal
kill_unix_process <- function(pid, timeout) {
  start_time <- Sys.time()

  # First try SIGTERM
  success <- system2("kill", pid, stdout = FALSE, stderr = FALSE) == 0

  # If SIGTERM failed, try SIGKILL with remaining timeout
  if (!success) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed >= timeout) {
      return(FALSE)
    }

    # Wait briefly before SIGKILL
    Sys.sleep(min(1, timeout - elapsed))

    success <- system2("kill",
                       c("-9", pid),
                       stdout = FALSE,
                       stderr = FALSE) == 0
  }

  success
}

#' Clean up watcher resources
#'
#' @param db_path Path to watcher database
#' @param force Whether to force cleanup of persistent watcher
#' @keywords internal
cleanup_watcher_resources <- function(db_path, force = FALSE) {
  checkmate::assert_file_exists(db_path)

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))

    # Check if this is a persistent watcher
    status <- DBI::dbGetQuery(
      con,
      "SELECT value FROM status WHERE key = 'persistent'"
    )

    if (nrow(status) > 0 &&
        identical(status$value, "true") &&
        !force) {
      return(invisible())
    }

    # Update status
    DBI::dbExecute(
      con,
      "INSERT OR REPLACE INTO status (key, value) VALUES ('state', 'stopped')"
    )

    # Close connection before deletion
    DBI::dbDisconnect(con)

    # Clean up database and log files
    fs::file_delete(db_path)

    id <- sub("watcher_(.*)\\.db$", "\\1", basename(db_path))
    log_files <- fs::dir_ls(
      fs::path_dir(db_path),
      glob = sprintf("*_%s.log", id)
    )
    fs::file_delete(log_files)

  }, error = function(e) {
    cli::cli_warn(c(
      "Error cleaning up watcher resources",
      "x" = e$message
    ))
  })

  invisible()
}
