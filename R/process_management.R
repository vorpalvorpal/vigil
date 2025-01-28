#' Get Vigil User Directory
#'
#' Returns the platform-specific directory where vigil stores its configuration and
#' temporary files.
#'
#' @return Character string containing the absolute path to the vigil directory
#' @details
#' On Windows, files are stored in %LOCALAPPDATA%/R/vigil
#' On Unix-like systems, files are stored in ~/.local/share/R/vigil
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

  # Create path
  path <- fs::path(base, "R", "vigil")

  # Try to create directory with error handling
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

  # Clean up old files
  cleanup_old_files(path)

  fs::path_norm(path)
}

#' Clean up old vigil files
#' @param dir Directory to clean
#' @param max_age Maximum age in days before cleanup
#' @keywords internal
cleanup_old_files <- function(dir, max_age = 7) {
  # Get all files and filter by extensions we care about
  files <- fs::dir_ls(dir)
  files <- files[grep("\\.(json|txt|vbs|sh)$", files)]

  if (length(files) == 0) {
    return(invisible())
  }

  # Find files older than max_age days
  old_files <- fs::file_info(files) |>
    dplyr::filter(
      modification_time < (Sys.time() - max_age * 24 * 60 * 60)
    )

  if (nrow(old_files) > 0) {
    # Try to clean up each file
    purrr::walk(old_files$path, function(file) {
      tryCatch({
        # Check if file belongs to active watcher
        if (!is_active_watcher_file(file)) {
          fs::file_delete(file)
        }
      }, error = function(e) {
        cli::cli_warn(c(
          "Could not delete old file",
          "!" = "File: {.path {file}}",
          "x" = e$message
        ))
      })
    })
  }
}

#' Check if file belongs to active watcher
#' @param file File path to check
#' @return Boolean indicating if file belongs to active watcher
#' @keywords internal
is_active_watcher_file <- function(file) {
  # Extract watcher ID from filename
  id <- stringr::str_extract(
    basename(file),
    "(?:watcher|process|event)_([^_\\.]+)",
    group = 1
  )

  if (is.na(id)) {
    return(FALSE)
  }

  # Check if there's an active watcher with this ID
  watchers <- list_watchers()
  id %in% watchers$id
}

#' Verify if a process is a vigil watcher
#' @param pid Process ID to check
#' @param id Watcher ID
#' @return Boolean indicating if process is a valid vigil watcher
#' @keywords internal
verify_vigil_process <- function(pid, id) {
  # Read the watcher config first
  config_file <- fs::path(get_vigil_dir(), sprintf("watcher_%s.json", id))
  if (!fs::file_exists(config_file)) {
    return(FALSE)
  }

  config <- jsonlite::read_json(config_file)

  # For non-persistent watchers, check process as before
  if (.Platform$OS.type == "windows") {
    # Check if it's a cscript.exe process running our VBS
    cmd_info <- system2("wmic",
                        c("process", "where", sprintf("ProcessId=%s", pid),
                          "get", "CommandLine", "/format:csv"),
                        stdout = TRUE)

    any(grepl(sprintf("watch_%s.vbs", id), cmd_info, fixed = TRUE))
  } else {
    # On Unix, check if process is bash/sh running our script
    cmd <- system2("ps", c("-p", pid, "-o", "command="), stdout = TRUE)
    any(grepl(sprintf("watch_%s.sh", id), cmd, fixed = TRUE))
  }
}

#' Kill a process
#' @param pid Process ID to kill
#' @param id Watcher ID
#' @param timeout Timeout in seconds for kill operation
#' @return Boolean indicating success
#' @keywords internal
kill_process <- function(pid, id, timeout = 10) {
  # Validate inputs
  checkmate::assert_string(id)
  checkmate::assert_number(timeout, lower = 0)

  # Read the watcher config
  config_file <- fs::path(get_vigil_dir(), sprintf("watcher_%s.json", id))
  if (!fs::file_exists(config_file)) {
    return(FALSE)
  }

  config <- jsonlite::read_json(config_file)

  # Handle persistent watchers differently
  if (isTRUE(config$persistent)) {
    success <- if (.Platform$OS.type == "windows") {
      unregister_persistent_windows(config)
    } else if (Sys.info()["sysname"] == "Darwin") {
      unregister_persistent_macos(config)
    } else {
      unregister_persistent_linux(config)
    }

    if (success) {
      # Clean up configuration files after successful unregistration
      cleanup_watcher_files(id, force = TRUE)
    }

    return(success)
  }

  # For non-persistent watchers, verify process exists
  checkmate::assert_string(pid)
  if (!verify_vigil_process(pid, id)) {
    return(FALSE)
  }

  # Kill process based on platform
  success <- if (.Platform$OS.type == "windows") {
    kill_windows_process(pid, timeout)
  } else {
    kill_unix_process(pid, timeout)
  }

  # Clean up files if kill was successful
  if (success) {
    cleanup_watcher_files(id)
  }

  success
}

#' Kill a Windows process
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

    # Wait a short time before force kill
    Sys.sleep(min(2, timeout - elapsed))

    success <- system2("taskkill",
                       c("/F", "/PID", pid),
                       stdout = FALSE,
                       stderr = FALSE) == 0
  }

  success
}

#' Kill a Unix process
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

    # Wait a short time before SIGKILL
    Sys.sleep(min(2, timeout - elapsed))

    success <- system2("kill",
                       c("-9", pid),
                       stdout = FALSE,
                       stderr = FALSE) == 0
  }

  success
}

#' Clean up watcher files
#' @param id Watcher ID
#' @param force Whether to force cleanup of persistent watcher files
#' @keywords internal
cleanup_watcher_files <- function(id, force = FALSE) {
  vigil_dir <- get_vigil_dir()

  # Check if this is a persistent watcher
  config_file <- fs::path(vigil_dir, sprintf("watcher_%s.json", id))
  if (fs::file_exists(config_file)) {
    config <- jsonlite::read_json(config_file)
    if (isTRUE(config$persistent) && !force) {
      # Don't clean up persistent watcher files unless forced
      return(invisible())
    }
  }

  # Clean up all files matching the watcher ID
  files <- fs::dir_ls(
    vigil_dir,
    glob = sprintf("*_%s.*", id)
  )
  fs::file_delete(files)
}
