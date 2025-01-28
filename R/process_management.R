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
  if (.Platform$OS.type == "windows") {
    base <- Sys.getenv("LOCALAPPDATA")
  } else {
    base <- "~/.local/share"
  }
  path <- fs::path(base, "R", "vigil")
  fs::dir_create(path)
  fs::path_norm(path)
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

  # Handle persistent watchers differently
  if (isTRUE(config$persistent)) {
    if (.Platform$OS.type == "windows") {
      return(verify_persistent_windows(config))
    } else if (Sys.info()["sysname"] == "Darwin") {
      return(verify_persistent_macos(config))
    } else {
      return(verify_persistent_linux(config))
    }
  }

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

#' Kill a vigil watcher process
#' @param pid Process ID to kill
#' @param id Watcher ID
#' @return Boolean indicating success
#' @keywords internal
kill_process <- function(pid, id) {
  # Read the watcher config
  config_file <- fs::path(get_vigil_dir(), sprintf("watcher_%s.json", id))
  if (!fs::file_exists(config_file)) {
    return(FALSE)
  }

  config <- jsonlite::read_json(config_file)

  # Handle persistent watchers
  if (isTRUE(config$persistent)) {
    if (.Platform$OS.type == "windows") {
      return(unregister_persistent_windows(config))
    } else if (Sys.info()["sysname"] == "Darwin") {
      return(unregister_persistent_macos(config))
    } else {
      return(unregister_persistent_linux(config))
    }
  }

  # For non-persistent watchers, verify before killing
  if (!verify_vigil_process(pid, id)) {
    return(FALSE)
  }

  if (.Platform$OS.type == "windows") {
    # Send terminate signal to allow cleanup
    success <- system2("taskkill", c("/PID", pid), stdout = FALSE, stderr = FALSE) == 0

    # If gentle kill failed, force kill after small delay
    if (!success) {
      Sys.sleep(2)
      success <- system2("taskkill", c("/F", "/PID", pid), stdout = FALSE, stderr = FALSE) == 0
    }

    success
  } else {
    # Send SIGTERM to allow cleanup
    success <- system2("kill", pid, stdout = FALSE, stderr = FALSE) == 0

    # If gentle kill failed, force kill after small delay
    if (!success) {
      Sys.sleep(2)
      success <- system2("kill", c("-9", pid), stdout = FALSE, stderr = FALSE) == 0
    }

    success
  }
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
