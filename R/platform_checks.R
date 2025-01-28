#' Check platform-specific requirements
#' @keywords internal
check_platform_requirements <- function() {
  if (.Platform$OS.type == "windows") {
    # Windows has no requirements for basic watching
    return(TRUE)
  } else if (Sys.info()["sysname"] == "Darwin") {
    check_fswatch()
  } else {
    check_inotify()
  }
}

#' Check fswatch availability on macOS
#' @keywords internal
check_fswatch <- function() {
  if (Sys.which("fswatch") == "") {
    cli::cli_abort(c(
      "fswatch is required for file watching on macOS",
      "i" = "Install with: brew install fswatch"
    ))
  }
  TRUE
}

#' Check inotify-tools availability on Linux
#' @keywords internal
check_inotify <- function() {
  if (Sys.which("inotifywait") == "") {
    cli::cli_abort(c(
      "inotify-tools is required for file watching on Linux",
      "i" = "Install with: sudo apt-get install inotify-tools (Ubuntu/Debian)",
      "i" = "or: sudo dnf install inotify-tools (Fedora)",
      "i" = "or: sudo pacman -S inotify-tools (Arch Linux)"
    ))
  }
  TRUE
}

#' Check persistent watcher requirements
#' @keywords internal
check_persistent_requirements <- function() {
  if (.Platform$OS.type == "windows") {
    if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
      cli::cli_abort(c(
        "taskscheduleR package is required for persistent watchers on Windows",
        "i" = "Install with: install.packages('taskscheduleR')"
      ))
    }
  } else if (Sys.info()["sysname"] == "Darwin") {
    if (Sys.which("launchctl") == "") {
      cli::cli_abort("launchd is required for persistent watchers on macOS")
    }
  } else {
    if (Sys.which("systemctl") == "") {
      cli::cli_abort(c(
        "systemd is required for persistent watchers on Linux",
        "i" = "Most Linux distributions include systemd by default"
      ))
    }
  }
  TRUE
}
