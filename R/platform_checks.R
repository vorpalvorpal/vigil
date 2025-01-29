#' Check platform-specific requirements
#' @return Logical indicating if all requirements are met
#' @keywords internal
check_platform_requirements <- function() {
  # Check SQLite first as it's required on all platforms
  check_sqlite_requirements()

  # Platform-specific file watching requirements
  if (.Platform$OS.type == "windows") {
    return(TRUE)  # No additional requirements for basic watching
  } else if (Sys.info()["sysname"] == "Darwin") {
    check_fswatch()
  } else {
    check_inotify()
  }

  TRUE
}

#' Check SQLite availability and accessibility
#' @return Logical indicating if SQLite requirements are met
#' @keywords internal
check_sqlite_requirements <- function() {
  # Check R packages first with specific error messaging
  pkg_errors <- character()
  for (pkg in c("DBI", "RSQLite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      pkg_errors <- c(pkg_errors, pkg)
    }
  }

  if (length(pkg_errors) > 0) {
    cli::cli_abort(c(
      "Required packages not available",
      "i" = "Install required packages with:",
      " " = "install.packages(c({toString(paste0(\"'\", pkg_errors, \"'\"))}))"
    ))
  }

  # Platform-specific SQLite checks
  if (.Platform$OS.type == "windows") {
    # Check for ODBC driver - not required but preferred
    tryCatch({
      con <- DBI::dbConnect(odbc::odbc(),
                            .connection_string = "Driver={SQLite3 ODBC Driver}")
      DBI::dbDisconnect(con)
    }, error = function(e) {
      cli::cli_alert_warning(c(
        "SQLite ODBC driver not found",
        "i" = "For better performance, install the SQLite ODBC driver",
        "i" = "Download from: http://www.ch-werner.de/sqliteodbc/"
      ))
    })
  } else {
    # Check sqlite3 command-line tool on Unix systems
    if (Sys.which("sqlite3") == "") {
      cli::cli_abort(c(
        "sqlite3 command-line tool not found",
        "i" = if (Sys.info()["sysname"] == "Darwin") {
          "Install with: brew install sqlite3"
        } else {
          "Install with: sudo apt-get install sqlite3 or equivalent"
        }
      ))
    }
  }

  # Check database directory permissions
  vigil_dir <- get_vigil_dir()
  if (!fs::dir_exists(vigil_dir)) {
    tryCatch({
      fs::dir_create(vigil_dir)
    }, error = function(e) {
      cli::cli_abort(c(
        "Cannot create vigil directory",
        "x" = e$message,
        "i" = "Check permissions for parent directory: {.path {fs::path_dir(vigil_dir)}}"
      ))
    })
  }

  if (!fs::file_access(vigil_dir, mode = "write")) {
    cli::cli_abort(c(
      "Vigil directory is not writable",
      "x" = "Cannot write to: {.path {vigil_dir}}",
      "i" = "Check directory permissions",
      "i" = "Current user: {Sys.info()['user']}"
    ))
  }

  TRUE
}

#' Check persistent watcher requirements
#' @return Logical indicating if persistent watcher requirements are met
#' @keywords internal
check_persistent_requirements <- function() {
  if (.Platform$OS.type == "windows") {
    # Check taskscheduleR
    if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
      cli::cli_abort(c(
        "taskscheduleR package is required for persistent watchers on Windows",
        "i" = "Install with: install.packages('taskscheduleR')"
      ))
    }

    # Check Task Scheduler access
    tryCatch({
      taskscheduleR::taskscheduler_ls()
    }, error = function(e) {
      cli::cli_abort(c(
        "Cannot access Windows Task Scheduler",
        "x" = e$message,
        "i" = "Run R with administrator privileges or check Task Scheduler permissions"
      ))
    })

  } else if (Sys.info()["sysname"] == "Darwin") {
    # Check launchd
    if (Sys.which("launchctl") == "") {
      cli::cli_abort("launchd is required for persistent watchers on macOS")
    }

    # Check LaunchAgents directory
    agents_dir <- "~/Library/LaunchAgents"
    if (!fs::dir_exists(agents_dir)) {
      tryCatch({
        fs::dir_create(agents_dir)
      }, error = function(e) {
        cli::cli_abort(c(
          "Cannot create LaunchAgents directory",
          "x" = e$message,
          "i" = "Check directory permissions"
        ))
      })
    }

  } else {
    # Check systemd
    if (Sys.which("systemctl") == "") {
      cli::cli_abort(c(
        "systemd is required for persistent watchers on Linux",
        "i" = "Most Linux distributions include systemd by default"
      ))
    }

    # Check user service directory
    service_dir <- "~/.config/systemd/user"
    if (!fs::dir_exists(service_dir)) {
      tryCatch({
        fs::dir_create(service_dir)
      }, error = function(e) {
        cli::cli_abort(c(
          "Cannot create systemd user directory",
          "x" = e$message
        ))
      })
    }

    # Check systemd user instance
    tryCatch({
      system2("systemctl", "--user", stdout = NULL, stderr = NULL)
    }, error = function(e) {
      cli::cli_abort(c(
        "Cannot access systemd user instance",
        "i" = "Ensure user session is properly initialized",
        "i" = "Try: loginctl enable-linger $USER"
      ))
    })
  }

  TRUE
}

#' Check fswatch availability on macOS
#' @return Logical indicating if fswatch is available
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

#' Check inotify-tools availability and configuration on Linux
#' @return Logical indicating if inotify-tools are properly configured
#' @keywords internal
check_inotify <- function() {
  if (Sys.which("inotifywait") == "") {
    cli::cli_abort(c(
      "inotify-tools is required for file watching on Linux",
      "i" = "Install with:",
      "*" = "Ubuntu/Debian: sudo apt-get install inotify-tools",
      "*" = "Fedora: sudo dnf install inotify-tools",
      "*" = "Arch Linux: sudo pacman -S inotify-tools"
    ))
  }

  # Check inotify limits with error handling
  tryCatch({
    watch_limit_file <- "/proc/sys/fs/inotify/max_user_watches"
    if (!file.exists(watch_limit_file)) {
      cli::cli_abort(c(
        "Cannot check inotify watch limits",
        "x" = "File not found: {watch_limit_file}",
        "i" = "Your system may not use standard inotify configuration"
      ))
    }

    watch_limit_content <- readLines(watch_limit_file, warn = FALSE)
    if (length(watch_limit_content) == 0) {
      cli::cli_abort(c(
        "Cannot read inotify watch limits",
        "x" = "File is empty: {watch_limit_file}",
        "i" = "Check system inotify configuration"
      ))
    }

    max_watches <- suppressWarnings(as.numeric(watch_limit_content[1]))
    if (is.na(max_watches)) {
      cli::cli_abort(c(
        "Invalid inotify watch limit value",
        "x" = "Found: {watch_limit_content[1]}",
        "i" = "Check system inotify configuration"
      ))
    }

    if (max_watches < 8192) {
      cli::cli_alert_warning(c(
        "Low inotify watch limit detected: {max_watches}",
        "i" = "For better performance, increase the limit with:",
        " " = "echo fs.inotify.max_user_watches=524288 | sudo tee -a /etc/sysctl.conf",
        " " = "sudo sysctl -p"
      ))
    }
  }, error = function(e) {
    if (!inherits(e, "error")) {
      cli::cli_warn(c(
        "Could not verify inotify watch limits",
        "x" = e$message,
        "i" = "File watching may still work but could be limited"
      ))
    }
  })

  TRUE
}
