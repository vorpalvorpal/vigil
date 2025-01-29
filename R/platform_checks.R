#' Check platform-specific requirements
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
#' @keywords internal
check_sqlite_requirements <- function() {
  # Check R packages first
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    cli::cli_abort(c(
      "Required packages not available",
      "i" = "Install required packages with:",
      " " = "install.packages(c('DBI', 'RSQLite'))"
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
        "i" = "Check permissions for parent directory"
      ))
    })
  }

  if (!fs::file_access(vigil_dir, mode = "write")) {
    cli::cli_abort(c(
      "Vigil directory is not writable",
      "x" = "Cannot write to: {.path {vigil_dir}}",
      "i" = "Check directory permissions"
    ))
  }

  TRUE
}

#' Check persistent watcher requirements
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
          "x" = e$message
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

  # Check inotify limits
  max_watches <- as.numeric(readLines("/proc/sys/fs/inotify/max_user_watches",
                                      warn = FALSE))
  if (max_watches < 8192) {
    cli::cli_alert_warning(c(
      "Low inotify watch limit detected",
      "i" = "For better performance, increase the limit with:",
      " " = "echo fs.inotify.max_user_watches=524288 | sudo tee -a /etc/sysctl.conf",
      " " = "sudo sysctl -p"
    ))
  }

  TRUE
}
