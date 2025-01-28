library(testthat)
library(vigil)

# Environment Setup Helpers -----------------------------------------------

# Helper function to create test directory structure
setup_watch_env <- function() {
  tmp_dir <- file.path(tempdir(), paste0("vigil-test-", uuid::UUIDgenerate()))
  fs::dir_create(tmp_dir)

  # Create subdirectories for recursive tests
  fs::dir_create(file.path(tmp_dir, "subdir"))

  # Return paths
  list(
    root = tmp_dir,
    subdir = file.path(tmp_dir, "subdir")
  )
}

# Helper to cleanup test environment
cleanup_watch_env <- function(env) {
  unlink(env$root, recursive = TRUE)
}

#' Create test files with various patterns
#' @param dir Directory to create files in
#' @param patterns Vector of file patterns to create
#' @return Vector of created file paths
create_test_files <- function(dir, patterns) {
  checkmate::assert_directory_exists(dir)
  checkmate::assert_character(patterns, min.len = 1)

  purrr::map_chr(patterns, function(pattern) {
    ext <- sub(".*\\.", "", pattern)
    file <- fs::path(dir, paste0("test.", ext))
    writeLines("test", file)
    Sys.sleep(0.1)  # Ensure file creation is registered
    file
  })
}

# Process Management Helpers ---------------------------------------------

#' Create mock process for testing
#' @param env Test environment
#' @return List containing process object and cleanup function
create_mock_process <- function(env) {
  checkmate::assert_list(env, names = "dir")

  # Create platform-specific script
  script <- if (.Platform$OS.type == "windows") {
    'while(1) { Start-Sleep -Seconds 1 }'
  } else {
    'while true; do sleep 1; done'
  }

  script_path <- fs::path(env$dir, "mock_process.bat")
  writeLines(script, script_path)

  if (.Platform$OS.type != "windows") {
    fs::file_chmod(script_path, "0755")
  }

  # Start process
  px <- processx::process$new(
    command = if (.Platform$OS.type == "windows") "powershell" else "bash",
    args = c(if (.Platform$OS.type == "windows") "-File" else "-c", script_path),
    stdout = "|",
    stderr = "|"
  )

  list(
    process = px,
    cleanup = function() {
      if (px$is_alive()) px$kill()
      unlink(script_path)
    }
  )
}

#' Create mock persistent process
#' @param config Watcher configuration
#' @return List containing process info and cleanup function
create_mock_persistent <- function(config) {
  checkmate::assert_list(config, names = "id")

  if (.Platform$OS.type == "windows") {
    # Create Windows scheduled task
    task_name <- sprintf("vigil_watcher_%s", config$id)
    taskscheduleR::taskscheduler_create(
      taskname = task_name,
      rscript = system.file("mock_task.R", package = "vigil"),
      schedule = "ONCE",
      starttime = format(Sys.time(), "%H:%M")
    )

    list(
      task_name = task_name,
      cleanup = function() {
        taskscheduleR::taskscheduler_delete(task_name)
      }
    )
  } else if (Sys.info()["sysname"] == "Darwin") {
    # Create macOS launch agent
    agent_label <- sprintf("com.r-lib.vigil.watcher.%s", config$id)
    plist_path <- fs::path(
      "~/Library/LaunchAgents",
      sprintf("%s.plist", agent_label)
    )

    list(
      agent_label = agent_label,
      cleanup = function() {
        system2("launchctl", c("unload", "-w", plist_path))
        fs::file_delete(plist_path)
      }
    )
  } else {
    # Create Linux systemd service
    service_name <- sprintf("vigil-watcher-%s.service", config$id)

    list(
      service_name = service_name,
      cleanup = function() {
        system2("systemctl", c("--user", "stop", service_name))
        system2("systemctl", c("--user", "disable", service_name))
      }
    )
  }
}

# Event Collection Helpers ----------------------------------------------

#' Create event collector
#' @return List of functions for collecting and managing events
event_collector <- function() {
  events <- list()

  list(
    callback = function(event) {
      events[[length(events) + 1]] <<- event
    },
    get_events = function() {
      events
    },
    clear = function() {
      events <<- list()
    },
    count = function() {
      length(events)
    }
  )
}

#' Wait for event count with timeout
#' @param collector Event collector
#' @param count Expected event count
#' @param timeout Timeout in seconds
#' @param interval Check interval in seconds
#' @return TRUE if count reached, FALSE if timeout
wait_for_events <- function(collector, count, timeout = 5, interval = 0.1) {
  checkmate::assert_number(count, lower = 0)
  checkmate::assert_number(timeout, lower = 0)
  checkmate::assert_number(interval, lower = 0)

  end_time <- Sys.time() + timeout
  while (Sys.time() < end_time) {
    if (collector$count() >= count) {
      return(TRUE)
    }
    Sys.sleep(interval)
  }
  FALSE
}

# File Operation Helpers -----------------------------------------------

#' Create file with content
#' @param path File path
#' @param content File content
create_file <- function(path, content = "test") {
  checkmate::assert_string(path)
  writeLines(content, path)
  Sys.sleep(0.1)  # Ensure file creation is registered
}

#' Modify existing file
#' @param path File path
#' @param content New content
modify_file <- function(path, content = "modified") {
  checkmate::assert_string(path)
  checkmate::assert_file_exists(path)
  writeLines(content, path)
  Sys.sleep(0.1)  # Ensure modification is registered
}

#' Delete file
#' @param path File path
delete_file <- function(path) {
  checkmate::assert_string(path)
  unlink(path)
  Sys.sleep(0.1)  # Ensure deletion is registered
}

# System Requirement Checks --------------------------------------------

#' Check if inotify is available
#' @return TRUE if inotify-tools is installed
has_inotify <- function() {
  if (.Platform$OS.type == "windows") return(FALSE)
  nzchar(Sys.which("inotifywait"))
}

#' Check if fswatch is available
#' @return TRUE if fswatch is installed
has_fswatch <- function() {
  if (.Platform$OS.type == "windows") return(FALSE)
  nzchar(Sys.which("fswatch"))
}

# Test Environment Wrappers -------------------------------------------

#' Run code with test environment
#' @param code Code to execute in test environment
with_test_env <- function(code) {
  env <- setup_test_env()
  withr::defer(env$cleanup())

  force(code)
}

#' Run code with mock process
#' @param code Code to execute with mock process
with_mock_process <- function(code) {
  env <- setup_test_env()
  withr::defer(env$cleanup())

  mock <- create_mock_process(env)
  withr::defer(mock$cleanup())

  force(code)
}

#' Create and clean up temporary callback script
#' @param code Code to execute with callback script
with_callback_script <- function(code) {
  env <- setup_test_env()
  withr::defer(env$cleanup())

  script_path <- fs::path(env$dir, "callback.R")
  writeLines('
    message("Callback executed for:", event$path)
    writeLines(
      sprintf("%s: %s", event$change_type, basename(event$path)),
      file.path(dirname(event$path), "events.txt"),
      append = TRUE
    )
  ', script_path)

  force(code)
}

# Skip Helpers -------------------------------------------------------

skip_on_windows <- function() {
  if (.Platform$OS.type == "windows") {
    skip("Test not run on Windows")
  }
}

skip_on_unix <- function() {
  if (.Platform$OS.type != "windows") {
    skip("Test not run on Unix-like systems")
  }
}

skip_without_inotify <- function() {
  if (!has_inotify()) {
    skip("Test requires inotify-tools")
  }
}

skip_without_fswatch <- function() {
  if (!has_fswatch()) {
    skip("Test requires fswatch")
  }
}
