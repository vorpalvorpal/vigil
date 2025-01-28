# tests/testthat/helper.R

library(testthat)
library(vigil)

# Create temporary test environment
setup_test_env <- function() {
  tmp_dir <- file.path(tempdir(), paste0("vigil-test-", uuid::UUIDgenerate()))
  fs::dir_create(tmp_dir)

  list(
    dir = tmp_dir,
    cleanup = function() {
      unlink(tmp_dir, recursive = TRUE)
    }
  )
}

# Mock process for testing
create_mock_process <- function(env) {
  script <- if (.Platform$OS.type == "windows") {
    'while(1) { Start-Sleep -Seconds 1 }'
  } else {
    'while true; do sleep 1; done'
  }

  script_path <- file.path(env$dir, "mock_process.bat")
  writeLines(script, script_path)

  if (.Platform$OS.type != "windows") {
    Sys.chmod(script_path, "755")
  }

  px <- processx::process$new(
    command = if (.Platform$OS.type == "windows") "powershell" else "bash",
    args = c(if (.Platform$OS.type == "windows") "-File" else "-c", script_path)
  )

  list(
    process = px,
    cleanup = function() {
      if (px$is_alive()) px$kill()
      unlink(script_path)
    }
  )
}

# Wait for condition with timeout
wait_for <- function(condition, timeout = 5, interval = 0.1) {
  end_time <- Sys.time() + timeout
  while (Sys.time() < end_time) {
    if (condition()) {
      return(TRUE)
    }
    Sys.sleep(interval)
  }
  FALSE
}

# Skip tests on specific platforms
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

# Verify if watcher exists
watcher_exists <- function(id) {
  watchers <- list_watchers()
  id %in% watchers$id
}

# Wait for watcher to initialize
wait_for_watcher <- function(id, timeout = 5) {
  wait_for(function() watcher_exists(id), timeout = timeout)
}

# Create test files with specific patterns
create_test_files <- function(dir, patterns) {
  files <- character()
  for (pattern in patterns) {
    ext <- sub("*.", "", pattern)
    file <- file.path(dir, paste0("test.", ext))
    writeLines("test", file)
    files <- c(files, file)
  }
  files
}

# Collect events in a list
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
    }
  )
}

# Check system requirements
has_inotify <- function() {
  if (.Platform$OS.type == "windows") return(FALSE)
  nzchar(Sys::which("inotifywait"))
}

has_fswatch <- function() {
  if (.Platform$OS.type == "windows") return(FALSE)
  nzchar(Sys::which("fswatch"))
}

# Platform-specific file operations
create_file <- function(path, content = "test") {
  con <- file(path, "wb")
  on.exit(close(con))
  writeLines(content, con)
  Sys.sleep(0.1)  # Small delay to ensure file is written
}

modify_file <- function(path, content = "modified") {
  con <- file(path, "wb")
  on.exit(close(con))
  writeLines(content, con)
  Sys.sleep(0.1)  # Small delay to ensure file is written
}

delete_file <- function(path) {
  unlink(path)
  Sys.sleep(0.1)  # Small delay to ensure file is deleted
}

# Test environment setup with cleanup
with_test_env <- function(code) {
  env <- setup_test_env()
  on.exit(env$cleanup())

  # Create common test directories
  fs::dir_create(file.path(env$dir, "subdir"))

  # Execute test code with environment
  force(code)
}

# Safe process creation and cleanup
with_mock_process <- function(code) {
  env <- setup_test_env()
  on.exit(env$cleanup())

  mock <- create_mock_process(env)
  on.exit(mock$cleanup(), add = TRUE)

  # Execute test code with mock process
  force(code)
}

# Temporary callback script creation
with_callback_script <- function(code) {
  env <- setup_test_env()
  on.exit(env$cleanup())

  script_path <- file.path(env$dir, "callback.R")
  writeLines('message("Callback executed for:", event$path)', script_path)

  # Execute test code with script path
  force(code)
}
