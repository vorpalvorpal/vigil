# tests/testthat/test-integration.R
library(testthat)
library(vigil)

# Helper to simulate file system activity
simulate_fs_activity <- function(dir, files = 5, delay = 0.5) {
  events <- data.frame(
    path = character(),
    action = character(),
    timestamp = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in 1:files) {
    filename <- sprintf("test_%d.txt", i)
    filepath <- file.path(dir, filename)

    # Create
    writeLines(as.character(i), filepath)
    events <- rbind(events, data.frame(
      path = filepath,
      action = "created",
      timestamp = Sys.time()
    ))
    Sys.sleep(delay)

    # Modify
    writeLines(as.character(i * 2), filepath)
    events <- rbind(events, data.frame(
      path = filepath,
      action = "modified",
      timestamp = Sys.time()
    ))
    Sys.sleep(delay)

    # Delete
    unlink(filepath)
    events <- rbind(events, data.frame(
      path = filepath,
      action = "deleted",
      timestamp = Sys.time()
    ))
    Sys.sleep(delay)
  }

  events
}

test_that("full workflow with callbacks works", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Set up event collection
  events_seen <- data.frame(
    path = character(),
    type = character(),
    timestamp = numeric(),
    stringsAsFactors = FALSE
  )

  # Start watcher with callback
  id <- watch(
    env$root,
    callback = function(event) {
      events_seen <<- rbind(events_seen, data.frame(
        path = event$path,
        type = event$change_type,
        timestamp = as.numeric(event$timestamp)
      ))
    }
  )

  # Simulate file activity
  Sys.sleep(1)  # Allow watcher to initialize
  events_generated <- simulate_fs_activity(env$root, files = 3)
  Sys.sleep(1)  # Allow final events to process

  # Clean up
  kill_watcher(id)

  # Verify events
  expect_equal(nrow(events_seen), nrow(events_generated))
  expect_setequal(basename(events_seen$path), basename(events_generated$path))
  expect_setequal(events_seen$type, events_generated$action)
})

test_that("persistent watchers survive R session", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Create persistent watcher
  id <- watch(env$root, persistent = TRUE)

  # Verify it exists
  watchers_before <- list_watchers()
  expect_true(id %in% watchers_before$id)

  # Simulate R session restart by cleaning package state
  rm(list = ls(envir = asNamespace("vigil")), envir = asNamespace("vigil"))
  vigil:::.onLoad(NULL, "vigil")

  # Verify watcher still exists
  watchers_after <- list_watchers()
  expect_true(id %in% watchers_after$id)

  # Clean up
  kill_watcher(id)
})

test_that("multiple watchers work concurrently", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Create subdirectories
  dirs <- lapply(1:3, function(i) {
    path <- file.path(env$root, sprintf("dir_%d", i))
    fs::dir_create(path)
    path
  })

  # Create watchers with different configurations
  events <- list()
  ids <- list()

  # Watcher 1: Basic
  events[[1]] <- character()
  ids[[1]] <- watch(dirs[[1]], callback = function(event) {
    events[[1]] <<- c(events[[1]], basename(event$path))
  })

  # Watcher 2: With pattern
  events[[2]] <- character()
  ids[[2]] <- watch(dirs[[2]], pattern = "*.csv", callback = function(event) {
    events[[2]] <<- c(events[[2]], basename(event$path))
  })

  # Watcher 3: Recursive
  events[[3]] <- character()
  ids[[3]] <- watch(dirs[[3]], recursive = TRUE, callback = function(event) {
    events[[3]] <<- c(events[[3]], basename(event$path))
  })

  # Create files in each directory
  Sys.sleep(1)  # Allow watchers to initialize

  # Dir 1: Basic files
  writeLines("test", file.path(dirs[[1]], "test1.txt"))
  writeLines("test", file.path(dirs[[1]], "test2.txt"))

  # Dir 2: Mix of CSV and TXT
  writeLines("test", file.path(dirs[[2]], "test1.csv"))
  writeLines("test", file.path(dirs[[2]], "test2.txt"))

  # Dir 3: Files in root and subdir
  writeLines("test", file.path(dirs[[3]], "test1.txt"))
  fs::dir_create(file.path(dirs[[3]], "subdir"))
  writeLines("test", file.path(dirs[[3]], "subdir", "test2.txt"))

  Sys.sleep(1)  # Allow events to process

  # Verify each watcher's events
  expect_length(events[[1]], 2)  # Both TXT files
  expect_length(events[[2]], 1)  # Only CSV file
  expect_length(events[[3]], 2)  # Both root and subdir files

  # Clean up
  walk(ids, kill_watcher)
})

test_that("stress test with rapid file changes", {
  skip_on_ci()
  skip_on_cran()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  events_seen <- 0
  id <- watch(env$root, callback = function(event) {
    events_seen <<- events_seen + 1
  })

  # Rapid file creation/modification
  Sys.sleep(1)  # Allow watcher to initialize
  events_generated <- 0

  for (i in 1:100) {
    filepath <- file.path(env$root, sprintf("file_%d.txt", i))
    writeLines(as.character(i), filepath)
    events_generated <- events_generated + 1

    if (i %% 10 == 0) {
      Sys.sleep(0.1)  # Brief pause every 10 files
    }
  }

  Sys.sleep(2)  # Allow events to process

  # Verify all events were caught
  expect_equal(events_seen, events_generated)

  kill_watcher(id)
})
