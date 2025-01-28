# tests/testthat/test-integration.R
library(testthat)
library(vigil)

# Helper to simulate file system activity with structured output
simulate_fs_activity <- function(dir, files = 5, delay = 0.5) {
  events <- tibble::tibble(
    path = character(),
    action = character(),
    timestamp = Sys.time()
  )

  for (i in 1:files) {
    filename <- sprintf("test_%d.txt", i)
    filepath <- fs::path(dir, filename)

    # Create
    writeLines(as.character(i), filepath)
    events <- dplyr::bind_rows(events, tibble::tibble(
      path = filepath,
      action = "created",
      timestamp = Sys.time()
    ))
    Sys.sleep(delay)

    # Modify
    writeLines(as.character(i * 2), filepath)
    events <- dplyr::bind_rows(events, tibble::tibble(
      path = filepath,
      action = "modified",
      timestamp = Sys.time()
    ))
    Sys.sleep(delay)

    # Delete
    unlink(filepath)
    events <- dplyr::bind_rows(events, tibble::tibble(
      path = filepath,
      action = "deleted",
      timestamp = Sys.time()
    ))
    Sys.sleep(delay)
  }

  events
}

test_that("full workflow with callbacks works for all change types", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Test each change type
  change_types <- c("created", "modified", "deleted", "any")

  for (change_type in change_types) {
    # Set up event collection
    events_seen <- tibble::tibble(
      path = character(),
      type = character(),
      timestamp = Sys.time()
    )

    # Start watcher with specific change type
    id <- watch(
      env$root,
      change_type = change_type,
      callback = function(event) {
        events_seen <<- dplyr::bind_rows(events_seen, tibble::tibble(
          path = event$path,
          type = event$change_type,
          timestamp = event$timestamp
        ))
      }
    )

    # Simulate file activity
    Sys.sleep(1)  # Allow watcher to initialize
    events_generated <- simulate_fs_activity(env$root, files = 2)
    Sys.sleep(1)  # Allow final events to process

    # Clean up
    kill_watcher(id)

    # Verify events based on change type
    if (change_type == "any") {
      expect_equal(nrow(events_seen), nrow(events_generated))
      expect_setequal(basename(events_seen$path), basename(events_generated$path))
      expect_setequal(events_seen$type, events_generated$action)
    } else {
      expected_events <- dplyr::filter(events_generated, action == change_type)
      expect_equal(nrow(events_seen), nrow(expected_events))
      expect_setequal(events_seen$type, rep(change_type, nrow(events_seen)))
    }
  }
})

test_that("watch_mode behavior is correct", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Test single event mode
  events_single <- integer(0)
  id_single <- watch(
    env$root,
    watch_mode = "single",
    callback = function(event) {
      events_single <<- c(events_single, 1L)
    }
  )

  Sys.sleep(1)
  writeLines("test1", fs::path(env$root, "test1.txt"))
  writeLines("test2", fs::path(env$root, "test2.txt"))
  Sys.sleep(1)

  expect_length(events_single, 1)
  expect_false(watcher_exists(id_single))

  # Test continuous mode
  events_continuous <- integer(0)
  id_continuous <- watch(
    env$root,
    watch_mode = "continuous",
    callback = function(event) {
      events_continuous <<- c(events_continuous, 1L)
    }
  )

  Sys.sleep(1)
  writeLines("test3", fs::path(env$root, "test3.txt"))
  writeLines("test4", fs::path(env$root, "test4.txt"))
  Sys.sleep(1)

  expect_length(events_continuous, 2)
  expect_true(watcher_exists(id_continuous))

  kill_watcher(id_continuous)
})

test_that("persistent watchers survive R session", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Create persistent watcher
  id <- watch(env$root, watch_mode = "persistent")

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

test_that("multiple watchers work concurrently with different configurations", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Create subdirectories
  dirs <- purrr::map(1:3, function(i) {
    path <- fs::path(env$root, sprintf("dir_%d", i))
    fs::dir_create(path)
    path
  })

  # Create watchers with different configurations
  events <- list()
  ids <- list()

  # Watcher 1: Basic continuous
  events[[1]] <- character()
  ids[[1]] <- watch(dirs[[1]],
                    watch_mode = "continuous",
                    callback = function(event) {
                      events[[1]] <<- c(events[[1]], basename(event$path))
                    })

  # Watcher 2: Pattern-specific single event
  events[[2]] <- character()
  ids[[2]] <- watch(dirs[[2]],
                    pattern = "*.csv",
                    watch_mode = "single",
                    callback = function(event) {
                      events[[2]] <<- c(events[[2]], basename(event$path))
                    })

  # Watcher 3: Recursive with specific change type
  events[[3]] <- character()
  ids[[3]] <- watch(dirs[[3]],
                    recursive = TRUE,
                    change_type = "created",
                    callback = function(event) {
                      events[[3]] <<- c(events[[3]], basename(event$path))
                    })

  # Create files in each directory
  Sys.sleep(1)  # Allow watchers to initialize

  # Dir 1: Multiple files
  writeLines("test", fs::path(dirs[[1]], "test1.txt"))
  writeLines("test", fs::path(dirs[[1]], "test2.txt"))

  # Dir 2: Mix of CSV and TXT
  writeLines("test", fs::path(dirs[[2]], "test1.csv"))
  writeLines("test", fs::path(dirs[[2]], "test2.txt"))

  # Dir 3: Files in root and subdir
  writeLines("test", fs::path(dirs[[3]], "test1.txt"))
  fs::dir_create(fs::path(dirs[[3]], "subdir"))
  writeLines("test", fs::path(dirs[[3]], "subdir", "test2.txt"))

  Sys.sleep(1)  # Allow events to process

  # Verify each watcher's events
  expect_length(events[[1]], 2)  # Both TXT files
  expect_length(events[[2]], 1)  # Only first CSV (single mode)
  expect_length(events[[3]], 2)  # Both created files

  # Clean up
  kill_watcher(ids[[1]])  # Only need to kill continuous watcher
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

  for (i in 1:50) {  # Reduced from 100 for faster tests
    filepath <- fs::path(env$root, sprintf("file_%d.txt", i))
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
