library(testthat)
library(vigil)

# Setup helpers
create_test_dir <- function() {
  dir <- withr::local_tempdir()
  fs::dir_create(fs::path(dir, "subdir"))
  return(dir)
}

create_test_file <- function(dir, name, content = "test") {
  path <- fs::path(dir, name)
  writeLines(content, path)
  Sys.sleep(0.1)  # Ensure file is written
  return(path)
}

# Basic watching functionality
test_that("watch() creates watcher with basic configuration", {
  skip_on_ci()
  dir <- create_test_dir()

  id <- watch(dir)
  expect_type(id, "character")

  watchers <- list_watchers()
  expect_true(id %in% watchers$id)

  watcher <- dplyr::filter(watchers, id == !!id)
  expect_equal(watcher$path, fs::path_norm(dir))
  expect_true(is.na(watcher$pattern))
  expect_false(watcher$recursive)
  expect_false(watcher$persistent)

  kill_watcher(id)
})

test_that("watch() handles invalid inputs correctly", {
  expect_error(watch("nonexistent/path"), "Directory.*not exist")

  dir <- create_test_dir()
  expect_error(watch(dir, pattern = "[invalid regex"), "Invalid regular expression")
  expect_error(watch(dir, watch_mode = "invalid"), "should be one of")
  expect_error(watch(dir, change_type = "invalid"), "should be one of")
})

# Pattern matching
test_that("watch() respects file patterns", {
  skip_on_ci()
  dir <- create_test_dir()

  events <- character()
  id <- watch(
    dir,
    pattern = ".*\\.csv$",
    callback = function(event) {
      events <<- c(events, basename(event$path))
    }
  )

  # Create different file types
  Sys.sleep(0.5)  # Allow watcher to initialize
  create_test_file(dir, "test.csv")
  create_test_file(dir, "test.txt")
  Sys.sleep(0.5)  # Allow events to process

  expect_equal(events, "test.csv")
  kill_watcher(id)
})

# Recursive watching
test_that("watch() handles recursive watching", {
  skip_on_ci()
  dir <- create_test_dir()
  subdir <- fs::path(dir, "subdir")

  events <- character()
  id <- watch(
    dir,
    recursive = TRUE,
    callback = function(event) {
      events <<- c(events, basename(event$path))
    }
  )

  # Create files in both directories
  Sys.sleep(0.5)
  create_test_file(dir, "root.txt")
  create_test_file(subdir, "sub.txt")
  Sys.sleep(0.5)

  expect_setequal(events, c("root.txt", "sub.txt"))
  kill_watcher(id)
})

# Callback handling
test_that("watch() handles different callback types", {
  skip_on_ci()
  dir <- create_test_dir()

  # Function callback
  events_fn <- character()
  id1 <- watch(dir, callback = function(event) {
    events_fn <<- c(events_fn, basename(event$path))
  })

  # Package function callback
  callback_script <- fs::path(dir, "callback.R")
  writeLines('events_script <<- c(events_script, basename(event$path))', callback_script)
  events_script <- character()
  id2 <- watch(dir, callback = callback_script)

  # Create test file
  Sys.sleep(0.5)
  create_test_file(dir, "test.txt")
  Sys.sleep(0.5)

  expect_equal(length(events_fn), 1)
  expect_equal(length(events_script), 1)

  kill_watcher(id1)
  kill_watcher(id2)
})

# Watch modes
test_that("watch() handles different watch modes", {
  skip_on_ci()
  dir <- create_test_dir()

  # Single event mode
  events_single <- integer(0)
  id_single <- watch(
    dir,
    watch_mode = "single",
    callback = function(event) {
      events_single <<- c(events_single, 1L)
    }
  )

  Sys.sleep(0.5)
  create_test_file(dir, "test1.txt")
  create_test_file(dir, "test2.txt")
  Sys.sleep(0.5)

  expect_length(events_single, 1)
  expect_false(id_single %in% list_watchers()$id)

  # Continuous mode
  events_cont <- integer(0)
  id_cont <- watch(
    dir,
    watch_mode = "continuous",
    callback = function(event) {
      events_cont <<- c(events_cont, 1L)
    }
  )

  Sys.sleep(0.5)
  create_test_file(dir, "test3.txt")
  create_test_file(dir, "test4.txt")
  Sys.sleep(0.5)

  expect_length(events_cont, 2)
  expect_true(id_cont %in% list_watchers()$id)

  kill_watcher(id_cont)
})

# Persistent watchers
test_that("watch() handles persistent mode", {
  skip_on_ci()
  dir <- create_test_dir()

  id <- watch(dir, watch_mode = "persistent")
  expect_true(id %in% list_watchers()$id)

  # Verify watcher persists after simulated R session restart
  rm(list = ls(envir = asNamespace("vigil")), envir = asNamespace("vigil"))
  vigil:::.onLoad(NULL, "vigil")

  expect_true(id %in% list_watchers()$id)
  kill_watcher(id)
})

# Change types
test_that("watch() handles different change types", {
  skip_on_ci()
  dir <- create_test_dir()

  for (change_type in c("created", "modified", "deleted")) {
    events <- character()
    id <- watch(
      dir,
      change_type = change_type,
      callback = function(event) {
        events <<- c(events, event$change_type)
      }
    )

    Sys.sleep(0.5)
    file <- create_test_file(dir, "test.txt")
    writeLines("modified", file)
    unlink(file)
    Sys.sleep(0.5)

    expect_equal(events, change_type)
    kill_watcher(id)
  }
})

# Watch until
test_that("watch_until() works correctly", {
  skip_on_ci()
  dir <- create_test_dir()

  # Test with timeout, no events
  result <- watch_until(dir, timeout = 1)
  expect_null(result)

  # Test with event before timeout
  future::future({
    Sys.sleep(0.5)
    create_test_file(dir, "test.txt")
  })

  result <- watch_until(dir, timeout = 2)
  expect_s3_class(result, "tbl_df")
  expect_equal(basename(result$path), "test.txt")
  expect_equal(result$change_type, "created")
})

# Multiple watchers
test_that("multiple watchers work concurrently", {
  skip_on_ci()
  dir <- create_test_dir()
  subdirs <- purrr::map(1:3, function(i) {
    path <- fs::path(dir, sprintf("dir_%d", i))
    fs::dir_create(path)
    path
  })

  # Create watchers with different configurations
  events <- list()
  ids <- list()

  # Basic continuous watcher
  events[[1]] <- character()
  ids[[1]] <- watch(
    subdirs[[1]],
    callback = function(event) {
      events[[1]] <<- c(events[[1]], basename(event$path))
    }
  )

  # Pattern-specific single event watcher
  events[[2]] <- character()
  ids[[2]] <- watch(
    subdirs[[2]],
    pattern = ".*\\.csv$",
    watch_mode = "single",
    callback = function(event) {
      events[[2]] <<- c(events[[2]], basename(event$path))
    }
  )

  # Recursive with specific change type
  events[[3]] <- character()
  ids[[3]] <- watch(
    subdirs[[3]],
    recursive = TRUE,
    change_type = "created",
    callback = function(event) {
      events[[3]] <<- c(events[[3]], basename(event$path))
    }
  )

  Sys.sleep(0.5)

  # Create files in each directory
  create_test_file(subdirs[[1]], "test1.txt")
  create_test_file(subdirs[[1]], "test2.txt")
  create_test_file(subdirs[[2]], "test1.csv")
  create_test_file(subdirs[[2]], "test2.txt")
  create_test_file(subdirs[[3]], "test1.txt")
  create_test_file(fs::path(subdirs[[3]], "subdir"), "test2.txt")

  Sys.sleep(0.5)

  expect_length(events[[1]], 2)  # Both TXT files
  expect_length(events[[2]], 1)  # Only CSV (single mode)
  expect_length(events[[3]], 2)  # Both created files

  kill_watcher(ids[[1]])
})

# Clean up
test_that("kill_watcher() and kill_all_watchers() work correctly", {
  skip_on_ci()
  dir <- create_test_dir()

  # Create multiple watchers
  ids <- replicate(3, watch(dir))

  # Kill one watcher
  expect_true(kill_watcher(ids[1]))
  expect_false(ids[1] %in% list_watchers()$id)
  expect_true(all(ids[-1] %in% list_watchers()$id))

  # Kill all watchers
  kill_all_watchers()
  expect_equal(nrow(list_watchers()), 0)
})
