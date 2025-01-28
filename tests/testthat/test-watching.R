# tests/testthat/test-watching.R
library(testthat)
library(vigil)

test_that("watch() creates watcher with basic configuration", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  id <- watch(env$root)
  expect_type(id, "character")

  watchers <- list_watchers()
  expect_true(id %in% watchers$id)

  watcher <- dplyr::filter(watchers, id == !!id)
  expect_equal(watcher$path, fs::path_norm(env$root))
  expect_true(is.na(watcher$pattern))
  expect_false(watcher$recursive)
  expect_false(watcher$persistent)

  kill_watcher(id)
})

test_that("watch_until() handles all change types correctly", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  test_file <- fs::path(env$root, "test.txt")

  # Test file creation
  future::future({
    Sys.sleep(1)
    writeLines("test", test_file)
  })
  result <- watch_until(env$root, change_type = "created", timeout = 5)
  expect_equal(basename(result$path), "test.txt")
  expect_equal(result$change_type, "created")

  # Test file modification
  future::future({
    Sys.sleep(1)
    writeLines("modified", test_file)
  })
  result <- watch_until(env$root, change_type = "modified", timeout = 5)
  expect_equal(result$change_type, "modified")

  # Test file deletion
  future::future({
    Sys.sleep(1)
    unlink(test_file)
  })
  result <- watch_until(env$root, change_type = "deleted", timeout = 5)
  expect_equal(result$change_type, "deleted")
})

test_that("watch() respects file patterns", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Create test files
  writeLines("test", fs::path(env$root, "test.csv"))
  writeLines("test", fs::path(env$root, "test.txt"))

  # Watch CSV files
  events <- character()
  id <- watch(
    env$root,
    pattern = "*.csv",
    callback = function(event) {
      events <<- c(events, basename(event$path))
    }
  )

  # Modify files
  Sys.sleep(1)  # Allow watcher to initialize
  writeLines("modified", fs::path(env$root, "test.csv"))
  writeLines("modified", fs::path(env$root, "test.txt"))
  Sys.sleep(1)  # Allow events to process

  # Should only detect CSV changes
  expect_equal(events, "test.csv")

  kill_watcher(id)
})

test_that("watch() handles recursive watching", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  events <- character()
  id <- watch(
    env$root,
    recursive = TRUE,
    callback = function(event) {
      events <<- c(events, basename(event$path))
    }
  )

  # Create files in root and subdirectory
  Sys.sleep(1)  # Allow watcher to initialize
  writeLines("test", fs::path(env$root, "root.txt"))
  writeLines("test", fs::path(env$subdir, "sub.txt"))
  Sys.sleep(1)  # Allow events to process

  # Should detect both files
  expect_setequal(events, c("root.txt", "sub.txt"))

  kill_watcher(id)
})

test_that("watch_until() respects timeout", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Test with short timeout, no events
  result <- watch_until(env$root, timeout = 1)
  expect_null(result)

  # Test with timeout but event occurs
  future::future({
    Sys.sleep(0.5)
    writeLines("test", fs::path(env$root, "test.txt"))
  })
  result <- watch_until(env$root, timeout = 2)
  expect_s3_class(result, "tbl_df")
  expect_equal(basename(result$path), "test.txt")
})

test_that("watch() handles all watch modes correctly", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Test single mode
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

  # Test persistent mode
  id_persistent <- watch(
    env$root,
    watch_mode = "persistent"
  )

  Sys.sleep(1)
  expect_true(watcher_exists(id_persistent))

  # Simulate R session restart
  rm(list = ls(envir = asNamespace("vigil")), envir = asNamespace("vigil"))
  vigil:::.onLoad(NULL, "vigil")

  expect_true(watcher_exists(id_persistent))
  kill_watcher(id_persistent)
})

test_that("watch() handles multiple file patterns", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Create test files
  writeLines("test", fs::path(env$root, "test.csv"))
  writeLines("test", fs::path(env$root, "test.txt"))
  writeLines("test", fs::path(env$root, "test.json"))

  events <- character()
  id <- watch(
    env$root,
    pattern = "*.{csv,json}",  # Watch both CSV and JSON
    callback = function(event) {
      events <<- c(events, basename(event$path))
    }
  )

  # Modify all files
  Sys.sleep(1)  # Allow watcher to initialize
  writeLines("modified", fs::path(env$root, "test.csv"))
  writeLines("modified", fs::path(env$root, "test.txt"))
  writeLines("modified", fs::path(env$root, "test.json"))
  Sys.sleep(1)  # Allow events to process

  # Should detect only CSV and JSON changes
  expect_setequal(events, c("test.csv", "test.json"))

  kill_watcher(id)
})

test_that("kill_watcher() cleans up resources properly", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  id <- watch(env$root)
  expect_true(kill_watcher(id))

  # Check watcher is removed from list
  watchers <- list_watchers()
  expect_false(id %in% watchers$id)

  # Check that files are cleaned up
  vigil_dir <- get_vigil_dir()
  watcher_files <- fs::dir_ls(
    vigil_dir,
    glob = sprintf("*_%s.*", id)
  )
  expect_length(watcher_files, 0)
})

test_that("list_watchers() shows correct information for all modes", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Create watchers with different configurations
  id1 <- watch(env$root, watch_mode = "continuous")
  id2 <- watch(env$root, pattern = "*.csv", recursive = TRUE, watch_mode = "single")
  id3 <- watch(env$root, watch_mode = "persistent")

  watchers <- list_watchers()

  # Continuous mode watcher
  w1 <- dplyr::filter(watchers, id == id1)
  expect_false(w1$recursive)
  expect_true(is.na(w1$pattern))
  expect_false(w1$persistent)

  # Single mode watcher with pattern
  w2 <- dplyr::filter(watchers, id == id2)
  expect_true(w2$recursive)
  expect_equal(w2$pattern, "*.csv")
  expect_false(w2$persistent)

  # Persistent watcher
  w3 <- dplyr::filter(watchers, id == id3)
  expect_true(w3$persistent)

  # Clean up
  kill_watcher(id1)
  # id2 should clean itself up (single mode)
  kill_watcher(id3)
})
