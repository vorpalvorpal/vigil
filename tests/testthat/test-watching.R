# tests/testthat/test-watching.R
library(testthat)
library(vigil)

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

test_that("watch() creates watcher with basic configuration", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  id <- watch(env$root)
  expect_type(id, "character")

  watchers <- list_watchers()
  expect_true(id %in% watchers$id)

  watcher <- subset(watchers, id == !!id)
  expect_equal(watcher$path, fs::path_norm(env$root))
  expect_true(is.na(watcher$pattern))
  expect_false(watcher$recursive)
  expect_false(watcher$persistent)

  kill_watcher(id)
})

test_that("watch() respects file patterns", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Create test files
  writeLines("test", file.path(env$root, "test.csv"))
  writeLines("test", file.path(env$root, "test.txt"))

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
  writeLines("modified", file.path(env$root, "test.csv"))
  writeLines("modified", file.path(env$root, "test.txt"))
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
  writeLines("test", file.path(env$root, "root.txt"))
  writeLines("test", file.path(env$subdir, "sub.txt"))
  Sys.sleep(1)  # Allow events to process

  # Should detect both files
  expect_setequal(events, c("root.txt", "sub.txt"))

  kill_watcher(id)
})

test_that("watch_until() returns correct event data", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Start watching in separate process
  future::future({
    Sys.sleep(1)  # Brief delay
    writeLines("test", file.path(env$root, "test.txt"))
  })

  result <- watch_until(env$root, timeout = 5)

  expect_s3_class(result, "tbl_df")
  expect_equal(basename(result$path), "test.txt")
  expect_equal(result$change_type, "created")
  expect_s3_class(result$timestamp, "POSIXct")
})

test_that("watch_until() respects timeout", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  result <- watch_until(env$root, timeout = 1)
  expect_null(result)
})

test_that("watch() handles file deletion", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Create initial file
  test_file <- file.path(env$root, "delete_test.txt")
  writeLines("test", test_file)

  events <- character()
  id <- watch(
    env$root,
    callback = function(event) {
      events <<- c(events, paste(event$change_type, basename(event$path)))
    }
  )

  # Delete file
  Sys.sleep(1)  # Allow watcher to initialize
  unlink(test_file)
  Sys.sleep(1)  # Allow event to process

  expect_equal(events, "deleted delete_test.txt")

  kill_watcher(id)
})

test_that("watch() handles rapid file changes", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  events <- list()
  id <- watch(
    env$root,
    callback = function(event) {
      events[[length(events) + 1]] <<- event
    }
  )

  # Rapid file operations
  Sys.sleep(1)  # Allow watcher to initialize
  test_file <- file.path(env$root, "rapid_test.txt")

  for(i in 1:10) {
    writeLines(as.character(i), test_file)
    Sys.sleep(0.1)
  }

  Sys.sleep(1)  # Allow events to process

  # Should have caught at least the first write and some modifications
  expect_gte(length(events), 2)
  expect_equal(events[[1]]$change_type, "created")
  expect_equal(basename(events[[1]]$path), "rapid_test.txt")

  kill_watcher(id)
})

test_that("kill_watcher() cleans up resources", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  id <- watch(env$root)
  expect_true(kill_watcher(id))

  watchers <- list_watchers()
  expect_false(id %in% watchers$id)

  # Check that files are cleaned up
  vigil_dir <- get_vigil_dir()
  expect_length(
    fs::dir_ls(vigil_dir, glob = sprintf("*_%s.*", id)),
    0
  )
})

test_that("watch() handles multiple patterns", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Create test files
  writeLines("test", file.path(env$root, "test.csv"))
  writeLines("test", file.path(env$root, "test.txt"))
  writeLines("test", file.path(env$root, "test.json"))

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
  writeLines("modified", file.path(env$root, "test.csv"))
  writeLines("modified", file.path(env$root, "test.txt"))
  writeLines("modified", file.path(env$root, "test.json"))
  Sys.sleep(1)  # Allow events to process

  # Should detect only CSV and JSON changes
  expect_setequal(events, c("test.csv", "test.json"))

  kill_watcher(id)
})

test_that("list_watchers() shows correct information", {
  skip_on_ci()
  env <- setup_watch_env()
  on.exit(cleanup_watch_env(env))

  # Create watchers with different configurations
  id1 <- watch(env$root)
  id2 <- watch(env$root, pattern = "*.csv", recursive = TRUE)
  id3 <- watch(env$root, persistent = TRUE)

  watchers <- list_watchers()

  # Basic watcher
  w1 <- subset(watchers, id == id1)
  expect_false(w1$recursive)
  expect_true(is.na(w1$pattern))
  expect_false(w1$persistent)

  # Pattern and recursive watcher
  w2 <- subset(watchers, id == id2)
  expect_true(w2$recursive)
  expect_equal(w2$pattern, "*.csv")
  expect_false(w2$persistent)

  # Persistent watcher
  w3 <- subset(watchers, id == id3)
  expect_true(w3$persistent)

  # Clean up
  kill_watcher(id1)
  kill_watcher(id2)
  kill_watcher(id3)
})
