library(testthat)
library(vigil)

test_that("get_vigil_dir returns correct path", {
  dir <- get_vigil_dir()
  expect_true(fs::dir_exists(dir))

  # Check platform-specific behavior
  if (.Platform$OS.type == "windows") {
    expect_true(grepl("AppData/Local/R/vigil$", dir, ignore.case = TRUE))
  } else {
    expect_true(grepl(".local/share/R/vigil$", dir))
  }
})

test_that("verify_vigil_process handles non-existent config", {
  expect_false(verify_vigil_process("12345", "nonexistent-id"))
})

test_that("cleanup_watcher_files handles non-existent watcher", {
  expect_no_error(cleanup_watcher_files("nonexistent-id"))
})

test_that("cleanup_watcher_files respects persistent flag", {
  # Create mock persistent watcher config
  vigil_dir <- get_vigil_dir()
  id <- "test-persistent"
  config <- list(
    id = id,
    persistent = TRUE
  )
  config_file <- fs::path(vigil_dir, sprintf("watcher_%s.json", id))
  jsonlite::write_json(config, config_file, auto_unbox = TRUE)

  # Create test files
  test_files <- c(
    sprintf("process_%s.txt", id),
    sprintf("event_%s_123.json", id)
  )
  for (file in test_files) {
    writeLines("test", fs::path(vigil_dir, file))
  }

  # Test cleanup without force
  cleanup_watcher_files(id, force = FALSE)
  for (file in c(config_file, fs::path(vigil_dir, test_files))) {
    expect_true(fs::file_exists(file))
  }

  # Test cleanup with force
  cleanup_watcher_files(id, force = TRUE)
  for (file in c(config_file, fs::path(vigil_dir, test_files))) {
    expect_false(fs::file_exists(file))
  }
})

# Mock process verification
mock_verify_process <- function() {
  vigil_dir <- get_vigil_dir()
  id <- "test-process"

  # Create mock config
  config <- list(
    id = id,
    persistent = FALSE
  )
  jsonlite::write_json(
    config,
    fs::path(vigil_dir, sprintf("watcher_%s.json", id))
  )

  # Create mock process file
  writeLines(
    "12345 test",
    fs::path(vigil_dir, sprintf("process_%s.txt", id))
  )

  return(list(
    id = id,
    pid = "12345"
  ))
}

test_that("verify_vigil_process works with valid process", {
  skip_on_ci()  # Skip on CI since we can't create real processes

  mock <- mock_verify_process()
  expect_true(verify_vigil_process(mock$pid, mock$id))
  cleanup_watcher_files(mock$id, force = TRUE)
})

test_that("kill_process handles non-existent process", {
  expect_false(kill_process("99999", "nonexistent-id"))
})
