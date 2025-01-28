library(testthat)
library(vigil)

test_that("get_vigil_dir returns correct path", {
  dir <- get_vigil_dir()
  expect_true(fs::dir_exists(dir))
  expect_true(fs::file_access(dir, mode = "write"))

  # Check platform-specific behavior
  if (.Platform$OS.type == "windows") {
    expect_match(dir, "AppData/Local/R/vigil$", ignore.case = TRUE)
  } else {
    expect_match(dir, ".local/share/R/vigil$")
  }
})

test_that("get_vigil_dir handles permission errors", {
  withr::with_envvar(
    new = c("LOCALAPPDATA" = ""),
    {
      if (.Platform$OS.type == "windows") {
        expect_error(
          get_vigil_dir(),
          "Could not determine AppData directory"
        )
      }
    }
  )
})

test_that("cleanup_old_files removes old files correctly", {
  # Create temporary test directory
  test_dir <- withr::local_tempdir()

  # Create some test files with different ages
  old_file <- fs::path(test_dir, "old.json")
  new_file <- fs::path(test_dir, "new.json")
  active_file <- fs::path(test_dir, "watcher_active.json")

  writeLines("test", old_file)
  Sys.sleep(1)
  writeLines("test", new_file)
  writeLines(jsonlite::toJSON(list(id = "active")), active_file)

  # Artificially age the old file
  old_time <- Sys.time() - (8 * 24 * 60 * 60)  # 8 days old
  fs::file_chmod(old_file, "0644")
  system2("touch", c("-t", format(old_time, "%Y%m%d%H%M.%S"), old_file))

  # Run cleanup
  cleanup_old_files(test_dir)

  # Check results
  expect_false(fs::file_exists(old_file))
  expect_true(fs::file_exists(new_file))
  expect_true(fs::file_exists(active_file))
})

test_that("is_active_watcher_file identifies watcher files correctly", {
  # Create test files
  test_dir <- withr::local_tempdir()

  valid_files <- c(
    "watcher_123.json",
    "process_123.txt",
    "event_123_20240101.json"
  )

  invalid_files <- c(
    "something_123.txt",
    "watcher.json",
    "process_.txt"
  )

  # Create the files
  purrr::walk(c(valid_files, invalid_files), function(file) {
    writeLines("test", fs::path(test_dir, file))
  })

  # Test valid files
  purrr::walk(valid_files, function(file) {
    expect_true(
      is_active_watcher_file(fs::path(test_dir, file)),
      info = sprintf("Testing %s", file)
    )
  })

  # Test invalid files
  purrr::walk(invalid_files, function(file) {
    expect_false(
      is_active_watcher_file(fs::path(test_dir, file)),
      info = sprintf("Testing %s", file)
    )
  })
})

test_that("verify_vigil_process checks processes correctly", {
  skip_on_ci()

  # Create mock process
  mock <- withr::with_tempdir({
    script <- if (.Platform$OS.type == "windows") {
      'Start-Sleep -Seconds 30'
    } else {
      'sleep 30'
    }

    script_path <- "mock_process"
    writeLines(script, script_path)

    if (.Platform$OS.type != "windows") {
      fs::file_chmod(script_path, "0755")
    }

    px <- processx::process$new(
      command = if (.Platform$OS.type == "windows") "powershell" else "bash",
      args = c(if (.Platform$OS.type == "windows") "-File" else "-c", script_path)
    )

    list(
      process = px,
      path = script_path
    )
  })

  # Clean up process when done
  withr::defer({
    if (mock$process$is_alive()) {
      mock$process$kill()
    }
  })

  # Test process verification
  expect_true(verify_vigil_process(mock$process$get_pid(), "test"))

  # Test non-existent process
  expect_false(verify_vigil_process("99999999", "test"))
})

test_that("kill_process handles timeouts correctly", {
  skip_on_ci()

  # Create a stubborn process
  mock <- withr::with_tempdir({
    script <- if (.Platform$OS.type == "windows") {
      'while($true) { Start-Sleep -Milliseconds 100 }'
    } else {
      'while true; do sleep 0.1; done'
    }

    script_path <- "stubborn_process"
    writeLines(script, script_path)

    if (.Platform$OS.type != "windows") {
      fs::file_chmod(script_path, "0755")
    }

    px <- processx::process$new(
      command = if (.Platform$OS.type == "windows") "powershell" else "bash",
      args = c(if (.Platform$OS.type == "windows") "-File" else "-c", script_path)
    )

    list(
      process = px,
      path = script_path
    )
  })

  # Clean up process when done
  withr::defer({
    if (mock$process$is_alive()) {
      mock$process$kill()
    }
  })

  # Test timeout handling
  pid <- mock$process$get_pid()

  # Short timeout should fail gracefully
  start_time <- Sys.time()
  result <- kill_process(pid, "test", timeout = 0.1)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_false(result)
  expect_true(elapsed < 1)  # Should return quickly
  expect_true(mock$process$is_alive())  # Process should still be running

  # Longer timeout should succeed
  result <- kill_process(pid, "test", timeout = 5)
  expect_true(result)
  expect_false(mock$process$is_alive())
})

test_that("cleanup_watcher_files respects persistent flag", {
  # Create test directory
  test_dir <- withr::local_tempdir()

  # Create mock watcher files
  watcher_id <- "test123"
  config <- list(
    id = watcher_id,
    persistent = TRUE
  )

  config_file <- fs::path(test_dir, sprintf("watcher_%s.json", watcher_id))
  process_file <- fs::path(test_dir, sprintf("process_%s.txt", watcher_id))
  event_file <- fs::path(test_dir, sprintf("event_%s_1.json", watcher_id))

  jsonlite::write_json(config, config_file)
  writeLines("12345 test", process_file)
  writeLines("test", event_file)

  # Test cleanup without force
  cleanup_watcher_files(watcher_id)
  expect_true(fs::file_exists(config_file))
  expect_true(fs::file_exists(process_file))
  expect_true(fs::file_exists(event_file))

  # Test cleanup with force
  cleanup_watcher_files(watcher_id, force = TRUE)
  expect_false(fs::file_exists(config_file))
  expect_false(fs::file_exists(process_file))
  expect_false(fs::file_exists(event_file))
})

# Platform-specific tests

if (.Platform$OS.type == "windows") {
  test_that("kill_windows_process handles processes correctly", {
    skip_on_ci()

    # Create test process
    px <- processx::process$new(
      "cmd.exe",
      c("/c", "timeout /t 30")
    )

    # Test normal termination
    expect_true(kill_windows_process(px$get_pid(), 5))
    expect_false(px$is_alive())
  })
} else {
  test_that("kill_unix_process handles processes correctly", {
    skip_on_ci()

    # Create test process
    px <- processx::process$new(
      "sleep",
      "30"
    )

    # Test normal termination
    expect_true(kill_unix_process(px$get_pid(), 5))
    expect_false(px$is_alive())
  })
}
