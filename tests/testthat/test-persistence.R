library(testthat)
library(vigil)

# Helper functions
create_test_config <- function(dir = tempdir()) {
  list(
    id = uuid::UUIDgenerate(),
    path = dir,
    pattern = ".*\\.txt$",
    recursive = FALSE,
    watch_mode = "persistent",
    change_type = "any",
    persistent = TRUE,
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

# Windows-specific tests
test_that("Windows persistence management works correctly", {
  skip_if_not(.Platform$OS.type == "windows")
  skip_on_ci()

  test_dir <- withr::local_tempdir()
  config <- create_test_config(test_dir)

  # Test registration
  expect_true(register_persistent_windows(config))

  # Verify task was created
  task_name <- sprintf("vigil_watcher_%s", config$id)
  tasks <- taskscheduleR::taskscheduler_ls()
  expect_true(any(tasks$TaskName == task_name))

  # Verify watcher is running
  expect_true(verify_persistent_windows(config))

  # Test unregistration
  expect_true(unregister_persistent_windows(config))

  # Verify cleanup
  tasks <- taskscheduleR::taskscheduler_ls()
  expect_false(any(tasks$TaskName == task_name))

  script_path <- fs::path(get_vigil_dir(), sprintf("watch_%s.vbs", config$id))
  expect_false(fs::file_exists(script_path))
})

# Linux-specific tests
test_that("Linux persistence management works correctly", {
  skip_if_not(Sys.info()["sysname"] == "Linux")
  skip_on_ci()

  test_dir <- withr::local_tempdir()
  config <- create_test_config(test_dir)

  # Mock systemctl commands
  mockery::stub(register_persistent_linux, "system2",
                function(command, args) {
                  if (command == "systemctl") {
                    if ("daemon-reload" %in% args) return(0)
                    if ("enable" %in% args) return(0)
                    if ("start" %in% args) return(0)
                  }
                  0
                }
  )

  mockery::stub(verify_persistent_linux, "system2",
                function(command, args) {
                  if (command == "systemctl" && "is-active" %in% args) {
                    return("active")
                  }
                  ""
                }
  )

  # Test registration
  expect_true(register_persistent_linux(config))

  # Verify service file was created
  service_name <- sprintf("vigil-watcher-%s.service", config$id)
  service_path <- fs::path("~/.config/systemd/user", service_name)
  expect_true(fs::file_exists(service_path))

  # Verify watcher is running
  expect_true(verify_persistent_linux(config))

  # Test unregistration
  expect_true(unregister_persistent_linux(config))

  # Verify cleanup
  expect_false(fs::file_exists(service_path))

  script_path <- fs::path(get_vigil_dir(), sprintf("watch_%s.sh", config$id))
  expect_false(fs::file_exists(script_path))
})

# macOS-specific tests
test_that("macOS persistence management works correctly", {
  skip_if_not(Sys.info()["sysname"] == "Darwin")
  skip_on_ci()

  test_dir <- withr::local_tempdir()
  config <- create_test_config(test_dir)

  # Mock launchctl commands
  mockery::stub(register_persistent_macos, "system2",
                function(command, args) {
                  if (command == "launchctl") {
                    if ("load" %in% args) return(0)
                  }
                  0
                }
  )

  mockery::stub(verify_persistent_macos, "system2",
                function(command, args) {
                  if (command == "launchctl" && "list" %in% args) {
                    structure("", status = 0)
                  } else {
                    structure("", status = 1)
                  }
                }
  )

  # Test registration
  expect_true(register_persistent_macos(config))

  # Verify plist file was created
  agent_label <- sprintf("com.r-lib.vigil.watcher.%s", config$id)
  plist_path <- fs::path(
    "~/Library/LaunchAgents",
    sprintf("%s.plist", agent_label)
  )
  expect_true(fs::file_exists(plist_path))

  # Verify watcher is running
  expect_true(verify_persistent_macos(config))

  # Test unregistration
  expect_true(unregister_persistent_macos(config))

  # Verify cleanup
  expect_false(fs::file_exists(plist_path))

  script_path <- fs::path(get_vigil_dir(), sprintf("watch_%s.sh", config$id))
  expect_false(fs::file_exists(script_path))
})

# Cross-platform persistence tests
test_that("Persistent watchers survive R session restart", {
  skip_on_ci()

  test_dir <- withr::local_tempdir()

  # Start persistent watcher
  id <- watch(test_dir, watch_mode = "persistent")
  expect_true(id %in% list_watchers()$id)

  # Simulate R session restart
  rm(list = ls(envir = asNamespace("vigil")), envir = asNamespace("vigil"))
  vigil:::.onLoad(NULL, "vigil")

  # Verify watcher still exists
  expect_true(id %in% list_watchers()$id)

  # Clean up
  kill_watcher(id)
  expect_false(id %in% list_watchers()$id)
})

test_that("Persistent watchers handle file events correctly", {
  skip_on_ci()

  test_dir <- withr::local_tempdir()
  events_file <- fs::path(test_dir, "events.txt")

  # Create callback script that logs events
  callback_script <- fs::path(test_dir, "callback.R")
  writeLines(sprintf('
    writeLines(
      sprintf("%%s: %%s", event$change_type, basename(event$path)),
      "%s",
      append = TRUE
    )
  ', events_file), callback_script)

  # Start persistent watcher
  id <- watch(
    test_dir,
    watch_mode = "persistent",
    callback = callback_script
  )

  # Create test file
  Sys.sleep(1)
  test_file <- fs::path(test_dir, "test.txt")
  writeLines("test", test_file)

  # Wait for event processing
  Sys.sleep(1)

  # Check events were logged
  expect_true(fs::file_exists(events_file))
  events <- readLines(events_file)
  expect_true(any(grepl("created: test.txt", events)))

  # Clean up
  kill_watcher(id)
})

test_that("Persistent watchers handle errors gracefully", {
  skip_on_ci()

  test_dir <- withr::local_tempdir()

  # Test invalid watch_mode
  expect_error(
    watch(test_dir, watch_mode = "invalid"),
    "should be one of"
  )

  # Test registration with invalid directory
  expect_error(
    watch("/nonexistent/path", watch_mode = "persistent"),
    "Directory.*not exist"
  )

  # Test with invalid callback
  expect_error(
    watch(test_dir,
          watch_mode = "persistent",
          callback = "/nonexistent/script.R"),
    "Invalid callback"
  )
})

test_that("list_watchers handles persistent watchers correctly", {
  skip_on_ci()

  test_dir <- withr::local_tempdir()

  # Create mix of persistent and non-persistent watchers
  id_persistent <- watch(test_dir, watch_mode = "persistent")
  id_normal <- watch(test_dir)

  # Check watcher list
  watchers <- list_watchers()

  persistent_watcher <- dplyr::filter(watchers, id == id_persistent)
  normal_watcher <- dplyr::filter(watchers, id == id_normal)

  expect_true(persistent_watcher$persistent)
  expect_false(normal_watcher$persistent)

  # Clean up
  kill_watcher(id_persistent)
  kill_watcher(id_normal)
})
