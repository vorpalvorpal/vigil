# Windows persistence management

#' Verify if a Windows persistent watcher is active
#' @param config Watcher configuration
#' @return Boolean indicating if watcher is active
#' @keywords internal
verify_persistent_windows <- function(config) {
  checkmate::assert_list(config)
  checkmate::assert_string(config$id)

  # Check if taskscheduleR is available
  if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
    cli::cli_abort(c(
      "taskscheduleR package is required for persistent watchers on Windows",
      "i" = "Install taskscheduleR with: install.packages('taskscheduleR')"
    ))
  }

  # Task name uses the watcher ID for uniqueness
  task_name <- sprintf("vigil_watcher_%s", config$id)

  # Use taskscheduleR to check if task exists and is running
  tasks <- taskscheduleR::taskscheduler_ls()

  if (length(tasks) == 0) {
    return(FALSE)
  }

  # Find our task
  task_info <- tasks[tasks$TaskName == task_name, ]

  if (nrow(task_info) == 0) {
    return(FALSE)
  }

  # Check if task is running (Status will be "Running" if active)
  task_info$Status == "Running"
}

#' Register a Windows persistent watcher
#' @param config Watcher configuration
#' @return TRUE if successful
#' @keywords internal
register_persistent_windows <- function(config) {
  checkmate::assert_list(config)
  checkmate::assert_string(config$id)

  # Check if taskscheduleR is available
  if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
    cli::cli_abort(c(
      "taskscheduleR package is required for persistent watchers on Windows",
      "i" = "Install taskscheduleR with: install.packages('taskscheduleR')"
    ))
  }

  # Create a VBScript watcher as before
  script <- create_windows_watcher_script(config)
  script_path <- fs::path(get_vigil_dir(), sprintf("watch_%s.vbs", config$id))

  tryCatch({
    # Write the script
    writeLines(script, script_path)

    # Create task name
    task_name <- sprintf("vigil_watcher_%s", config$id)

    # Schedule task to run the VBScript
    taskscheduleR::taskscheduler_create(
      taskname = task_name,
      rscript = script_path,
      schedule = "ONCE",
      starttime = format(Sys.time(), "%H:%M"),
      startdate = format(Sys.Date(), "%d/%m/%Y"),
      modifier = "AFTERSTART",  # Keep running after start
      force = TRUE  # Replace if exists
    )

    TRUE
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to register persistent watcher",
      "x" = e$message
    ))
  })
}

#' Unregister a Windows persistent watcher
#' @param config Watcher configuration
#' @return TRUE if successful
#' @keywords internal
unregister_persistent_windows <- function(config) {
  checkmate::assert_list(config)
  checkmate::assert_string(config$id)

  # Check if taskscheduleR is available
  if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
    cli::cli_abort(c(
      "taskscheduleR package is required for persistent watchers on Windows",
      "i" = "Install taskscheduleR with: install.packages('taskscheduleR')"
    ))
  }

  task_name <- sprintf("vigil_watcher_%s", config$id)

  tryCatch({
    # Delete the scheduled task
    taskscheduleR::taskscheduler_delete(task_name)

    # Clean up the script file
    script_path <- fs::path(get_vigil_dir(), sprintf("watch_%s.vbs", config$id))
    if (fs::file_exists(script_path)) {
      fs::file_delete(script_path)
    }

    TRUE
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to unregister persistent watcher",
      "x" = e$message
    ))
    FALSE
  })
}

# Linux persistence management

#' Verify if a Linux persistent watcher is active
#' @param config Watcher configuration
#' @return Boolean indicating if watcher is active
#' @keywords internal
verify_persistent_linux <- function(config) {
  checkmate::assert_list(config)
  checkmate::assert_string(config$id)

  service_name <- sprintf("vigil-watcher-%s.service", config$id)

  tryCatch({
    status <- system2("systemctl",
                      c("--user", "is-active", service_name),
                      stdout = TRUE,
                      stderr = TRUE)
    identical(status, "active")
  }, error = function(e) {
    FALSE
  })
}

#' Register a Linux persistent watcher
#' @param config Watcher configuration
#' @return TRUE if successful
#' @keywords internal
register_persistent_linux <- function(config) {
  checkmate::assert_list(config)
  checkmate::assert_string(config$id)

  service_name <- sprintf("vigil-watcher-%s.service", config$id)
  service_path <- fs::path("~/.config/systemd/user", service_name)

  # Create shell script watcher
  script <- create_unix_watcher_script(config)
  script_path <- fs::path(get_vigil_dir(), sprintf("watch_%s.sh", config$id))

  tryCatch({
    # Write and make executable
    writeLines(script, script_path)
    fs::file_chmod(script_path, "0755")

    # Create systemd service file
    service_content <- sprintf(
      "[Unit]
Description=Vigil file watcher %s
After=network.target

[Service]
Type=simple
ExecStart=%s
Restart=on-failure
Environment=HOME=%s

[Install]
WantedBy=default.target",
      config$id,
      script_path,
      Sys.getenv("HOME")
    )

    # Ensure systemd user directory exists
    fs::dir_create(fs::path_dir(service_path))
    writeLines(service_content, service_path)

    # Enable and start service
    system2("systemctl", c("--user", "daemon-reload"))
    system2("systemctl", c("--user", "enable", service_name))
    result <- system2("systemctl",
                      c("--user", "start", service_name),
                      stdout = TRUE,
                      stderr = TRUE)

    # Verify service started
    if (!verify_persistent_linux(config)) {
      cli::cli_abort("Service failed to start")
    }

    TRUE
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to register persistent watcher",
      "x" = e$message
    ))
  })
}

#' Unregister a Linux persistent watcher
#' @param config Watcher configuration
#' @return TRUE if successful
#' @keywords internal
unregister_persistent_linux <- function(config) {
  checkmate::assert_list(config)
  checkmate::assert_string(config$id)

  service_name <- sprintf("vigil-watcher-%s.service", config$id)
  service_path <- fs::path("~/.config/systemd/user", service_name)

  tryCatch({
    # Stop and disable service
    system2("systemctl", c("--user", "stop", service_name))
    system2("systemctl", c("--user", "disable", service_name))

    # Remove service file
    if (fs::file_exists(service_path)) {
      fs::file_delete(service_path)
    }

    # Reload systemd
    system2("systemctl", c("--user", "daemon-reload"))

    # Clean up script
    script_path <- fs::path(get_vigil_dir(), sprintf("watch_%s.sh", config$id))
    if (fs::file_exists(script_path)) {
      fs::file_delete(script_path)
    }

    TRUE
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to unregister persistent watcher",
      "x" = e$message
    ))
    FALSE
  })
}

# macOS persistence management

#' Verify if a macOS persistent watcher is active
#' @param config Watcher configuration
#' @return Boolean indicating if watcher is active
#' @keywords internal
verify_persistent_macos <- function(config) {
  checkmate::assert_list(config)
  checkmate::assert_string(config$id)

  agent_label <- sprintf("com.r-lib.vigil.watcher.%s", config$id)

  tryCatch({
    result <- system2("launchctl",
                      c("list", agent_label),
                      stdout = TRUE,
                      stderr = TRUE)
    # launchctl list returns 0 if service exists and is running
    attr(result, "status") == 0
  }, error = function(e) {
    FALSE
  })
}

#' Register a macOS persistent watcher
#' @param config Watcher configuration
#' @return TRUE if successful
#' @keywords internal
register_persistent_macos <- function(config) {
  checkmate::assert_list(config)
  checkmate::assert_string(config$id)

  agent_label <- sprintf("com.r-lib.vigil.watcher.%s", config$id)
  plist_path <- fs::path(
    "~/Library/LaunchAgents",
    sprintf("%s.plist", agent_label)
  )

  # Create shell script watcher
  script <- create_unix_watcher_script(config)
  script_path <- fs::path(get_vigil_dir(), sprintf("watch_%s.sh", config$id))

  tryCatch({
    # Write and make executable
    writeLines(script, script_path)
    fs::file_chmod(script_path, "0755")

    # Create plist file
    plist_content <- sprintf(
      '<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>%s</string>
    <key>ProgramArguments</key>
    <array>
        <string>%s</string>
    </array>
    <key>KeepAlive</key>
    <true/>
    <key>RunAtLoad</key>
    <true/>
    <key>StandardOutPath</key>
    <string>%s</string>
    <key>StandardErrorPath</key>
    <string>%s</string>
    <key>EnvironmentVariables</key>
    <dict>
        <key>HOME</key>
        <string>%s</string>
        <key>PATH</key>
        <string>/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin</string>
    </dict>
</dict>
</plist>',
      agent_label,
      script_path,
      fs::path(get_vigil_dir(), sprintf("stdout_%s.log", config$id)),
      fs::path(get_vigil_dir(), sprintf("stderr_%s.log", config$id)),
      Sys.getenv("HOME")
    )

    # Write plist
    fs::dir_create(fs::path_dir(plist_path))
    writeLines(plist_content, plist_path)

    # Load agent
    system2("launchctl", c("load", "-w", plist_path))

    # Verify agent started
    if (!verify_persistent_macos(config)) {
      cli::cli_abort("Agent failed to start")
    }

    TRUE
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to register persistent watcher",
      "x" = e$message
    ))
  })
}

#' Unregister a macOS persistent watcher
#' @param config Watcher configuration
#' @return TRUE if successful
#' @keywords internal
unregister_persistent_macos <- function(config) {
  checkmate::assert_list(config)
  checkmate::assert_string(config$id)

  agent_label <- sprintf("com.r-lib.vigil.watcher.%s", config$id)
  plist_path <- fs::path(
    "~/Library/LaunchAgents",
    sprintf("%s.plist", agent_label)
  )

  tryCatch({
    # Unload agent
    system2("launchctl", c("unload", "-w", plist_path))

    # Remove plist file
    if (fs::file_exists(plist_path)) {
      fs::file_delete(plist_path)
    }

    # Clean up script and logs
    files_to_clean <- c(
      fs::path(get_vigil_dir(), sprintf("watch_%s.sh", config$id)),
      fs::path(get_vigil_dir(), sprintf("stdout_%s.log", config$id)),
      fs::path(get_vigil_dir(), sprintf("stderr_%s.log", config$id))
    )

    purrr::walk(files_to_clean, function(file) {
      if (fs::file_exists(file)) {
        fs::file_delete(file)
      }
    })

    TRUE
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to unregister persistent watcher",
      "x" = e$message
    ))
    FALSE
  })
}
