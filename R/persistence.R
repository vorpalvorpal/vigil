#' Verify if a watcher is running as a persistent service
#'
#' @param id Watcher identifier
#' @return Logical indicating if service is active
#' @keywords internal
verify_persistent <- function(id) {
  checkmate::assert_string(id)

  db_path <- fs::path(get_vigil_dir(), sprintf("watcher_%s.db", id))
  if (!fs::file_exists(db_path)) {
    return(FALSE)
  }

  # Platform-specific verification
  if (.Platform$OS.type == "windows") {
    verify_persistent_windows(id)
  } else if (Sys.info()["sysname"] == "Darwin") {
    verify_persistent_macos(id)
  } else {
    verify_persistent_linux(id)
  }
}

#' Register a watcher as a persistent service
#'
#' @param id Watcher identifier
#' @return Logical indicating success
#' @keywords internal
register_persistent <- function(id) {
  checkmate::assert_string(id)

  # Get database path
  db_path <- fs::path(get_vigil_dir(), sprintf("watcher_%s.db", id))
  if (!fs::file_exists(db_path)) {
    cli::cli_abort("Watcher database not found")
  }

  # Get watcher script path
  watcher_script <- get_script_path(
    if (.Platform$OS.type == "windows") "watch-files.vbs" else "watch-files.sh"
  )

  # Platform-specific registration
  success <- if (.Platform$OS.type == "windows") {
    register_persistent_windows(id, db_path, watcher_script)
  } else if (Sys.info()["sysname"] == "Darwin") {
    register_persistent_macos(id, db_path, watcher_script)
  } else {
    register_persistent_linux(id, db_path, watcher_script)
  }

  if (!success) {
    cli::cli_abort("Failed to register persistent watcher")
  }

  success
}

#' Unregister a persistent watcher service
#'
#' @param id Watcher identifier
#' @return Logical indicating success
#' @keywords internal
unregister_persistent <- function(id) {
  checkmate::assert_string(id)

  # Platform-specific unregistration
  if (.Platform$OS.type == "windows") {
    unregister_persistent_windows(id)
  } else if (Sys.info()["sysname"] == "Darwin") {
    unregister_persistent_macos(id)
  } else {
    unregister_persistent_linux(id)
  }
}

# Windows implementation ------------------------------------------------------

verify_persistent_windows <- function(id) {
  if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
    cli::cli_abort(c(
      "taskscheduleR package is required for persistent watchers on Windows",
      "i" = "Install taskscheduleR with: install.packages('taskscheduleR')"
    ))
  }

  task_name <- sprintf("vigil_watcher_%s", id)

  tryCatch({
    tasks <- taskscheduleR::taskscheduler_ls()
    if (length(tasks) == 0) {
      return(FALSE)
    }

    task_info <- tasks[tasks$TaskName == task_name, ]
    if (nrow(task_info) == 0) {
      return(FALSE)
    }

    task_info$Status == "Running"
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to verify Windows task status",
      "x" = e$message,
      "i" = "Check Task Scheduler permissions"
    ))
    FALSE
  })
}

register_persistent_windows <- function(id, db_path, watcher_script) {
  if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
    cli::cli_abort(c(
      "taskscheduleR package is required for persistent watchers on Windows",
      "i" = "Install taskscheduleR with: install.packages('taskscheduleR')"
    ))
  }

  task_name <- sprintf("vigil_watcher_%s", id)

  tryCatch({
    taskscheduleR::taskscheduler_create(
      taskname = task_name,
      rscript = sprintf(
        'shell("cscript //NoLogo %s %s")',
        watcher_script,
        db_path
      ),
      schedule = "ONSTART",
      starttime = format(Sys.time(), "%H:%M"),
      force = TRUE
    )
    TRUE
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to register Windows task",
      "x" = e$message,
      "i" = "Check Task Scheduler permissions"
    ))
    FALSE
  })
}

unregister_persistent_windows <- function(id) {
  if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
    return(FALSE)
  }

  task_name <- sprintf("vigil_watcher_%s", id)

  tryCatch({
    taskscheduleR::taskscheduler_delete(task_name)
    TRUE
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to unregister Windows task",
      "x" = e$message,
      "i" = "Check Task Scheduler permissions"
    ))
    FALSE
  })
}

# Linux implementation -----------------------------------------------------

verify_persistent_linux <- function(id) {
  checkmate::assert_string(id)
  service_name <- sprintf("vigil-watcher-%s.service", id)

  tryCatch({
    status <- system2(
      "systemctl",
      c("--user", "is-active", service_name),
      stdout = TRUE,
      stderr = TRUE
    )
    identical(status, "active")
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to verify systemd service status",
      "x" = e$message,
      "i" = "Check if systemd is running: 'systemctl --user status'"
    ))
    FALSE
  })
}

register_persistent_linux <- function(id, db_path, watcher_script) {
  checkmate::assert_string(id)
  service_name <- sprintf("vigil-watcher-%s.service", id)
  service_path <- fs::path("~/.config/systemd/user", service_name)

  # Create systemd service file
  service_content <- sprintf(
    "[Unit]
Description=Vigil file watcher %s
After=network.target

[Service]
Type=simple
ExecStart=%s %s
Restart=on-failure
Environment=HOME=%s

[Install]
WantedBy=default.target",
    id,
    watcher_script,
    db_path,
    Sys::getenv("HOME")
  )

  tryCatch({
    # Ensure systemd user directory exists
    service_dir <- fs::path_dir(service_path)
    if (!fs::dir_exists(service_dir)) {
      fs::dir_create(service_dir)
    }

    # Check write permissions
    if (!fs::file_access(service_dir, mode = "write")) {
      cli::cli_abort(c(
        "Cannot write to systemd user directory",
        "x" = "No write permission for {.path {service_dir}}",
        "i" = "Check directory permissions and ownership"
      ))
    }

    writeLines(service_content, service_path)

    # Enable and start service with error checking
    result <- system2("systemctl", c("--user", "daemon-reload"),
                      stdout = TRUE, stderr = TRUE)
    if (result != 0) {
      cli::cli_abort("Failed to reload systemd configuration")
    }

    result <- system2("systemctl", c("--user", "enable", service_name),
                      stdout = TRUE, stderr = TRUE)
    if (result != 0) {
      cli::cli_abort("Failed to enable systemd service")
    }

    result <- system2("systemctl", c("--user", "start", service_name),
                      stdout = TRUE, stderr = TRUE)
    if (result != 0) {
      cli::cli_abort("Failed to start systemd service")
    }

    # Verify service started
    if (!verify_persistent_linux(id)) {
      cli::cli_abort(c(
        "Service failed to start",
        "i" = "Check service logs with: systemctl --user status {service_name}"
      ))
    }

    TRUE
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to register Linux service",
      "x" = e$message,
      "i" = "Check systemd user service configuration"
    ))
    FALSE
  })
}

unregister_persistent_linux <- function(id) {
  service_name <- sprintf("vigil-watcher-%s.service", id)
  service_path <- fs::path("~/.config/systemd/user", service_name)

  tryCatch({
    # Stop and disable service
    system2("systemctl", c("--user", "stop", service_name),
            stdout = TRUE, stderr = TRUE)
    system2("systemctl", c("--user", "disable", service_name),
            stdout = TRUE, stderr = TRUE)

    # Remove service file
    if (fs::file_exists(service_path)) {
      fs::file_delete(service_path)
    }

    # Reload systemd
    system2("systemctl", c("--user", "daemon-reload"),
            stdout = TRUE, stderr = TRUE)

    TRUE
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to unregister Linux service",
      "x" = e$message,
      "i" = "Check systemd user service status"
    ))
    FALSE
  })
}

# macOS implementation ---------------------------------------------------

verify_persistent_macos <- function(id) {
  agent_label <- sprintf("com.r-lib.vigil.watcher.%s", id)

  tryCatch({
    result <- system2(
      "launchctl",
      c("list", agent_label),
      stdout = TRUE,
      stderr = TRUE
    )
    attr(result, "status") == 0
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to verify launchd agent status",
      "x" = e$message,
      "i" = "Check if launchd is running"
    ))
    FALSE
  })
}

register_persistent_macos <- function(id, db_path, watcher_script) {
  agent_label <- sprintf("com.r-lib.vigil.watcher.%s", id)
  plist_path <- fs::path(
    "~/Library/LaunchAgents",
    sprintf("%s.plist", agent_label)
  )

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
    watcher_script,
    db_path,
    fs::path(get_vigil_dir(), sprintf("stdout_%s.log", id)),
    fs::path(get_vigil_dir(), sprintf("stderr_%s.log", id)),
    Sys::getenv("HOME")
  )

  tryCatch({
    # Write plist
    agent_dir <- fs::path_dir(plist_path)
    if (!fs::dir_exists(agent_dir)) {
      fs::dir_create(agent_dir)
    }

    # Check write permissions
    if (!fs::file_access(agent_dir, mode = "write")) {
      cli::cli_abort(c(
        "Cannot write to LaunchAgents directory",
        "x" = "No write permission for {.path {agent_dir}}",
        "i" = "Check directory permissions and ownership"
      ))
    }

    writeLines(plist_content, plist_path)

    # Load agent
    result <- system2("launchctl", c("load", "-w", plist_path),
                      stdout = TRUE, stderr = TRUE)
    if (result != 0) {
      cli::cli_abort("Failed to load launchd agent")
    }

    # Verify agent started
    if (!verify_persistent_macos(id)) {
      cli::cli_abort(c(
        "Agent failed to start",
        "i" = "Check logs in {.path {get_vigil_dir()}}"
      ))
    }

    TRUE
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to register macOS agent",
      "x" = e$message,
      "i" = "Check launchd configuration"
    ))
    FALSE
  })
}

unregister_persistent_macos <- function(id) {
  agent_label <- sprintf("com.r-lib.vigil.watcher.%s", id)
  plist_path <- fs::path(
    "~/Library/LaunchAgents",
    sprintf("%s.plist", agent_label)
  )

  tryCatch({
    # Unload agent
    system2("launchctl", c("unload", "-w", plist_path),
            stdout = TRUE, stderr = TRUE)

    # Remove plist file
    if (fs::file_exists(plist_path)) {
      fs::file_delete(plist_path)
    }

    # Clean up logs
    files_to_clean <- c(
      fs::path(get_vigil_dir(), sprintf("stdout_%s.log", id)),
      fs::path(get_vigil_dir(), sprintf("stderr_%s.log", id))
    )

    purrr::walk(files_to_clean, function(file) {
      if (fs::file_exists(file)) {
        fs::file_delete(file)
      }
    })

    TRUE
  }, error = function(e) {
    cli::cli_warn(c(
      "Failed to unregister macOS agent",
      "x" = e$message,
      "i" = "Check launchd status"
    ))
    FALSE
  })
}
