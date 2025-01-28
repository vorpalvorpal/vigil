#' Watch directory for file changes
#'
#' Sets up a file system watcher to monitor changes in a specified directory.
#'
#' @param path Directory path to watch
#' @param pattern Optional file pattern to match (e.g. "*.csv")
#' @param recursive Whether to watch subdirectories (default: FALSE)
#' @param callback Function to call when files change. Can be:
#'   \itemize{
#'     \item A function or expression
#'     \item A package function name (e.g. "dplyr::select")
#'     \item Path to an R script
#'   }
#' @param persistent Whether to create a persistent watcher that survives R session
#'   restarts (default: FALSE)
#' @return Invisibly returns the watcher ID (character string)
#' @examples
#' \dontrun{
#' # Watch for all changes
#' watch("~/Documents")
#'
#' # Watch CSV files only
#' watch("~/Data", pattern = "*.csv")
#'
#' # Watch with callback
#' watch("~/Downloads", callback = function(event) {
#'   message("File changed: ", event$path)
#' })
#'
#' # Watch with package function
#' watch("~/Data", callback = "dplyr::glimpse")
#'
#' # Persistent watcher
#' watch("~/important", persistent = TRUE)
#' }
#' @export
watch <- function(path, pattern = NULL,
                  recursive = FALSE,
                  callback = NULL,
                  mode = "continuous",
                  change_type = "any") {
  # Validate inputs
  checkmate::assert_directory_exists(path)
  checkmate::assert_character(pattern, null.ok = TRUE)
  checkmate::assert_flag(recursive)
  checkmate::assert_choice(mode, c("single", "continuous", "persistent"), null.ok = FALSE)
  checkmate::assert_choice(change_type, c("created", "modified", "deleted", "any"))

  # Create unique ID for this watcher
  id <- uuid::UUIDgenerate()

  # Create config
  config <- list(
    id = id,
    path = fs::path_norm(path),
    pattern = pattern,
    recursive = recursive,
    callback = validate_callback(callback),
    watch_mode = mode,
    change_type = change_type,
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # Write config file
  config_file <- fs::path(get_vigil_dir(), sprintf("watcher_%s.json", id))
  jsonlite::write_json(config, config_file, auto_unbox = TRUE)

  if (mode == "persistent") {
    # Launch persistent watcher based on OS
    if (.Platform$OS.type == "windows") {
      register_persistent_windows(config)
    } else if (Sys.info()["sysname"] == "Darwin") {
      register_persistent_macos(config)
    } else {
      register_persistent_linux(config)
    }
  } else {
    print("Persistent: ELSE")
    # Launch regular watcher based on OS
    if (.Platform$OS.type == "windows") {
      watch_files_windows(config)
    } else {
      print("Unix: ELSE")
      watch_files_unix(config)
      print("end unix ELSE")
    }
  }

  cli::cli_alert_success(
    "Started {if_else(persistent, '/regular} watching {.path {path}}"
  )
  invisible(id)
}

#' List active file watchers
#'
#' Returns information about all currently active file watchers.
#'
#' @return tibble with columns:
#'   \describe{
#'     \item{id}{Watcher identifier}
#'     \item{path}{Directory being watched}
#'     \item{pattern}{File pattern (NA if none)}
#'     \item{recursive}{Whether watching subdirectories}
#'     \item{persistent}{Whether watcher is persistent}
#'     \item{created}{When watcher was created}
#'   }
#' @examples
#' \dontrun{
#' # List all watchers
#' list_watchers()
#'
#' # Check for persistent watchers
#' subset(list_watchers(), persistent)
#' }
#' @export
list_watchers <- function() {
  # List all watcher config files
  vigil_dir <- get_vigil_dir()
  config_files <- fs::dir_ls(vigil_dir, glob = "watcher_*.json")

  if (length(config_files) == 0) {
    return(tibble::tibble(
      id = character(),
      path = character(),
      pattern = character(),
      recursive = logical(),
      persistent = logical(),
      created = character()
    ))
  }

  # Read configs and check if processes still exist
  purrr::map_dfr(config_files, function(file) {
    config <- jsonlite::read_json(file)

    # For persistent watchers, check if they're still registered
    if (isTRUE(config$persistent)) {
      is_active <- if (.Platform$OS.type == "windows") {
        verify_persistent_windows(config)
      } else if (Sys.info()["sysname"] == "Darwin") {
        verify_persistent_macos(config)
      } else {
        verify_persistent_linux(config)
      }

      if (!is_active) {
        # Clean up dead persistent watcher
        cleanup_watcher_files(config$id, force = TRUE)
        return(NULL)
      }
    } else {
      # Check regular watcher process
      process_file <- fs::path(vigil_dir, sprintf("process_%s.txt", config$id))

      if (fs::file_exists(process_file)) {
        process_info <- readLines(process_file)
        pid <- strsplit(process_info, " ")[[1]][1]

        if (!verify_vigil_process(pid, config$id)) {
          # Clean up dead watcher
          cleanup_watcher_files(config$id)
          return(NULL)
        }
      } else {
        cleanup_watcher_files(config$id)
        return(NULL)
      }
    }

    # Return watcher info
    tibble::tibble(
      id = config$id,
      path = config$path,
      pattern = if(is.null(config$pattern)) NA_character_ else config$pattern,
      recursive = config$recursive,
      persistent = isTRUE(config$persistent),
      created = config$created
    )
  })
}

#' Watch until specific file changes occur
#'
#' Sets up a temporary file watcher that runs until specified changes occur or
#' timeout is reached.
#'
#' @param path Directory path to watch
#' @param pattern Optional file pattern to match (e.g. "*.csv")
#' @param change_type Type of change to watch for: "created", "modified",
#'   "deleted", or "any" (default: "any")
#' @param timeout Timeout in seconds. NULL means wait indefinitely
#' @return tibble with columns:
#'   \describe{
#'     \item{path}{Full path to changed file}
#'     \item{name}{File name}
#'     \item{change_type}{Type of change detected}
#'     \item{timestamp}{When change occurred}
#'   }
#'   Returns NULL if timeout is reached before any changes
#' @examples
#' \dontrun{
#' # Wait for any change
#' watch_until("~/Documents")
#'
#' # Wait for new CSV file (10 second timeout)
#' watch_until("~/Data",
#'            pattern = "*.csv",
#'            change_type = "created",
#'            timeout = 10)
#' }
#' @export
watch_until <- function(path, pattern = NULL, change_type = "any", timeout = NULL) {
  checkmate::assert_directory_exists(path)
  checkmate::assert_character(pattern, null.ok = TRUE)
  checkmate::assert_choice(change_type, c("created", "modified", "deleted", "any"))
  checkmate::assert_number(timeout, null.ok = TRUE, lower = 0)

  # Create watcher ID and config
  id <- uuid::UUIDgenerate()
  config <- list(
    id = id,
    path = fs::path_norm(path),
    pattern = pattern,
    recursive = FALSE,
    watch_mode = "single",
    change_type = change_type,
    no_cleanup = "true",  # We'll handle cleanup ourselves
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # Write config
  config_file <- fs::path(get_vigil_dir(), sprintf("watcher_%s.json", id))
  jsonlite::write_json(config, config_file, auto_unbox = TRUE)

  # Start platform-specific watcher process
  if (.Platform$OS.type == "windows") {
    # Create and run VBScript
    script <- create_windows_watcher_script(config)
    script_path <- fs::path(get_vigil_dir(), sprintf("watch_%s.vbs", id))
    writeLines(script, script_path)

    px <- processx::process$new(
      command = "cscript",
      args = c("//NoLogo", script_path),
      stdout = "|",
      stderr = "|"
    )
  } else {
    # Create and run shell script
    script <- create_unix_watcher_script(config)
    script_path <- fs::path(get_vigil_dir(), sprintf("watch_%s.sh", id))
    writeLines(script, script_path)
    fs::chmod(script_path, "0755")

    px <- processx::process$new(
      command = script_path,
      stdout = "|",
      stderr = "|"
    )
  }

  # Cleanup function
  cleanup <- function() {
    if (px$is_alive()) px$kill()
    cleanup_watcher_files(id)
  }
  on.exit(cleanup())

  # Wait for process to exit (indicating event occurred) or timeout
  if (is.null(timeout)) {
    px$wait()
  } else {
    if (!px$wait(timeout * 1000)) {  # milliseconds
      return(NULL)
    }
  }

  # Process has exited - find and read the event file
  event_files <- fs::dir_ls(
    get_vigil_dir(),
    glob = sprintf("event_%s_*.json", id)
  )

  if (length(event_files) == 0) {
    return(NULL)
  }

  # Get most recent event file
  event_file <- event_files[which.max(file.info(event_files)$mtime)]
  event <- jsonlite::read_json(event_file)

  # Return tibble with event details
  tibble::tibble(
    path = event$path,
    name = basename(event$path),
    change_type = event$type,
    timestamp = as.POSIXct(event$timestamp)
  )
}

#' Kill a file watcher
#'
#' Stops a specific file watcher and cleans up associated resources.
#'
#' @param id Watcher ID to kill
#' @return Invisibly returns TRUE if successful, FALSE otherwise
#' @examples
#' \dontrun{
#' # Start a watcher
#' id <- watch("~/Documents")
#'
#' # Later, kill it
#' kill_watcher(id)
#' }
#' @export
kill_watcher <- function(id) {
  process_file <- fs::path(get_vigil_dir(), sprintf("process_%s.txt", id))

  if (!fs::file_exists(process_file)) {
    cli::cli_alert_warning("Watcher {.val {id}} not found")
    return(invisible(FALSE))
  }

  process_info <- readLines(process_file)
  pid <- strsplit(process_info, " ")[[1]][1]

  if (kill_process(pid, id)) {
    # Give process time to clean up its own files
    Sys.sleep(1)

    # Clean up any remaining files
    cleanup_watcher_files(id)

    cli::cli_alert_success("Killed watcher {.val {id}}")
    invisible(TRUE)
  } else {
    cli::cli_alert_warning("Failed to kill watcher {.val {id}}")
    invisible(FALSE)
  }
}

#' Kill all file watchers
#'
#' Stops all active file watchers and cleans up their resources.
#'
#' @examples
#' \dontrun{
#' # Kill all watchers
#' kill_all_watchers()
#' }
#' @export
kill_all_watchers <- function() {
  watchers <- list_watchers()
  if (nrow(watchers) == 0) {
    cli::cli_alert_info("No active watchers found")
    return(invisible())
  }

  purrr::walk(watchers$id, kill_watcher)
}
