#' Watch Files on Windows
#'
#' Creates and manages a file system watcher on Windows using VBScript and FileSystemWatcher.
#'
#' @param config A list containing watcher configuration:
#'   \describe{
#'     \item{id}{Unique identifier for the watcher}
#'     \item{path}{Directory path to watch}
#'     \item{pattern}{Optional file pattern to match (e.g. "*.csv")}
#'     \item{recursive}{Whether to watch subdirectories}
#'     \item{watch_mode}{"continuous" or "single" for event handling}
#'     \item{change_type}{Type of changes to watch for}
#'     \item{no_cleanup}{Whether to preserve event files}
#'   }
#' @return Invisibly returns NULL
#' @seealso \code{\link{watch_files_unix}} for Unix implementation
#' @examples
#' \dontrun{
#' config <- list(
#'   id = "test123",
#'   path = "~/Documents",
#'   pattern = "*.txt",
#'   recursive = TRUE
#' )
#' watch_files_windows(config)
#' }
#' @keywords internal
watch_files_windows <- function(config) {
  # Create VBScript for FileSystemWatcher
  script <- create_windows_watcher_script(config)
  script_path <- fs::path(
    get_vigil_dir(),
    sprintf("watch_%s.vbs", config$id)
  )

  tryCatch({
    # Ensure temp directory exists
    fs::dir_create(fs::path_dirname(script_path))
    writeLines(script, script_path)

    # Run VBScript
    system2(
      "cscript",
      args = c("//NoLogo", shQuote(script_path)),
      stdout = TRUE,
      stderr = TRUE,
      wait = FALSE
    )
  }, error = function(e) {
    cli::cli_alert_error(c(
      "Error in file watching:",
      x = e$message
    ))
  })
}

#' Watch Files on Unix
#'
#' Creates and manages a file system watcher on Unix-like systems using inotifywait
#' or fswatch.
#'
#' @param config A list containing watcher configuration (see watch_files_windows)
#' @return Invisibly returns NULL
#' @note Requires either inotify-tools (Linux) or fswatch (macOS) to be installed
#' @seealso \code{\link{watch_files_windows}} for Windows implementation
#' @examples
#' \dontrun{
#' config <- list(
#'   id = "test123",
#'   path = "~/Documents",
#'   pattern = "*.txt",
#'   recursive = TRUE
#' )
#' watch_files_unix(config)
#' }
#' @keywords internal
watch_files_unix <- function(config) {
  print("watch_files_unix")
  # Check for required tools
  has_inotify <-
    sys::exec_internal("command", args = c("-v", "inotifywait"), error = FALSE) |>
    purrr::pluck("stdout") |>
    paste0(collapse = "") |>
    nzchar()
  has_fswatch <-
    sys::exec_internal("command", args = c("-v", "fswatch"), error = FALSE) |>
    purrr::pluck("stdout") |>
    paste0(collapse = "") |>
    nzchar()

  if (!has_inotify && !has_fswatch) {
    cli::cli_abort(c(
      "File watching not supported on this system",
      "x" = "Neither inotifywait nor fswatch found",
      "i" = "Install inotify-tools package on Linux",
      "i" = "Install fswatch package on macOS"
    ))
  }

  # Create and run appropriate script
  script <- create_unix_watcher_script(config)
  script_path <- fs::path(
    get_vigil_dir(),
    sprintf("watch_%s.sh", config$id)
  )

  tryCatch({
    print("tryCatch")
    # Write and set permissions
    writeLines(script, script_path)
    fs::file_chmod(script_path, "0755")

    # Run script
    sys::exec_background(
      script_path,
      std_out = FALSE,
      std_err = FALSE
    )
  }, error = function(e) {
    cli::cli_alert_error(c(
      "Error in file watching:",
      x = e$message
    ))
  })
}

#' Create Windows watcher VBScript
#' @param config Watcher configuration
#' @return Character string containing VBScript code
#' @keywords internal
create_windows_watcher_script <- function(config) {
  # Read template
  template <-
    pkgload::inst("vigil") |>
    c("watch-files.vbs.template") |>
    fs::path_join() |>
    readLines()

  # Set defaults for new options if not provided
  config$watch_mode <- config$watch_mode %||% "continuous"
  config$change_type <- config$change_type %||% "any"
  config$no_cleanup <- config$no_cleanup %||% "false"

  # Replace template variables
  script <- paste(template, collapse = "\n")
  script <- gsub("{{WATCH_MODE}}", config$watch_mode, script, fixed = TRUE)
  script <- gsub("{{CHANGE_TYPE}}", config$change_type, script, fixed = TRUE)
  script <- gsub("{{NO_CLEANUP}}", config$no_cleanup, script, fixed = TRUE)
  script <- gsub("{{WATCHER_ID}}", config$id, script, fixed = TRUE)
  script <- gsub("{{VIGIL_DIR}}", get_vigil_dir(), script, fixed = TRUE)
  script <- gsub("{{WATCH_PATH}}", config$path, script, fixed = TRUE)
  script <- gsub("{{PATTERN}}", config$pattern %||% "*.*", script, fixed = TRUE)
  script <- gsub("{{RECURSIVE}}", if(config$recursive) "True" else "False", script, fixed = TRUE)

  script
}

#' Create Unix watcher shell script
#' @param config Watcher configuration
#' @return Character string containing shell script code
#' @keywords internal
create_unix_watcher_script <- function(config) {
  print("create_unix_watcher_script")
  # Read template
  template <-
    pkgload::inst("vigil") |>
    c("watch-files.sh.template") |>
    fs::path_join() |>
    readLines()

  # Set defaults for new options if not provided
  config$watch_mode <- config$watch_mode %||% "continuous"
  config$change_type <- config$change_type %||% "any"
  config$no_cleanup <- config$no_cleanup %||% "false"

  # Replace template variables
  script <- paste(template, collapse = "\n")
  script <- gsub("{{WATCH_MODE}}", config$watch_mode, script, fixed = TRUE)
  script <- gsub("{{CHANGE_TYPE}}", config$change_type, script, fixed = TRUE)
  script <- gsub("{{NO_CLEANUP}}", config$no_cleanup, script, fixed = TRUE)
  script <- gsub("{{WATCHER_ID}}", config$id, script, fixed = TRUE)
  script <- gsub("{{VIGIL_DIR}}", get_vigil_dir(), script, fixed = TRUE)
  script <- gsub("{{WATCH_PATH}}", config$path, script, fixed = TRUE)

  # Handle recursive flag
  recursive_flag <- if(config$recursive) "-r" else ""
  script <- gsub("{{RECURSIVE}}", recursive_flag, script, fixed = TRUE)

  # Handle pattern for inotifywait/fswatch
  if (!is.null(config$pattern)) {
    # Convert glob pattern to extended regex for inotifywait
    pattern <- glob2regexp(config$pattern)
    script <- gsub("inotifywait -m", sprintf("inotifywait -m --include '%s'", pattern), script)
    script <- gsub("fswatch", sprintf("fswatch -e '%s'", pattern), script)
  }
  print("return script")
  script
}

#' Convert glob pattern to extended regular expression
#' @param pattern Glob pattern (e.g., "*.csv")
#' @return Extended regular expression
#' @keywords internal
glob2regexp <- function(pattern) {
  # Remove any directory parts
  pattern <- basename(pattern)

  # Escape special regex characters
  pattern <- gsub("([.^$+(){}\\[\\]|])", "\\\\\\1", pattern)

  # Convert glob patterns to regex
  pattern <- gsub("\\*", ".*", pattern)
  pattern <- gsub("\\?", ".", pattern)

  # Add start/end anchors
  paste0("^", pattern, "$")
}

