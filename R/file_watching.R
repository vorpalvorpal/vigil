#' Watch Files on Windows
#'
#' Creates and manages a file system watcher on Windows using VBScript and FileSystemWatcher.
#'
#' @param config A list containing watcher configuration:
#'   \describe{
#'     \item{id}{Unique identifier for the watcher}
#'     \item{path}{Directory path to watch}
#'     \item{pattern}{Optional regex pattern to match}
#'     \item{recursive}{Whether to watch subdirectories}
#'     \item{watch_mode}{"continuous" or "single" for event handling}
#'     \item{change_type}{Type of changes to watch for}
#'     \item{no_cleanup}{Whether to preserve event files}
#'   }
#' @return Invisibly returns NULL
#' @seealso \code{\link{watch_files_unix}} for Unix implementation
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
#' @keywords internal
watch_files_unix <- function(config) {
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

#' Convert R regex to VBScript compatible pattern
#'
#' @param pattern Character string containing R regular expression
#' @return Character string containing VBScript compatible regular expression
#' @note VBScript has limited regex support. Some R regex features may be removed
#'       or simplified.
#' @keywords internal
convert_regex_to_vbs <- function(pattern) {
  checkmate::assert_character(pattern, len = 1, null.ok = TRUE)

  if (is.null(pattern)) {
    return(".*")  # Default pattern
  }

  # Verify input is a valid R regex
  tryCatch(
    grepl(pattern, "test string"),
    error = function(e) cli::cli_abort("Invalid regular expression pattern")
  )

  # VBScript specific transformations
  vbs_pattern <- pattern |>
    # Convert shorthand character classes
    gsub("\\\\d", "[0-9]", x = _) |>
    gsub("\\\\w", "[A-Za-z0-9_]", x = _) |>
    gsub("\\\\s", "[ \\t\\n\\r]", x = _) |>
    # Remove unsupported lookarounds
    gsub("\\(\\?=[^)]*\\)", "", x = _) |>
    gsub("\\(\\?<=[^)]*\\)", "", x = _) |>
    gsub("\\(\\?![^)]*\\)", "", x = _) |>
    gsub("\\(\\?<![^)]*\\)", "", x = _) |>
    # Convert named groups to regular groups
    gsub("\\(\\?<[^>]+>", "(", x = _)

  if (grepl("\\P{\\w+}|\\p{\\w+}", vbs_pattern)) {
    cli::cli_abort("Unicode character classes are not supported in VBScript regex")
  }

  vbs_pattern
}

#' Escape pattern for shell commands
#'
#' @param pattern Character string containing regex pattern
#' @return Character string with shell metacharacters escaped
#' @keywords internal
escape_shell_pattern <- function(pattern) {
  checkmate::assert_character(pattern, len = 1, null.ok = TRUE)

  if (is.null(pattern)) {
    return(NULL)
  }

  # Escape single quotes for shell safety (replace ' with '\''')
  gsub("'", "'\\''", pattern, fixed = TRUE)
}

#' Create Windows watcher VBScript
#' @param config Watcher configuration
#' @return Character string containing VBScript code
#' @details
#' The pattern matching uses VBScript's regex engine which has limited support
#' compared to R's regex. Some features (lookaround, Unicode classes) are removed
#' or simplified.
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

  # Get absolute paths
  vigil_dir <- fs::path_abs(get_vigil_dir())
  watch_path <- fs::path_abs(config$path)

  # Convert R regex to VBScript regex pattern if provided
  vbs_pattern <- convert_regex_to_vbs(config$pattern)

  # Replace template variables
  script <- paste(template, collapse = "\n")
  script <- gsub("{{WATCH_MODE}}", config$watch_mode, script, fixed = TRUE)
  script <- gsub("{{CHANGE_TYPE}}", config$change_type, script, fixed = TRUE)
  script <- gsub("{{NO_CLEANUP}}", config$no_cleanup, script, fixed = TRUE)
  script <- gsub("{{WATCHER_ID}}", config$id, script, fixed = TRUE)
  script <- gsub("{{VIGIL_DIR}}", vigil_dir, script, fixed = TRUE)
  script <- gsub("{{WATCH_PATH}}", watch_path, script, fixed = TRUE)
  script <- gsub("{{PATTERN}}", vbs_pattern, script, fixed = TRUE)
  script <- gsub("{{RECURSIVE}}", if(config$recursive) "True" else "False", script, fixed = TRUE)

  script
}

#' Create Unix watcher shell script
#' @param config Watcher configuration
#' @return Character string containing shell script code
#' @details
#' The pattern matching uses Extended Regular Expressions (ERE) which is compatible
#' with R's regex syntax. The pattern is passed to either inotifywait or fswatch
#' using their respective --regexp flags.
#'
#' For inotifywait (Linux), see: https://linux.die.net/man/1/inotifywait
#' For fswatch (macOS), see: https://emcrisostomo.github.io/fswatch/doc/
#' @keywords internal
create_unix_watcher_script <- function(config) {
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

  # Get absolute paths
  vigil_dir <- fs::path_abs(get_vigil_dir())
  watch_path <- fs::path_abs(config$path)

  # Replace template variables
  script <- paste(template, collapse = "\n")
  script <- gsub("{{WATCH_MODE}}", config$watch_mode, script, fixed = TRUE)
  script <- gsub("{{CHANGE_TYPE}}", config$change_type, script, fixed = TRUE)
  script <- gsub("{{NO_CLEANUP}}", config$no_cleanup, script, fixed = TRUE)
  script <- gsub("{{WATCHER_ID}}", config$id, script, fixed = TRUE)
  script <- gsub("{{VIGIL_DIR}}", vigil_dir, script, fixed = TRUE)
  script <- gsub("{{WATCH_PATH}}", watch_path, script, fixed = TRUE)

  # Handle recursive flag
  recursive_flag <- if(config$recursive) "-r" else ""
  script <- gsub("{{RECURSIVE}}", recursive_flag, script, fixed = TRUE)

  # Handle pattern for inotifywait/fswatch with proper escaping
  if (!is.null(config$pattern)) {
    safe_pattern <- escape_shell_pattern(config$pattern)
    script <- gsub(
      "inotifywait -m",
      sprintf("inotifywait -m --regexp '%s'", safe_pattern),
      script
    )
    script <- gsub(
      "fswatch",
      sprintf("fswatch --regexp '%s'", safe_pattern),
      script
    )
  }

  script
}
