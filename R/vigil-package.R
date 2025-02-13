#' Vigil: Cross-Platform File System Watching for R
#'
#' @description
#' Provides a unified interface for file system watching across Windows and Unix-like
#' systems. Supports pattern matching, custom callbacks, and persistent watchers.
#'
#' Platform-specific requirements:
#' * Windows: taskscheduleR package (only for persistent watchers)
#' * Linux: inotify-tools
#' * macOS: fswatch
#'
#' @section Platform-Specific Features:
#' Different platforms use different mechanisms for file watching:
#'
#' * Windows uses FileSystemWatcher through VBScript, and Task Scheduler (via taskscheduleR)
#'   for persistent watchers
#' * Linux uses inotify-tools for file watching and systemd for persistent watchers
#' * macOS uses fswatch for file watching and launchd for persistent watchers
#'
#' @importFrom fs path_norm dir_create file_exists path path_ext file_chmod path_expand
#' @importFrom jsonlite write_json read_json
#' @importFrom cli cli_alert_info cli_alert_warning cli_abort cli_alert_success
#' @importFrom uuid UUIDgenerate
#' @importFrom checkmate assert_directory_exists assert_character assert_logical assert_choice assert_number
#' @importFrom purrr map_dfr walk
#' @importFrom tibble tibble
#' @importFrom evaluate evaluate new_output_handler
#' @importFrom qs qserialize qdeserialize base85_encode base85_decode
#'
#' @keywords internal
"_PACKAGE"
