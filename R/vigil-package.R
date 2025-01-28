#' Vigil: Cross-Platform File System Watching for R
#'
#' @description
#' Provides a unified interface for file system watching across Windows and Unix-like
#' systems. Supports pattern matching, custom callbacks, and persistent watchers.
#'
#' @importFrom fs path_norm dir_create file_exists path path_ext chmod path_expand
#' @importFrom jsonlite write_json read_json
#' @importFrom cli cli_alert_info cli_alert_warning cli_abort cli_alert_success
#' @importFrom uuid UUIDgenerate
#' @importFrom checkmate assert_directory_exists assert_character assert_logical assert_choice assert_number
#' @importFrom purrr map_dfr walk
#' @importFrom tibble tibble
#' @importFrom taskscheduleR taskscheduler_create taskscheduler_delete taskscheduler_ls
#'
#' @keywords internal
"_PACKAGE"
