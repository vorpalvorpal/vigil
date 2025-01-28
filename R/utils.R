#' Validate Callback
#'
#' Validates and normalizes callback specifications for file watchers.
#'
#' @param callback Function, expression, package function name, or path to R script
#' @return List containing:
#'   \describe{
#'     \item{type}{One of "none", "package", "script", or "expression"}
#'     \item{value}{The validated callback value}
#'   }
#' @examples
#' # Function callback
#' validate_callback(function(x) print(x))
#'
#' # Package function
#' validate_callback("dplyr::select")
#'
#' # R script
#' validate_callback("~/scripts/process_file.R")
#' @keywords internal
validate_callback <- function(callback) {
  # Handle NULL callback
  if (is.null(callback)) {
    return(list(
      type = "none",
      value = NULL
    ))
  }

  # Handle package function specification (e.g. "dplyr::select")
  if (is.character(callback) && length(callback) == 1) {
    if (grepl("::", callback, fixed = TRUE)) {
      parts <- strsplit(callback, "::")[[1]]
      if (length(parts) == 2) {
        if (requireNamespace(parts[1], quietly = TRUE) &&
            exists(parts[2], envir = asNamespace(parts[1]))) {
          return(list(
            type = "package",
            value = callback
          ))
        }
      }
    }

    # Check for R script path
    if (fs::file_exists(callback) && fs::path_ext(callback) == "R") {
      return(list(
        type = "script",
        value = fs::path_abs(callback)
      ))
    }
  }

  # Handle function or expression
  if (is.function(callback) || is.language(callback)) {
    return(list(
      type = "expression",
      value = callback
    ))
  }

  # Invalid callback
  cli::cli_abort(c(
    "Invalid callback specification",
    i = "Must be one of:",
    "*" = "Package function (e.g. 'dplyr::select')",
    "*" = "Path to R script",
    "*" = "Function or expression"
  ))
}

#' Null coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
