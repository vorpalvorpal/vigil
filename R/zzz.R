#' Package startup function
#'
#' Creates necessary directories and sets up package environment.
#'
#' @param libname Library name
#' @param pkgname Package name
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Create vigil directory on package load
  tryCatch({
    fs::dir_create(get_vigil_dir())
  }, error = function(e) {
    warning("Could not create vigil directory: ", e$message)
  })
}

#' Package unload function
#'
#' Cleans up non-persistent watchers when package is unloaded.
#'
#' @param libpath Library path
#' @keywords internal
.onUnload <- function(libpath) {
  # Only clean up non-persistent watchers
  tryCatch({
    watchers <- list_watchers()
    non_persistent <- watchers[!watchers$persistent, ]
    if (nrow(non_persistent) > 0) {
      purrr::walk(non_persistent$id, kill_watcher)
    }
  }, error = function(e) {
    warning("Could not clean up watchers: ", e$message)
  })
}
