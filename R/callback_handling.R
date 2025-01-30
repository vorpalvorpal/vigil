#' Create output capture handler
#' @keywords internal
create_capture_handler <- function() {
  output_env <- new.env(parent = emptyenv())

  output_env$sources <- character()
  output_env$texts <- character()
  output_env$messages <- character()
  output_env$warnings <- character()
  output_env$errors <- character()
  output_env$values <- list()
  output_env$plots <- list()

  handler <- evaluate::new_output_handler(
    source = function(src, expr) {
      output_env$sources <<- c(output_env$sources, src)
      NULL
    },
    text = function(x) {
      output_env$texts <<- c(output_env$texts, x)
      NULL
    },
    message = function(x) {
      output_env$messages <<- c(output_env$messages, x$message)
      NULL
    },
    warning = function(x) {
      output_env$warnings <<- c(output_env$warnings, x$message)
      NULL
    },
    error = function(x) {
      output_env$errors <<- c(output_env$errors, x$message)
      NULL
    },
    value = function(x, visible, env = NULL) {
      if (visible) {
        output_env$values <<- c(output_env$values, list(x))
      }
      NULL
    },
    graphics = function(x) {
      output_env$plots <<- c(output_env$plots, list(x))
      NULL
    }
  )

  handler$output_env <- output_env
  handler
}

#' Write captured results to database
#' @keywords internal
write_results <- function(con, event_id, handler) {
  # Extract results from handler environment
  results <- handler$output_env

  # Helper function to serialize and encode
  serialize_and_encode <- function(data) {
    if (length(data) > 0) {
      blob <- qs::qserialize(data, preset = "fast")
      qs::base85_encode(blob)
    } else {
      ""
    }
  }

  # Convert all results to text
  values_encoded <- serialize_and_encode(results$values)
  plots_encoded <- serialize_and_encode(results$plots)

  # Prepare other text fields
  sources <- paste(results$sources, collapse = "\n")
  texts <- paste(results$texts, collapse = "\n")
  messages <- paste(results$messages, collapse = "\n")
  warnings <- paste(results$warnings, collapse = "\n")
  errors <- paste(results$errors, collapse = "\n")

  # Create parameter list with names matching SQL placeholders
  params <- list(
    sources = if (length(sources) > 0) sources else "",
    texts = if (length(texts) > 0) texts else "",
    messages = if (length(messages) > 0) messages else "",
    warnings = if (length(warnings) > 0) warnings else "",
    errors = if (length(errors) > 0) errors else "",
    values = values_encoded,
    plots = plots_encoded,
    id = event_id
  )

  # Update database using named parameters
  DBI::dbExecute(
    con,
    "UPDATE events SET
      callback_source = :sources,
      callback_text = :texts,
      callback_messages = :messages,
      callback_warnings = :warnings,
      callback_errors = :errors,
      callback_values = :values,
      callback_plots = :plots,
      callback_timestamp = datetime('now')
    WHERE id = :id",
    params = params
  )
}

#' Execute a callback with enhanced output capture
#'
#' This function executes a callback stored in a watcher's database and captures
#' various types of output including printed text, messages, warnings, errors,
#' return values, and plots. All outputs are stored back in the database.
#'
#' @param db_path Path to the SQLite database containing the watcher configuration
#' @param event_id Integer ID of the event that triggered the callback
#'
#' @details
#' The function retrieves the callback content from the database's config table
#' and executes it in an environment where the event data is available as the
#' 'event' variable. Different types of output are captured:
#'
#' * Source code (stored in callback_source)
#' * Printed output (stored in callback_text)
#' * Messages (stored in callback_messages)
#' * Warnings (stored in callback_warnings)
#' * Errors (stored in callback_errors)
#' * Return values (serialized and stored in callback_values)
#' * Plots (serialized and stored in callback_plots)
#'
#' Return values and plots are serialized using the qs package and encoded
#' using base85 encoding before storage.
#'
#' @export
execute_callback <- function(db_path, event_id) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  # Get callback content
  callback_content <- DBI::dbGetQuery(
    con,
    "SELECT value FROM config WHERE key='callback_content'"
  )$value

  if (is.null(callback_content) || length(callback_content) == 0) {
    stop("No callback configured")
  }

  # Get event data
  event <- DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM events WHERE id = %d", event_id)
  )

  # Create output handler
  handler <- create_capture_handler()

  # Evaluate callback
  evaluate::evaluate(
    callback_content,
    envir = list(event = event),
    output_handler = handler,
    stop_on_error = 1L,
    keep_warning = TRUE,
    keep_message = TRUE,
    new_device = TRUE
  )

  # Write results back to database
  write_results(con, event_id, handler)
}
