#!/usr/bin/env Rscript

# Simple wrapper for SQLite operations from VBScript
# Usage: Rscript sqlite-wrapper.R <database_path> <sql_query>

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2L) {
  stop("Usage: Rscript sqlite-wrapper.R <database_path> <sql_query>")
}

db_path <- args[1]
sql_query <- args[2]

if (!file.exists(db_path)) {
  stop("Database not found: ", db_path)
}

# Execute query and return results
tryCatch({
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  # Check if query returns results
  if (grepl("^\\s*SELECT", sql_query, ignore.case = TRUE)) {
    result <- dbGetQuery(con, sql_query)
    if (nrow(result) > 0) {
      # For VBScript compatibility, return first column of first row
      cat(as.character(result[[1]][1]))
    }
  } else {
    # Execute non-SELECT query
    dbExecute(con, sql_query)
  }
}, error = function(e) {
  stop("SQL error: ", conditionMessage(e))
})
