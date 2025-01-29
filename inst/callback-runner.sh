#!/bin/bash

# Usage: ./callback-runner.sh <database_path> <event_id> <callback_script>
#
# Runs the callback script for a specific event and records the output

set -e  # Exit on any error

DB_PATH="$1"
EVENT_ID="$2"
CALLBACK="$3"

if [ ! -f "$DB_PATH" ] || [ ! -f "$CALLBACK" ]; then
    echo "Required files not found" >&2
    exit 1
fi

# Helper function for SQLite operations - ensures proper escaping
sqlite_exec() {
    sqlite3 "$DB_PATH" "$1"
}

# Create a temporary file for output capture
TEMP_OUT=$(mktemp)
trap 'rm -f "$TEMP_OUT"' EXIT

# Run the callback in R and capture output
Rscript -e "
  tryCatch({
    event <- DBI::dbGetQuery(
      DBI::dbConnect(RSQLite::SQLite(), '$DB_PATH'),
      'SELECT * FROM events WHERE id = $EVENT_ID'
    )
    source('$CALLBACK')
  }, error = function(e) {
    cat('Error in callback: ', conditionMessage(e), '\n')
  })
" 2>&1 > "$TEMP_OUT"

# Read output and escape for SQL
OUTPUT=$(cat "$TEMP_OUT" | sed "s/'/''/g")

# Update event with callback output and timestamp
TIMESTAMP=$(date -u "+%Y-%m-%d %H:%M:%S")
sqlite_exec "UPDATE events
            SET callback_output = '$OUTPUT',
                callback_timestamp = '$TIMESTAMP'
            WHERE id = $EVENT_ID;"
