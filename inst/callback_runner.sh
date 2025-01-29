# callback_runner.sh
#!/bin/bash

# Usage: ./callback_runner.sh <database_path> <event_id>
set -e

DB_PATH="$1"
EVENT_ID="$2"

if [ ! -f "$DB_PATH" ]; then
    echo "Database not found" >&2
    exit 1
fi

# Create temporary file for output capture
TEMP_OUT=$(mktemp)
trap 'rm -f "$TEMP_OUT"' EXIT

# Helper function for SQLite operations
sqlite_exec() {
    sqlite3 "$DB_PATH" "$1"
}

# Get callback content
CALLBACK_CONTENT=$(sqlite_exec "SELECT value FROM config WHERE key='callback_content';")

if [ -z "$CALLBACK_CONTENT" ]; then
    echo "No callback configured" >&2
    exit 1
fi

# Run the callback in R and capture output
Rscript -e "
tryCatch({
  event <- DBI::dbGetQuery(
    DBI::dbConnect(RSQLite::SQLite(), '$DB_PATH'),
    'SELECT * FROM events WHERE id = $EVENT_ID'
  );
  $CALLBACK_CONTENT
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
