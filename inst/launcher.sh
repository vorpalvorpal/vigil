#!/bin/bash

# Usage: ./launcher.sh <database_path> <watcher_script_path> [--wait-for-event]
#
# Returns:
#   0 - Database created and watcher running
#   1 - Error
#   2 - First event written (only with --wait-for-event)

set -e  # Exit on any error

DB_PATH="$1"
WATCHER_SCRIPT="$2"
WAIT_FOR_EVENT="$3"

if [ ! -f "$DB_PATH" ] || [ ! -f "$WATCHER_SCRIPT" ]; then
    echo "Required files not found" >&2
    exit 1
fi

# Create a named pipe for communication
FIFO="/tmp/vigil_launcher_$$"
trap 'rm -f "$FIFO"' EXIT
mkfifo "$FIFO"

# Start watcher process in background
"$WATCHER_SCRIPT" "$DB_PATH" &
WATCHER_PID=$!

# Use inotifywait/fswatch to monitor database file
if command -v inotifywait >/dev/null 2>&1; then
    # Linux: Use inotifywait
    inotifywait -q -e modify "$DB_PATH" > "$FIFO" &
elif command -v fswatch >/dev/null 2>&1; then
    # macOS: Use fswatch
    fswatch -1 -o "$DB_PATH" > "$FIFO" &
else
    echo "No file watching tool found" >&2
    kill $WATCHER_PID
    exit 1
fi
WATCH_PID=$!

# Wait for modification with 5 second timeout
read -t 5 < "$FIFO"
STATUS=$?

if [ $STATUS -eq 0 ]; then
    # Check if watcher process wrote its PID to database
    if sqlite3 "$DB_PATH" "SELECT value FROM status WHERE key='pid' AND value IS NOT NULL" >/dev/null 2>&1; then
        if [ "$WAIT_FOR_EVENT" = "--wait-for-event" ]; then
            # Keep watching for first event
            while true; do
                EVENT_COUNT=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM events")
                if [ "$EVENT_COUNT" -gt 0 ]; then
                    exit 2
                fi
                # Check if watcher is still running
                if ! kill -0 $WATCHER_PID 2>/dev/null; then
                    exit 1
                fi
                sleep 0.1
            done
        fi
        echo $WATCHER_PID
        exit 0
    fi
fi

# If we get here, something went wrong
kill $WATCHER_PID 2>/dev/null || true
echo "Watcher failed to start within 5 seconds" >&2
exit 1
