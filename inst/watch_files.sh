#!/bin/bash

# Usage: ./watch_files.sh <database_path>
#
# Watches files according to configuration in SQLite database and logs events

set -e  # Exit on any error

DB_PATH="$1"
if [ ! -f "$DB_PATH" ]; then
    echo "Database not found: $DB_PATH" >&2
    exit 1
fi

# Helper function for SQLite operations - ensures proper escaping
sqlite_exec() {
    sqlite3 "$DB_PATH" "$1"
}

# Read configuration from database
read_config() {
    local key="$1"
    sqlite_exec "SELECT value FROM config WHERE key='$key';"
}

WATCH_PATH=$(read_config "path")
PATTERN=$(read_config "pattern")
RECURSIVE=$(read_config "recursive")
WATCH_MODE=$(read_config "watch_mode")
CHANGE_TYPE=$(read_config "change_type")

if [ -z "$WATCH_PATH" ]; then
    echo "Invalid configuration: path not found" >&2
    exit 1
fi

# Write our PID as the watcher process
sqlite_exec "INSERT INTO active_processes (pid, type) VALUES ($$, 'watcher');"

# Function to write event to database and launch callback
write_event() {
    local type="$1"
    local file="$2"
    local timestamp=$(date -u "+%Y-%m-%d %H:%M:%S")

    # Only process events matching configured type
    if [ "$CHANGE_TYPE" != "any" ] && [ "$CHANGE_TYPE" != "$type" ]; then
        return
    fi

    # Begin transaction
    sqlite_exec "BEGIN TRANSACTION;
      INSERT INTO events (timestamp, event_type, file_path)
      VALUES ('$timestamp', '$type', '$file');
      COMMIT;"

    # Launch callback if configured
    sqlite_exec "SELECT value FROM config WHERE key='callback_script';" | while read -r callback_script; do
        if [ -n "$callback_script" ]; then
            event_id=$(sqlite_exec "SELECT last_insert_rowid();")

            # Launch callback in background subshell with completion trap
            (
                trap 'sqlite_exec "INSERT INTO active_processes (pid, type, event_id)
                                 VALUES ($$, '\''callback'\'', $event_id)
                                 ON CONFLICT (pid) DO UPDATE SET active = FALSE;"' EXIT
                Rscript -e "library(vigil); vigil:::execute_callback('$DB_PATH', $event_id)"
            ) &

            callback_pid=$!
            sqlite_exec "INSERT INTO active_processes (pid, type, event_id)
                        VALUES ($callback_pid, 'callback', $event_id);"
        fi
    done

    # Exit after first event in single mode
    if [ "$WATCH_MODE" = "single" ]; then
        exit 0
    fi
}

# Simple cleanup - just exit
cleanup() {
    exit 0
}

# Set up signal handling for clean exit
trap cleanup SIGTERM SIGINT

# Set up recursive flag if configured
WATCH_OPTS=""
if [ "$RECURSIVE" = "true" ]; then
    WATCH_OPTS="-r"
fi

# Determine which watching tool to use
if command -v inotifywait >/dev/null 2>&1; then
    # Use inotifywait (Linux)
    WATCH_CMD="inotifywait"
    if [ -n "$PATTERN" ]; then
        WATCH_OPTS="$WATCH_OPTS --event CREATE,MODIFY,DELETE --format '%w%f:%e' --monitor"
    fi
elif command -v fswatch >/dev/null 2>&1; then
    # Use fswatch (macOS)
    WATCH_CMD="fswatch"
    if [ -n "$PATTERN" ]; then
        WATCH_OPTS="$WATCH_OPTS --event Created,Updated,Removed"
    fi
else
    echo "No file watching tool found (requires inotifywait or fswatch)" >&2
    exit 1
fi

# Start watching based on selected tool
if [ "$WATCH_CMD" = "inotifywait" ]; then
    inotifywait $WATCH_OPTS "$WATCH_PATH" | while read -r file_event; do
        file="${file_event%%:*}"
        event="${file_event##*:}"

        # Convert inotify event types
        case "$event" in
            CREATE) write_event "created" "$file" ;;
            MODIFY) write_event "modified" "$file" ;;
            DELETE) write_event "deleted" "$file" ;;
        esac
    done
else
    # fswatch
    fswatch $WATCH_OPTS "$WATCH_PATH" | while read -r file; do
        # fswatch doesn't distinguish event types
        write_event "modified" "$file"
    done
fi
