#!/bin/bash

# Test script for watch_files.sh
set -e  # Exit on any error

# Setup
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WATCH_SCRIPT="../inst/watch_files.sh"
TEST_DIR="$(mktemp -d)"
DB_PATH="$TEST_DIR/test.db"

# Check if watch_files.sh exists and is executable
if [ ! -f "$WATCH_SCRIPT" ]; then
    echo "❌ watch_files.sh not found at $WATCH_SCRIPT"
    exit 1
fi

# Make watch_files.sh executable if it isn't already
if [ ! -x "$WATCH_SCRIPT" ]; then
    echo "Making watch_files.sh executable..."
    chmod +x "$WATCH_SCRIPT" || {
        echo "❌ Failed to make watch_files.sh executable. Try: chmod +x $WATCH_SCRIPT"
        exit 1
    }
fi

# Cleanup function
cleanup() {
    # Make sure watcher process is dead
    if [ ! -z "$WATCHER_PID" ]; then
        kill $WATCHER_PID 2>/dev/null || true
        sleep 0.5  # Give process time to write final state
    fi
    # Remove test directory
    rm -rf "$TEST_DIR"
}
trap cleanup EXIT

# Helper function to initialize test database
init_test_db() {
    sqlite3 "$DB_PATH" <<EOF
CREATE TABLE config (
    key TEXT PRIMARY KEY,
    value TEXT,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
CREATE TABLE events (
    id INTEGER PRIMARY KEY,
    timestamp TEXT NOT NULL,
    event_type TEXT NOT NULL,
    file_path TEXT NOT NULL,
    callback_source TEXT,
    callback_text TEXT,
    callback_messages TEXT,
    callback_warnings TEXT,
    callback_errors TEXT,
    callback_values TEXT,
    callback_plots TEXT,
    callback_timestamp TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
CREATE TABLE status (
    key TEXT PRIMARY KEY,
    value TEXT,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Insert test configuration
INSERT INTO config (key, value) VALUES
    ('path', '$TEST_DIR'),
    ('pattern', ''),
    ('recursive', 'false'),
    ('watch_mode', 'continuous'),
    ('change_type', 'any');
EOF
}

# Test 1: Basic Initialization
echo "Test 1: Basic Initialization"
init_test_db
"$WATCH_SCRIPT" "$DB_PATH" &
WATCHER_PID=$!
sleep 2  # Give the script time to initialize

# Check if PID was written to database
STORED_PID=$(sqlite3 "$DB_PATH" "SELECT value FROM status WHERE key='pid';")
if [ "$STORED_PID" != "$WATCHER_PID" ]; then
    echo "❌ PID mismatch: Expected $WATCHER_PID, got $STORED_PID"
    kill $WATCHER_PID
    exit 1
fi

# Check if state was set to running
STATE=$(sqlite3 "$DB_PATH" "SELECT value FROM status WHERE key='state';")
if [ "$STATE" != "running" ]; then
    echo "❌ Incorrect state: Expected 'running', got '$STATE'"
    kill $WATCHER_PID
    exit 1
fi

echo "✅ Basic initialization passed"

# Test 2: File Creation Detection
echo "Test 2: File Creation Detection"
touch "$TEST_DIR/test_file.txt"
sleep 2  # Give fswatch time to detect and process the event

EVENT_COUNT=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM events WHERE event_type='modified' AND file_path LIKE '%test_file.txt';")
if [ "$EVENT_COUNT" -eq 0 ]; then
    echo "❌ File creation event not detected"
    kill $WATCHER_PID
    exit 1
fi

echo "✅ File creation detection passed"

# Test 3: File Modification Detection
echo "Test 3: File Modification Detection"
echo "test content" > "$TEST_DIR/test_file.txt"
sleep 2

EVENT_COUNT=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM events WHERE event_type='modified' AND file_path LIKE '%test_file.txt';")
if [ "$EVENT_COUNT" -lt 2 ]; then
    echo "❌ File modification event not detected"
    kill $WATCHER_PID
    exit 1
fi

echo "✅ File modification detection passed"

# Test 4: File Deletion Detection
echo "Test 4: File Deletion Detection"
rm "$TEST_DIR/test_file.txt"
sleep 2

EVENT_COUNT=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM events WHERE event_type='modified' AND file_path LIKE '%test_file.txt';")
if [ "$EVENT_COUNT" -lt 3 ]; then
    echo "❌ File deletion event not detected"
    kill $WATCHER_PID
    exit 1
fi

echo "✅ File deletion detection passed"

# Test 5: Clean Shutdown
echo "Test 5: Clean Shutdown"

# First check if process is still running
if ! kill -0 $WATCHER_PID 2>/dev/null; then
    echo "❌ Process died unexpectedly before cleanup test"
    exit 1
fi

# Get the database state before killing
PRE_STATE=$(sqlite3 "$DB_PATH" "SELECT value FROM status WHERE key='state';")
if [ "$PRE_STATE" != "running" ]; then
    echo "❌ Unexpected state before shutdown: $PRE_STATE"
    exit 1
fi

# Send SIGTERM to trigger cleanup
kill $WATCHER_PID

# Wait for process to exit (up to 5 seconds)
COUNTER=0
while kill -0 $WATCHER_PID 2>/dev/null && [ $COUNTER -lt 50 ]; do
    sleep 0.1
    COUNTER=$((COUNTER + 1))
done

# Try to get final state (multiple attempts)
for i in {1..5}; do
    if STATE=$(sqlite3 "$DB_PATH" "SELECT value FROM status WHERE key='state';"); then
        if [ "$STATE" = "stopped" ]; then
            echo "✅ Clean shutdown passed"
            break
        fi
    fi
    sleep 0.5
    if [ $i -eq 5 ]; then
        echo "❌ Watcher did not shut down cleanly. Final state: ${STATE:-unknown}"
        exit 1
    fi
done

echo "All tests passed! ✅"
