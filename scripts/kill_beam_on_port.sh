#!/bin/bash
# Kill processes listening on a specific port

PORT=$1

if [ -z "$PORT" ]; then
    echo "Usage: $0 <port>"
    exit 1
fi

echo "🛑 Killing processes on port $PORT..."

# Try to kill with fuser first (most reliable)
if command -v fuser >/dev/null 2>&1; then
    echo "Using fuser to kill processes on port $PORT"
    fuser -k $PORT/tcp 2>/dev/null
    if [ $? -eq 0 ]; then
        echo "✅ Processes killed with fuser"
        exit 0
    fi
fi

# Fallback: Use lsof
PIDS=$(lsof -ti :$PORT 2>/dev/null)
if [ ! -z "$PIDS" ]; then
    echo "Found processes: $PIDS"
    for PID in $PIDS; do
        echo "Killing PID $PID"
        kill -TERM $PID 2>/dev/null
        sleep 1
        kill -KILL $PID 2>/dev/null
    done
    echo "✅ Processes killed"
else
    echo "❌ No processes found on port $PORT"
fi