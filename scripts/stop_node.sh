#!/bin/bash
# Generic script to stop a cluster node
# Usage: ./stop_node.sh [node_index]

set -e

# Parse arguments
NODE_INDEX=${1:-1}

# Validate node index
if ! [[ "$NODE_INDEX" =~ ^[0-9]+$ ]] || [ "$NODE_INDEX" -lt 1 ]; then
    echo "Error: NODE_INDEX must be a positive integer"
    echo "Usage: $0 [node_index]"
    exit 1
fi

# Get hostname
NODE_HOSTNAME=${NODE_HOSTNAME:-${HOSTNAME:-localhost}}

# Generate node name
if [ "$NODE_INDEX" -eq 1 ]; then
    NODE_NAME="superlearner@${NODE_HOSTNAME}"
else
    NODE_NAME="superlearner${NODE_INDEX}@${NODE_HOSTNAME}"
fi

echo "🛑 Stopping Node $NODE_INDEX ($NODE_NAME)..."

# Kill processes by node name
pkill -f "$NODE_NAME" 2>/dev/null || true

# Also kill by port
BASE_HTTP_PORT=${BASE_HTTP_PORT:-4000}
PORT_SPACING=${PORT_SPACING:-10}
NODE_PORT=$((BASE_HTTP_PORT + (NODE_INDEX - 1) * PORT_SPACING))

fuser -k $NODE_PORT/tcp 2>/dev/null || true
lsof -ti:$NODE_PORT | xargs kill -9 2>/dev/null || true

echo "✅ Node $NODE_INDEX stopped"