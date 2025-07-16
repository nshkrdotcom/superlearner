#!/bin/bash
# Generic script to start a cluster node
# Usage: ./start_node.sh [node_index] [cluster_size]

set -e

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/.."

# Parse arguments
NODE_INDEX=${1:-1}
CLUSTER_SIZE=${2:-2}

# Validate node index
if ! [[ "$NODE_INDEX" =~ ^[0-9]+$ ]] || [ "$NODE_INDEX" -lt 1 ]; then
    echo "Error: NODE_INDEX must be a positive integer"
    echo "Usage: $0 [node_index] [cluster_size]"
    exit 1
fi

# Set environment variables
export NODE_INDEX=$NODE_INDEX
export CLUSTER_SIZE=$CLUSTER_SIZE
export NODE_HOSTNAME=${NODE_HOSTNAME:-${HOSTNAME:-localhost}}

# Calculate port
BASE_HTTP_PORT=${BASE_HTTP_PORT:-4000}
PORT_SPACING=${PORT_SPACING:-10}
NODE_PORT=$((BASE_HTTP_PORT + (NODE_INDEX - 1) * PORT_SPACING))
export PHX_PORT=$NODE_PORT

# Generate node name
if [ "$NODE_INDEX" -eq 1 ]; then
    NODE_NAME="superlearner@${NODE_HOSTNAME}"
else
    NODE_NAME="superlearner${NODE_INDEX}@${NODE_HOSTNAME}"
fi

echo "🚀 Starting Node $NODE_INDEX (of $CLUSTER_SIZE)"
echo "Node name: $NODE_NAME"
echo "Web interface: http://localhost:$NODE_PORT"
echo "Press Ctrl+C to stop"
echo "=================================================="

# Clean up any existing processes on this port
echo "Cleaning up port $NODE_PORT..."
fuser -k $NODE_PORT/tcp 2>/dev/null || true
lsof -ti:$NODE_PORT | xargs kill -9 2>/dev/null || true

# Start the node
export MIX_ENV=dev
exec iex --name "$NODE_NAME" --cookie secret_cluster_cookie -S mix phx.server