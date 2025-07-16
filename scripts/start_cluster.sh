#!/bin/bash
# Start a complete cluster with N nodes
# Usage: ./start_cluster.sh [cluster_size]

set -e

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Parse arguments
CLUSTER_SIZE=${1:-2}

# Validate cluster size
if ! [[ "$CLUSTER_SIZE" =~ ^[0-9]+$ ]] || [ "$CLUSTER_SIZE" -lt 1 ]; then
    echo "Error: CLUSTER_SIZE must be a positive integer"
    echo "Usage: $0 [cluster_size]"
    exit 1
fi

echo "🚀 Starting cluster with $CLUSTER_SIZE nodes..."
echo ""

# Export cluster size for all nodes
export CLUSTER_SIZE=$CLUSTER_SIZE

# Start each node in the background
for i in $(seq 1 $CLUSTER_SIZE); do
    echo "Starting node $i..."
    "$SCRIPT_DIR/start_node.sh" $i $CLUSTER_SIZE &
    
    # Small delay between starts to avoid port conflicts
    sleep 2
done

echo ""
echo "✅ All nodes started!"
echo ""
echo "Access the nodes at:"
BASE_HTTP_PORT=${BASE_HTTP_PORT:-4000}
PORT_SPACING=${PORT_SPACING:-10}
for i in $(seq 1 $CLUSTER_SIZE); do
    PORT=$((BASE_HTTP_PORT + (i - 1) * PORT_SPACING))
    echo "  Node $i: http://localhost:$PORT"
done

echo ""
echo "To stop the cluster, run: $SCRIPT_DIR/stop_cluster.sh"
echo ""
echo "Press Ctrl+C to stop all nodes..."

# Wait for all background processes
wait