#!/bin/bash
# Stop all nodes in the cluster
# Usage: ./stop_cluster.sh [cluster_size]

set -e

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Parse arguments
CLUSTER_SIZE=${1:-10}  # Default to stopping up to 10 nodes

echo "🛑 Stopping cluster nodes..."
echo ""

# Stop each node
nodes_stopped=0
for i in $(seq 1 $CLUSTER_SIZE); do
    # Check if node might be running
    NODE_HOSTNAME=${NODE_HOSTNAME:-${HOSTNAME:-localhost}}
    if [ "$i" -eq 1 ]; then
        NODE_NAME="superlearner@${NODE_HOSTNAME}"
    else
        NODE_NAME="superlearner${i}@${NODE_HOSTNAME}"
    fi
    
    # Check if process exists
    if pgrep -f "$NODE_NAME" > /dev/null 2>&1; then
        echo "Stopping node $i..."
        "$SCRIPT_DIR/stop_node.sh" $i
        nodes_stopped=$((nodes_stopped + 1))
    fi
done

if [ $nodes_stopped -eq 0 ]; then
    echo "No running nodes found."
else
    echo ""
    echo "✅ Stopped $nodes_stopped nodes"
fi

# Also clean up any stray beam processes
echo ""
echo "Cleaning up any remaining processes..."
pkill -f "beam.*superlearner" 2>/dev/null || true

echo "✅ Cluster stopped"