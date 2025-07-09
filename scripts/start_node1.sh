#!/bin/bash
# start_node1.sh - Start Node 1 (Primary) wrapper

# Get the directory of this script
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"

# Source common functions and start node 1
source "$SCRIPT_DIR/cluster_common.sh"
start_node 1