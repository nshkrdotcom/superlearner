#!/bin/bash
# start_node2.sh - Start Node 2 (Secondary) wrapper

# Get the directory of this script
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"

# Source common functions and start node 2
source "$SCRIPT_DIR/cluster_common.sh"
start_node 2