#!/bin/bash
# stop_node1.sh - Stop Node 1 wrapper

# Get the directory of this script
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"

# Source common functions and stop node 1
source "$SCRIPT_DIR/cluster_common.sh"
stop_node 1