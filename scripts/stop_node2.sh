#!/bin/bash
# stop_node2.sh - Stop Node 2 wrapper

# Get the directory of this script
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"

# Source common functions and stop node 2
source "$SCRIPT_DIR/cluster_common.sh"
stop_node 2