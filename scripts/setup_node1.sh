#!/bin/bash
# setup_node1.sh - Setup Node 1 (Primary) wrapper

# Get the directory of this script
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"

# Source common functions and run setup for node 1
source "$SCRIPT_DIR/cluster_common.sh"
setup_node 1