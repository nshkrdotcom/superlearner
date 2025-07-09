#!/bin/bash
# setup_node2.sh - Setup Node 2 (Secondary) wrapper

# Get the directory of this script
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"

# Source common functions and run setup for node 2
source "$SCRIPT_DIR/cluster_common.sh"
setup_node 2