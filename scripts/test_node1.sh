#!/bin/bash
# test_node1.sh - Test Node 1 connectivity wrapper

# Get the directory of this script
SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"

# Source common functions and test node 1
source "$SCRIPT_DIR/cluster_common.sh"
test_node 1