#!/bin/bash
# Start Node 1 (primary)

cd "$(dirname "$0")/.."

echo "🚀 Starting Node 1 (primary) - superlearner@U2401"
echo "Web interface: http://localhost:4000"
echo "Press Ctrl+C to stop"
echo "=================================================="

# Set environment
export MIX_ENV=dev
export NODE_ROLE=primary

# Start with proper node name, cookie, and config
exec iex --name superlearner@U2401 --cookie secret_cluster_cookie -S mix phx.server
