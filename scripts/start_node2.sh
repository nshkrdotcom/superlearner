#!/bin/bash
# Start Node 2 (secondary)

cd "$(dirname "$0")/.."

echo "🚀 Starting Node 2 (secondary) - superlearner2@localhost"
echo "Web interface: http://localhost:4010"
echo "Press Ctrl+C to stop"
echo "=================================================="

# Set environment
export MIX_ENV=dev
export NODE_ROLE=secondary
export MIX_CONFIG=config/dev2.exs

# Start with proper node name, cookie, and config
exec iex --name superlearner2@localhost --cookie secret_cluster_cookie -S mix phx.server
