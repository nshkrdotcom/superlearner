#!/bin/bash
# Start Node 1 (primary) - Same Node Deployment

cd "$(dirname "$0")/.."

echo "🚀 Starting Node 1 (primary) - superlearner1"
echo "Web interface: http://localhost:4000"
echo "Press Ctrl+C to stop"
echo "=================================================="

# Set environment
export MIX_ENV=dev
export NODE_ROLE=primary

# Start with proper node name, cookie, and config
exec iex --sname superlearner1 --cookie secret_cluster_cookie -S mix phx.server
