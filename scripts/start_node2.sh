#!/bin/bash
# Start Node 2 (secondary) - Same Node Deployment

cd "$(dirname "$0")/.."

echo "🚀 Starting Node 2 (secondary) - superlearner2"
echo "Web interface: http://localhost:4010"
echo "Press Ctrl+C to stop"
echo "=================================================="

# Set environment variables for Node 2 configuration
export MIX_ENV=dev
export NODE_ROLE=secondary
export RELEASE_COOKIE=secret_cluster_cookie

# Start Node 2 with proper runtime configuration
exec iex --sname superlearner2 --cookie secret_cluster_cookie -S mix phx.server
