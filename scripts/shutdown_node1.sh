#!/bin/bash
# Shutdown Node 1 (port 4000)

cd "$(dirname "$0")"

echo "🛑 Shutting down Node 1 (port 4000)..."
./kill_beam_on_port.sh 4000