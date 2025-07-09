#!/bin/bash
# Shutdown Node 2 (port 4010)

cd "$(dirname "$0")"

echo "🛑 Shutting down Node 2 (port 4010)..."
./kill_beam_on_port.sh 4010