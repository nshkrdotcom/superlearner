#!/bin/bash
# Test Node 1 connectivity

echo "Testing Node 1 connectivity..."
curl -s http://localhost:4000 > /dev/null && echo "✅ Node 1 web interface is accessible" || echo "❌ Node 1 web interface is not accessible"

# Test if node is running
if pgrep -f "superlearner@U2401" > /dev/null; then
    echo "✅ Node 1 Elixir process is running"
else
    echo "❌ Node 1 Elixir process is not running"
fi
