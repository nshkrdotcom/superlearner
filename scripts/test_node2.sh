#!/bin/bash
# Test Node 2 connectivity

echo "Testing Node 2 connectivity..."
curl -s http://localhost:4010 > /dev/null && echo "✅ Node 2 web interface is accessible" || echo "❌ Node 2 web interface is not accessible"

# Test if node is running
if pgrep -f "superlearner2@U2402" > /dev/null; then
    echo "✅ Node 2 Elixir process is running"
else
    echo "❌ Node 2 Elixir process is not running"
fi
