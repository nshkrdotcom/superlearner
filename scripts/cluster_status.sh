#!/bin/bash
# cluster_status.sh - Check status of both nodes and cluster connectivity

echo "🔍 Checking Cluster Status"
echo "========================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

# Check Node 1 status
echo -e "${BLUE}Node 1 Status (superlearner@localhost):${NC}"
if curl -s http://localhost:4000 > /dev/null; then
    print_success "✅ Web interface accessible at http://localhost:4000"
else
    print_error "❌ Web interface not accessible at http://localhost:4000"
fi

if pgrep -f "superlearner@localhost" > /dev/null; then
    print_success "✅ Elixir process running"
else
    print_error "❌ Elixir process not running"
fi

echo ""

# Check Node 2 status
echo -e "${BLUE}Node 2 Status (superlearner2@localhost):${NC}"
if curl -s http://localhost:4001 > /dev/null; then
    print_success "✅ Web interface accessible at http://localhost:4001"
else
    print_error "❌ Web interface not accessible at http://localhost:4001"
fi

if pgrep -f "superlearner2@localhost" > /dev/null; then
    print_success "✅ Elixir process running"
else
    print_error "❌ Elixir process not running"
fi

echo ""

# Check EPMD status
echo -e "${BLUE}EPMD Status:${NC}"
if command -v epmd &> /dev/null; then
    epmd_output=$(epmd -names 2>&1)
    if echo "$epmd_output" | grep -q "superlearner"; then
        print_success "✅ EPMD running with nodes:"
        echo "$epmd_output" | grep -E "(superlearner|port)" | sed 's/^/    /'
    else
        print_warning "⚠️ EPMD running but no superlearner nodes found"
        echo "$epmd_output"
    fi
else
    print_error "❌ EPMD not found"
fi

echo ""

# Check network connectivity
echo -e "${BLUE}Network Connectivity:${NC}"
if ping -c 1 localhost > /dev/null 2>&1; then
    print_success "✅ localhost connectivity OK"
else
    print_error "❌ localhost connectivity failed"
fi

# Check if ports are available
check_port() {
    local port=$1
    local service=$2
    
    if netstat -tuln 2>/dev/null | grep -q ":$port "; then
        print_success "✅ Port $port ($service) is in use"
    else
        print_warning "⚠️ Port $port ($service) is not in use"
    fi
}

check_port 4000 "Node 1 Phoenix"
check_port 4001 "Node 2 Phoenix"
check_port 4369 "EPMD"

echo ""

# Cluster connectivity test
echo -e "${BLUE}Cluster Connectivity Test:${NC}"
print_status "Testing inter-node communication..."

# Create a temporary Elixir script to test cluster connectivity
cat > /tmp/cluster_test.exs << 'EOF'
# Connect to Node 1 and test cluster
case Node.connect(:"superlearner@localhost") do
  true -> 
    IO.puts("✅ Successfully connected to superlearner@localhost")
    connected_nodes = Node.list()
    IO.puts("📡 Connected nodes: #{inspect(connected_nodes)}")
    
    # Test if we can see Node 2 from Node 1
    if :"superlearner2@localhost" in connected_nodes do
      IO.puts("✅ Node 2 is visible from Node 1")
    else
      IO.puts("❌ Node 2 is not visible from Node 1")
    end
    
  false -> 
    IO.puts("❌ Failed to connect to superlearner@localhost")
end
EOF

# Run the test if we can connect
if pgrep -f "superlearner@localhost" > /dev/null; then
    timeout 5 elixir --name cluster_test@localhost --cookie secret_cluster_cookie /tmp/cluster_test.exs 2>/dev/null || {
        print_warning "⚠️ Cluster connectivity test failed or timed out"
    }
else
    print_warning "⚠️ Cannot test cluster connectivity - Node 1 not running"
fi

# Clean up
rm -f /tmp/cluster_test.exs

echo ""
echo -e "${BLUE}Quick Commands:${NC}"
echo "Start Node 1: ./scripts/start_node1.sh"
echo "Start Node 2: ./scripts/start_node2.sh"
echo "Test Node 1: ./scripts/test_node1.sh"
echo "Test Node 2: ./scripts/test_node2.sh"
echo "Stop Node 1: ./scripts/stop_node1.sh"
echo "Stop Node 2: ./scripts/stop_node2.sh"
echo ""
echo -e "${BLUE}Web Interfaces:${NC}"
echo "Node 1: http://localhost:4000"
echo "Node 2: http://localhost:4001"
echo ""
echo -e "${BLUE}Manual Testing in IEx:${NC}"
echo "Node.self()           # Check current node"
echo "Node.list()           # List connected nodes"
echo "Node.ping(:<node>)    # Test connectivity"