#!/bin/bash
# setup_node2.sh - Setup and start Node 2 (Secondary)

set -e

echo "🚀 Setting up Node 2 (Secondary) - superlearner2@localhost"
echo "=================================================="

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

# Check if we're in the right directory
if [[ ! -f "mix.exs" ]]; then
    print_error "mix.exs not found. Please run this script from the superlearner project root."
    exit 1
fi

# Check if Elixir is installed
if ! command -v elixir &> /dev/null; then
    print_error "Elixir is not installed. Please install Elixir first."
    exit 1
fi

# Check if Node.js is installed (for assets)
if ! command -v npm &> /dev/null; then
    print_warning "Node.js/npm not found. Assets may not compile properly."
fi

print_status "Installing Elixir dependencies..."
mix deps.get

print_status "Compiling project..."
mix compile

# Install Node.js dependencies if package.json exists
if [[ -f "assets/package.json" ]]; then
    print_status "Installing Node.js dependencies..."
    cd assets && npm install && cd ..
fi

print_status "Setting up database (if needed)..."
mix ecto.setup 2>/dev/null || {
    print_warning "Database setup failed or already exists. Continuing..."
}

print_status "Creating Node 2 configuration..."

# Ensure config directory exists
mkdir -p config

# Create config/dev2.exs for Node 2 (imports from dev.exs and overrides)
cat > config/dev2.exs << 'EOF'
import Config

# Import base development configuration
import_config "dev.exs"

# Node 2 specific overrides
config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4010],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "a-very-long-secret-key-base-for-development-only-change-in-production"

# Node 2 specific configuration  
config :superlearner, :node_name, :"superlearner2@localhost"
config :superlearner, :node_port, 4010
config :superlearner, :node_role, :secondary

# Database configuration - use same database but different pool name to avoid conflicts
config :otp_supervisor, OtpSupervisor.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "otp_supervisor_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10
EOF

print_status "Node 2 configuration created successfully!"

# Create startup script for Node 2
cat > scripts/start_node2.sh << 'EOF'
#!/bin/bash
# Start Node 2 (Secondary)

cd "$(dirname "$0")/.."

echo "🚀 Starting Node 2 (Secondary) - superlearner2@localhost"
echo "Web interface: http://localhost:4010"
echo "Press Ctrl+C to stop"
echo "=================================================="

# Set environment
export MIX_ENV=dev
export NODE_ROLE=secondary

# Start with proper node name, cookie, and config
exec iex --name superlearner2@localhost --cookie secret_cluster_cookie --erl "-config dev2" -S mix phx.server
EOF

chmod +x scripts/start_node2.sh

print_status "Node 2 startup script created at scripts/start_node2.sh"

# Create a quick test script
cat > scripts/test_node2.sh << 'EOF'
#!/bin/bash
# Test Node 2 connectivity

echo "Testing Node 2 connectivity..."
curl -s http://localhost:4010 > /dev/null && echo "✅ Node 2 web interface is accessible" || echo "❌ Node 2 web interface is not accessible"

# Test if node is running
if pgrep -f "superlearner2@localhost" > /dev/null; then
    echo "✅ Node 2 Elixir process is running"
else
    echo "❌ Node 2 Elixir process is not running"
fi
EOF

chmod +x scripts/test_node2.sh

print_status "Node 2 test script created at scripts/test_node2.sh"

# Create a stop script
cat > scripts/stop_node2.sh << 'EOF'
#!/bin/bash
# Stop Node 2

echo "Stopping Node 2..."
pkill -f "superlearner2@localhost"
echo "Node 2 stopped"
EOF

chmod +x scripts/stop_node2.sh

print_status "Node 2 stop script created at scripts/stop_node2.sh"

print_status "Node 2 setup complete!"
echo ""
echo -e "${BLUE}Next steps:${NC}"
echo "1. Run: ./scripts/start_node2.sh"
echo "2. Access web interface: http://localhost:4010"
echo "3. Test connectivity: ./scripts/test_node2.sh"
echo "4. In IEx console, verify node: Node.self()"
echo "5. Check cluster: Node.list() (should show Node 1)"
echo ""
echo -e "${YELLOW}Note:${NC} Make sure Node 1 is running on the other WSL instance!"