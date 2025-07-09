#!/bin/bash
# setup_node1.sh - Setup and start Node 1 (Primary)

set -e

echo "🚀 Setting up Node 1 (Primary) - superlearner@localhost"
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

print_status "Setting up database..."
mix ecto.setup 2>/dev/null || {
    print_warning "Database setup failed or already exists. Continuing..."
}

print_status "Creating Node 1 configuration..."

# Ensure config directory exists
mkdir -p config

# Create or update config/dev.exs for Node 1
cat > config/dev.exs << 'EOF'
import Config

# Configure your database
config :otp_supervisor, OtpSupervisor.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "otp_supervisor_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

# For development, we disable any cache and enable
# debugging and code reloading.
config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4000],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "a-very-long-secret-key-base-for-development-only-change-in-production",
  watchers: [
    esbuild: {Esbuild, :install_and_run, [:default, ~w(--sourcemap=inline --watch)]},
    tailwind: {Tailwind, :install_and_run, [:default, ~w(--watch)]}
  ]

# Watch static and templates for browser reloading.
config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  live_reload: [
    patterns: [
      ~r"priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"priv/gettext/.*(po)$",
      ~r"lib/otp_supervisor_web/(controllers|live|components)/.*(ex|heex)$"
    ]
  ]

# Enable dev routes for dashboard and mailbox
config :otp_supervisor, dev_routes: true

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"

# Set a higher stacktrace during development. Avoid configuring such
# in production as building large stacktraces may be expensive.
config :phoenix, :stacktrace_depth, 20

# Initialize plugs at runtime for faster development compilation
config :phoenix, :plug_init_mode, :runtime

# Node 1 specific configuration
config :superlearner, :node_name, :"superlearner@localhost"
config :superlearner, :node_port, 4000
config :superlearner, :node_role, :primary
EOF

print_status "Node 1 configuration created successfully!"

# Create startup script for Node 1
cat > scripts/start_node1.sh << 'EOF'
#!/bin/bash
# Start Node 1 (Primary)

cd "$(dirname "$0")/.."

echo "🚀 Starting Node 1 (Primary) - superlearner@localhost"
echo "Web interface: http://localhost:4000"
echo "Press Ctrl+C to stop"
echo "=================================================="

# Set environment
export MIX_ENV=dev
export NODE_ROLE=primary

# Start with proper node name and cookie
exec iex --name superlearner@localhost --cookie secret_cluster_cookie -S mix phx.server
EOF

chmod +x scripts/start_node1.sh

print_status "Node 1 startup script created at scripts/start_node1.sh"

# Create a quick test script
cat > scripts/test_node1.sh << 'EOF'
#!/bin/bash
# Test Node 1 connectivity

echo "Testing Node 1 connectivity..."
curl -s http://localhost:4000 > /dev/null && echo "✅ Node 1 web interface is accessible" || echo "❌ Node 1 web interface is not accessible"

# Test if node is running
if pgrep -f "superlearner@localhost" > /dev/null; then
    echo "✅ Node 1 Elixir process is running"
else
    echo "❌ Node 1 Elixir process is not running"
fi
EOF

chmod +x scripts/test_node1.sh

print_status "Node 1 test script created at scripts/test_node1.sh"

# Create a stop script
cat > scripts/stop_node1.sh << 'EOF'
#!/bin/bash
# Stop Node 1

echo "Stopping Node 1..."
pkill -f "superlearner@localhost"
echo "Node 1 stopped"
EOF

chmod +x scripts/stop_node1.sh

print_status "Node 1 stop script created at scripts/stop_node1.sh"

print_status "Node 1 setup complete!"
echo ""
echo -e "${BLUE}Next steps:${NC}"
echo "1. Run: ./scripts/start_node1.sh"
echo "2. Access web interface: http://localhost:4000"
echo "3. Test connectivity: ./scripts/test_node1.sh"
echo "4. In IEx console, verify node: Node.self()"
echo ""
echo -e "${YELLOW}Note:${NC} Make sure to run setup_node2.sh on your second WSL instance!"