#!/bin/bash
# cluster_common.sh - Common functions and operations for WSL cluster setup

set -e

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

# Node configurations
get_node_config() {
    local node_num=$1
    local max_nodes=${MAX_NODES:-10}
    
    if [[ $node_num -lt 1 || $node_num -gt $max_nodes ]]; then
        print_error "Invalid node number: $node_num. Use 1-$max_nodes."
        exit 1
    fi
    
    # Get hostname from environment or use default
    local hostname=${NODE_HOSTNAME:-${HOSTNAME:-localhost}}
    
    # Dynamic configuration
    if [[ $node_num -eq 1 ]]; then
        NODE_NAME="superlearner@${hostname}"
    else
        NODE_NAME="superlearner${node_num}@${hostname}"
    fi
    
    # Calculate port dynamically (base port + (node_num - 1) * spacing)
    local base_port=${BASE_HTTP_PORT:-4000}
    local port_spacing=${PORT_SPACING:-10}
    NODE_PORT=$((base_port + (node_num - 1) * port_spacing))
    
    NODE_INDEX=$node_num
    NODE_ROLE=$([ $node_num -eq 1 ] && echo "primary" || echo "node${node_num}")
    CONFIG_FILE="dev"  # Use same config file with environment variables
}

# Setup function
setup_node() {
    local node_num=$1
    get_node_config $node_num
    
    echo "🚀 Setting up Node $node_num ($NODE_ROLE) - $NODE_NAME"
    echo "=================================================="
    
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
    
    if [[ $node_num == 1 ]]; then
        print_status "Setting up database..."
        mix ecto.setup 2>/dev/null || {
            print_warning "Database setup failed or already exists. Continuing..."
        }
    else
        print_status "Setting up database (if needed)..."
        mix ecto.setup 2>/dev/null || {
            print_warning "Database setup failed or already exists. Continuing..."
        }
    fi
    
    print_status "Creating Node $node_num configuration..."
    
    # Ensure config and scripts directories exist
    mkdir -p config scripts
    
    create_config_file $node_num
    create_startup_script $node_num
    create_test_script $node_num
    create_stop_script $node_num
    
    print_status "Node $node_num setup complete!"
    echo ""
    echo -e "${BLUE}Next steps:${NC}"
    echo "1. Run: ./scripts/start_node$node_num.sh"
    echo "2. Access web interface: http://localhost:$NODE_PORT"
    echo "3. Test connectivity: ./scripts/test_node$node_num.sh"
    echo "4. In IEx console, verify node: Node.self()"
    if [[ $node_num == 1 ]]; then
        echo ""
        echo -e "${YELLOW}Note:${NC} Start Node 2 in a different terminal with: ./scripts/start_node2.sh"
    else
        echo "5. Check cluster: Node.list() (should show Node 1)"
        echo ""
        echo -e "${YELLOW}Note:${NC} Make sure Node 1 is running in another terminal!"
    fi
}

# Create configuration file
create_config_file() {
    local node_num=$1
    get_node_config $node_num
    
    if [[ $node_num == 1 ]]; then
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
config :otp_supervisor, :node_name, :"superlearner@U2401"
config :otp_supervisor, :node_port, 4000
config :otp_supervisor, :node_role, :primary
EOF
    else
        # Create config/dev2.exs for Node 2
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
config :otp_supervisor, :node_name, :"superlearner2@U2402"
config :otp_supervisor, :node_port, 4010
config :otp_supervisor, :node_role, :secondary

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
    fi
    
    print_status "Node $node_num configuration created successfully!"
}

# Create startup script
create_startup_script() {
    local node_num=$1
    get_node_config $node_num
    
    cat > scripts/start_node$node_num.sh << EOF
#!/bin/bash
# Start Node $node_num ($NODE_ROLE)

cd "\$(dirname "\$0")/.."

echo "🚀 Starting Node $node_num ($NODE_ROLE) - $NODE_NAME"
echo "Web interface: http://localhost:$NODE_PORT"
echo "Press Ctrl+C to stop"
echo "=================================================="

# Set environment variables for dynamic configuration
export MIX_ENV=dev
export NODE_INDEX=$node_num
export NODE_HOSTNAME=${NODE_HOSTNAME:-${HOSTNAME:-localhost}}
export CLUSTER_SIZE=${CLUSTER_SIZE:-2}
export PHX_PORT=$NODE_PORT

# Start with proper node name, cookie, and config
exec iex --name $NODE_NAME --cookie secret_cluster_cookie -S mix phx.server
EOF
    
    chmod +x scripts/start_node$node_num.sh
    print_status "Node $node_num startup script created at scripts/start_node$node_num.sh"
}

# Create test script
create_test_script() {
    local node_num=$1
    get_node_config $node_num
    
    cat > scripts/test_node$node_num.sh << EOF
#!/bin/bash
# Test Node $node_num connectivity

echo "Testing Node $node_num connectivity..."
curl -s http://localhost:$NODE_PORT > /dev/null && echo "✅ Node $node_num web interface is accessible" || echo "❌ Node $node_num web interface is not accessible"

# Test if node is running
if pgrep -f "$NODE_NAME" > /dev/null; then
    echo "✅ Node $node_num Elixir process is running"
else
    echo "❌ Node $node_num Elixir process is not running"
fi
EOF
    
    chmod +x scripts/test_node$node_num.sh
    print_status "Node $node_num test script created at scripts/test_node$node_num.sh"
}

# Create stop script
create_stop_script() {
    local node_num=$1
    get_node_config $node_num
    
    cat > scripts/stop_node$node_num.sh << EOF
#!/bin/bash
# Stop Node $node_num

echo "Stopping Node $node_num..."
pkill -f "$NODE_NAME"
echo "Node $node_num stopped"
EOF
    
    chmod +x scripts/stop_node$node_num.sh
    print_status "Node $node_num stop script created at scripts/stop_node$node_num.sh"
}

# Start function
start_node() {
    local node_num=$1
    get_node_config $node_num
    
    cd "$(dirname "$0")/.."
    
    echo "🚀 Starting Node $node_num ($NODE_ROLE) - $NODE_NAME"
    echo "Web interface: http://localhost:$NODE_PORT"
    echo "Press Ctrl+C to stop"
    echo "=================================================="
    
    # Nuclear approach - kill any existing processes on our port and node name
    print_status "Cleaning up any existing processes..."
    
    # Kill any process using our target port (multiple approaches)
    sudo fuser -k $NODE_PORT/tcp 2>/dev/null || true
    lsof -ti:$NODE_PORT | xargs kill -9 2>/dev/null || true
    
    # Kill any Elixir/Erlang processes with our node name
    pkill -f "$NODE_NAME" 2>/dev/null || true
    pkill -f "superlearner" 2>/dev/null || true
    
    # Kill any beam processes 
    pkill -f "beam.smp" 2>/dev/null || true
    pkill -f "mix phx.server" 2>/dev/null || true
    pkill -f "iex" 2>/dev/null || true
    
    # Extra aggressive port cleanup
    netstat -tlnp | grep ":$NODE_PORT " | awk '{print $7}' | cut -d'/' -f1 | xargs kill -9 2>/dev/null || true
    
    # Wait a moment for cleanup
    sleep 2
    
    print_status "Port cleanup complete, checking availability..."
    if netstat -tlnp | grep ":$NODE_PORT " > /dev/null; then
        print_warning "Port $NODE_PORT still in use, proceeding anyway..."
    else
        print_success "Port $NODE_PORT is now available"
    fi
    
    print_status "Starting clean node..."
    
    # Set environment
    export MIX_ENV=dev
    export NODE_ROLE=$NODE_ROLE
    
    # Start with proper node name, cookie, and config
    if [[ $node_num == 2 ]]; then
        export MIX_CONFIG=config/dev2.exs
        exec iex --name $NODE_NAME --cookie secret_cluster_cookie -S mix phx.server
    else
        exec iex --name $NODE_NAME --cookie secret_cluster_cookie -S mix phx.server
    fi
}

# Test function
test_node() {
    local node_num=$1
    get_node_config $node_num
    
    echo "Testing Node $node_num connectivity..."
    curl -s http://localhost:$NODE_PORT > /dev/null && echo "✅ Node $node_num web interface is accessible" || echo "❌ Node $node_num web interface is not accessible"
    
    # Test if node is running
    if pgrep -f "$NODE_NAME" > /dev/null; then
        echo "✅ Node $node_num Elixir process is running"
    else
        echo "❌ Node $node_num Elixir process is not running"
    fi
}

# Stop function
stop_node() {
    local node_num=$1
    get_node_config $node_num
    
    echo "Stopping Node $node_num..."
    pkill -f "$NODE_NAME"
    echo "Node $node_num stopped"
}

# Main function to handle command line arguments
main() {
    local command=$1
    local node_num=$2
    
    case $command in
        setup)
            setup_node $node_num
            ;;
        start)
            start_node $node_num
            ;;
        test)
            test_node $node_num
            ;;
        stop)
            stop_node $node_num
            ;;
        *)
            echo "Usage: $0 {setup|start|test|stop} {1|2}"
            echo "  setup: Setup a node"
            echo "  start: Start a node"
            echo "  test:  Test node connectivity"
            echo "  stop:  Stop a node"
            exit 1
            ;;
    esac
}

# Only run main if script is executed directly (not sourced)
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi