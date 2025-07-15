#!/bin/bash
# Distributed Testing Script
# 
# Provides convenient commands for running different types of distributed tests

set -e

cd "$(dirname "$0")/.."

echo "🧪 OTP Supervisor Distributed Testing"
echo "======================================"

# Function to show usage
show_usage() {
    echo "Usage: $0 [COMMAND]"
    echo ""
    echo "Commands:"
    echo "  simulation    Run simulation-based tests (fast, default)"
    echo "  integration   Run real multi-node integration tests"
    echo "  all          Run all distributed tests"
    echo "  manual       Setup for manual testing with two nodes"
    echo "  help         Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 simulation"
    echo "  $0 integration"
    echo "  $0 manual"
}

# Function to run simulation tests
run_simulation_tests() {
    echo "🎭 Running simulation-based distributed tests..."
    echo "These tests use SingleNodeSimulator and run quickly."
    echo ""
    
    mix test test/otp_supervisor/distributed/ \
        --exclude distributed \
        --exclude real_nodes \
        --trace
}

# Function to run integration tests
run_integration_tests() {
    echo "🌐 Running real multi-node integration tests..."
    echo "These tests require actual BEAM nodes and may be slower."
    echo ""
    
    # Ensure epmd is running
    epmd -daemon 2>/dev/null || true
    
    mix test test/otp_supervisor/distributed/ \
        --include distributed \
        --include real_nodes \
        --trace \
        --timeout 30000
}

# Function to run all tests
run_all_tests() {
    echo "🚀 Running all distributed tests..."
    echo ""
    
    echo "Phase 1: Simulation Tests"
    echo "------------------------"
    run_simulation_tests
    
    echo ""
    echo "Phase 2: Integration Tests"
    echo "-------------------------"
    run_integration_tests
    
    echo ""
    echo "✅ All distributed tests completed!"
}

# Function to setup manual testing
setup_manual_testing() {
    echo "🔧 Setting up manual distributed testing..."
    echo ""
    echo "This will help you start two nodes for manual testing."
    echo ""
    
    echo "Step 1: Start primary node (Terminal 1)"
    echo "---------------------------------------"
    echo "Run: ./scripts/start_node1.sh"
    echo ""
    
    echo "Step 2: Start secondary node (Terminal 2)"
    echo "-----------------------------------------"
    echo "Run: ./scripts/start_node2.sh"
    echo ""
    
    echo "Step 3: Test connection (in Terminal 1)"
    echo "---------------------------------------"
    echo "In the iex session, run:"
    echo "  Node.connect(:\"superlearner2@$(hostname)\")"
    echo "  Node.list()"
    echo ""
    
    echo "Step 4: Test distributed components"
    echo "----------------------------------"
    echo "  OTPSupervisor.Distributed.ToolManager.get_cluster_status()"
    echo "  OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()"
    echo ""
    
    echo "Step 5: Test Arsenal operations (Terminal 3)"
    echo "--------------------------------------------"
    echo "  curl http://localhost:4000/api/v1/cluster/health"
    echo "  curl http://localhost:4000/api/v1/cluster/topology"
    echo ""
    
    echo "For more examples, see README.md section 'Testing Distributed Features'"
}

# Function to run health check
run_health_check() {
    echo "🏥 Running distributed components health check..."
    echo ""
    
    mix run -e "
    alias DebugHelpers
    
    IO.puts(\"Starting health check...\")
    DebugHelpers.health_check_distributed_components()
    
    IO.puts(\"\nDumping cluster state...\")
    DebugHelpers.dump_cluster_state()
    "
}

# Main script logic
case "${1:-simulation}" in
    "simulation")
        run_simulation_tests
        ;;
    "integration")
        run_integration_tests
        ;;
    "all")
        run_all_tests
        ;;
    "manual")
        setup_manual_testing
        ;;
    "health")
        run_health_check
        ;;
    "help"|"-h"|"--help")
        show_usage
        ;;
    *)
        echo "❌ Unknown command: $1"
        echo ""
        show_usage
        exit 1
        ;;
esac