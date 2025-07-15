# Task 4a: Distributed Test Infrastructure Tooling - Design Document

## Overview

Create robust tooling to manage distributed test clusters automatically, eliminating manual server management and ensuring reliable automated testing of distributed functionality.

## Problem Statement

**Current Issues:**
- Manual server management (`./scripts/start_node1.sh`, `./scripts/start_node2.sh`)
- Risk of testing against stale code from old running servers
- **CRITICAL: Distributed Erlang startup failures** - `{:EXIT, :nodistribution}` errors
- **CRITICAL: Network configuration issues** - 127.0.0.1 vs localhost binding problems
- **CRITICAL: Cookie authentication failures** - Inconsistent cookie management
- **CRITICAL: EPMD dependency issues** - Erlang Port Mapper Daemon not properly managed
- **CRITICAL: Test environment constraints** - `:peer` module requires current node to be alive
- Port conflicts and cleanup issues
- Unreliable test environment setup
- No validation that test nodes are running correct code
- Missing pre-flight checks for distributed testing environment
- No network connectivity validation before node startup
- Inadequate error diagnostics for distributed failures

**Goal:** 
Automated, reliable, isolated test cluster management with **robust distributed Erlang startup** and proper lifecycle controls.

## Architecture Design

### 1. CLI Tool: `mix cluster.test`

```bash
# Core commands
mix cluster.test start           # Start test cluster
mix cluster.test stop            # Stop test cluster  
mix cluster.test restart         # Restart test cluster
mix cluster.test status          # Show cluster status
mix cluster.test clean           # Clean up all test artifacts

# Advanced commands
mix cluster.test run             # Start cluster + run tests + cleanup
mix cluster.test logs [node]     # Show logs from test nodes
mix cluster.test shell [node]    # Connect to test node shell
mix cluster.test health          # Comprehensive health check
```

### 2. Test Cluster Manager

```elixir
defmodule OTPSupervisor.TestCluster.Manager do
  @moduledoc """
  Manages lifecycle of distributed test clusters.
  
  Features:
  - Automated node provisioning on dedicated ports
  - Code synchronization to ensure fresh deployments
  - Health monitoring and validation
  - Proper cleanup and isolation
  """
  
  # Configuration
  @test_ports %{
    node1: 4100,
    node2: 4101, 
    node3: 4102
  }
  
  @test_nodes %{
    node1: :"test_node1@127.0.0.1",
    node2: :"test_node2@127.0.0.1", 
    node3: :"test_node3@127.0.0.1"
  }
end
```

### 3. Node Provisioning System

```elixir
defmodule OTPSupervisor.TestCluster.NodeProvisioner do
  @moduledoc """
  Provisions and manages individual test nodes with robust distributed Erlang startup.
  
  CRITICAL FEATURES:
  - Pre-flight distributed Erlang environment validation
  - EPMD management and verification
  - Network configuration validation (127.0.0.1 vs localhost)
  - Cookie authentication management
  - Retry logic for node startup failures
  - Comprehensive error diagnostics
  """
  
  def provision_node(node_config) do
    with :ok <- validate_distributed_environment(),
         :ok <- ensure_epmd_running(),
         :ok <- validate_network_configuration(),
         :ok <- ensure_current_node_alive(),
         :ok <- ensure_port_available(node_config.port),
         {:ok, node} <- start_node_with_retry(node_config),
         :ok <- verify_node_connectivity(node),
         :ok <- sync_code(node),
         :ok <- validate_node_health(node) do
      {:ok, node}
    else
      error -> 
        diagnose_startup_failure(error, node_config)
        {:error, error}
    end
  end
  
  # CRITICAL: Address distributed Erlang startup issues
  defp validate_distributed_environment do
    checks = [
      &check_epmd_availability/0,
      &check_network_interfaces/0,
      &check_hostname_resolution/0,
      &check_port_availability/0
    ]
    
    run_environment_checks(checks)
  end
  
  defp ensure_current_node_alive do
    unless Node.alive?() do
      start_primary_test_node()
    else
      :ok
    end
  end
  
  defp start_node_with_retry(node_config, attempts \\ 3) do
    case start_node(node_config) do
      {:ok, node} -> {:ok, node}
      {:error, reason} when attempts > 1 ->
        :timer.sleep(1000)  # Wait before retry
        start_node_with_retry(node_config, attempts - 1)
      {:error, reason} ->
        {:error, {:max_retries_exceeded, reason}}
    end
  end
end
```

### 4. Health Verification System

```elixir
defmodule OTPSupervisor.TestCluster.HealthChecker do
  @moduledoc """
  Comprehensive health checking for test clusters.
  
  Validates:
  - Node connectivity and communication
  - Code version synchronization
  - Arsenal operations functionality
  - LibCluster formation
  - Network partition detection
  """
  
  def comprehensive_health_check(nodes) do
    checks = [
      &check_node_connectivity/1,
      &check_code_synchronization/1,
      &check_arsenal_operations/1,
      &check_cluster_formation/1,
      &check_distributed_functionality/1
    ]
    
    run_health_checks(nodes, checks)
  end
end
```

## Implementation Plan

### Phase 1: Core CLI Tool (Week 1)

#### 1.1 Create Mix Task Structure
```elixir
# lib/mix/tasks/cluster/test.ex
defmodule Mix.Tasks.Cluster.Test do
  use Mix.Task
  
  @shortdoc "Manage distributed test clusters"
  @moduledoc """
  Distributed test cluster management tool.
  
  ## Commands
  
      mix cluster.test start    # Start test cluster
      mix cluster.test stop     # Stop test cluster
      mix cluster.test status   # Show cluster status
      mix cluster.test run      # Full test cycle
  """
  
  def run(args) do
    case args do
      ["start"] -> start_cluster()
      ["stop"] -> stop_cluster()
      ["status"] -> show_status()
      ["run"] -> run_full_cycle()
      _ -> show_help()
    end
  end
end
```

#### 1.2 Basic Node Management
- Port allocation and management
- Node startup with proper configuration
- Basic health checking
- Cleanup mechanisms

#### 1.3 Integration with Test Suite
```elixir
# test/support/cluster_test_helper.ex - Enhanced
defmodule ClusterTestHelper do
  def ensure_test_cluster_running do
    case OTPSupervisor.TestCluster.Manager.status() do
      :running -> :ok
      :stopped -> start_test_cluster()
      :unhealthy -> restart_test_cluster()
    end
  end
end
```

### Phase 2: Advanced Features (Week 2)

#### 2.1 Code Synchronization
```elixir
defmodule OTPSupervisor.TestCluster.CodeSync do
  @moduledoc """
  Ensures all test nodes run the same code version.
  
  Features:
  - Compile and sync code to all nodes
  - Validate module versions across cluster
  - Hot code reloading for test nodes
  """
  
  def sync_code_to_cluster(nodes) do
    with :ok <- compile_project(),
         :ok <- sync_beam_files(nodes),
         :ok <- validate_code_versions(nodes) do
      :ok
    end
  end
end
```

#### 2.2 Environment Isolation
```elixir
defmodule OTPSupervisor.TestCluster.Isolation do
  @moduledoc """
  Provides complete isolation for test environments.
  
  Features:
  - Separate data directories per test run
  - Isolated ETS tables and persistent storage
  - Clean environment setup and teardown
  """
end
```

#### 2.3 Comprehensive Health Monitoring
- Network connectivity validation
- Arsenal operation testing
- Performance benchmarking
- Resource usage monitoring

### Phase 3: Production Features (Week 3)

#### 3.1 Advanced Diagnostics
```elixir
defmodule OTPSupervisor.TestCluster.Diagnostics do
  @moduledoc """
  Advanced diagnostic capabilities for test clusters.
  
  Features:
  - Network latency measurement
  - Resource usage profiling  
  - Error pattern analysis
  - Performance regression detection
  """
end
```

#### 3.2 CI/CD Integration
```bash
# .github/workflows/distributed-tests.yml
name: Distributed Tests
on: [push, pull_request]

jobs:
  distributed-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Setup Elixir
        uses: erlef/setup-beam@v1
      - name: Install dependencies
        run: mix deps.get
      - name: Run distributed tests
        run: mix cluster.test run
```

#### 3.3 Performance Testing
- Load testing with multiple nodes
- Latency measurement across operations
- Resource usage profiling
- Scalability testing

## Technical Specifications

### CRITICAL: Distributed Erlang Startup Requirements

#### Pre-flight Environment Validation
```elixir
@environment_checks [
  # EPMD Management
  {:epmd_running, "EPMD daemon is running on port 4369"},
  {:epmd_responsive, "EPMD responds to queries"},
  
  # Network Configuration  
  {:localhost_resolution, "localhost resolves correctly"},
  {:ip_127_binding, "127.0.0.1 interface is available"},
  {:hostname_resolution, "System hostname resolves"},
  
  # Port Availability
  {:test_ports_free, "Test ports 4100-4102 are available"},
  {:dist_ports_free, "Distribution ports 9100-9199 are available"},
  
  # Current Node Status
  {:current_node_alive, "Current node is running in distributed mode"},
  {:cookie_set, "Cluster cookie is properly configured"}
]
```

#### Enhanced Node Startup Process
```elixir
defp start_distributed_node(name, opts \\ []) do
  # CRITICAL: Address the specific issues from LIVE_CLUSTER_TESTING_ANALYSIS.md
  with :ok <- validate_network_setup(),
       :ok <- ensure_epmd_running(),
       :ok <- ensure_current_node_distributed(),
       {:ok, node} <- start_node_with_proper_config(name, opts),
       :ok <- set_cluster_cookie(node),
       :ok <- verify_node_connectivity(node),
       :ok <- wait_for_cluster_formation([node]) do
    {:ok, node}
  else
    {:error, :nodistribution} -> 
      diagnose_nodistribution_error(name, opts)
    {:error, :not_alive} ->
      diagnose_not_alive_error()
    {:error, reason} ->
      diagnose_startup_failure(reason, name, opts)
  end
end

defp ensure_current_node_distributed do
  unless Node.alive?() do
    # Start primary test node to enable distributed mode
    case Node.start(:"test_primary@127.0.0.1", :shortnames) do
      {:ok, _} -> 
        Node.set_cookie(:test_cluster_cookie)
        :ok
      {:error, {:already_started, _}} -> 
        :ok
      {:error, reason} ->
        {:error, {:primary_node_startup_failed, reason}}
    end
  else
    :ok
  end
end
```

### Port Management
```elixir
@test_port_ranges %{
  http: 4100..4199,      # HTTP endpoints
  distribution: 9100..9199, # Erlang distribution
  epmd: 4369             # Standard EPMD port
}
```

### Node Configuration
```elixir
@node_configs %{
  node1: %{
    name: :"test_node1@127.0.0.1",
    http_port: 4100,
    dist_port: 9100,
    cookie: :test_cluster_cookie,
    env: :test
  },
  node2: %{
    name: :"test_node2@127.0.0.1", 
    http_port: 4101,
    dist_port: 9101,
    cookie: :test_cluster_cookie,
    env: :test
  }
}
```

### Health Check Matrix
```elixir
@health_checks [
  # Basic connectivity
  {:ping, "Node responds to ping"},
  {:rpc, "RPC calls work correctly"},
  {:cookie, "Cookie authentication works"},
  
  # Application health
  {:app_started, "OTP Supervisor app running"},
  {:genservers, "All GenServers responding"},
  {:arsenal, "Arsenal operations functional"},
  
  # Cluster health  
  {:cluster_formed, "LibCluster formed correctly"},
  {:node_discovery, "All nodes see each other"},
  {:process_distribution, "Process distribution working"},
  
  # Performance
  {:latency, "Inter-node latency acceptable"},
  {:memory, "Memory usage within limits"},
  {:cpu, "CPU usage reasonable"}
]
```

## Usage Examples

### Development Workflow
```bash
# Start development with fresh cluster
mix cluster.test start
mix test --only real_nodes

# Make code changes, restart cluster with new code
mix cluster.test restart
mix test --only real_nodes

# Debug specific node
mix cluster.test shell node1

# Clean shutdown
mix cluster.test stop
```

### CI/CD Pipeline
```bash
# Full automated test cycle
mix cluster.test run --timeout 300 --verbose

# Equivalent to:
# mix cluster.test start
# mix test --only real_nodes
# mix cluster.test stop
```

### Debugging Failed Tests
```bash
# Check cluster health
mix cluster.test health

# View logs from all nodes
mix cluster.test logs

# Connect to problematic node
mix cluster.test shell node2

# Restart just one node
mix cluster.test restart node2
```

## Success Criteria

### Functional Requirements
- [ ] CLI tool provides all basic cluster management commands
- [ ] Automated node provisioning on dedicated ports
- [ ] Code synchronization ensures fresh deployments
- [ ] Health checks validate cluster functionality
- [ ] Proper cleanup prevents resource leaks
- [ ] Integration with existing test suite

### Non-Functional Requirements  
- [ ] Cluster startup time < 10 seconds
- [ ] Health check completion < 5 seconds
- [ ] Zero manual intervention required
- [ ] Reliable cleanup on failures
- [ ] Clear error messages and diagnostics
- [ ] CI/CD pipeline integration

### Quality Gates
- [ ] All existing distributed tests pass with new tooling
- [ ] No port conflicts or resource leaks
- [ ] Comprehensive error handling and recovery
- [ ] Documentation and usage examples
- [ ] Performance benchmarks established

## Risk Mitigation

### Technical Risks
- **Port conflicts**: Use dynamic port allocation with validation
- **Code sync failures**: Implement retry logic and validation
- **Node startup failures**: Comprehensive error handling and diagnostics
- **Resource leaks**: Robust cleanup with timeout mechanisms

### Process Risks
- **Complexity**: Start with MVP, iterate based on feedback
- **Testing**: Extensive testing of the testing infrastructure itself
- **Documentation**: Clear usage examples and troubleshooting guides

## Next Steps

1. **Create basic CLI structure** - Mix task with core commands
2. **Implement node provisioning** - Start/stop individual nodes
3. **Add health checking** - Basic connectivity and functionality tests
4. **Integrate with test suite** - Seamless integration with existing tests
5. **Add advanced features** - Code sync, diagnostics, performance testing

This tooling will transform our distributed testing from manual and error-prone to automated and reliable, enabling confident development of distributed features.