# Distributed Testing Documentation

## Overview

This document describes the comprehensive testing strategy for distributed functionality in the OTP Supervisor Educational Platform. Our distributed testing system supports both automated simulation-based tests and real multi-node integration tests.

## Testing Architecture

### 1. **Test Categories**

#### **Simulation Tests** (Always Available)
- Use `SingleNodeSimulator` to fake multi-node behavior
- Run in standard test environment without external dependencies
- Fast execution, reliable, deterministic
- Cover 90% of distributed logic without real nodes

#### **Integration Tests** (Optional)
- Use real BEAM nodes for authentic distributed testing
- Require manual setup or CI/CD orchestration
- Test actual network communication, RPC calls, node failures
- Validate real-world distributed behavior

#### **Manual Tests** (Developer Workflow)
- Interactive testing with `./scripts/start_node1.sh` and `./scripts/start_node2.sh`
- Real-time debugging and exploration
- Documented in README.md with example outputs

### 2. **Test Infrastructure Components**

```
test/
├── support/
│   ├── cluster_test_helper.ex          # Multi-node test utilities
│   └── distributed_test_case.ex        # Base test case for distributed tests
├── otp_supervisor/distributed/
│   ├── single_node_simulator_test.exs  # Simulator unit tests
│   ├── cluster_integration_test.exs    # Real multi-node tests
│   ├── multi_node_test.exs            # Hybrid simulation/real tests
│   └── arsenal_distributed_test.exs    # Arsenal operations tests
└── integration/
    └── full_cluster_test.exs           # End-to-end cluster tests
```

## Test Execution Strategies

### **Standard Test Run** (Default)
```bash
mix test
```
- Runs all simulation-based tests
- No external dependencies required
- Fast execution (~2-5 seconds for distributed tests)
- Uses `SingleNodeSimulator` for multi-node behavior

### **Distributed Integration Tests**
```bash
# Run tests that require real nodes
mix test --include distributed

# Run only distributed tests
mix test test/otp_supervisor/distributed/ --include distributed

# Run with verbose output
mix test --include distributed --trace
```

### **Manual Multi-Node Testing**
```bash
# Terminal 1: Start primary test node
iex --sname test_primary@127.0.0.1 --cookie test_cluster_cookie -S mix

# Terminal 2: Start secondary test node  
iex --sname test_secondary@127.0.0.1 --cookie test_cluster_cookie -S mix

# Terminal 3: Run integration tests
mix test test/otp_supervisor/distributed/multi_node_test.exs --include distributed
```

## Test Helper System

### **ClusterTestHelper** (`test/support/cluster_test_helper.ex`)

Provides utilities for managing test nodes and cluster operations:

```elixir
# Start a test node
{:ok, node} = ClusterTestHelper.start_test_node("secondary")

# Wait for cluster formation
:ok = ClusterTestHelper.wait_for_cluster(2, timeout: 5000)

# Verify distributed components are working
assert ClusterTestHelper.verify_distributed_components()

# Clean shutdown
ClusterTestHelper.stop_test_node(node)
```

#### **Key Functions:**

- `start_test_node/2` - Start a named test node with proper setup
- `stop_test_node/1` - Gracefully shutdown a test node
- `wait_for_cluster/2` - Wait for expected number of nodes to connect
- `wait_until/2` - Generic condition waiting with timeout
- `verify_distributed_components/0` - Verify our distributed services work
- `cluster_size/0` - Get current cluster size

### **DistributedTestCase** (Planned)

Base test case module for distributed tests:

```elixir
defmodule MyDistributedTest do
  use OTPSupervisor.DistributedTestCase
  
  # Automatically handles:
  # - Node startup/shutdown
  # - Cluster formation waiting
  # - Simulation mode management
  # - Cleanup between tests
end
```

## Testing Patterns

### **Pattern 1: Simulation-First Testing**

Most distributed tests should use simulation for speed and reliability:

```elixir
test "cluster health monitoring works" do
  # Enable simulation with 3 nodes
  {:ok, nodes} = SingleNodeSimulator.enable_simulation(3)
  
  # Test distributed functionality
  {:ok, health} = ClusterHealth.execute(%{})
  assert health.nodes_total == 3
  
  # Test failure scenarios
  SingleNodeSimulator.simulate_node_failure(List.first(nodes))
  {:ok, updated_health} = ClusterHealth.execute(%{})
  assert updated_health.nodes_healthy == 2
  
  # Cleanup
  SingleNodeSimulator.disable_simulation()
end
```

### **Pattern 2: Real Multi-Node Testing**

For testing actual network behavior:

```elixir
@tag :distributed
test "real cluster formation and communication" do
  {:ok, secondary} = ClusterTestHelper.start_test_node("test")
  
  try do
    # Wait for cluster to form
    :ok = ClusterTestHelper.wait_for_cluster(2)
    
    # Test real RPC calls
    result = :rpc.call(secondary, Enum, :count, [1..10])
    assert result == 10
    
    # Test our distributed components
    topology = ClusterStateManager.get_cluster_topology()
    assert secondary in topology.nodes
    
  after
    ClusterTestHelper.stop_test_node(secondary)
  end
end
```

### **Pattern 3: Hybrid Testing**

Combine simulation and real nodes for comprehensive coverage:

```elixir
test "handles mixed simulation and real nodes" do
  # Start with simulation
  {:ok, _sim_nodes} = SingleNodeSimulator.enable_simulation(2)
  
  # Add a real node
  {:ok, real_node} = ClusterTestHelper.start_test_node("real")
  
  try do
    # Test that both simulated and real nodes are handled
    cluster_status = ToolManager.get_cluster_status()
    assert cluster_status.mode == :multi_node
    
  after
    ClusterTestHelper.stop_test_node(real_node)
    SingleNodeSimulator.disable_simulation()
  end
end
```

## Advanced Testing Scenarios

### **Network Partition Testing**

```elixir
test "handles network partitions gracefully" do
  {:ok, node1} = ClusterTestHelper.start_test_node("partition1")
  {:ok, node2} = ClusterTestHelper.start_test_node("partition2")
  
  try do
    # Form cluster
    :ok = ClusterTestHelper.wait_for_cluster(3)
    
    # Create partition
    Node.disconnect(node1)
    Node.disconnect(node2)
    
    # Test partition detection
    :ok = ClusterTestHelper.wait_until(fn ->
      status = ClusterStateManager.get_partition_status()
      status in [:partial_partition, :minority_partition]
    end)
    
  after
    ClusterTestHelper.stop_test_node(node1)
    ClusterTestHelper.stop_test_node(node2)
  end
end
```

### **Load Testing**

```elixir
test "handles high load across cluster" do
  {:ok, nodes} = start_cluster(5)  # Helper to start 5 nodes
  
  try do
    # Generate load across all nodes
    tasks = for node <- nodes do
      Task.async(fn ->
        :rpc.call(node, :timer, :sleep, [100])
      end)
    end
    
    # Wait for all tasks
    Task.await_many(tasks, 5000)
    
    # Verify cluster health under load
    {:ok, health} = ClusterHealth.execute(%{})
    assert health.overall_status in [:healthy, :warning]
    
  after
    cleanup_cluster(nodes)
  end
end
```

## CI/CD Integration

### **GitHub Actions Example**

```yaml
name: Distributed Tests

on: [push, pull_request]

jobs:
  distributed-tests:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Setup Elixir
      uses: erlef/setup-beam@v1
      with:
        elixir-version: '1.18.3'
        otp-version: '27.3.3'
    
    - name: Install dependencies
      run: mix deps.get
    
    - name: Run simulation tests
      run: mix test test/otp_supervisor/distributed/
    
    - name: Run integration tests
      run: |
        # Start background nodes for integration tests
        epmd -daemon
        mix test --include distributed
```

### **Docker-Based Testing**

```dockerfile
# Dockerfile.test
FROM elixir:1.18.3-otp-27

WORKDIR /app
COPY . .

RUN mix deps.get
RUN mix compile

# Start multiple nodes for testing
CMD ["mix", "test", "--include", "distributed"]
```

## Test Data Management

### **Test Fixtures**

```elixir
# test/fixtures/cluster_scenarios.ex
defmodule ClusterScenarios do
  def healthy_cluster do
    %{
      nodes: 3,
      partitions: [],
      failed_nodes: []
    }
  end
  
  def partitioned_cluster do
    %{
      nodes: 5,
      partitions: [[:node1, :node2], [:node3, :node4, :node5]],
      failed_nodes: []
    }
  end
end
```

### **Mock Data Generators**

```elixir
# test/support/cluster_data_generator.ex
defmodule ClusterDataGenerator do
  def generate_process_list(node, count \\ 100) do
    for i <- 1..count do
      %{
        pid: "<#{node_id(node)}.#{i}.0>",
        name: "process_#{i}",
        memory: :rand.uniform(1000000),
        node: node
      }
    end
  end
  
  def generate_node_health(node, status \\ :up) do
    %{
      name: node,
      status: status,
      memory_usage: %{total: 100_000_000, processes: 50_000_000},
      cpu_usage: :rand.uniform(100),
      processes: :rand.uniform(1000)
    }
  end
end
```

## Performance Testing

### **Latency Testing**

```elixir
test "distributed operations have acceptable latency" do
  {:ok, nodes} = start_cluster(3)
  
  try do
    # Measure Arsenal operation latency
    {time, {:ok, _result}} = :timer.tc(fn ->
      ClusterHealth.execute(%{"include_metrics" => true})
    end)
    
    # Should complete within 100ms for 3 nodes
    assert time < 100_000  # microseconds
    
  after
    cleanup_cluster(nodes)
  end
end
```

### **Throughput Testing**

```elixir
test "can handle concurrent distributed requests" do
  {:ok, nodes} = start_cluster(3)
  
  try do
    # Start 50 concurrent requests
    tasks = for _i <- 1..50 do
      Task.async(fn ->
        ClusterHealth.execute(%{})
      end)
    end
    
    # All should complete successfully
    results = Task.await_many(tasks, 10_000)
    assert Enum.all?(results, &match?({:ok, _}, &1))
    
  after
    cleanup_cluster(nodes)
  end
end
```

## Debugging Distributed Tests

### **Logging Configuration**

```elixir
# config/test.exs
config :logger, level: :debug

# Enable distributed debugging
config :otp_supervisor, :distributed_tooling,
  debug_mode: true,
  log_rpc_calls: true,
  trace_cluster_events: true
```

### **Test Debugging Helpers**

```elixir
# test/support/debug_helpers.ex
defmodule DebugHelpers do
  def dump_cluster_state do
    IO.puts("=== Cluster State ===")
    IO.puts("Current node: #{Node.self()}")
    IO.puts("Connected nodes: #{inspect(Node.list())}")
    
    topology = ClusterStateManager.get_cluster_topology()
    IO.puts("Topology: #{inspect(topology)}")
    
    cluster_status = ToolManager.get_cluster_status()
    IO.puts("Tool Manager Status: #{inspect(cluster_status)}")
  end
  
  def trace_rpc_calls(node, module, function, args) do
    IO.puts("RPC Call: #{node}.#{module}.#{function}(#{inspect(args)})")
    result = :rpc.call(node, module, function, args)
    IO.puts("RPC Result: #{inspect(result)}")
    result
  end
end
```

## Future Enhancements

### **Planned Test Helper Features**

1. **Automatic Node Discovery**
   - Auto-detect available test nodes
   - Dynamic cluster sizing based on available resources

2. **Chaos Testing Framework**
   - Random node failures during tests
   - Network latency simulation
   - Resource exhaustion testing

3. **Visual Test Reporting**
   - Cluster topology visualization during tests
   - Performance metrics graphs
   - Test coverage heat maps

4. **Property-Based Testing**
   - Generate random cluster configurations
   - Test distributed properties with StreamData
   - Verify consistency across all scenarios

### **Integration with Development Workflow**

1. **Pre-commit Hooks**
   - Run simulation tests before commits
   - Validate distributed API contracts

2. **Development Server Integration**
   - Hot-reload distributed tests during development
   - Real-time test feedback in development UI

3. **Performance Regression Detection**
   - Benchmark distributed operations
   - Alert on performance degradation

## Best Practices

### **Test Organization**
- Use simulation tests for logic validation
- Use integration tests for network behavior
- Keep tests focused and independent
- Clean up resources in `after` blocks

### **Performance Considerations**
- Prefer simulation over real nodes for speed
- Use timeouts appropriate for test complexity
- Batch related tests to minimize node startup overhead

### **Reliability Guidelines**
- Always clean up test nodes
- Use unique node names to avoid conflicts
- Handle node startup failures gracefully
- Add retries for flaky network operations

### **Documentation Standards**
- Document test scenarios and expected outcomes
- Include setup requirements for integration tests
- Provide troubleshooting guides for common issues
- Keep examples up-to-date with code changes

This testing system provides comprehensive coverage of distributed functionality while maintaining fast feedback loops for developers and reliable CI/CD integration.