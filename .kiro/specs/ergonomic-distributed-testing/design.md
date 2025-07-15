# Design Document

## Overview

This design integrates the robust cluster testing system into Mix's test workflow, making distributed testing as ergonomic as regular testing. The solution automatically detects when tests need a cluster, starts one if needed, and cleans up afterward, all while providing clear feedback and configuration options.

## Architecture

### High-Level Integration Flow

```
Mix Test Execution
├── Test Discovery & Analysis
│   ├── Scan for distributed test tags
│   ├── Determine cluster requirements
│   └── Check existing cluster status
├── Cluster Management Decision
│   ├── Start new cluster if needed
│   ├── Reuse existing cluster
│   └── Skip cluster for non-distributed tests
├── Test Execution
│   ├── Run tests with cluster context
│   ├── Provide cluster info to tests
│   └── Monitor cluster health
└── Cleanup & Reporting
    ├── Cleanup cluster if started by us
    ├── Generate test reports
    └── Provide performance metrics
```

### Core Components

#### 1. Mix Task Integration

```elixir
defmodule Mix.Tasks.Test.Distributed do
  @moduledoc """
  Enhanced Mix test task with automatic cluster management.
  
  Extends the standard `mix test` command with distributed testing capabilities:
  
  ## Usage
  
      mix test --distributed          # Run all tests with cluster management
      mix test --only distributed     # Run only distributed tests
      mix test --force-cluster        # Force cluster for all tests
      mix test --cluster-size 3       # Override cluster size
      mix test --cluster-debug        # Enable cluster debugging
  
  ## Configuration
  
      # config/test.exs
      config :otp_supervisor, :distributed_testing,
        auto_cluster: true,
        default_cluster_size: 2,
        cluster_startup_timeout: 30_000,
        cluster_cleanup_timeout: 10_000,
        reuse_clusters: true
  """
  
  use Mix.Task
  
  @switches [
    distributed: :boolean,
    force_cluster: :boolean,
    cluster_size: :integer,
    cluster_debug: :boolean,
    cluster_timeout: :integer
  ]
  
  def run(args) do
    {opts, remaining_args} = OptionParser.parse!(args, switches: @switches)
    
    # Analyze test requirements
    test_analysis = analyze_test_requirements(remaining_args, opts)
    
    # Manage cluster lifecycle
    cluster_context = manage_cluster_lifecycle(test_analysis)
    
    # Run tests with cluster context
    run_tests_with_cluster(remaining_args, opts, cluster_context)
  end
  
  defp analyze_test_requirements(args, opts) do
    %{
      needs_cluster: determine_cluster_need(args, opts),
      cluster_size: determine_cluster_size(args, opts),
      test_files: discover_test_files(args),
      distributed_tests: scan_for_distributed_tests(args)
    }
  end
  
  defp manage_cluster_lifecycle(analysis) do
    if analysis.needs_cluster do
      start_or_reuse_cluster(analysis)
    else
      %{cluster_active: false, cluster_managed: false}
    end
  end
end
```

#### 2. Test Discovery and Analysis

```elixir
defmodule OTPSupervisor.Testing.TestAnalyzer do
  @moduledoc """
  Analyzes test files to determine distributed testing requirements.
  """
  
  def analyze_test_files(file_patterns) do
    file_patterns
    |> expand_file_patterns()
    |> Enum.map(&analyze_single_file/1)
    |> aggregate_requirements()
  end
  
  defp analyze_single_file(file_path) do
    file_path
    |> File.read!()
    |> extract_test_metadata()
    |> analyze_distributed_requirements()
  end
  
  defp extract_test_metadata(content) do
    %{
      has_distributed_tag: String.contains?(content, "@tag :distributed"),
      has_cluster_tag: String.contains?(content, "@tag :cluster"),
      has_multi_node_tag: String.contains?(content, "@tag :multi_node"),
      cluster_size_requirements: extract_cluster_size_tags(content),
      uses_cluster_helper: String.contains?(content, "ClusterTestHelper"),
      uses_node_operations: String.contains?(content, "Node.list()") or 
                           String.contains?(content, "Node.connect")
    }
  end
  
  defp analyze_distributed_requirements(metadata) do
    %{
      needs_cluster: metadata.has_distributed_tag or 
                    metadata.has_cluster_tag or 
                    metadata.has_multi_node_tag or
                    metadata.uses_cluster_helper,
      min_cluster_size: determine_min_cluster_size(metadata),
      test_type: determine_test_type(metadata)
    }
  end
end
```

#### 3. Automatic Cluster Manager

```elixir
defmodule OTPSupervisor.Testing.AutoClusterManager do
  @moduledoc """
  Manages automatic cluster lifecycle for distributed testing.
  
  Integrates with the existing TestCluster.Manager but adds:
  - Automatic startup/shutdown
  - Cluster reuse across test runs
  - Configuration management
  - Error handling and recovery
  """
  
  use GenServer
  
  alias OTPSupervisor.TestCluster.{Manager, Diagnostics}
  
  def start_cluster_for_tests(requirements) do
    GenServer.call(__MODULE__, {:start_cluster_for_tests, requirements}, 60_000)
  end
  
  def get_cluster_info do
    GenServer.call(__MODULE__, :get_cluster_info)
  end
  
  def cleanup_if_managed do
    GenServer.call(__MODULE__, :cleanup_if_managed)
  end
  
  def handle_call({:start_cluster_for_tests, requirements}, _from, state) do
    case check_existing_cluster(requirements) do
      {:ok, :reuse_existing} ->
        cluster_info = get_existing_cluster_info()
        new_state = %{state | cluster_info: cluster_info, managed: false}
        {:reply, {:ok, cluster_info}, new_state}
        
      {:error, :no_suitable_cluster} ->
        case start_new_cluster(requirements) do
          {:ok, cluster_info} ->
            new_state = %{state | cluster_info: cluster_info, managed: true}
            {:reply, {:ok, cluster_info}, new_state}
            
          {:error, reason} ->
            diagnosis = Diagnostics.diagnose_startup_failure(reason)
            {:reply, {:error, diagnosis}, state}
        end
    end
  end
  
  defp check_existing_cluster(requirements) do
    case Manager.get_cluster_status() do
      {:ok, %{status: :running, nodes: nodes}} when length(nodes) >= requirements.min_cluster_size ->
        {:ok, :reuse_existing}
      _ ->
        {:error, :no_suitable_cluster}
    end
  end
  
  defp start_new_cluster(requirements) do
    cluster_opts = [
      node_count: requirements.min_cluster_size,
      startup_timeout: get_config(:cluster_startup_timeout, 30_000),
      managed_by: :auto_test_manager
    ]
    
    Manager.start_cluster(cluster_opts)
  end
end
```

#### 4. Enhanced Test Configuration

```elixir
# config/test.exs
config :otp_supervisor, :distributed_testing,
  # Automatic cluster management
  auto_cluster: true,
  reuse_clusters: true,
  
  # Cluster configuration
  default_cluster_size: 2,
  max_cluster_size: 5,
  cluster_startup_timeout: 30_000,
  cluster_cleanup_timeout: 10_000,
  
  # Port configuration
  http_port_base: 4200,  # Different from dev to avoid conflicts
  dist_port_base: 9200,
  
  # Environment detection
  ci_mode: System.get_env("CI") != nil,
  ci_cluster_size: 2,  # Smaller clusters in CI
  ci_timeout_multiplier: 2.0,
  
  # Debugging
  verbose_cluster_logs: false,
  save_cluster_logs: false,
  
  # Test organization
  distributed_test_timeout: 60_000,
  cluster_test_timeout: 120_000
```

#### 5. Enhanced Test Helpers

```elixir
defmodule OTPSupervisor.Testing.DistributedTestCase do
  @moduledoc """
  Test case template for distributed tests with automatic cluster management.
  
  ## Usage
  
      defmodule MyDistributedTest do
        use OTPSupervisor.Testing.DistributedTestCase
        
        @tag :distributed
        test "distributed functionality" do
          # Cluster is automatically available
          nodes = cluster_nodes()
          assert length(nodes) >= 2
        end
        
        @tag cluster_size: 3
        test "three node functionality" do
          # Cluster with 3 nodes is automatically available
          nodes = cluster_nodes()
          assert length(nodes) == 3
        end
      end
  """
  
  defmacro __using__(opts) do
    quote do
      use ExUnit.Case, unquote(opts)
      
      import OTPSupervisor.Testing.DistributedTestCase
      
      setup_all do
        cluster_info = OTPSupervisor.Testing.AutoClusterManager.get_cluster_info()
        {:ok, cluster_info: cluster_info}
      end
      
      setup %{cluster_info: cluster_info} do
        # Ensure cluster is healthy before each test
        case cluster_info do
          %{nodes: nodes} when length(nodes) > 0 ->
            wait_for_cluster_health(nodes)
            {:ok, cluster_nodes: nodes}
          _ ->
            {:ok, cluster_nodes: []}
        end
      end
    end
  end
  
  def cluster_nodes do
    case OTPSupervisor.Testing.AutoClusterManager.get_cluster_info() do
      %{nodes: nodes} -> nodes
      _ -> []
    end
  end
  
  def wait_for_cluster_health(nodes, timeout \\ 5000) do
    ClusterTestHelper.wait_for_cluster(length(nodes), timeout)
  end
  
  def with_cluster_size(size, test_fun) do
    current_nodes = cluster_nodes()
    
    if length(current_nodes) >= size do
      test_fun.(Enum.take(current_nodes, size))
    else
      ExUnit.skip("Insufficient cluster size: need #{size}, have #{length(current_nodes)}")
    end
  end
end
```

## Components and Interfaces

### Mix Task Integration Points

#### Command Line Interface
```bash
# Standard usage
mix test --distributed              # Auto-detect and manage clusters
mix test --only distributed         # Only distributed tests
mix test --exclude distributed      # Skip distributed tests

# Advanced usage
mix test --force-cluster            # Force cluster for all tests
mix test --cluster-size 3           # Override cluster size
mix test --cluster-debug            # Enable cluster debugging
mix test --no-cluster-reuse         # Force new cluster

# CI/CD usage
CI=true mix test --distributed      # CI-optimized settings
mix test --distributed --ci-mode    # Explicit CI mode
```

#### Configuration Interface
```elixir
# Application configuration
config :otp_supervisor, :distributed_testing,
  auto_cluster: true,
  default_cluster_size: 2,
  reuse_clusters: true,
  cluster_startup_timeout: 30_000

# Runtime configuration
Application.put_env(:otp_supervisor, :distributed_testing, 
  cluster_size: 3, reuse_clusters: false)

# Test-specific configuration
@tag cluster_size: 4
@tag cluster_timeout: 60_000
@tag cluster_config: %{strategy: :one_for_all}
```

### Test Organization Patterns

#### Test Tagging Strategy
```elixir
defmodule MyDistributedTests do
  use OTPSupervisor.Testing.DistributedTestCase
  
  # Basic distributed test
  @tag :distributed
  test "basic cluster functionality" do
    nodes = cluster_nodes()
    assert length(nodes) >= 2
  end
  
  # Specific cluster size requirement
  @tag :distributed
  @tag cluster_size: 3
  test "three node consensus" do
    with_cluster_size(3, fn nodes ->
      # Test logic requiring exactly 3 nodes
    end)
  end
  
  # Performance-sensitive test
  @tag :distributed
  @tag :performance
  @tag cluster_size: 5
  test "high load distributed processing" do
    # Large cluster test
  end
  
  # Integration test
  @tag :distributed
  @tag :integration
  test "full system integration" do
    # End-to-end distributed test
  end
end
```

#### Test Helper Integration
```elixir
defmodule MyClusterTests do
  use ExUnit.Case
  
  # Manual cluster management (backward compatibility)
  setup do
    {:ok, nodes} = ClusterTestHelper.start_test_cluster(2)
    on_exit(fn -> ClusterTestHelper.stop_test_cluster(nodes) end)
    {:ok, nodes: nodes}
  end
  
  # Automatic cluster management
  @tag :distributed
  test "automatic cluster test" do
    # Cluster automatically available
    nodes = OTPSupervisor.Testing.DistributedTestCase.cluster_nodes()
    # Test logic
  end
end
```

## Data Models

### Cluster Requirements Analysis
```elixir
%{
  needs_cluster: boolean(),
  min_cluster_size: integer(),
  max_cluster_size: integer(),
  test_type: :distributed | :cluster | :multi_node | :integration,
  timeout_requirements: integer(),
  special_config: map()
}
```

### Cluster Context Information
```elixir
%{
  cluster_active: boolean(),
  cluster_managed: boolean(),  # Whether we started it
  nodes: [atom()],
  cluster_size: integer(),
  startup_time: DateTime.t(),
  cluster_id: String.t(),
  performance_metrics: map()
}
```

### Test Execution Report
```elixir
%{
  total_tests: integer(),
  distributed_tests: integer(),
  cluster_startup_time: integer(),
  cluster_reused: boolean(),
  cluster_cleanup_time: integer(),
  errors: [map()],
  performance_summary: map()
}
```

## Error Handling

### Cluster Startup Failures
```elixir
defmodule OTPSupervisor.Testing.ErrorHandler do
  def handle_cluster_startup_failure(reason, test_context) do
    diagnosis = Diagnostics.diagnose_startup_failure(reason)
    
    case determine_fallback_strategy(reason, test_context) do
      :skip_distributed_tests ->
        skip_tests_with_message(diagnosis)
        
      :retry_with_smaller_cluster ->
        retry_cluster_startup(test_context, smaller_size: true)
        
      :use_simulation_mode ->
        enable_simulation_mode(test_context)
        
      :fail_fast ->
        fail_with_detailed_error(diagnosis)
    end
  end
  
  defp skip_tests_with_message(diagnosis) do
    Mix.shell().info("⚠️  Skipping distributed tests: #{diagnosis.problem}")
    Mix.shell().info("💡 To fix this issue:")
    Enum.each(diagnosis.solutions, &Mix.shell().info("  • #{&1}"))
    
    # Configure ExUnit to skip distributed tests
    ExUnit.configure(exclude: [:distributed, :cluster, :multi_node])
  end
end
```

### Graceful Degradation
```elixir
defmodule OTPSupervisor.Testing.GracefulDegradation do
  def handle_partial_cluster_failure(failed_nodes, remaining_nodes, test_requirements) do
    if length(remaining_nodes) >= test_requirements.min_cluster_size do
      {:continue_with_reduced_cluster, remaining_nodes}
    else
      {:insufficient_cluster, suggest_alternatives(test_requirements)}
    end
  end
  
  defp suggest_alternatives(requirements) do
    [
      "Reduce cluster size requirement to #{length(remaining_nodes)}",
      "Use simulation mode for testing",
      "Run tests individually with smaller clusters",
      "Check system resources and try again"
    ]
  end
end
```

## Testing Strategy

### Integration Testing Approach
```elixir
defmodule OTPSupervisor.Testing.IntegrationTest do
  use ExUnit.Case
  
  @tag :integration
  test "complete workflow integration" do
    # Test the entire workflow from test discovery to cleanup
    
    # 1. Test discovery
    test_files = ["test/distributed_example_test.exs"]
    analysis = TestAnalyzer.analyze_test_files(test_files)
    assert analysis.needs_cluster
    
    # 2. Cluster startup
    {:ok, cluster_context} = AutoClusterManager.start_cluster_for_tests(analysis)
    assert cluster_context.cluster_active
    
    # 3. Test execution simulation
    # (We'll simulate rather than actually run tests to avoid recursion)
    
    # 4. Cleanup
    :ok = AutoClusterManager.cleanup_if_managed()
  end
end
```

### Performance Testing
```elixir
defmodule OTPSupervisor.Testing.PerformanceTest do
  use ExUnit.Case
  
  @tag :performance
  test "cluster startup performance" do
    start_time = System.monotonic_time(:millisecond)
    
    {:ok, _cluster} = AutoClusterManager.start_cluster_for_tests(%{min_cluster_size: 2})
    
    startup_time = System.monotonic_time(:millisecond) - start_time
    
    # Should start within 30 seconds
    assert startup_time < 30_000
    
    # Cleanup
    AutoClusterManager.cleanup_if_managed()
  end
end
```

This design provides a comprehensive solution for ergonomic distributed testing that integrates seamlessly with Mix's existing test workflow while maintaining all the robustness and reliability of our cluster testing system.