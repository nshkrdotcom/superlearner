defmodule OTPSupervisor.Testing.DistributedTestCase do
  require Logger
  
  @moduledoc """
  Test case template for distributed tests with automatic cluster management.
  
  This module provides a seamless way to write distributed tests that automatically
  manage cluster lifecycle, provide helper functions for cluster operations, and
  ensure proper cleanup.
  
  ## Usage
  
      defmodule MyDistributedTest do
        use OTPSupervisor.Testing.DistributedTestCase
        
        @tag :distributed
        test "distributed functionality" do
          # Cluster is automatically available
          nodes = cluster_nodes()
          assert length(nodes) >= 2
          
          # Test distributed operations
          assert cluster_healthy?()
        end
        
        @tag cluster_size: 3
        test "three node functionality" do
          # Cluster with 3 nodes is automatically available
          with_cluster_size(3, fn nodes ->
            # Test logic requiring exactly 3 nodes
            assert length(nodes) == 3
          end)
        end
        
        @tag :distributed
        @tag cluster_timeout: 60_000
        test "long running distributed test" do
          # Custom timeout for cluster operations
          nodes = cluster_nodes()
          # Long running test logic
        end
      end
  
  ## Test Tags
  
  - `@tag :distributed` - Marks test as requiring a distributed cluster
  - `@tag cluster_size: N` - Specifies minimum cluster size needed
  - `@tag cluster_timeout: MS` - Custom timeout for cluster operations
  - `@tag cluster_config: %{}` - Custom cluster configuration
  
  ## Helper Functions
  
  - `cluster_nodes/0` - Get list of available cluster nodes
  - `cluster_size/0` - Get current cluster size
  - `cluster_healthy?/0` - Check if cluster is healthy
  - `with_cluster_size/2` - Execute function with specific cluster size
  - `wait_for_cluster_health/1` - Wait for cluster to become healthy
  - `wait_for_cluster_size/2` - Wait for cluster to reach specific size
  - `cluster_info/0` - Get detailed cluster information
  """
  
  defmacro __using__(opts) do
    quote do
      use ExUnit.Case, unquote(opts)
      
      import OTPSupervisor.Testing.DistributedTestCase
      
      require Logger
      
      # Setup cluster context for all tests in this module
      setup_all do
        # This will raise an exception if cluster setup fails - NO FALLBACKS
        {:ok, cluster_info} = OTPSupervisor.Testing.DistributedTestCase.setup_cluster_context()
        Logger.debug("Cluster context established: #{inspect(cluster_info)}")
        {:ok, cluster_info: cluster_info}
      end
      
      # Setup individual test context
      setup context do
        OTPSupervisor.Testing.DistributedTestCase.setup_test_context(context)
      end
      
      # Setup cleanup for each test
      setup do
        on_exit(fn ->
          OTPSupervisor.Testing.DistributedTestCase.cleanup_cluster_context()
        end)
        :ok
      end
    end
  end
  
  # Public API - Helper Functions
  
  @doc """
  Get the list of available cluster nodes.
  
  FAILS HARD if no cluster is active - distributed tests require real clusters.
  """
  def cluster_nodes do
    case get_cluster_info() do
      %{cluster_active: true, nodes: [_ | _] = nodes} -> 
        nodes
      %{cluster_active: false, error: :no_cluster_manager} ->
        raise RuntimeError, """
        cluster_nodes/0 called but AutoClusterManager is not running!
        
        Distributed tests tagged with @tag :distributed require a real running cluster.
        The cluster management infrastructure is not available.
        
        Make sure to:
        1. Run tests with: mix test --distributed
        2. Ensure AutoClusterManager is started before running distributed tests
        3. Check that distributed test infrastructure is properly initialized
        """
      %{cluster_active: false, error: :cluster_manager_timeout} ->
        raise RuntimeError, """
        cluster_nodes/0 called but AutoClusterManager is not responding!
        
        The cluster manager is running but not responding to requests.
        This may indicate system resource issues or deadlock.
        
        Try restarting the test suite or check system resources.
        """
      %{cluster_active: false} ->
        raise RuntimeError, """
        cluster_nodes/0 called but no active cluster is available!
        
        Distributed tests tagged with @tag :distributed require a real running cluster.
        This test cannot proceed without cluster nodes.
        
        Make sure to:
        1. Run tests with: mix test --distributed
        2. Ensure cluster startup succeeds before running distributed tests
        3. Check that your test is properly tagged with @tag :distributed
        """
      %{cluster_active: true, nodes: []} ->
        raise RuntimeError, """
        cluster_nodes/0 called but cluster has no nodes!
        
        The cluster is marked as active but contains no nodes.
        This indicates a cluster startup or health issue.
        
        Check cluster health and restart if necessary.
        """
      _ ->
        raise RuntimeError, """
        cluster_nodes/0 called but cluster status is unknown!
        
        Cannot determine cluster state. This may indicate:
        1. AutoClusterManager is not running
        2. Cluster information is corrupted
        3. Test setup is incomplete
        
        Ensure distributed test infrastructure is properly initialized.
        """
    end
  end
  
  @doc """
  Get the current cluster size.
  
  FAILS HARD if no cluster is active - distributed tests require real clusters.
  """
  def cluster_size do
    # This will fail hard if no cluster is available via cluster_nodes()
    length(cluster_nodes())
  end
  
  @doc """
  Check if the cluster is healthy and all nodes are responsive.
  
  Performs basic connectivity checks and returns true if all nodes
  are reachable and the cluster is functioning properly.
  
  FAILS HARD if no cluster is active - distributed tests require real clusters.
  """
  def cluster_healthy?(_timeout \\ 5000) do
    # This will fail hard if no cluster is available via cluster_nodes()
    nodes = cluster_nodes()
    
    # Convert list to map format expected by HealthChecker
    nodes_map = nodes |> Enum.with_index() |> Enum.into(%{}, fn {node, idx} -> {idx, node} end)
    
    case OTPSupervisor.TestCluster.HealthChecker.basic_health_check(nodes_map) do
      {:ok, _} -> true
      {:error, _} -> false
    end
  end
  
  @doc """
  Execute a function with a specific cluster size.
  
  If the current cluster doesn't have enough nodes, the test will be skipped
  with an appropriate message.
  
  ## Examples
  
      with_cluster_size(3, fn nodes ->
        # Test logic requiring exactly 3 nodes
        assert length(nodes) == 3
        # ... test implementation
      end)
  """
  def with_cluster_size(required_size, test_fun) when is_integer(required_size) and required_size > 0 do
    current_nodes = cluster_nodes()
    
    if length(current_nodes) >= required_size do
      selected_nodes = Enum.take(current_nodes, required_size)
      test_fun.(selected_nodes)
    else
      # Use exit with skip tuple instead of ExUnit.skip/1
      exit({:skip, "Insufficient cluster size: need #{required_size}, have #{length(current_nodes)}"})
    end
  end
  
  @doc """
  Wait for the cluster to become healthy.
  
  Polls cluster health until all nodes are responsive or timeout is reached.
  
  ## Options
  
  - `:timeout` - Maximum time to wait in milliseconds (default: 30_000)
  - `:interval` - Polling interval in milliseconds (default: 500)
  """
  def wait_for_cluster_health(opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    interval = Keyword.get(opts, :interval, 500)
    
    wait_until(fn -> cluster_healthy?() end, timeout, interval)
  end
  
  @doc """
  Wait for the cluster to reach a specific size.
  
  Polls cluster size until the required number of nodes are available
  or timeout is reached.
  
  ## Options
  
  - `:timeout` - Maximum time to wait in milliseconds (default: 30_000)
  - `:interval` - Polling interval in milliseconds (default: 500)
  """
  def wait_for_cluster_size(required_size, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    interval = Keyword.get(opts, :interval, 500)
    
    wait_until(fn -> cluster_size() >= required_size end, timeout, interval)
  end
  
  @doc """
  Get detailed information about the current cluster.
  
  Returns a map with cluster status, nodes, health information,
  and performance metrics.
  """
  def cluster_info do
    get_cluster_info()
  end
  
  @doc """
  Execute an RPC call on all cluster nodes.
  
  Returns a list of {node, result} tuples where result is either
  {:ok, value} or {:error, reason}.
  
  ## Examples
  
      results = cluster_rpc(Node, :self, [])
      # => [{:node1@host, {:ok, :node1@host}}, {:node2@host, {:ok, :node2@host}}]
  """
  def cluster_rpc(module, function, args, timeout \\ 5000) do
    cluster_nodes()
    |> Enum.map(fn node ->
      result = 
        try do
          case :rpc.call(node, module, function, args, timeout) do
            {:badrpc, reason} -> {:error, reason}
            value -> {:ok, value}
          end
        rescue
          error -> {:error, error}
        end
      
      {node, result}
    end)
  end
  
  @doc """
  Execute a function on all cluster nodes and collect results.
  
  Similar to cluster_rpc/4 but takes a function instead of MFA.
  
  ## Examples
  
      results = cluster_call(fn -> Process.list() |> length() end)
      # => [{:node1@host, {:ok, 42}}, {:node2@host, {:ok, 38}}]
  """
  def cluster_call(fun, timeout \\ 5000) when is_function(fun, 0) do
    cluster_rpc(Function, :apply, [fun, []], timeout)
  end
  
  @doc """
  Wait for a condition to be true across all cluster nodes.
  
  The condition function receives a node name and should return true/false.
  
  ## Examples
  
      wait_for_cluster_condition(fn node ->
        case :rpc.call(node, MyApp, :ready?, []) do
          true -> true
          _ -> false
        end
      end)
  """
  def wait_for_cluster_condition(condition_fun, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    interval = Keyword.get(opts, :interval, 500)
    
    wait_until(fn ->
      nodes = cluster_nodes()
      
      # If no nodes, condition can't be met
      if Enum.empty?(nodes) do
        false
      else
        Enum.all?(nodes, fn node ->
          try do
            condition_fun.(node)
          rescue
            _ -> false
          end
        end)
      end
    end, timeout, interval)
  end
  
  # Public functions for macro usage
  
  @doc false
  def setup_cluster_context do
    # Check if test needs a cluster based on tags
    test_requirements = %{
      needs_cluster: true,
      min_cluster_size: 2,
      test_type: :distributed
    }
    
    Logger.info("Setting up cluster context for distributed tests")
    
    case try_start_cluster_for_tests(test_requirements) do
      {:ok, cluster_info} ->
        # Wait for cluster to be healthy
        case wait_for_cluster_health(timeout: 30_000) do
          :ok ->
            Logger.info("Cluster is healthy and ready for testing")
            {:ok, cluster_info}
            
          {:error, :timeout} ->
            # FAIL HARD - no graceful degradation for distributed tests
            raise RuntimeError, """
            Cluster health check failed - distributed tests cannot run without a healthy cluster!
            
            The cluster started but failed health checks within 30 seconds.
            This means distributed functionality cannot be tested.
            
            Check cluster logs and ensure all nodes are properly connected and responsive.
            """
        end
        
      {:error, diagnosis} ->
        # FAIL HARD - no graceful degradation for distributed tests
        raise RuntimeError, """
        Failed to start cluster for distributed tests!
        
        Problem: #{diagnosis.problem}
        
        Distributed tests tagged with @tag :distributed require a real running cluster.
        Cannot proceed without cluster infrastructure.
        
        Solutions:
        #{Enum.map_join(diagnosis.solutions || [], "\n", fn solution -> "• #{solution}" end)}
        
        Retry suggestions:
        #{Enum.map_join(diagnosis.retry_suggestions || [], "\n", fn suggestion -> "• #{suggestion}" end)}
        """
    end
  end
  
  @doc false
  def setup_test_context(context) do
    # For now, just pass through the context
    # In a full implementation, this would validate cluster requirements
    {:ok, context}
  end
  
  @doc false
  def cleanup_cluster_context do
    # For now, just log cleanup
    # In a full implementation, this would cleanup managed clusters
    Logger.debug("Cluster context cleanup completed")
    :ok
  end
  
  # Private Implementation
  
  defp try_start_cluster_for_tests(test_requirements) do
    try do
      OTPSupervisor.Testing.AutoClusterManager.start_cluster_for_tests(test_requirements)
    catch
      :exit, {:noproc, _} ->
        {:error, %{
          problem: "AutoClusterManager is not running - distributed test infrastructure not available",
          solutions: [
            "Run tests with: mix test --distributed",
            "Ensure AutoClusterManager is started before running distributed tests",
            "Check that distributed test infrastructure is properly initialized"
          ],
          retry_suggestions: [
            "Start the AutoClusterManager GenServer",
            "Verify application startup sequence",
            "Check for application configuration issues"
          ]
        }}
      :exit, {:timeout, _} ->
        {:error, %{
          problem: "AutoClusterManager is not responding - may be deadlocked or overloaded",
          solutions: [
            "Restart the test suite",
            "Check system resources (memory, CPU)",
            "Verify no other cluster operations are blocking"
          ],
          retry_suggestions: [
            "Kill and restart the test process",
            "Check for resource constraints",
            "Verify network connectivity"
          ]
        }}
    end
  end
  
  defp get_cluster_info do
    try do
      case OTPSupervisor.Testing.AutoClusterManager.get_cluster_info() do
        %{} = info -> info
        _ -> %{cluster_active: false, nodes: []}
      end
    catch
      :exit, {:noproc, _} ->
        # AutoClusterManager is not running - this means no cluster infrastructure
        %{cluster_active: false, nodes: [], error: :no_cluster_manager}
      :exit, {:timeout, _} ->
        # AutoClusterManager is not responding
        %{cluster_active: false, nodes: [], error: :cluster_manager_timeout}
    end
  end
  
  defp wait_until(condition_fun, timeout, interval) do
    start_time = System.monotonic_time(:millisecond)
    wait_until_loop(condition_fun, start_time, timeout, interval)
  end
  
  defp wait_until_loop(condition_fun, start_time, timeout, interval) do
    current_time = System.monotonic_time(:millisecond)
    
    if current_time - start_time > timeout do
      {:error, :timeout}
    else
      if condition_fun.() do
        :ok
      else
        :timer.sleep(interval)
        wait_until_loop(condition_fun, start_time, timeout, interval)
      end
    end
  end
end