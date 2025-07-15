defmodule OTPSupervisor.DistributedTestCase do
  @moduledoc """
  Base test case for distributed functionality testing.
  
  Provides common setup, teardown, and utilities for testing distributed
  components. Handles both simulation-based and real multi-node testing.
  
  ## Usage
  
      defmodule MyDistributedTest do
        use OTPSupervisor.DistributedTestCase
        
        test "my distributed feature" do
          # Test code here - simulation is automatically managed
        end
        
        @tag :real_nodes
        test "with real nodes" do
          {:ok, node} = start_test_node("secondary")
          # Test with real node
          stop_test_node(node)
        end
      end
  """
  
  use ExUnit.CaseTemplate
  
  using do
    quote do
      import OTPSupervisor.DistributedTestCase
      alias OTPSupervisor.Distributed.{ToolManager, ClusterStateManager, SingleNodeSimulator}
      alias ClusterTestHelper
      
      # Import common test helpers
      import DebugHelpers
      import ClusterAssertions
    end
  end
  
  setup_all do
    # Ensure distributed Erlang is running (should already be started by test_helper.exs)
    unless Node.alive?() do
      # Try to start distributed Erlang with a unique name if not already started
      node_name = :"test_case_#{System.system_time(:millisecond)}@127.0.0.1"
      case Node.start(node_name, :shortnames) do
        {:ok, _} -> 
          Node.set_cookie(:test_cluster_cookie)
          IO.puts("Started distributed Erlang in test case: #{Node.self()}")
        {:error, {:already_started, _}} ->
          Node.set_cookie(:test_cluster_cookie)
        {:error, reason} ->
          IO.puts("Warning: Could not start distributed node: #{inspect(reason)}")
          IO.puts("Real node tests will be skipped")
      end
    else
      # Node is already alive, just ensure cookie is set
      Node.set_cookie(:test_cluster_cookie)
    end
    
    # Wait a bit for the application to fully start
    Process.sleep(100)
    
    :ok
  end
  
  setup tags do
    # Reset simulation state before each test
    try do
      OTPSupervisor.Distributed.SingleNodeSimulator.disable_simulation()
    catch
      _, _ -> :ok
    end
    
    # Setup based on test tags
    context = setup_test_context(tags)
    
    on_exit(fn ->
      cleanup_test_context(context)
    end)
    
    context
  end
  
  # Public API for tests
  
  @doc """
  Start a test node with automatic cleanup.
  """
  def start_test_node(suffix, opts \\ []) do
    ClusterTestHelper.start_test_node(suffix, opts)
  end
  
  @doc """
  Stop a test node gracefully.
  """
  def stop_test_node(node) do
    ClusterTestHelper.stop_test_node(node)
  end
  
  @doc """
  Enable simulation with automatic cleanup.
  """
  def enable_simulation(node_count \\ 3) do
    OTPSupervisor.Distributed.SingleNodeSimulator.enable_simulation(node_count)
  end
  
  @doc """
  Wait for cluster to reach expected size.
  """
  def wait_for_cluster(expected_nodes, timeout \\ 5000) do
    ClusterTestHelper.wait_for_cluster(expected_nodes, timeout)
  end
  
  @doc """
  Wait for a condition to be true.
  """
  def wait_until(fun, timeout \\ 5000) do
    ClusterTestHelper.wait_until(fun, timeout)
  end
  
  @doc """
  Verify distributed components are working.
  """
  def verify_distributed_components do
    ClusterTestHelper.verify_distributed_components()
  end
  
  # Private implementation
  
  defp setup_test_context(tags) do
    context = %{
      simulation_enabled: false,
      test_nodes: [],
      cleanup_functions: []
    }
    
    cond do
      tags[:simulation] ->
        setup_simulation_context(context, tags)
      
      tags[:real_nodes] ->
        setup_real_nodes_context(context, tags)
      
      tags[:hybrid] ->
        setup_hybrid_context(context, tags)
      
      true ->
        # Default: enable simulation for convenience
        setup_default_context(context)
    end
  end
  
  defp setup_simulation_context(context, tags) do
    node_count = tags[:node_count] || 3
    {:ok, _nodes} = OTPSupervisor.Distributed.SingleNodeSimulator.enable_simulation(node_count)
    
    %{context | 
      simulation_enabled: true,
      cleanup_functions: [&OTPSupervisor.Distributed.SingleNodeSimulator.disable_simulation/0 | context.cleanup_functions]
    }
  end
  
  defp setup_real_nodes_context(context, tags) do
    node_count = tags[:node_count] || 1
    
    nodes = for i <- 1..node_count do
      {:ok, node} = ClusterTestHelper.start_test_node("test_#{i}")
      node
    end
    
    # Wait for cluster formation
    :ok = ClusterTestHelper.wait_for_cluster(node_count + 1)  # +1 for primary
    
    cleanup_fn = fn ->
      Enum.each(nodes, &ClusterTestHelper.stop_test_node/1)
    end
    
    %{context |
      test_nodes: nodes,
      cleanup_functions: [cleanup_fn | context.cleanup_functions]
    }
  end
  
  defp setup_hybrid_context(context, tags) do
    # Enable simulation first
    sim_nodes = tags[:sim_nodes] || 2
    {:ok, _} = OTPSupervisor.Distributed.SingleNodeSimulator.enable_simulation(sim_nodes)
    
    # Add real nodes
    real_nodes = tags[:real_nodes] || 1
    nodes = for i <- 1..real_nodes do
      {:ok, node} = ClusterTestHelper.start_test_node("hybrid_#{i}")
      node
    end
    
    cleanup_fn = fn ->
      Enum.each(nodes, &ClusterTestHelper.stop_test_node/1)
      OTPSupervisor.Distributed.SingleNodeSimulator.disable_simulation()
    end
    
    %{context |
      simulation_enabled: true,
      test_nodes: nodes,
      cleanup_functions: [cleanup_fn | context.cleanup_functions]
    }
  end
  
  defp setup_default_context(context) do
    # Enable simulation by default for convenience
    {:ok, _nodes} = OTPSupervisor.Distributed.SingleNodeSimulator.enable_simulation(2)
    
    %{context |
      simulation_enabled: true,
      cleanup_functions: [&OTPSupervisor.Distributed.SingleNodeSimulator.disable_simulation/0 | context.cleanup_functions]
    }
  end
  
  defp cleanup_test_context(context) do
    # Run all cleanup functions
    Enum.each(context.cleanup_functions, fn cleanup_fn ->
      try do
        cleanup_fn.()
      catch
        _, error ->
          IO.warn("Cleanup function failed: #{inspect(error)}")
      end
    end)
  end
end