defmodule ClusterAssertions do
  @moduledoc """
  Custom assertions for distributed/cluster testing.
  
  Provides specialized assertion functions for validating distributed
  system behavior, cluster state, and multi-node operations.
  """
  
  import ExUnit.Assertions
  alias OTPSupervisor.Distributed.{ToolManager, ClusterStateManager}
  
  @doc """
  Assert that the cluster has the expected number of nodes.
  """
  def assert_cluster_size(expected_size) do
    actual_size = length([Node.self() | Node.list()])
    
    assert actual_size == expected_size,
      "Expected cluster size #{expected_size}, but got #{actual_size}. " <>
      "Current nodes: #{inspect([Node.self() | Node.list()])}"
  end
  
  @doc """
  Assert that a specific node is connected to the cluster.
  """
  def assert_node_connected(node) do
    connected_nodes = Node.list()
    
    assert node in connected_nodes,
      "Expected node #{node} to be connected, but it's not in: #{inspect(connected_nodes)}"
  end
  
  @doc """
  Assert that a node is not connected to the cluster.
  """
  def assert_node_disconnected(node) do
    connected_nodes = Node.list()
    
    refute node in connected_nodes,
      "Expected node #{node} to be disconnected, but it's still connected. " <>
      "Connected nodes: #{inspect(connected_nodes)}"
  end
  
  @doc """
  Assert that the ToolManager is in the expected mode.
  """
  def assert_tool_manager_mode(expected_mode) do
    cluster_status = ToolManager.get_cluster_status()
    actual_mode = cluster_status.mode
    
    assert actual_mode == expected_mode,
      "Expected ToolManager mode #{expected_mode}, but got #{actual_mode}"
  end
  
  @doc """
  Assert that the cluster topology contains expected nodes.
  """
  def assert_cluster_topology(expected_nodes) when is_list(expected_nodes) do
    topology = ClusterStateManager.get_cluster_topology()
    actual_nodes = topology.nodes
    
    missing_nodes = expected_nodes -- actual_nodes
    extra_nodes = actual_nodes -- expected_nodes
    
    assert missing_nodes == [],
      "Missing nodes from topology: #{inspect(missing_nodes)}"
    
    assert extra_nodes == [],
      "Unexpected nodes in topology: #{inspect(extra_nodes)}"
  end
  
  @doc """
  Assert that cluster health is in expected status.
  """
  def assert_cluster_health(expected_status) do
    {:ok, health_data} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.execute(%{})
    actual_status = health_data.overall_status
    
    assert actual_status == expected_status,
      "Expected cluster health #{expected_status}, but got #{actual_status}. " <>
      "Health data: #{inspect(health_data, limit: 3)}"
  end
  
  @doc """
  Assert that all nodes in the cluster are healthy.
  """
  def assert_all_nodes_healthy do
    {:ok, health_data} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.execute(%{})
    
    unhealthy_nodes = 
      health_data.node_statuses
      |> Enum.filter(fn {_node, status} -> status.status != :up end)
      |> Enum.map(fn {node, _status} -> node end)
    
    assert unhealthy_nodes == [],
      "Expected all nodes to be healthy, but these are not: #{inspect(unhealthy_nodes)}"
  end
  
  @doc """
  Assert that a specific node has the expected status.
  """
  def assert_node_status(node, expected_status) do
    {:ok, node_info} = OTPSupervisor.Core.Arsenal.Operations.Distributed.NodeInfo.execute(%{
      "node" => Atom.to_string(node)
    })
    
    actual_status = node_info.status
    
    assert actual_status == expected_status,
      "Expected node #{node} to have status #{expected_status}, but got #{actual_status}"
  end
  
  @doc """
  Assert that process distribution includes all expected nodes.
  """
  def assert_process_distribution(expected_nodes) do
    process_dist = ClusterStateManager.get_process_distribution()
    actual_nodes = Map.keys(process_dist)
    
    missing_nodes = expected_nodes -- actual_nodes
    
    assert missing_nodes == [],
      "Process distribution missing nodes: #{inspect(missing_nodes)}. " <>
      "Actual nodes: #{inspect(actual_nodes)}"
    
    # Also verify each node has processes
    Enum.each(expected_nodes, fn node ->
      processes = Map.get(process_dist, node, [])
      assert length(processes) > 0,
        "Expected node #{node} to have processes, but got #{length(processes)}"
    end)
  end
  
  @doc """
  Assert that an Arsenal operation succeeds.
  """
  def assert_arsenal_operation_succeeds(operation_module, params \\ %{}) do
    # First validate parameters
    case operation_module.validate_params(params) do
      {:ok, validated_params} ->
        # Then execute with validated parameters
        result = operation_module.execute(validated_params)
        
        assert match?({:ok, _}, result),
          "Expected Arsenal operation #{operation_module} to succeed, but got: #{inspect(result)}"
        
        {:ok, data} = result
        data
        
      {:error, validation_error} ->
        flunk("Arsenal operation #{operation_module} parameter validation failed: #{inspect(validation_error)}")
    end
  end
  
  @doc """
  Assert that an Arsenal operation fails with expected error.
  """
  def assert_arsenal_operation_fails(operation_module, params \\ %{}, expected_error \\ nil) do
    result = operation_module.execute(params)
    
    assert match?({:error, _}, result),
      "Expected Arsenal operation #{operation_module} to fail, but got: #{inspect(result)}"
    
    if expected_error do
      {:error, actual_error} = result
      assert actual_error == expected_error,
        "Expected error #{inspect(expected_error)}, but got #{inspect(actual_error)}"
    end
    
    result
  end
  
  @doc """
  Assert that simulation is enabled with expected node count.
  """
  def assert_simulation_enabled(expected_node_count \\ nil) do
    simulator = OTPSupervisor.Distributed.SingleNodeSimulator
    
    assert simulator.simulation_enabled?(),
      "Expected simulation to be enabled, but it's disabled"
    
    if expected_node_count do
      simulated_nodes = simulator.get_simulated_nodes()
      actual_count = length(simulated_nodes)
      
      assert actual_count == expected_node_count,
        "Expected #{expected_node_count} simulated nodes, but got #{actual_count}"
    end
  end
  
  @doc """
  Assert that simulation is disabled.
  """
  def assert_simulation_disabled do
    simulator = OTPSupervisor.Distributed.SingleNodeSimulator
    
    refute simulator.simulation_enabled?(),
      "Expected simulation to be disabled, but it's enabled"
  end
  
  @doc """
  Assert that a node failure is properly detected.
  """
  def assert_node_failure_detected(failed_node) do
    {:ok, health_data} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.execute(%{})
    
    node_status = Map.get(health_data.node_statuses, failed_node)
    
    assert node_status != nil,
      "Failed node #{failed_node} not found in health data"
    
    assert node_status.status in [:down, :unreachable],
      "Expected node #{failed_node} to be down/unreachable, but status is #{node_status.status}"
  end
  
  @doc """
  Assert that network partition is detected.
  """
  def assert_partition_detected do
    partition_status = ClusterStateManager.get_partition_status()
    
    assert partition_status in [:partial_partition, :minority_partition],
      "Expected network partition to be detected, but status is #{partition_status}"
  end
  
  @doc """
  Assert that cluster is healthy (no partitions).
  """
  def assert_no_partition do
    partition_status = ClusterStateManager.get_partition_status()
    
    assert partition_status == :healthy,
      "Expected no network partition, but status is #{partition_status}"
  end
  
  @doc """
  Assert that RPC call succeeds to a specific node.
  """
  def assert_rpc_succeeds(node, module, function, args \\ []) do
    result = :rpc.call(node, module, function, args)
    
    refute match?({:badrpc, _}, result),
      "RPC call to #{node}.#{module}.#{function} failed: #{inspect(result)}"
    
    result
  end
  
  @doc """
  Assert that RPC call fails to a specific node.
  """
  def assert_rpc_fails(node, module, function, args \\ []) do
    result = :rpc.call(node, module, function, args)
    
    assert match?({:badrpc, _}, result),
      "Expected RPC call to #{node}.#{module}.#{function} to fail, but got: #{inspect(result)}"
    
    result
  end
  
  @doc """
  Assert that a condition becomes true within timeout.
  """
  def assert_eventually(condition_fun, timeout \\ 5000, message \\ "Condition not met within timeout") do
    start_time = System.monotonic_time(:millisecond)
    assert_eventually_loop(condition_fun, start_time, timeout, message)
  end
  
  @doc """
  Assert that cluster stabilizes (no changes) for a period.
  """
  def assert_cluster_stable(duration \\ 1000) do
    initial_topology = ClusterStateManager.get_cluster_topology()
    initial_nodes = Node.list()
    
    :timer.sleep(duration)
    
    final_topology = ClusterStateManager.get_cluster_topology()
    final_nodes = Node.list()
    
    assert initial_topology.total_nodes == final_topology.total_nodes,
      "Cluster size changed during stability check: #{initial_topology.total_nodes} → #{final_topology.total_nodes}"
    
    assert initial_nodes == final_nodes,
      "Connected nodes changed during stability check: #{inspect(initial_nodes)} → #{inspect(final_nodes)}"
  end
  
  # Private helper functions
  
  defp assert_eventually_loop(condition_fun, start_time, timeout, message) do
    current_time = System.monotonic_time(:millisecond)
    
    if current_time - start_time > timeout do
      assert false, message
    else
      if condition_fun.() do
        :ok
      else
        :timer.sleep(50)
        assert_eventually_loop(condition_fun, start_time, timeout, message)
      end
    end
  end
end