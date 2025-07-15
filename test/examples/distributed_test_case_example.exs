defmodule DistributedTestCaseExample do
  @moduledoc """
  Example demonstrating how to use OTPSupervisor.Testing.DistributedTestCase
  for writing distributed tests.
  
  This file shows various patterns and best practices for distributed testing.
  """
  
  use OTPSupervisor.Testing.DistributedTestCase
  
  # Basic distributed test
  @tag :distributed
  test "basic cluster functionality" do
    nodes = cluster_nodes()
    
    # Should have at least 2 nodes in a basic cluster
    assert length(nodes) >= 2
    
    # Cluster should be healthy
    assert cluster_healthy?()
    
    # Each node should be able to see the others
    Enum.each(nodes, fn node ->
      connected_nodes = :rpc.call(node, Node, :list, [])
      assert is_list(connected_nodes)
      assert length(connected_nodes) >= 1
    end)
  end
  
  # Test requiring specific cluster size
  @tag :distributed
  @tag cluster_size: 3
  test "three node consensus algorithm" do
    with_cluster_size(3, fn nodes ->
      assert length(nodes) == 3
      
      # Test a simple consensus scenario
      # Each node votes and we collect results
      votes = 
        Enum.map(nodes, fn node ->
          :rpc.call(node, :rand, :uniform, [2])
        end)
      
      assert length(votes) == 3
      assert Enum.all?(votes, &(&1 in [1, 2]))
    end)
  end
  
  # Test with custom timeout
  @tag :distributed
  @tag cluster_timeout: 60_000
  test "long running distributed operation" do
    nodes = cluster_nodes()
    assert length(nodes) >= 2
    
    # Simulate a long-running distributed operation
    results = cluster_call(fn ->
      :timer.sleep(100)  # Simulate work
      Node.self()
    end, 10_000)  # 10 second timeout
    
    assert length(results) == length(nodes)
    
    # Each node should return its own name
    Enum.each(results, fn {node, {:ok, returned_node}} ->
      assert node == returned_node
    end)
  end
  
  # Test cluster health monitoring
  @tag :distributed
  test "cluster health monitoring" do
    # Wait for cluster to be fully healthy
    assert wait_for_cluster_health(timeout: 10_000) == :ok
    
    # Get detailed cluster information
    info = cluster_info()
    assert info.cluster_active == true
    assert length(info.nodes) >= 2
    
    # Test cluster condition waiting
    assert wait_for_cluster_condition(fn node ->
      case :rpc.call(node, :erlang, :is_alive, []) do
        true -> true
        _ -> false
      end
    end, timeout: 5_000) == :ok
  end
  
  # Test distributed process communication
  @tag :distributed
  test "distributed process communication" do
    nodes = cluster_nodes()
    assert length(nodes) >= 2
    
    [node1, node2 | _] = nodes
    
    # Start a simple process on node1
    pid1 = :rpc.call(node1, :erlang, :spawn, [fn ->
      receive do
        {:ping, from} -> send(from, :pong)
      end
    end])
    
    # Send message from node2
    :rpc.call(node2, :erlang, :send, [pid1, {:ping, self()}])
    
    # Should receive response
    assert_receive :pong, 5_000
  end
  
  # Test error handling in distributed environment
  @tag :distributed
  test "distributed error handling" do
    nodes = cluster_nodes()
    
    # Test RPC error handling
    results = cluster_rpc(:non_existent_module, :non_existent_function, [])
    
    # All calls should fail gracefully
    Enum.each(results, fn {_node, result} ->
      assert match?({:error, _}, result)
    end)
  end
  
  # Performance test
  @tag :distributed
  @tag :performance
  test "cluster performance characteristics" do
    nodes = cluster_nodes()
    assert length(nodes) >= 2
    
    # Measure inter-node latency
    start_time = System.monotonic_time(:microsecond)
    
    results = cluster_call(fn ->
      System.monotonic_time(:microsecond)
    end)
    
    end_time = System.monotonic_time(:microsecond)
    total_time = end_time - start_time
    
    # Basic performance assertions
    assert total_time < 1_000_000  # Should complete within 1 second
    assert length(results) == length(nodes)
    
    # All calls should succeed
    Enum.each(results, fn {_node, result} ->
      assert match?({:ok, _}, result)
    end)
  end
end