defmodule ExampleDistributedTest do
  @moduledoc """
  Example distributed test to verify that the enforcement is working.

  This test demonstrates proper distributed testing patterns and WILL FAIL HARD
  if no cluster is available when run with --distributed flag.

  This shows how distributed tests should be written to require real clusters
  and demonstrate actual distributed functionality.

  Run with: mix test --distributed test/example_distributed_test.exs
  """

  use OTPSupervisor.Testing.DistributedTestCase

  @tag :distributed
  test "basic distributed functionality requires live cluster" do
    # This will fail hard if no cluster is available - no graceful degradation
    nodes = cluster_nodes()
    assert length(nodes) >= 2

    # Test basic distributed operations with real nodes
    assert cluster_healthy?()
    assert cluster_size() >= 2

    # Verify nodes are actually reachable and functional
    Enum.each(nodes, fn node ->
      assert Node.ping(node) == :pong

      # Verify node is running and responsive
      result = :rpc.call(node, :erlang, :node, [])
      assert result == node
    end)

    # Test that nodes can see each other
    [first_node | rest_nodes] = nodes
    connected_nodes = :rpc.call(first_node, Node, :list, [])

    # First node should see at least some of the other nodes
    assert is_list(connected_nodes)
    assert length(connected_nodes) >= 1

    # At least one of the connected nodes should be in our cluster
    assert Enum.any?(connected_nodes, fn node -> node in rest_nodes end)
  end

  @tag :distributed
  @tag cluster_size: 3
  test "multi-node functionality requires specific cluster size" do
    # This will fail hard if cluster doesn't have at least 3 nodes
    with_cluster_size(3, fn nodes ->
      assert length(nodes) == 3

      # Test multi-node operations with real RPC calls
      results = cluster_rpc(Node, :self, [])
      assert length(results) == 3

      # Verify all nodes are different and calls succeeded
      node_names = Enum.map(results, fn {node, {:ok, node_name}} ->
        assert node == node_name
        assert node in nodes
        node
      end)

      assert length(Enum.uniq(node_names)) == 3

      # Test more complex distributed operations
      [node1, node2, node3] = nodes

      # Test distributed process registry
      # Start a process on node1
      test_pid = :rpc.call(node1, :erlang, :spawn, [fn ->
        receive do
          {:test, from} -> send(from, {:ok, node1})
        after
          5000 -> :timeout
        end
      end])

      # Register it globally
      :rpc.call(node1, :global, :register_name, [:test_process, test_pid])

      # Wait for registration to propagate
      :timer.sleep(200)

      # Access from node2
      :rpc.call(node2, :global, :send, [:test_process, {:test, self()}])

      # Should receive response
      assert_receive {:ok, ^node1}, 3_000

      # Test that node3 can also see the global registration
      global_names = :rpc.call(node3, :global, :registered_names, [])
      assert :test_process in global_names
    end)
  end

  @tag :distributed
  test "distributed data sharing and synchronization" do
    nodes = cluster_nodes()
    assert length(nodes) >= 2

    [node1, node2 | _] = nodes

    # Test ETS table sharing across nodes
    # Create a table on node1
    table_name = :distributed_test_table
    table = :rpc.call(node1, :ets, :new, [table_name, [:named_table, :public]])
    assert is_reference(table) or table == table_name

    # Insert data on node1
    :rpc.call(node1, :ets, :insert, [table_name, {:key1, "value1"}])
    :rpc.call(node1, :ets, :insert, [table_name, {:key2, "value2"}])

    # Verify data exists on node1
    result1 = :rpc.call(node1, :ets, :lookup, [table_name, :key1])
    assert result1 == [{:key1, "value1"}]

    # Test distributed message passing for data synchronization
    # Start a data sync process on node2
    sync_pid = :rpc.call(node2, :erlang, :spawn, [fn ->
      receive do
        {:sync_data, data} ->
          # Create local table and insert data
          local_table = :ets.new(:local_sync_table, [:named_table, :public])
          Enum.each(data, fn item -> :ets.insert(local_table, item) end)
          send(self(), :sync_complete)
      end
    end])

    # Get all data from node1 and send to node2
    all_data = :rpc.call(node1, :ets, :tab2list, [table_name])
    :rpc.call(node2, :erlang, :send, [sync_pid, {:sync_data, all_data}])

    # Verify synchronization worked
    :timer.sleep(100)
    synced_data = :rpc.call(node2, :ets, :tab2list, [:local_sync_table])
    assert length(synced_data) == 2
    assert {:key1, "value1"} in synced_data
    assert {:key2, "value2"} in synced_data

    # Cleanup
    :rpc.call(node1, :ets, :delete, [table_name])
    :rpc.call(node2, :ets, :delete, [:local_sync_table])
  end

  @tag :distributed
  test "distributed error handling and fault tolerance" do
    nodes = cluster_nodes()
    assert length(nodes) >= 2

    # Test RPC error handling across nodes
    results = cluster_rpc(:non_existent_module, :non_existent_function, [])

    # All calls should fail gracefully with proper error information
    Enum.each(results, fn {node, result} ->
      assert {:error, reason} = result
      assert reason in [:undef, {:badrpc, :EXIT}] or
             (is_tuple(reason) and elem(reason, 0) == :undef)
      assert node in nodes
    end)

    # Test timeout handling
    timeout_results = cluster_call(fn ->
      :timer.sleep(100)  # Short delay
      :ok
    end, 50)  # Very short timeout

    # Some or all calls might timeout, but should handle gracefully
    Enum.each(timeout_results, fn {node, result} ->
      assert result in [{:ok, :ok}, {:error, :timeout}]
      assert node in nodes
    end)

    # Test successful operations after errors
    recovery_results = cluster_rpc(:erlang, :node, [])

    # All calls should succeed after the previous errors
    Enum.each(recovery_results, fn {node, result} ->
      assert {:ok, ^node} = result
    end)
  end

  @tag :distributed
  test "cluster performance and load distribution" do
    nodes = cluster_nodes()
    assert length(nodes) >= 2

    # Test parallel execution across nodes
    start_time = System.monotonic_time(:millisecond)

    # Execute CPU-intensive work on all nodes simultaneously
    work_results = cluster_call(fn ->
      # Simulate some work
      Enum.reduce(1..1000, 0, fn i, acc -> acc + i end)
    end, 10_000)

    end_time = System.monotonic_time(:millisecond)
    elapsed = end_time - start_time

    # All nodes should complete the work
    assert length(work_results) == length(nodes)

    # All results should be the same (sum of 1..1000 = 500500)
    Enum.each(work_results, fn {node, result} ->
      assert {:ok, 500500} = result
      assert node in nodes
    end)

    # Parallel execution should be reasonably fast
    assert elapsed < 5_000  # Should complete within 5 seconds

    # Test load balancing by measuring response times
    response_times = Enum.map(nodes, fn node ->
      start = System.monotonic_time(:microsecond)
      {:ok, _} = :rpc.call(node, :erlang, :node, [])
      finish = System.monotonic_time(:microsecond)
      {node, finish - start}
    end)

    # All nodes should respond reasonably quickly
    Enum.each(response_times, fn {node, time} ->
      assert time < 100_000  # Less than 100ms
      assert node in nodes
    end)
  end
end
