defmodule SimpleDistributedExampleTest do
  @moduledoc """
  Simple example test to verify the ergonomic distributed testing system works.

  This test demonstrates how to use @tag :distributed to automatically get a cluster.
  All tests in this module require a live, running cluster - they will fail hard
  if no cluster is available.

  Run with: mix test --distributed test/simple_distributed_example_test.exs
  """

  use OTPSupervisor.Testing.DistributedTestCase

  @tag :distributed
  test "the cluster has at least 2 nodes and is functional" do
    # This test will only run if the ergonomic test runner
    # successfully starts a cluster for it.
    nodes = cluster_nodes()
    assert length(nodes) >= 2

    # Verify nodes are actually different
    assert Enum.uniq(nodes) == nodes

    # Verify cluster is healthy and all nodes are responsive
    assert cluster_healthy?()

    # Verify each node is actually reachable
    Enum.each(nodes, fn node ->
      assert Node.ping(node) == :pong
    end)

    # Verify nodes can see each other
    [first_node | other_nodes] = nodes
    connected_from_first = :rpc.call(first_node, Node, :list, [])

    # First node should see at least the other nodes
    assert is_list(connected_from_first)
    assert length(connected_from_first) >= length(other_nodes)
  end

  @tag :distributed
  @tag cluster_size: 3
  test "the cluster can be started with a specific size" do
    nodes = cluster_nodes()
    assert length(nodes) >= 3

    # Test with specific cluster size
    with_cluster_size(3, fn selected_nodes ->
      assert length(selected_nodes) == 3

      # Verify all nodes are reachable
      Enum.each(selected_nodes, fn node ->
        assert Node.ping(node) == :pong
      end)

      # Test inter-node communication
      [node1, node2, node3] = selected_nodes

      # Start a process on node1 that waits for messages
      test_pid = :rpc.call(node1, :erlang, :spawn, [fn ->
        receive do
          {:test_message, from} -> send(from, {:response, node1})
        after
          5000 -> :timeout
        end
      end])

      # Send message from node2
      :rpc.call(node2, :erlang, :send, [test_pid, {:test_message, self()}])

      # Should receive response
      assert_receive {:response, ^node1}, 3_000

      # Verify node3 can also communicate
      node3_result = :rpc.call(node3, :erlang, :node, [])
      assert node3_result == node3
    end)
  end

  @tag :distributed
  test "cluster RPC functionality works with live nodes" do
    nodes = cluster_nodes()

    # Test RPC to all nodes - get their actual node names
    results = cluster_rpc(Node, :self, [])

    # Should get results from all nodes
    assert length(results) == length(nodes)

    # All calls should succeed and return the correct node name
    Enum.each(results, fn {node, result} ->
      assert {:ok, ^node} = result
      assert node in nodes
    end)

    # Test RPC with a more complex operation
    process_count_results = cluster_rpc(:erlang, :system_info, [:process_count])

    # All nodes should return process counts
    Enum.each(process_count_results, fn {node, result} ->
      assert {:ok, count} = result
      assert is_integer(count)
      assert count > 0  # Should have some processes running
      assert node in nodes
    end)
  end

  @tag :distributed
  test "cluster call functionality works with live nodes" do
    nodes = cluster_nodes()

    # Test function call on all nodes
    results = cluster_call(fn -> Node.self() end)

    # Should get results from all nodes
    assert length(results) == length(nodes)

    # All calls should succeed and return the node name
    Enum.each(results, fn {node, result} ->
      assert {:ok, ^node} = result
      assert node in nodes
    end)

    # Test a more complex function
    memory_results = cluster_call(fn ->
      :erlang.memory(:total)
    end)

    # All nodes should return memory information
    Enum.each(memory_results, fn {node, result} ->
      assert {:ok, memory} = result
      assert is_integer(memory)
      assert memory > 0  # Should have some memory usage
      assert node in nodes
    end)
  end

  @tag :distributed
  test "distributed process spawning and communication" do
    nodes = cluster_nodes()
    assert length(nodes) >= 2

    [node1, node2 | _] = nodes

    # Spawn a process on node1 that acts as a simple echo server
    echo_pid = :rpc.call(node1, :erlang, :spawn, [fn ->
      echo_loop()
    end])

    assert is_pid(echo_pid)

    # Register the process globally so node2 can find it
    :rpc.call(node1, :global, :register_name, [:echo_server, echo_pid])

    # Wait for global registration to propagate
    :timer.sleep(100)

    # From node2, send a message to the echo server on node1
    :rpc.call(node2, :global, :send, [:echo_server, {:echo, "hello", self()}])

    # Should receive the echo back
    assert_receive {:echo_response, "hello"}, 3_000
  end

  @tag :distributed
  test "cluster health monitoring and recovery" do
    # Verify cluster starts healthy
    assert cluster_healthy?()

    # Test waiting for cluster health (should succeed immediately)
    start_time = System.monotonic_time(:millisecond)
    assert wait_for_cluster_health(timeout: 5_000) == :ok
    end_time = System.monotonic_time(:millisecond)

    # Should succeed quickly since cluster is already healthy
    elapsed = end_time - start_time
    assert elapsed < 2_000

    # Test cluster condition waiting
    assert wait_for_cluster_condition(fn node ->
      case :rpc.call(node, :erlang, :is_alive, []) do
        true -> true
        _ -> false
      end
    end, timeout: 5_000) == :ok
  end

  # Helper function for the echo server
  defp echo_loop do
    receive do
      {:echo, message, from} ->
        send(from, {:echo_response, message})
        echo_loop()
    after
      10_000 -> :timeout
    end
  end
end
