defmodule DistributedTestCaseIntegrationTest do
  @moduledoc """
  Integration test to verify that the DistributedTestCase template
  works correctly when used in a real test module with live clusters.

  This test demonstrates the proper usage of DistributedTestCase
  with actual cluster infrastructure.
  """

  use OTPSupervisor.Testing.DistributedTestCase

  # These tests require a live cluster - they will fail hard if no cluster is available
  @tag :distributed
  test "basic functionality with live cluster" do
    # These should work with a live cluster
    nodes = cluster_nodes()
    assert length(nodes) >= 2

    size = cluster_size()
    assert size >= 2
    assert size == length(nodes)

    # Cluster should be healthy
    assert cluster_healthy?()

    info = cluster_info()
    assert info.cluster_active == true
    assert length(info.nodes) >= 2
  end

  @tag :distributed
  test "helper functions work correctly with live cluster" do
    # Test RPC functions with live cluster
    results = cluster_rpc(Node, :self, [])
    nodes = cluster_nodes()

    assert length(results) == length(nodes)

    # All calls should succeed and return the node name
    Enum.each(results, fn {node, result} ->
      assert {:ok, ^node} = result
      assert node in nodes
    end)

    # Test cluster call with live cluster
    results = cluster_call(fn -> :erlang.node() end)
    assert length(results) == length(nodes)

    # All calls should succeed
    Enum.each(results, fn {node, result} ->
      assert {:ok, ^node} = result
      assert node in nodes
    end)
  end

  @tag :distributed
  test "wait functions work with live cluster" do
    # These should succeed quickly since there's a live cluster
    start_time = System.monotonic_time(:millisecond)

    result = wait_for_cluster_health(timeout: 10_000, interval: 100)

    end_time = System.monotonic_time(:millisecond)
    elapsed = end_time - start_time

    assert result == :ok
    # Should succeed quickly, much faster than timeout
    assert elapsed < 5_000
  end

  @tag :distributed
  test "with_cluster_size works with sufficient nodes" do
    _current_size = cluster_size()

    # This should work since we have at least 2 nodes
    result =
      with_cluster_size(2, fn nodes ->
        assert length(nodes) == 2
        assert Enum.all?(nodes, &is_atom/1)
        :test_passed
      end)

    assert result == :test_passed
  end

  @tag :distributed
  test "with_cluster_size skips when requesting too many nodes" do
    current_size = cluster_size()

    # Request more nodes than available
    try do
      with_cluster_size(current_size + 10, fn _nodes ->
        flunk("This should not execute")
      end)
    catch
      :exit, {:skip, message} ->
        assert String.contains?(message, "Insufficient cluster size")
        assert String.contains?(message, "need #{current_size + 10}")
        assert String.contains?(message, "have #{current_size}")
    end
  end

  @tag :distributed
  @tag cluster_size: 3
  test "cluster_size tag is respected when possible" do
    # This test will only run if we can get a cluster with at least 3 nodes
    with_cluster_size(3, fn nodes ->
      assert length(nodes) == 3

      # Test inter-node communication
      [node1, node2, node3] = nodes

      # Verify all nodes can see each other
      Enum.each([node1, node2, node3], fn node ->
        connected = :rpc.call(node, Node, :list, [])
        assert is_list(connected)
        # Each node should see at least the other 2
        assert length(connected) >= 2
      end)
    end)
  end

  @tag :distributed
  test "cluster condition waiting works" do
    # Test waiting for a condition across all nodes
    result =
      wait_for_cluster_condition(
        fn node ->
          case :rpc.call(node, :erlang, :is_alive, []) do
            true -> true
            _ -> false
          end
        end,
        timeout: 10_000
      )

    assert result == :ok
  end

  @tag :distributed
  test "distributed process communication" do
    nodes = cluster_nodes()
    assert length(nodes) >= 2

    [node1, node2 | _] = nodes

    # Start a simple process on node1
    pid1 =
      :rpc.call(node1, :erlang, :spawn, [
        fn ->
          receive do
            {:ping, from} -> send(from, :pong)
          after
            5000 -> :timeout
          end
        end
      ])

    assert is_pid(pid1)

    # Send message from node2
    :rpc.call(node2, :erlang, :send, [pid1, {:ping, self()}])

    # Should receive response
    assert_receive :pong, 5_000
  end
end
