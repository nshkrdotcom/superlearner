defmodule SimpleDistributedExampleTest do
  @moduledoc """
  Simple example test to verify the ergonomic distributed testing system works.

  This test uses the new magic @tag :distributed to automatically get a cluster.
  """

  use OTPSupervisor.Testing.DistributedTestCase

  @tag :distributed
  test "the cluster has at least 2 nodes" do
    # This test will only run if the ergonomic test runner
    # successfully starts a cluster for it.
    nodes = cluster_nodes()
    assert length(nodes) >= 2

    # Verify nodes are actually different
    assert Enum.uniq(nodes) == nodes

    # Verify cluster is healthy
    assert cluster_healthy?()
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
    end)
  end

  @tag :distributed
  test "cluster RPC functionality works" do
    nodes = cluster_nodes()

    # Test RPC to all nodes
    results = cluster_rpc(Node, :self, [])

    # Should get results from all nodes
    assert length(results) == length(nodes)

    # All calls should succeed
    Enum.each(results, fn {node, result} ->
      assert {:ok, ^node} = result
    end)
  end

  @tag :distributed
  test "cluster call functionality works" do
    nodes = cluster_nodes()

    # Test function call on all nodes
    results = cluster_call(fn -> Node.self() end)

    # Should get results from all nodes
    assert length(results) == length(nodes)

    # All calls should succeed and return the node name
    Enum.each(results, fn {node, result} ->
      assert {:ok, ^node} = result
    end)
  end
end
