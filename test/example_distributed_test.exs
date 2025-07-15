defmodule ExampleDistributedTest do
  @moduledoc """
  Example distributed test file for testing the TestAnalyzer.
  """
  use ExUnit.Case
  
  @tag :distributed
  test "basic distributed functionality" do
    # This test needs a cluster
    assert true
  end
  
  @tag :cluster
  @tag cluster_size: 3
  test "three node cluster test" do
    # This test needs exactly 3 nodes
    assert true
  end
  
  @tag :multi_node
  @tag cluster_size: 5
  test "large cluster test" do
    # This test needs 5 nodes
    # In a real cluster environment, we would have 4 other nodes
    # For now, just verify the test structure works
    nodes = Node.list()
    # This assertion would pass in a real 5-node cluster
    # assert length(nodes) >= 4  # 5 nodes total, 4 others
    
    # For testing purposes, just verify we can call Node.list()
    assert is_list(nodes)
  end
  
  test "regular test" do
    # This test doesn't need a cluster
    assert 1 + 1 == 2
  end
  
  setup do
    # Uses cluster helper
    {:ok, nodes} = ClusterTestHelper.start_test_cluster(2)
    on_exit(fn -> ClusterTestHelper.stop_test_cluster(nodes) end)
    {:ok, nodes: nodes}
  end
end