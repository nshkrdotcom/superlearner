defmodule ExampleDistributedTest do
  @moduledoc """
  Example distributed test to verify that the enforcement is working.
  
  This test should FAIL HARD if no cluster is available when run with --distributed flag.
  """
  
  use OTPSupervisor.Testing.DistributedTestCase
  
  @tag :distributed
  test "basic distributed functionality requires cluster" do
    # This should fail hard if no cluster is available
    nodes = cluster_nodes()
    assert length(nodes) >= 2
    
    # Test basic distributed operations
    assert cluster_healthy?()
    assert cluster_size() >= 2
  end
  
  @tag :distributed
  @tag cluster_size: 3
  test "multi-node functionality requires specific cluster size" do
    # This should fail hard if cluster doesn't have at least 3 nodes
    with_cluster_size(3, fn nodes ->
      assert length(nodes) == 3
      
      # Test multi-node operations
      results = cluster_rpc(Node, :self, [])
      assert length(results) == 3
      
      # Verify all nodes are different
      node_names = Enum.map(results, fn {node, {:ok, node_name}} -> 
        assert node == node_name
        node
      end)
      
      assert length(Enum.uniq(node_names)) == 3
    end)
  end
end