defmodule DistributedTestCaseIntegrationTest do
  @moduledoc """
  Integration test to verify that the DistributedTestCase template
  works correctly when used in a real test module.
  """
  
  use OTPSupervisor.Testing.DistributedTestCase
  
  test "basic functionality without cluster" do
    # This should work even without a cluster
    assert cluster_nodes() == []
    assert cluster_size() == 0
    refute cluster_healthy?()
    
    info = cluster_info()
    assert info.cluster_active == false
  end
  
  test "helper functions work correctly" do
    # Test RPC functions with no cluster
    results = cluster_rpc(Node, :self, [])
    assert results == []
    
    # Test cluster call with no cluster
    results = cluster_call(fn -> :ok end)
    assert results == []
  end
  
  test "wait functions timeout appropriately" do
    # These should timeout quickly since there's no cluster
    start_time = System.monotonic_time(:millisecond)
    
    result = wait_for_cluster_health(timeout: 500, interval: 100)
    
    end_time = System.monotonic_time(:millisecond)
    elapsed = end_time - start_time
    
    assert result == {:error, :timeout}
    # Should timeout around 500ms, allow some variance
    assert elapsed >= 400 and elapsed <= 800
  end
  
  test "with_cluster_size skips appropriately" do
    # This should skip since we have no cluster
    try do
      with_cluster_size(2, fn _nodes ->
        flunk("This should not execute")
      end)
    catch
      :exit, {:skip, message} ->
        assert String.contains?(message, "Insufficient cluster size")
    end
  end
end