defmodule OTPSupervisor.Testing.DistributedTestCaseTest do
  use ExUnit.Case, async: false
  
  # Import the module we're testing
  import OTPSupervisor.Testing.DistributedTestCase
  
  describe "helper functions" do
    test "cluster_nodes/0 returns empty list when no cluster" do
      # Mock the AutoClusterManager to return no cluster
      assert cluster_nodes() == []
    end
    
    test "cluster_size/0 returns 0 when no cluster" do
      assert cluster_size() == 0
    end
    
    test "cluster_healthy?/0 returns false when no cluster" do
      refute cluster_healthy?()
    end
    
    test "with_cluster_size/2 skips test when insufficient nodes" do
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
    
    test "cluster_info/0 returns inactive cluster info" do
      info = cluster_info()
      assert info.cluster_active == false
    end
    
    test "cluster_rpc/4 returns empty list when no cluster" do
      results = cluster_rpc(Node, :self, [])
      assert results == []
    end
    
    test "cluster_call/2 returns empty list when no cluster" do
      results = cluster_call(fn -> :ok end)
      assert results == []
    end
  end
  
  describe "wait functions" do
    test "wait_for_cluster_health/1 times out quickly when no cluster" do
      start_time = System.monotonic_time(:millisecond)
      
      result = wait_for_cluster_health(timeout: 1000, interval: 100)
      
      end_time = System.monotonic_time(:millisecond)
      elapsed = end_time - start_time
      
      assert result == {:error, :timeout}
      # Should timeout around 1000ms, allow some variance
      assert elapsed >= 900 and elapsed <= 1500
    end
    
    test "wait_for_cluster_size/2 times out quickly when no cluster" do
      start_time = System.monotonic_time(:millisecond)
      
      result = wait_for_cluster_size(2, timeout: 1000, interval: 100)
      
      end_time = System.monotonic_time(:millisecond)
      elapsed = end_time - start_time
      
      assert result == {:error, :timeout}
      assert elapsed >= 900 and elapsed <= 1500
    end
    
    test "wait_for_cluster_condition/2 times out when condition never met" do
      result = wait_for_cluster_condition(
        fn _node -> false end,
        timeout: 1000,
        interval: 100
      )
      
      assert result == {:error, :timeout}
    end
  end
end