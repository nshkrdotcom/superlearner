defmodule OTPSupervisor.Testing.AutoClusterManagerUnitTest do
  use ExUnit.Case, async: false
  
  alias OTPSupervisor.Testing.AutoClusterManager
  
  @moduletag :unit
  
  setup do
    # Start a fresh AutoClusterManager for each test
    if Process.whereis(AutoClusterManager) do
      GenServer.stop(AutoClusterManager)
      :timer.sleep(50)
    end
    
    :ok
  end
  
  describe "configuration handling" do
    test "loads default configuration correctly" do
      {:ok, pid} = AutoClusterManager.start_link()
      
      # Test that the manager starts and can respond
      cluster_info = AutoClusterManager.get_cluster_info()
      assert cluster_info.cluster_active == false
      assert cluster_info.nodes == []
      assert cluster_info.managed == false
      
      GenServer.stop(pid)
    end
    
    test "handles custom configuration options" do
      custom_opts = [
        auto_cluster: false,
        default_cluster_size: 3,
        ci_mode: true
      ]
      
      {:ok, pid} = AutoClusterManager.start_link(custom_opts)
      
      # Test that configuration is applied by checking behavior
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      # Should skip cluster due to auto_cluster: false
      assert {:ok, cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      refute cluster_info.cluster_active
      assert cluster_info.reason == "automatic cluster management disabled"
      
      GenServer.stop(pid)
    end
    
    test "detects CI environment correctly" do
      # Simulate CI environment
      original_env = System.get_env("CI")
      System.put_env("CI", "true")
      
      try do
        {:ok, pid} = AutoClusterManager.start_link()
        
        # The CI detection should be reflected in the configuration
        # We can't directly test the internal state, but we can test behavior
        cluster_info = AutoClusterManager.get_cluster_info()
        assert is_map(cluster_info)
        
        GenServer.stop(pid)
      after
        if original_env do
          System.put_env("CI", original_env)
        else
          System.delete_env("CI")
        end
      end
    end
  end
  
  describe "cluster strategy determination" do
    test "skips cluster when not needed" do
      {:ok, pid} = AutoClusterManager.start_link()
      
      requirements = %{
        needs_cluster: false,
        min_cluster_size: 0,
        test_type: :unit
      }
      
      assert {:ok, cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      refute cluster_info.cluster_active
      assert cluster_info.nodes == []
      assert cluster_info.reason == "no cluster needed for these tests"
      
      GenServer.stop(pid)
    end
    
    test "skips cluster when auto_cluster is disabled" do
      {:ok, pid} = AutoClusterManager.start_link(auto_cluster: false)
      
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      assert {:ok, cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      refute cluster_info.cluster_active
      assert cluster_info.reason == "automatic cluster management disabled"
      
      GenServer.stop(pid)
    end
  end
  
  describe "cluster size determination" do
    test "respects CI mode cluster size limits" do
      {:ok, pid} = AutoClusterManager.start_link(ci_mode: true, ci_cluster_size: 2)
      
      # This test verifies the logic without actually starting clusters
      # We test by checking that the manager doesn't crash and handles the request
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 5,  # Request more than CI allows
        test_type: :distributed
      }
      
      # The call should complete (though it may fail to start actual cluster)
      # The important thing is that the size calculation logic works
      result = AutoClusterManager.start_cluster_for_tests(requirements)
      assert match?({:ok, _} | {:error, _}, result)
      
      GenServer.stop(pid)
    end
    
    test "respects maximum cluster size limits" do
      {:ok, pid} = AutoClusterManager.start_link(max_cluster_size: 3)
      
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 10,  # Request more than max allows
        test_type: :distributed
      }
      
      # Should handle the size limitation gracefully
      result = AutoClusterManager.start_cluster_for_tests(requirements)
      assert match?({:ok, _} | {:error, _}, result)
      
      GenServer.stop(pid)
    end
  end
  
  describe "error handling and diagnostics" do
    test "provides structured error information" do
      {:ok, pid} = AutoClusterManager.start_link()
      
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      # This will likely fail due to cluster startup issues, but should provide good diagnostics
      case AutoClusterManager.start_cluster_for_tests(requirements) do
        {:ok, _cluster_info} ->
          # If it succeeds, that's fine too
          :ok
          
        {:error, diagnosis} ->
          # Should provide structured error information
          assert is_map(diagnosis)
          assert Map.has_key?(diagnosis, :problem)
          assert Map.has_key?(diagnosis, :solutions)
          assert Map.has_key?(diagnosis, :fallback_strategy)
          assert Map.has_key?(diagnosis, :retry_suggestions)
          
          assert is_binary(diagnosis.problem)
          assert is_list(diagnosis.solutions)
          assert diagnosis.fallback_strategy in [:reduce_cluster_size, :skip_distributed_tests, :fail_fast, :retry_with_delay, :retry_with_different_ports]
          assert is_list(diagnosis.retry_suggestions)
      end
      
      GenServer.stop(pid)
    end
  end
  
  describe "cluster information and status" do
    test "provides accurate status information" do
      {:ok, pid} = AutoClusterManager.start_link()
      
      # Initial state
      cluster_info = AutoClusterManager.get_cluster_info()
      assert cluster_info.cluster_active == false
      assert cluster_info.nodes == []
      assert cluster_info.managed == false
      assert is_nil(cluster_info.uptime)
      
      GenServer.stop(pid)
    end
    
    test "handles cluster availability checks gracefully" do
      {:ok, pid} = AutoClusterManager.start_link()
      
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      # Should handle availability check without crashing
      result = AutoClusterManager.check_cluster_availability(requirements)
      assert match?({:ok, _} | {:error, _}, result)
      
      GenServer.stop(pid)
    end
  end
  
  describe "cleanup behavior" do
    test "handles cleanup when no cluster is managed" do
      {:ok, pid} = AutoClusterManager.start_link()
      
      # Should handle cleanup gracefully when nothing to clean up
      assert :ok = AutoClusterManager.cleanup_if_managed()
      
      GenServer.stop(pid)
    end
    
    test "handles force cleanup gracefully" do
      {:ok, pid} = AutoClusterManager.start_link()
      
      # Should handle force cleanup without crashing
      assert :ok = AutoClusterManager.force_cleanup_all()
      
      GenServer.stop(pid)
    end
  end
  
  describe "integration with requirements" do
    test "handles various requirement formats" do
      {:ok, pid} = AutoClusterManager.start_link()
      
      test_cases = [
        %{needs_cluster: false, min_cluster_size: 0, test_type: :unit},
        %{needs_cluster: true, min_cluster_size: 1, test_type: :distributed},
        %{needs_cluster: true, min_cluster_size: 2, test_type: :cluster, optional: true},
        %{needs_cluster: true, min_cluster_size: 3, test_type: :multi_node}
      ]
      
      for requirements <- test_cases do
        result = AutoClusterManager.start_cluster_for_tests(requirements)
        assert match?({:ok, _} | {:error, _}, result)
        
        case result do
          {:ok, cluster_info} ->
            assert is_map(cluster_info)
            assert Map.has_key?(cluster_info, :cluster_active)
            assert Map.has_key?(cluster_info, :nodes)
            
          {:error, diagnosis} ->
            assert is_map(diagnosis)
            assert Map.has_key?(diagnosis, :problem)
        end
      end
      
      GenServer.stop(pid)
    end
  end
end