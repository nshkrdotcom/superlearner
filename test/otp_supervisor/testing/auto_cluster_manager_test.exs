defmodule OTPSupervisor.Testing.AutoClusterManagerTest do
  use ExUnit.Case, async: false
  
  alias OTPSupervisor.Testing.AutoClusterManager
  alias OTPSupervisor.TestCluster.Manager
  
  @moduletag :integration
  
  setup do
    # Ensure clean state before each test
    AutoClusterManager.force_cleanup_all()
    :timer.sleep(100)
    
    :ok
  end
  
  describe "cluster lifecycle management" do
    test "starts cluster for valid requirements" do
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      assert {:ok, cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      assert cluster_info.cluster_active
      assert length(cluster_info.nodes) >= 2
      assert cluster_info.managed_by == :auto_cluster_manager
      
      # Cleanup
      AutoClusterManager.cleanup_if_managed()
    end
    
    test "skips cluster when not needed" do
      requirements = %{
        needs_cluster: false,
        min_cluster_size: 0,
        test_type: :unit
      }
      
      assert {:ok, cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      refute cluster_info.cluster_active
      assert cluster_info.nodes == []
      assert cluster_info.reason == "no cluster needed for these tests"
    end
    
    test "reuses existing cluster when suitable" do
      # First, start a cluster manually
      {:ok, _nodes} = Manager.start_cluster(node_count: 3)
      
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      assert {:ok, cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      assert cluster_info.cluster_active
      assert length(cluster_info.nodes) >= 2
      # Should not be managed by us since we're reusing
      refute cluster_info.managed_by == :auto_cluster_manager
      
      # Cleanup should not stop the cluster since we didn't start it
      AutoClusterManager.cleanup_if_managed()
      
      # Verify cluster is still running
      {:ok, status} = Manager.get_status()
      assert status.overall == :running
      
      # Manual cleanup
      Manager.stop_cluster()
    end
    
    test "handles cluster startup failure gracefully" do
      # Create requirements that might fail (very large cluster)
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 10,  # Unreasonably large
        test_type: :distributed
      }
      
      case AutoClusterManager.start_cluster_for_tests(requirements) do
        {:ok, _cluster_info} ->
          # If it somehow succeeds, clean up
          AutoClusterManager.cleanup_if_managed()
          
        {:error, diagnosis} ->
          # Should provide helpful diagnosis
          assert is_binary(diagnosis.problem)
          assert is_list(diagnosis.solutions)
          assert diagnosis.fallback_strategy in [:reduce_cluster_size, :skip_distributed_tests, :fail_fast]
      end
    end
  end
  
  describe "cluster reuse logic" do
    test "checks cluster availability correctly" do
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      # No cluster running
      assert {:error, _} = AutoClusterManager.check_cluster_availability(requirements)
      
      # Start a cluster
      {:ok, _nodes} = Manager.start_cluster(node_count: 2)
      
      # Should now be available
      assert {:ok, :suitable} = AutoClusterManager.check_cluster_availability(requirements)
      
      # Cleanup
      Manager.stop_cluster()
    end
    
    test "detects insufficient cluster size" do
      # Start small cluster
      {:ok, _nodes} = Manager.start_cluster(node_count: 1)
      
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 3,  # Larger than available
        test_type: :distributed
      }
      
      assert {:error, :insufficient_size} = AutoClusterManager.check_cluster_availability(requirements)
      
      # Cleanup
      Manager.stop_cluster()
    end
  end
  
  describe "configuration handling" do
    test "respects CI mode configuration" do
      # Simulate CI environment
      original_env = System.get_env("CI")
      System.put_env("CI", "true")
      
      try do
        # Start with CI-specific config
        {:ok, _pid} = AutoClusterManager.start_link(ci_mode: true, ci_cluster_size: 2)
        
        requirements = %{
          needs_cluster: true,
          min_cluster_size: 5,  # Request large cluster
          test_type: :distributed
        }
        
        case AutoClusterManager.start_cluster_for_tests(requirements) do
          {:ok, cluster_info} ->
            # Should be limited to CI cluster size
            assert length(cluster_info.nodes) <= 2
            AutoClusterManager.cleanup_if_managed()
            
          {:error, _} ->
            # Acceptable in CI with resource constraints
            :ok
        end
      after
        if original_env do
          System.put_env("CI", original_env)
        else
          System.delete_env("CI")
        end
      end
    end
    
    test "handles disabled auto cluster configuration" do
      {:ok, _pid} = AutoClusterManager.start_link(auto_cluster: false)
      
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      assert {:ok, cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      refute cluster_info.cluster_active
      assert cluster_info.reason == "automatic cluster management disabled"
    end
  end
  
  describe "error handling and diagnostics" do
    test "provides detailed error diagnosis" do
      # Force an error by trying to start when ports might be in use
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      # Start a cluster first to potentially cause port conflicts
      {:ok, _nodes} = Manager.start_cluster(node_count: 2)
      
      case AutoClusterManager.start_cluster_for_tests(requirements) do
        {:ok, cluster_info} ->
          # If reuse works, that's fine too
          assert cluster_info.cluster_active
          AutoClusterManager.cleanup_if_managed()
          
        {:error, diagnosis} ->
          # Should provide helpful information
          assert is_map(diagnosis)
          assert Map.has_key?(diagnosis, :problem)
          assert Map.has_key?(diagnosis, :solutions)
          assert Map.has_key?(diagnosis, :fallback_strategy)
      end
      
      # Cleanup
      Manager.stop_cluster()
    end
  end
  
  describe "cluster information and status" do
    test "provides accurate cluster information" do
      # No cluster initially
      cluster_info = AutoClusterManager.get_cluster_info()
      refute cluster_info.cluster_active
      assert cluster_info.nodes == []
      refute cluster_info.managed
      
      # Start a cluster
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      {:ok, _cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      
      # Check updated information
      updated_info = AutoClusterManager.get_cluster_info()
      assert updated_info.cluster_active
      assert length(updated_info.nodes) >= 2
      assert updated_info.managed
      assert is_integer(updated_info.uptime)
      
      # Cleanup
      AutoClusterManager.cleanup_if_managed()
    end
  end
  
  describe "cleanup behavior" do
    test "cleans up only managed clusters" do
      # Start cluster manually
      {:ok, _nodes} = Manager.start_cluster(node_count: 2)
      
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      # Should reuse existing cluster
      {:ok, cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      refute cluster_info.managed_by == :auto_cluster_manager
      
      # Cleanup should not affect the cluster
      AutoClusterManager.cleanup_if_managed()
      
      # Verify cluster still running
      {:ok, status} = Manager.get_status()
      assert status.overall == :running
      
      # Manual cleanup
      Manager.stop_cluster()
    end
    
    test "force cleanup stops all clusters" do
      # Start a managed cluster
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }
      
      {:ok, _cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      
      # Force cleanup should stop everything
      AutoClusterManager.force_cleanup_all()
      
      # Verify no cluster running
      cluster_info = AutoClusterManager.get_cluster_info()
      refute cluster_info.cluster_active
    end
  end
end