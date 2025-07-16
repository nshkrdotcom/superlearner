defmodule OTPSupervisor.Testing.AutoClusterManagerSimpleTest do
  use ExUnit.Case, async: false

  alias OTPSupervisor.Testing.AutoClusterManager

  @moduletag :cluster_management

  describe "basic functionality without cluster dependencies" do
    setup do
      # Ensure clean state - force cleanup any existing clusters
      AutoClusterManager.force_cleanup_all()
      :timer.sleep(100)
      :ok
    end

    test "provides cluster info when no cluster is active" do
      # Test the basic get_cluster_info functionality
      cluster_info = AutoClusterManager.get_cluster_info()

      # Should return a map with expected keys
      assert is_map(cluster_info)
      assert Map.has_key?(cluster_info, :cluster_active)
      assert Map.has_key?(cluster_info, :nodes)
      assert Map.has_key?(cluster_info, :managed)

      # After cleanup, no cluster should be active
      assert cluster_info.cluster_active == false
      assert cluster_info.nodes == []
      assert cluster_info.managed == false
    end

    test "handles requirements that don't need clusters" do
      requirements = %{
        needs_cluster: false,
        min_cluster_size: 0,
        test_type: :unit
      }

      # Should skip cluster startup
      assert {:ok, cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      refute cluster_info.cluster_active
      assert cluster_info.nodes == []
      assert cluster_info.reason == "no cluster needed for these tests"
    end

    test "handles cleanup when no cluster is managed" do
      # Should handle cleanup gracefully when nothing to clean up
      assert :ok = AutoClusterManager.cleanup_if_managed()
    end
  end

  describe "configuration-based behavior" do
    test "skips cluster when auto_cluster would be disabled" do
      # We can't easily test with different configs since the manager is already started
      # But we can test the logic by checking that the manager responds appropriately
      # to requirements that would trigger different behaviors

      requirements = %{
        needs_cluster: false,
        min_cluster_size: 0,
        test_type: :unit
      }

      result = AutoClusterManager.start_cluster_for_tests(requirements)
      assert match?({:ok, _}, result)

      {:ok, cluster_info} = result
      refute cluster_info.cluster_active
    end
  end

  describe "error handling without dependencies" do
    test "handles requests gracefully even when dependencies unavailable" do
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }

      # This may fail due to TestCluster.Manager not being available,
      # but it should fail gracefully with structured error information
      result = AutoClusterManager.start_cluster_for_tests(requirements)

      case result do
        {:ok, cluster_info} ->
          # If it somehow succeeds, that's fine
          assert is_map(cluster_info)

        {:error, diagnosis} ->
          # Should provide structured error information
          assert is_map(diagnosis)
          # The exact keys may vary based on the error, but it should be structured
          assert map_size(diagnosis) > 0
      end
    end

    test "availability checks handle missing dependencies" do
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }

      # Should handle availability check without crashing
      result = AutoClusterManager.check_cluster_availability(requirements)

      # Should return either success or a structured error
      assert match?({:ok, _}, result) or match?({:error, _}, result)
    end
  end

  describe "process lifecycle" do
    test "manager process is alive and responsive" do
      # Verify the AutoClusterManager process is running
      pid = Process.whereis(AutoClusterManager)
      assert is_pid(pid)
      assert Process.alive?(pid)

      # Verify it responds to basic calls
      cluster_info = AutoClusterManager.get_cluster_info()
      assert is_map(cluster_info)
    end
  end
end
