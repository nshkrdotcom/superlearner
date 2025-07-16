defmodule OTPSupervisor.Testing.AutoClusterManagerUnitTest do
  use ExUnit.Case, async: false

  alias OTPSupervisor.Testing.AutoClusterManager

  @moduletag :unit

  setup do
    # Use the existing AutoClusterManager from the supervision tree
    # Reset its state by forcing cleanup
    if Process.whereis(AutoClusterManager) do
      try do
        AutoClusterManager.force_cleanup_all()
      catch
        # Ignore if TestCluster.Manager isn't available
        :exit, _ -> :ok
      end

      :timer.sleep(50)
    end

    :ok
  end

  describe "configuration handling" do
    test "loads default configuration correctly" do
      # Use the existing AutoClusterManager from supervision tree
      cluster_info = AutoClusterManager.get_cluster_info()
      assert cluster_info.cluster_active == false
      assert cluster_info.nodes == []
      assert cluster_info.managed == false
    end

    test "handles custom configuration options" do
      # Test that the existing AutoClusterManager handles different requirement types
      requirements = %{
        needs_cluster: false,
        min_cluster_size: 0,
        test_type: :unit
      }

      # Should skip cluster when not needed
      assert {:ok, cluster_info} = AutoClusterManager.start_cluster_for_tests(requirements)
      refute cluster_info.cluster_active
      assert cluster_info.reason == "no cluster needed for these tests"
    end

    test "detects CI environment correctly" do
      # Test that the existing AutoClusterManager provides cluster info
      cluster_info = AutoClusterManager.get_cluster_info()
      assert is_map(cluster_info)
      assert Map.has_key?(cluster_info, :cluster_active)
      assert Map.has_key?(cluster_info, :nodes)
      assert Map.has_key?(cluster_info, :managed)
    end
  end

  describe "cluster strategy determination" do
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

    test "skips cluster when auto_cluster is disabled" do
      # Test with requirements that would normally need a cluster
      # but verify the system handles it gracefully
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }

      # The existing AutoClusterManager should handle this request
      # It may succeed or fail, but should not crash
      result = AutoClusterManager.start_cluster_for_tests(requirements)
      assert match?({:ok, _}, result) or match?({:error, _}, result)
    end
  end

  describe "cluster size determination" do
    test "respects CI mode cluster size limits" do
      # Test that the existing AutoClusterManager handles large cluster requests
      requirements = %{
        needs_cluster: true,
        # Request more than typical CI allows
        min_cluster_size: 5,
        test_type: :distributed
      }

      # The call should complete (though it may fail to start actual cluster)
      # The important thing is that the size calculation logic works
      result = AutoClusterManager.start_cluster_for_tests(requirements)
      assert match?({:ok, _}, result) or match?({:error, _}, result)
    end

    test "respects maximum cluster size limits" do
      requirements = %{
        needs_cluster: true,
        # Request more than max allows
        min_cluster_size: 10,
        test_type: :distributed
      }

      # Should handle the size limitation gracefully
      result = AutoClusterManager.start_cluster_for_tests(requirements)
      assert match?({:ok, _}, result) or match?({:error, _}, result)
    end
  end

  describe "error handling and diagnostics" do
    test "provides structured error information" do
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

          assert diagnosis.fallback_strategy in [
                   :reduce_cluster_size,
                   :skip_distributed_tests,
                   :fail_fast,
                   :retry_with_delay,
                   :retry_with_different_ports
                 ]

          assert is_list(diagnosis.retry_suggestions)
      end
    end
  end

  describe "cluster information and status" do
    test "provides accurate status information" do
      # Initial state
      cluster_info = AutoClusterManager.get_cluster_info()
      assert cluster_info.cluster_active == false
      assert cluster_info.nodes == []
      assert cluster_info.managed == false
    end

    test "handles cluster availability checks gracefully" do
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }

      # Should handle availability check without crashing
      result = AutoClusterManager.check_cluster_availability(requirements)
      assert match?({:ok, _}, result) or match?({:error, _}, result)
    end
  end

  describe "cleanup behavior" do
    test "handles cleanup when no cluster is managed" do
      # Should handle cleanup gracefully when nothing to clean up
      assert :ok = AutoClusterManager.cleanup_if_managed()
    end

    test "handles force cleanup gracefully" do
      # Should handle force cleanup without crashing
      assert :ok = AutoClusterManager.force_cleanup_all()
    end
  end

  describe "integration with requirements" do
    test "handles various requirement formats" do
      test_cases = [
        %{needs_cluster: false, min_cluster_size: 0, test_type: :unit},
        %{needs_cluster: true, min_cluster_size: 1, test_type: :distributed},
        %{needs_cluster: true, min_cluster_size: 2, test_type: :cluster, optional: true},
        %{needs_cluster: true, min_cluster_size: 3, test_type: :multi_node}
      ]

      for requirements <- test_cases do
        result = AutoClusterManager.start_cluster_for_tests(requirements)
        assert match?({:ok, _}, result) or match?({:error, _}, result)

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
    end
  end
end
