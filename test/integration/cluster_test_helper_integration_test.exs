defmodule ClusterTestHelperIntegrationTest do
  @moduledoc """
  Integration tests for enhanced ClusterTestHelper with AutoClusterManager integration.

  Tests the coordination between automatic and manual cluster management,
  backward compatibility, and enhanced error reporting.
  """

  use ExUnit.Case, async: false

  alias OTPSupervisor.Testing.AutoClusterManager

  require Logger

  setup_all do
    # Ensure AutoClusterManager is running for integration tests
    case Process.whereis(AutoClusterManager) do
      nil ->
        {:ok, _pid} = AutoClusterManager.start_link()

      _pid ->
        :ok
    end

    :ok
  end

  setup do
    # Clean up any existing clusters before each test
    ClusterTestHelper.coordinate_cleanup()

    # Give cleanup time to complete
    :timer.sleep(500)

    :ok
  end

  describe "automatic cluster integration" do
    test "get_cluster_nodes/0 returns empty list when no cluster is active" do
      nodes = ClusterTestHelper.get_cluster_nodes()
      assert nodes == []
    end

    test "get_cluster_nodes/0 returns nodes when auto cluster is active" do
      # Start a cluster through AutoClusterManager
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :integration_test
      }

      case AutoClusterManager.start_cluster_for_tests(requirements) do
        {:ok, %{cluster_active: true, nodes: expected_nodes}} ->
          # Now ClusterTestHelper should see these nodes
          actual_nodes = ClusterTestHelper.get_cluster_nodes()
          assert length(actual_nodes) >= 2
          assert Enum.all?(expected_nodes, &(&1 in actual_nodes))

        {:error, reason} ->
          # Skip test if cluster startup fails (e.g., in CI environments)
          Logger.warning("Skipping test due to cluster startup failure: #{inspect(reason)}")
          ExUnit.skip("Cluster startup failed: #{inspect(reason)}")
      end
    end

    test "check_auto_cluster_status/1 reports correct cluster status" do
      # Initially no cluster
      assert {:error, _} = ClusterTestHelper.check_auto_cluster_status(2)

      # Start cluster and check status
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :status_test
      }

      case AutoClusterManager.start_cluster_for_tests(requirements) do
        {:ok, %{cluster_active: true}} ->
          # Should now report cluster as available
          assert {:ok, cluster_info} = ClusterTestHelper.check_auto_cluster_status(2)
          assert cluster_info.cluster_active == true
          assert length(cluster_info.nodes) >= 2

        {:error, reason} ->
          Logger.warning("Skipping test due to cluster startup failure: #{inspect(reason)}")
          ExUnit.skip("Cluster startup failed: #{inspect(reason)}")
      end
    end
  end

  describe "cluster coordination" do
    test "ensure_cluster/1 uses auto cluster when suitable" do
      # Start auto cluster first
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :coordination_test
      }

      case AutoClusterManager.start_cluster_for_tests(requirements) do
        {:ok, %{cluster_active: true, nodes: auto_nodes}} ->
          # ensure_cluster should reuse the auto cluster
          case ClusterTestHelper.ensure_cluster(size: 2) do
            {:ok, ensured_nodes} ->
              # Should get the same nodes from auto cluster
              assert length(ensured_nodes) >= 2
              # The nodes should overlap (auto cluster reused)
              assert Enum.any?(auto_nodes, &(&1 in ensured_nodes))

            {:error, reason} ->
              flunk("ensure_cluster failed: #{inspect(reason)}")
          end

        {:error, reason} ->
          Logger.warning("Skipping test due to cluster startup failure: #{inspect(reason)}")
          ExUnit.skip("Cluster startup failed: #{inspect(reason)}")
      end
    end

    test "ensure_cluster/1 falls back to manual when auto cluster insufficient" do
      # Start small auto cluster
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 1,
        test_type: :fallback_test
      }

      case AutoClusterManager.start_cluster_for_tests(requirements) do
        {:ok, %{cluster_active: true}} ->
          # Request larger cluster - should fall back to manual
          case ClusterTestHelper.ensure_cluster(size: 3, force_new: true) do
            {:ok, nodes} ->
              assert length(nodes) >= 3

              # Clean up manual cluster
              ClusterTestHelper.stop_test_cluster(nodes)

            {:error, reason} ->
              # Manual cluster creation might fail in constrained environments
              Logger.warning("Manual cluster creation failed: #{inspect(reason)}")
              ExUnit.skip("Manual cluster creation failed: #{inspect(reason)}")
          end

        {:error, reason} ->
          Logger.warning("Skipping test due to auto cluster startup failure: #{inspect(reason)}")
          ExUnit.skip("Auto cluster startup failed: #{inspect(reason)}")
      end
    end
  end

  describe "backward compatibility" do
    test "manual cluster management still works independently" do
      # Traditional manual cluster management should work unchanged
      case ClusterTestHelper.start_test_cluster(2) do
        {:ok, nodes} ->
          assert length(nodes) == 2
          assert Enum.all?(nodes, &is_atom/1)

          # Verify cluster connectivity
          :ok = ClusterTestHelper.wait_for_cluster(2, 10_000)

          # Clean up
          :ok = ClusterTestHelper.stop_test_cluster(nodes)

        {:error, reason} ->
          Logger.warning("Manual cluster test failed: #{inspect(reason)}")
          ExUnit.skip("Manual cluster creation failed: #{inspect(reason)}")
      end
    end

    test "existing helper functions remain functional" do
      # Test that all existing functions still work as expected
      assert is_integer(ClusterTestHelper.cluster_size())
      assert ClusterTestHelper.cluster_size() >= 1  # At least this node

      # Test wait_until function
      assert :ok = ClusterTestHelper.wait_until(fn -> true end, 1000)
      assert {:error, :timeout} = ClusterTestHelper.wait_until(fn -> false end, 100)

      # Test verify_distributed_components (may fail in test environment)
      result = ClusterTestHelper.verify_distributed_components()
      assert is_boolean(result)
    end
  end

  describe "enhanced error reporting" do
    test "diagnose_cluster_error/2 provides detailed diagnostics" do
      error = {:timeout, :cluster_startup}
      context = %{operation: :test, cluster_size: 2}

      diagnosis = ClusterTestHelper.diagnose_cluster_error(error, context)

      assert is_map(diagnosis)
      assert Map.has_key?(diagnosis, :error)
      assert Map.has_key?(diagnosis, :context)
      assert Map.has_key?(diagnosis, :timestamp)
      assert Map.has_key?(diagnosis, :cluster_state)
      assert Map.has_key?(diagnosis, :system_info)
      assert Map.has_key?(diagnosis, :problem)
      assert Map.has_key?(diagnosis, :likely_causes)
      assert Map.has_key?(diagnosis, :suggestions)

      assert is_list(diagnosis.likely_causes)
      assert is_list(diagnosis.suggestions)
      assert is_binary(diagnosis.problem)
    end

    test "wait_for_cluster_health/2 provides detailed failure information" do
      # This should timeout quickly and provide diagnostic info
      case ClusterTestHelper.wait_for_cluster_health(10, 100) do
        :ok ->
          # Unexpected success - we don't have 10 nodes
          flunk("Expected cluster health check to fail")

        {:error, diagnosis} ->
          assert is_map(diagnosis)
          assert Map.has_key?(diagnosis, :expected_nodes)
          assert Map.has_key?(diagnosis, :current_size)
          assert Map.has_key?(diagnosis, :connected_nodes)
          assert Map.has_key?(diagnosis, :failure_reason)
          assert Map.has_key?(diagnosis, :diagnosis)

          assert diagnosis.expected_nodes == 10
          assert is_integer(diagnosis.current_size)
          assert is_list(diagnosis.connected_nodes)
          assert is_binary(diagnosis.diagnosis)
      end
    end
  end

  describe "cleanup coordination" do
    test "coordinate_cleanup/0 handles mixed cluster scenarios" do
      # This should always succeed, even with no clusters
      result = ClusterTestHelper.coordinate_cleanup()
      assert result == :ok or match?({:warning, _}, result)
    end

    test "cleanup_all_test_resources/0 remains functional" do
      # Should not crash even if no resources to clean up
      assert :ok = ClusterTestHelper.cleanup_all_test_resources()
    end
  end

  describe "integration edge cases" do
    test "handles AutoClusterManager not running gracefully" do
      # Stop AutoClusterManager temporarily
      original_pid = Process.whereis(AutoClusterManager)
      if original_pid do
        Process.exit(original_pid, :kill)
        :timer.sleep(100)  # Give it time to die
      end

      # Functions should handle this gracefully
      nodes = ClusterTestHelper.get_cluster_nodes()
      assert nodes == []

      status = ClusterTestHelper.check_auto_cluster_status(2)
      assert {:error, _} = status

      # Restart AutoClusterManager for other tests
      {:ok, _pid} = AutoClusterManager.start_link()
    end

    test "handles concurrent access safely" do
      # Spawn multiple processes trying to use cluster functions
      tasks = for i <- 1..5 do
        Task.async(fn ->
          nodes = ClusterTestHelper.get_cluster_nodes()
          status = ClusterTestHelper.check_auto_cluster_status(1)
          {i, nodes, status}
        end)
      end

      results = Task.await_many(tasks, 5000)

      # All should complete without crashing
      assert length(results) == 5
      Enum.each(results, fn {i, nodes, status} ->
        assert is_integer(i)
        assert is_list(nodes)
        assert match?({:ok, _}, status) or match?({:error, _}, status)
      end)
    end
  end
end
