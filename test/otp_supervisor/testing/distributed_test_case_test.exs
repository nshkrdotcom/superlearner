defmodule OTPSupervisor.Testing.DistributedTestCaseTest do
  use ExUnit.Case, async: false
  @moduletag :distributed

  # Import the module we're testing
  import OTPSupervisor.Testing.DistributedTestCase

  alias OTPSupervisor.Testing.AutoClusterManager

  setup do
    # Ensure clean state before each test
    AutoClusterManager.force_cleanup_all()
    :timer.sleep(100)
    :ok
  end

  describe "helper functions without cluster (enforcement tests)" do
    test "cluster_nodes/0 raises when no cluster is active" do
      # Ensure no cluster is running
      AutoClusterManager.force_cleanup_all()
      :timer.sleep(100)

      # Should raise RuntimeError when no cluster is available
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     cluster_nodes()
                   end
    end

    test "cluster_size/0 raises when no cluster is active" do
      # Ensure no cluster is running
      AutoClusterManager.force_cleanup_all()
      :timer.sleep(100)

      # Should raise RuntimeError because it calls cluster_nodes/0
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     cluster_size()
                   end
    end

    test "cluster_healthy?/0 raises when no cluster is active" do
      # Ensure no cluster is running
      AutoClusterManager.force_cleanup_all()
      :timer.sleep(100)

      # Should raise RuntimeError because it calls cluster_nodes/0
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     cluster_healthy?()
                   end
    end

    test "with_cluster_size/2 raises when no cluster is active" do
      # Ensure no cluster is running
      AutoClusterManager.force_cleanup_all()
      :timer.sleep(100)

      # Should raise RuntimeError because it calls cluster_nodes/0
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     with_cluster_size(2, fn _nodes ->
                       flunk("This should not execute")
                     end)
                   end
    end

    test "cluster_info/0 returns inactive cluster info when no cluster manager" do
      # Ensure no cluster is running
      AutoClusterManager.force_cleanup_all()
      :timer.sleep(100)

      info = cluster_info()
      assert info.cluster_active == false
      assert info.nodes == []
      assert info.error == :no_cluster_manager
    end

    test "cluster_rpc/4 raises when no cluster is active" do
      # Ensure no cluster is running
      AutoClusterManager.force_cleanup_all()
      :timer.sleep(100)

      # Should raise RuntimeError because it calls cluster_nodes/0
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     cluster_rpc(Node, :self, [])
                   end
    end

    test "cluster_call/2 raises when no cluster is active" do
      # Ensure no cluster is running
      AutoClusterManager.force_cleanup_all()
      :timer.sleep(100)

      # Should raise RuntimeError because it calls cluster_nodes/0
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     cluster_call(fn -> :ok end)
                   end
    end
  end

  describe "helper functions with live cluster" do
    setup do
      # Start a test cluster for these tests
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }

      case AutoClusterManager.start_cluster_for_tests(requirements) do
        {:ok, cluster_info} ->
          # Wait for cluster to be healthy
          case wait_for_cluster_health(timeout: 30_000) do
            :ok ->
              {:ok, cluster_info: cluster_info}

            {:error, :timeout} ->
              exit({:skip, "Could not establish healthy cluster for testing"})
          end

        {:error, _diagnosis} ->
          exit(
            {:skip,
             "Could not start cluster for testing - may be running in constrained environment"}
          )
      end
    end

    test "cluster_nodes/0 returns actual cluster nodes", %{cluster_info: cluster_info} do
      nodes = cluster_nodes()

      # Should have at least 2 nodes
      assert length(nodes) >= 2
      # Should match the cluster info
      assert length(nodes) == length(cluster_info.nodes)
      # All nodes should be atoms
      assert Enum.all?(nodes, &is_atom/1)
    end

    test "cluster_size/0 returns actual cluster size", %{cluster_info: cluster_info} do
      size = cluster_size()

      # Should match expected size
      assert size >= 2
      assert size == length(cluster_info.nodes)
    end

    test "cluster_healthy?/0 returns true for healthy cluster" do
      # Should return true for a healthy cluster
      assert cluster_healthy?() == true
    end

    test "with_cluster_size/2 works with sufficient nodes" do
      # Should work with cluster size 2
      result =
        with_cluster_size(2, fn nodes ->
          assert length(nodes) == 2
          assert Enum.all?(nodes, &is_atom/1)
          :test_passed
        end)

      assert result == :test_passed
    end

    test "with_cluster_size/2 skips when requesting more nodes than available" do
      current_size = cluster_size()

      # Request more nodes than available
      try do
        with_cluster_size(current_size + 5, fn _nodes ->
          flunk("This should not execute")
        end)
      catch
        :exit, {:skip, message} ->
          assert String.contains?(message, "Insufficient cluster size")
          assert String.contains?(message, "need #{current_size + 5}")
          assert String.contains?(message, "have #{current_size}")
      end
    end

    test "cluster_info/0 returns active cluster info" do
      info = cluster_info()

      assert info.cluster_active == true
      assert length(info.nodes) >= 2
      refute Map.has_key?(info, :error)
    end

    test "cluster_rpc/4 works with live cluster" do
      results = cluster_rpc(Node, :self, [])

      # Should get results from all nodes
      nodes = cluster_nodes()
      assert length(results) == length(nodes)

      # All calls should succeed and return the node name
      Enum.each(results, fn {node, result} ->
        assert {:ok, ^node} = result
        assert node in nodes
      end)
    end

    test "cluster_call/2 works with live cluster" do
      results = cluster_call(fn -> Node.self() end)

      # Should get results from all nodes
      nodes = cluster_nodes()
      assert length(results) == length(nodes)

      # All calls should succeed and return the node name
      Enum.each(results, fn {node, result} ->
        assert {:ok, ^node} = result
        assert node in nodes
      end)
    end
  end

  describe "wait functions without cluster (enforcement tests)" do
    test "wait_for_cluster_health/1 raises when no cluster" do
      # Ensure no cluster is running
      AutoClusterManager.force_cleanup_all()
      :timer.sleep(100)

      # Should raise RuntimeError because it calls cluster_healthy?() which calls cluster_nodes/0
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     wait_for_cluster_health(timeout: 1000, interval: 100)
                   end
    end

    test "wait_for_cluster_size/2 raises when no cluster" do
      # Ensure no cluster is running
      AutoClusterManager.force_cleanup_all()
      :timer.sleep(100)

      # Should raise RuntimeError because it calls cluster_size/0 which calls cluster_nodes/0
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     wait_for_cluster_size(2, timeout: 1000, interval: 100)
                   end
    end

    test "wait_for_cluster_condition/2 raises when no cluster" do
      # Ensure no cluster is running
      AutoClusterManager.force_cleanup_all()
      :timer.sleep(100)

      # Should raise RuntimeError because it calls cluster_nodes/0
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     wait_for_cluster_condition(
                       fn _node -> false end,
                       timeout: 1000,
                       interval: 100
                     )
                   end
    end
  end

  describe "wait functions with live cluster" do
    setup do
      # Start a test cluster for these tests
      requirements = %{
        needs_cluster: true,
        min_cluster_size: 2,
        test_type: :distributed
      }

      case AutoClusterManager.start_cluster_for_tests(requirements) do
        {:ok, cluster_info} ->
          # Wait for cluster to be healthy
          case wait_for_cluster_health(timeout: 30_000) do
            :ok ->
              {:ok, cluster_info: cluster_info}

            {:error, :timeout} ->
              exit({:skip, "Could not establish healthy cluster for testing"})
          end

        {:error, _diagnosis} ->
          exit(
            {:skip,
             "Could not start cluster for testing - may be running in constrained environment"}
          )
      end
    end

    test "wait_for_cluster_health/1 succeeds quickly with healthy cluster" do
      start_time = System.monotonic_time(:millisecond)

      result = wait_for_cluster_health(timeout: 10_000, interval: 100)

      end_time = System.monotonic_time(:millisecond)
      elapsed = end_time - start_time

      assert result == :ok
      # Should succeed quickly since cluster is already healthy
      assert elapsed < 5_000
    end

    test "wait_for_cluster_size/2 succeeds when cluster has sufficient size", %{
      cluster_info: cluster_info
    } do
      current_size = length(cluster_info.nodes)

      start_time = System.monotonic_time(:millisecond)

      result = wait_for_cluster_size(current_size, timeout: 10_000, interval: 100)

      end_time = System.monotonic_time(:millisecond)
      elapsed = end_time - start_time

      assert result == :ok
      # Should succeed quickly since cluster already has the required size
      assert elapsed < 5_000
    end

    test "wait_for_cluster_condition/2 works with live cluster" do
      result =
        wait_for_cluster_condition(
          fn node ->
            case :rpc.call(node, :erlang, :is_alive, []) do
              true -> true
              _ -> false
            end
          end,
          timeout: 10_000,
          interval: 100
        )

      assert result == :ok
    end
  end
end
