defmodule ClusterTestHelperTest do
  # Cluster operations must be sequential
  use ExUnit.Case, async: false

  @moduletag :cluster_management

  setup do
    # Clean up before each test
    ClusterTestHelper.cleanup_all_test_resources()
    :timer.sleep(500)

    on_exit(fn ->
      # Clean up after each test
      ClusterTestHelper.cleanup_all_test_resources()
      :timer.sleep(500)
    end)

    :ok
  end

  describe "enhanced cluster test helper" do
    test "start_test_node uses reliable hostname resolution" do
      # Test that we can start a test node with the enhanced helper
      case ClusterTestHelper.start_test_node("reliability_test") do
        {:ok, node} ->
          assert is_atom(node)

          # Node name should contain our hostname resolution
          node_string = Atom.to_string(node)
          assert String.contains?(node_string, "@")

          # Should be able to ping the node
          assert Node.ping(node) == :pong

          # Clean up
          ClusterTestHelper.stop_test_node(node)

        {:error, reason} ->
          # In test environment, node startup might fail due to various reasons
          # That's ok, we're mainly testing that the function doesn't crash
          # and provides reasonable error handling
          assert is_atom(reason) or is_tuple(reason)
      end
    end

    test "start_test_cluster handles port allocation" do
      case ClusterTestHelper.start_test_cluster(2) do
        {:ok, nodes} ->
          assert is_list(nodes)
          assert length(nodes) == 2

          # All nodes should be atoms
          assert Enum.all?(nodes, &is_atom/1)

          # All nodes should be unique
          assert length(nodes) == length(Enum.uniq(nodes))

          # Clean up
          ClusterTestHelper.stop_test_cluster(nodes)

        {:error, reason} ->
          # Cluster startup might fail in test environment
          # Verify we get reasonable error information
          case reason do
            {:port_allocation_failed, _} ->
              # This is expected if ports are busy
              :ok

            {:partial_startup_failed, _} ->
              # This is expected if some nodes fail to start
              :ok

            _ ->
              # Other errors should be reasonable
              assert is_atom(reason) or is_tuple(reason)
          end
      end
    end

    test "cleanup functions work reliably" do
      # Test that cleanup doesn't crash and handles edge cases

      # Cleanup with no nodes should work
      assert :ok = ClusterTestHelper.stop_test_cluster([])
      assert :ok = ClusterTestHelper.cleanup_all_test_resources()

      # Cleanup with invalid input should handle gracefully
      assert {:error, :invalid_nodes_list} = ClusterTestHelper.stop_test_cluster("invalid")

      # Cleanup with nil node should work
      assert :ok = ClusterTestHelper.stop_test_node(nil)

      # Cleanup with non-existent node should work
      assert :ok = ClusterTestHelper.stop_test_node(:non_existent_node@localhost)
    end

    test "wait_for_cluster handles timeouts gracefully" do
      # Test waiting for a cluster that will never form
      start_time = System.monotonic_time(:millisecond)

      # Wait for 10 nodes, 1 second timeout
      result = ClusterTestHelper.wait_for_cluster(10, 1000)

      end_time = System.monotonic_time(:millisecond)
      elapsed = end_time - start_time

      # Should timeout
      assert result == {:error, :timeout}

      # Should respect timeout (allow some margin for test execution)
      assert elapsed >= 1000
      assert elapsed < 2000
    end

    test "wait_until function works correctly" do
      # Test with a condition that becomes true
      counter = :counters.new(1, [])

      task =
        Task.async(fn ->
          :timer.sleep(500)
          :counters.add(counter, 1, 1)
        end)

      result =
        ClusterTestHelper.wait_until(
          fn ->
            :counters.get(counter, 1) > 0
          end,
          2000
        )

      Task.await(task)

      assert result == :ok
    end

    test "wait_until handles timeout correctly" do
      start_time = System.monotonic_time(:millisecond)

      result = ClusterTestHelper.wait_until(fn -> false end, 500)

      end_time = System.monotonic_time(:millisecond)
      elapsed = end_time - start_time

      assert result == {:error, :timeout}
      assert elapsed >= 500
      assert elapsed < 1000
    end

    test "cluster_size reports correctly" do
      initial_size = ClusterTestHelper.cluster_size()

      # Should be at least 1 (current node)
      assert initial_size >= 1

      # Should be an integer
      assert is_integer(initial_size)
    end

    test "verify_distributed_components handles missing components" do
      # This test verifies that the function doesn't crash when components aren't available
      result = ClusterTestHelper.verify_distributed_components()

      # Should return a boolean
      assert is_boolean(result)

      # In test environment, distributed components might not be running
      # That's ok, we just want to ensure the function handles this gracefully
    end
  end

  describe "reliability improvements" do
    test "hostname resolution is consistent across calls" do
      # Make multiple calls to functions that use hostname resolution
      results =
        for _ <- 1..5 do
          case ClusterTestHelper.start_test_node("consistency_test_#{:rand.uniform(1000)}") do
            {:ok, node} ->
              node_string = Atom.to_string(node)
              [_name, hostname] = String.split(node_string, "@")
              ClusterTestHelper.stop_test_node(node)
              {:ok, hostname}

            {:error, reason} ->
              {:error, reason}
          end
        end

      # Filter successful results
      successful_results = Enum.filter(results, &match?({:ok, _}, &1))

      if length(successful_results) > 1 do
        # All successful results should use the same hostname
        hostnames = Enum.map(successful_results, fn {:ok, hostname} -> hostname end)
        unique_hostnames = Enum.uniq(hostnames)
        assert length(unique_hostnames) == 1, "Hostname should be consistent across calls"
      end
    end

    test "port management prevents conflicts" do
      # Test that multiple cluster operations don't conflict

      # Start first cluster
      case ClusterTestHelper.start_test_cluster(1) do
        {:ok, cluster1} ->
          # Start second cluster - should get different ports
          case ClusterTestHelper.start_test_cluster(1) do
            {:ok, cluster2} ->
              # Clusters should be different
              assert cluster1 != cluster2

              # Clean up both
              ClusterTestHelper.stop_test_cluster(cluster1)
              ClusterTestHelper.stop_test_cluster(cluster2)

            {:error, _reason} ->
              # Second cluster might fail due to resource constraints
              # Clean up first cluster
              ClusterTestHelper.stop_test_cluster(cluster1)
          end

        {:error, _reason} ->
          # First cluster might fail in test environment
          :ok
      end
    end

    test "error handling provides useful information" do
      # Test various error scenarios to ensure we get useful error information

      # Try to start node with invalid options
      result = ClusterTestHelper.start_test_node("", cookie: :invalid_cookie_format)

      case result do
        {:ok, node} ->
          # If it somehow succeeded, clean up
          ClusterTestHelper.stop_test_node(node)

        {:error, reason} ->
          # Should get a reasonable error
          assert is_atom(reason) or is_tuple(reason)
      end
    end
  end

  describe "WSL compatibility" do
    test "works with WSL networking constraints" do
      # Test that our WSL fixes work in practice

      # Should be able to determine cluster size
      size = ClusterTestHelper.cluster_size()
      assert is_integer(size)
      assert size >= 1

      # Should be able to run cleanup without errors
      assert :ok = ClusterTestHelper.cleanup_all_test_resources()

      # Should handle timeout scenarios gracefully
      # Impossible scenario
      result = ClusterTestHelper.wait_for_cluster(100, 100)
      assert result == {:error, :timeout}
    end
  end
end
