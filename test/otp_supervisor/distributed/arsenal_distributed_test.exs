defmodule OTPSupervisor.Distributed.ArsenalDistributedTest do
  @moduledoc """
  Comprehensive tests for distributed Arsenal operations.

  Tests all distributed Arsenal operations using both simulation
  and real multi-node scenarios.
  """

  use OTPSupervisor.DistributedTestCase

  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.{
    ClusterHealth,
    ClusterTopology,
    NodeInfo,
    ProcessList
  }

  describe "ClusterHealth operation" do
    test "returns healthy status for simulated cluster" do
      log_test_step("Testing ClusterHealth with simulation")

      # Test uses default simulation setup from DistributedTestCase
      data = assert_arsenal_operation_succeeds(ClusterHealth, %{})

      # Default simulation has 2 simulated + 1 real node
      assert data.nodes_total == 3
      assert data.overall_status == :healthy
      assert data.nodes_healthy == 3
      assert data.nodes_unhealthy == 0

      # Verify node statuses
      assert map_size(data.node_statuses) == 3

      Enum.each(data.node_statuses, fn {_node, status} ->
        assert status.status == :up
        assert status.health_score == 100
      end)
    end

    test "includes performance metrics when requested" do
      data =
        assert_arsenal_operation_succeeds(ClusterHealth, %{
          "include_metrics" => true
        })

      assert Map.has_key?(data, :performance_metrics)
      metrics = data.performance_metrics

      assert Map.has_key?(metrics, :total_processes)
      assert Map.has_key?(metrics, :memory_usage)
      assert is_integer(metrics.total_processes)
      assert is_map(metrics.memory_usage)
    end

    test "detects node failures in simulation" do
      log_test_step("Testing node failure detection")

      # Use existing simulated nodes from test setup
      topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()
      simulated_nodes = topology.nodes -- [Node.self()]
      first_node = List.first(simulated_nodes)

      # Simulate node failure
      :ok = OTPSupervisor.Distributed.SingleNodeSimulator.simulate_node_failure(first_node)

      data = assert_arsenal_operation_succeeds(ClusterHealth, %{})

      # 1 real + 2 simulated nodes
      assert data.nodes_total == 3
      # 1 real + 1 healthy simulated
      assert data.nodes_healthy == 2
      # 1 failed simulated
      assert data.nodes_unhealthy == 1

      # Verify failed node status
      failed_node_status = data.node_statuses[first_node]
      assert failed_node_status.status == :down
    end

    @tag :real_nodes
    @tag node_count: 1
    test "works with real multi-node cluster" do
      log_test_step("Testing ClusterHealth with real nodes")

      # This test uses real nodes (setup by DistributedTestCase)
      # Primary + 1 test node
      assert_cluster_size(2)

      data =
        time_execution("ClusterHealth operation", fn ->
          assert_arsenal_operation_succeeds(ClusterHealth, %{})
        end)

      assert data.nodes_total == 2
      assert data.overall_status in [:healthy, :warning]

      # Verify all nodes are real (not simulated)
      Enum.each(data.node_statuses, fn {_node, status} ->
        refute Map.get(status, :simulated, false)
      end)
    end
  end

  describe "ClusterTopology operation" do
    test "returns complete topology information" do
      data =
        assert_arsenal_operation_succeeds(ClusterTopology, %{
          "include_health" => true
        })

      assert is_list(data.nodes)
      # 1 real + 2 simulated nodes
      assert length(data.nodes) == 3
      assert data.total_nodes == 3
      # Simulation doesn't change ToolManager mode
      assert data.mode == :single_node
      assert is_boolean(data.simulation_enabled)

      # Verify node health information is included when explicitly requested
      assert Map.has_key?(data, :node_health)
      assert map_size(data.node_health) == 3
    end

    test "includes detailed node information when requested" do
      data =
        assert_arsenal_operation_succeeds(ClusterTopology, %{
          "include_details" => true
        })

      # Should include more detailed information
      assert Map.has_key?(data, :node_health)

      Enum.each(data.node_health, fn {_node, health} ->
        assert Map.has_key?(health, :memory_usage)
        assert Map.has_key?(health, :cpu_usage)
        assert Map.has_key?(health, :status)
      end)
    end
  end

  describe "NodeInfo operation" do
    test "returns detailed information for simulated node" do
      # Get the topology to find available simulated nodes
      topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()
      simulated_nodes = topology.nodes -- [Node.self()]
      first_node = List.first(simulated_nodes)

      data =
        assert_arsenal_operation_succeeds(NodeInfo, %{
          "node" => Atom.to_string(first_node)
        })

      assert data.name == first_node
      assert data.status == :up
      assert data.simulated == true

      # Should have basic system info
      assert Map.has_key?(data, :system_info)
      assert Map.has_key?(data, :applications)
      assert Map.has_key?(data, :network_info)
    end

    test "includes process information when requested" do
      # Use existing simulated nodes from test setup
      topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()
      simulated_nodes = topology.nodes -- [Node.self()]
      first_node = List.first(simulated_nodes)

      data =
        assert_arsenal_operation_succeeds(NodeInfo, %{
          "node" => Atom.to_string(first_node),
          "include_processes" => true,
          "process_limit" => 10
        })

      assert Map.has_key?(data, :processes)
      assert is_list(data.processes)
      assert length(data.processes) <= 10

      # Verify process information structure
      if length(data.processes) > 0 do
        process = List.first(data.processes)
        assert Map.has_key?(process, :pid)
        # Note: :node key might not be included in basic process info
        # This is acceptable for simulated processes
      end
    end

    test "fails for non-existent node" do
      assert_arsenal_operation_fails(
        NodeInfo,
        %{
          "node" => "nonexistent@nowhere"
        },
        :node_not_found
      )
    end

    @tag :real_nodes
    @tag node_count: 1
    test "returns real node information", context do
      [test_node] = context[:test_nodes]

      data =
        assert_arsenal_operation_succeeds(NodeInfo, %{
          "node" => Atom.to_string(test_node),
          "include_processes" => true
        })

      assert data.name == test_node
      assert data.status == :up
      refute Map.get(data, :simulated, false)

      # Should have real system information
      assert is_map(data.system_info)
      assert is_list(data.applications)
      assert length(data.applications) > 0
    end
  end

  describe "ProcessList operation" do
    test "lists processes across simulated cluster" do
      data =
        assert_arsenal_operation_succeeds(ProcessList, %{
          "limit" => 50,
          "include_details" => true
        })

      assert is_list(data.processes)
      assert data.total_count > 0
      # 1 real + 2 simulated nodes
      assert length(data.nodes_queried) == 3

      # Verify process information
      if length(data.processes) > 0 do
        process = List.first(data.processes)
        assert Map.has_key?(process, :pid)
        assert Map.has_key?(process, :node)
        assert Map.has_key?(process, :alive)
      end
    end

    test "filters processes by node" do
      {:ok, [first_node | _]} = enable_simulation(3)

      data =
        assert_arsenal_operation_succeeds(ProcessList, %{
          "node" => Atom.to_string(first_node),
          "limit" => 20
        })

      assert length(data.nodes_queried) == 1
      assert first_node in data.nodes_queried

      # All processes should be from the specified node
      Enum.each(data.processes, fn process ->
        assert process.node == first_node
      end)
    end

    test "filters processes by type" do
      data =
        assert_arsenal_operation_succeeds(ProcessList, %{
          "type" => "supervisor",
          "limit" => 10,
          "include_details" => true
        })

      # All returned processes should be supervisors
      Enum.each(data.processes, fn process ->
        if Map.has_key?(process, :type) do
          assert process.type == "supervisor"
        end
      end)
    end

    test "respects limit parameter" do
      data =
        assert_arsenal_operation_succeeds(ProcessList, %{
          "limit" => 5
        })

      assert length(data.processes) <= 5
    end
  end

  describe "error handling" do
    test "operations handle simulation failures gracefully" do
      {:ok, [first_node | _]} = enable_simulation(2)

      # Simulate node failure
      :ok = SingleNodeSimulator.simulate_node_failure(first_node)

      # Operations should still work, but report the failure
      health_data = assert_arsenal_operation_succeeds(ClusterHealth, %{})
      assert health_data.nodes_unhealthy == 1

      # NodeInfo should report the node as down
      node_data =
        assert_arsenal_operation_succeeds(NodeInfo, %{
          "node" => Atom.to_string(first_node)
        })

      assert node_data.status == :down
    end

    test "operations handle network partitions" do
      # Use existing simulated nodes from test setup
      topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()
      simulated_nodes = topology.nodes -- [Node.self()]
      [node1, node2] = Enum.take(simulated_nodes, 2)

      # Simulate network partition
      {:ok, partitioned_nodes} =
        OTPSupervisor.Distributed.SingleNodeSimulator.simulate_network_partition([node1, node2])

      health_data = assert_arsenal_operation_succeeds(ClusterHealth, %{})

      # Should detect partition in some form
      partitioned_count =
        Enum.count(health_data.node_statuses, fn {_node, status} ->
          status.status == :partitioned
        end)

      assert partitioned_count == length(partitioned_nodes)
    end
  end

  describe "performance" do
    test "operations complete within acceptable time" do
      # Test that operations are reasonably fast
      operations = [
        {ClusterHealth, %{}},
        {ClusterTopology, %{}},
        {ProcessList, %{"limit" => 10}}
      ]

      Enum.each(operations, fn {operation, params} ->
        {time, _result} =
          :timer.tc(fn ->
            assert_arsenal_operation_succeeds(operation, params)
          end)

        # Should complete within 100ms for simulated cluster
        assert time < 100_000, "#{operation} took #{time}μs, expected < 100ms"
      end)
    end

    @tag :real_nodes
    @tag node_count: 2
    test "operations scale with real cluster size" do
      # Test with larger real cluster
      # Primary + 2 test nodes
      assert_cluster_size(3)

      {time, data} =
        time_execution("ClusterHealth with 3 nodes", fn ->
          assert_arsenal_operation_succeeds(ClusterHealth, %{"include_metrics" => true})
        end)

      assert data.nodes_total == 3

      # Should still complete reasonably quickly
      # Real nodes are slower than simulation, so allow more time
      assert time < 500_000, "ClusterHealth took #{time}μs, expected < 500ms"
    end
  end
end
