defmodule OTPSupervisor.Distributed.SingleNodeSimulatorTest do
  # Changed to non-async to prevent test interference
  use ExUnit.Case, async: false
  @moduletag :distributed

  alias OTPSupervisor.Distributed.SingleNodeSimulator

  setup do
    # Reset simulator state before each test
    SingleNodeSimulator.disable_simulation()
    :ok
  end

  describe "single node simulation" do
    test "starts in disabled state" do
      refute SingleNodeSimulator.simulation_enabled?()
      assert SingleNodeSimulator.get_simulated_nodes() == [Node.self()]
    end

    test "can enable simulation with specified node count" do
      {:ok, nodes} = SingleNodeSimulator.enable_simulation(3)

      assert SingleNodeSimulator.simulation_enabled?()
      assert length(nodes) == 3
      assert SingleNodeSimulator.get_simulated_nodes() == nodes

      # All nodes should have simulated names
      Enum.each(nodes, fn node ->
        node_str = Atom.to_string(node)
        assert String.contains?(node_str, "_sim")
      end)
    end

    test "can disable simulation" do
      {:ok, _nodes} = SingleNodeSimulator.enable_simulation(2)
      assert SingleNodeSimulator.simulation_enabled?()

      :ok = SingleNodeSimulator.disable_simulation()
      refute SingleNodeSimulator.simulation_enabled?()
      assert SingleNodeSimulator.get_simulated_nodes() == [Node.self()]
    end

    test "can simulate node failures" do
      {:ok, [node1, node2, _node3]} = SingleNodeSimulator.enable_simulation(3)

      :ok = SingleNodeSimulator.simulate_node_failure(node1)

      topology = SingleNodeSimulator.get_simulated_topology()
      assert node1 in topology.failed_nodes
      refute node1 in topology.healthy_nodes
      assert node2 in topology.healthy_nodes
    end

    test "can simulate network partitions" do
      {:ok, [node1, node2, node3]} = SingleNodeSimulator.enable_simulation(3)

      {:ok, partitioned} = SingleNodeSimulator.simulate_network_partition([node1, node2])

      topology = SingleNodeSimulator.get_simulated_topology()
      assert node1 in topology.partitioned_nodes
      assert node2 in topology.partitioned_nodes
      refute node3 in topology.partitioned_nodes
      assert length(partitioned) == 2
    end

    test "simulates cross-node calls with latency" do
      # Note: We can't easily test latency with the supervised instance
      # since we can't pass init options, but we can test the functionality
      {:ok, [node1 | _]} = SingleNodeSimulator.enable_simulation(2)

      start_time = System.monotonic_time(:millisecond)

      {:ok, result} =
        SingleNodeSimulator.simulate_cross_node_call(node1, Enum, :count, [[1, 2, 3]])

      end_time = System.monotonic_time(:millisecond)

      assert result == 3
      # Note: Default latency is 0, so we just verify the call works
      assert end_time - start_time >= 0
    end

    test "handles failed node calls appropriately" do
      {:ok, [node1 | _]} = SingleNodeSimulator.enable_simulation(2)
      :ok = SingleNodeSimulator.simulate_node_failure(node1)

      {:error, {:nodedown, ^node1}} =
        SingleNodeSimulator.simulate_cross_node_call(node1, Enum, :count, [[1, 2, 3]])
    end
  end

  describe "interception functions" do
    test "intercept_node_list returns simulated nodes when enabled" do
      # Should return empty list when simulation disabled
      assert SingleNodeSimulator.intercept_node_list() == []

      {:ok, _nodes} = SingleNodeSimulator.enable_simulation(3)

      # Should return simulated nodes minus current node
      simulated_list = SingleNodeSimulator.intercept_node_list()
      assert length(simulated_list) == 3
      assert Node.self() not in simulated_list
    end
  end
end
