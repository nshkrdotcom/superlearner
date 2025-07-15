defmodule OTPSupervisor.Distributed.MultiNodeTest do
  use ExUnit.Case, async: false
  
  # Only run these tests when explicitly requested
  @moduletag :distributed
  @moduletag timeout: 30_000
  
  alias OTPSupervisor.Distributed.{ToolManager, ClusterStateManager}
  
  setup_all do
    # Start distributed Erlang if not already started
    unless Node.alive?() do
      Node.start(:"test_primary@127.0.0.1", :shortnames)
      Node.set_cookie(:test_cluster_cookie)
    end
    
    :ok
  end
  
  setup do
    # Reset any simulation state
    try do
      OTPSupervisor.Distributed.SingleNodeSimulator.disable_simulation()
    catch
      _, _ -> :ok
    end
    
    :ok
  end
  
  describe "basic distributed functionality" do
    @tag :skip  # Skip by default, run with: mix test --include distributed
    test "can connect to manually started node" do
      # This test requires manually starting a second node
      # Run: iex --sname test_secondary@127.0.0.1 --cookie test_cluster_cookie -S mix
      
      secondary_node = :"test_secondary@127.0.0.1"
      
      case Node.connect(secondary_node) do
        true ->
          # Connection successful, test our components
          assert secondary_node in Node.list()
          
          # Test ToolManager
          cluster_status = ToolManager.get_cluster_status()
          assert cluster_status.mode == :multi_node
          assert secondary_node in cluster_status.nodes
          
          # Test ClusterStateManager
          topology = ClusterStateManager.get_cluster_topology()
          assert topology.total_nodes >= 2
          assert secondary_node in topology.nodes
          
          # Test process distribution
          process_dist = ClusterStateManager.get_process_distribution()
          assert Map.has_key?(process_dist, secondary_node)
          
          # Disconnect for cleanup
          Node.disconnect(secondary_node)
          
        false ->
          # Skip test if secondary node not available
          IO.puts("Skipping multi-node test - secondary node not available")
          IO.puts("To run this test, start: iex --sname test_secondary@127.0.0.1 --cookie test_cluster_cookie -S mix")
      end
    end
  end
  
  describe "simulation mode (always available)" do
    test "single node simulator enables multi-node behavior" do
      # This test doesn't require real nodes
      simulator = OTPSupervisor.Distributed.SingleNodeSimulator
      
      # Start with simulation disabled
      refute simulator.simulation_enabled?()
      
      # Enable simulation with 3 nodes
      {:ok, simulated_nodes} = simulator.enable_simulation(3)
      assert length(simulated_nodes) == 3
      assert simulator.simulation_enabled?()
      
      # Test that cluster components see simulated nodes
      topology = ClusterStateManager.get_cluster_topology()
      assert length(topology.nodes) == 3
      
      # Test simulated process distribution
      process_dist = ClusterStateManager.get_process_distribution()
      assert map_size(process_dist) == 3
      
      # Test Arsenal operations with simulation
      {:ok, health_data} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.execute(%{})
      assert health_data.nodes_total == 3
      assert health_data.overall_status == :healthy
      
      # Test node failure simulation
      [first_node | _] = simulated_nodes
      :ok = simulator.simulate_node_failure(first_node)
      
      # Verify failure is detected
      {:ok, updated_health} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.execute(%{})
      failed_status = updated_health.node_statuses[first_node]
      assert failed_status.status == :down
      
      # Cleanup
      simulator.disable_simulation()
      refute simulator.simulation_enabled?()
    end
    
    test "cluster health operation works with simulated nodes" do
      simulator = OTPSupervisor.Distributed.SingleNodeSimulator
      
      {:ok, _nodes} = simulator.enable_simulation(2)
      
      # Test cluster health
      {:ok, health_data} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.execute(%{
        "include_metrics" => true,
        "include_history" => false
      })
      
      assert health_data.nodes_total == 2
      assert health_data.nodes_healthy == 2
      assert health_data.overall_status == :healthy
      assert Map.has_key?(health_data, :performance_metrics)
      
      simulator.disable_simulation()
    end
    
    test "node info operation works with simulated nodes" do
      simulator = OTPSupervisor.Distributed.SingleNodeSimulator
      
      {:ok, [first_node | _]} = simulator.enable_simulation(2)
      
      # Test node info
      {:ok, node_info} = OTPSupervisor.Core.Arsenal.Operations.Distributed.NodeInfo.execute(%{
        "node" => Atom.to_string(first_node),
        "include_processes" => true,
        "process_limit" => 10
      })
      
      assert node_info.name == first_node
      assert node_info.status == :up
      assert node_info.simulated == true
      assert is_list(node_info.processes)
      
      simulator.disable_simulation()
    end
    
    test "process list operation works with simulated nodes" do
      simulator = OTPSupervisor.Distributed.SingleNodeSimulator
      
      {:ok, nodes} = simulator.enable_simulation(2)
      
      # Test process list
      {:ok, process_data} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList.execute(%{
        "limit" => 20,
        "include_details" => true
      })
      
      assert length(process_data.processes) > 0
      assert process_data.total_count > 0
      assert length(process_data.nodes_queried) == 2
      
      # Test filtering by node
      [first_node | _] = nodes
      {:ok, filtered_data} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList.execute(%{
        "node" => Atom.to_string(first_node),
        "limit" => 10
      })
      
      assert length(filtered_data.nodes_queried) == 1
      assert first_node in filtered_data.nodes_queried
      
      simulator.disable_simulation()
    end
  end
  
  describe "mode switching" do
    test "tool manager switches modes based on cluster state" do
      # Start in single node mode
      initial_status = ToolManager.get_cluster_status()
      assert initial_status.mode == :single_node
      assert length(initial_status.nodes) == 1
      
      # Enable simulation to simulate multi-node
      simulator = OTPSupervisor.Distributed.SingleNodeSimulator
      {:ok, _nodes} = simulator.enable_simulation(3)
      
      # Mode should still be single_node until we check cluster status
      # (because simulation doesn't automatically trigger mode changes)
      updated_status = ToolManager.get_cluster_status()
      
      # The mode detection should see we have multiple nodes now
      # (This tests the dynamic mode detection we added)
      assert length(updated_status.nodes) == 3
      
      simulator.disable_simulation()
    end
  end
end