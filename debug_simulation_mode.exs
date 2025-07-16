# Test simulation mode behavior
IO.puts("=== Testing Simulation Mode ===")

# Check if simulation is enabled
simulation_enabled = OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
IO.puts("🔍 Simulation enabled: #{simulation_enabled}")

if not simulation_enabled do
  IO.puts("Enabling simulation mode...")
  OTPSupervisor.Distributed.SingleNodeSimulator.enable_simulation()
  
  # Wait a moment for simulation to activate
  Process.sleep(1000)
end

# Re-check simulation status
simulation_enabled = OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
IO.puts("🔍 Simulation enabled after activation: #{simulation_enabled}")

if simulation_enabled do
  # Test getting simulated topology
  simulated_topology = OTPSupervisor.Distributed.SingleNodeSimulator.get_simulated_topology()
  IO.puts("🔍 Simulated topology: #{inspect(simulated_topology, pretty: true)}")
  
  # Test ClusterTopology with simulation
  {:ok, params} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopology.validate_params(%{
    "include_processes" => false,
    "include_health" => true
  })
  
  {:ok, topology} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopology.execute(params)
  IO.puts("🔍 ClusterTopology with simulation: #{inspect(topology, pretty: true)}")
  
  # Test ClusterHealth with simulation
  {:ok, health_params} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.validate_params(%{
    "include_metrics" => true,
    "include_history" => false
  })
  
  {:ok, health} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.execute(health_params)
  IO.puts("🔍 ClusterHealth with simulation: #{inspect(health, pretty: true)}")
else
  IO.puts("Could not enable simulation mode")
end