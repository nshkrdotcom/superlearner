# Test ClusterTopology operation
IO.puts("=== Testing ClusterTopology ===")

{:ok, params} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopology.validate_params(%{
  "include_processes" => false,
  "include_health" => true
})

IO.puts("🔍 ClusterTopology params: #{inspect(params, pretty: true)}")

case OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopology.execute(params) do
  {:ok, topology} -> 
    IO.puts("🔍 ClusterTopology result: #{inspect(topology, pretty: true)}")
  {:error, reason} -> 
    IO.puts("🔍 ClusterTopology error: #{inspect(reason)}")
end

IO.puts("\n=== Testing ClusterHealth ===")

# Test ClusterHealth operation  
{:ok, health_params} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.validate_params(%{
  "include_metrics" => true,
  "include_history" => false
})

IO.puts("🔍 ClusterHealth params: #{inspect(health_params, pretty: true)}")

case OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.execute(health_params) do
  {:ok, health} -> 
    IO.puts("🔍 ClusterHealth result: #{inspect(health, pretty: true)}")
  {:error, reason} -> 
    IO.puts("🔍 ClusterHealth error: #{inspect(reason)}")
end

IO.puts("\n=== Testing ClusterStateManager directly ===")

# Test ClusterStateManager calls directly
topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()
IO.puts("🔍 ClusterStateManager topology: #{inspect(topology, pretty: true)}")

partition_status = OTPSupervisor.Distributed.ClusterStateManager.get_partition_status()
IO.puts("🔍 ClusterStateManager partition_status: #{inspect(partition_status, pretty: true)}")

process_distribution = OTPSupervisor.Distributed.ClusterStateManager.get_process_distribution()
IO.puts("🔍 ClusterStateManager process_distribution count: #{map_size(process_distribution)} nodes")

# Test node info for current node
node_info = OTPSupervisor.Distributed.ClusterStateManager.get_node_info(Node.self())
IO.puts("🔍 ClusterStateManager node_info for #{Node.self()}: #{inspect(node_info, pretty: true)}")