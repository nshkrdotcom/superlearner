# Simulate ClusterLive assigns and test status bar metrics
IO.puts("=== Testing ClusterLive status_bar_metrics ===")

# Get actual data
{:ok, params} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopology.validate_params(%{
  "include_processes" => false,
  "include_health" => true
})

{:ok, topology} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopology.execute(params)

{:ok, health_params} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.validate_params(%{
  "include_metrics" => true,
  "include_history" => false
})

{:ok, health} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.execute(health_params)

# Create assigns like ClusterLive would have
assigns = %{
  cluster_topology: topology,
  cluster_health: health,
  current_node: Node.self()
}

IO.puts("🔍 Topology data: #{inspect(topology, pretty: true)}")
IO.puts("\n🔍 Health data: #{inspect(health, pretty: true)}")

# Test the status_bar_metrics logic from ClusterLive
total_nodes = Map.get(topology, :total_nodes, 1)
healthy_nodes = Map.get(health, :nodes_healthy, 1)
current_node = assigns.current_node

IO.puts("\n🔍 Status bar calculation:")
IO.puts("  total_nodes: #{total_nodes}")
IO.puts("  healthy_nodes: #{healthy_nodes}")
IO.puts("  current_node: #{current_node}")

cluster_status = 
  case Map.get(health, :overall_status, :healthy) do
    :healthy -> %{label: "Status", value: "Healthy", color: "text-green-400"}
    :warning -> %{label: "Status", value: "Warning", color: "text-yellow-400"} 
    :degraded -> %{label: "Status", value: "Degraded", color: "text-orange-400"}
    :critical -> %{label: "Status", value: "Critical", color: "text-red-400"}
    _ -> %{label: "Status", value: "Unknown", color: "text-gray-400"}
  end

IO.puts("  cluster_status: #{inspect(cluster_status)}")

# Test the get_cluster_mode_display logic
mode_display = case Map.get(topology, :mode, :single_node) do
  :multi_node -> "Multi-Node"
  :single_node -> "Single Node"
  _ -> "Unknown"
end

IO.puts("  mode_display: #{mode_display}")

metrics = [
  %{label: "Current Node", value: to_string(current_node)},
  %{label: "Total Nodes", value: "#{total_nodes}"},
  %{label: "Healthy", value: "#{healthy_nodes}"},
  cluster_status,
  %{label: "Mode", value: mode_display}
]

IO.puts("\n🔍 Final status bar metrics:")
IO.puts("#{inspect(metrics, pretty: true)}")