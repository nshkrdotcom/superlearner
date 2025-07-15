defmodule OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopology do
  @moduledoc """
  Arsenal operation to get real-time cluster topology and node information.

  This operation provides comprehensive cluster state including node health,
  connectivity, and process distribution across the cluster.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :get,
      path: "/api/v1/cluster/topology",
      summary: "Get real-time cluster topology and node information",
      parameters: [
        %{
          name: :include_processes,
          type: :boolean,
          required: false,
          location: :query,
          description: "Include process distribution information"
        },
        %{
          name: :include_health,
          type: :boolean,
          required: false,
          location: :query,
          description: "Include detailed node health metrics"
        }
      ],
      responses: %{
        200 => %{description: "Cluster topology information"}
      }
    }
  end

  def validate_params(params) do
    # Convert string boolean parameters
    validated_params =
      params
      |> convert_boolean_param("include_processes", false)
      |> convert_boolean_param("include_health", true)

    {:ok, validated_params}
  end

  def execute(params) do
    try do
      # Get base topology from ClusterStateManager
      topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()

      # Enhance with additional information based on parameters
      enhanced_topology =
        topology
        |> maybe_add_process_distribution(params["include_processes"])
        |> maybe_add_health_info(params["include_health"])
        |> add_cluster_metadata()

      {:ok, enhanced_topology}
    rescue
      error -> {:error, {:cluster_topology_error, Exception.message(error)}}
    end
  end

  def format_response(topology) do
    %{
      data: topology,
      timestamp: DateTime.utc_now(),
      success: true,
      metadata: %{
        operation: "ClusterTopology",
        cluster_size: topology.total_nodes,
        simulation_mode: Map.get(topology, :simulation_mode, false)
      }
    }
  end

  # Private helper functions

  defp convert_boolean_param(params, key, default) do
    case Map.get(params, key) do
      nil -> Map.put(params, key, default)
      "true" -> Map.put(params, key, true)
      "false" -> Map.put(params, key, false)
      true -> Map.put(params, key, true)
      false -> Map.put(params, key, false)
      _ -> Map.put(params, key, default)
    end
  end

  defp maybe_add_process_distribution(topology, true) do
    try do
      process_distribution =
        OTPSupervisor.Distributed.ClusterStateManager.get_process_distribution()

      # Convert PIDs to strings for JSON serialization
      serializable_distribution =
        process_distribution
        |> Enum.map(fn {node, processes} ->
          {node,
           %{
             count: length(processes),
             sample_pids: processes |> Enum.take(5) |> Enum.map(&inspect/1)
           }}
        end)
        |> Enum.into(%{})

      Map.put(topology, :process_distribution, serializable_distribution)
    rescue
      _ -> topology
    end
  end

  defp maybe_add_process_distribution(topology, _), do: topology

  defp maybe_add_health_info(topology, true) do
    try do
      # Get detailed node information
      node_health =
        topology.nodes
        |> Enum.map(fn node ->
          case OTPSupervisor.Distributed.ClusterStateManager.get_node_info(node) do
            {:error, _} -> {node, %{status: :unknown}}
            node_info -> {node, sanitize_node_info(node_info)}
          end
        end)
        |> Enum.into(%{})

      # Get partition status
      partition_status = OTPSupervisor.Distributed.ClusterStateManager.get_partition_status()

      topology
      |> Map.put(:node_health, node_health)
      |> Map.put(:partition_status, partition_status)
    rescue
      _ -> topology
    end
  end

  defp maybe_add_health_info(topology, _), do: topology

  defp add_cluster_metadata(topology) do
    # Get current mode from ToolManager
    mode =
      try do
        OTPSupervisor.Distributed.ToolManager.get_mode()
      rescue
        _ -> :unknown
      end

    # Check if simulation is enabled
    simulation_enabled =
      try do
        OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
      rescue
        _ -> false
      end

    topology
    |> Map.put(:mode, mode)
    |> Map.put(:simulation_enabled, simulation_enabled)
    |> Map.put(:collected_at, DateTime.utc_now())
  end

  defp sanitize_node_info(node_info) do
    # Remove or sanitize fields that might not serialize well
    node_info
    # Remove process list, keep count
    |> Map.drop([:processes])
    |> Map.update(:memory_usage, %{}, fn
      %{} = memory -> memory
      _ -> %{total: 0, processes: 0}
    end)
    |> Map.update(:cpu_usage, 0.0, fn
      usage when is_number(usage) -> usage
      _ -> 0.0
    end)
  end
end
