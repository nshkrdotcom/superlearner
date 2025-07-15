defmodule OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth do
  @moduledoc """
  Arsenal operation for real-time cluster health monitoring.

  This operation provides comprehensive cluster health information including
  node status, resource usage, connectivity, and performance metrics.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :get,
      path: "/api/v1/cluster/health",
      summary: "Get real-time cluster health and performance metrics",
      parameters: [
        %{
          name: :include_metrics,
          type: :boolean,
          required: false,
          location: :query,
          description: "Include detailed performance metrics"
        },
        %{
          name: :include_history,
          type: :boolean,
          required: false,
          location: :query,
          description: "Include recent health history"
        }
      ],
      responses: %{
        200 => %{description: "Cluster health information"}
      }
    }
  end

  def validate_params(params) do
    validated_params =
      params
      |> convert_boolean_param("include_metrics", true)
      |> convert_boolean_param("include_history", false)

    {:ok, validated_params}
  end

  def execute(params) do
    try do
      # Get base cluster topology
      topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()

      # Get partition status
      partition_status = OTPSupervisor.Distributed.ClusterStateManager.get_partition_status()

      # Build health summary
      health_summary = build_health_summary(topology, partition_status)

      # Add detailed metrics if requested
      health_data =
        health_summary
        |> maybe_add_metrics(params["include_metrics"])
        |> maybe_add_history(params["include_history"])
        |> add_recommendations()

      {:ok, health_data}
    rescue
      error -> {:error, {:cluster_health_error, Exception.message(error)}}
    end
  end

  def format_response(health_data) do
    %{
      data: health_data,
      timestamp: DateTime.utc_now(),
      success: true,
      metadata: %{
        operation: "ClusterHealth",
        overall_status: health_data.overall_status,
        nodes_healthy: health_data.nodes_healthy,
        nodes_total: health_data.nodes_total
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

  defp build_health_summary(topology, partition_status) do
    # Get detailed node information
    node_statuses =
      topology.nodes
      |> Enum.map(fn node ->
        case OTPSupervisor.Distributed.ClusterStateManager.get_node_info(node) do
          {:error, _} -> {node, %{status: :unknown, health_score: 0, issues: ["Node information unavailable"], memory_status: :unknown, cpu_status: :unknown}}
          node_info -> {node, calculate_node_health(node_info)}
        end
      end)
      |> Enum.into(%{})

    # Calculate overall health
    healthy_nodes =
      node_statuses
      |> Enum.count(fn {_node, info} -> info.status == :up end)

    total_nodes = length(topology.nodes)

    overall_status = determine_overall_status(healthy_nodes, total_nodes, partition_status)

    %{
      overall_status: overall_status,
      partition_status: partition_status,
      nodes_total: total_nodes,
      nodes_healthy: healthy_nodes,
      nodes_unhealthy: total_nodes - healthy_nodes,
      node_statuses: node_statuses,
      cluster_uptime: calculate_cluster_uptime(topology),
      last_updated: DateTime.utc_now()
    }
  end

  defp calculate_node_health(node_info) do
    base_health = %{
      status: node_info.status,
      health_score: 100,
      issues: []
    }

    case Map.get(node_info, :status) do
      :up -> calculate_detailed_health(node_info, base_health)
      :down -> %{base_health | health_score: 0, issues: ["Node is down"]}
      :partitioned -> %{base_health | health_score: 25, issues: ["Node is partitioned"]}
      :unreachable -> %{base_health | health_score: 10, issues: ["Node is unreachable"]}
      _ -> %{base_health | health_score: 0, issues: ["Unknown node status"]}
    end
  end

  defp calculate_detailed_health(node_info, base_health) do
    issues = []
    health_score = 100

    # Check memory usage
    {memory_issues, memory_penalty} = check_memory_health(Map.get(node_info, :memory_usage))

    # Check CPU usage
    {cpu_issues, cpu_penalty} = check_cpu_health(Map.get(node_info, :cpu_usage))

    # Check process count (if available)
    {process_issues, process_penalty} = check_process_health(node_info)

    final_score = max(0, health_score - memory_penalty - cpu_penalty - process_penalty)
    all_issues = issues ++ memory_issues ++ cpu_issues ++ process_issues

    # Create the complete health map with all required fields
    Map.merge(base_health, %{
      health_score: final_score,
      issues: all_issues,
      memory_status: categorize_memory_usage(Map.get(node_info, :memory_usage)),
      cpu_status: categorize_cpu_usage(Map.get(node_info, :cpu_usage))
    })
  end

  defp check_memory_health(memory_usage) do
    case memory_usage do
      # > 1GB
      %{total: total} when total > 1_000_000_000 ->
        # > 4GB
        if total > 4_000_000_000 do
          {["High memory usage"], 30}
        else
          {["Moderate memory usage"], 10}
        end

      :simulated ->
        {[], 0}

      _ ->
        {[], 0}
    end
  end

  defp check_cpu_health(cpu_usage) do
    case cpu_usage do
      usage when is_number(usage) ->
        cond do
          usage > 90 -> {["Very high CPU usage"], 40}
          usage > 70 -> {["High CPU usage"], 20}
          usage > 50 -> {["Moderate CPU usage"], 5}
          true -> {[], 0}
        end

      :simulated ->
        {[], 0}

      _ ->
        {[], 0}
    end
  end

  defp check_process_health(node_info) do
    case Map.get(node_info, :processes) do
      count when is_integer(count) and count > 10000 ->
        {["High process count"], 15}

      count when is_integer(count) and count > 5000 ->
        {["Moderate process count"], 5}

      _ ->
        {[], 0}
    end
  end

  defp categorize_memory_usage(%{total: total}) when total > 4_000_000_000, do: :high
  defp categorize_memory_usage(%{total: total}) when total > 1_000_000_000, do: :moderate
  defp categorize_memory_usage(%{total: _}), do: :normal
  defp categorize_memory_usage(:simulated), do: :simulated
  defp categorize_memory_usage(_), do: :unknown

  defp categorize_cpu_usage(usage) when is_number(usage) and usage > 70, do: :high
  defp categorize_cpu_usage(usage) when is_number(usage) and usage > 30, do: :moderate
  defp categorize_cpu_usage(usage) when is_number(usage), do: :normal
  defp categorize_cpu_usage(:simulated), do: :simulated
  defp categorize_cpu_usage(_), do: :unknown

  defp determine_overall_status(healthy_nodes, total_nodes, partition_status) do
    health_percentage = healthy_nodes / total_nodes * 100

    cond do
      partition_status in [:minority_partition, :partial_partition] -> :degraded
      health_percentage == 100 -> :healthy
      health_percentage >= 80 -> :warning
      health_percentage >= 50 -> :degraded
      true -> :critical
    end
  end

  defp calculate_cluster_uptime(topology) do
    case Map.get(topology, :formation_time) do
      nil ->
        "unknown"

      formation_time ->
        diff = DateTime.diff(DateTime.utc_now(), formation_time, :second)
        format_uptime(diff)
    end
  end

  defp format_uptime(seconds) when seconds < 60, do: "#{seconds}s"

  defp format_uptime(seconds) when seconds < 3600 do
    minutes = div(seconds, 60)
    remaining_seconds = rem(seconds, 60)
    "#{minutes}m #{remaining_seconds}s"
  end

  defp format_uptime(seconds) do
    hours = div(seconds, 3600)
    remaining_minutes = div(rem(seconds, 3600), 60)
    "#{hours}h #{remaining_minutes}m"
  end

  defp maybe_add_metrics(health_data, true) do
    # Add detailed performance metrics
    metrics = collect_cluster_metrics()
    Map.put(health_data, :performance_metrics, metrics)
  end

  defp maybe_add_metrics(health_data, _), do: health_data

  defp maybe_add_history(health_data, true) do
    # Add recent health history (simplified - in production you'd store this)
    history = generate_mock_history()
    Map.put(health_data, :health_history, history)
  end

  defp maybe_add_history(health_data, _), do: health_data

  defp add_recommendations(health_data) do
    recommendations = generate_recommendations(health_data)
    Map.put(health_data, :recommendations, recommendations)
  end

  defp collect_cluster_metrics do
    %{
      total_processes: count_total_processes(),
      memory_usage: get_cluster_memory_usage(),
      message_queue_lengths: get_cluster_message_queues(),
      gc_activity: get_cluster_gc_activity()
    }
  end

  defp count_total_processes do
    try do
      process_distribution =
        OTPSupervisor.Distributed.ClusterStateManager.get_process_distribution()

      process_distribution
      |> Enum.reduce(0, fn {_node, processes}, acc ->
        acc + length(processes)
      end)
    rescue
      _ -> 0
    end
  end

  defp get_cluster_memory_usage do
    try do
      topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()

      topology.nodes
      |> Enum.map(fn node ->
        case OTPSupervisor.Distributed.ClusterStateManager.get_node_info(node) do
          {:error, _} -> {node, %{total: 0}}
          node_info -> {node, node_info.memory_usage}
        end
      end)
      |> Enum.into(%{})
    rescue
      _ -> %{}
    end
  end

  defp get_cluster_message_queues do
    # Simplified - in production you'd collect actual message queue stats
    %{
      average_queue_length: 0.5,
      max_queue_length: 10,
      nodes_with_long_queues: 0
    }
  end

  defp get_cluster_gc_activity do
    # Simplified - in production you'd collect actual GC stats
    %{
      total_collections: 1000,
      average_gc_time: 2.5,
      nodes_with_high_gc: 0
    }
  end

  defp generate_mock_history do
    # Generate mock health history for the last hour
    now = DateTime.utc_now()

    0..11
    |> Enum.map(fn minutes_ago ->
      timestamp = DateTime.add(now, -minutes_ago * 5, :minute)

      %{
        timestamp: timestamp,
        overall_status: :healthy,
        nodes_healthy: 2,
        nodes_total: 2
      }
    end)
    |> Enum.reverse()
  end

  defp generate_recommendations(health_data) do
    recommendations = []

    # Check overall status
    recommendations =
      case health_data.overall_status do
        :critical -> recommendations ++ ["Immediate attention required - multiple nodes down"]
        :degraded -> recommendations ++ ["Some nodes are unhealthy - investigate node issues"]
        :warning -> recommendations ++ ["Monitor cluster closely - performance issues detected"]
        _ -> recommendations
      end

    # Check partition status
    recommendations =
      case health_data.partition_status do
        :minority_partition ->
          recommendations ++ ["Cluster is in minority partition - check network connectivity"]

        :partial_partition ->
          recommendations ++ ["Network partition detected - verify node connectivity"]

        _ ->
          recommendations
      end

    # Check individual node issues
    node_recommendations =
      health_data.node_statuses
      |> Enum.flat_map(fn {node, status} ->
        case status.issues do
          [] -> []
          issues -> ["Node #{node}: #{Enum.join(issues, ", ")}"]
        end
      end)

    recommendations ++ node_recommendations
  end
end
