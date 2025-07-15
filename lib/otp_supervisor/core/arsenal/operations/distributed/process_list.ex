defmodule OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList do
  @moduledoc """
  Arsenal operation to list all processes across the cluster with node information.

  This operation provides comprehensive process listing across all cluster nodes,
  with filtering capabilities and detailed process information.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :get,
      path: "/api/v1/cluster/processes",
      summary: "List all processes across the cluster with node information",
      parameters: [
        %{
          name: :node,
          type: :string,
          required: false,
          location: :query,
          description: "Filter processes by specific node"
        },
        %{
          name: :type,
          type: :string,
          required: false,
          location: :query,
          description: "Filter processes by type (supervisor, gen_server, etc.)"
        },
        %{
          name: :application,
          type: :string,
          required: false,
          location: :query,
          description: "Filter processes by application"
        },
        %{
          name: :limit,
          type: :integer,
          required: false,
          location: :query,
          description: "Maximum number of processes to return (default: 100)"
        },
        %{
          name: :include_details,
          type: :boolean,
          required: false,
          location: :query,
          description: "Include detailed process information"
        }
      ],
      responses: %{
        200 => %{description: "List of processes across the cluster"}
      }
    }
  end

  def validate_params(params) do
    validated_params =
      params
      |> validate_node_param()
      |> validate_type_param()
      |> validate_limit_param()
      |> convert_boolean_param("include_details", false)

    {:ok, validated_params}
  end

  def execute(params) do
    try do
      # Get cluster topology to know which nodes to query
      topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()

      # Determine which nodes to query
      target_nodes =
        case params["node"] do
          nil ->
            topology.nodes

          node_str when is_binary(node_str) ->
            try do
              node = String.to_atom(node_str)
              if node in topology.nodes, do: [node], else: []
            rescue
              _ -> []
            end

          _ ->
            []
        end

      if target_nodes == [] do
        {:error, :node_not_found}
      else
        # Collect processes from target nodes
        processes = collect_processes_from_nodes(target_nodes, params)

        # Apply filters
        filtered_processes =
          processes
          |> apply_type_filter(params["type"])
          |> apply_application_filter(params["application"])
          |> apply_limit(params["limit"])

        result = %{
          processes: filtered_processes,
          total_count: length(filtered_processes),
          nodes_queried: target_nodes,
          filters_applied: build_filters_summary(params)
        }

        {:ok, result}
      end
    rescue
      error -> {:error, {:process_list_error, Exception.message(error)}}
    end
  end

  def format_response(result) do
    %{
      data: result,
      timestamp: DateTime.utc_now(),
      success: true,
      metadata: %{
        operation: "DistributedProcessList",
        nodes_queried: length(result.nodes_queried),
        total_processes: result.total_count
      }
    }
  end

  # Private helper functions

  defp validate_node_param(params) do
    case params["node"] do
      nil ->
        params

      node_str when is_binary(node_str) ->
        # Validate node name format
        if String.contains?(node_str, "@") do
          params
        else
          # Invalid format, ignore
          Map.delete(params, "node")
        end

      _ ->
        Map.delete(params, "node")
    end
  end

  defp validate_type_param(params) do
    case params["type"] do
      nil -> params
      type when type in ["supervisor", "gen_server", "gen_event", "task", "process"] -> params
      # Invalid type, ignore
      _ -> Map.delete(params, "type")
    end
  end

  defp validate_limit_param(params) do
    case params["limit"] do
      # Default limit
      nil ->
        Map.put(params, "limit", 100)

      limit_str when is_binary(limit_str) ->
        case Integer.parse(limit_str) do
          {limit, ""} when limit > 0 and limit <= 1000 ->
            Map.put(params, "limit", limit)

          _ ->
            # Invalid, use default
            Map.put(params, "limit", 100)
        end

      limit when is_integer(limit) and limit > 0 and limit <= 1000 ->
        Map.put(params, "limit", limit)

      _ ->
        # Invalid, use default
        Map.put(params, "limit", 100)
    end
  end

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

  defp collect_processes_from_nodes(nodes, params) do
    include_details = params["include_details"]

    nodes
    |> Enum.flat_map(fn node ->
      collect_processes_from_node(node, include_details)
    end)
  end

  defp collect_processes_from_node(node, include_details) do
    if node == Node.self() do
      # Local node - get processes directly
      Process.list()
      |> Enum.map(fn pid ->
        build_process_info(pid, node, include_details)
      end)
      |> Enum.filter(&(&1 != nil))
    else
      # Remote node - use RPC or simulation
      case get_remote_processes(node, include_details) do
        {:ok, processes} -> processes
        {:error, _} -> []
      end
    end
  end

  defp get_remote_processes(node, include_details) do
    # Check if we're in simulation mode
    simulation_enabled =
      try do
        OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
      rescue
        _ -> false
      end

    if simulation_enabled do
      get_simulated_processes(node, include_details)
    else
      get_real_remote_processes(node, include_details)
    end
  end

  defp get_simulated_processes(node, include_details) do
    try do
      topology = OTPSupervisor.Distributed.SingleNodeSimulator.get_simulated_topology()

      case Map.get(topology.process_distribution, node) do
        nil ->
          {:error, :node_not_found}

        processes ->
          process_infos =
            processes
            |> Enum.map(fn pid ->
              build_process_info(pid, node, include_details)
            end)
            |> Enum.filter(&(&1 != nil))

          {:ok, process_infos}
      end
    rescue
      error -> {:error, error}
    end
  end

  defp get_real_remote_processes(node, include_details) do
    try do
      case :rpc.call(node, Process, :list, [], 10000) do
        {:badrpc, reason} ->
          {:error, reason}

        processes when is_list(processes) ->
          process_infos =
            processes
            |> Enum.map(fn pid ->
              build_remote_process_info(pid, node, include_details)
            end)
            |> Enum.filter(&(&1 != nil))

          {:ok, process_infos}

        _ ->
          {:error, :invalid_response}
      end
    rescue
      error -> {:error, error}
    end
  end

  defp build_process_info(pid, node, include_details) do
    if Process.alive?(pid) do
      base_info = %{
        pid: inspect(pid),
        node: node,
        alive: true
      }

      if include_details do
        case Process.info(pid, [
               :registered_name,
               :initial_call,
               :current_function,
               :memory,
               :message_queue_len
             ]) do
          nil ->
            base_info

          info ->
            Map.merge(base_info, %{
              registered_name: info[:registered_name],
              initial_call: format_mfa(info[:initial_call]),
              current_function: format_mfa(info[:current_function]),
              memory: info[:memory],
              message_queue_len: info[:message_queue_len],
              type: determine_process_type(info),
              application: determine_application(info)
            })
        end
      else
        base_info
      end
    else
      nil
    end
  end

  defp build_remote_process_info(pid, node, include_details) do
    base_info = %{
      pid: inspect(pid),
      node: node,
      # Assume alive since we got it from remote list
      alive: true
    }

    if include_details do
      # Try to get remote process info
      case :rpc.call(
             node,
             Process,
             :info,
             [pid, [:registered_name, :initial_call, :memory]],
             5000
           ) do
        {:badrpc, _} ->
          base_info

        nil ->
          base_info

        info ->
          Map.merge(base_info, %{
            registered_name: info[:registered_name],
            initial_call: format_mfa(info[:initial_call]),
            memory: info[:memory],
            type: determine_process_type(info),
            application: determine_application(info)
          })
      end
    else
      base_info
    end
  end

  defp format_mfa(nil), do: nil
  defp format_mfa({module, function, arity}), do: "#{module}.#{function}/#{arity}"
  defp format_mfa(other), do: inspect(other)

  defp determine_process_type(info) do
    case info[:initial_call] do
      {:supervisor, _, _} -> "supervisor"
      {:gen_server, _, _} -> "gen_server"
      {:gen_event, _, _} -> "gen_event"
      {Task, _, _} -> "task"
      _ -> "process"
    end
  end

  defp determine_application(info) do
    case info[:registered_name] do
      nil ->
        "unknown"
      
      [] ->
        "unknown"

      name when is_atom(name) ->
        name_str = Atom.to_string(name)

        cond do
          String.starts_with?(name_str, "Elixir.OTPSupervisor") -> "otp_supervisor"
          String.starts_with?(name_str, "Elixir.Phoenix") -> "phoenix"
          String.starts_with?(name_str, "Elixir.") -> "application"
          true -> "erlang"
        end
        
      _ ->
        "unknown"
    end
  end

  defp apply_type_filter(processes, nil), do: processes

  defp apply_type_filter(processes, type) do
    Enum.filter(processes, fn process ->
      Map.get(process, :type) == type
    end)
  end

  defp apply_application_filter(processes, nil), do: processes

  defp apply_application_filter(processes, application) do
    Enum.filter(processes, fn process ->
      Map.get(process, :application) == application
    end)
  end

  defp apply_limit(processes, limit) do
    Enum.take(processes, limit)
  end

  defp build_filters_summary(params) do
    %{
      node: params["node"],
      type: params["type"],
      application: params["application"],
      limit: params["limit"],
      include_details: params["include_details"]
    }
    |> Enum.filter(fn {_k, v} -> v != nil end)
    |> Enum.into(%{})
  end
end
