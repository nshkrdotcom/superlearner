defmodule OTPSupervisor.Core.Arsenal.Operations.Distributed.NodeInfo do
  @moduledoc """
  Arsenal operation for detailed node inspection and information.

  This operation provides comprehensive information about a specific node
  including processes, resource usage, and health metrics.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :get,
      path: "/api/v1/cluster/nodes/:node/info",
      summary: "Get detailed information about a specific cluster node",
      parameters: [
        %{
          name: :node,
          type: :string,
          required: true,
          location: :path,
          description: "Node name to inspect"
        },
        %{
          name: :include_processes,
          type: :boolean,
          required: false,
          location: :query,
          description: "Include detailed process information"
        },
        %{
          name: :process_limit,
          type: :integer,
          required: false,
          location: :query,
          description: "Maximum number of processes to return (default: 50)"
        }
      ],
      responses: %{
        200 => %{description: "Detailed node information"},
        404 => %{description: "Node not found"}
      }
    }
  end

  def validate_params(params) do
    # Validate node parameter
    case params["node"] do
      nil ->
        {:error, {:missing_parameter, :node}}

      node_str when is_binary(node_str) ->
        if String.contains?(node_str, "@") do
          try do
            node_atom = String.to_atom(node_str)

            validated_params =
              params
              |> Map.put("node", node_atom)
              |> convert_boolean_param("include_processes", false)
              |> validate_process_limit()

            {:ok, validated_params}
          rescue
            _ -> {:error, {:invalid_parameter, :node, "Invalid node name format"}}
          end
        else
          {:error,
           {:invalid_parameter, :node, "Node name must include hostname (e.g., node@host)"}}
        end

      _ ->
        {:error, {:invalid_parameter, :node, "Node name must be a string"}}
    end
  end

  def execute(params) do
    node = params["node"]

    try do
      # Check if node exists in cluster
      topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()

      if node in topology.nodes do
        # Get detailed node information
        node_info = get_detailed_node_info(node, params)
        {:ok, node_info}
      else
        {:error, :node_not_found}
      end
    rescue
      error -> {:error, {:node_info_error, Exception.message(error)}}
    end
  end

  def format_response(node_info) do
    %{
      data: node_info,
      timestamp: DateTime.utc_now(),
      success: true,
      metadata: %{
        operation: "NodeInfo",
        node: node_info.name,
        status: node_info.status
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

  defp validate_process_limit(params) do
    case params["process_limit"] do
      nil ->
        Map.put(params, "process_limit", 50)

      limit_str when is_binary(limit_str) ->
        case Integer.parse(limit_str) do
          {limit, ""} when limit > 0 and limit <= 500 ->
            Map.put(params, "process_limit", limit)

          _ ->
            Map.put(params, "process_limit", 50)
        end

      limit when is_integer(limit) and limit > 0 and limit <= 500 ->
        Map.put(params, "process_limit", limit)

      _ ->
        Map.put(params, "process_limit", 50)
    end
  end

  defp get_detailed_node_info(node, params) do
    # Get base node info from ClusterStateManager
    base_info =
      case OTPSupervisor.Distributed.ClusterStateManager.get_node_info(node) do
        {:error, reason} -> %{name: node, status: :error, error: reason}
        info -> info
      end

    # Enhance with additional information
    enhanced_info =
      base_info
      |> add_system_info(node)
      |> add_application_info(node)
      |> add_network_info(node)
      |> maybe_add_process_details(node, params)
      |> add_performance_metrics(node)

    enhanced_info
  end

  defp add_system_info(node_info, node) do
    system_info = get_system_info(node)
    Map.put(node_info, :system_info, system_info)
  end

  defp add_application_info(node_info, node) do
    applications = get_application_info(node)
    Map.put(node_info, :applications, applications)
  end

  defp add_network_info(node_info, node) do
    network_info = get_network_info(node)
    Map.put(node_info, :network_info, network_info)
  end

  defp maybe_add_process_details(node_info, node, %{
         "include_processes" => true,
         "process_limit" => limit
       }) do
    processes = get_process_details(node, limit)
    Map.put(node_info, :processes, processes)
  end

  defp maybe_add_process_details(node_info, _node, _params), do: node_info

  defp add_performance_metrics(node_info, node) do
    metrics = get_performance_metrics(node)
    Map.put(node_info, :performance_metrics, metrics)
  end

  defp get_system_info(node) do
    if node == Node.self() do
      # Local node - get detailed system info
      %{
        otp_release: :erlang.system_info(:otp_release),
        erts_version: :erlang.system_info(:version),
        elixir_version: System.version(),
        system_architecture: :erlang.system_info(:system_architecture),
        schedulers: :erlang.system_info(:schedulers),
        logical_processors: :erlang.system_info(:logical_processors),
        uptime: get_node_uptime()
      }
    else
      # Remote node - try RPC
      get_remote_system_info(node)
    end
  end

  defp get_remote_system_info(node) do
    # Check if simulation mode
    simulation_enabled =
      try do
        OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
      rescue
        _ -> false
      end

    if simulation_enabled do
      # Return simulated system info
      %{
        otp_release: "simulated",
        erts_version: "simulated",
        elixir_version: "simulated",
        system_architecture: "simulated",
        schedulers: 4,
        logical_processors: 4,
        uptime: "simulated"
      }
    else
      # Try real RPC
      try do
        case :rpc.call(node, :erlang, :system_info, [:otp_release], 5000) do
          {:badrpc, reason} ->
            %{error: reason}

          otp_release ->
            %{
              otp_release: otp_release,
              erts_version: safe_rpc(node, :erlang, :system_info, [:version]),
              schedulers: safe_rpc(node, :erlang, :system_info, [:schedulers]),
              logical_processors: safe_rpc(node, :erlang, :system_info, [:logical_processors])
            }
        end
      rescue
        _ -> %{error: "Failed to get system info"}
      end
    end
  end

  defp get_application_info(node) do
    if node == Node.self() do
      # Local node
      Application.loaded_applications()
      |> Enum.map(fn {app, description, version} ->
        %{
          name: app,
          description: to_string(description),
          version: to_string(version),
          status: Application.get_application(app)
        }
      end)
      # Limit to prevent huge responses
      |> Enum.take(20)
    else
      # Remote node
      get_remote_application_info(node)
    end
  end

  defp get_remote_application_info(node) do
    simulation_enabled =
      try do
        OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
      rescue
        _ -> false
      end

    if simulation_enabled do
      # Return simulated applications
      [
        %{name: :kernel, description: "ERTS CXC 138 10", version: "simulated", status: :ok},
        %{name: :stdlib, description: "ERTS CXC 138 10", version: "simulated", status: :ok},
        %{
          name: :otp_supervisor,
          description: "OTP Supervisor Platform",
          version: "simulated",
          status: :ok
        }
      ]
    else
      case safe_rpc(node, Application, :loaded_applications, []) do
        {:error, _} ->
          []

        applications when is_list(applications) ->
          applications
          |> Enum.map(fn {app, description, version} ->
            %{
              name: app,
              description: to_string(description),
              version: to_string(version)
            }
          end)
          |> Enum.take(20)

        _ ->
          []
      end
    end
  end

  defp get_network_info(node) do
    %{
      node_name: node,
      is_alive: Node.alive?(),
      connected_to_current: node in Node.list(),
      cookie_hash: get_cookie_hash(),
      distribution_port: get_distribution_port(node)
    }
  end

  defp get_process_details(node, limit) do
    if node == Node.self() do
      # Local processes
      Process.list()
      |> Enum.take(limit)
      |> Enum.map(&build_detailed_process_info/1)
      |> Enum.filter(&(&1 != nil))
    else
      # Remote processes
      get_remote_process_details(node, limit)
    end
  end

  defp get_remote_process_details(node, limit) do
    simulation_enabled =
      try do
        OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
      rescue
        _ -> false
      end

    if simulation_enabled do
      # Get simulated processes
      try do
        topology = OTPSupervisor.Distributed.SingleNodeSimulator.get_simulated_topology()

        case Map.get(topology.process_distribution, node) do
          nil ->
            []

          processes ->
            processes
            |> Enum.take(limit)
            |> Enum.map(&build_detailed_process_info/1)
            |> Enum.filter(&(&1 != nil))
        end
      rescue
        _ -> []
      end
    else
      # Real remote processes
      case safe_rpc(node, Process, :list, []) do
        {:error, _} ->
          []

        processes when is_list(processes) ->
          processes
          |> Enum.take(limit)
          |> Enum.map(fn pid ->
            build_remote_process_info(pid, node)
          end)
          |> Enum.filter(&(&1 != nil))

        _ ->
          []
      end
    end
  end

  defp build_detailed_process_info(pid) do
    if Process.alive?(pid) do
      case Process.info(pid, [
             :registered_name,
             :initial_call,
             :current_function,
             :memory,
             :message_queue_len,
             :links,
             :monitors
           ]) do
        nil ->
          nil

        info ->
          %{
            pid: inspect(pid),
            registered_name: info[:registered_name],
            initial_call: format_mfa(info[:initial_call]),
            current_function: format_mfa(info[:current_function]),
            memory: info[:memory],
            message_queue_len: info[:message_queue_len],
            links_count: length(info[:links] || []),
            monitors_count: length(info[:monitors] || [])
          }
      end
    else
      nil
    end
  end

  defp build_remote_process_info(pid, node) do
    case safe_rpc(node, Process, :info, [pid, [:registered_name, :initial_call, :memory]]) do
      {:error, _} ->
        nil

      nil ->
        nil

      info ->
        %{
          pid: inspect(pid),
          registered_name: info[:registered_name],
          initial_call: format_mfa(info[:initial_call]),
          memory: info[:memory]
        }
    end
  end

  defp get_performance_metrics(node) do
    if node == Node.self() do
      %{
        reductions: format_statistics(:erlang.statistics(:reductions)),
        run_queue: :erlang.statistics(:run_queue),
        io: format_statistics(:erlang.statistics(:io)),
        garbage_collection: format_statistics(:erlang.statistics(:garbage_collection))
      }
    else
      get_remote_performance_metrics(node)
    end
  end

  defp get_remote_performance_metrics(node) do
    simulation_enabled =
      try do
        OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
      rescue
        _ -> false
      end

    if simulation_enabled do
      %{
        reductions: {1_000_000, 50000},
        run_queue: 0,
        io: {{1000, 2000}, {500, 1500}},
        garbage_collection: {100, 50}
      }
    else
      %{
        reductions: safe_rpc(node, :erlang, :statistics, [:reductions]),
        run_queue: safe_rpc(node, :erlang, :statistics, [:run_queue]),
        io: safe_rpc(node, :erlang, :statistics, [:io]),
        garbage_collection: safe_rpc(node, :erlang, :statistics, [:garbage_collection])
      }
    end
  end

  # Helper functions

  defp safe_rpc(node, module, function, args) do
    try do
      case :rpc.call(node, module, function, args, 5000) do
        {:badrpc, reason} -> {:error, reason}
        result -> result
      end
    rescue
      _ -> {:error, :rpc_failed}
    end
  end

  defp format_mfa(nil), do: nil
  defp format_mfa({module, function, arity}), do: "#{module}.#{function}/#{arity}"
  defp format_mfa(other), do: inspect(other)

  defp get_node_uptime do
    {uptime_ms, _} = :erlang.statistics(:wall_clock)
    seconds = div(uptime_ms, 1000)
    format_uptime(seconds)
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

  defp get_cookie_hash do
    # Return a hash of the cookie for security
    cookie = Node.get_cookie()

    :crypto.hash(:sha256, Atom.to_string(cookie))
    |> Base.encode16()
    |> String.slice(0, 8)
  end

  defp get_distribution_port(_node) do
    # Try to get the distribution port
    case :net_kernel.get_net_ticktime() do
      {:ongoing_change_to, _} -> "changing"
      seconds when is_integer(seconds) -> "active"
      _ -> "unknown"
    end
  end

  defp format_statistics(stat) do
    case stat do
      {a, b} when is_integer(a) and is_integer(b) ->
        %{total: a, since_last: b}

      {a, b, c} when is_integer(a) and is_integer(b) and is_integer(c) ->
        %{a: a, b: b, c: c}

      {{:input, input_bytes}, {:output, output_bytes}}
      when is_integer(input_bytes) and is_integer(output_bytes) ->
        %{input: input_bytes, output: output_bytes}

      other ->
        inspect(other)
    end
  end
end
