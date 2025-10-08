defmodule OtpSupervisorWeb.Live.ClusterVisualization.DataLoader do
  @moduledoc """
  Handles data loading and processing for cluster visualization.
  """

  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterSupervisionTrees
  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList

  def load_cluster_data(socket) do
    socket =
      Phoenix.Component.assign(socket, %{
        loading: true,
        error_message: nil
      })

    case fetch_cluster_data() do
      {:ok, unified_data} ->
        socket
        |> Phoenix.Component.assign(%{
          cluster_data: unified_data.cluster_nodes,
          total_nodes: unified_data.total_nodes,
          total_processes: unified_data.total_processes,
          loading: false,
          last_updated: DateTime.utc_now()
        })
        |> OtpSupervisorWeb.Live.ClusterVisualization.Filters.apply_filters_and_search()

      {:error, error_message} ->
        require Logger
        Logger.error("Failed to load cluster data: #{error_message}")

        Phoenix.Component.assign(socket, %{
          cluster_data: %{},
          filtered_cluster_data: %{},
          total_nodes: 0,
          total_processes: 0,
          filtered_process_count: 0,
          loading: false,
          error_message: error_message
        })
    end
  end

  defp fetch_cluster_data do
    try do
      supervision_params = %{
        "include_children" => true,
        "include_process_details" => true
      }

      process_params = %{
        "include_details" => true,
        "limit" => 5000
      }

      with {:ok, supervision_result} <- ClusterSupervisionTrees.execute(supervision_params),
           {:ok, process_result} <- ProcessList.execute(process_params) do
        unified_data = create_unified_data_structure(supervision_result, process_result)
        {:ok, unified_data}
      else
        {:error, reason} -> {:error, format_error(reason)}
      end
    rescue
      error -> {:error, "Failed to fetch cluster data: #{inspect(error)}"}
    end
  end

  defp create_unified_data_structure(supervision_result, process_result) do
    supervision_trees = supervision_result.supervision_trees
    processes_by_node = group_processes_by_node(process_result.processes)

    cluster_nodes =
      supervision_trees
      |> Enum.map(fn {node, node_data} ->
        all_processes = Map.get(processes_by_node, node, [])
        processes_by_app = group_processes_by_application(all_processes)

        unified_node_data = %{
          name: to_string(node),
          type: :cluster_node,
          node_info: %{
            total_processes: length(all_processes),
            applications: Map.keys(processes_by_app),
            memory_usage: calculate_node_memory(all_processes)
          },
          children:
            build_enhanced_node_structure(node_data.supervisors, processes_by_app, all_processes)
        }

        {node, unified_node_data}
      end)
      |> Enum.into(%{})

    total_nodes = map_size(cluster_nodes)
    total_processes = calculate_total_processes_enhanced(cluster_nodes)

    %{
      cluster_nodes: cluster_nodes,
      total_nodes: total_nodes,
      total_processes: total_processes
    }
  end

  defp group_processes_by_node(processes) do
    processes
    |> Enum.group_by(fn process -> process.node end)
  end

  defp group_processes_by_application(processes) do
    processes
    |> Enum.group_by(fn process ->
      Map.get(process, :application, :unknown)
    end)
  end

  defp calculate_node_memory(processes) do
    processes
    |> Enum.reduce(0, fn process, acc ->
      acc + Map.get(process, :memory, 0)
    end)
  end

  defp build_enhanced_node_structure(supervisors, processes_by_app, all_processes) do
    application_containers = create_application_containers(processes_by_app)
    supervision_trees = build_supervision_trees(supervisors, all_processes)
    application_containers ++ supervision_trees
  end

  defp create_application_containers(processes_by_app) do
    processes_by_app
    |> Enum.map(fn {app_name, processes} ->
      %{
        name: to_string(app_name),
        type: :application,
        application: app_name,
        process_count: length(processes),
        memory_usage: calculate_node_memory(processes),
        children: create_application_processes(processes)
      }
    end)
  end

  defp create_application_processes(processes) do
    processes
    |> Enum.map(fn process ->
      %{
        name: Map.get(process, :registered_name) || process.pid,
        pid: process.pid,
        type: determine_process_type(process),
        behavior: determine_otp_behavior(process),
        alive: Map.get(process, :alive, true),
        node: Map.get(process, :node),
        application: Map.get(process, :application, :unknown),
        memory: Map.get(process, :memory, 0),
        message_queue_len: Map.get(process, :message_queue_len, 0),
        status: Map.get(process, :status, :running),
        links: Map.get(process, :links, []),
        monitors: Map.get(process, :monitors, [])
      }
    end)
  end

  defp build_supervision_trees(supervisors, all_processes) do
    supervisors
    |> Enum.map(fn supervisor ->
      build_supervisor_tree(supervisor, all_processes)
    end)
  end

  defp build_supervisor_tree(supervisor, all_processes) do
    base_supervisor = %{
      name: supervisor.name,
      pid: supervisor.pid,
      type: :supervisor,
      behavior: :supervisor,
      strategy: Map.get(supervisor, :strategy, :one_for_one),
      alive: supervisor.alive,
      level: supervisor.level,
      node: supervisor.node,
      application: supervisor.application,
      restart_intensity: Map.get(supervisor, :restart_intensity, 1),
      restart_period: Map.get(supervisor, :restart_period, 5),
      child_count: length(Map.get(supervisor, :children, []))
    }

    case Map.get(supervisor, :children) do
      nil ->
        base_supervisor

      children ->
        enhanced_children =
          children
          |> Enum.map(fn child ->
            if child.type == :supervisor do
              build_supervisor_tree(child, all_processes)
            else
              enhance_worker_process(child, all_processes)
            end
          end)

        Map.put(base_supervisor, :children, enhanced_children)
    end
  end

  defp enhance_worker_process(worker, all_processes) do
    process_details =
      Enum.find(all_processes, fn p ->
        p.pid == worker.pid
      end)

    base_worker = %{
      name: worker.name,
      pid: worker.pid,
      type: :worker,
      behavior: determine_otp_behavior(process_details || worker),
      alive: worker.alive,
      level: worker.level,
      node: worker.node,
      application: worker.application
    }

    if process_details do
      Map.merge(base_worker, %{
        memory: Map.get(process_details, :memory, 0),
        message_queue_len: Map.get(process_details, :message_queue_len, 0),
        status: Map.get(process_details, :status, :running),
        links: Map.get(process_details, :links, []),
        monitors: Map.get(process_details, :monitors, []),
        registered_name: Map.get(process_details, :registered_name)
      })
    else
      base_worker
    end
  end

  defp determine_process_type(process) do
    cond do
      Map.get(process, :type) == :supervisor -> :supervisor
      Map.get(process, :registered_name) -> :named_process
      true -> :worker
    end
  end

  defp determine_otp_behavior(process) when is_nil(process), do: :unknown

  defp determine_otp_behavior(process) do
    case Map.get(process, :behavior) || Map.get(process, :module) do
      nil ->
        :gen_server

      behavior when is_atom(behavior) ->
        case to_string(behavior) do
          "gen_server" -> :gen_server
          "gen_statem" -> :gen_statem
          "gen_event" -> :gen_event
          "supervisor" -> :supervisor
          "task" -> :task
          "agent" -> :agent
          _ -> :gen_server
        end

      _ ->
        :gen_server
    end
  end

  defp calculate_total_processes_enhanced(cluster_nodes) do
    cluster_nodes
    |> Enum.reduce(0, fn {_node, node_data}, acc ->
      acc + Map.get(node_data.node_info, :total_processes, 0)
    end)
  end

  defp format_error(reason) when is_binary(reason), do: reason
  defp format_error(reason), do: inspect(reason)
end
