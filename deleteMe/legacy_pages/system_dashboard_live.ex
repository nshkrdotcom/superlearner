defmodule OtpSupervisorWeb.SystemDashboardLive do
  use OtpSupervisorWeb, :live_view

  alias OTPSupervisor.Core.Control
  alias OTPSupervisor.Core.SystemAnalyzer

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(1000, self(), :update_metrics)
    end

    socket =
      socket
      |> assign(:page_title, "System Dashboard")
      |> assign(:system_metrics, get_system_metrics())
      |> assign(:anomalies, detect_anomalies())
      |> assign(:search_results, [])
      |> assign(:selected_processes, [])
      |> assign(:process_analysis, get_process_analysis())
      |> assign(:supervisor_health, get_supervisor_health())
      |> assign(:top_processes, get_top_processes())
      |> assign(:bottlenecks, get_bottlenecks())
      |> assign(:process_graph, get_process_graph())
      |> assign(:memory_analysis, get_memory_analysis())
      |> assign(:network_status, get_network_status())
      |> assign(:otp_info, get_otp_info())

    {:ok, socket}
  end

  @impl true
  def handle_event("search_processes", %{"search" => query}, socket) do
    results = SystemAnalyzer.search_processes(query)
    {:noreply, assign(socket, :search_results, results)}
  end

  def handle_event("toggle_process_selection", %{"pid" => pid_string}, socket) do
    selected = socket.assigns.selected_processes

    new_selected =
      if pid_string in selected do
        List.delete(selected, pid_string)
      else
        [pid_string | selected]
      end

    {:noreply, assign(socket, :selected_processes, new_selected)}
  end

  def handle_event("bulk_kill_processes", _params, socket) do
    selected = socket.assigns.selected_processes

    Enum.each(selected, fn pid_string ->
      case Control.to_pid(pid_string) do
        {:ok, pid} -> Control.kill_process(pid)
        _ -> :ok
      end
    end)

    {:noreply,
     socket
     |> assign(:selected_processes, [])
     |> put_flash(:info, "Killed #{length(selected)} processes")}
  end

  def handle_event("export_system_report", _params, socket) do
    _report = SystemAnalyzer.generate_system_report()

    # In a real implementation, this would trigger a download
    {:noreply, put_flash(socket, :info, "System report exported")}
  end

  @impl true
  def handle_info(:update_metrics, socket) do
    socket =
      socket
      |> assign(:system_metrics, get_system_metrics())
      |> assign(:anomalies, detect_anomalies())
      |> assign(:process_analysis, get_process_analysis())
      |> assign(:supervisor_health, get_supervisor_health())
      |> assign(:top_processes, get_top_processes())
      |> assign(:bottlenecks, get_bottlenecks())
      |> assign(:memory_analysis, get_memory_analysis())

    {:noreply, socket}
  end

  defp get_system_metrics do
    memory = :erlang.memory()

    %{
      total_processes: length(Process.list()),
      memory_usage: memory[:total],
      memory_total: memory[:total],
      message_queue_lengths: get_queue_lengths(),
      cpu_usage: get_cpu_usage(),
      supervision_health: calculate_supervision_health(),
      schedulers: :erlang.system_info(:schedulers),
      uptime: :erlang.statistics(:wall_clock) |> elem(0),
      scheduler_utilization: get_scheduler_utilization(),
      atom_count: :erlang.system_info(:atom_count),
      module_count: length(:code.all_loaded()),
      io_wait: 0.0,
      gc_count: :erlang.statistics(:garbage_collection) |> elem(0),
      gc_time: :erlang.statistics(:garbage_collection) |> elem(1),
      reductions: :erlang.statistics(:reductions) |> elem(0),
      run_queue: :erlang.statistics(:run_queue)
    }
  end

  defp detect_anomalies do
    try do
      SystemAnalyzer.detect_anomalies()
    rescue
      error ->
        IO.inspect(error, label: "Error in detect_anomalies")
        []
    end
  end

  defp get_queue_lengths do
    Process.list()
    |> Enum.map(fn pid ->
      case Process.info(pid, :message_queue_len) do
        {:message_queue_len, len} -> len
        _ -> 0
      end
    end)
    |> Enum.sum()
  end

  defp get_cpu_usage do
    # Use scheduler utilization as CPU approximation since :cpu_sup may not be available
    get_scheduler_utilization()
  end

  defp get_scheduler_utilization do
    # Get scheduler utilization as a rough approximation of CPU usage
    case :erlang.statistics(:scheduler_wall_time) do
      schedulers when is_list(schedulers) ->
        # Calculate average utilization across all schedulers
        total_utilization =
          Enum.reduce(schedulers, 0, fn {_id, active, total}, acc ->
            if total > 0 do
              acc + active / total * 100
            else
              acc
            end
          end)

        if length(schedulers) > 0 do
          total_utilization / length(schedulers)
        else
          0.0
        end

      _ ->
        0.0
    end
  rescue
    _ -> 0.0
  end

  defp calculate_supervision_health do
    supervisors = Control.list_supervisors()

    if length(supervisors) > 0 do
      healthy_count = Enum.count(supervisors, &supervisor_healthy?/1)
      healthy_count / length(supervisors) * 100
    else
      100.0
    end
  end

  defp supervisor_healthy?(supervisor_info) when is_map(supervisor_info) do
    case Control.get_supervision_tree(supervisor_info.name) do
      {:ok, _children} -> true
      _ -> false
    end
  end

  # Helper functions for the template

  def format_bytes(bytes) when is_integer(bytes) do
    cond do
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 2)} GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 2)} MB"
      bytes >= 1024 -> "#{Float.round(bytes / 1024, 2)} KB"
      true -> "#{bytes} B"
    end
  end

  def format_bytes(_), do: "N/A"

  def format_number(num) when is_integer(num) do
    cond do
      num >= 1_000_000_000 -> "#{Float.round(num / 1_000_000_000, 1)}B"
      num >= 1_000_000 -> "#{Float.round(num / 1_000_000, 1)}M"
      num >= 1_000 -> "#{Float.round(num / 1_000, 1)}K"
      true -> Integer.to_string(num)
    end
  end

  def format_number(_), do: "N/A"

  def format_uptime(uptime_ms) when is_integer(uptime_ms) do
    seconds = div(uptime_ms, 1000)

    cond do
      seconds >= 86400 -> "#{div(seconds, 86400)}d"
      seconds >= 3600 -> "#{div(seconds, 3600)}h"
      seconds >= 60 -> "#{div(seconds, 60)}m"
      true -> "#{seconds}s"
    end
  end

  def format_uptime(_), do: "N/A"

  def status_color(status) do
    case status do
      :running -> "text-green-400"
      :waiting -> "text-yellow-400"
      :suspended -> "text-orange-400"
      :exiting -> "text-red-400"
      _ -> "text-gray-400"
    end
  end

  defp get_process_analysis do
    processes = Process.list()

    %{
      total_processes: length(processes),
      supervisors: count_processes_by_type(processes, :supervisor),
      genservers: count_processes_by_type(processes, :genserver),
      workers: count_processes_by_type(processes, :worker),
      other: count_processes_by_type(processes, :other),
      high_memory_count: count_high_memory_processes(processes),
      high_queue_count: count_high_queue_processes(processes)
    }
  end

  defp get_supervisor_health do
    supervisors = Control.list_supervisors()

    %{
      total_supervisors: length(supervisors),
      healthy: Enum.count(supervisors, & &1.alive),
      warning: 0,
      critical: Enum.count(supervisors, &(not &1.alive)),
      recent_restarts: 0,
      failure_rate: 0
    }
  end

  defp get_top_processes do
    Process.list()
    |> Enum.take(20)
    |> Enum.map(fn pid ->
      info =
        Process.info(pid, [:memory, :message_queue_len, :reductions, :status, :registered_name])

      %{
        pid: inspect(pid),
        name: get_process_name(info[:registered_name]),
        memory: info[:memory] || 0,
        message_queue_len: info[:message_queue_len] || 0,
        reductions: info[:reductions] || 0,
        status: info[:status] || :unknown
      }
    end)
    |> Enum.sort_by(& &1.memory, :desc)
  end

  defp get_bottlenecks do
    []
  end

  defp get_process_graph do
    %{
      node_count: length(Process.list()),
      edge_count: 0,
      cluster_count: length(Control.list_supervisors())
    }
  end

  defp get_memory_analysis do
    memory = :erlang.memory()

    %{
      total: memory[:total] || 0,
      processes: memory[:processes] || 0,
      system: memory[:system] || 0,
      atom: memory[:atom] || 0,
      binary: memory[:binary] || 0,
      code: memory[:code] || 0,
      ets: memory[:ets] || 0
    }
  end

  defp get_network_status do
    %{
      node_name: to_string(Node.self()),
      cookie: to_string(:erlang.get_cookie()),
      connected_nodes: length(Node.list()),
      open_ports: length(Port.list())
    }
  end

  defp get_otp_info do
    %{
      version: to_string(:erlang.system_info(:otp_release)),
      elixir_version: System.version(),
      erts_version: to_string(:erlang.system_info(:version)),
      start_time: "N/A",
      mode: "development"
    }
  end

  defp count_processes_by_type(processes, type) do
    Enum.count(processes, fn pid ->
      classify_process(pid) == type
    end)
  end

  defp classify_process(pid) do
    case Process.info(pid, :initial_call) do
      {:initial_call, {Supervisor, _, _}} -> :supervisor
      {:initial_call, {GenServer, _, _}} -> :genserver
      {:initial_call, {:gen_server, _, _}} -> :genserver
      _ -> :worker
    end
  rescue
    _ -> :other
  end

  defp count_high_memory_processes(processes) do
    # 10MB
    threshold = 10 * 1024 * 1024

    Enum.count(processes, fn pid ->
      case Process.info(pid, :memory) do
        {:memory, memory} when memory > threshold -> true
        _ -> false
      end
    end)
  end

  defp count_high_queue_processes(processes) do
    threshold = 100

    Enum.count(processes, fn pid ->
      case Process.info(pid, :message_queue_len) do
        {:message_queue_len, len} when len > threshold -> true
        _ -> false
      end
    end)
  end

  defp get_process_name(nil), do: nil
  defp get_process_name(name), do: to_string(name)
end
