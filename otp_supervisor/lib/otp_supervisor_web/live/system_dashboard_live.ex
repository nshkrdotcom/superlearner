defmodule OtpSupervisorWeb.SystemDashboardLive do
  use OtpSupervisorWeb, :live_view

  alias OTPSupervisor.Core.Control
  alias OtpSupervisor.Core.SystemAnalyzer

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

    {:noreply, socket}
  end

  defp get_system_metrics do
    %{
      total_processes: length(Process.list()),
      memory_usage: :erlang.memory(:total),
      message_queue_lengths: get_queue_lengths(),
      cpu_usage: get_cpu_usage(),
      supervision_health: calculate_supervision_health()
    }
  end

  defp detect_anomalies do
    SystemAnalyzer.detect_anomalies()
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
end
