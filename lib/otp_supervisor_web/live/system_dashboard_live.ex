defmodule OtpSupervisorWeb.Live.SystemDashboardLive do
  use Phoenix.LiveView

  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OtpSupervisorWeb.Components.Layout.TerminalPanelLayout
  alias OtpSupervisorWeb.Components.Widgets.SystemMetricsWidget
  alias OtpSupervisorWeb.Components.Widgets.ProcessListWidget
  alias OtpSupervisorWeb.Components.Widgets.ChartWidget
  alias OtpSupervisorWeb.Components.Widgets.AlertWidget

  @moduledoc """
  High-density system monitoring dashboard with real-time metrics.

  Refactored to use LiveComponents for better performance and maintainability.
  """

  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(1000, self(), :update_metrics)
      Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "system_metrics")
    end

    {:ok,
     socket
     |> assign(:page_title, "System Dashboard")
     |> assign(:current_page, "dashboard")
     |> assign(:alerts_count, 0)
     |> assign(:selected_process, nil)
     |> assign(:distributed_status, get_distributed_status())
     |> load_initial_data()}
  end

  def handle_info(:update_metrics, socket) do
    {:noreply, update_system_metrics(socket)}
  end

  def handle_info({:system_update, data}, socket) do
    {:noreply, handle_system_update(socket, data)}
  end

  def handle_info(:update_chart_data, socket) do
    {:noreply, update_system_metrics(socket)}
  end

  def render(assigns) do
    ~H"""
    <div class="fixed inset-0 bg-gray-900 text-green-400 flex flex-col">
      <!-- Status Bar -->
      <.live_component
        module={TerminalStatusBar}
        id="dashboard-status-bar"
        title="System Dashboard"
        metrics={status_bar_metrics(assigns)}
        navigation_links={TerminalNavigationLinks.page_navigation_links("dashboard", %{})}
      />
      
    <!-- Main Grid Layout -->
      <.live_component
        module={TerminalPanelLayout}
        id="dashboard-grid-layout"
        layout_type={:grid}
        panels={dashboard_panels(assigns)}
        gap="gap-4"
        padding="p-4"
      />
    </div>
    """
  end

  # Event handlers

  def handle_event("select_process", %{"process_id" => process_id}, socket) do
    process = find_process(process_id, socket.assigns.processes)
    {:noreply, assign(socket, :selected_process, process)}
  end

  def handle_event("kill_process", %{"process_id" => process_id}, socket) do
    # Here you would call your process management API
    # For now, we'll just simulate it
    {:noreply, put_flash(socket, :info, "Process #{process_id} terminated")}
  end

  def handle_event("restart_process", %{"process_id" => process_id}, socket) do
    # Here you would call your process management API
    {:noreply, put_flash(socket, :info, "Process #{process_id} restarted")}
  end

  def handle_event("clear_alerts", _params, socket) do
    {:noreply, assign(socket, :alerts_count, 0)}
  end

  def handle_event("refresh_metrics", _params, socket) do
    {:noreply, update_system_metrics(socket)}
  end

  # Private functions

  defp load_initial_data(socket) do
    socket
    |> assign(:system_metrics, get_system_metrics())
    |> assign(:processes, get_process_list())
    |> assign(:performance_data, get_performance_data())
    |> assign(:alerts, get_system_alerts())
    |> assign(:network_stats, get_network_stats())
    |> assign(:memory_breakdown, get_memory_breakdown())
  end

  defp update_system_metrics(socket) do
    socket
    |> assign(:system_metrics, get_system_metrics())
    |> assign(:processes, get_process_list())
    |> assign(:performance_data, get_performance_data())
    |> assign(:network_stats, get_network_stats())
    |> assign(:memory_breakdown, get_memory_breakdown())
    |> update_alerts()
  end

  defp handle_system_update(socket, %{type: :process_update, data: processes}) do
    assign(socket, :processes, processes)
  end

  defp handle_system_update(socket, %{type: :metrics_update, data: metrics}) do
    assign(socket, :system_metrics, metrics)
  end

  defp handle_system_update(socket, _data), do: socket

  defp update_alerts(socket) do
    alerts = get_system_alerts()
    assign(socket, :alerts_count, length(alerts))
  end

  defp status_bar_metrics(assigns) do
    distributed_indicator = case assigns.distributed_status do
      %{mode: :multi_node, total_nodes: nodes} when nodes > 1 ->
        %{label: "Cluster", value: "#{nodes} nodes", color: "text-green-400"}
      %{simulation_enabled: true} ->
        %{label: "Mode", value: "Simulation", color: "text-yellow-400"}
      _ ->
        %{label: "Mode", value: "Single Node", color: "text-blue-400"}
    end

    [
      %{label: "CPU", value: "#{assigns.system_metrics.cpu_usage}%"},
      %{label: "Memory", value: format_bytes(assigns.system_metrics.memory_used)},
      %{label: "Processes", value: "#{length(assigns.processes)}"},
      distributed_indicator,
      %{label: "Alerts", value: "#{assigns.alerts_count}"}
    ]
  end

  defp dashboard_panels(assigns) do
    [
      # System Metrics Widget (comprehensive system monitoring)
      %{
        title: "System Metrics",
        component: SystemMetricsWidget,
        assigns: %{
          id: "system-metrics",
          metrics: system_metrics_data(assigns),
          real_time: true,
          show_charts: true,
          alert_thresholds: %{
            cpu_usage: %{warning: 70, critical: 90},
            memory_usage: %{warning: 80, critical: 95},
            disk_usage: %{warning: 85, critical: 95}
          }
        },
        span: %{cols: 2, rows: 2}
      },

      # Process List Widget (interactive process management)
      %{
        title: "Process Management",
        component: ProcessListWidget,
        assigns: %{
          id: "process-list",
          processes: process_list_data(assigns),
          selected_process: assigns.selected_process,
          real_time: true,
          show_actions: true,
          filters: %{
            show_system_processes: false,
            min_memory: 0,
            status: :all
          }
        },
        span: %{cols: 2, rows: 2}
      },

      # Performance Chart Widget (time series data)
      %{
        title: "Performance Trends",
        component: ChartWidget,
        assigns: %{
          id: "performance-chart",
          chart_type: :line,
          data: performance_chart_data(assigns),
          title: "CPU & Memory Usage Over Time",
          x_axis_label: "Time",
          y_axis_label: "Usage %",
          real_time: true,
          show_legend: true,
          height: 200
        },
        span: %{cols: 2, rows: 1}
      },

      # Memory Breakdown Chart Widget
      %{
        title: "Memory Distribution",
        component: ChartWidget,
        assigns: %{
          id: "memory-chart",
          chart_type: :pie,
          data: memory_chart_data(assigns),
          title: "Memory Usage by Category",
          show_legend: true,
          color_scheme: :multi,
          height: 200
        },
        span: %{cols: 1, rows: 1}
      },

      # Network Activity Chart Widget
      %{
        title: "Network Activity",
        component: ChartWidget,
        assigns: %{
          id: "network-chart",
          chart_type: :area,
          data: network_chart_data(assigns),
          title: "Network I/O",
          x_axis_label: "Time",
          y_axis_label: "Bytes/sec",
          real_time: true,
          color_scheme: :blue,
          height: 200
        },
        span: %{cols: 1, rows: 1}
      },

      # System Alerts Widget
      %{
        title: "System Alerts",
        component: AlertWidget,
        assigns: %{
          id: "system-alerts",
          alerts: system_alerts_data(assigns),
          max_alerts: 20,
          show_timestamps: true,
          auto_dismiss: false
        },
        span: %{cols: 2, rows: 1}
      }
    ]
  end

  # Mock data functions (replace with real implementations)

  defp get_system_metrics do
    %{
      cpu_usage: :rand.uniform(100),
      memory_used: 1_024_000_000 + :rand.uniform(512_000_000),
      memory_total: 2_048_000_000,
      uptime: :rand.uniform(86400),
      load_average: :rand.uniform(4) + :rand.uniform()
    }
  end

  defp get_process_list do
    for i <- 1..50 do
      %{
        id: "#{i}",
        pid: "<0.#{100 + i}.0>",
        name: "process_#{i}",
        status: Enum.random([:running, :stopped, :error]),
        memory: :rand.uniform(10_000_000),
        cpu: :rand.uniform(100),
        uptime: :rand.uniform(3600)
      }
    end
  end

  defp get_performance_data do
    %{
      requests_per_second: :rand.uniform(1000),
      avg_response_time: :rand.uniform(100),
      gc_count: :rand.uniform(1000),
      gc_time: :rand.uniform(50)
    }
  end

  defp get_network_stats do
    %{
      active_connections: :rand.uniform(100),
      bytes_in: :rand.uniform(1_000_000),
      bytes_out: :rand.uniform(1_000_000),
      errors: :rand.uniform(5)
    }
  end

  defp get_memory_breakdown do
    total = 2_048_000_000
    processes = :rand.uniform(div(total, 2))
    system = :rand.uniform(div(total, 4))
    atom = :rand.uniform(div(total, 10))
    code = :rand.uniform(div(total, 8))

    %{
      processes: processes,
      system: system,
      atom: atom,
      code: code
    }
  end

  defp get_system_alerts do
    # Mock alerts - replace with real implementation
    []
  end

  defp find_process(process_id, processes) do
    Enum.find(processes, &(&1.id == process_id))
  end

  defp format_bytes(bytes) do
    cond do
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 1)}GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 1)}MB"
      bytes >= 1_024 -> "#{Float.round(bytes / 1_024, 1)}KB"
      true -> "#{bytes}B"
    end
  end

  # New data transformation functions for specialized widgets

  defp system_metrics_data(assigns) do
    %{
      cpu: %{
        usage: assigns.system_metrics.cpu_usage,
        cores: 4,
        load_average: assigns.system_metrics.load_average,
        temperature: 65
      },
      memory: %{
        total: assigns.system_metrics.memory_total,
        used: assigns.system_metrics.memory_used,
        available: assigns.system_metrics.memory_total - assigns.system_metrics.memory_used,
        swap_total: 2_048_000_000,
        swap_used: 0
      },
      disk: %{
        total: 100_000_000_000,
        used: 45_000_000_000,
        available: 55_000_000_000,
        io_read: 1_000_000,
        io_write: 500_000
      },
      network: %{
        interfaces: [
          %{
            name: "eth0",
            rx_bytes: assigns.network_stats.bytes_in,
            tx_bytes: assigns.network_stats.bytes_out
          },
          %{name: "lo", rx_bytes: 1000, tx_bytes: 1000}
        ],
        connections: assigns.network_stats.active_connections
      },
      uptime: assigns.system_metrics.uptime
    }
  end

  defp process_list_data(assigns) do
    Enum.map(assigns.processes, fn process ->
      %{
        id: process.id,
        pid: process.pid,
        name: process.name,
        status: process.status,
        memory: process.memory,
        cpu_usage: process.cpu,
        uptime: process.uptime,
        parent: nil,
        children: [],
        priority: :normal,
        command: "/usr/bin/#{process.name}",
        user: "system"
      }
    end)
  end

  defp performance_chart_data(assigns) do
    # Generate time series data for the last 60 seconds
    now = DateTime.utc_now()

    for i <- 59..0//-1 do
      timestamp = DateTime.add(now, -i, :second)

      %{
        timestamp: timestamp,
        cpu: assigns.system_metrics.cpu_usage + :rand.uniform(20) - 10,
        memory:
          assigns.system_metrics.memory_used / assigns.system_metrics.memory_total * 100 +
            :rand.uniform(10) - 5,
        x: DateTime.to_unix(timestamp),
        y: assigns.system_metrics.cpu_usage + :rand.uniform(20) - 10
      }
    end
  end

  defp memory_chart_data(assigns) do
    [
      %{
        label: "Processes",
        value: assigns.memory_breakdown.processes,
        y: assigns.memory_breakdown.processes
      },
      %{
        label: "System",
        value: assigns.memory_breakdown.system,
        y: assigns.memory_breakdown.system
      },
      %{label: "Atom", value: assigns.memory_breakdown.atom, y: assigns.memory_breakdown.atom},
      %{label: "Code", value: assigns.memory_breakdown.code, y: assigns.memory_breakdown.code}
    ]
  end

  defp network_chart_data(assigns) do
    # Generate network I/O data for the last 30 seconds
    now = DateTime.utc_now()

    for i <- 29..0//-1 do
      timestamp = DateTime.add(now, -i, :second)

      %{
        timestamp: timestamp,
        bytes_in: assigns.network_stats.bytes_in + :rand.uniform(1000),
        bytes_out: assigns.network_stats.bytes_out + :rand.uniform(1000),
        x: DateTime.to_unix(timestamp),
        y: assigns.network_stats.bytes_in + :rand.uniform(1000)
      }
    end
  end

  defp get_distributed_status do
    try do
      cluster_status = OTPSupervisor.Distributed.ToolManager.get_cluster_status()
      simulation_enabled = OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
      
      %{
        mode: cluster_status.mode,
        nodes: cluster_status.nodes,
        connected_nodes: cluster_status.connected_nodes,
        current_node: cluster_status.current_node,
        simulation_enabled: simulation_enabled,
        total_nodes: length(cluster_status.nodes)
      }
    rescue
      _ -> 
        %{
          mode: :single_node,
          nodes: [Node.self()],
          connected_nodes: [],
          current_node: Node.self(),
          simulation_enabled: false,
          total_nodes: 1
        }
    end
  end

  defp system_alerts_data(assigns) do
    base_alerts = [
      %{
        id: "cpu-warning",
        severity: :warning,
        title: "High CPU Usage",
        message: "CPU usage is above 70%",
        timestamp: DateTime.utc_now(),
        source: "system_monitor",
        count: 1
      },
      %{
        id: "memory-info",
        severity: :info,
        title: "Memory Usage Normal",
        message: "System memory usage is within normal range",
        timestamp: DateTime.add(DateTime.utc_now(), -300, :second),
        source: "system_monitor",
        count: 1
      }
    ]

    # Add dynamic alerts based on system state
    dynamic_alerts = []

    dynamic_alerts =
      if assigns.system_metrics.cpu_usage > 90 do
        [
          %{
            id: "cpu-critical",
            severity: :critical,
            title: "Critical CPU Usage",
            message: "CPU usage is dangerously high at #{assigns.system_metrics.cpu_usage}%",
            timestamp: DateTime.utc_now(),
            source: "system_monitor",
            count: 1
          }
          | dynamic_alerts
        ]
      else
        dynamic_alerts
      end

    dynamic_alerts =
      if assigns.network_stats.errors > 0 do
        [
          %{
            id: "network-error",
            severity: :error,
            title: "Network Errors Detected",
            message: "#{assigns.network_stats.errors} network errors detected",
            timestamp: DateTime.utc_now(),
            source: "network_monitor",
            count: assigns.network_stats.errors
          }
          | dynamic_alerts
        ]
      else
        dynamic_alerts
      end

    base_alerts ++ dynamic_alerts
  end
end
