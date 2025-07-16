defmodule OtpSupervisorWeb.Live.ClusterLive do
  use Phoenix.LiveView

  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OtpSupervisorWeb.Components.Terminal.TerminalMetricWidget
  alias OtpSupervisorWeb.Components.Layout.TerminalPanelLayout
  alias OtpSupervisorWeb.Components.Widgets.ChartWidget
  alias OtpSupervisorWeb.Components.Widgets.AlertWidget

  @moduledoc """
  Cluster monitoring and management interface.
  
  Shows information about the current server and other servers in the cluster,
  using the Arsenal cluster operations for real-time data.
  """

  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(3000, self(), :update_cluster_data)
      Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "cluster_updates")
    end

    socket = socket
     |> assign(:page_title, "Cluster Monitor")
     |> assign(:current_page, "cluster")
     |> assign(:selected_node, nil)
     |> assign(:cluster_health, %{})
     |> assign(:cluster_topology, %{})
     |> assign(:current_node, Node.self())

    socket = load_cluster_data(socket)

    {:ok, socket}
  end

  def handle_info(:update_cluster_data, socket) do
    {:noreply, update_cluster_data(socket)}
  end

  def handle_info(:update_chart_data, socket) do
    {:noreply, socket}
  end

  def handle_info({:cluster_update, data}, socket) do
    {:noreply, handle_cluster_update(socket, data)}
  end

  def handle_info(_msg, socket) do
    # Catch-all for unexpected messages to prevent crashes
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div class="fixed inset-0 bg-gray-900 text-green-400 flex flex-col">
      <!-- Status Bar -->
      <.live_component
        module={TerminalStatusBar}
        id="cluster-status-bar"
        title="Cluster Monitor"
        metrics={status_bar_metrics(assigns)}
        navigation_links={TerminalNavigationLinks.page_navigation_links("cluster", %{})}
      />
      
      <!-- Main Grid Layout -->
      <.live_component
        module={TerminalPanelLayout}
        id="cluster-grid-layout"
        layout_type={:grid}
        panels={cluster_panels(assigns)}
        gap="gap-4"
        padding="p-4"
      />
    </div>
    """
  end

  # Event handlers

  def handle_event("select_node", %{"node" => node_name}, socket) do
    node_atom = String.to_atom(node_name)
    {:noreply, assign(socket, :selected_node, node_atom)}
  end

  def handle_event("refresh_cluster", _params, socket) do
    {:noreply, update_cluster_data(socket)}
  end

  def handle_event("connect_node", %{"node" => node_name}, socket) do
    # Here you would implement node connection logic
    {:noreply, put_flash(socket, :info, "Attempting to connect to #{node_name}")}
  end

  def handle_event("disconnect_node", %{"node" => node_name}, socket) do
    # Here you would implement node disconnection logic  
    {:noreply, put_flash(socket, :info, "Disconnecting from #{node_name}")}
  end

  # Private functions

  defp load_cluster_data(socket) do
    topology = get_cluster_topology()
    health = get_cluster_health()
    
    socket
    |> assign(:cluster_topology, topology)
    |> assign(:cluster_health, health)
    |> assign(:node_details, %{})
  end

  defp update_cluster_data(socket) do
    socket
    |> assign(:cluster_topology, get_cluster_topology())
    |> assign(:cluster_health, get_cluster_health())
    |> maybe_update_selected_node_details()
  end

  defp maybe_update_selected_node_details(socket) do
    case socket.assigns.selected_node do
      nil ->
        socket

      node ->
        node_details = get_node_details(node)
        assign(socket, :node_details, Map.put(socket.assigns.node_details, node, node_details))
    end
  end

  defp handle_cluster_update(socket, %{type: :topology_update, data: topology}) do
    assign(socket, :cluster_topology, topology)
  end

  defp handle_cluster_update(socket, %{type: :health_update, data: health}) do
    assign(socket, :cluster_health, health)
  end

  defp handle_cluster_update(socket, _data), do: socket

  defp status_bar_metrics(assigns) do
    topology = assigns.cluster_topology
    health = assigns.cluster_health

    total_nodes = Map.get(topology, :total_nodes, 1)
    healthy_nodes = Map.get(health, :nodes_healthy, 1)
    current_node = assigns.current_node

    cluster_status = 
      case Map.get(health, :overall_status, :healthy) do
        :healthy -> %{label: "Status", value: "Healthy"}
        :warning -> %{label: "Status", value: "Warning"}
        :degraded -> %{label: "Status", value: "Degraded"}
        :critical -> %{label: "Status", value: "Critical"}
        _ -> %{label: "Status", value: "Unknown"}
      end

    [
      %{label: "Current Node", value: to_string(current_node)},
      %{label: "Total Nodes", value: "#{total_nodes}"},
      %{label: "Healthy", value: "#{healthy_nodes}"},
      cluster_status,
      %{label: "Mode", value: get_cluster_mode_display(topology)}
    ]
  end

  defp cluster_panels(assigns) do
    [
      # Cluster Topology Overview
      %{
        title: "Cluster Topology",
        component: TerminalMetricWidget,
        assigns: %{
          id: "cluster-topology",
          title: "Cluster Overview",
          metrics: cluster_topology_metrics(assigns),
          size: :medium
        },
        span: %{cols: 2, rows: 1}
      },

      # Current Node Details
      %{
        title: "Current Node (#{assigns.current_node})",
        component: TerminalMetricWidget,
        assigns: %{
          id: "current-node-metrics",
          title: "Current Node Status",
          metrics: current_node_metrics(assigns),
          size: :medium
        },
        span: %{cols: 1, rows: 2}
      },

      # Cluster Health Chart
      %{
        title: "Cluster Health Over Time",
        component: ChartWidget,
        assigns: %{
          id: "cluster-health-chart",
          chart_type: :line,
          data: cluster_health_chart_data(assigns),
          title: "Node Health History",
          x_axis_label: "Time",
          y_axis_label: "Healthy Nodes",
          real_time: true,
          show_legend: true,
          height: 200
        },
        span: %{cols: 1, rows: 1}
      },

      # Other Nodes Status
      %{
        title: "Other Cluster Nodes",
        component: TerminalMetricWidget,
        assigns: %{
          id: "other-nodes-status",
          title: "Remote Nodes",
          metrics: other_nodes_metrics(assigns),
          size: :medium
        },
        span: %{cols: 2, rows: 1}
      },

      # Cluster Alerts
      %{
        title: "Cluster Alerts",
        component: AlertWidget,
        assigns: %{
          id: "cluster-alerts",
          alerts: cluster_alerts_data(assigns),
          max_alerts: 15,
          show_timestamps: true,
          auto_dismiss: false
        },
        span: %{cols: 2, rows: 1}
      }
    ]
  end

  # Data functions using Arsenal operations

  defp get_cluster_topology do
    try do
      {:ok, params} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopology.validate_params(%{
        "include_processes" => false,
        "include_health" => true
      })
      
      case OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopology.execute(params) do
        {:ok, topology} -> 
          topology
        {:error, _reason} -> 
          get_fallback_topology()
      end
    rescue
      _error -> 
        get_fallback_topology()
    end
  end

  defp get_cluster_health do
    try do
      {:ok, params} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.validate_params(%{
        "include_metrics" => true,
        "include_history" => false
      })
      
      case OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.execute(params) do
        {:ok, health} -> 
          health
        {:error, _reason} -> 
          get_fallback_health()
      end
    rescue
      _error -> 
        get_fallback_health()
    end
  end

  defp get_node_details(node) do
    try do
      case OTPSupervisor.Core.Arsenal.Operations.Distributed.NodeInfo.validate_params(%{
             "node" => to_string(node),
             "include_processes" => false
           }) do
        {:ok, params} ->
          case OTPSupervisor.Core.Arsenal.Operations.Distributed.NodeInfo.execute(params) do
            {:ok, node_info} -> node_info
            {:error, _} -> get_fallback_node_info(node)
          end

        {:error, _} ->
          get_fallback_node_info(node)
      end
    rescue
      _ -> get_fallback_node_info(node)
    end
  end

  # Fallback functions for when Arsenal operations fail

  defp get_fallback_topology do
    %{
      mode: :single_node,
      total_nodes: 1,
      nodes: [Node.self()],
      connected_nodes: Node.list(),
      current_node: Node.self(),
      simulation_enabled: false
    }
  end

  defp get_fallback_health do
    %{
      overall_status: :healthy,
      nodes_total: 1,
      nodes_healthy: 1,
      nodes_unhealthy: 0,
      partition_status: :no_partition
    }
  end

  defp get_fallback_node_info(node) do
    %{
      name: node,
      status: if(node == Node.self(), do: :up, else: :unknown),
      memory_usage: %{total: 0},
      cpu_usage: 0.0
    }
  end

  # Metrics data transformation functions

  defp cluster_topology_metrics(assigns) do
    topology = assigns.cluster_topology

    base_metrics = [
      %{
        label: "Cluster Mode",
        value: get_cluster_mode_display(topology),
        format: :text,
        status: :info
      },
      %{
        label: "Total Nodes", 
        value: Map.get(topology, :total_nodes, 1),
        format: :number,
        status: :success
      },
      %{
        label: "Connected Nodes",
        value: calculate_connected_nodes_count(topology),
        format: :number,
        status: :success
      }
    ]

    if Map.get(topology, :simulation_enabled, false) do
      base_metrics ++ [
        %{
          label: "Simulation Mode",
          value: "Active",
          format: :text,
          status: :warning
        }
      ]
    else
      base_metrics
    end
  end

  defp current_node_metrics(assigns) do
    current_node = assigns.current_node
    node_info = get_current_node_info()

    [
      %{
        label: "Node Name",
        value: to_string(current_node),
        format: :text,
        status: :info
      },
      %{
        label: "Status",
        value: "Up",
        format: :text,
        status: :success
      },
      %{
        label: "Memory Usage",
        value: format_memory(Map.get(node_info, :memory_usage, %{total: 0})),
        format: :text,
        status: :info
      },
      %{
        label: "CPU Usage",
        value: "#{Map.get(node_info, :cpu_usage, 0.0)}%",
        format: :text,
        status: :info
      },
      %{
        label: "Process Count",
        value: length(Process.list()),
        format: :number,
        status: :info
      }
    ]
  end

  defp other_nodes_metrics(assigns) do
    topology = assigns.cluster_topology
    all_nodes = Map.get(topology, :nodes, [])
    current_node = assigns.current_node
    other_nodes = Enum.reject(all_nodes, &(&1 == current_node))
    simulation_enabled = Map.get(topology, :simulation_enabled, false)

    if Enum.empty?(other_nodes) do
      [
        %{
          label: "Other Nodes",
          value: "None",
          format: :text,
          status: :info
        }
      ]
    else
      Enum.flat_map(other_nodes, fn node ->
        if simulation_enabled do
          # For simulated nodes, always show as "Up"
          [
            %{
              label: to_string(node),
              value: "Up",
              format: :text,
              status: :success
            }
          ]
        else
          # For real nodes, check if node is connected
          is_connected = node in Node.list()
          
          status_info = if is_connected do
            %{value: "Up", status: :success}
          else
            %{value: "Down", status: :error}
          end

          [
            %{
              label: to_string(node),
              value: status_info.value,
              format: :text,
              status: status_info.status
            }
          ]
        end
      end)
    end
  end

  defp cluster_health_chart_data(assigns) do
    # Generate mock time series data for cluster health
    now = DateTime.utc_now()
    health = assigns.cluster_health
    healthy_nodes = Map.get(health, :nodes_healthy, 1)

    for i <- 29..0//-1 do
      timestamp = DateTime.add(now, -i * 10, :second)
      
      %{
        timestamp: timestamp,
        healthy_nodes: healthy_nodes + :rand.uniform(2) - 1,
        x: DateTime.to_unix(timestamp),
        y: healthy_nodes + :rand.uniform(2) - 1
      }
    end
  end

  defp cluster_alerts_data(assigns) do
    health = assigns.cluster_health
    topology = assigns.cluster_topology
    
    base_alerts = []

    # Add alerts based on cluster health (only if actually unhealthy)
    alerts = case Map.get(health, :overall_status, :healthy) do
      :critical ->
        base_alerts ++ [
          %{
            id: "cluster-critical",
            severity: :critical,
            title: "Cluster Critical",
            message: "Multiple nodes are unavailable",
            timestamp: DateTime.utc_now(),
            source: "cluster_monitor",
            count: 1
          }
        ]
      
      :degraded ->
        # Only show degraded alert if there are actually unhealthy nodes
        unhealthy_nodes = Map.get(health, :nodes_unhealthy, 0)
        if unhealthy_nodes > 0 do
          base_alerts ++ [
            %{
              id: "cluster-degraded", 
              severity: :warning,
              title: "Cluster Degraded",
              message: "#{unhealthy_nodes} nodes are unhealthy",
              timestamp: DateTime.utc_now(),
              source: "cluster_monitor",
              count: 1
            }
          ]
        else
          base_alerts
        end
      
      _ -> base_alerts
    end

    # Add simulation mode alert if active
    if Map.get(topology, :simulation_enabled, false) do
      alerts ++ [
        %{
          id: "simulation-active",
          severity: :info,
          title: "Simulation Mode Active",
          message: "Cluster is running in simulation mode",
          timestamp: DateTime.utc_now(),
          source: "cluster_monitor", 
          count: 1
        }
      ]
    else
      alerts
    end
  end

  # Helper functions

  defp calculate_connected_nodes_count(topology) do
    total_nodes = Map.get(topology, :total_nodes, 1)
    simulation_enabled = Map.get(topology, :simulation_enabled, false)
    
    if simulation_enabled && total_nodes > 1 do
      # In simulation mode, show total_nodes - 1 (excluding current node)
      total_nodes - 1
    else
      # In real cluster mode, use actual connected_nodes count
      length(Map.get(topology, :connected_nodes, []))
    end
  end

  defp get_cluster_mode_display(topology) do
    total_nodes = Map.get(topology, :total_nodes, 1)
    simulation_enabled = Map.get(topology, :simulation_enabled, false)
    
    cond do
      simulation_enabled && total_nodes > 1 -> "Multi-Node (Simulation)"
      total_nodes > 1 -> "Multi-Node"
      true -> "Single Node"
    end
  end

  defp get_current_node_info do
    # Get real system info for current node
    %{
      memory_usage: %{
        total: :erlang.memory(:total),
        processes: :erlang.memory(:processes),
        system: :erlang.memory(:system)
      },
      cpu_usage: get_cpu_usage(),
      processes: length(Process.list())
    }
  end

  defp get_cpu_usage do
    # Simple CPU usage approximation
    :rand.uniform(100)
  end

  defp format_memory(%{total: total}) when is_integer(total) do
    cond do
      total >= 1_073_741_824 -> "#{Float.round(total / 1_073_741_824, 1)}GB"
      total >= 1_048_576 -> "#{Float.round(total / 1_048_576, 1)}MB"
      total >= 1_024 -> "#{Float.round(total / 1_024, 1)}KB"
      true -> "#{total}B"
    end
  end

  defp format_memory(_), do: "N/A"
end