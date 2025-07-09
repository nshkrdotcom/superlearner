defmodule OtpSupervisorWeb.Components.Widgets.SystemMetricsWidget do
  use Phoenix.LiveComponent

  @moduledoc """
  Real-time system metrics widget with charts and alerts.

  Displays comprehensive system information including CPU, memory, processes,
  network stats, and performance metrics with visual indicators.
  """

  attr :metrics, :map, required: true
  attr :show_charts, :boolean, default: true
  attr :compact_mode, :boolean, default: false
  attr :alert_thresholds, :map, default: %{}
  attr :update_interval, :integer, default: 2000
  attr :class, :string, default: ""

  def render(assigns) do
    ~H"""
    <div class={[
      "bg-gray-900 border border-green-500/30 rounded text-green-400",
      if(@compact_mode, do: "p-2", else: "p-4"),
      @class
    ]}>
      <!-- Header -->
      <div class="flex items-center justify-between mb-4">
        <h3 class="text-lg font-mono font-bold text-green-300">System Metrics</h3>
        <div class="flex items-center space-x-2">
          <div class="flex items-center space-x-1">
            <div class="w-2 h-2 bg-green-400 rounded-full animate-pulse"></div>
            <span class="text-xs text-green-400/70 font-mono">Live</span>
          </div>
          <span class="text-xs text-green-400/50 font-mono">
            {format_timestamp(DateTime.utc_now())}
          </span>
        </div>
      </div>
      
    <!-- Core System Metrics -->
      <div class="grid grid-cols-2 gap-4 mb-6">
        <!-- CPU Usage -->
        <div class="space-y-2">
          <div class="flex items-center justify-between">
            <span class="text-sm font-mono text-green-300">CPU Usage</span>
            <span class={[
              "text-sm font-mono font-bold",
              cpu_usage_color(@metrics.cpu.usage)
            ]}>
              {@metrics.cpu.usage}%
            </span>
          </div>
          <div class="h-2 bg-gray-700 rounded overflow-hidden">
            <div
              class={[
                "h-full rounded transition-all duration-500",
                cpu_progress_color(@metrics.cpu.usage)
              ]}
              style={"width: #{@metrics.cpu.usage}%"}
            >
            </div>
          </div>
          <%= if @show_charts do %>
            <div class="h-8 flex items-end space-x-px">
              <%= for value <- (@metrics.cpu[:history] || []) do %>
                <div class="flex-1 bg-green-500/30 rounded-t" style={"height: #{value}%"}></div>
              <% end %>
            </div>
          <% end %>
        </div>
        
    <!-- Memory Usage -->
        <div class="space-y-2">
          <div class="flex items-center justify-between">
            <span class="text-sm font-mono text-green-300">Memory</span>
            <span class={[
              "text-sm font-mono font-bold",
              memory_usage_color(@metrics.memory.used, @metrics.memory.total)
            ]}>
              {format_bytes(@metrics.memory.used)} / {format_bytes(@metrics.memory.total)}
            </span>
          </div>
          <div class="h-2 bg-gray-700 rounded overflow-hidden">
            <div
              class={[
                "h-full rounded transition-all duration-500",
                memory_progress_color(@metrics.memory.used, @metrics.memory.total)
              ]}
              style={"width: #{memory_percentage(@metrics.memory.used, @metrics.memory.total)}%"}
            >
            </div>
          </div>
          <%= if @show_charts do %>
            <div class="h-8 flex items-end space-x-px">
              <%= for value <- (@metrics.memory[:history] || []) do %>
                <div class="flex-1 bg-blue-500/30 rounded-t" style={"height: #{value}%"}></div>
              <% end %>
            </div>
          <% end %>
        </div>
      </div>
      
    <!-- Additional Metrics Grid -->
      <div class="grid grid-cols-3 gap-4 mb-4">
        <!-- Process Count -->
        <div class="text-center">
          <div class="text-2xl font-mono font-bold text-green-400">
            {@metrics[:process_count] || 0}
          </div>
          <div class="text-xs text-green-400/70 font-mono">Processes</div>
        </div>
        
    <!-- Load Average -->
        <div class="text-center">
          <div class={[
            "text-2xl font-mono font-bold",
            load_average_color(@metrics.cpu.load_average)
          ]}>
            {Float.round(@metrics.cpu.load_average, 2)}
          </div>
          <div class="text-xs text-green-400/70 font-mono">Load Avg</div>
        </div>
        
    <!-- Uptime -->
        <div class="text-center">
          <div class="text-2xl font-mono font-bold text-green-400">
            {format_uptime(@metrics.uptime)}
          </div>
          <div class="text-xs text-green-400/70 font-mono">Uptime</div>
        </div>
      </div>
      
    <!-- Network & I/O Stats -->
      <%= if not @compact_mode do %>
        <div class="border-t border-green-500/20 pt-4">
          <h4 class="text-sm font-mono font-bold text-green-300 mb-3">Network & I/O</h4>
          <div class="grid grid-cols-2 gap-4">
            <div class="space-y-2">
              <div class="flex justify-between">
                <span class="text-xs text-green-400/70 font-mono">Network In</span>
                <span class="text-xs text-green-400 font-mono">
                  {format_bytes(get_network_total(@metrics.network, :rx))}
                </span>
              </div>
              <div class="flex justify-between">
                <span class="text-xs text-green-400/70 font-mono">Network Out</span>
                <span class="text-xs text-green-400 font-mono">
                  {format_bytes(get_network_total(@metrics.network, :tx))}
                </span>
              </div>
            </div>
            <div class="space-y-2">
              <div class="flex justify-between">
                <span class="text-xs text-green-400/70 font-mono">Disk Read</span>
                <span class="text-xs text-green-400 font-mono">
                  {format_bytes(@metrics.disk.io_read)}
                </span>
              </div>
              <div class="flex justify-between">
                <span class="text-xs text-green-400/70 font-mono">Disk Write</span>
                <span class="text-xs text-green-400 font-mono">
                  {format_bytes(@metrics.disk.io_write)}
                </span>
              </div>
            </div>
          </div>
        </div>
      <% end %>
      
    <!-- Alerts -->
      <%= if has_alerts?(assigns) do %>
        <div class="border-t border-red-500/20 pt-4 mt-4">
          <div class="flex items-center space-x-2 mb-2">
            <div class="w-2 h-2 bg-red-400 rounded-full animate-pulse"></div>
            <span class="text-sm font-mono font-bold text-red-300">Active Alerts</span>
          </div>
          <div class="space-y-1">
            <%= for alert <- get_active_alerts(assigns) do %>
              <div class="flex items-center space-x-2 text-xs">
                <span class="text-red-400 font-mono">⚠</span>
                <span class="text-red-300 font-mono">{alert.message}</span>
              </div>
            <% end %>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  def mount(socket) do
    {:ok, socket}
  end

  def update(assigns, socket) do
    socket = assign(socket, assigns)

    # Set up auto-refresh if interval is provided
    if assigns[:update_interval] && assigns.update_interval > 0 do
      Process.send_after(self(), :refresh_system_metrics, assigns.update_interval)
    end

    {:ok, socket}
  end

  # Private helper functions

  defp cpu_usage_color(usage) when usage >= 90, do: "text-red-400"
  defp cpu_usage_color(usage) when usage >= 70, do: "text-yellow-400"
  defp cpu_usage_color(_), do: "text-green-400"

  defp cpu_progress_color(usage) when usage >= 90, do: "bg-red-500"
  defp cpu_progress_color(usage) when usage >= 70, do: "bg-yellow-500"
  defp cpu_progress_color(_), do: "bg-green-500"

  defp memory_usage_color(used, total) when is_number(used) and is_number(total) and total > 0 do
    percentage = used / total * 100

    cond do
      percentage >= 90 -> "text-red-400"
      percentage >= 70 -> "text-yellow-400"
      true -> "text-green-400"
    end
  end

  defp memory_usage_color(_, _), do: "text-green-400"

  defp memory_progress_color(used, total)
       when is_number(used) and is_number(total) and total > 0 do
    percentage = used / total * 100

    cond do
      percentage >= 90 -> "bg-red-500"
      percentage >= 70 -> "bg-yellow-500"
      true -> "bg-blue-500"
    end
  end

  defp memory_progress_color(_, _), do: "bg-blue-500"

  defp memory_percentage(used, total) when is_number(used) and is_number(total) and total > 0 do
    min(100, used / total * 100)
  end

  defp memory_percentage(_, _), do: 0

  defp load_average_color(load) when load >= 2.0, do: "text-red-400"
  defp load_average_color(load) when load >= 1.0, do: "text-yellow-400"
  defp load_average_color(_), do: "text-green-400"

  defp format_bytes(bytes) when is_integer(bytes) do
    cond do
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 1)}GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 1)}MB"
      bytes >= 1_024 -> "#{Float.round(bytes / 1_024, 1)}KB"
      true -> "#{bytes}B"
    end
  end

  defp format_bytes(bytes), do: to_string(bytes)

  defp format_uptime(seconds) when is_integer(seconds) do
    days = div(seconds, 86400)
    hours = div(rem(seconds, 86400), 3600)
    minutes = div(rem(seconds, 3600), 60)

    cond do
      days > 0 -> "#{days}d #{hours}h"
      hours > 0 -> "#{hours}h #{minutes}m"
      true -> "#{minutes}m"
    end
  end

  defp format_uptime(uptime), do: to_string(uptime)

  defp format_timestamp(datetime) do
    Calendar.strftime(datetime, "%H:%M:%S")
  end

  defp get_network_total(network, direction) do
    network.interfaces
    |> Enum.reduce(0, fn interface, acc ->
      case direction do
        :rx -> acc + interface.rx_bytes
        :tx -> acc + interface.tx_bytes
      end
    end)
  end

  defp has_alerts?(assigns) do
    thresholds = assigns.alert_thresholds
    metrics = assigns.metrics

    cond do
      Map.get(thresholds, :cpu_threshold) && metrics.cpu.usage >= thresholds.cpu_threshold ->
        true

      Map.get(thresholds, :memory_threshold) &&
          metrics.memory.used / metrics.memory.total * 100 >= thresholds.memory_threshold ->
        true

      Map.get(thresholds, :load_threshold) &&
          metrics.cpu.load_average >= thresholds.load_threshold ->
        true

      true ->
        false
    end
  end

  defp get_active_alerts(assigns) do
    thresholds = assigns.alert_thresholds
    metrics = assigns.metrics
    alerts = []

    alerts =
      if Map.get(thresholds, :cpu_threshold) && metrics.cpu.usage >= thresholds.cpu_threshold do
        [%{message: "High CPU usage: #{metrics.cpu.usage}%", severity: :critical} | alerts]
      else
        alerts
      end

    alerts =
      if Map.get(thresholds, :memory_threshold) &&
           metrics.memory.used / metrics.memory.total * 100 >= thresholds.memory_threshold do
        memory_pct = Float.round(metrics.memory.used / metrics.memory.total * 100, 1)
        [%{message: "High memory usage: #{memory_pct}%", severity: :critical} | alerts]
      else
        alerts
      end

    alerts =
      if Map.get(thresholds, :load_threshold) &&
           metrics.cpu.load_average >= thresholds.load_threshold do
        [
          %{message: "High load average: #{metrics.cpu.load_average}", severity: :warning}
          | alerts
        ]
      else
        alerts
      end

    alerts
  end
end
