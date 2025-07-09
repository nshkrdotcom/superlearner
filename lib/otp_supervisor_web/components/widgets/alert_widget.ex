defmodule OtpSupervisorWeb.Components.Widgets.AlertWidget do
  use Phoenix.LiveComponent

  @moduledoc """
  System alerts and notifications widget.

  Displays various types of alerts with:
  - Different severity levels (info, warning, error, critical)
  - Dismissible notifications
  - Auto-expiring alerts
  - Grouped alert display
  - Real-time alert updates
  """

  attr :alerts, :list, required: true
  attr :max_alerts, :integer, default: 10
  attr :show_timestamps, :boolean, default: true
  attr :auto_dismiss, :boolean, default: false
  attr :dismiss_timeout, :integer, default: 5000
  attr :compact_mode, :boolean, default: false
  attr :class, :string, default: ""

  def render(assigns) do
    ~H"""
    <div class={[
      "bg-gray-900 border border-green-500/30 rounded text-green-400",
      @class
    ]}>
      <!-- Header -->
      <div class="flex items-center justify-between p-3 border-b border-green-500/20">
        <div class="flex items-center space-x-4">
          <h3 class="text-sm font-mono font-bold text-green-300">
            System Alerts
          </h3>
          <span class="text-xs text-green-400/70 font-mono">
            ({length(@alerts)} alerts)
          </span>
        </div>

        <div class="flex items-center space-x-2">
          <!-- Alert summary -->
          <div class="flex items-center space-x-3">
            <%= for {severity, count} <- alert_summary(@alerts) do %>
              <div class="flex items-center space-x-1">
                <div class={[
                  "w-2 h-2 rounded-full",
                  alert_color_indicator(severity)
                ]}>
                </div>
                <span class="text-xs font-mono text-green-400/70">
                  {count}
                </span>
              </div>
            <% end %>
          </div>
          
    <!-- Clear all button -->
          <%= if @alerts != [] do %>
            <button
              phx-target={@myself}
              phx-click="clear_all"
              class="px-2 py-1 text-xs bg-red-500/20 border border-red-500/30 rounded text-red-400 font-mono hover:bg-red-500/30 transition-colors"
            >
              Clear All
            </button>
          <% end %>
        </div>
      </div>
      
    <!-- Alerts list -->
      <div class="max-h-96 overflow-y-auto">
        <%= if @alerts == [] do %>
          <div class="p-8 text-center">
            <div class="text-green-400/50 text-sm font-mono">No alerts</div>
            <div class="text-green-400/30 text-xs font-mono mt-1">System is operating normally</div>
          </div>
        <% else %>
          <div class="space-y-2 p-3">
            <%= for alert <- Enum.take(@alerts, @max_alerts) do %>
              <div class={[
                "flex items-start space-x-3 p-3 rounded border transition-all duration-200",
                alert_border_classes(alert.severity),
                alert_background_classes(alert.severity)
              ]}>
                <!-- Alert icon -->
                <div class="flex-shrink-0 mt-0.5">
                  <span class={[
                    "text-sm",
                    alert_text_color(alert.severity)
                  ]}>
                    {alert_icon(alert.severity)}
                  </span>
                </div>
                
    <!-- Alert content -->
                <div class="flex-1 min-w-0">
                  <div class="flex items-start justify-between">
                    <div class="flex-1">
                      <!-- Alert title -->
                      <%= if alert.title do %>
                        <div class={[
                          "text-sm font-mono font-bold",
                          alert_text_color(alert.severity)
                        ]}>
                          {alert.title}
                        </div>
                      <% end %>
                      
    <!-- Alert message -->
                      <div class={[
                        "text-sm font-mono",
                        if(@compact_mode, do: "mt-1", else: "mt-2"),
                        alert_text_color(alert.severity)
                      ]}>
                        {alert.message}
                      </div>
                      
    <!-- Alert metadata -->
                      <%= if not @compact_mode do %>
                        <div class="flex items-center space-x-4 mt-2">
                          <%= if alert.source do %>
                            <span class="text-xs text-green-400/70 font-mono">
                              Source: {alert.source}
                            </span>
                          <% end %>

                          <%= if Map.get(alert, :count) && alert.count > 1 do %>
                            <span class="text-xs text-green-400/70 font-mono">
                              Count: {alert.count}
                            </span>
                          <% end %>
                        </div>
                      <% end %>
                    </div>
                    
    <!-- Alert actions -->
                    <div class="flex items-center space-x-2 ml-2">
                      <%= if @show_timestamps do %>
                        <span class="text-xs text-green-400/50 font-mono">
                          {format_timestamp(alert.timestamp)}
                        </span>
                      <% end %>

                      <button
                        phx-target={@myself}
                        phx-click="dismiss_alert"
                        phx-value-alert-id={alert.id}
                        class="p-1 text-xs text-green-400/70 hover:text-green-400 transition-colors"
                        title="Dismiss"
                      >
                        ✕
                      </button>
                    </div>
                  </div>
                </div>
              </div>
            <% end %>
          </div>
        <% end %>
      </div>
      
    <!-- Footer with stats -->
      <%= if @alerts != [] do %>
        <div class="border-t border-green-500/20 p-2">
          <div class="flex items-center justify-between text-xs">
            <div class="flex items-center space-x-4">
              <span class="text-green-400/70 font-mono">
                Showing {min(length(@alerts), @max_alerts)} of {length(@alerts)} alerts
              </span>
            </div>
            <div class="text-green-400/70 font-mono">
              Last updated: {format_timestamp(DateTime.utc_now())}
            </div>
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

    # Set up auto-dismiss timer if enabled
    if assigns.auto_dismiss and assigns.dismiss_timeout > 0 do
      Process.send_after(self(), :auto_dismiss_alerts, assigns.dismiss_timeout)
    end

    {:ok, socket}
  end

  # Event handlers

  def handle_event("dismiss_alert", %{"alert_id" => alert_id}, socket) do
    send(self(), {:dismiss_alert, alert_id})
    {:noreply, socket}
  end

  def handle_event("clear_all", _params, socket) do
    send(self(), :clear_all_alerts)
    {:noreply, socket}
  end

  # Private helper functions

  defp alert_summary(alerts) do
    alerts
    |> Enum.group_by(& &1.severity)
    |> Enum.map(fn {severity, alerts} -> {severity, length(alerts)} end)
    |> Enum.sort_by(fn {severity, _} -> severity_order(severity) end)
  end

  defp severity_order(:critical), do: 1
  defp severity_order(:error), do: 2
  defp severity_order(:warning), do: 3
  defp severity_order(:info), do: 4

  defp alert_color_indicator(:critical), do: "bg-red-500 animate-pulse"
  defp alert_color_indicator(:error), do: "bg-red-400"
  defp alert_color_indicator(:warning), do: "bg-yellow-400"
  defp alert_color_indicator(:info), do: "bg-blue-400"

  defp alert_border_classes(:critical), do: "border-red-500/50"
  defp alert_border_classes(:error), do: "border-red-500/30"
  defp alert_border_classes(:warning), do: "border-yellow-500/30"
  defp alert_border_classes(:info), do: "border-blue-500/30"

  defp alert_background_classes(:critical), do: "bg-red-500/10"
  defp alert_background_classes(:error), do: "bg-red-500/5"
  defp alert_background_classes(:warning), do: "bg-yellow-500/5"
  defp alert_background_classes(:info), do: "bg-blue-500/5"

  defp alert_text_color(:critical), do: "text-red-300"
  defp alert_text_color(:error), do: "text-red-400"
  defp alert_text_color(:warning), do: "text-yellow-400"
  defp alert_text_color(:info), do: "text-blue-400"

  defp alert_icon(:critical), do: "🔥"
  defp alert_icon(:error), do: "❌"
  defp alert_icon(:warning), do: "⚠️"
  defp alert_icon(:info), do: "ℹ️"

  defp format_timestamp(%DateTime{} = dt) do
    Calendar.strftime(dt, "%H:%M:%S")
  end

  defp format_timestamp(timestamp) when is_binary(timestamp) do
    case DateTime.from_iso8601(timestamp) do
      {:ok, dt, _} -> format_timestamp(dt)
      _ -> timestamp
    end
  end

  defp format_timestamp(_), do: "N/A"
end
