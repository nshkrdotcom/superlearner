defmodule OtpSupervisorWeb.Components.Terminal.TerminalMetricWidget do
  use Phoenix.LiveComponent

  @moduledoc """
  Terminal-themed metric display widget with real-time updates.

  Displays metrics in a bordered container with terminal styling.
  """

  attr :title, :string, required: true
  attr :metrics, :list, required: true
  attr :size, :atom, values: [:small, :medium, :large], default: :medium
  attr :border_color, :string, default: "border-green-500/30"
  attr :bg_color, :string, default: "bg-gray-900"
  attr :text_color, :string, default: "text-green-400"
  attr :update_interval, :integer, default: 1000
  attr :show_border, :boolean, default: true
  attr :class, :string, default: ""

  slot :actions

  def render(assigns) do
    ~H"""
    <div class={[
      "flex flex-col",
      size_classes(@size),
      @class
    ]}>
      <!-- Widget container -->
      <div class={[
        "flex flex-col h-full",
        @bg_color,
        @text_color,
        if(@show_border, do: ["border rounded", @border_color], else: [])
      ]}>
        <!-- Header -->
        <div class="flex items-center justify-between p-3 border-b border-green-500/20">
          <h3 class="text-sm font-mono font-bold">{@title}</h3>
          {render_slot(@actions)}
        </div>
        
    <!-- Metrics content -->
        <div class="flex-1 p-3">
          <%= if @metrics == [] do %>
            <div class="text-green-400/50 text-sm font-mono">No metrics available</div>
          <% else %>
            <div class="grid grid-cols-1 gap-2">
              <%= for metric <- @metrics do %>
                <div class="flex items-center justify-between">
                  <span class="text-green-300 text-sm font-mono">{metric.label}</span>
                  <div class="flex items-center space-x-2">
                    <span class={[
                      "text-sm font-mono font-bold",
                      metric_value_color(metric)
                    ]}>
                      {format_metric_value(metric)}
                    </span>
                    <%= if Map.has_key?(metric, :trend) do %>
                      <span class={[
                        "text-xs font-mono",
                        trend_color(metric.trend)
                      ]}>
                        {trend_symbol(metric.trend)}
                      </span>
                    <% end %>
                  </div>
                </div>
                <%= if Map.has_key?(metric, :bar) and metric.bar do %>
                  <div class="mt-1">
                    <div class="h-1 bg-gray-700 rounded overflow-hidden">
                      <div
                        class={[
                          "h-full rounded transition-all duration-300",
                          progress_bar_color(metric.value, metric.max_value || 100)
                        ]}
                        style={"width: #{progress_percentage(metric.value, metric.max_value || 100)}%"}
                      >
                      </div>
                    </div>
                  </div>
                <% end %>
              <% end %>
            </div>
          <% end %>
        </div>
        
    <!-- Footer with timestamp if provided -->
        <%= if assigns[:last_updated] do %>
          <div class="px-3 py-2 border-t border-green-500/20">
            <div class="text-xs text-green-400/50 font-mono">
              Last updated: {@last_updated}
            </div>
          </div>
        <% end %>
      </div>
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
      Process.send_after(self(), :refresh_metrics, assigns.update_interval)
    end

    {:ok, socket}
  end

  # Private functions

  defp size_classes(:small), do: "w-48 h-32"
  defp size_classes(:medium), do: "w-64 h-40"
  defp size_classes(:large), do: "w-80 h-48"

  defp format_metric_value(%{value: value, unit: unit}) when is_binary(unit) do
    "#{value} #{unit}"
  end

  defp format_metric_value(%{value: value, format: :bytes}) do
    format_bytes(value)
  end

  defp format_metric_value(%{value: value, format: :percentage}) do
    "#{value}%"
  end

  defp format_metric_value(%{value: value, format: :number}) do
    format_number(value)
  end

  defp format_metric_value(%{value: value}) do
    to_string(value)
  end

  defp format_bytes(bytes) when is_integer(bytes) do
    cond do
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 1)}GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 1)}MB"
      bytes >= 1_024 -> "#{Float.round(bytes / 1_024, 1)}KB"
      true -> "#{bytes}B"
    end
  end

  defp format_bytes(bytes), do: to_string(bytes)

  defp format_number(num) when is_integer(num) and num >= 1000 do
    num
    |> Integer.to_string()
    |> String.reverse()
    |> String.replace(~r/(\d{3})(?=\d)/, "\\1,")
    |> String.reverse()
  end

  defp format_number(num), do: to_string(num)

  defp metric_value_color(%{status: :error}), do: "text-red-400"
  defp metric_value_color(%{status: :warning}), do: "text-yellow-400"
  defp metric_value_color(%{status: :success}), do: "text-green-400"

  defp metric_value_color(%{value: value, threshold: threshold})
       when is_number(value) and is_number(threshold) do
    if value > threshold, do: "text-red-400", else: "text-green-400"
  end

  defp metric_value_color(_), do: "text-green-400"

  defp trend_color(:up), do: "text-green-400"
  defp trend_color(:down), do: "text-red-400"
  defp trend_color(:stable), do: "text-gray-400"
  defp trend_color(_), do: "text-gray-400"

  defp trend_symbol(:up), do: "↑"
  defp trend_symbol(:down), do: "↓"
  defp trend_symbol(:stable), do: "→"
  defp trend_symbol(_), do: ""

  defp progress_percentage(value, max_value)
       when is_number(value) and is_number(max_value) and max_value > 0 do
    min(100, (value / max_value * 100) |> Float.round(1))
  end

  defp progress_percentage(_, _), do: 0

  defp progress_bar_color(value, max_value)
       when is_number(value) and is_number(max_value) and max_value > 0 do
    percentage = value / max_value * 100

    cond do
      percentage > 90 -> "bg-red-500"
      percentage > 70 -> "bg-yellow-500"
      true -> "bg-green-500"
    end
  end

  defp progress_bar_color(_, _), do: "bg-gray-500"
end
