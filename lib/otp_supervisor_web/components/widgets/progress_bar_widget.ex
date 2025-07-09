defmodule OtpSupervisorWeb.Components.Widgets.ProgressBarWidget do
  use Phoenix.Component

  @moduledoc """
  Visual progress bar widget for various progress indicators.

  Supports multiple styles and configurations:
  - Horizontal and vertical orientations
  - Different color schemes and themes
  - Animated progress updates
  - Text labels and percentage displays
  """

  attr :value, :integer, required: true
  attr :max_value, :integer, default: 100
  attr :orientation, :atom, values: [:horizontal, :vertical], default: :horizontal
  attr :size, :atom, values: [:small, :medium, :large], default: :medium
  attr :color, :atom, values: [:green, :blue, :red, :yellow, :purple], default: :green
  attr :show_percentage, :boolean, default: true
  attr :show_label, :boolean, default: false
  attr :label, :string, default: ""
  attr :animated, :boolean, default: true
  attr :striped, :boolean, default: false
  attr :class, :string, default: ""

  def render(assigns) do
    ~H"""
    <div class={[
      "flex items-center",
      if(@orientation == :vertical, do: "flex-col", else: ""),
      @class
    ]}>
      <!-- Label -->
      <%= if @show_label and @label != "" do %>
        <div class={[
          "text-green-400/70 font-mono text-xs",
          if(@orientation == :vertical, do: "mb-2", else: "mr-2")
        ]}>
          {@label}
        </div>
      <% end %>
      
    <!-- Progress bar container -->
      <div class={[
        "relative overflow-hidden rounded",
        container_classes(@orientation, @size),
        background_classes()
      ]}>
        <!-- Progress fill -->
        <div
          class={[
            "rounded transition-all duration-300",
            fill_classes(@orientation, @size),
            progress_color_classes(@color),
            if(@animated, do: "transition-all duration-500 ease-out", else: ""),
            if(@striped,
              do:
                "bg-gradient-to-r from-current via-transparent to-current bg-[length:20px_20px] animate-pulse",
              else: ""
            )
          ]}
          style={progress_style(@value, @max_value, @orientation)}
        >
        </div>
        
    <!-- Percentage text overlay -->
        <%= if @show_percentage and @orientation == :horizontal do %>
          <div class="absolute inset-0 flex items-center justify-center">
            <span class="text-xs font-mono font-bold text-gray-900 mix-blend-difference">
              {progress_percentage(@value, @max_value)}%
            </span>
          </div>
        <% end %>
      </div>
      
    <!-- Percentage text (vertical) -->
      <%= if @show_percentage and @orientation == :vertical do %>
        <div class="mt-2 text-xs font-mono font-bold text-green-400">
          {progress_percentage(@value, @max_value)}%
        </div>
      <% end %>
    </div>
    """
  end

  # Helper functions

  defp container_classes(:horizontal, :small), do: "w-24 h-2"
  defp container_classes(:horizontal, :medium), do: "w-32 h-3"
  defp container_classes(:horizontal, :large), do: "w-48 h-4"
  defp container_classes(:vertical, :small), do: "w-2 h-24"
  defp container_classes(:vertical, :medium), do: "w-3 h-32"
  defp container_classes(:vertical, :large), do: "w-4 h-48"

  defp fill_classes(:horizontal, _), do: "h-full"
  defp fill_classes(:vertical, _), do: "w-full"

  defp background_classes, do: "bg-gray-700"

  defp progress_color_classes(:green), do: "bg-green-500"
  defp progress_color_classes(:blue), do: "bg-blue-500"
  defp progress_color_classes(:red), do: "bg-red-500"
  defp progress_color_classes(:yellow), do: "bg-yellow-500"
  defp progress_color_classes(:purple), do: "bg-purple-500"

  defp progress_style(value, max_value, :horizontal) do
    percentage = progress_percentage(value, max_value)
    "width: #{percentage}%"
  end

  defp progress_style(value, max_value, :vertical) do
    percentage = progress_percentage(value, max_value)
    "height: #{percentage}%"
  end

  defp progress_percentage(value, max_value) when max_value > 0 do
    min(100, Float.round(value / max_value * 100, 1))
  end

  defp progress_percentage(_, _), do: 0
end
