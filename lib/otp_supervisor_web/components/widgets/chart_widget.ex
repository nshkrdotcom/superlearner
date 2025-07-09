defmodule OtpSupervisorWeb.Components.Widgets.ChartWidget do
  use Phoenix.LiveComponent

  @moduledoc """
  Data visualization widget for various charts and graphs.
  
  Supports multiple chart types and visualization options:
  - Line charts for time series data
  - Bar charts for categorical data
  - Pie charts for proportional data
  - Area charts for cumulative data
  - Real-time data updates
  - Interactive zooming and panning
  """

  attr :chart_type, :atom, values: [:line, :bar, :pie, :area], required: true
  attr :data, :list, required: true
  attr :title, :string, default: ""
  attr :x_axis_label, :string, default: ""
  attr :y_axis_label, :string, default: ""
  attr :height, :integer, default: 300
  attr :width, :integer, default: 400
  attr :color_scheme, :atom, values: [:green, :blue, :multi], default: :green
  attr :show_legend, :boolean, default: true
  attr :show_grid, :boolean, default: true
  attr :animated, :boolean, default: true
  attr :interactive, :boolean, default: true
  attr :real_time, :boolean, default: false
  attr :max_data_points, :integer, default: 100
  attr :class, :string, default: ""

  def render(assigns) do
    ~H"""
    <div class={[
      "bg-gray-900 border border-green-500/30 rounded text-green-400 h-full flex flex-col",
      @class
    ]}>
      <!-- Header -->
      <div class="flex items-center justify-between p-3 border-b border-green-500/20">
        <div class="flex items-center space-x-4">
          <h3 class="text-sm font-mono font-bold text-green-300">
            <%= if @title != "", do: @title, else: chart_type_name(@chart_type) %>
          </h3>
          <span class="text-xs text-green-400/70 font-mono">
            (<%= length(@data) %> points)
          </span>
        </div>
        
        <div class="flex items-center space-x-2">
          <!-- Chart controls -->
          <button
            phx-target={@myself}
            phx-click="toggle_grid"
            class={[
              "px-2 py-1 text-xs border border-green-500/30 rounded font-mono hover:bg-green-500/30 transition-colors",
              if(@show_grid, do: "bg-green-500/20 text-green-400", else: "bg-gray-500/20 text-gray-400")
            ]}
          >
            Grid
          </button>
          
          <button
            phx-target={@myself}
            phx-click="toggle_legend"
            class={[
              "px-2 py-1 text-xs border border-green-500/30 rounded font-mono hover:bg-green-500/30 transition-colors",
              if(@show_legend, do: "bg-green-500/20 text-green-400", else: "bg-gray-500/20 text-gray-400")
            ]}
          >
            Legend
          </button>
          
          <button
            phx-target={@myself}
            phx-click="reset_zoom"
            class="px-2 py-1 text-xs bg-blue-500/20 border border-blue-500/30 rounded text-blue-400 font-mono hover:bg-blue-500/30 transition-colors"
          >
            Reset
          </button>
          
          <button
            phx-target={@myself}
            phx-click="export_chart"
            class="px-2 py-1 text-xs bg-purple-500/20 border border-purple-500/30 rounded text-purple-400 font-mono hover:bg-purple-500/30 transition-colors"
          >
            Export
          </button>
        </div>
      </div>

      <!-- Chart area -->
      <div class="flex-1 flex flex-col min-h-0">
        <%= if @data == [] do %>
          <div class="flex-1 flex items-center justify-center">
            <div class="text-center">
              <div class="text-green-400/50 text-sm font-mono">No data available</div>
              <div class="text-green-400/30 text-xs font-mono mt-1">
                Add data to display <%= chart_type_name(@chart_type) %> chart
              </div>
            </div>
          </div>
        <% else %>
          <div class="flex-1 relative">
            <!-- Chart container -->
            <div 
              class="absolute inset-0 m-4"
              id={"chart-#{@myself}"}
              phx-hook={if @interactive, do: "ChartWidget", else: nil}
              data-chart-type={@chart_type}
              data-chart-data={Jason.encode!(@data)}
              data-chart-config={Jason.encode!(chart_config(assigns))}
            >
              <!-- SVG Chart will be rendered here by JavaScript -->
              <%= render_chart(assigns) %>
            </div>
          </div>
        <% end %>
      </div>

      <!-- Legend -->
      <%= if @show_legend and @data != [] do %>
        <div class="border-t border-green-500/20 p-3">
          <div class="flex items-center justify-center space-x-4 text-xs">
            <%= for {label, index} <- Enum.with_index(get_legend_labels(@data, @chart_type)) do %>
              <div class="flex items-center space-x-2">
                <div class={[
                  "w-3 h-3 rounded",
                  legend_color(index, @color_scheme)
                ]}></div>
                <span class="text-green-400/70 font-mono"><%= label %></span>
              </div>
            <% end %>
          </div>
        </div>
      <% end %>

      <!-- Chart statistics -->
      <div class="border-t border-green-500/20 p-2">
        <div class="flex items-center justify-between text-xs">
          <div class="flex items-center space-x-4">
            <span class="text-green-400/70 font-mono">
              Min: <span class="text-green-400"><%= format_value(get_min_value(@data)) %></span>
            </span>
            <span class="text-green-400/70 font-mono">
              Max: <span class="text-green-400"><%= format_value(get_max_value(@data)) %></span>
            </span>
            <span class="text-green-400/70 font-mono">
              Avg: <span class="text-green-400"><%= format_value(get_avg_value(@data)) %></span>
            </span>
          </div>
          <div class="text-green-400/70 font-mono">
            Last updated: <%= format_timestamp(DateTime.utc_now()) %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  def mount(socket) do
    {:ok, socket}
  end

  def update(assigns, socket) do
    socket = assign(socket, assigns)
    
    # Set up real-time updates if enabled
    if Map.get(assigns, :real_time, false) do
      Process.send_after(self(), :update_chart_data, 1000)
    end

    {:ok, socket}
  end

  # Event handlers

  def handle_event("toggle_grid", _params, socket) do
    {:noreply, assign(socket, :show_grid, !socket.assigns.show_grid)}
  end

  def handle_event("toggle_legend", _params, socket) do
    {:noreply, assign(socket, :show_legend, !socket.assigns.show_legend)}
  end

  def handle_event("reset_zoom", _params, socket) do
    send(self(), :reset_chart_zoom)
    {:noreply, socket}
  end

  def handle_event("export_chart", _params, socket) do
    send(self(), :export_chart)
    {:noreply, socket}
  end

  # Private helper functions

  defp chart_type_name(:line), do: "Line Chart"
  defp chart_type_name(:bar), do: "Bar Chart"
  defp chart_type_name(:pie), do: "Pie Chart"
  defp chart_type_name(:area), do: "Area Chart"

  defp chart_config(assigns) do
    %{
      type: assigns.chart_type,
      height: assigns.height,
      width: assigns.width,
      animated: assigns.animated,
      interactive: assigns.interactive,
      color_scheme: assigns.color_scheme,
      show_grid: assigns.show_grid,
      x_axis_label: assigns.x_axis_label,
      y_axis_label: assigns.y_axis_label
    }
  end

  defp render_chart(%{chart_type: :line, data: data} = assigns) do
    ~H"""
    <svg viewBox="0 0 400 300" class="w-full h-full">
      <!-- Grid lines -->
      <%= if @show_grid do %>
        <defs>
          <pattern id={"grid-#{@myself}"} width="40" height="30" patternUnits="userSpaceOnUse">
            <path d="M 40 0 L 0 0 0 30" fill="none" stroke="#10b981" stroke-width="0.5" opacity="0.2"/>
          </pattern>
        </defs>
        <rect width="100%" height="100%" fill={"url(#grid-#{@myself})"} />
      <% end %>
      
      <!-- Chart line -->
      <%= if length(data) > 1 do %>
        <polyline
          points={line_points(data)}
          fill="none"
          stroke="#10b981"
          stroke-width="2"
          class={if @animated, do: "animate-pulse", else: ""}
        />
      <% end %>
      
      <!-- Data points -->
      <%= for {point, index} <- Enum.with_index(data) do %>
        <circle
          cx={point_x(point, index, length(data))}
          cy={point_y(point, data)}
          r="3"
          fill="#10b981"
          class="hover:fill-green-300 transition-colors cursor-pointer"
        />
      <% end %>
    </svg>
    """
  end

  defp render_chart(%{chart_type: :bar, data: data} = assigns) do
    ~H"""
    <svg viewBox="0 0 400 300" class="w-full h-full">
      <!-- Grid lines -->
      <%= if @show_grid do %>
        <defs>
          <pattern id={"grid-#{@myself}"} width="40" height="30" patternUnits="userSpaceOnUse">
            <path d="M 40 0 L 0 0 0 30" fill="none" stroke="#10b981" stroke-width="0.5" opacity="0.2"/>
          </pattern>
        </defs>
        <rect width="100%" height="100%" fill={"url(#grid-#{@myself})"} />
      <% end %>
      
      <!-- Bars -->
      <%= for {point, index} <- Enum.with_index(data) do %>
        <rect
          x={bar_x(index, length(data))}
          y={bar_y(point, data)}
          width={bar_width(length(data))}
          height={bar_height(point, data)}
          fill="#10b981"
          class="hover:fill-green-300 transition-colors cursor-pointer"
        />
      <% end %>
    </svg>
    """
  end

  defp render_chart(%{chart_type: :pie, data: data} = assigns) do
    ~H"""
    <svg viewBox="0 0 400 300" class="w-full h-full">
      <!-- Pie slices -->
      <%= for {slice, index} <- Enum.with_index(pie_slices(data)) do %>
        <path
          d={slice.path}
          fill={pie_color(index, @color_scheme)}
          class="hover:opacity-80 transition-opacity cursor-pointer"
          transform="translate(200, 150)"
        />
      <% end %>
    </svg>
    """
  end

  defp render_chart(%{chart_type: :area, data: data} = assigns) do
    ~H"""
    <svg viewBox="0 0 400 300" class="w-full h-full">
      <!-- Grid lines -->
      <%= if @show_grid do %>
        <defs>
          <pattern id={"grid-#{@myself}"} width="40" height="30" patternUnits="userSpaceOnUse">
            <path d="M 40 0 L 0 0 0 30" fill="none" stroke="#10b981" stroke-width="0.5" opacity="0.2"/>
          </pattern>
        </defs>
        <rect width="100%" height="100%" fill={"url(#grid-#{@myself})"} />
      <% end %>
      
      <!-- Area fill -->
      <%= if length(data) > 1 do %>
        <polygon
          points={area_points(data)}
          fill="rgba(16, 185, 129, 0.3)"
          stroke="#10b981"
          stroke-width="2"
        />
      <% end %>
    </svg>
    """
  end

  defp get_legend_labels(data, chart_type) do
    case chart_type do
      :pie -> Enum.map(data, fn point -> point.label || "Series #{point.index}" end)
      _ -> ["Data Series"]
    end
  end

  defp legend_color(index, :green), do: "bg-green-500"
  defp legend_color(index, :blue), do: "bg-blue-500"
  defp legend_color(index, :multi) do
    colors = ["bg-green-500", "bg-blue-500", "bg-yellow-500", "bg-red-500", "bg-purple-500"]
    Enum.at(colors, rem(index, length(colors)))
  end

  defp pie_color(index, :green), do: "#10b981"
  defp pie_color(index, :blue), do: "#3b82f6"
  defp pie_color(index, :multi) do
    colors = ["#10b981", "#3b82f6", "#f59e0b", "#ef4444", "#8b5cf6"]
    Enum.at(colors, rem(index, length(colors)))
  end

  defp get_min_value(data) do
    data
    |> Enum.map(fn point -> get_point_value(point) end)
    |> Enum.min()
  end

  defp get_max_value(data) do
    data
    |> Enum.map(fn point -> get_point_value(point) end)
    |> Enum.max()
  end

  defp get_avg_value(data) do
    values = Enum.map(data, fn point -> get_point_value(point) end)
    if length(values) > 0 do
      Float.round(Enum.sum(values) / length(values), 2)
    else
      0
    end
  end

  defp get_point_value(point) do
    cond do
      Map.has_key?(point, :value) -> point.value
      Map.has_key?(point, :y) -> point.y
      Map.has_key?(point, :cpu) -> point.cpu
      Map.has_key?(point, :memory) -> point.memory
      true -> 0
    end
  end

  defp format_value(value) when is_number(value), do: to_string(value)
  defp format_value(_), do: "N/A"

  defp format_timestamp(%DateTime{} = dt) do
    Calendar.strftime(dt, "%H:%M:%S")
  end

  defp format_timestamp(_), do: "N/A"

  # Chart rendering math helpers

  defp line_points(data) do
    data
    |> Enum.with_index()
    |> Enum.map(fn {point, index} ->
      x = point_x(point, index, length(data))
      y = point_y(point, data)
      "#{x},#{y}"
    end)
    |> Enum.join(" ")
  end

  defp area_points(data) do
    line_points = line_points(data)
    first_x = point_x(Enum.at(data, 0), 0, length(data))
    last_x = point_x(Enum.at(data, -1), length(data) - 1, length(data))
    "#{first_x},300 #{line_points} #{last_x},300"
  end

  defp point_x(_point, index, total) do
    (index / (total - 1)) * 360 + 20
  end

  defp point_y(point, data) do
    value = get_point_value(point)
    max_val = get_max_value(data)
    min_val = get_min_value(data)
    
    if max_val == min_val do
      150
    else
      280 - ((value - min_val) / (max_val - min_val)) * 260
    end
  end

  defp bar_x(index, total) do
    bar_width = 360 / total
    index * bar_width + 20
  end

  defp bar_y(point, data) do
    point_y(point, data)
  end

  defp bar_width(total) do
    max(360 / total - 2, 1)
  end

  defp bar_height(point, data) do
    value = get_point_value(point)
    max_val = get_max_value(data)
    min_val = get_min_value(data)
    
    if max_val == min_val do
      1
    else
      ((value - min_val) / (max_val - min_val)) * 260
    end
  end

  defp pie_slices(data) do
    total = Enum.sum(Enum.map(data, fn point -> get_point_value(point) end))
    
    if total > 0 do
      {slices, _} = Enum.reduce(data, {[], 0}, fn point, {slices, start_angle} ->
        value = get_point_value(point)
        angle = (value / total) * 360
        
        slice = %{
          path: pie_slice_path(start_angle, angle),
          angle: angle,
          value: value
        }
        
        {[slice | slices], start_angle + angle}
      end)
      
      Enum.reverse(slices)
    else
      []
    end
  end

  defp pie_slice_path(start_angle, angle) do
    start_radians = (start_angle * :math.pi()) / 180
    end_radians = ((start_angle + angle) * :math.pi()) / 180
    
    x1 = 80 * :math.cos(start_radians)
    y1 = 80 * :math.sin(start_radians)
    x2 = 80 * :math.cos(end_radians)
    y2 = 80 * :math.sin(end_radians)
    
    large_arc = if angle > 180, do: 1, else: 0
    
    "M 0,0 L #{x1},#{y1} A 80,80 0 #{large_arc},1 #{x2},#{y2} Z"
  end
end