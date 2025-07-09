defmodule OtpSupervisorWeb.Components.Layout.TerminalPanelLayout do
  use Phoenix.LiveComponent

  @moduledoc """
  Flexible terminal-themed panel layout system for different page structures.
  
  Supports various layout configurations:
  - :two_panel - Side-by-side panels (used by DocsLive, SupervisorLive)
  - :three_panel - Three column layout (used by ArsenalLive)
  - :grid - Grid layout (used by SystemDashboardLive)
  - :stacked - Vertically stacked panels
  """

  attr :layout_type, :atom, values: [:two_panel, :three_panel, :grid, :stacked], required: true
  attr :panels, :list, required: true
  attr :resizable, :boolean, default: false
  attr :min_panel_width, :string, default: "min-w-64"
  attr :bg_color, :string, default: "bg-gray-900"
  attr :text_color, :string, default: "text-green-400"
  attr :border_color, :string, default: "border-green-500/30"
  attr :gap, :string, default: "gap-4"
  attr :padding, :string, default: "p-4"
  attr :class, :string, default: ""

  slot :header
  slot :footer

  def render(assigns) do
    ~H"""
    <div class={[
      "flex flex-col h-full",
      @bg_color,
      @text_color,
      @class
    ]}>
      <!-- Header -->
      <%= if @header != [] do %>
        <div class="flex-shrink-0">
          <%= render_slot(@header) %>
        </div>
      <% end %>

      <!-- Main content area -->
      <div class="flex-1 overflow-hidden">
        <%= case @layout_type do %>
          <% :two_panel -> %>
            <div class="flex h-full gap-4">
              <%= render_two_panel_layout(assigns) %>
            </div>
          
          <% :three_panel -> %>
            <div class="flex h-full gap-4">
              <%= render_three_panel_layout(assigns) %>
            </div>
          
          <% :grid -> %>
            <div class={[
              "grid h-full gap-4",
              grid_classes(@panels)
            ]}>
              <%= render_grid_layout(assigns) %>
            </div>
          
          <% :stacked -> %>
            <div class="flex flex-col h-full gap-4">
              <%= render_stacked_layout(assigns) %>
            </div>
        <% end %>
      </div>

      <!-- Footer -->
      <%= if @footer != [] do %>
        <div class="flex-shrink-0">
          <%= render_slot(@footer) %>
        </div>
      <% end %>
    </div>
    """
  end

  def mount(socket) do
    {:ok, socket}
  end

  def update(assigns, socket) do
    {:ok, assign(socket, assigns)}
  end

  # Layout rendering functions

  defp render_two_panel_layout(assigns) do
    ~H"""
    <%= for {panel, index} <- Enum.with_index(@panels) do %>
      <div class={[
        panel_classes(panel, @layout_type, index),
        @min_panel_width,
        @border_color
      ]}>
        <div class="flex-1 overflow-auto h-full">
          <%= render_panel_content(panel) %>
        </div>
      </div>
    <% end %>
    """
  end

  defp render_three_panel_layout(assigns) do
    ~H"""
    <%= for {panel, index} <- Enum.with_index(@panels) do %>
      <div class={[
        panel_classes(panel, @layout_type, index),
        three_panel_width_classes(index, length(@panels)),
        @border_color
      ]}>
        <div class="flex-1 overflow-auto h-full">
          <%= render_panel_content(panel) %>
        </div>
      </div>
    <% end %>
    """
  end

  defp render_grid_layout(assigns) do
    ~H"""
    <%= for {panel, index} <- Enum.with_index(@panels) do %>
      <div class={[
        panel_classes(panel, @layout_type, index),
        grid_item_classes(panel),
        @border_color
      ]}>
        <div class="flex-1 overflow-auto h-full">
          <%= render_panel_content(panel) %>
        </div>
      </div>
    <% end %>
    """
  end

  defp render_stacked_layout(assigns) do
    ~H"""
    <%= for {panel, index} <- Enum.with_index(@panels) do %>
      <div class={[
        panel_classes(panel, @layout_type, index),
        stacked_height_classes(panel),
        @border_color
      ]}>
        <div class="flex-1 overflow-auto h-full">
          <%= render_panel_content(panel) %>
        </div>
      </div>
    <% end %>
    """
  end

  # Panel content rendering

  defp render_panel_content(%{component: component, assigns: component_assigns}) do
    assigns = %{component: component, component_assigns: component_assigns}
    
    ~H"""
    <.live_component
      module={@component}
      id={@component_assigns.id}
      {@component_assigns}
    />
    """
  end

  defp render_panel_content(%{content: content}) when is_binary(content) do
    Phoenix.HTML.raw(content)
  end

  defp render_panel_content(%{slot: slot_content}) do
    slot_content
  end

  defp render_panel_content(_panel) do
    Phoenix.HTML.raw("<div class='p-4 text-green-400/50 font-mono'>No content provided</div>")
  end

  # Action rendering

  defp render_action(%{type: :button, label: label, event: event}) do
    Phoenix.HTML.raw(~s(<button class="px-2 py-1 text-xs bg-green-500/20 border border-green-500/30 rounded text-green-400 font-mono hover:bg-green-500/30 transition-colors" phx-click="#{event}">#{label}</button>))
  end

  defp render_action(%{type: :link, label: label, href: href}) do
    Phoenix.HTML.raw(~s(<a href="#{href}" class="px-2 py-1 text-xs bg-green-500/20 border border-green-500/30 rounded text-green-400 font-mono hover:bg-green-500/30 transition-colors">#{label}</a>))
  end

  defp render_action(%{type: :toggle, label: label, event: event, active: active}) do
    active_class = if active, do: "bg-green-500/30", else: "bg-green-500/20"
    Phoenix.HTML.raw(~s(<button class="px-2 py-1 text-xs #{active_class} border border-green-500/30 rounded text-green-400 font-mono hover:bg-green-500/30 transition-colors" phx-click="#{event}">#{label}</button>))
  end

  defp render_action(action) when is_binary(action) do
    Phoenix.HTML.raw(action)
  end

  defp render_action(_), do: nil

  # CSS class helpers

  defp panel_classes(panel, layout_type, index) do
    base_classes = [
      "flex flex-col bg-gray-900 border rounded overflow-hidden"
    ]

    panel_specific_classes = panel[:class] || []
    layout_specific_classes = layout_specific_panel_classes(layout_type, index)

    base_classes ++ panel_specific_classes ++ layout_specific_classes
  end

  defp layout_specific_panel_classes(:two_panel, 0), do: ["flex-1"]
  defp layout_specific_panel_classes(:two_panel, 1), do: ["flex-1"]
  defp layout_specific_panel_classes(:three_panel, _), do: ["flex-1"]
  defp layout_specific_panel_classes(:grid, _), do: []
  defp layout_specific_panel_classes(:stacked, _), do: []

  defp three_panel_width_classes(0, 3), do: ["w-1/4", "min-w-64"]  # Left panel
  defp three_panel_width_classes(1, 3), do: ["flex-1"]              # Center panel
  defp three_panel_width_classes(2, 3), do: ["w-1/3", "min-w-80"]   # Right panel
  defp three_panel_width_classes(index, 2) when index < 2, do: ["flex-1"]
  defp three_panel_width_classes(_, _), do: ["flex-1"]

  defp grid_classes(panels) do
    # Look at the actual spans to determine grid structure
    max_cols = panels |> Enum.map(fn p -> Map.get(p[:span] || %{}, :cols, 1) end) |> Enum.max(fn -> 1 end)
    
    case max_cols do
      1 -> "grid-cols-1"
      2 -> "grid-cols-2"
      3 -> "grid-cols-3"
      _ -> "grid-cols-#{max_cols}"
    end
  end

  defp grid_item_classes(%{span: span}) when is_map(span) do
    col_span = Map.get(span, :cols, 1)
    row_span = Map.get(span, :rows, 1)
    ["col-span-#{col_span}", "row-span-#{row_span}"]
  end

  defp grid_item_classes(%{span: span}) when is_integer(span) do
    ["col-span-#{span}"]
  end

  defp grid_item_classes(_), do: []

  defp stacked_height_classes(%{height: height}) when is_binary(height), do: [height]
  defp stacked_height_classes(%{height: :auto}), do: ["flex-1"]
  defp stacked_height_classes(%{height: :fixed}), do: ["flex-none"]
  defp stacked_height_classes(_), do: ["flex-1"]
end