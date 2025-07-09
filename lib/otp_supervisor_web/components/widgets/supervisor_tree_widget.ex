defmodule OtpSupervisorWeb.Components.Widgets.SupervisorTreeWidget do
  use Phoenix.LiveComponent

  @moduledoc """
  Hierarchical supervisor tree widget with interactive navigation.

  Displays the OTP supervisor tree structure with:
  - Collapsible tree nodes
  - Interactive selection and drilling down
  - Process health indicators
  - Real-time updates
  - Zoom and pan capabilities
  """

  attr :supervisors, :list, required: true
  attr :selected_supervisor, :map, default: nil
  attr :show_children, :boolean, default: true
  attr :compact_mode, :boolean, default: false
  attr :max_depth, :integer, default: 10
  attr :class, :string, default: ""

  def render(assigns) do
    ~H"""
    <div class={[
      "bg-gray-900 border border-green-500/30 rounded text-green-400 h-full flex flex-col",
      @class
    ]}>
      <!-- Header -->
      <div class="flex items-center justify-between p-3 border-b border-green-500/20">
        <h3 class="text-sm font-mono font-bold text-green-300">
          Supervisor Tree
        </h3>
        <div class="flex items-center space-x-2">
          <button
            phx-target={@myself}
            phx-click="collapse_all"
            class="px-2 py-1 text-xs bg-green-500/20 border border-green-500/30 rounded text-green-400 font-mono hover:bg-green-500/30 transition-colors"
          >
            Collapse All
          </button>
          <button
            phx-target={@myself}
            phx-click="expand_all"
            class="px-2 py-1 text-xs bg-green-500/20 border border-green-500/30 rounded text-green-400 font-mono hover:bg-green-500/30 transition-colors"
          >
            Expand All
          </button>
          <button
            phx-target={@myself}
            phx-click="refresh"
            class="px-2 py-1 text-xs bg-blue-500/20 border border-blue-500/30 rounded text-blue-400 font-mono hover:bg-blue-500/30 transition-colors"
          >
            Refresh
          </button>
        </div>
      </div>
      
    <!-- Tree view -->
      <div class="flex-1 overflow-auto p-4">
        <%= if @supervisors == [] do %>
          <div class="text-center py-8">
            <div class="text-green-400/50 text-sm font-mono">No supervisors found</div>
          </div>
        <% else %>
          <div class="space-y-2">
            <%= for supervisor <- @supervisors do %>
              {render_supervisor_node(assigns, supervisor, 0)}
            <% end %>
          </div>
        <% end %>
      </div>
      
    <!-- Footer with selected supervisor info -->
      <%= if @selected_supervisor do %>
        <div class="border-t border-green-500/20 p-3">
          <div class="text-sm">
            <div class="font-mono font-bold text-green-300 mb-1">
              Selected: {@selected_supervisor.name}
            </div>
            <div class="grid grid-cols-2 gap-4 text-xs">
              <div>
                <span class="text-green-400/70">PID:</span>
                <span class="text-green-400 font-mono">{@selected_supervisor.pid}</span>
              </div>
              <div>
                <span class="text-green-400/70">Strategy:</span>
                <span class="text-green-400 font-mono">{@selected_supervisor.strategy}</span>
              </div>
              <div>
                <span class="text-green-400/70">Children:</span>
                <span class="text-green-400 font-mono">{@selected_supervisor.children_count}</span>
              </div>
              <div>
                <span class="text-green-400/70">Status:</span>
                <span class={[
                  "font-mono",
                  supervisor_status_color(@selected_supervisor.status)
                ]}>
                  {String.capitalize(to_string(@selected_supervisor.status))}
                </span>
              </div>
            </div>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  def mount(socket) do
    {:ok,
     socket
     |> assign(:expanded_nodes, MapSet.new())
     |> assign(:hovered_node, nil)
     |> assign(:children_by_supervisor, %{})}
  end

  def update(assigns, socket) do
    {:ok, assign(socket, assigns)}
  end

  # Event handlers

  def handle_event("toggle_node", %{"supervisor_id" => supervisor_id}, socket) do
    expanded = socket.assigns.expanded_nodes

    new_expanded =
      if MapSet.member?(expanded, supervisor_id) do
        MapSet.delete(expanded, supervisor_id)
      else
        MapSet.put(expanded, supervisor_id)
      end

    # If expanding, fetch children for this supervisor
    if !MapSet.member?(expanded, supervisor_id) do
      send(self(), {:fetch_supervisor_children, supervisor_id})
    end

    {:noreply, assign(socket, :expanded_nodes, new_expanded)}
  end

  def handle_event("select_supervisor", %{"supervisor_id" => supervisor_id}, socket) do
    supervisor = find_supervisor(supervisor_id, socket.assigns.supervisors)

    # Notify parent component via Phoenix.LiveView.send_update
    send(self(), {:supervisor_selected, supervisor_id})

    {:noreply, assign(socket, :selected_supervisor, supervisor)}
  end

  def handle_event("collapse_all", _params, socket) do
    {:noreply, assign(socket, :expanded_nodes, MapSet.new())}
  end

  def handle_event("expand_all", _params, socket) do
    all_ids = get_all_supervisor_ids(socket.assigns.supervisors)
    {:noreply, assign(socket, :expanded_nodes, MapSet.new(all_ids))}
  end

  def handle_event("refresh", _params, socket) do
    send(self(), :refresh_supervisors)
    {:noreply, socket}
  end

  def handle_event(
        "supervisor_action",
        %{"action" => action, "supervisor_id" => supervisor_id},
        socket
      ) do
    send(self(), {:supervisor_action, action, supervisor_id})
    {:noreply, socket}
  end

  # Private rendering functions

  defp render_supervisor_node(assigns, supervisor, depth) do
    expanded = MapSet.member?(assigns.expanded_nodes, supervisor.id)
    assigns = assign(assigns, :expanded, expanded)
    assigns = assign(assigns, :supervisor, supervisor)
    assigns = assign(assigns, :depth, depth)

    ~H"""
    <div class={[
      "flex flex-col",
      if(@depth > 0, do: "ml-#{min(@depth * 4, 16)}", else: "")
    ]}>
      <!-- Supervisor node -->
      <div class={[
        "flex items-center space-x-2 p-2 rounded hover:bg-green-500/5 transition-colors cursor-pointer",
        if(@selected_supervisor && @selected_supervisor.id == @supervisor.id,
          do: "bg-green-500/10",
          else: ""
        )
      ]}>
        <!-- Expand/collapse button -->
        <%= if has_children?(@supervisor) do %>
          <button
            phx-target={@myself}
            phx-click="toggle_node"
            phx-value-supervisor_id={@supervisor.id}
            class="w-4 h-4 flex items-center justify-center text-green-400 hover:text-green-300"
          >
            <%= if @expanded do %>
              <span class="text-xs">▼</span>
            <% else %>
              <span class="text-xs">▶</span>
            <% end %>
          </button>
        <% else %>
          <div class="w-4 h-4 flex items-center justify-center">
            <span class="text-xs text-green-400/30">•</span>
          </div>
        <% end %>
        
    <!-- Tree connector lines -->
        <%= if @depth > 0 do %>
          <div class="flex items-center">
            <div class="w-4 h-px bg-green-500/30"></div>
          </div>
        <% end %>
        
    <!-- Supervisor icon and info -->
        <div class="flex items-center space-x-2 flex-1">
          <div class="flex items-center space-x-1">
            <span class="text-green-400">{supervisor_icon(@supervisor)}</span>
            <span class={[
              "w-2 h-2 rounded-full",
              supervisor_health_color(@supervisor.status)
            ]}>
            </span>
          </div>

          <button
            phx-target={@myself}
            phx-click="select_supervisor"
            phx-value-supervisor_id={@supervisor.id}
            class="flex items-center space-x-2 text-left hover:text-green-300 transition-colors"
          >
            <span class="font-mono text-sm text-green-400">
              {@supervisor.name}
            </span>
            <span class="font-mono text-xs text-green-400/70">
              ({@supervisor.children_count} children)
            </span>
          </button>
        </div>
        
    <!-- Supervisor actions -->
        <div class="flex items-center space-x-1">
          <button
            phx-target={@myself}
            phx-click="supervisor_action"
            phx-value-action="inspect"
            phx-value-supervisor_id={@supervisor.id}
            class="p-1 text-xs text-blue-400 hover:text-blue-300 transition-colors"
            title="Inspect"
          >
            🔍
          </button>
          <button
            phx-target={@myself}
            phx-click="supervisor_action"
            phx-value-action="restart"
            phx-value-supervisor_id={@supervisor.id}
            class="p-1 text-xs text-yellow-400 hover:text-yellow-300 transition-colors"
            title="Restart"
          >
            🔄
          </button>
        </div>
        
    <!-- Status indicator -->
        <div class={[
          "px-2 py-1 rounded text-xs font-mono",
          supervisor_status_classes(@supervisor.status)
        ]}>
          {supervisor_status_text(@supervisor.status)}
        </div>
      </div>
      
    <!-- Children (if expanded) -->
      <%= if @expanded && @show_children do %>
        <div class="ml-6 mt-2 space-y-1">
          <%= for child <- Map.get(@children_by_supervisor, @supervisor.id, []) do %>
            <div class="flex items-center space-x-2 p-2 rounded hover:bg-green-500/5 transition-colors">
              <!-- Tree connector -->
              <div class="w-4 h-4 flex items-center justify-center">
                <span class="text-xs text-green-400/30">├</span>
              </div>
              
    <!-- Child info -->
              <div class="flex items-center space-x-2 flex-1">
                <span class="text-green-400/70">{child_icon(child)}</span>
                <span class={[
                  "w-2 h-2 rounded-full",
                  child_health_color(child.status)
                ]}>
                </span>
                <span class="font-mono text-sm text-green-400/80">
                  {child.name}
                </span>
                <span class="font-mono text-xs text-green-400/50">
                  {child.pid}
                </span>
              </div>
              
    <!-- Child status -->
              <div class={[
                "px-2 py-1 rounded text-xs font-mono",
                child_status_classes(child.status)
              ]}>
                {child_status_text(child.status)}
              </div>
            </div>
          <% end %>
        </div>
      <% end %>
    </div>
    """
  end

  # Private helper functions

  defp find_supervisor(supervisor_id, supervisors) do
    Enum.find(supervisors, &(&1.id == supervisor_id))
  end

  defp get_all_supervisor_ids(supervisors) do
    Enum.map(supervisors, & &1.id)
  end

  defp has_children?(supervisor) do
    supervisor.children_count > 0
  end

  defp supervisor_icon(supervisor) do
    case supervisor.strategy do
      :one_for_one -> "🎯"
      :one_for_all -> "🔗"
      :rest_for_one -> "⚡"
      :simple_one_for_one -> "🔄"
      _ -> "👥"
    end
  end

  defp child_icon(child) do
    case child.type do
      :supervisor -> "👥"
      :worker -> "⚙️"
      _ -> "●"
    end
  end

  defp supervisor_health_color(:running), do: "bg-green-400"
  defp supervisor_health_color(:stopped), do: "bg-yellow-400"
  defp supervisor_health_color(:error), do: "bg-red-400"
  defp supervisor_health_color(_), do: "bg-gray-400"

  defp child_health_color(:running), do: "bg-green-400"
  defp child_health_color(:stopped), do: "bg-yellow-400"
  defp child_health_color(:error), do: "bg-red-400"
  defp child_health_color(_), do: "bg-gray-400"

  defp supervisor_status_color(:running), do: "text-green-400"
  defp supervisor_status_color(:stopped), do: "text-yellow-400"
  defp supervisor_status_color(:error), do: "text-red-400"
  defp supervisor_status_color(_), do: "text-gray-400"

  defp supervisor_status_classes(:running), do: "bg-green-500/20 text-green-400"
  defp supervisor_status_classes(:stopped), do: "bg-yellow-500/20 text-yellow-400"
  defp supervisor_status_classes(:error), do: "bg-red-500/20 text-red-400"
  defp supervisor_status_classes(_), do: "bg-gray-500/20 text-gray-400"

  defp child_status_classes(:running), do: "bg-green-500/20 text-green-400"
  defp child_status_classes(:stopped), do: "bg-yellow-500/20 text-yellow-400"
  defp child_status_classes(:error), do: "bg-red-500/20 text-red-400"
  defp child_status_classes(_), do: "bg-gray-500/20 text-gray-400"

  defp supervisor_status_text(:running), do: "Running"
  defp supervisor_status_text(:stopped), do: "Stopped"
  defp supervisor_status_text(:error), do: "Error"
  defp supervisor_status_text(_), do: "Unknown"

  defp child_status_text(:running), do: "OK"
  defp child_status_text(:stopped), do: "Stop"
  defp child_status_text(:error), do: "Err"
  defp child_status_text(_), do: "?"
end
