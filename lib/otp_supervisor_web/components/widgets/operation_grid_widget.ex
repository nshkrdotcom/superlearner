defmodule OtpSupervisorWeb.Components.Widgets.OperationGridWidget do
  use Phoenix.LiveComponent

  @moduledoc """
  Arsenal-specific operation grid widget with status indicators.

  Displays operations in a grid format with:
  - Color-coded status indicators
  - Interactive selection and execution
  - Filtering and search capabilities
  - Real-time status updates
  - Keyboard navigation support
  """

  attr :operations, :list, required: true
  attr :selected_operation, :map, default: nil
  attr :grid_size, :string, default: "grid-cols-8"
  attr :show_status, :boolean, default: true
  attr :interactive, :boolean, default: true
  attr :filters, :map, default: %{}
  attr :compact_mode, :boolean, default: false
  attr :class, :string, default: ""

  def render(assigns) do
    ~H"""
    <div class={[
      "bg-gray-900 border border-green-500/30 rounded text-green-400 h-full flex flex-col",
      @class
    ]}>
      <!-- Header with filters -->
      <div class="flex items-center justify-between p-3 border-b border-green-500/20">
        <div class="flex items-center space-x-4">
          <h3 class="text-sm font-mono font-bold text-green-300">
            Operations Grid
          </h3>
          <span class="text-xs text-green-400/70 font-mono">
            ({length(filtered_operations(@operations, @filters))} operations)
          </span>
        </div>

        <div class="flex items-center space-x-2">
          <!-- Status filter -->
          <select
            phx-target={@myself}
            phx-change="filter_status"
            class="px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:border-green-500"
          >
            <option value="">All Status</option>
            <option value="active">Active</option>
            <option value="planned">Planned</option>
            <option value="inactive">Inactive</option>
          </select>
          
    <!-- Category filter -->
          <select
            phx-target={@myself}
            phx-change="filter_category"
            class="px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:border-green-500"
          >
            <option value="">All Categories</option>
            <%= for category <- get_categories(@operations) do %>
              <option value={category}>{category}</option>
            <% end %>
          </select>
          
    <!-- Search -->
          <input
            type="text"
            placeholder="Search operations..."
            phx-target={@myself}
            phx-keyup="search"
            phx-debounce="300"
            value={@filters[:search] || ""}
            class="px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:border-green-500"
          />
          
    <!-- Grid size selector -->
          <select
            phx-target={@myself}
            phx-change="change_grid_size"
            class="px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:border-green-500"
          >
            <option value="grid-cols-6">6 Columns</option>
            <option value="grid-cols-8" selected>8 Columns</option>
            <option value="grid-cols-10">10 Columns</option>
            <option value="grid-cols-12">12 Columns</option>
          </select>
        </div>
      </div>
      
    <!-- Operations grid -->
      <div class="flex-1 overflow-auto p-4">
        <div class={[
          "grid gap-2",
          @grid_size
        ]}>
          <%= for {operation, index} <- Enum.with_index(filtered_operations(@operations, @filters)) do %>
            <div
              phx-target={@myself}
              phx-click={if @interactive, do: "select_operation", else: nil}
              phx-value-operation-id={operation.id}
              phx-value-index={index}
              class={[
                "h-16 rounded border transition-all duration-200 flex items-center justify-start text-left cursor-pointer relative",
                if(@compact_mode, do: "p-1", else: "p-2"),
                operation_classes(operation),
                if(@selected_operation && @selected_operation.id == operation.id,
                  do: "ring-2 ring-green-400",
                  else: "hover:bg-opacity-80"
                )
              ]}
              title={operation_tooltip(operation)}
            >
              <!-- Status indicator -->
              <%= if @show_status do %>
                <div class={[
                  "absolute top-1 right-1 w-2 h-2 rounded-full",
                  status_indicator_color(operation.status)
                ]}>
                </div>
              <% end %>
              
    <!-- Priority indicator -->
              <%= if operation.priority == :high or operation.priority == :critical do %>
                <div class="absolute top-1 left-1 w-2 h-2 rounded-full bg-red-400 animate-pulse">
                </div>
              <% end %>
              
    <!-- Operation icon (smaller) -->
              <div class="text-xs mr-1 flex-shrink-0 w-4 text-center">
                {operation.icon}
              </div>
              
    <!-- Operation name (full width) -->
              <div class="flex-1 min-w-0 pr-1 flex items-center">
                <div class="text-xs font-mono leading-tight font-semibold text-left overflow-hidden max-h-full">
                  {format_operation_name(operation.name)}
                </div>
              </div>
            </div>
          <% end %>
        </div>
        
    <!-- Empty state -->
        <%= if filtered_operations(@operations, @filters) == [] do %>
          <div class="text-center py-8">
            <div class="text-green-400/50 text-sm font-mono">No operations found</div>
            <div class="text-green-400/30 text-xs font-mono mt-1">Try adjusting your filters</div>
          </div>
        <% end %>
      </div>
      
    <!-- Footer with selected operation details -->
      <%= if @selected_operation do %>
        <div class="border-t border-green-500/20 p-3">
          <div class="flex items-center justify-between">
            <div class="flex-1">
              <div class="flex items-center space-x-2 mb-2">
                <span class="text-lg">{@selected_operation.icon}</span>
                <span class="font-mono font-bold text-green-300">
                  {@selected_operation.name}
                </span>
                <span class={[
                  "px-2 py-1 rounded text-xs font-mono",
                  operation_status_classes(@selected_operation.status)
                ]}>
                  {String.capitalize(to_string(@selected_operation.status))}
                </span>
              </div>

              <div class="text-xs text-green-400/80 font-mono mb-2">
                {@selected_operation.description}
              </div>

              <div class="grid grid-cols-3 gap-4 text-xs">
                <div>
                  <span class="text-green-400/70">Category:</span>
                  <span class="text-green-400 font-mono ml-1">{@selected_operation.category}</span>
                </div>
                <div>
                  <span class="text-green-400/70">Priority:</span>
                  <span class={[
                    "font-mono ml-1",
                    priority_color(@selected_operation.priority)
                  ]}>
                    {String.capitalize(to_string(@selected_operation.priority))}
                  </span>
                </div>
                <div>
                  <span class="text-green-400/70">Executions:</span>
                  <span class="text-green-400 font-mono ml-1">
                    {@selected_operation.execution_count}
                  </span>
                </div>
              </div>
            </div>

            <div class="flex items-center space-x-2 ml-4">
              <button
                phx-target={@myself}
                phx-click="toggle_status"
                phx-value-operation-id={@selected_operation.id}
                class="px-3 py-1 bg-green-500/20 border border-green-500/30 rounded text-green-400 font-mono text-sm hover:bg-green-500/30 transition-colors"
              >
                Toggle
              </button>

              <%= if @selected_operation.status == :active do %>
                <button
                  phx-target={@myself}
                  phx-click="execute_operation"
                  phx-value-operation-id={@selected_operation.id}
                  class="px-3 py-1 bg-blue-500/20 border border-blue-500/30 rounded text-blue-400 font-mono text-sm hover:bg-blue-500/30 transition-colors"
                >
                  Execute
                </button>
              <% end %>
            </div>
          </div>
        </div>
      <% end %>
      
    <!-- Status legend -->
      <div class="border-t border-green-500/20 p-2">
        <div class="flex items-center justify-center space-x-6 text-xs">
          <div class="flex items-center space-x-2">
            <div class="w-2 h-2 bg-green-400 rounded-full"></div>
            <span class="text-green-400/70 font-mono">Active</span>
          </div>
          <div class="flex items-center space-x-2">
            <div class="w-2 h-2 bg-blue-400 rounded-full"></div>
            <span class="text-green-400/70 font-mono">Planned</span>
          </div>
          <div class="flex items-center space-x-2">
            <div class="w-2 h-2 bg-gray-400 rounded-full"></div>
            <span class="text-green-400/70 font-mono">Inactive</span>
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
    {:ok, assign(socket, assigns)}
  end

  # Event handlers

  def handle_event("select_operation", %{"operation-id" => operation_id}, socket) do
    operation = find_operation(operation_id, socket.assigns.operations)

    # Notify parent LiveView if parent_pid is available
    if socket.parent_pid do
      send(socket.parent_pid, {:operation_selected, operation})
    end

    {:noreply, assign(socket, :selected_operation, operation)}
  end

  def handle_event("filter_status", %{"value" => status}, socket) do
    filters = Map.put(socket.assigns.filters, :status, status)
    {:noreply, assign(socket, :filters, filters)}
  end

  def handle_event("filter_category", %{"value" => category}, socket) do
    filters = Map.put(socket.assigns.filters, :category, category)
    {:noreply, assign(socket, :filters, filters)}
  end

  def handle_event("search", %{"value" => search}, socket) do
    filters = Map.put(socket.assigns.filters, :search, search)
    {:noreply, assign(socket, :filters, filters)}
  end

  def handle_event("change_grid_size", %{"value" => grid_size}, socket) do
    {:noreply, assign(socket, :grid_size, grid_size)}
  end

  def handle_event("toggle_status", %{"operation-id" => operation_id}, socket) do
    send(self(), {:toggle_operation_status, operation_id})
    {:noreply, socket}
  end

  def handle_event("execute_operation", %{"operation-id" => operation_id}, socket) do
    send(self(), {:execute_operation, operation_id})
    {:noreply, socket}
  end

  # Private helper functions

  defp filtered_operations(operations, filters) do
    operations
    |> filter_by_status(filters[:status])
    |> filter_by_category(filters[:category])
    |> filter_by_search(filters[:search])
  end

  defp filter_by_status(operations, nil), do: operations
  defp filter_by_status(operations, ""), do: operations

  defp filter_by_status(operations, status) do
    status_atom = String.to_atom(status)
    Enum.filter(operations, &(&1.status == status_atom))
  end

  defp filter_by_category(operations, nil), do: operations
  defp filter_by_category(operations, ""), do: operations

  defp filter_by_category(operations, category) do
    Enum.filter(operations, &(&1.category == category))
  end

  defp filter_by_search(operations, nil), do: operations
  defp filter_by_search(operations, ""), do: operations

  defp filter_by_search(operations, search) do
    search = String.downcase(search)

    Enum.filter(operations, fn operation ->
      String.contains?(String.downcase(operation.name), search) ||
        String.contains?(String.downcase(operation.description), search) ||
        String.contains?(String.downcase(operation.category), search)
    end)
  end

  defp find_operation(operation_id, operations) do
    Enum.find(operations, &(&1.id == operation_id))
  end

  defp get_categories(operations) do
    operations
    |> Enum.map(& &1.category)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp operation_classes(operation) do
    base_classes = ["font-mono", "text-xs"]

    status_classes =
      case operation.status do
        :active ->
          ["bg-green-500/20", "border-green-500/50", "text-green-400", "hover:bg-green-500/30"]

        :planned ->
          ["bg-blue-500/20", "border-blue-500/50", "text-blue-400", "hover:bg-blue-500/30"]

        :inactive ->
          ["bg-gray-500/20", "border-gray-500/50", "text-gray-400", "hover:bg-gray-500/30"]

        _ ->
          ["bg-gray-500/20", "border-gray-500/50", "text-gray-400"]
      end

    base_classes ++ status_classes
  end

  defp operation_status_classes(status) do
    case status do
      :active -> "bg-green-500/20 text-green-400"
      :planned -> "bg-blue-500/20 text-blue-400"
      :inactive -> "bg-gray-500/20 text-gray-400"
      _ -> "bg-gray-500/20 text-gray-400"
    end
  end

  defp status_indicator_color(status) do
    case status do
      :active -> "bg-green-400"
      :planned -> "bg-blue-400"
      :inactive -> "bg-gray-400"
      _ -> "bg-gray-400"
    end
  end

  defp priority_color(priority) do
    case priority do
      :critical -> "text-red-400"
      :high -> "text-red-400"
      :medium -> "text-yellow-400"
      :low -> "text-green-400"
      _ -> "text-green-400"
    end
  end

  defp format_operation_name(name) do
    # Remove common prefixes and clean up the name
    name
    |> String.replace("OTPSupervisor.Core.Arsenal.Operations.", "")
    |> String.replace("Elixir.", "")
    |> String.replace("Operation", "")
    |> String.split(".")
    |> List.last()
    |> String.trim()
    # Convert CamelCase to plain words with spaces
    |> String.replace(~r/([a-z])([A-Z])/, "\\1 \\2")
    |> String.downcase()
    |> String.split(" ")
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end

  defp operation_tooltip(operation) do
    """
    #{operation.name}
    Category: #{operation.category}
    Priority: #{operation.priority}
    Status: #{operation.status}
    Executions: #{operation.execution_count}

    #{operation.description}
    """
  end
end
