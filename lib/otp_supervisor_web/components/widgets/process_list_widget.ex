defmodule OtpSupervisorWeb.Components.Widgets.ProcessListWidget do
  use Phoenix.LiveComponent

  alias OtpSupervisorWeb.Components.Terminal.TerminalDataFormatters

  @moduledoc """
  Interactive process list widget with filtering, sorting, and actions.
  
  Provides comprehensive process management capabilities including:
  - Real-time process monitoring
  - Interactive filtering and sorting
  - Process actions (kill, restart, inspect)
  - Grouping and categorization
  - Performance metrics
  """

  attr :processes, :list, required: true
  attr :show_actions, :boolean, default: false
  attr :filters, :map, default: %{}
  attr :grouping, :atom, values: [:none, :supervisor, :type, :status], default: :none
  attr :selectable, :boolean, default: false
  attr :compact_mode, :boolean, default: false
  attr :update_interval, :integer, default: 1000
  attr :max_height, :string, default: "max-h-96"
  attr :class, :string, default: ""

  def render(assigns) do
    ~H"""
    <div class={[
      "bg-gray-900 border border-green-500/30 rounded text-green-400",
      @class
    ]}>
      <!-- Header with controls -->
      <div class="flex items-center justify-between p-3 border-b border-green-500/20">
        <div class="flex items-center space-x-4">
          <h3 class="text-sm font-mono font-bold text-green-300">
            Process List
          </h3>
          <span class="text-xs text-green-400/70 font-mono">
            (<%= length(filtered_processes(@processes, @filters)) %> processes)
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
            <option value="running">Running</option>
            <option value="stopped">Stopped</option>
            <option value="error">Error</option>
          </select>
          
          <!-- Grouping selector -->
          <select 
            phx-target={@myself}
            phx-change="change_grouping"
            class="px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:border-green-500"
          >
            <option value="none">No Grouping</option>
            <option value="supervisor">By Supervisor</option>
            <option value="type">By Type</option>
            <option value="status">By Status</option>
          </select>
          
          <!-- Search -->
          <input
            type="text"
            placeholder="Search processes..."
            phx-target={@myself}
            phx-keyup="search"
            phx-debounce="300"
            value={@filters[:search] || ""}
            class="px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:border-green-500"
          />
          
          <!-- Refresh indicator -->
          <div class="flex items-center space-x-1">
            <div class="w-2 h-2 bg-green-400 rounded-full animate-pulse"></div>
            <span class="text-xs text-green-400/70 font-mono">Auto</span>
          </div>
        </div>
      </div>

      <!-- Process list content -->
      <div class={[
        "overflow-auto",
        @max_height
      ]}>
        <%= if @grouping == :none do %>
          <%= render_flat_list(assigns) %>
        <% else %>
          <%= render_grouped_list(assigns) %>
        <% end %>
      </div>

      <!-- Footer with summary stats -->
      <div class="border-t border-green-500/20 p-2">
        <div class="flex items-center justify-between text-xs">
          <div class="flex items-center space-x-4">
            <span class="text-green-400/70 font-mono">
              Running: <span class="text-green-400"><%= count_by_status(@processes, :running) %></span>
            </span>
            <span class="text-green-400/70 font-mono">
              Stopped: <span class="text-yellow-400"><%= count_by_status(@processes, :stopped) %></span>
            </span>
            <span class="text-green-400/70 font-mono">
              Errors: <span class="text-red-400"><%= count_by_status(@processes, :error) %></span>
            </span>
          </div>
          <div class="flex items-center space-x-2">
            <span class="text-green-400/70 font-mono">
              Total Memory: <span class="text-green-400"><%= format_bytes(total_memory(@processes)) %></span>
            </span>
          </div>
        </div>
      </div>
    </div>
    """
  end

  def mount(socket) do
    {:ok, 
     socket
     |> assign(:selected_processes, MapSet.new())
     |> assign(:sort_column, :name)
     |> assign(:sort_direction, :asc)}
  end

  def update(assigns, socket) do
    {:ok, assign(socket, assigns)}
  end

  # Event handlers

  def handle_event("filter_status", %{"value" => status}, socket) do
    filters = Map.put(socket.assigns.filters, :status, status)
    {:noreply, assign(socket, :filters, filters)}
  end

  def handle_event("change_grouping", %{"value" => grouping}, socket) do
    {:noreply, assign(socket, :grouping, String.to_atom(grouping))}
  end

  def handle_event("search", %{"value" => search}, socket) do
    filters = Map.put(socket.assigns.filters, :search, search)
    {:noreply, assign(socket, :filters, filters)}
  end

  def handle_event("sort", %{"column" => column}, socket) do
    column = String.to_atom(column)
    
    {sort_column, sort_direction} = 
      if socket.assigns.sort_column == column do
        {column, toggle_direction(socket.assigns.sort_direction)}
      else
        {column, :asc}
      end
    
    {:noreply, 
     socket
     |> assign(:sort_column, sort_column)
     |> assign(:sort_direction, sort_direction)}
  end

  def handle_event("toggle_process", %{"process_id" => process_id}, socket) do
    selected = socket.assigns.selected_processes
    
    new_selected = if MapSet.member?(selected, process_id) do
      MapSet.delete(selected, process_id)
    else
      MapSet.put(selected, process_id)
    end
    
    {:noreply, assign(socket, :selected_processes, new_selected)}
  end

  def handle_event("process_action", %{"action" => action, "process_id" => process_id}, socket) do
    # Send action to parent LiveView
    send(self(), {:process_action, action, process_id})
    {:noreply, socket}
  end

  def handle_event("bulk_action", %{"action" => action}, socket) do
    selected_ids = MapSet.to_list(socket.assigns.selected_processes)
    send(self(), {:bulk_process_action, action, selected_ids})
    {:noreply, assign(socket, :selected_processes, MapSet.new())}
  end

  # Private rendering functions

  defp render_flat_list(assigns) do
    ~H"""
    <table class="w-full">
      <thead class="sticky top-0 bg-gray-900 border-b border-green-500/20">
        <tr>
          <%= if @selectable do %>
            <th class="px-3 py-2 text-left">
              <input
                type="checkbox"
                class="text-green-400 bg-gray-800 border-green-500/30 rounded focus:ring-green-500"
                phx-target={@myself}
                phx-click="toggle_all"
              />
            </th>
          <% end %>
          
          <th class="px-3 py-2 text-left">
            <button
              class="flex items-center space-x-1 text-green-300 hover:text-green-400 font-mono text-xs font-bold"
              phx-target={@myself}
              phx-click="sort"
              phx-value-column="name"
            >
              <span>Name</span>
              <%= if @sort_column == :name do %>
                <span class="text-xs">
                  <%= if @sort_direction == :asc, do: "↑", else: "↓" %>
                </span>
              <% end %>
            </button>
          </th>
          
          <th class="px-3 py-2 text-left">
            <button
              class="flex items-center space-x-1 text-green-300 hover:text-green-400 font-mono text-xs font-bold"
              phx-target={@myself}
              phx-click="sort"
              phx-value-column="pid"
            >
              <span>PID</span>
              <%= if @sort_column == :pid do %>
                <span class="text-xs">
                  <%= if @sort_direction == :asc, do: "↑", else: "↓" %>
                </span>
              <% end %>
            </button>
          </th>
          
          <th class="px-3 py-2 text-left">
            <span class="text-green-300 font-mono text-xs font-bold">Status</span>
          </th>
          
          <th class="px-3 py-2 text-left">
            <button
              class="flex items-center space-x-1 text-green-300 hover:text-green-400 font-mono text-xs font-bold"
              phx-target={@myself}
              phx-click="sort"
              phx-value-column="memory"
            >
              <span>Memory</span>
              <%= if @sort_column == :memory do %>
                <span class="text-xs">
                  <%= if @sort_direction == :asc, do: "↑", else: "↓" %>
                </span>
              <% end %>
            </button>
          </th>
          
          <th class="px-3 py-2 text-left">
            <button
              class="flex items-center space-x-1 text-green-300 hover:text-green-400 font-mono text-xs font-bold"
              phx-target={@myself}
              phx-click="sort"
              phx-value-column="cpu"
            >
              <span>CPU</span>
              <%= if @sort_column == :cpu do %>
                <span class="text-xs">
                  <%= if @sort_direction == :asc, do: "↑", else: "↓" %>
                </span>
              <% end %>
            </button>
          </th>
          
          <th class="px-3 py-2 text-left">
            <button
              class="flex items-center space-x-1 text-green-300 hover:text-green-400 font-mono text-xs font-bold"
              phx-target={@myself}
              phx-click="sort"
              phx-value-column="parent"
            >
              <span>Parent</span>
              <%= if @sort_column == :parent do %>
                <span class="text-xs">
                  <%= if @sort_direction == :asc, do: "↑", else: "↓" %>
                </span>
              <% end %>
            </button>
          </th>
          
          <th class="px-3 py-2 text-left">
            <button
              class="flex items-center space-x-1 text-green-300 hover:text-green-400 font-mono text-xs font-bold"
              phx-target={@myself}
              phx-click="sort"
              phx-value-column="strategy"
            >
              <span>Strategy</span>
              <%= if @sort_column == :strategy do %>
                <span class="text-xs">
                  <%= if @sort_direction == :asc, do: "↑", else: "↓" %>
                </span>
              <% end %>
            </button>
          </th>
          
          <%= if @show_actions do %>
            <th class="px-3 py-2 text-left">
              <span class="text-green-300 font-mono text-xs font-bold">Actions</span>
            </th>
          <% end %>
        </tr>
      </thead>
      <tbody>
        <%= for process <- sorted_processes(filtered_processes(@processes, @filters), @sort_column, @sort_direction) do %>
          <tr class={[
            "border-b border-green-500/10 hover:bg-green-500/5 transition-colors",
            if(@selectable and MapSet.member?(@selected_processes, process.id), do: "bg-green-500/10", else: "")
          ]}>
            <%= if @selectable do %>
              <td class="px-3 py-2">
                <input
                  type="checkbox"
                  class="text-green-400 bg-gray-800 border-green-500/30 rounded focus:ring-green-500"
                  checked={MapSet.member?(@selected_processes, process.id)}
                  phx-target={@myself}
                  phx-click="toggle_process"
                  phx-value-process-id={process.id}
                />
              </td>
            <% end %>
            
            <td class="px-3 py-2 text-sm font-mono text-green-400">
              <%= process.name %>
            </td>
            
            <td class="px-3 py-2 text-sm font-mono text-green-400/70">
              <%= process.pid %>
            </td>
            
            <td class="px-3 py-2">
              <span class={[
                "inline-flex items-center space-x-1 px-2 py-1 rounded text-xs font-mono",
                process_status_classes(process.status)
              ]}>
                <span><%= process_status_icon(process.status) %></span>
                <span><%= String.capitalize(to_string(process.status)) %></span>
              </span>
            </td>
            
            <td class="px-3 py-2 text-sm font-mono text-green-400">
              <%= format_bytes(process.memory) %>
            </td>
            
            <td class="px-3 py-2 text-sm font-mono text-green-400">
              <%= process.cpu_usage %>%
            </td>
            
            <td class="px-3 py-2 text-sm font-mono text-green-400/70">
              <div class="flex flex-col">
                <span class="truncate max-w-32" title={Map.get(process, :parent, "N/A")}>
                  <%= Map.get(process, :parent, "N/A") %>
                </span>
                <span class="text-xs text-green-400/50 truncate max-w-32" title={Map.get(process, :parent_pid, "N/A")}>
                  <%= Map.get(process, :parent_pid, "N/A") %>
                </span>
              </div>
            </td>
            
            <td class="px-3 py-2 text-sm font-mono text-green-400">
              <span class={[
                "inline-flex items-center px-2 py-1 rounded text-xs font-mono",
                strategy_classes(Map.get(process, :strategy, :unknown))
              ]}>
                <%= strategy_text(Map.get(process, :strategy, :unknown)) %>
              </span>
            </td>
            
            <%= if @show_actions do %>
              <td class="px-3 py-2">
                <div class="flex items-center space-x-1">
                  <button
                    class="px-2 py-1 text-xs bg-blue-500/20 border border-blue-500/30 rounded text-blue-400 font-mono hover:bg-blue-500/30 transition-colors"
                    phx-target={@myself}
                    phx-click="process_action"
                    phx-value-action="inspect"
                    phx-value-process-id={process.id}
                  >
                    Inspect
                  </button>
                  <%= if process.status == :running do %>
                    <button
                      class="px-2 py-1 text-xs bg-red-500/20 border border-red-500/30 rounded text-red-400 font-mono hover:bg-red-500/30 transition-colors"
                      phx-target={@myself}
                      phx-click="process_action"
                      phx-value-action="kill"
                      phx-value-process-id={process.id}
                    >
                      Kill
                    </button>
                  <% else %>
                    <button
                      class="px-2 py-1 text-xs bg-green-500/20 border border-green-500/30 rounded text-green-400 font-mono hover:bg-green-500/30 transition-colors"
                      phx-target={@myself}
                      phx-click="process_action"
                      phx-value-action="restart"
                      phx-value-process-id={process.id}
                    >
                      Restart
                    </button>
                  <% end %>
                </div>
              </td>
            <% end %>
          </tr>
        <% end %>
      </tbody>
    </table>
    """
  end

  defp render_grouped_list(assigns) do
    ~H"""
    <div class="space-y-4 p-4">
      <%= for {group_name, processes} <- group_processes(filtered_processes(@processes, @filters), @grouping) do %>
        <div class="border border-green-500/20 rounded">
          <div class="bg-green-500/10 px-4 py-2 border-b border-green-500/20">
            <h4 class="text-sm font-mono font-bold text-green-300">
              <%= group_name %> (<%= length(processes) %> processes)
            </h4>
          </div>
          <div class="p-2">
            <%= for process <- sorted_processes(processes, @sort_column, @sort_direction) do %>
              <div class="flex items-center justify-between py-1 hover:bg-green-500/5 transition-colors">
                <div class="flex items-center space-x-4">
                  <span class="text-sm font-mono text-green-400"><%= process.name %></span>
                  <span class="text-xs font-mono text-green-400/70"><%= process.pid %></span>
                  <span class={[
                    "inline-flex items-center space-x-1 px-2 py-1 rounded text-xs font-mono",
                    process_status_classes(process.status)
                  ]}>
                    <span><%= process_status_icon(process.status) %></span>
                    <span><%= String.capitalize(to_string(process.status)) %></span>
                  </span>
                  <span class="text-xs font-mono text-green-400/50">
                    Parent: <%= Map.get(process, :parent, "N/A") %>
                  </span>
                  <span class={[
                    "inline-flex items-center px-2 py-1 rounded text-xs font-mono",
                    strategy_classes(Map.get(process, :strategy, :unknown))
                  ]}>
                    <%= strategy_text(Map.get(process, :strategy, :unknown)) %>
                  </span>
                </div>
                <div class="flex items-center space-x-4">
                  <span class="text-xs font-mono text-green-400">
                    <%= format_bytes(process.memory) %>
                  </span>
                  <span class="text-xs font-mono text-green-400">
                    <%= process.cpu_usage %>%
                  </span>
                </div>
              </div>
            <% end %>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  # Private helper functions

  defp filtered_processes(processes, filters) do
    processes
    |> filter_by_status(filters[:status])
    |> filter_by_search(filters[:search])
  end

  defp filter_by_status(processes, nil), do: processes
  defp filter_by_status(processes, ""), do: processes
  defp filter_by_status(processes, :all), do: processes
  defp filter_by_status(processes, "all"), do: processes
  defp filter_by_status(processes, status) when is_atom(status) do
    Enum.filter(processes, &(&1.status == status))
  end
  defp filter_by_status(processes, status) when is_binary(status) do
    status_atom = String.to_atom(status)
    Enum.filter(processes, &(&1.status == status_atom))
  end

  defp filter_by_search(processes, nil), do: processes
  defp filter_by_search(processes, ""), do: processes
  defp filter_by_search(processes, search) do
    search = String.downcase(search)
    Enum.filter(processes, fn process ->
      String.contains?(String.downcase(process.name), search) ||
      String.contains?(String.downcase(process.pid), search)
    end)
  end

  defp sorted_processes(processes, column, direction) do
    sorted = Enum.sort_by(processes, &Map.get(&1, column))
    
    case direction do
      :asc -> sorted
      :desc -> Enum.reverse(sorted)
    end
  end

  defp group_processes(processes, :supervisor) do
    processes
    |> Enum.group_by(&Map.get(&1, :supervisor, "Unknown"))
    |> Enum.sort_by(&elem(&1, 0))
  end

  defp group_processes(processes, :type) do
    processes
    |> Enum.group_by(&Map.get(&1, :type, "worker"))
    |> Enum.sort_by(&elem(&1, 0))
  end

  defp group_processes(processes, :status) do
    processes
    |> Enum.group_by(&Map.get(&1, :status))
    |> Enum.sort_by(&elem(&1, 0))
  end

  defp group_processes(processes, _), do: [{"All Processes", processes}]

  defp toggle_direction(:asc), do: :desc
  defp toggle_direction(:desc), do: :asc

  defp count_by_status(processes, status) do
    Enum.count(processes, &(&1.status == status))
  end

  defp total_memory(processes) do
    Enum.reduce(processes, 0, fn process, acc ->
      acc + (process.memory || 0)
    end)
  end

  defp process_status_classes(:running), do: "bg-green-500/20 text-green-400"
  defp process_status_classes(:stopped), do: "bg-yellow-500/20 text-yellow-400"
  defp process_status_classes(:error), do: "bg-red-500/20 text-red-400"
  defp process_status_classes(_), do: "bg-gray-500/20 text-gray-400"

  defp process_status_icon(:running), do: "●"
  defp process_status_icon(:stopped), do: "■"
  defp process_status_icon(:error), do: "✗"
  defp process_status_icon(_), do: "○"

  defp format_bytes(bytes) when is_integer(bytes) do
    cond do
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 1)}GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 1)}MB"
      bytes >= 1_024 -> "#{Float.round(bytes / 1_024, 1)}KB"
      true -> "#{bytes}B"
    end
  end
  defp format_bytes(bytes), do: to_string(bytes)

  defp strategy_classes(:one_for_one), do: "bg-blue-500/20 text-blue-400"
  defp strategy_classes(:one_for_all), do: "bg-purple-500/20 text-purple-400"
  defp strategy_classes(:rest_for_one), do: "bg-orange-500/20 text-orange-400"
  defp strategy_classes(:simple_one_for_one), do: "bg-cyan-500/20 text-cyan-400"
  defp strategy_classes(:unknown), do: "bg-gray-500/20 text-gray-400"
  defp strategy_classes(_), do: "bg-gray-500/20 text-gray-400"

  defp strategy_text(:one_for_one), do: "One for One"
  defp strategy_text(:one_for_all), do: "One for All"
  defp strategy_text(:rest_for_one), do: "Rest for One"
  defp strategy_text(:simple_one_for_one), do: "Simple One for One"
  defp strategy_text(:unknown), do: "Unknown"
  defp strategy_text(strategy), do: String.capitalize(to_string(strategy))
end