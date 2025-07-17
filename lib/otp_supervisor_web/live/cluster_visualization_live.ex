defmodule OtpSupervisorWeb.Live.ClusterVisualizationLive do
  use Phoenix.LiveView
  import Phoenix.Component

  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterSupervisionTrees
  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList

  @moduledoc """
  Cluster visualization interface with high-density text-based display.

  Displays all nodes, supervision trees, and processes in a terminal-style
  ASCII tree format with Unicode box-drawing characters and color coding.
  """

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:page_title, "Cluster Visualization")
      |> assign(:current_page, "cluster-visualization")
      |> assign_initial_state()
      |> load_cluster_data()

    if connected?(socket) do
      # Set up periodic refresh timer (5 seconds)
      Process.send_after(self(), :refresh_data, 5_000)
    end

    {:ok, socket}
  end

  def handle_params(params, _url, socket) do
    {:noreply, handle_url_params(socket, params)}
  end

  def handle_info(:refresh_data, socket) do
    socket =
      if socket.assigns.auto_refresh and not socket.assigns.loading do
        load_cluster_data(socket)
      else
        socket
      end

    # Schedule next refresh
    Process.send_after(self(), :refresh_data, 5_000)
    {:noreply, socket}
  end

  def handle_info({:debounced_search, search_term}, socket) do
    # Handle debounced search
    socket =
      socket
      |> assign(:search_term, search_term)
      |> assign(:search_debounce_timer, nil)
      |> apply_filters_and_search()
      |> update_url_params()

    {:noreply, socket}
  end

  # Simulate loading completion for now
  def handle_info(:simulate_load_complete, socket) do
    socket =
      socket
      |> assign(:loading, false)
      |> assign(:last_updated, DateTime.utc_now())

    {:noreply, socket}
  end

  def handle_info(_msg, socket) do
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div class="fixed inset-0 bg-gray-900 text-green-400 flex flex-col">
      <!-- Status Bar -->
      <.live_component
        module={TerminalStatusBar}
        id="cluster-visualization-status-bar"
        title="Cluster Visualization"
        metrics={status_bar_metrics(assigns)}
        navigation_links={TerminalNavigationLinks.page_navigation_links("cluster-visualization", %{})}
      />
      
    <!-- Main Content Area -->
      <div class="flex-1 p-4 overflow-hidden">
        <div class="h-full bg-gray-800 rounded border border-green-500/30 p-4 flex flex-col">
          <!-- Controls Panel -->
          <div class="mb-4 p-3 bg-gray-900/50 rounded border border-green-500/20">
            <!-- Search Form -->
            <form phx-change="search_change" class="mb-4">
              <div class="flex items-center space-x-2">
                <label class="text-green-400/70 font-mono text-sm">Search:</label>
                <input
                  type="text"
                  value={@search_term}
                  placeholder="Search by PID, name, or module..."
                  class="bg-gray-800 border border-green-500/30 text-green-400 font-mono text-sm rounded px-3 py-2 focus:border-green-500 focus:ring-1 focus:ring-green-500/50 focus:outline-none flex-1 max-w-md transition-all duration-200 placeholder:text-green-400/40"
                  name="search"
                  phx-debounce="300"
                />
                <%= if @search_term != "" do %>
                  <button
                    type="button"
                    class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-2 py-2 rounded transition-colors"
                    phx-click="clear_search"
                    title="Clear search"
                  >
                    ✕
                  </button>
                <% end %>
              </div>
            </form>
            
    <!-- Filter Form -->
            <form phx-change="filter_change" class="mb-4">
              <div class="flex flex-wrap items-center gap-4">
                <!-- Node Filter -->
                <div class="flex items-center space-x-2">
                  <label class="text-green-400/70 font-mono text-sm">Node:</label>
                  <select
                    class="bg-gray-800 border border-green-500/30 text-green-400 font-mono text-sm rounded px-2 py-1 focus:border-green-500 focus:ring-1 focus:ring-green-500/50 focus:outline-none disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200"
                    name="node"
                    disabled={@loading}
                  >
                    <option value="all" selected={@filters.node == :all}>All Nodes</option>
                    
                    <%= for node <- get_unique_nodes_from_cluster_data(@cluster_data) do %>
                      <option value={node} selected={@filters.node == node}>{node}</option>
                    <% end %>
                  </select>
                </div>
                
    <!-- Type Filter -->
                <div class="flex items-center space-x-2">
                  <label class="text-green-400/70 font-mono text-sm">Type:</label>
                  <select
                    class="bg-gray-800 border border-green-500/30 text-green-400 font-mono text-sm rounded px-2 py-1 focus:border-green-500 focus:ring-1 focus:ring-green-500/50 focus:outline-none disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200"
                    name="type"
                    disabled={@loading}
                  >
                    <option value="all" selected={@filters.type == :all}>All Types</option>
                    
                    <%= for type <- get_unique_types_from_cluster_data(@cluster_data) do %>
                      <option value={type} selected={@filters.type == type}>
                        {format_process_type(type)}
                      </option>
                    <% end %>
                  </select>
                </div>
                
    <!-- Application Filter -->
                <div class="flex items-center space-x-2">
                  <label class="text-green-400/70 font-mono text-sm">App:</label>
                  <select
                    class="bg-gray-800 border border-green-500/30 text-green-400 font-mono text-sm rounded px-2 py-1 focus:border-green-500 focus:ring-1 focus:ring-green-500/50 focus:outline-none disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200"
                    name="application"
                    disabled={@loading}
                  >
                    <option value="all" selected={@filters.application == :all}>All Apps</option>
                    
                    <%= for app <- get_unique_applications_from_cluster_data(@cluster_data) do %>
                      <option value={app} selected={@filters.application == app}>{app}</option>
                    <% end %>
                  </select>
                </div>
                
    <!-- Clear Filters Button -->
                <%= if has_active_filters?(@filters) do %>
                  <button
                    type="button"
                    class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
                    phx-click="clear_filters"
                    disabled={@loading}
                  >
                    <%= if @loading do %>
                      Processing...
                    <% else %>
                      Clear Filters
                    <% end %>
                  </button>
                <% end %>
              </div>
            </form>
            
    <!-- Active Filter Indicators -->
            <%= if has_active_filters?(@filters) do %>
              <div class="flex items-center space-x-2 text-xs font-mono mb-3">
                <span class="text-green-400/70">Active:</span>
                <%= if @filters.node != :all do %>
                  <span class="bg-green-500/20 text-green-300 px-2 py-1 rounded">
                    Node: {@filters.node}
                  </span>
                <% end %>
                
                <%= if @filters.type != :all do %>
                  <span class="bg-green-500/20 text-green-300 px-2 py-1 rounded">
                    Type: {format_process_type(@filters.type)}
                  </span>
                <% end %>
                
                <%= if @filters.application != :all do %>
                  <span class="bg-green-500/20 text-green-300 px-2 py-1 rounded">
                    App: {@filters.application}
                  </span>
                <% end %>
              </div>
            <% end %>
            
    <!-- Search/Filter Results Counter -->
            <%= if @search_term != "" or has_active_filters?(@filters) do %>
              <div class="mb-3 pt-3 border-t border-green-500/20">
                <div class="text-green-400/70 font-mono text-sm">
                  Showing {@filtered_process_count} of {@total_processes} processes
                  <%= if @search_term != "" do %>
                    matching "<span class="text-green-300"><%= @search_term %></span>"
                  <% end %>
                </div>
              </div>
            <% end %>
            
    <!-- Refresh Controls -->
            <div class="flex items-center justify-between pt-3 border-t border-green-500/20">
              <!-- Left Controls -->
              <div class="flex items-center space-x-4">
                <!-- Auto-refresh Toggle -->
                <div class="flex items-center space-x-2">
                  <label class="text-green-400/70 font-mono text-sm">Auto-refresh:</label>
                  <button
                    type="button"
                    class={[
                      "px-3 py-1 rounded font-mono text-sm transition-colors border",
                      if(@auto_refresh,
                        do: "bg-green-500/20 text-green-300 border-green-500/50",
                        else:
                          "bg-gray-700 text-green-400/70 border-green-500/30 hover:border-green-500/50"
                      )
                    ]}
                    phx-click="toggle_auto_refresh"
                    disabled={@loading}
                  >
                    <%= if @auto_refresh do %>
                      ✓ ON
                    <% else %>
                      ✗ OFF
                    <% end %>
                  </button>
                </div>
                
    <!-- Refresh Interval Display -->
                <%= if @auto_refresh do %>
                  <div class="text-green-400/70 font-mono text-sm">
                    (5s interval)
                  </div>
                <% end %>
              </div>
              
    <!-- Right Controls -->
              <div class="flex items-center space-x-4">
                <!-- Manual Refresh Button -->
                <button
                  type="button"
                  class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-2 rounded transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
                  phx-click="manual_refresh"
                  disabled={@loading}
                >
                  <%= if @loading do %>
                    ⟳ Loading...
                  <% else %>
                    🔄 Refresh
                  <% end %>
                </button>
                
    <!-- Last Updated -->
                <%= if @last_updated do %>
                  <div class="text-green-400/70 font-mono text-sm">
                    Updated: {format_timestamp(@last_updated)}
                  </div>
                <% end %>
              </div>
            </div>
          </div>
          
    <!-- Visualization Container -->
          <div class="flex-1 bg-gray-900/30 rounded border border-green-500/20 relative overflow-hidden">
            <%= if @loading do %>
              <div class="absolute inset-0 flex items-center justify-center">
                <div class="text-center text-green-400/70 font-mono">
                  <div class="animate-pulse text-lg mb-2">Loading cluster data...</div>
                  
                  <div class="text-sm">Fetching supervision trees and processes</div>
                </div>
              </div>
            <% else %>
              <%= if @error_message do %>
                <div class="absolute inset-0 flex items-center justify-center">
                  <div class="text-center text-red-400 font-mono p-6 bg-red-500/10 rounded border border-red-500/30">
                    <div class="text-lg mb-2">✗ Error Loading Data</div>
                    
                    <div class="text-sm mb-4">{@error_message}</div>
                    
                    <button
                      type="button"
                      class="bg-red-500/20 hover:bg-red-500/30 border border-red-500/50 text-red-300 font-mono text-sm px-3 py-2 rounded transition-colors"
                      phx-click="manual_refresh"
                    >
                      🔄 Retry
                    </button>
                  </div>
                </div>
              <% else %>
                <!-- High-density Text-based Cluster Tree Display -->
                <div class="w-full h-full overflow-auto p-4 bg-gray-900/50 rounded">
                  <%= if @filtered_cluster_data && map_size(@filtered_cluster_data) > 0 do %>
                    {OtpSupervisorWeb.Components.ClusterTreeRenderer.render_cluster_tree(
                      @filtered_cluster_data
                    )}
                  <% else %>
                    <%= if @cluster_data && map_size(@cluster_data) > 0 do %>
                      <div class="text-center text-green-400/70 font-mono py-8">
                        <div class="text-lg mb-2">🔍 No Results</div>
                        
                        <div class="text-sm">No processes match the current filters</div>
                        
                        <div class="text-xs mt-2 text-green-400/50">
                          Try adjusting your search or filter criteria
                        </div>
                      </div>
                    <% else %>
                      <div class="text-center text-green-400/70 font-mono py-8">
                        <div class="text-lg mb-2">🎯 Cluster Visualization</div>
                        
                        <div class="text-sm">No cluster data available</div>
                        
                        <div class="text-xs mt-2 text-green-400/50">
                          Click refresh to load cluster information
                        </div>
                      </div>
                    <% end %>
                  <% end %>
                </div>
              <% end %>
            <% end %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  # Event handlers

  def handle_event("toggle_auto_refresh", _params, socket) do
    new_auto_refresh = not socket.assigns.auto_refresh
    {:noreply, assign(socket, :auto_refresh, new_auto_refresh)}
  end

  def handle_event("manual_refresh", _params, socket) do
    socket =
      if not socket.assigns.loading do
        load_cluster_data(socket)
      else
        socket
      end

    {:noreply, socket}
  end

  def handle_event("filter_change", form_params, socket) do
    # Extract filter values from form
    updated_filters = %{
      node: parse_filter_value(form_params["node"] || "all"),
      type: parse_filter_value(form_params["type"] || "all"),
      application: parse_filter_value(form_params["application"] || "all")
    }

    socket =
      socket
      |> assign(:filters, updated_filters)
      |> apply_filters_and_search()
      |> update_url_params()

    {:noreply, socket}
  end

  def handle_event("clear_filters", _params, socket) do
    # Reset all filters to :all
    default_filters = %{
      node: :all,
      type: :all,
      application: :all
    }

    # Get all nodes to expand them
    all_nodes = get_unique_nodes_from_cluster_data(socket.assigns.cluster_data)
    expanded_nodes = MapSet.new(all_nodes)

    socket =
      socket
      |> assign(:filters, default_filters)
      |> assign(:search_term, "")
      |> assign(:expanded_nodes, expanded_nodes)
      |> apply_filters_and_search()
      |> update_url_params()

    {:noreply, socket}
  end

  def handle_event("search_change", %{"search" => search_term}, socket) do
    # Cancel existing debounce timer if any
    if socket.assigns.search_debounce_timer do
      Process.cancel_timer(socket.assigns.search_debounce_timer)
    end

    # Set up debounced search
    timer_ref = Process.send_after(self(), {:debounced_search, search_term}, 300)

    socket =
      socket
      |> assign(:search_term, search_term)
      |> assign(:search_debounce_timer, timer_ref)

    {:noreply, socket}
  end

  def handle_event("clear_search", _params, socket) do
    # Clear search and apply filters
    socket =
      socket
      |> assign(:search_term, "")
      |> assign(:search_debounce_timer, nil)
      |> apply_filters_and_search()
      |> update_url_params()

    {:noreply, socket}
  end

  def handle_event("toggle_node", %{"node" => node_string}, socket) do
    node = String.to_atom(node_string)
    expanded_nodes = socket.assigns.expanded_nodes

    updated_expanded_nodes =
      if MapSet.member?(expanded_nodes, node) do
        MapSet.delete(expanded_nodes, node)
      else
        MapSet.put(expanded_nodes, node)
      end

    {:noreply, assign(socket, :expanded_nodes, updated_expanded_nodes)}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private functions

  defp assign_initial_state(socket) do
    socket
    |> assign(:cluster_data, %{})
    |> assign(:filtered_cluster_data, %{})
    |> assign(:loading, false)
    |> assign(:auto_refresh, true)
    |> assign(:error_message, nil)
    |> assign(:last_updated, nil)
    |> assign(:total_nodes, 0)
    |> assign(:total_processes, 0)
    |> assign(:filtered_process_count, 0)
    |> assign(:filters, %{node: :all, type: :all, application: :all})
    |> assign(:search_term, "")
    |> assign(:search_debounce_timer, nil)
    |> assign(:expanded_nodes, MapSet.new())
  end

  defp status_bar_metrics(assigns) do
    [
      %{label: "Nodes", value: format_number(assigns.total_nodes)},
      %{label: "Processes", value: format_number(assigns.total_processes)},
      %{label: "Auto-refresh", value: if(assigns.auto_refresh, do: "ON", else: "OFF")},
      %{label: "Status", value: get_status_indicator(assigns)}
    ]
  end

  defp format_number(num) when is_integer(num) do
    num
    |> Integer.to_string()
    |> String.reverse()
    |> String.replace(~r/(\d{3})(?=\d)/, "\\1,")
    |> String.reverse()
  end

  defp format_number(_), do: "0"

  defp get_status_indicator(assigns) do
    cond do
      assigns.loading -> "⟳ Loading"
      assigns.error_message -> "✗ Error"
      true -> "✓ Ready"
    end
  end

  defp format_timestamp(datetime) do
    datetime
    |> DateTime.to_time()
    |> Time.to_string()
    |> String.slice(0, 8)
  end

  # Data loading functions for Task 2

  defp load_cluster_data(socket) do
    socket =
      socket
      |> assign(:loading, true)
      |> assign(:error_message, nil)

    case fetch_cluster_data() do
      {:ok, unified_data} ->
        socket
        |> assign(:cluster_data, unified_data.cluster_nodes)
        |> assign(:total_nodes, unified_data.total_nodes)
        |> assign(:total_processes, unified_data.total_processes)
        |> assign(:loading, false)
        |> assign(:last_updated, DateTime.utc_now())
        |> apply_filters_and_search()

      {:error, error_message} ->
        require Logger
        Logger.error("Failed to load cluster data: #{error_message}")

        socket
        |> assign(:cluster_data, %{})
        |> assign(:filtered_cluster_data, %{})
        |> assign(:total_nodes, 0)
        |> assign(:total_processes, 0)
        |> assign(:filtered_process_count, 0)
        |> assign(:loading, false)
        |> assign(:error_message, error_message)
    end
  end

  defp fetch_cluster_data do
    try do
      # Call both Arsenal operations
      supervision_params = %{
        "include_children" => true,
        "include_process_details" => true
      }

      process_params = %{
        "include_details" => true,
        "limit" => 5000
      }

      with {:ok, supervision_result} <- ClusterSupervisionTrees.execute(supervision_params),
           {:ok, process_result} <- ProcessList.execute(process_params) do
        # Merge data into unified hierarchical structure
        unified_data = create_unified_data_structure(supervision_result, process_result)
        {:ok, unified_data}
      else
        {:error, reason} -> {:error, format_error(reason)}
      end
    rescue
      error -> {:error, "Failed to fetch cluster data: #{inspect(error)}"}
    end
  end

  defp create_unified_data_structure(supervision_result, process_result) do
    # Extract supervision trees by node
    supervision_trees = supervision_result.supervision_trees

    # Extract all processes by node
    processes_by_node = group_processes_by_node(process_result.processes)

    # Create unified structure per node
    cluster_nodes =
      supervision_trees
      |> Enum.map(fn {node, node_data} ->
        # Get all supervised PIDs for this node
        supervised_pids = extract_supervised_pids(node_data.supervisors)

        # Get all processes for this node
        all_processes = Map.get(processes_by_node, node, [])

        # Identify standalone processes (not in supervision trees)
        standalone_processes = identify_standalone_processes(all_processes, supervised_pids)

        # Create unified hierarchical structure for D3.js
        unified_node_data = %{
          name: to_string(node),
          type: :node,
          children: build_node_children(node_data.supervisors, standalone_processes)
        }

        {node, unified_node_data}
      end)
      |> Enum.into(%{})

    # Calculate totals
    total_nodes = map_size(cluster_nodes)
    total_processes = calculate_total_processes(cluster_nodes)

    %{
      cluster_nodes: cluster_nodes,
      total_nodes: total_nodes,
      total_processes: total_processes
    }
  end

  defp group_processes_by_node(processes) do
    processes
    |> Enum.group_by(fn process -> process.node end)
  end

  defp extract_supervised_pids(supervisors) do
    supervisors
    |> Enum.reduce(MapSet.new(), fn supervisor, acc ->
      # Add supervisor PID
      acc = MapSet.put(acc, supervisor.pid)

      # Recursively add children PIDs
      extract_children_pids(supervisor, acc)
    end)
  end

  defp extract_children_pids(supervisor, acc) do
    case Map.get(supervisor, :children) do
      nil ->
        acc

      children ->
        children
        |> Enum.reduce(acc, fn child, child_acc ->
          # Add child PID
          child_acc = MapSet.put(child_acc, child.pid)

          # Recursively process if child has children
          extract_children_pids(child, child_acc)
        end)
    end
  end

  defp identify_standalone_processes(all_processes, supervised_pids) do
    all_processes
    |> Enum.filter(fn process ->
      not MapSet.member?(supervised_pids, process.pid)
    end)
    |> Enum.map(fn process ->
      %{
        name: process.registered_name || process.pid,
        pid: process.pid,
        type: :worker,
        alive: process.alive,
        level: 1,
        node: process.node,
        application: Map.get(process, :application, :unknown),
        memory: Map.get(process, :memory, 0),
        message_queue_len: Map.get(process, :message_queue_len, 0)
      }
    end)
  end

  defp build_node_children(supervisors, standalone_processes) do
    # Convert supervision trees to D3.js format
    supervision_children =
      supervisors
      |> Enum.map(&convert_supervisor_to_d3_format/1)

    # Create virtual supervisor for standalone processes if any exist
    standalone_children =
      if length(standalone_processes) > 0 do
        [
          %{
            name: "Standalone Processes",
            type: :virtual_supervisor,
            alive: true,
            level: 0,
            children: standalone_processes
          }
        ]
      else
        []
      end

    supervision_children ++ standalone_children
  end

  defp convert_supervisor_to_d3_format(supervisor) do
    base_supervisor = %{
      name: supervisor.name,
      pid: supervisor.pid,
      type: supervisor.type,
      alive: supervisor.alive,
      level: supervisor.level,
      node: supervisor.node,
      application: supervisor.application,
      strategy: Map.get(supervisor, :strategy, :unknown)
    }

    case Map.get(supervisor, :children) do
      nil ->
        base_supervisor

      children ->
        converted_children =
          children
          |> Enum.map(&convert_supervisor_to_d3_format/1)

        Map.put(base_supervisor, :children, converted_children)
    end
  end

  defp calculate_total_processes(cluster_nodes) do
    cluster_nodes
    |> Enum.reduce(0, fn {_node, node_data}, acc ->
      acc + count_processes_in_node(node_data)
    end)
  end

  defp count_processes_in_node(node_data) do
    case Map.get(node_data, :children) do
      nil -> 0
      children -> Enum.reduce(children, 0, &(&2 + count_processes_recursive(&1)))
    end
  end

  defp count_processes_recursive(item) do
    base_count = 1

    case Map.get(item, :children) do
      nil -> base_count
      children -> base_count + Enum.reduce(children, 0, &(&2 + count_processes_recursive(&1)))
    end
  end

  defp format_error(reason) when is_binary(reason), do: reason
  defp format_error(reason), do: inspect(reason)

  # URL parameter handling functions

  defp handle_url_params(socket, params) do
    socket
    |> maybe_update_filters(params)
    |> maybe_update_search(params)
    |> apply_filters_and_search()
  end

  defp maybe_update_filters(socket, params) do
    filters = socket.assigns.filters

    updated_filters = %{
      filters
      | node: parse_filter_param(params["node"], :all),
        type: parse_filter_param(params["type"], :all),
        application: parse_filter_param(params["application"], :all)
    }

    assign(socket, :filters, updated_filters)
  end

  defp maybe_update_search(socket, params) do
    search_term = Map.get(params, "search", "")
    assign(socket, :search_term, search_term)
  end

  defp parse_filter_param(nil, default), do: default
  defp parse_filter_param("", default), do: default
  defp parse_filter_param("all", _default), do: :all

  defp parse_filter_param(value, _default) when is_binary(value) do
    try do
      String.to_existing_atom(value)
    rescue
      ArgumentError -> String.to_atom(value)
    end
  end

  defp parse_filter_param(_value, default), do: default

  defp update_url_params(socket) do
    # Build URL parameters from current state
    params = %{}

    params =
      if socket.assigns.filters.node != :all do
        Map.put(params, "node", to_string(socket.assigns.filters.node))
      else
        params
      end

    params =
      if socket.assigns.filters.type != :all do
        Map.put(params, "type", to_string(socket.assigns.filters.type))
      else
        params
      end

    params =
      if socket.assigns.filters.application != :all do
        Map.put(params, "application", to_string(socket.assigns.filters.application))
      else
        params
      end

    params =
      if socket.assigns.search_term != "" do
        Map.put(params, "search", socket.assigns.search_term)
      else
        params
      end

    # Push URL update without triggering navigation
    url_path =
      "/cluster-visualization" <>
        if map_size(params) > 0, do: "?" <> URI.encode_query(params), else: ""

    push_patch(socket, to: url_path)
    socket
  end

  # Filter helper functions

  defp parse_filter_value(""), do: :all
  defp parse_filter_value("all"), do: :all

  defp parse_filter_value(value) when is_binary(value) do
    try do
      String.to_existing_atom(value)
    rescue
      ArgumentError -> String.to_atom(value)
    end
  end

  defp parse_filter_value(value), do: value

  defp has_active_filters?(filters) do
    filters.node != :all or filters.type != :all or filters.application != :all
  end

  # Data extraction functions for filter options

  defp get_unique_nodes_from_cluster_data(cluster_data) when is_map(cluster_data) do
    cluster_data
    |> Map.keys()
    |> Enum.map(&to_string/1)
    |> Enum.sort()
  end

  defp get_unique_nodes_from_cluster_data(_), do: []

  defp get_unique_types_from_cluster_data(cluster_data) when is_map(cluster_data) do
    cluster_data
    |> extract_all_processes_from_cluster_data()
    |> Enum.map(fn process -> Map.get(process, :type, :worker) end)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp get_unique_types_from_cluster_data(_), do: []

  defp get_unique_applications_from_cluster_data(cluster_data) when is_map(cluster_data) do
    cluster_data
    |> extract_all_processes_from_cluster_data()
    |> Enum.map(fn process -> Map.get(process, :application, :unknown) end)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp get_unique_applications_from_cluster_data(_), do: []

  defp extract_all_processes_from_cluster_data(cluster_data) do
    cluster_data
    |> Enum.flat_map(fn {_node, node_data} ->
      extract_processes_from_node(node_data)
    end)
  end

  defp extract_processes_from_node(node_data) do
    case Map.get(node_data, :children) do
      nil -> []
      children -> Enum.flat_map(children, &extract_processes_recursive/1)
    end
  end

  defp extract_processes_recursive(item) do
    base_process = [item]

    case Map.get(item, :children) do
      nil -> base_process
      children -> base_process ++ Enum.flat_map(children, &extract_processes_recursive/1)
    end
  end

  defp format_process_type(type) when is_atom(type) do
    case type do
      :supervisor -> "Supervisor"
      :virtual_supervisor -> "Virtual Supervisor"
      :worker -> "Worker"
      :node -> "Node"
      _ -> type |> to_string() |> String.capitalize()
    end
  end

  defp format_process_type(type), do: to_string(type)

  # Filtering and search functions

  defp apply_filters_and_search(socket) do
    cluster_data = socket.assigns.cluster_data
    filters = socket.assigns.filters
    search_term = socket.assigns.search_term

    # Apply filters and search to cluster data
    filtered_data = apply_filters_to_cluster_data(cluster_data, filters, search_term)
    filtered_count = count_processes_in_filtered_data(filtered_data)

    socket
    |> assign(:filtered_cluster_data, filtered_data)
    |> assign(:filtered_process_count, filtered_count)
  end

  defp apply_filters_to_cluster_data(cluster_data, filters, search_term) do
    cluster_data
    |> filter_by_node_cluster(filters.node)
    |> filter_by_type_cluster(filters.type)
    |> filter_by_application_cluster(filters.application)
    |> search_cluster_data(search_term)
  end

  defp filter_by_node_cluster(cluster_data, :all), do: cluster_data

  defp filter_by_node_cluster(cluster_data, node) when is_atom(node) do
    node_string = to_string(node)

    cluster_data
    |> Enum.filter(fn {node_key, _node_data} ->
      to_string(node_key) == node_string
    end)
    |> Enum.into(%{})
  end

  defp filter_by_type_cluster(cluster_data, :all), do: cluster_data

  defp filter_by_type_cluster(cluster_data, type) when is_atom(type) do
    cluster_data
    |> Enum.map(fn {node_key, node_data} ->
      filtered_node_data = filter_node_by_type(node_data, type)
      {node_key, filtered_node_data}
    end)
    |> Enum.filter(fn {_node_key, node_data} ->
      # Only keep nodes that have matching processes
      has_processes_in_node?(node_data)
    end)
    |> Enum.into(%{})
  end

  defp filter_by_application_cluster(cluster_data, :all), do: cluster_data

  defp filter_by_application_cluster(cluster_data, application) when is_atom(application) do
    cluster_data
    |> Enum.map(fn {node_key, node_data} ->
      filtered_node_data = filter_node_by_application(node_data, application)
      {node_key, filtered_node_data}
    end)
    |> Enum.filter(fn {_node_key, node_data} ->
      # Only keep nodes that have matching processes
      has_processes_in_node?(node_data)
    end)
    |> Enum.into(%{})
  end

  defp search_cluster_data(cluster_data, ""), do: cluster_data
  defp search_cluster_data(cluster_data, nil), do: cluster_data

  defp search_cluster_data(cluster_data, search_term) when is_binary(search_term) do
    normalized_search = String.downcase(String.trim(search_term))

    if String.length(normalized_search) == 0 do
      cluster_data
    else
      cluster_data
      |> Enum.map(fn {node_key, node_data} ->
        filtered_node_data = search_node_data(node_data, normalized_search)
        {node_key, filtered_node_data}
      end)
      |> Enum.filter(fn {_node_key, node_data} ->
        # Only keep nodes that have matching processes
        has_processes_in_node?(node_data)
      end)
      |> Enum.into(%{})
    end
  end

  # Helper functions for filtering individual nodes

  defp filter_node_by_type(node_data, type) do
    case Map.get(node_data, :children) do
      nil ->
        node_data

      children ->
        filtered_children = filter_children_by_type(children, type)
        Map.put(node_data, :children, filtered_children)
    end
  end

  defp filter_children_by_type(children, type) do
    children
    |> Enum.filter(fn child ->
      child_type = Map.get(child, :type, :worker)
      child_type == type
    end)
    |> Enum.map(fn child ->
      case Map.get(child, :children) do
        nil ->
          child

        child_children ->
          filtered_child_children = filter_children_by_type(child_children, type)
          Map.put(child, :children, filtered_child_children)
      end
    end)
    |> Enum.filter(fn child ->
      # Keep if it matches type or has matching children
      child_type = Map.get(child, :type, :worker)
      child_type == type or has_processes_in_item?(child)
    end)
  end

  defp filter_node_by_application(node_data, application) do
    case Map.get(node_data, :children) do
      nil ->
        node_data

      children ->
        filtered_children = filter_children_by_application(children, application)
        Map.put(node_data, :children, filtered_children)
    end
  end

  defp filter_children_by_application(children, application) do
    children
    |> Enum.map(fn child ->
      case Map.get(child, :children) do
        nil ->
          child

        child_children ->
          filtered_child_children = filter_children_by_application(child_children, application)
          Map.put(child, :children, filtered_child_children)
      end
    end)
    |> Enum.filter(fn child ->
      # Keep if it matches application or has matching children
      child_app = Map.get(child, :application, :unknown)
      child_app == application or has_processes_in_item?(child)
    end)
  end

  defp search_node_data(node_data, search_term) do
    case Map.get(node_data, :children) do
      nil ->
        node_data

      children ->
        filtered_children = search_children(children, search_term)
        Map.put(node_data, :children, filtered_children)
    end
  end

  defp search_children(children, search_term) do
    children
    |> Enum.map(fn child ->
      case Map.get(child, :children) do
        nil ->
          child

        child_children ->
          filtered_child_children = search_children(child_children, search_term)
          Map.put(child, :children, filtered_child_children)
      end
    end)
    |> Enum.filter(fn child ->
      # Keep if it matches search or has matching children
      matches_search?(child, search_term) or has_processes_in_item?(child)
    end)
  end

  defp matches_search?(item, search_term) do
    name = Map.get(item, :name, "") |> to_string() |> String.downcase()
    pid = Map.get(item, :pid, "") |> to_string() |> String.downcase()
    application = Map.get(item, :application, "") |> to_string() |> String.downcase()

    String.contains?(name, search_term) or
      String.contains?(pid, search_term) or
      String.contains?(application, search_term)
  end

  defp has_processes_in_node?(node_data) do
    case Map.get(node_data, :children) do
      nil -> false
      [] -> false
      children -> Enum.any?(children, &has_processes_in_item?/1)
    end
  end

  defp has_processes_in_item?(item) do
    case Map.get(item, :children) do
      # Leaf node counts as a process
      nil -> true
      # Empty children still counts as a process
      [] -> true
      children -> Enum.any?(children, &has_processes_in_item?/1)
    end
  end

  defp count_processes_in_filtered_data(filtered_data) do
    filtered_data
    |> Enum.reduce(0, fn {_node, node_data}, acc ->
      acc + count_processes_in_node(node_data)
    end)
  end
end
