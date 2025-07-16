defmodule OtpSupervisorWeb.Live.ClusterProcessesLive do
  use Phoenix.LiveView
  import Phoenix.HTML

  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList
  alias OTPSupervisor.Distributed.ClusterStateManager

  @moduledoc """
  Cluster Processes page for comprehensive process distribution analysis across BEAM clusters.

  Provides real-time visibility into all processes running across cluster nodes with smart
  filtering, search capabilities, and detailed process information.
  """

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:page_title, "Cluster Processes")
      |> assign(:current_page, "cluster-processes")
      |> assign_initial_state()
      |> load_process_data()

    # Set up real-time updates when connected
    if connected?(socket) do
      # Subscribe to cluster state changes
      ClusterStateManager.subscribe_to_changes()

      # Set up periodic refresh timer (5 seconds)
      Process.send_after(self(), :refresh_processes, 5_000)
    end

    {:ok, socket}
  end

  def handle_params(params, _url, socket) do
    {:noreply, handle_url_params(socket, params)}
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

  def handle_event("filter_change", %{"filter_type" => filter_type, "value" => value}, socket) do
    current_filters = socket.assigns.filters
    filter_key = String.to_atom(filter_type)

    # Parse the filter value
    parsed_value = parse_filter_value(value)

    # Update the specific filter
    updated_filters = Map.put(current_filters, filter_key, parsed_value)

    # Apply filters and update socket
    socket =
      socket
      |> assign(:filters, updated_filters)
      |> assign(:current_page, 1)  # Reset to first page when filtering
      |> apply_filters_and_update_display()

    {:noreply, socket}
  end

  def handle_event("clear_filters", _params, socket) do
    # Reset all filters to :all
    default_filters = %{
      node: :all,
      type: :all,
      application: :all
    }

    socket =
      socket
      |> assign(:filters, default_filters)
      |> assign(:search_term, "")
      |> assign(:current_page, 1)  # Reset to first page when clearing filters
      |> apply_filters_and_search_and_update_display()

    {:noreply, socket}
  end

  def handle_event("search_change", %{"search" => search_term}, socket) do
    socket =
      socket
      |> assign(:search_term, search_term)
      |> assign(:current_page, 1)  # Reset to first page when searching
      |> apply_filters_and_search_and_update_display()

    {:noreply, socket}
  end

  def handle_event("page_change", %{"page" => page_string}, socket) do
    case Integer.parse(page_string) do
      {page, ""} when page > 0 ->
        max_page = calculate_max_page(socket.assigns.filtered_process_count, socket.assigns.per_page)
        valid_page = min(page, max_page)

        socket =
          socket
          |> assign(:current_page, valid_page)
          |> apply_filters_and_search_and_update_display()

        {:noreply, socket}

      _ ->
        {:noreply, socket}
    end
  end

  # Handle real-time updates

  def handle_info(%{type: :cluster_state_change} = _event, socket) do
    # Cluster topology changed, refresh process data
    socket = update_process_data(socket)
    {:noreply, socket}
  end

  def handle_info(:refresh_processes, socket) do
    # Periodic refresh timer
    socket = update_process_data(socket)

    # Schedule next refresh
    Process.send_after(self(), :refresh_processes, 5_000)

    {:noreply, socket}
  end

  def handle_info(_msg, socket) do
    # Handle any other messages gracefully
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div class="fixed inset-0 bg-gray-900 text-green-400 flex flex-col">
      <!-- Status Bar -->
      <.live_component
        module={TerminalStatusBar}
        id="cluster-processes-status-bar"
        title="Cluster Processes"
        metrics={status_bar_metrics(assigns)}
        navigation_links={TerminalNavigationLinks.page_navigation_links("cluster-processes", %{})}
      />

      <!-- Main Content Area -->
      <div class="flex-1 p-4 overflow-hidden">
        <div class="h-full bg-gray-800 rounded border border-green-500/30 p-4 overflow-y-auto">
          <%= if @loading do %>
            <div class="text-center text-green-400/70 font-mono py-8">
              <div class="animate-pulse">Loading cluster processes...</div>
            </div>
          <% else %>
            <!-- Filter Controls -->
            <div class="mb-4 p-3 bg-gray-900/50 rounded border border-green-500/20">
              <!-- Search Input -->
              <div class="mb-4">
                <div class="flex items-center space-x-2">
                  <label class="text-green-400/70 font-mono text-sm">Search:</label>
                  <input
                    type="text"
                    value={@search_term}
                    placeholder="Search by PID, name, or module..."
                    class="bg-gray-800 border border-green-500/30 text-green-400 font-mono text-sm rounded px-3 py-2 focus:border-green-500 focus:outline-none flex-1 max-w-md"
                    phx-change="search_change"
                    phx-value-search={@search_term}
                    phx-debounce="300"
                  />
                  <%= if @search_term != "" do %>
                    <button
                      class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-2 py-2 rounded transition-colors"
                      phx-click="search_change"
                      phx-value-search=""
                      title="Clear search">
                      ✕
                    </button>
                  <% end %>
                </div>
              </div>

              <div class="flex flex-wrap items-center gap-4">
                <!-- Node Filter -->
                <div class="flex items-center space-x-2">
                  <label class="text-green-400/70 font-mono text-sm">Node:</label>
                  <select
                    class="bg-gray-800 border border-green-500/30 text-green-400 font-mono text-sm rounded px-2 py-1 focus:border-green-500 focus:outline-none"
                    phx-change="filter_change"
                    phx-value-filter_type="node">
                    <option value="all" selected={@filters.node == :all}>All Nodes</option>
                    <%= for node <- get_unique_nodes(@processes) do %>
                      <option value={node} selected={@filters.node == node}><%= node %></option>
                    <% end %>
                  </select>
                </div>

                <!-- Type Filter -->
                <div class="flex items-center space-x-2">
                  <label class="text-green-400/70 font-mono text-sm">Type:</label>
                  <select
                    class="bg-gray-800 border border-green-500/30 text-green-400 font-mono text-sm rounded px-2 py-1 focus:border-green-500 focus:outline-none"
                    phx-change="filter_change"
                    phx-value-filter_type="type">
                    <option value="all" selected={@filters.type == :all}>All Types</option>
                    <%= for type <- get_unique_types(@processes) do %>
                      <option value={type} selected={@filters.type == type}><%= format_process_type(type) %></option>
                    <% end %>
                  </select>
                </div>

                <!-- Application Filter -->
                <div class="flex items-center space-x-2">
                  <label class="text-green-400/70 font-mono text-sm">App:</label>
                  <select
                    class="bg-gray-800 border border-green-500/30 text-green-400 font-mono text-sm rounded px-2 py-1 focus:border-green-500 focus:outline-none"
                    phx-change="filter_change"
                    phx-value-filter_type="application">
                    <option value="all" selected={@filters.application == :all}>All Apps</option>
                    <%= for app <- get_unique_applications(@processes) do %>
                      <option value={app} selected={@filters.application == app}><%= app %></option>
                    <% end %>
                  </select>
                </div>

                <!-- Clear Filters Button -->
                <%= if has_active_filters?(@filters) do %>
                  <button
                    class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors"
                    phx-click="clear_filters">
                    Clear Filters
                  </button>
                <% end %>

                <!-- Active Filter Indicators -->
                <%= if has_active_filters?(@filters) do %>
                  <div class="flex items-center space-x-2 text-xs font-mono">
                    <span class="text-green-400/70">Active:</span>
                    <%= if @filters.node != :all do %>
                      <span class="bg-green-500/20 text-green-300 px-2 py-1 rounded">Node: <%= @filters.node %></span>
                    <% end %>
                    <%= if @filters.type != :all do %>
                      <span class="bg-green-500/20 text-green-300 px-2 py-1 rounded">Type: <%= format_process_type(@filters.type) %></span>
                    <% end %>
                    <%= if @filters.application != :all do %>
                      <span class="bg-green-500/20 text-green-300 px-2 py-1 rounded">App: <%= @filters.application %></span>
                    <% end %>
                  </div>
                <% end %>
              </div>

              <!-- Search/Filter Results Counter -->
              <%= if @search_term != "" or has_active_filters?(@filters) do %>
                <div class="mt-3 pt-3 border-t border-green-500/20">
                  <div class="text-green-400/70 font-mono text-sm">
                    Showing <%= @filtered_process_count %> of <%= @total_processes %> processes
                    <%= if @search_term != "" do %>
                      matching "<span class="text-green-300"><%= @search_term %></span>"
                    <% end %>
                  </div>
                </div>
              <% end %>
            </div>
            <%= if map_size(@processes_by_node) == 0 do %>
              <div class="text-center text-green-400/70 font-mono py-8">
                No cluster nodes found or no processes available
              </div>
            <% else %>
              <div class="space-y-4">
                <%= for {node, processes} <- Enum.sort(@processes_by_node) do %>
                  <div class="border border-green-500/30 rounded bg-gray-900/50">
                    <!-- Node Header -->
                    <div class="p-3 border-b border-green-500/20 cursor-pointer hover:bg-gray-700/30 transition-colors"
                         phx-click="toggle_node"
                         phx-value-node={node}>
                      <div class="flex items-center justify-between">
                        <div class="flex items-center space-x-3">
                          <span class="text-green-400 font-mono text-sm">
                            <%= if node_expanded?(assigns, node) do %>
                              ▼
                            <% else %>
                              ▶
                            <% end %>
                          </span>
                          <span class="text-green-300 font-mono font-semibold">
                            <%= node %>
                          </span>
                        </div>
                        <div class="text-green-400/70 font-mono text-sm">
                          <%= length(processes) %> processes
                        </div>
                      </div>
                    </div>

                    <!-- Node Content (Expandable) -->
                    <%= if node_expanded?(assigns, node) do %>
                      <div class="p-3">
                        <%= if length(processes) == 0 do %>
                          <div class="text-green-400/50 font-mono text-sm italic text-center py-4">
                            No processes
                          </div>
                        <% else %>
                          <div class="space-y-2">
                            <%= for process <- processes do %>
                              <% formatted = format_process_info(process) %>
                              <div class="bg-gray-800/50 rounded p-3 border border-green-500/10 hover:border-green-500/30 transition-colors">
                                <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-3 text-sm font-mono">
                                  <!-- Process Identity -->
                                  <div class="space-y-1">
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">PID:</span>
                                      <span><%= raw(highlight_search_term(formatted.pid_display, @search_term)) %></span>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Name:</span>
                                      <span><%= raw(highlight_search_term(formatted.name_display, @search_term)) %></span>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Type:</span> <%= formatted.type_display %>
                                    </div>
                                  </div>

                                  <!-- Process Details -->
                                  <div class="space-y-1">
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Memory:</span> <%= formatted.memory_display %>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Queue:</span> <%= formatted.message_queue_display %>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">App:</span>
                                      <span><%= raw(highlight_search_term(formatted.application_display, @search_term)) %></span>
                                    </div>
                                  </div>

                                  <!-- Function Information -->
                                  <div class="space-y-1 md:col-span-2 lg:col-span-1">
                                    <div class="text-green-300 text-xs">
                                      <span class="text-green-400/70">Initial:</span>
                                      <div class="break-all"><%= raw(highlight_search_term(formatted.initial_call_display, @search_term)) %></div>
                                    </div>
                                    <div class="text-green-300 text-xs">
                                      <span class="text-green-400/70">Current:</span>
                                      <div class="break-all"><%= raw(highlight_search_term(formatted.current_function_display, @search_term)) %></div>
                                    </div>
                                  </div>
                                </div>
                              </div>
                            <% end %>
                          </div>
                        <% end %>
                      </div>
                    <% end %>
                  </div>
                <% end %>
              </div>

              <!-- Pagination Controls -->
              <%= if @filtered_process_count > @per_page do %>
                <div class="mt-6 p-3 bg-gray-900/50 rounded border border-green-500/20">
                  <% max_page = calculate_max_page(@filtered_process_count, @per_page) %>
                  <% start_item = (@current_page - 1) * @per_page + 1 %>
                  <% end_item = min(@current_page * @per_page, @filtered_process_count) %>

                  <div class="flex items-center justify-between">
                    <!-- Page Info -->
                    <div class="text-green-400/70 font-mono text-sm">
                      Showing <%= start_item %>-<%= end_item %> of <%= @filtered_process_count %> processes
                      (Page <%= @current_page %> of <%= max_page %>)
                    </div>

                    <!-- Pagination Buttons -->
                    <div class="flex items-center space-x-2">
                      <!-- First Page -->
                      <%= if @current_page > 1 do %>
                        <button
                          class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors"
                          phx-click="page_change"
                          phx-value-page="1"
                          title="First page">
                          ««
                        </button>
                      <% else %>
                        <button
                          class="bg-gray-800 border border-green-500/10 text-green-400/30 font-mono text-sm px-3 py-1 rounded cursor-not-allowed"
                          disabled
                          title="First page">
                          ««
                        </button>
                      <% end %>

                      <!-- Previous Page -->
                      <%= if @current_page > 1 do %>
                        <button
                          class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors"
                          phx-click="page_change"
                          phx-value-page={@current_page - 1}
                          title="Previous page">
                          ‹
                        </button>
                      <% else %>
                        <button
                          class="bg-gray-800 border border-green-500/10 text-green-400/30 font-mono text-sm px-3 py-1 rounded cursor-not-allowed"
                          disabled
                          title="Previous page">
                          ‹
                        </button>
                      <% end %>

                      <!-- Page Numbers -->
                      <%= for page_num <- pagination_range(@current_page, max_page) do %>
                        <%= if page_num == @current_page do %>
                          <button
                            class="bg-green-500/20 border border-green-500/50 text-green-300 font-mono text-sm px-3 py-1 rounded font-semibold"
                            disabled>
                            <%= page_num %>
                          </button>
                        <% else %>
                          <button
                            class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors"
                            phx-click="page_change"
                            phx-value-page={page_num}>
                            <%= page_num %>
                          </button>
                        <% end %>
                      <% end %>

                      <!-- Next Page -->
                      <%= if @current_page < max_page do %>
                        <button
                          class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors"
                          phx-click="page_change"
                          phx-value-page={@current_page + 1}
                          title="Next page">
                          ›
                        </button>
                      <% else %>
                        <button
                          class="bg-gray-800 border border-green-500/10 text-green-400/30 font-mono text-sm px-3 py-1 rounded cursor-not-allowed"
                          disabled
                          title="Next page">
                          ›
                        </button>
                      <% end %>

                      <!-- Last Page -->
                      <%= if @current_page < max_page do %>
                        <button
                          class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors"
                          phx-click="page_change"
                          phx-value-page={max_page}
                          title="Last page">
                          »»
                        </button>
                      <% else %>
                        <button
                          class="bg-gray-800 border border-green-500/10 text-green-400/30 font-mono text-sm px-3 py-1 rounded cursor-not-allowed"
                          disabled
                          title="Last page">
                          »»
                        </button>
                      <% end %>
                    </div>
                  </div>
                </div>
              <% end %>
            <% end %>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  # Private functions

  defp assign_initial_state(socket) do
    socket
    |> assign(:processes, [])
    |> assign(:processes_by_node, %{})
    |> assign(:total_processes, 0)
    |> assign(:filtered_process_count, 0)
    |> assign(:filters, %{
      node: :all,
      type: :all,
      application: :all
    })
    |> assign(:search_term, "")
    |> assign(:current_page, 1)
    |> assign(:per_page, 100)
    |> assign(:loading, false)
    |> assign(:cluster_nodes, [])
    |> assign(:last_updated, nil)
    |> assign(:expanded_nodes, MapSet.new())
  end

  defp handle_url_params(socket, params) do
    socket
    |> maybe_update_filters(params)
    |> maybe_update_search(params)
    |> maybe_update_page(params)
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

  defp maybe_update_page(socket, params) do
    page =
      case Integer.parse(Map.get(params, "page", "1")) do
        {page_num, ""} when page_num > 0 -> page_num
        _ -> 1
      end

    assign(socket, :current_page, page)
  end

  defp parse_filter_param(nil, default), do: default
  defp parse_filter_param("", default), do: default
  defp parse_filter_param("all", _default), do: :all

  defp parse_filter_param(value, _default) when is_binary(value) do
    try do
      String.to_existing_atom(value)
    rescue
      ArgumentError -> :all
    end
  end

  defp parse_filter_param(_value, default), do: default

  defp status_bar_metrics(assigns) do
    [
      %{label: "Total Processes", value: "#{assigns.total_processes}"},
      %{label: "Cluster Nodes", value: "#{length(assigns.cluster_nodes)}"},
      %{label: "Filtered", value: filtered_count_display(assigns)},
      %{label: "Page", value: page_display(assigns)},
      %{label: "Last Updated", value: last_updated_display(assigns.last_updated)}
    ]
  end

  defp filtered_count_display(assigns) do
    if assigns.search_term != "" or
         assigns.filters != %{node: :all, type: :all, application: :all} do
      "#{Map.get(assigns, :filtered_process_count, 0)}"
    else
      "#{assigns.total_processes}"
    end
  end

  defp page_display(assigns) do
    if assigns.filtered_process_count > assigns.per_page do
      max_page = calculate_max_page(assigns.filtered_process_count, assigns.per_page)
      "#{assigns.current_page}/#{max_page}"
    else
      "1/1"
    end
  end

  defp last_updated_display(nil), do: "Never"

  defp last_updated_display(datetime) do
    datetime
    |> DateTime.to_time()
    |> Time.to_string()
    |> String.slice(0, 8)
  end

  defp node_expanded?(assigns, node) do
    MapSet.member?(assigns.expanded_nodes, node)
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

  defp apply_filters_and_search_and_update_display(socket) do
    filtered_processes = apply_filters(socket.assigns.processes, socket.assigns.filters)
    searched_processes = search_processes(filtered_processes, socket.assigns.search_term)

    # Apply pagination to the searched processes
    paginated_processes = paginate_processes(searched_processes, socket.assigns.current_page, socket.assigns.per_page)
    paginated_processes_by_node = group_processes_by_node(paginated_processes)

    socket
    |> assign(:processes_by_node, paginated_processes_by_node)
    |> assign(:filtered_process_count, length(searched_processes))
  end

  defp apply_filters_and_update_display(socket) do
    apply_filters_and_search_and_update_display(socket)
  end

  defp apply_filters(processes, filters) do
    processes
    |> filter_by_node(filters.node)
    |> filter_by_type(filters.type)
    |> filter_by_application(filters.application)
  end

  defp filter_by_node(processes, :all), do: processes
  defp filter_by_node(processes, node) when is_atom(node) do
    Enum.filter(processes, fn process -> process.node == node end)
  end

  defp filter_by_type(processes, :all), do: processes
  defp filter_by_type(processes, type) when is_atom(type) do
    type_string = Atom.to_string(type)
    Enum.filter(processes, fn process ->
      process_type = process.type
      cond do
        is_atom(process_type) -> process_type == type
        is_binary(process_type) -> String.downcase(process_type) == String.downcase(type_string)
        true -> false
      end
    end)
  end

  defp filter_by_application(processes, :all), do: processes
  defp filter_by_application(processes, application) when is_atom(application) do
    app_string = Atom.to_string(application)
    Enum.filter(processes, fn process ->
      process_app = process.application
      cond do
        is_atom(process_app) -> process_app == application
        is_binary(process_app) -> String.downcase(process_app) == String.downcase(app_string)
        true -> false
      end
    end)
  end

  defp get_available_filter_options(processes) do
    %{
      nodes: get_unique_nodes(processes),
      types: get_unique_types(processes),
      applications: get_unique_applications(processes)
    }
  end

  defp get_unique_nodes(processes) do
    processes
    |> Enum.map(& &1.node)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp get_unique_types(processes) do
    processes
    |> Enum.map(& &1.type)
    |> Enum.map(fn type ->
      cond do
        is_atom(type) -> type
        is_binary(type) -> String.to_atom(String.downcase(type))
        true -> :process
      end
    end)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp get_unique_applications(processes) do
    processes
    |> Enum.map(& &1.application)
    |> Enum.map(fn app ->
      cond do
        is_atom(app) -> app
        is_binary(app) -> String.to_atom(String.downcase(app))
        true -> :unknown
      end
    end)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp has_active_filters?(filters) do
    filters.node != :all or filters.type != :all or filters.application != :all
  end

  # Search functionality

  defp search_processes(processes, ""), do: processes
  defp search_processes(processes, search_term) when is_binary(search_term) do
    normalized_search = String.downcase(String.trim(search_term))

    if String.length(normalized_search) == 0 do
      processes
    else
      Enum.filter(processes, fn process ->
        process_matches_search?(process, normalized_search)
      end)
    end
  end

  defp process_matches_search?(process, search_term) do
    # Search across PID, registered name, and module names
    searchable_fields = [
      format_pid(process.pid),
      format_process_name(process),
      extract_module_from_initial_call(process.initial_call),
      extract_module_from_current_function(process.current_function),
      format_application(process.application)
    ]

    searchable_fields
    |> Enum.any?(fn field ->
      field
      |> String.downcase()
      |> String.contains?(search_term)
    end)
  end

  defp extract_module_from_initial_call({module, _function, _arity}) when is_atom(module) do
    Atom.to_string(module)
  end
  defp extract_module_from_initial_call(_), do: ""

  defp extract_module_from_current_function({module, _function, _arity}) when is_atom(module) do
    Atom.to_string(module)
  end
  defp extract_module_from_current_function(_), do: ""

  # Pagination functionality

  defp paginate_processes(processes, current_page, per_page) do
    start_index = (current_page - 1) * per_page
    processes
    |> Enum.drop(start_index)
    |> Enum.take(per_page)
  end

  defp calculate_max_page(total_count, per_page) when total_count > 0 and per_page > 0 do
    ceil(total_count / per_page)
  end
  defp calculate_max_page(_total_count, _per_page), do: 1

  defp pagination_range(current_page, max_page) do
    # Show up to 5 page numbers around the current page
    start_page = max(1, current_page - 2)
    end_page = min(max_page, current_page + 2)

    # Adjust range to always show 5 pages when possible
    cond do
      end_page - start_page < 4 and start_page > 1 ->
        start_page = max(1, end_page - 4)
        Enum.to_list(start_page..end_page)

      end_page - start_page < 4 and end_page < max_page ->
        end_page = min(max_page, start_page + 4)
        Enum.to_list(start_page..end_page)

      true ->
        Enum.to_list(start_page..end_page)
    end
  end

  # Search highlighting functionality

  defp highlight_search_term(text, ""), do: text
  defp highlight_search_term(text, search_term) when is_binary(text) and is_binary(search_term) do
    normalized_search = String.downcase(String.trim(search_term))

    if String.length(normalized_search) == 0 do
      text
    else
      # Case-insensitive highlighting
      regex = ~r/#{Regex.escape(normalized_search)}/i
      String.replace(text, regex, fn match ->
        "<span class=\"bg-yellow-400/30 text-yellow-200\">#{match}</span>"
      end)
    end
  end
  defp highlight_search_term(text, _search_term), do: text

  # Arsenal ProcessList operation integration

  defp get_processes(params \\ %{}) do
    # Build parameters for Arsenal ProcessList operation
    arsenal_params = build_arsenal_params(params)

    case ProcessList.execute(arsenal_params) do
      {:ok, result} ->
        {:ok, result}

      {:error, reason} ->
        {:error, format_arsenal_error(reason)}
    end
  end

  defp build_arsenal_params(params) do
    %{
      "include_details" => true,
      "limit" => Map.get(params, :limit, 1000)
    }
    |> maybe_add_node_filter(params)
    |> maybe_add_type_filter(params)
    |> maybe_add_application_filter(params)
  end

  defp maybe_add_node_filter(arsenal_params, params) do
    case Map.get(params, :node) do
      nil -> arsenal_params
      :all -> arsenal_params
      node when is_atom(node) -> Map.put(arsenal_params, "node", Atom.to_string(node))
      node when is_binary(node) -> Map.put(arsenal_params, "node", node)
      _ -> arsenal_params
    end
  end

  defp maybe_add_type_filter(arsenal_params, params) do
    case Map.get(params, :type) do
      nil -> arsenal_params
      :all -> arsenal_params
      type when is_atom(type) -> Map.put(arsenal_params, "type", Atom.to_string(type))
      type when is_binary(type) -> Map.put(arsenal_params, "type", type)
      _ -> arsenal_params
    end
  end

  defp maybe_add_application_filter(arsenal_params, params) do
    case Map.get(params, :application) do
      nil -> arsenal_params
      :all -> arsenal_params
      app when is_atom(app) -> Map.put(arsenal_params, "application", Atom.to_string(app))
      app when is_binary(app) -> Map.put(arsenal_params, "application", app)
      _ -> arsenal_params
    end
  end

  defp format_arsenal_error(reason) do
    case reason do
      :node_not_found ->
        "Specified node not found in cluster"

      {:process_list_error, message} ->
        "Process list operation failed: #{message}"

      :timeout ->
        "Operation timed out while fetching process data"

      other ->
        "Arsenal operation failed: #{inspect(other)}"
    end
  end

  defp format_processes(arsenal_result) do
    processes = arsenal_result.processes || []

    formatted_processes =
      processes
      |> Enum.map(&format_single_process/1)
      |> Enum.filter(&(&1 != nil))

    %{
      processes: formatted_processes,
      total_count: length(formatted_processes),
      nodes_queried: arsenal_result.nodes_queried || [],
      last_updated: DateTime.utc_now()
    }
  end

  defp format_single_process(process) do
    %{
      pid: process.pid,
      node: process.node,
      registered_name: process[:registered_name],
      initial_call: process[:initial_call],
      current_function: process[:current_function],
      memory: process[:memory] || 0,
      message_queue_len: process[:message_queue_len] || 0,
      type: process[:type] || "process",
      application: process[:application] || "unknown",
      alive: Map.get(process, :alive, true)
    }
  end

  defp load_process_data(socket) do
    socket = assign(socket, :loading, true)

    case get_processes() do
      {:ok, arsenal_result} ->
        formatted_result = format_processes(arsenal_result)

        socket
        |> assign(:processes, formatted_result.processes)
        |> assign(:processes_by_node, group_processes_by_node(formatted_result.processes))
        |> assign(:total_processes, formatted_result.total_count)
        |> assign(:filtered_process_count, formatted_result.total_count)
        |> assign(:cluster_nodes, formatted_result.nodes_queried)
        |> assign(:last_updated, formatted_result.last_updated)
        |> assign(:loading, false)

      {:error, error_message} ->
        # Log error and set empty state
        require Logger
        Logger.error("Failed to load process data: #{error_message}")

        socket
        |> assign(:processes, [])
        |> assign(:processes_by_node, %{})
        |> assign(:total_processes, 0)
        |> assign(:cluster_nodes, [])
        |> assign(:last_updated, nil)
        |> assign(:loading, false)
    end
  end

  defp group_processes_by_node(processes) do
    processes
    |> Enum.group_by(& &1.node)
    |> Enum.into(%{})
  end

  defp calculate_process_stats(processes_by_node) do
    total_processes =
      processes_by_node
      |> Map.values()
      |> List.flatten()
      |> length()

    node_stats =
      processes_by_node
      |> Enum.map(fn {node, processes} ->
        %{
          node: node,
          process_count: length(processes),
          memory_total: Enum.sum(Enum.map(processes, & &1.memory)),
          avg_message_queue:
            case length(processes) do
              0 -> 0
              count ->
                processes
                |> Enum.map(& &1.message_queue_len)
                |> Enum.sum()
                |> div(count)
            end
        }
      end)
      |> Enum.sort_by(& &1.node)

    %{
      total_processes: total_processes,
      total_nodes: map_size(processes_by_node),
      node_stats: node_stats
    }
  end

  defp format_process_info(process) do
    %{
      pid_display: format_pid(process.pid),
      name_display: format_process_name(process),
      initial_call_display: format_initial_call(process.initial_call),
      current_function_display: format_current_function(process.current_function),
      memory_display: format_memory(process.memory),
      message_queue_display: format_message_queue(process.message_queue_len),
      type_display: format_process_type(process.type),
      application_display: format_application(process.application),
      status_display: format_process_status(process)
    }
  end

  defp format_pid(pid) when is_pid(pid) do
    pid |> :erlang.pid_to_list() |> to_string()
  end
  defp format_pid(pid) when is_binary(pid), do: pid
  defp format_pid(_), do: "unknown"

  defp format_process_name(process) do
    case process.registered_name do
      nil -> "unnamed"
      name when is_atom(name) -> Atom.to_string(name)
      name when is_binary(name) -> name
      _ -> "unnamed"
    end
  end

  defp format_initial_call({module, function, arity}) when is_atom(module) and is_atom(function) do
    "#{module}.#{function}/#{arity}"
  end
  defp format_initial_call(_), do: "unknown"

  defp format_current_function({module, function, arity}) when is_atom(module) and is_atom(function) do
    "#{module}.#{function}/#{arity}"
  end
  defp format_current_function(_), do: "unknown"

  defp format_memory(memory) when is_integer(memory) and memory > 0 do
    cond do
      memory >= 1_048_576 -> "#{Float.round(memory / 1_048_576, 1)} MB"
      memory >= 1_024 -> "#{Float.round(memory / 1_024, 1)} KB"
      true -> "#{memory} B"
    end
  end
  defp format_memory(_), do: "0 B"

  defp format_message_queue(queue_len) when is_integer(queue_len) do
    cond do
      queue_len == 0 -> "empty"
      queue_len == 1 -> "1 message"
      true -> "#{queue_len} messages"
    end
  end
  defp format_message_queue(_), do: "unknown"

  defp format_process_type(type) when is_atom(type) do
    case type do
      :supervisor -> "Supervisor"
      :gen_server -> "GenServer"
      :gen_event -> "GenEvent"
      :task -> "Task"
      :process -> "Process"
      _ -> String.capitalize(Atom.to_string(type))
    end
  end
  defp format_process_type(type) when is_binary(type) do
    String.capitalize(type)
  end
  defp format_process_type(_), do: "Process"

  defp format_application(app) when is_atom(app) do
    Atom.to_string(app)
  end
  defp format_application(app) when is_binary(app) do
    app
  end
  defp format_application(_), do: "unknown"

  defp format_process_status(process) do
    if Map.get(process, :alive, true) do
      "running"
    else
      "terminated"
    end
  end

  # Real-time update functions

  defp update_process_data(socket) do
    # Preserve current UI state (filters, search, pagination, expanded nodes)
    current_filters = socket.assigns.filters
    current_search = socket.assigns.search_term
    current_page = socket.assigns.current_page
    expanded_nodes = socket.assigns.expanded_nodes

    case get_processes() do
      {:ok, arsenal_result} ->
        formatted_result = format_processes(arsenal_result)

        # Apply current filters and search to new data
        filtered_processes = apply_filters(formatted_result.processes, current_filters)
        searched_processes = search_processes(filtered_processes, current_search)

        # Apply pagination to the searched processes
        paginated_processes = paginate_processes(searched_processes, current_page, socket.assigns.per_page)
        paginated_processes_by_node = group_processes_by_node(paginated_processes)

        socket
        |> assign(:processes, formatted_result.processes)
        |> assign(:processes_by_node, paginated_processes_by_node)
        |> assign(:total_processes, formatted_result.total_count)
        |> assign(:filtered_process_count, length(searched_processes))
        |> assign(:cluster_nodes, formatted_result.nodes_queried)
        |> assign(:last_updated, formatted_result.last_updated)
        |> assign(:expanded_nodes, expanded_nodes)  # Preserve expanded state
        |> assign(:loading, false)

      {:error, error_message} ->
        # Log error but don't disrupt the UI - keep existing data
        require Logger
        Logger.error("Failed to update process data: #{error_message}")

        # Just update the last_updated timestamp to show we tried
        assign(socket, :loading, false)
    end
  end

  # Cleanup function for proper resource management
  def terminate(_reason, _socket) do
    # PubSub subscriptions are automatically cleaned up when the process terminates
    # Timers are also automatically cancelled when the process terminates
    :ok
  end
end
