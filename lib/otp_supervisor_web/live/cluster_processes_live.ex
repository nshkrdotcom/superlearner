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

  def handle_event("filter_change", form_params, socket) do
    # Extract filter values from form
    updated_filters = %{
      node: parse_filter_value(form_params["node"] || "all"),
      type: parse_filter_value(form_params["type"] || "all"),
      application: parse_filter_value(form_params["application"] || "all")
    }

    # According to requirements, filters should re-execute Arsenal operation with updated parameters
    socket =
      socket
      |> assign(:filters, updated_filters)
      # Reset to first page when filtering
      |> assign(:current_page, 1)
      |> assign(:operation_in_progress, true)
      |> reload_process_data_with_filters()

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
    all_nodes = get_unique_nodes(socket.assigns.processes)
    expanded_nodes = MapSet.new(all_nodes)

    socket =
      socket
      |> assign(:filters, default_filters)
      |> assign(:search_term, "")
      # Reset to first page when clearing filters
      |> assign(:current_page, 1)
      |> assign(:operation_in_progress, true)
      # Expand all nodes when resetting filters
      |> assign(:expanded_nodes, expanded_nodes)
      |> reload_process_data_with_filters()

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
      |> assign(:current_page, 1)
      |> assign(:search_debounce_timer, nil)
      |> apply_filters_and_search_and_update_display()

    {:noreply, socket}
  end

  def handle_event("page_change", %{"page" => page_string}, socket) do
    case Integer.parse(page_string) do
      {page, ""} when page > 0 ->
        max_page =
          calculate_max_page(socket.assigns.filtered_process_count, socket.assigns.per_page)

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

  def handle_event("retry_operation", _params, socket) do
    # Retry the last failed operation
    socket =
      socket
      |> assign(:operation_in_progress, true)
      |> assign(:error_message, nil)
      |> retry_load_process_data()

    {:noreply, socket}
  end

  # Handle real-time updates

  def handle_info(%{type: :cluster_state_change} = _event, socket) do
    # Cluster topology changed, refresh process data
    socket = update_process_data(socket)
    {:noreply, socket}
  end

  def handle_info(:refresh_processes, socket) do
    # Periodic refresh timer - only refresh if not already loading
    socket =
      if not socket.assigns.operation_in_progress do
        update_process_data(socket)
      else
        socket
      end

    # Schedule next refresh
    Process.send_after(self(), :refresh_processes, 5_000)

    {:noreply, socket}
  end

  def handle_info({:debounced_search, search_term}, socket) do
    # Handle debounced search
    socket =
      socket
      |> assign(:search_term, search_term)
      # Reset to first page when searching
      |> assign(:current_page, 1)
      |> assign(:search_debounce_timer, nil)
      |> apply_filters_and_search_and_update_display()

    {:noreply, socket}
  end

  def handle_info({:retry_operation, operation}, socket) do
    # Handle retry operations with exponential backoff
    case operation do
      :load_process_data ->
        socket = retry_load_process_data(socket)
        {:noreply, socket}

      :update_process_data ->
        socket = retry_update_process_data(socket)
        {:noreply, socket}

      _ ->
        {:noreply, socket}
    end
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
              <%= if @error_message do %>
                <div class="mt-4 p-3 bg-red-900/20 border border-red-500/30 rounded">
                  <div class="text-red-400 font-mono text-sm">
                    Error: {@error_message}
                  </div>
                  <%= if @retry_count > 0 do %>
                    <div class="text-red-400/70 font-mono text-xs mt-1">
                      Retry attempt {@retry_count}/3
                    </div>
                  <% end %>
                </div>
              <% end %>
            </div>
          <% else %>
            <!-- Refreshing Indicator -->
            <%= if @refreshing do %>
              <div class="mb-2 p-2 bg-blue-900/20 border border-blue-500/30 rounded">
                <div class="text-blue-400 font-mono text-sm flex items-center">
                  <div class="animate-spin mr-2">⟳</div>
                  Refreshing process data...
                </div>
              </div>
            <% end %>
            
    <!-- Error Message Display -->
            <%= if @error_message do %>
              <div class="mb-4 p-3 bg-red-900/20 border border-red-500/30 rounded">
                <div class="text-red-400 font-mono text-sm">
                  {@error_message}
                </div>
                <button
                  class="mt-2 bg-red-700 hover:bg-red-600 border border-red-500/30 hover:border-red-500/50 text-red-200 font-mono text-sm px-3 py-1 rounded transition-colors"
                  phx-click="retry_operation"
                  disabled={@operation_in_progress}
                >
                  <%= if @operation_in_progress do %>
                    Retrying...
                  <% else %>
                    Retry
                  <% end %>
                </button>
              </div>
            <% end %>
            <!-- Filter Controls -->
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
              <form phx-change="filter_change">
                <div class="flex flex-wrap items-center gap-4">
                  <!-- Node Filter -->
                  <div class="flex items-center space-x-2">
                    <label class="text-green-400/70 font-mono text-sm">Node:</label>
                    <select
                      class="bg-gray-800 border border-green-500/30 text-green-400 font-mono text-sm rounded px-2 py-1 focus:border-green-500 focus:ring-1 focus:ring-green-500/50 focus:outline-none disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200"
                      name="node"
                      disabled={@operation_in_progress}
                    >
                      <option value="all" selected={@filters.node == :all}>All Nodes</option>
                      <%= for node <- get_unique_nodes(@processes) do %>
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
                      disabled={@operation_in_progress}
                    >
                      <option value="all" selected={@filters.type == :all}>All Types</option>
                      <%= for type <- get_unique_types(@processes) do %>
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
                      disabled={@operation_in_progress}
                    >
                      <option value="all" selected={@filters.application == :all}>All Apps</option>
                      <%= for app <- get_unique_applications(@processes) do %>
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
                      disabled={@operation_in_progress}
                    >
                      <%= if @operation_in_progress do %>
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
                <div class="flex items-center space-x-2 text-xs font-mono">
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
                <div class="mt-3 pt-3 border-t border-green-500/20">
                  <div class="text-green-400/70 font-mono text-sm">
                    Showing {@filtered_process_count} of {@total_processes} processes
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
                    <div
                      class="p-3 border-b border-green-500/20 cursor-pointer hover:bg-gray-700/30 transition-all duration-200 ease-in-out"
                      phx-click="toggle_node"
                      phx-value-node={node}
                    >
                      <div class="flex items-center justify-between">
                        <div class="flex items-center space-x-3">
                          <span class="text-green-400 font-mono text-sm transition-transform duration-200 ease-in-out">
                            <%= if node_expanded?(assigns, node) do %>
                              <span class="inline-block transform rotate-90">▶</span>
                            <% else %>
                              <span class="inline-block">▶</span>
                            <% end %>
                          </span>
                          <span class="text-green-300 font-mono font-semibold">
                            {node}
                          </span>
                        </div>
                        <div class="text-green-400/70 font-mono text-sm">
                          {length(processes)} processes
                        </div>
                      </div>
                    </div>
                    
    <!-- Node Content (Expandable) -->
                    <%= if node_expanded?(assigns, node) do %>
                      <div class="p-3 animate-in slide-in-from-top-2 duration-300 ease-out">
                        <%= if length(processes) == 0 do %>
                          <div class="text-green-400/50 font-mono text-sm italic text-center py-4">
                            No processes
                          </div>
                        <% else %>
                          <div class="space-y-2">
                            <%= for process <- processes do %>
                              <% formatted = format_process_info(process) %>
                              <% type_colors = get_process_type_colors(process.type) %>
                              <% status_colors = get_process_status_colors(process) %>
                              <div class="bg-gray-800/50 rounded p-3 border border-green-500/10 hover:border-green-500/30 transition-all duration-200 ease-in-out hover:shadow-lg hover:shadow-green-500/10">
                                <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-3 text-sm font-mono">
                                  <!-- Process Identity -->
                                  <div class="space-y-1">
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">PID:</span>
                                      <span class="font-semibold">
                                        {raw(
                                          highlight_search_term(formatted.pid_display, @search_term)
                                        )}
                                      </span>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Name:</span>
                                      <span class="font-medium">
                                        {raw(
                                          highlight_search_term(formatted.name_display, @search_term)
                                        )}
                                      </span>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Type:</span>
                                      <span class={"inline-flex items-center px-2 py-1 rounded text-xs font-medium #{type_colors.bg} #{type_colors.text} #{type_colors.border}"}>
                                        <span class={"mr-1 #{type_colors.icon}"}>
                                          {get_process_type_icon(process.type)}
                                        </span>
                                        {formatted.type_display}
                                      </span>
                                    </div>
                                  </div>
                                  
    <!-- Process Details -->
                                  <div class="space-y-1">
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Memory:</span>
                                      <span class={get_memory_color_class(process.memory)}>
                                        {formatted.memory_display}
                                      </span>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Queue:</span>
                                      <span class={get_queue_color_class(process.message_queue_len)}>
                                        {formatted.message_queue_display}
                                      </span>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">App:</span>
                                      <span class="font-medium">
                                        {raw(
                                          highlight_search_term(
                                            formatted.application_display,
                                            @search_term
                                          )
                                        )}
                                      </span>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Status:</span>
                                      <span class={"inline-flex items-center px-2 py-1 rounded text-xs font-medium #{status_colors.bg} #{status_colors.text} #{status_colors.border}"}>
                                        <span class="mr-1">{get_process_status_icon(process)}</span>
                                        {formatted.status_display}
                                      </span>
                                    </div>
                                  </div>
                                  
    <!-- Function Information -->
                                  <div class="space-y-1 md:col-span-2 lg:col-span-1">
                                    <div class="text-green-300 text-xs">
                                      <span class="text-green-400/70">Initial:</span>
                                      <div class="break-all">
                                        {raw(
                                          highlight_search_term(
                                            formatted.initial_call_display,
                                            @search_term
                                          )
                                        )}
                                      </div>
                                    </div>
                                    <div class="text-green-300 text-xs">
                                      <span class="text-green-400/70">Current:</span>
                                      <div class="break-all">
                                        {raw(
                                          highlight_search_term(
                                            formatted.current_function_display,
                                            @search_term
                                          )
                                        )}
                                      </div>
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
                      Showing {start_item}-{end_item} of {@filtered_process_count} processes
                      (Page {@current_page} of {max_page})
                    </div>
                    
    <!-- Pagination Buttons -->
                    <div class="flex items-center space-x-2">
                      <!-- First Page -->
                      <%= if @current_page > 1 and not @operation_in_progress do %>
                        <button
                          class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors"
                          phx-click="page_change"
                          phx-value-page="1"
                          title="First page"
                        >
                          ««
                        </button>
                      <% else %>
                        <button
                          class="bg-gray-800 border border-green-500/10 text-green-400/30 font-mono text-sm px-3 py-1 rounded cursor-not-allowed"
                          disabled
                          title="First page"
                        >
                          ««
                        </button>
                      <% end %>
                      
    <!-- Previous Page -->
                      <%= if @current_page > 1 and not @operation_in_progress do %>
                        <button
                          class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors"
                          phx-click="page_change"
                          phx-value-page={@current_page - 1}
                          title="Previous page"
                        >
                          ‹
                        </button>
                      <% else %>
                        <button
                          class="bg-gray-800 border border-green-500/10 text-green-400/30 font-mono text-sm px-3 py-1 rounded cursor-not-allowed"
                          disabled
                          title="Previous page"
                        >
                          ‹
                        </button>
                      <% end %>
                      
    <!-- Page Numbers -->
                      <%= for page_num <- pagination_range(@current_page, max_page) do %>
                        <%= if page_num == @current_page do %>
                          <button
                            class="bg-green-500/20 border border-green-500/50 text-green-300 font-mono text-sm px-3 py-1 rounded font-semibold"
                            disabled
                          >
                            {page_num}
                          </button>
                        <% else %>
                          <button
                            class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
                            phx-click="page_change"
                            phx-value-page={page_num}
                            disabled={@operation_in_progress}
                          >
                            {page_num}
                          </button>
                        <% end %>
                      <% end %>
                      
    <!-- Next Page -->
                      <%= if @current_page < max_page and not @operation_in_progress do %>
                        <button
                          class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors"
                          phx-click="page_change"
                          phx-value-page={@current_page + 1}
                          title="Next page"
                        >
                          ›
                        </button>
                      <% else %>
                        <button
                          class="bg-gray-800 border border-green-500/10 text-green-400/30 font-mono text-sm px-3 py-1 rounded cursor-not-allowed"
                          disabled
                          title="Next page"
                        >
                          ›
                        </button>
                      <% end %>
                      
    <!-- Last Page -->
                      <%= if @current_page < max_page and not @operation_in_progress do %>
                        <button
                          class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 hover:border-green-500/50 text-green-400 font-mono text-sm px-3 py-1 rounded transition-colors"
                          phx-click="page_change"
                          phx-value-page={max_page}
                          title="Last page"
                        >
                          »»
                        </button>
                      <% else %>
                        <button
                          class="bg-gray-800 border border-green-500/10 text-green-400/30 font-mono text-sm px-3 py-1 rounded cursor-not-allowed"
                          disabled
                          title="Last page"
                        >
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
    # Get configuration values
    process_config = Application.get_env(:otp_supervisor, :process_listing, [])
    per_page = Keyword.get(process_config, :per_page, 100)

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
    |> assign(:per_page, per_page)
    |> assign(:loading, false)
    |> assign(:refreshing, false)
    |> assign(:operation_in_progress, false)
    |> assign(:error_message, nil)
    |> assign(:retry_count, 0)
    |> assign(:cluster_nodes, [])
    |> assign(:last_updated, nil)
    |> assign(:expanded_nodes, MapSet.new())
    |> assign(:search_debounce_timer, nil)
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
    cluster_health = get_cluster_health_status()

    base_metrics = [
      %{
        label: "Total Processes",
        value: format_number(assigns.total_processes)
      },
      %{
        label: "Filtered",
        value: filtered_count_display(assigns)
      },
      %{
        label: "Cluster Nodes",
        value: format_number(length(assigns.cluster_nodes))
      }
    ]

    # Add cluster health indicator
    health_metric = %{
      label: "Health",
      value: format_cluster_health(cluster_health)
    }

    # Add loading/refresh status
    status_metric = %{
      label: "Status",
      value: get_status_indicator(assigns)
    }

    # Add last updated timestamp
    updated_metric = %{
      label: "Updated",
      value: format_last_updated_enhanced(assigns.last_updated)
    }

    base_metrics ++ [health_metric, status_metric, updated_metric]
  end

  defp filtered_count_display(assigns) do
    if assigns.search_term != "" or
         assigns.filters != %{node: :all, type: :all, application: :all} do
      "#{Map.get(assigns, :filtered_process_count, 0)}"
    else
      "#{assigns.total_processes}"
    end
  end

  # Enhanced status bar helper functions

  defp get_cluster_health_status do
    try do
      ClusterStateManager.get_partition_status()
    rescue
      _ -> :unknown
    catch
      :exit, _ -> :unknown
    end
  end

  defp format_cluster_health(:healthy), do: "✓ Healthy"
  defp format_cluster_health(:partitioned), do: "⚠ Partitioned"
  defp format_cluster_health(:minority_partition), do: "⚠ Minority"
  defp format_cluster_health(:partial_partition), do: "⚠ Partial"
  defp format_cluster_health(:unknown), do: "? Unknown"
  defp format_cluster_health(_), do: "? Unknown"

  defp get_status_indicator(assigns) do
    cond do
      assigns.loading -> "⟳ Loading..."
      assigns.refreshing -> "⟳ Refreshing"
      assigns.operation_in_progress -> "⟳ Processing"
      assigns.error_message -> "✗ Error"
      true -> "✓ Ready"
    end
  end

  defp format_last_updated_enhanced(nil), do: "Never"

  defp format_last_updated_enhanced(datetime) do
    now = DateTime.utc_now()
    diff_seconds = DateTime.diff(now, datetime, :second)

    cond do
      diff_seconds < 60 -> "#{diff_seconds}s ago"
      diff_seconds < 3600 -> "#{div(diff_seconds, 60)}m ago"
      true -> "#{div(diff_seconds, 3600)}h ago"
    end
  end

  defp format_number(num) when is_integer(num) do
    num
    |> Integer.to_string()
    |> String.reverse()
    |> String.replace(~r/(\d{3})(?=\d)/, "\\1,")
    |> String.reverse()
  end

  defp format_number(_), do: "0"

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
    # Optimize by only updating what's necessary
    current_processes = socket.assigns.processes
    current_filters = socket.assigns.filters
    current_search = socket.assigns.search_term
    current_page = socket.assigns.current_page
    per_page = socket.assigns.per_page

    # Apply filters and search efficiently
    filtered_processes = apply_filters(current_processes, current_filters)
    searched_processes = search_processes(filtered_processes, current_search)

    # Calculate pagination efficiently with fair node distribution
    total_filtered = length(searched_processes)
    processes_by_node = group_processes_by_node(searched_processes)
    paginated_by_node = paginate_processes_fairly(processes_by_node, current_page, per_page)

    # Only update assigns that have changed to minimize re-rendering
    socket
    |> maybe_assign(:processes_by_node, paginated_by_node, socket.assigns.processes_by_node)
    |> maybe_assign(
      :filtered_process_count,
      total_filtered,
      socket.assigns.filtered_process_count
    )
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

  # Reload process data with current filters applied
  defp reload_process_data_with_filters(socket) do
    filters = socket.assigns.filters

    # Build filter parameters for Arsenal operation
    filter_params =
      %{}
      |> maybe_add_filter_param(:node, filters.node)
      |> maybe_add_filter_param(:type, filters.type)
      |> maybe_add_filter_param(:application, filters.application)

    case get_processes(filter_params) do
      {:ok, arsenal_result} ->
        formatted_result = format_processes(arsenal_result)

        # Apply client-side search to the filtered results
        search_term = socket.assigns.search_term
        searched_processes = search_processes(formatted_result.processes, search_term)

        # Apply pagination with fair node distribution
        current_page = socket.assigns.current_page
        per_page = socket.assigns.per_page

        # Group by node first, then paginate to ensure all nodes are represented
        processes_by_node = group_processes_by_node(searched_processes)
        paginated_by_node = paginate_processes_fairly(processes_by_node, current_page, per_page)

        socket
        |> assign(:processes, formatted_result.processes)
        |> assign(:processes_by_node, paginated_by_node)
        |> assign(:total_processes, formatted_result.total_count)
        |> assign(:filtered_process_count, length(searched_processes))
        |> assign(:cluster_nodes, formatted_result.nodes_queried)
        |> assign(:last_updated, formatted_result.last_updated)
        |> assign(:operation_in_progress, false)
        |> assign(:error_message, nil)
        |> assign(:retry_count, 0)

      {:error, error_message} ->
        socket
        |> assign(:operation_in_progress, false)
        |> assign(:error_message, error_message)
    end
  end

  defp maybe_add_filter_param(params, _key, :all), do: params

  defp maybe_add_filter_param(params, key, value) when value != nil do
    Map.put(params, key, value)
  end

  defp maybe_add_filter_param(params, _key, _value), do: params

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
    # Get configuration values
    process_config = Application.get_env(:otp_supervisor, :process_listing, [])
    default_limit = Keyword.get(process_config, :default_limit, 1000)

    %{
      "include_details" => true,
      "limit" => Map.get(params, :limit, default_limit)
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
    end
  end

  defp format_processes(arsenal_result) do
    processes = arsenal_result.processes

    formatted_processes =
      processes
      |> Enum.map(&format_single_process/1)
      |> Enum.filter(&(&1 != nil))

    %{
      processes: formatted_processes,
      total_count: length(formatted_processes),
      nodes_queried: arsenal_result.nodes_queried,
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
    socket =
      socket
      |> assign(:loading, true)
      |> assign(:operation_in_progress, true)
      |> assign(:error_message, nil)

    case get_processes() do
      {:ok, arsenal_result} ->
        formatted_result = format_processes(arsenal_result)

        # Get all nodes and expand them by default
        all_nodes = get_unique_nodes(formatted_result.processes)
        expanded_nodes = MapSet.new(all_nodes)

        # Group by node first, then paginate fairly
        processes_by_node = group_processes_by_node(formatted_result.processes)

        paginated_by_node =
          paginate_processes_fairly(processes_by_node, 1, socket.assigns.per_page)

        socket
        |> assign(:processes, formatted_result.processes)
        |> assign(:processes_by_node, paginated_by_node)
        |> assign(:total_processes, formatted_result.total_count)
        |> assign(:filtered_process_count, formatted_result.total_count)
        |> assign(:cluster_nodes, formatted_result.nodes_queried)
        |> assign(:last_updated, formatted_result.last_updated)
        |> assign(:expanded_nodes, expanded_nodes)
        |> assign(:loading, false)
        |> assign(:operation_in_progress, false)
        |> assign(:retry_count, 0)

      {:error, error_message} ->
        # Log error and set error state
        require Logger
        Logger.error("Failed to load process data: #{error_message}")

        socket
        |> assign(:processes, [])
        |> assign(:processes_by_node, %{})
        |> assign(:total_processes, 0)
        |> assign(:cluster_nodes, [])
        |> assign(:last_updated, nil)
        |> assign(:loading, false)
        |> assign(:operation_in_progress, false)
        |> assign(:error_message, error_message)
    end
  end

  defp group_processes_by_node(processes) do
    processes
    |> Enum.group_by(& &1.node)
    |> Enum.into(%{})
  end

  # Paginate processes fairly across nodes
  defp paginate_processes_fairly(processes_by_node, current_page, per_page) do
    nodes = Map.keys(processes_by_node) |> Enum.sort()
    num_nodes = length(nodes)

    if num_nodes == 0 do
      %{}
    else
      # Calculate how many processes to show per node on this page
      processes_per_node = max(1, div(per_page, num_nodes))
      # Add any remainder to ensure we fill the page
      remainder = rem(per_page, num_nodes)

      # Calculate the offset for each node based on the current page
      offset_per_node = (current_page - 1) * processes_per_node

      # Paginate each node's processes
      nodes
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {node, index}, acc ->
        node_processes = Map.get(processes_by_node, node, [])

        # Add extra process to first nodes if there's a remainder
        extra = if index < remainder, do: 1, else: 0
        node_limit = processes_per_node + extra
        node_offset = offset_per_node + if index < remainder, do: index, else: remainder

        paginated =
          node_processes
          |> Enum.drop(node_offset)
          |> Enum.take(node_limit)

        if length(paginated) > 0 do
          Map.put(acc, node, paginated)
        else
          acc
        end
      end)
    end
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

  defp format_initial_call({module, function, arity})
       when is_atom(module) and is_atom(function) do
    "#{module}.#{function}/#{arity}"
  end

  defp format_initial_call(_), do: "unknown"

  defp format_current_function({module, function, arity})
       when is_atom(module) and is_atom(function) do
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
    # Don't update if already in progress
    if socket.assigns.operation_in_progress do
      socket
    else
      # Preserve current UI state (filters, search, pagination, expanded nodes)
      current_filters = socket.assigns.filters
      current_search = socket.assigns.search_term
      current_page = socket.assigns.current_page
      expanded_nodes = socket.assigns.expanded_nodes

      socket =
        socket
        |> assign(:refreshing, true)
        |> assign(:operation_in_progress, true)

      case get_processes() do
        {:ok, arsenal_result} ->
          formatted_result = format_processes(arsenal_result)

          # Apply current filters and search to new data
          filtered_processes = apply_filters(formatted_result.processes, current_filters)
          searched_processes = search_processes(filtered_processes, current_search)

          # Apply pagination with fair node distribution
          processes_by_node = group_processes_by_node(searched_processes)

          paginated_by_node =
            paginate_processes_fairly(processes_by_node, current_page, socket.assigns.per_page)

          socket
          |> assign(:processes, formatted_result.processes)
          |> assign(:processes_by_node, paginated_by_node)
          |> assign(:total_processes, formatted_result.total_count)
          |> assign(:filtered_process_count, length(searched_processes))
          |> assign(:cluster_nodes, formatted_result.nodes_queried)
          |> assign(:last_updated, formatted_result.last_updated)
          # Preserve expanded state
          |> assign(:expanded_nodes, expanded_nodes)
          |> assign(:refreshing, false)
          |> assign(:operation_in_progress, false)
          |> assign(:error_message, nil)
          |> assign(:retry_count, 0)

        {:error, error_message} ->
          # Log error but don't disrupt the UI - keep existing data
          require Logger
          Logger.error("Failed to update process data: #{error_message}")

          socket
          |> assign(:refreshing, false)
          |> assign(:operation_in_progress, false)
          |> assign(:error_message, error_message)
      end
    end
  end

  # Retry functions with exponential backoff
  defp retry_load_process_data(socket) do
    retry_count = socket.assigns.retry_count + 1
    error_type = categorize_error(socket.assigns.error_message || "")

    if should_retry_operation?(retry_count, error_type) do
      # Calculate exponential backoff delay (300ms, 600ms, 1200ms)
      delay = (300 * :math.pow(2, retry_count - 1)) |> round()

      # Schedule retry after delay
      Process.send_after(self(), {:retry_operation, :load_process_data}, delay)

      socket
      |> assign(:retry_count, retry_count)
      |> assign(:operation_in_progress, true)
    else
      # Max retries reached
      socket
      |> assign(:operation_in_progress, false)
      |> assign(
        :error_message,
        "Failed to load process data after #{retry_count} attempts. Please try again manually."
      )
    end
  end

  defp retry_update_process_data(socket) do
    retry_count = socket.assigns.retry_count + 1
    error_type = categorize_error(socket.assigns.error_message || "")

    if should_retry_operation?(retry_count, error_type) do
      # Calculate exponential backoff delay
      delay = (300 * :math.pow(2, retry_count - 1)) |> round()

      # Schedule retry after delay
      Process.send_after(self(), {:retry_operation, :update_process_data}, delay)

      socket
      |> assign(:retry_count, retry_count)
      |> assign(:operation_in_progress, true)
    else
      # Max retries reached - keep existing data
      socket
      |> assign(:operation_in_progress, false)
      |> assign(
        :error_message,
        "Failed to update process data after #{retry_count} attempts. Retrying in next refresh cycle."
      )
    end
  end

  # Performance optimization functions

  # Helper function to only assign if value has changed (performance optimization)
  defp maybe_assign(socket, key, new_value, current_value) do
    if new_value != current_value do
      assign(socket, key, new_value)
    else
      socket
    end
  end

  # Enhanced error handling with circuit breaker pattern
  defp should_retry_operation?(retry_count, error_type) do
    case error_type do
      # Fewer retries for timeouts
      :timeout -> retry_count < 2
      # More retries for network issues
      :network_error -> retry_count < 3
      # Don't retry for permanent errors
      :node_not_found -> false
      _ -> retry_count < 3
    end
  end

  defp categorize_error(error_message) when is_binary(error_message) do
    cond do
      String.contains?(error_message, "timeout") -> :timeout
      String.contains?(error_message, "network") -> :network_error
      String.contains?(error_message, "not found") -> :node_not_found
      true -> :generic_error
    end
  end

  defp categorize_error(_), do: :generic_error

  # Process type styling functions

  defp get_process_type_colors(type) do
    case normalize_process_type(type) do
      :supervisor ->
        %{
          bg: "bg-blue-500/20",
          text: "text-blue-300",
          border: "border border-blue-500/30",
          icon: "text-blue-400"
        }

      :gen_server ->
        %{
          bg: "bg-green-500/20",
          text: "text-green-300",
          border: "border border-green-500/30",
          icon: "text-green-400"
        }

      :gen_event ->
        %{
          bg: "bg-purple-500/20",
          text: "text-purple-300",
          border: "border border-purple-500/30",
          icon: "text-purple-400"
        }

      :task ->
        %{
          bg: "bg-yellow-500/20",
          text: "text-yellow-300",
          border: "border border-yellow-500/30",
          icon: "text-yellow-400"
        }

      :process ->
        %{
          bg: "bg-gray-500/20",
          text: "text-gray-300",
          border: "border border-gray-500/30",
          icon: "text-gray-400"
        }

      _ ->
        %{
          bg: "bg-gray-500/20",
          text: "text-gray-300",
          border: "border border-gray-500/30",
          icon: "text-gray-400"
        }
    end
  end

  defp get_process_status_colors(process) do
    if Map.get(process, :alive, true) do
      %{
        bg: "bg-green-500/10",
        text: "text-green-400",
        border: "border-green-500/20"
      }
    else
      %{
        bg: "bg-red-500/10",
        text: "text-red-400",
        border: "border-red-500/20"
      }
    end
  end

  defp get_process_type_icon(type) do
    case normalize_process_type(type) do
      :supervisor -> "⚡"
      :gen_server -> "⚙"
      :gen_event -> "📡"
      :task -> "⚡"
      :process -> "●"
      _ -> "●"
    end
  end

  defp normalize_process_type(type) when is_atom(type), do: type

  defp normalize_process_type(type) when is_binary(type) do
    case String.downcase(type) do
      "supervisor" -> :supervisor
      "gen_server" -> :gen_server
      "gen_event" -> :gen_event
      "task" -> :task
      "process" -> :process
      _ -> :process
    end
  end

  defp normalize_process_type(_), do: :process

  # Memory and queue color coding functions

  defp get_memory_color_class(memory) when is_integer(memory) do
    cond do
      # >= 10MB - high memory usage
      memory >= 10_485_760 -> "text-red-400 font-semibold"
      # >= 1MB - moderate memory usage
      memory >= 1_048_576 -> "text-yellow-400 font-medium"
      # >= 100KB - normal memory usage
      memory >= 102_400 -> "text-green-300"
      # < 100KB - low memory usage
      true -> "text-green-400/70"
    end
  end

  defp get_memory_color_class(_), do: "text-gray-400"

  defp get_queue_color_class(queue_len) when is_integer(queue_len) do
    cond do
      # >= 100 messages - high queue
      queue_len >= 100 -> "text-red-400 font-semibold"
      # >= 10 messages - moderate queue
      queue_len >= 10 -> "text-yellow-400 font-medium"
      # > 0 messages - some queue
      queue_len > 0 -> "text-green-300"
      # 0 messages - empty queue
      true -> "text-green-400/70"
    end
  end

  defp get_queue_color_class(_), do: "text-gray-400"

  defp get_process_status_icon(process) do
    if Map.get(process, :alive, true) do
      # Running process
      "●"
    else
      # Terminated process
      "○"
    end
  end

  # Cleanup function for proper resource management
  def terminate(_reason, socket) do
    # Cancel any pending debounce timers
    if socket.assigns[:search_debounce_timer] do
      Process.cancel_timer(socket.assigns.search_debounce_timer)
    end

    # PubSub subscriptions are automatically cleaned up when the process terminates
    # Timers are also automatically cancelled when the process terminates
    :ok
  end
end
