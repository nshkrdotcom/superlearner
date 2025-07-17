defmodule OtpSupervisorWeb.Live.SupervisorLive do
  use Phoenix.LiveView
  import Phoenix.HTML

  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterSupervisionTrees
  alias OTPSupervisor.Distributed.ClusterStateManager

  @moduledoc """
  OTP supervisor monitoring and control interface with cluster-wide view.

  Displays supervisors from all nodes in the cluster with filtering capabilities.
  """

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:page_title, "Supervisor Monitor")
      |> assign(:current_page, "supervisor")
      |> assign_initial_state()
      |> load_supervisor_data()

    if connected?(socket) do
      # Subscribe to cluster state changes
      ClusterStateManager.subscribe_to_changes()
      # Set up periodic refresh timer (5 seconds)
      Process.send_after(self(), :refresh_supervisors, 5_000)
    end

    {:ok, socket}
  end

  def handle_info(%{type: :cluster_state_change} = _event, socket) do
    # Cluster topology changed, refresh supervisor data
    socket = update_supervisor_data(socket)
    {:noreply, socket}
  end

  def handle_info(:refresh_supervisors, socket) do
    socket =
      if not socket.assigns.operation_in_progress do
        update_supervisor_data(socket)
      else
        socket
      end

    # Schedule next refresh
    Process.send_after(self(), :refresh_supervisors, 5_000)
    {:noreply, socket}
  end

  def handle_info({:debounced_search, search_term}, socket) do
    # Handle debounced search
    socket =
      socket
      |> assign(:search_term, search_term)
      |> assign(:search_debounce_timer, nil)
      |> apply_filters()

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
        id="supervisor-status-bar"
        title="Supervisor Monitor"
        metrics={status_bar_metrics(assigns)}
        navigation_links={TerminalNavigationLinks.page_navigation_links("supervisor", %{})}
      />
      
    <!-- Main Content Area -->
      <div class="flex-1 p-4 overflow-hidden">
        <div class="h-full bg-gray-800 rounded border border-green-500/30 p-4 overflow-y-auto">
          <%= if @loading do %>
            <div class="text-center text-green-400/70 font-mono py-8">
              <div class="animate-pulse">Loading cluster supervisors...</div>
            </div>
          <% else %>
            <!-- Filter Controls -->
            <div class="mb-4 p-3 bg-gray-900/50 rounded border border-green-500/20">
              <!-- Search Form -->
              <form phx-change="search_change" class="mb-4">
                <div class="flex items-center space-x-2">
                  <label class="text-green-400/70 font-mono text-sm">Search:</label>
                  <input
                    type="text"
                    value={@search_term}
                    placeholder="Search by name or PID..."
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
                      <%= for node <- get_unique_nodes(@supervisors_by_node) do %>
                        <option value={node} selected={@filters.node == node}>{node}</option>
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
                      <%= for app <- get_unique_applications(@supervisors) do %>
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
                      Clear Filters
                    </button>
                  <% end %>
                </div>
              </form>
              
    <!-- Active Filter Indicators -->
              <%= if has_active_filters?(@filters) or @search_term != "" do %>
                <div class="mt-3 pt-3 border-t border-green-500/20">
                  <div class="flex items-center space-x-2 text-xs font-mono">
                    <span class="text-green-400/70">Active:</span>
                    <%= if @search_term != "" do %>
                      <span class="bg-green-500/20 text-green-300 px-2 py-1 rounded">
                        Search: "{@search_term}"
                      </span>
                    <% end %>
                    <%= if @filters.node != :all do %>
                      <span class="bg-green-500/20 text-green-300 px-2 py-1 rounded">
                        Node: {@filters.node}
                      </span>
                    <% end %>
                    <%= if @filters.application != :all do %>
                      <span class="bg-green-500/20 text-green-300 px-2 py-1 rounded">
                        App: {@filters.application}
                      </span>
                    <% end %>
                  </div>

                  <div class="text-green-400/70 font-mono text-sm mt-2">
                    Showing {count_filtered_supervisors(@filtered_supervisors_by_node)} of {@total_supervisors} supervisors
                  </div>
                </div>
              <% end %>
            </div>

            <%= if map_size(@filtered_supervisors_by_node) == 0 do %>
              <div class="text-center text-green-400/70 font-mono py-8">
                No supervisors found
              </div>
            <% else %>
              <div class="space-y-4">
                <%= for {node, node_data} <- Enum.sort(@filtered_supervisors_by_node) do %>
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
                          {Map.get(node_data, :total_supervisors, 0)} supervisors
                        </div>
                      </div>
                    </div>
                    
    <!-- Node Content (Expandable) -->
                    <%= if node_expanded?(assigns, node) do %>
                      <div class="p-3 animate-in slide-in-from-top-2 duration-300 ease-out">
                        <% supervisors = Map.get(node_data, :supervisors, []) %>
                        <%= if length(supervisors) == 0 do %>
                          <div class="text-green-400/50 font-mono text-sm italic text-center py-4">
                            No supervisors
                          </div>
                        <% else %>
                          <div class="space-y-2">
                            <%= for supervisor <- supervisors do %>
                              <div class="bg-gray-800/50 rounded p-3 border border-green-500/10 hover:border-green-500/30 transition-all duration-200 ease-in-out hover:shadow-lg hover:shadow-green-500/10">
                                <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-3 text-sm font-mono">
                                  <div class="space-y-1">
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Name:</span>
                                      <span class="font-semibold">
                                        {raw(
                                          highlight_search_term(
                                            to_string(supervisor.name),
                                            @search_term
                                          )
                                        )}
                                      </span>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">PID:</span>
                                      <span>
                                        {raw(
                                          highlight_search_term(
                                            format_pid(supervisor.pid),
                                            @search_term
                                          )
                                        )}
                                      </span>
                                    </div>
                                  </div>

                                  <div class="space-y-1">
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Strategy:</span>
                                      <span>{format_strategy(supervisor.strategy)}</span>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Children:</span>
                                      <span>{supervisor.child_count}</span>
                                    </div>
                                  </div>

                                  <div class="space-y-1">
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">App:</span>
                                      <span>{supervisor.application}</span>
                                    </div>
                                    <div class="text-green-300">
                                      <span class="text-green-400/70">Status:</span>
                                      <span class={get_status_color_class(supervisor.alive)}>
                                        {if supervisor.alive, do: "Running", else: "Stopped"}
                                      </span>
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
            <% end %>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  # Event handlers

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
    updated_filters = %{
      node: parse_filter_value(form_params["node"] || "all"),
      application: parse_filter_value(form_params["application"] || "all")
    }

    socket =
      socket
      |> assign(:filters, updated_filters)
      |> apply_filters()

    {:noreply, socket}
  end

  def handle_event("clear_filters", _params, socket) do
    default_filters = %{
      node: :all,
      application: :all
    }

    # Get all nodes to expand them
    all_nodes = get_unique_nodes(socket.assigns.supervisors_by_node)
    expanded_nodes = MapSet.new(all_nodes)

    socket =
      socket
      |> assign(:filters, default_filters)
      |> assign(:expanded_nodes, expanded_nodes)
      |> apply_filters()

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
      |> apply_filters()

    {:noreply, socket}
  end

  # Private functions

  defp assign_initial_state(socket) do
    socket
    |> assign(:supervisors, [])
    |> assign(:supervisors_by_node, %{})
    |> assign(:filtered_supervisors_by_node, %{})
    |> assign(:total_supervisors, 0)
    |> assign(:filters, %{
      node: :all,
      application: :all
    })
    |> assign(:search_term, "")
    |> assign(:search_debounce_timer, nil)
    |> assign(:loading, false)
    |> assign(:refreshing, false)
    |> assign(:operation_in_progress, false)
    |> assign(:error_message, nil)
    |> assign(:cluster_nodes, [])
    |> assign(:last_updated, nil)
    |> assign(:expanded_nodes, MapSet.new())
  end

  defp load_supervisor_data(socket) do
    socket =
      socket
      |> assign(:loading, true)
      |> assign(:operation_in_progress, true)
      |> assign(:error_message, nil)

    case get_cluster_supervisors() do
      {:ok, result} ->
        all_nodes = Map.keys(result.supervision_trees) |> Enum.sort()
        expanded_nodes = MapSet.new(all_nodes)

        socket
        |> assign(:supervisors, flatten_supervisors(result.supervision_trees))
        |> assign(:supervisors_by_node, result.supervision_trees)
        |> assign(:filtered_supervisors_by_node, result.supervision_trees)
        |> assign(:total_supervisors, result.summary.total_supervisors)
        |> assign(:cluster_nodes, result.summary.nodes_queried)
        |> assign(:last_updated, DateTime.utc_now())
        |> assign(:expanded_nodes, expanded_nodes)
        |> assign(:loading, false)
        |> assign(:operation_in_progress, false)

      {:error, error_message} ->
        require Logger
        Logger.error("Failed to load supervisor data: #{error_message}")

        socket
        |> assign(:supervisors, [])
        |> assign(:supervisors_by_node, %{})
        |> assign(:filtered_supervisors_by_node, %{})
        |> assign(:total_supervisors, 0)
        |> assign(:cluster_nodes, [])
        |> assign(:last_updated, nil)
        |> assign(:loading, false)
        |> assign(:operation_in_progress, false)
        |> assign(:error_message, error_message)
    end
  end

  defp update_supervisor_data(socket) do
    if socket.assigns.operation_in_progress do
      socket
    else
      socket =
        socket
        |> assign(:refreshing, true)
        |> assign(:operation_in_progress, true)

      case get_cluster_supervisors() do
        {:ok, result} ->
          socket
          |> assign(:supervisors, flatten_supervisors(result.supervision_trees))
          |> assign(:supervisors_by_node, result.supervision_trees)
          |> assign(:total_supervisors, result.summary.total_supervisors)
          |> assign(:cluster_nodes, result.summary.nodes_queried)
          |> assign(:last_updated, DateTime.utc_now())
          |> assign(:refreshing, false)
          |> assign(:operation_in_progress, false)
          |> assign(:error_message, nil)
          |> apply_filters()

        {:error, error_message} ->
          require Logger
          Logger.error("Failed to update supervisor data: #{error_message}")

          socket
          |> assign(:refreshing, false)
          |> assign(:operation_in_progress, false)
          |> assign(:error_message, error_message)
      end
    end
  end

  defp apply_filters(socket) do
    filters = socket.assigns.filters
    search_term = socket.assigns.search_term
    supervisors_by_node = socket.assigns.supervisors_by_node

    filtered_by_node =
      supervisors_by_node
      |> filter_by_node(filters.node)
      |> filter_by_application(filters.application)
      |> filter_by_search(search_term)

    socket
    |> assign(:filtered_supervisors_by_node, filtered_by_node)
  end

  defp filter_by_node(supervisors_by_node, :all), do: supervisors_by_node

  defp filter_by_node(supervisors_by_node, node) do
    case Map.get(supervisors_by_node, node) do
      nil -> %{}
      node_data -> %{node => node_data}
    end
  end

  defp filter_by_application(supervisors_by_node, :all), do: supervisors_by_node

  defp filter_by_application(supervisors_by_node, app) do
    supervisors_by_node
    |> Enum.map(fn {node, node_data} ->
      filtered_supervisors =
        Enum.filter(node_data.supervisors, fn supervisor ->
          supervisor.application == app
        end)

      updated_node_data =
        node_data
        |> Map.put(:supervisors, filtered_supervisors)
        |> Map.put(:total_supervisors, length(filtered_supervisors))

      {node, updated_node_data}
    end)
    |> Enum.filter(fn {_node, node_data} ->
      length(node_data.supervisors) > 0
    end)
    |> Enum.into(%{})
  end

  defp filter_by_search(supervisors_by_node, ""), do: supervisors_by_node

  defp filter_by_search(supervisors_by_node, search_term) do
    normalized_search = String.downcase(String.trim(search_term))

    supervisors_by_node
    |> Enum.map(fn {node, node_data} ->
      filtered_supervisors =
        Enum.filter(node_data.supervisors, fn supervisor ->
          supervisor_matches_search?(supervisor, normalized_search)
        end)

      updated_node_data =
        node_data
        |> Map.put(:supervisors, filtered_supervisors)
        |> Map.put(:total_supervisors, length(filtered_supervisors))

      {node, updated_node_data}
    end)
    |> Enum.filter(fn {_node, node_data} ->
      length(node_data.supervisors) > 0
    end)
    |> Enum.into(%{})
  end

  defp supervisor_matches_search?(supervisor, search_term) do
    searchable_fields = [
      to_string(supervisor.name),
      format_pid(supervisor.pid)
    ]

    Enum.any?(searchable_fields, fn field ->
      field
      |> String.downcase()
      |> String.contains?(search_term)
    end)
  end

  defp status_bar_metrics(assigns) do
    [
      %{label: "Total Supervisors", value: format_number(assigns.total_supervisors)},
      %{label: "Nodes", value: format_number(length(assigns.cluster_nodes))},
      %{label: "Filtered", value: filtered_count_display(assigns)},
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

  defp filtered_count_display(assigns) do
    filtered_count =
      assigns.filtered_supervisors_by_node
      |> Enum.reduce(0, fn {_node, node_data}, acc ->
        acc + node_data.total_supervisors
      end)

    if has_active_filters?(assigns.filters) do
      "#{filtered_count}"
    else
      "#{assigns.total_supervisors}"
    end
  end

  defp get_status_indicator(assigns) do
    cond do
      assigns.loading -> "⟳ Loading..."
      assigns.refreshing -> "⟳ Refreshing"
      assigns.operation_in_progress -> "⟳ Processing"
      assigns.error_message -> "✗ Error"
      true -> "✓ Ready"
    end
  end

  # Helper functions

  defp node_expanded?(assigns, node) do
    MapSet.member?(assigns.expanded_nodes, node)
  end

  defp has_active_filters?(filters) do
    filters.node != :all or filters.application != :all
  end

  defp count_filtered_supervisors(supervisors_by_node) do
    supervisors_by_node
    |> Enum.reduce(0, fn {_node, node_data}, acc ->
      acc + Map.get(node_data, :total_supervisors, 0)
    end)
  end

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

  defp get_unique_nodes(supervisors_by_node) do
    supervisors_by_node
    |> Map.keys()
    |> Enum.sort()
  end

  defp get_unique_applications(supervisors) do
    supervisors
    |> Enum.map(& &1.application)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp format_pid(pid) when is_pid(pid) do
    pid |> :erlang.pid_to_list() |> to_string()
  end

  defp format_pid(pid) when is_binary(pid), do: pid
  defp format_pid(_), do: "unknown"

  defp format_strategy(strategy) when is_atom(strategy) do
    case strategy do
      :one_for_one -> "One for One"
      :one_for_all -> "One for All"
      :rest_for_one -> "Rest for One"
      :simple_one_for_one -> "Simple One for One"
      _ -> Atom.to_string(strategy)
    end
  end

  defp format_strategy(_), do: "Unknown"

  defp get_status_color_class(true), do: "text-green-400"
  defp get_status_color_class(false), do: "text-red-400"
  defp get_status_color_class(_), do: "text-gray-400"

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

  # Data fetching functions

  defp get_cluster_supervisors do
    params = %{
      "include_children" => false,
      "include_process_details" => false
    }

    case ClusterSupervisionTrees.execute(params) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, format_error(reason)}
    end
  end

  defp flatten_supervisors(supervision_trees) do
    supervision_trees
    |> Enum.flat_map(fn {_node, node_data} ->
      node_data.supervisors
    end)
  end

  defp format_error(reason) when is_binary(reason), do: reason

  def format_bytes(bytes) when is_integer(bytes) and bytes >= 0 do
    cond do
      bytes >= 1_073_741_824 ->
        "#{Float.round(bytes / 1_073_741_824, 1)} GB"

      bytes >= 1_048_576 ->
        "#{Float.round(bytes / 1_048_576, 1)} MB"

      bytes >= 1024 ->
        "#{Float.round(bytes / 1024, 1)} KB"

      true ->
        "#{bytes} B"
    end
  end

  def format_bytes(bytes) when is_integer(bytes) and bytes < 0 do
    "#{bytes} B"
  end

  def format_bytes(_), do: "N/A"

  def format_key(key) when is_atom(key) do
    key
    |> Atom.to_string()
    |> String.split("_")
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end

  def format_value({module, function, arity})
      when is_atom(module) and is_atom(function) and is_integer(arity) do
    "#{module}.#{function}/#{arity}"
  end

  def format_value(value) when is_atom(value) do
    ":#{value}"
  end

  def format_value(value) when is_binary(value) do
    "\"#{value}\""
  end

  def format_value(value) when is_integer(value) do
    "#{value}"
  end

  def format_value(value) when is_list(value) do
    inspect(value)
  end

  def format_value(value) when is_map(value) do
    inspect(value)
  end

  def format_value(value) do
    inspect(value)
  end
end
