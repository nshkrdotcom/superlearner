defmodule OtpSupervisorWeb.Live.ClusterVisualizationLive do
  use Phoenix.LiveView
  import Phoenix.Component
  import Phoenix.HTML

  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterSupervisionTrees
  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList

  @moduledoc """
  Cluster visualization interface with interactive 3D display.
  """

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:page_title, "Cluster Visualization")
      |> assign(:current_page, "cluster-visualization")
      |> assign_initial_state()
      |> load_cluster_data()

    if connected?(socket) do
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

    Process.send_after(self(), :refresh_data, 5_000)
    {:noreply, socket}
  end

  def handle_info({:debounced_search, search_term}, socket) do
    socket =
      socket
      |> assign(:search_term, search_term)
      |> assign(:search_debounce_timer, nil)
      |> apply_filters_and_search()
      |> update_url_params()

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
            
            <!-- Three.js 3D Visualization -->
            <div id="cluster-3d-viz" class="mb-4 bg-gray-900/70 rounded border border-green-500/20 relative" style="height: 500px;" phx-update="ignore">
              <!-- Details Panel -->
              <div id="node-details-panel" class="absolute top-4 left-4 w-80 bg-gray-800/95 border border-green-500/30 rounded p-4 font-mono text-sm text-green-400 hidden z-10">
                <div class="flex justify-between items-center mb-3">
                  <h3 id="details-title" class="text-green-300 font-bold text-base">Node Details</h3>
                  <button id="close-details" class="text-green-400 hover:text-green-300 text-lg leading-none">&times;</button>
                </div>
                <div id="details-content" class="space-y-2">
                  <!-- Content will be populated by JavaScript -->
                </div>
              </div>
              
              <script src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"></script>
            </div>
            
            <!-- Hidden data element for Three.js -->
            <div id="cluster-data-json" style="display: none;">
              <%= raw(Jason.encode!(@filtered_cluster_data)) %>
            </div>
            
            <!-- Filter Form -->
            <form phx-change="filter_change" class="mb-4">
              <div class="flex flex-wrap items-center gap-4">
                <div class="flex items-center space-x-2">
                  <label class="text-green-400/70 font-mono text-sm">Node:</label>
                  <select
                    class="bg-gray-800 border border-green-500/30 text-green-400 font-mono text-sm rounded px-2 py-1 focus:border-green-500 focus:ring-1 focus:ring-green-500/50 focus:outline-none disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200"
                    name="node"
                    disabled={@loading}
                  >
                    <option value="all" selected={@filters.node == :all}>All Nodes</option>
                    <%= for node <- get_unique_nodes_from_cluster_data(@cluster_data) do %>
                      <option value={node} selected={@filters.node == node}><%= node %></option>
                    <% end %>
                  </select>
                </div>
              </div>
            </form>
            
            <!-- Refresh Controls -->
            <div class="flex items-center justify-between pt-3 border-t border-green-500/20">
              <div class="flex items-center space-x-4">
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
                    <div class="text-sm mb-4"><%= @error_message %></div>
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
                <div class="w-full h-full p-4">
                  <div class="text-center text-green-400/70 font-mono py-8">
                    <div class="text-lg mb-2">🎯 3D Cluster Visualization</div>
                    <div class="text-sm">Interactive Three.js visualization above</div>
                  </div>
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

  def handle_event("search_change", %{"search" => search_term}, socket) do
    if socket.assigns.search_debounce_timer do
      Process.cancel_timer(socket.assigns.search_debounce_timer)
    end

    timer_ref = Process.send_after(self(), {:debounced_search, search_term}, 300)

    socket =
      socket
      |> assign(:search_term, search_term)
      |> assign(:search_debounce_timer, timer_ref)

    {:noreply, socket}
  end

  def handle_event("clear_search", _params, socket) do
    socket =
      socket
      |> assign(:search_term, "")
      |> assign(:search_debounce_timer, nil)
      |> apply_filters_and_search()
      |> update_url_params()

    {:noreply, socket}
  end

  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  # Private functions - delegate to separate modules
  defp assign_initial_state(socket) do
    OtpSupervisorWeb.Live.ClusterVisualization.State.assign_initial_state(socket)
  end

  defp status_bar_metrics(assigns) do
    OtpSupervisorWeb.Live.ClusterVisualization.Metrics.status_bar_metrics(assigns)
  end

  defp load_cluster_data(socket) do
    OtpSupervisorWeb.Live.ClusterVisualization.DataLoader.load_cluster_data(socket)
  end

  defp apply_filters_and_search(socket) do
    OtpSupervisorWeb.Live.ClusterVisualization.Filters.apply_filters_and_search(socket)
  end

  defp get_unique_nodes_from_cluster_data(cluster_data) do
    OtpSupervisorWeb.Live.ClusterVisualization.Filters.get_unique_nodes_from_cluster_data(cluster_data)
  end

  defp parse_filter_value(value) do
    OtpSupervisorWeb.Live.ClusterVisualization.Filters.parse_filter_value(value)
  end

  defp update_url_params(socket), do: socket
  defp handle_url_params(socket, _params), do: socket
end