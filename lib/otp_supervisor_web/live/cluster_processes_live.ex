defmodule OtpSupervisorWeb.Live.ClusterProcessesLive do
  use Phoenix.LiveView

  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList

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

    {:ok, socket}
  end

  def handle_params(params, _url, socket) do
    {:noreply, handle_url_params(socket, params)}
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
        <div class="h-full bg-gray-800 rounded border border-green-500/30 p-4">
          <div class="text-center text-green-400/70 font-mono">
            Cluster Processes page content will be implemented in subsequent tasks
          </div>
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
      %{label: "Last Updated", value: last_updated_display(assigns.last_updated)}
    ]
  end

  defp filtered_count_display(assigns) do
    if assigns.search_term != "" or
         assigns.filters != %{node: :all, type: :all, application: :all} do
      # This will be calculated when filtering is implemented
      "0"
    else
      "#{assigns.total_processes}"
    end
  end

  defp last_updated_display(nil), do: "Never"

  defp last_updated_display(datetime) do
    datetime
    |> DateTime.to_time()
    |> Time.to_string()
    |> String.slice(0, 8)
  end

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
end
