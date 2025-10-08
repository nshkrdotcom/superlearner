defmodule OtpSupervisorWeb.Live.ClusterVisualization.Filters do
  @moduledoc """
  Handles filtering and search functionality for cluster visualization.
  """

  def apply_filters_and_search(socket) do
    filtered_data =
      filter_cluster_data(
        socket.assigns.cluster_data,
        socket.assigns.filters,
        socket.assigns.search_term
      )

    Phoenix.Component.assign(socket, %{
      filtered_cluster_data: filtered_data,
      filtered_process_count: count_filtered_processes(filtered_data)
    })
  end

  def get_unique_nodes_from_cluster_data(cluster_data) do
    Map.keys(cluster_data)
  end

  def parse_filter_value("all"), do: :all
  def parse_filter_value(value), do: value

  defp filter_cluster_data(cluster_data, _filters, _search_term) do
    # For now, return all data - filtering can be implemented later
    cluster_data
  end

  defp count_filtered_processes(filtered_data) do
    filtered_data
    |> Enum.reduce(0, fn {_node, node_data}, acc ->
      acc + count_processes_in_node_data(node_data)
    end)
  end

  defp count_processes_in_node_data(node_data) do
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
end
