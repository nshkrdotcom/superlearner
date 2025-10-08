defmodule OtpSupervisorWeb.Live.ClusterVisualization.State do
  @moduledoc """
  Handles initial state assignment for cluster visualization.
  """

  def assign_initial_state(socket) do
    Phoenix.Component.assign(socket, %{
      cluster_data: %{},
      filtered_cluster_data: %{},
      loading: false,
      auto_refresh: true,
      error_message: nil,
      last_updated: nil,
      total_nodes: 0,
      total_processes: 0,
      filtered_process_count: 0,
      filters: %{node: :all, type: :all, application: :all},
      search_term: "",
      search_debounce_timer: nil,
      expanded_nodes: MapSet.new()
    })
  end
end
