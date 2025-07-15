defmodule OTPSupervisor.Distributed.ClusterStateManager do
  @moduledoc """
  Real-time cluster topology and state management.

  This module tracks cluster membership changes, maintains process distribution
  mapping, provides cluster topology visualization data, and handles network
  partition detection and recovery.
  """

  use GenServer
  require Logger

  # PubSub topic for cluster state changes
  @cluster_topic "cluster_state_changes"

  @doc """
  Starts the Cluster State Manager.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Get the current cluster topology.
  """
  def get_cluster_topology do
    GenServer.call(__MODULE__, :get_cluster_topology)
  end

  @doc """
  Get detailed information about a specific node.
  """
  def get_node_info(node) do
    GenServer.call(__MODULE__, {:get_node_info, node})
  end

  @doc """
  Get the current process distribution across the cluster.
  """
  def get_process_distribution do
    GenServer.call(__MODULE__, :get_process_distribution)
  end

  @doc """
  Subscribe to cluster state changes via PubSub.
  """
  def subscribe_to_changes do
    Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, @cluster_topic)
  end

  @doc """
  Get the current network partition status.
  """
  def get_partition_status do
    GenServer.call(__MODULE__, :get_partition_status)
  end

  @doc """
  Force a cluster state refresh.
  """
  def refresh_cluster_state do
    GenServer.cast(__MODULE__, :refresh_cluster_state)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    Logger.info("Starting Cluster State Manager")

    # Subscribe to cluster events if LibCluster is available
    if Code.ensure_loaded?(Cluster.Events) do
      apply(Cluster.Events, :subscribe, [])
    end

    # Subscribe to node monitoring
    :net_kernel.monitor_nodes(true, node_type: :visible)

    # Set up periodic state refresh
    refresh_interval = opts[:refresh_interval] || 5_000
    Process.send_after(self(), :periodic_refresh, refresh_interval)

    state = %{
      topology: build_initial_topology(),
      node_info: %{},
      process_distribution: %{},
      partition_status: :healthy,
      last_updated: DateTime.utc_now(),
      refresh_interval: refresh_interval,
      subscribers: MapSet.new()
    }

    # Initial state collection
    {:ok, collect_cluster_state(state)}
  end

  @impl true
  def handle_call(:get_cluster_topology, _from, state) do
    # Check if we're in simulation mode and need to return simulated topology
    simulation_enabled =
      try do
        OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
      rescue
        _ -> false
      end

    topology =
      if simulation_enabled do
        build_simulated_topology()
      else
        state.topology
      end

    {:reply, topology, state}
  end

  @impl true
  def handle_call({:get_node_info, node}, _from, state) do
    # Check if we're in simulation mode and this is a simulated node
    simulation_enabled =
      try do
        OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
      rescue
        _ -> false
      end

    node_info =
      if simulation_enabled do
        get_simulated_node_info(node, state)
      else
        Map.get(state.node_info, node, {:error, :node_not_found})
      end

    {:reply, node_info, state}
  end

  @impl true
  def handle_call(:get_process_distribution, _from, state) do
    # Check if we're in simulation mode and need to return simulated process distribution
    simulation_enabled =
      try do
        OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
      rescue
        _ -> false
      end

    process_distribution =
      if simulation_enabled do
        get_simulated_process_distribution()
      else
        state.process_distribution
      end

    {:reply, process_distribution, state}
  end

  @impl true
  def handle_call(:get_partition_status, _from, state) do
    {:reply, state.partition_status, state}
  end

  # Private function to get simulated process distribution
  defp get_simulated_process_distribution do
    try do
      topology = OTPSupervisor.Distributed.SingleNodeSimulator.get_simulated_topology()
      # Include both simulated nodes and the real node
      Map.put(topology.process_distribution, Node.self(), Process.list())
    rescue
      _ -> %{Node.self() => Process.list()}
    end
  end

  @impl true
  def handle_cast(:refresh_cluster_state, state) do
    Logger.debug("Refreshing cluster state")
    new_state = collect_cluster_state(state)
    broadcast_state_change(new_state, state)
    {:noreply, new_state}
  end

  @impl true
  def handle_info({:nodeup, node, _info}, state) do
    Logger.info("Node joined cluster: #{node}")

    new_topology = add_node_to_topology(state.topology, node)
    new_state = %{state | topology: new_topology, last_updated: DateTime.utc_now()}

    # Collect fresh state and broadcast changes
    updated_state = collect_cluster_state(new_state)
    broadcast_state_change(updated_state, state)

    {:noreply, updated_state}
  end

  @impl true
  def handle_info({:nodedown, node, _info}, state) do
    Logger.info("Node left cluster: #{node}")

    new_topology = remove_node_from_topology(state.topology, node)
    new_partition_status = detect_partition_status(new_topology)

    new_state = %{
      state
      | topology: new_topology,
        partition_status: new_partition_status,
        last_updated: DateTime.utc_now()
    }

    # Clean up node info and process distribution
    cleaned_state = cleanup_node_data(new_state, node)
    broadcast_state_change(cleaned_state, state)

    {:noreply, cleaned_state}
  end

  @impl true
  def handle_info({:cluster_event, event}, state) do
    Logger.debug("Received cluster event: #{inspect(event)}")

    # Handle LibCluster events
    new_state = handle_cluster_event(state, event)
    broadcast_state_change(new_state, state)

    {:noreply, new_state}
  end

  @impl true
  def handle_info(:periodic_refresh, state) do
    # Schedule next refresh
    Process.send_after(self(), :periodic_refresh, state.refresh_interval)

    # Refresh cluster state
    new_state = collect_cluster_state(state)

    # Only broadcast if there are significant changes
    if significant_changes?(new_state, state) do
      broadcast_state_change(new_state, state)
    end

    {:noreply, new_state}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.debug("Unexpected message in ClusterStateManager: #{inspect(msg)}")
    {:noreply, state}
  end

  # Private helper functions

  defp build_initial_topology do
    current_node = Node.self()
    connected_nodes = Node.list()
    all_nodes = [current_node | connected_nodes]

    %{
      nodes: all_nodes,
      current_node: current_node,
      connected_nodes: connected_nodes,
      total_nodes: length(all_nodes),
      cluster_name: get_cluster_name(),
      formation_time: DateTime.utc_now()
    }
  end

  defp collect_cluster_state(state) do
    # Check if we're in simulation mode
    simulation_enabled =
      try do
        OTPSupervisor.Distributed.SingleNodeSimulator.simulation_enabled?()
      rescue
        _ -> false
      end

    if simulation_enabled do
      collect_simulated_state(state)
    else
      collect_real_state(state)
    end
  end

  defp collect_simulated_state(state) do
    try do
      simulated_topology = OTPSupervisor.Distributed.SingleNodeSimulator.get_simulated_topology()

      # Build node info for simulated nodes
      node_info =
        simulated_topology.nodes
        |> Enum.reduce(%{}, fn node, acc ->
          info = %{
            name: node,
            status:
              cond do
                node in simulated_topology.failed_nodes -> :down
                node in simulated_topology.partitioned_nodes -> :partitioned
                true -> :up
              end,
            processes: length(Map.get(simulated_topology.process_distribution, node, [])),
            memory_usage: :simulated,
            cpu_usage: :simulated,
            last_seen: DateTime.utc_now(),
            simulated: true
          }

          Map.put(acc, node, info)
        end)

      %{
        state
        | topology: %{
            state.topology
            | nodes: simulated_topology.nodes,
              connected_nodes: simulated_topology.healthy_nodes -- [Node.self()],
              total_nodes: length(simulated_topology.nodes)
          },
          node_info: node_info,
          process_distribution: simulated_topology.process_distribution,
          partition_status:
            if(length(simulated_topology.partitioned_nodes) > 0, do: :partitioned, else: :healthy),
          last_updated: DateTime.utc_now()
      }
    rescue
      _ -> collect_real_state(state)
    end
  end

  defp collect_real_state(state) do
    # Collect real cluster state
    all_nodes = [Node.self() | Node.list()]

    # Collect node information
    node_info =
      all_nodes
      |> Enum.reduce(%{}, fn node, acc ->
        info = collect_node_info(node)
        Map.put(acc, node, info)
      end)

    # Collect process distribution
    process_distribution = collect_process_distribution(all_nodes)

    %{
      state
      | topology: %{
          state.topology
          | nodes: all_nodes,
            connected_nodes: Node.list(),
            total_nodes: length(all_nodes)
        },
        node_info: node_info,
        process_distribution: process_distribution,
        last_updated: DateTime.utc_now()
    }
  end

  defp collect_node_info(node) do
    if node == Node.self() do
      # Local node - we can get detailed info
      %{
        name: node,
        status: :up,
        processes: length(Process.list()),
        memory_usage: get_memory_usage(),
        cpu_usage: get_cpu_usage(),
        last_seen: DateTime.utc_now(),
        simulated: false
      }
    else
      # Remote node - try to get info via RPC
      try do
        case :rpc.call(node, Process, :list, [], 5000) do
          {:badrpc, reason} ->
            %{
              name: node,
              status: :unreachable,
              error: reason,
              last_seen: DateTime.utc_now(),
              simulated: false
            }

          processes when is_list(processes) ->
            %{
              name: node,
              status: :up,
              processes: length(processes),
              memory_usage: get_remote_memory_usage(node),
              cpu_usage: get_remote_cpu_usage(node),
              last_seen: DateTime.utc_now(),
              simulated: false
            }
        end
      rescue
        _ ->
          %{
            name: node,
            status: :error,
            last_seen: DateTime.utc_now(),
            simulated: false
          }
      end
    end
  end

  defp collect_process_distribution(nodes) do
    nodes
    |> Enum.reduce(%{}, fn node, acc ->
      processes =
        if node == Node.self() do
          Process.list()
        else
          case :rpc.call(node, Process, :list, [], 5000) do
            {:badrpc, _} -> []
            processes when is_list(processes) -> processes
            _ -> []
          end
        end

      Map.put(acc, node, processes)
    end)
  end

  defp add_node_to_topology(topology, node) do
    new_nodes = [node | topology.nodes] |> Enum.uniq()
    new_connected = [node | topology.connected_nodes] |> Enum.uniq()

    %{topology | nodes: new_nodes, connected_nodes: new_connected, total_nodes: length(new_nodes)}
  end

  defp remove_node_from_topology(topology, node) do
    new_nodes = topology.nodes -- [node]
    new_connected = topology.connected_nodes -- [node]

    %{topology | nodes: new_nodes, connected_nodes: new_connected, total_nodes: length(new_nodes)}
  end

  defp cleanup_node_data(state, node) do
    new_node_info = Map.delete(state.node_info, node)
    new_process_distribution = Map.delete(state.process_distribution, node)

    %{state | node_info: new_node_info, process_distribution: new_process_distribution}
  end

  defp detect_partition_status(topology) do
    total_expected = get_expected_cluster_size()
    current_size = topology.total_nodes

    cond do
      current_size < total_expected / 2 -> :minority_partition
      current_size < total_expected -> :partial_partition
      true -> :healthy
    end
  end

  defp handle_cluster_event(state, event) do
    # Handle LibCluster specific events
    case event do
      {:node_up, node} ->
        new_topology = add_node_to_topology(state.topology, node)
        %{state | topology: new_topology, last_updated: DateTime.utc_now()}

      {:node_down, node} ->
        new_topology = remove_node_from_topology(state.topology, node)
        %{state | topology: new_topology, last_updated: DateTime.utc_now()}

      _ ->
        state
    end
  end

  defp broadcast_state_change(new_state, old_state) do
    change_event = %{
      type: :cluster_state_change,
      timestamp: DateTime.utc_now(),
      topology: new_state.topology,
      changes: detect_changes(new_state, old_state)
    }

    Phoenix.PubSub.broadcast(OtpSupervisor.PubSub, @cluster_topic, change_event)
  end

  defp significant_changes?(new_state, old_state) do
    new_state.topology.total_nodes != old_state.topology.total_nodes or
      new_state.partition_status != old_state.partition_status or
      map_size(new_state.node_info) != map_size(old_state.node_info)
  end

  defp detect_changes(new_state, old_state) do
    %{
      nodes_added: new_state.topology.nodes -- old_state.topology.nodes,
      nodes_removed: old_state.topology.nodes -- new_state.topology.nodes,
      partition_status_changed: new_state.partition_status != old_state.partition_status
    }
  end

  # Helper functions for system metrics

  defp get_memory_usage do
    try do
      memory = :erlang.memory()
      total = Keyword.get(memory, :total, 0)
      processes = Keyword.get(memory, :processes, 0)
      %{total: total, processes: processes}
    rescue
      _ -> %{total: 0, processes: 0}
    end
  end

  defp get_cpu_usage do
    # Check if cpu_sup application is available and started
    case Application.ensure_all_started(:os_mon) do
      {:ok, _} ->
        try do
          case apply(:cpu_sup, :util, []) do
            {:error, _} -> 0.0
            usage when is_number(usage) -> usage
            _ -> 0.0
          end
        rescue
          _ -> 0.0
        end

      {:error, _} ->
        # Fallback: return 0.0 if no CPU monitoring is available
        0.0
    end
  end

  defp get_remote_memory_usage(node) do
    case :rpc.call(node, :erlang, :memory, [], 5000) do
      {:badrpc, _} ->
        %{total: 0, processes: 0}

      memory when is_list(memory) ->
        total = Keyword.get(memory, :total, 0)
        processes = Keyword.get(memory, :processes, 0)
        %{total: total, processes: processes}

      _ ->
        %{total: 0, processes: 0}
    end
  end

  defp get_remote_cpu_usage(node) do
    case :rpc.call(node, :cpu_sup, :util, [], 5000) do
      {:badrpc, _} -> 0.0
      {:error, _} -> 0.0
      usage when is_number(usage) -> usage
      _ -> 0.0
    end
  end

  defp get_cluster_name do
    case Application.get_env(:libcluster, :topologies) do
      topologies when is_list(topologies) and length(topologies) > 0 ->
        {name, _config} = List.first(topologies)
        name

      _ ->
        :default_cluster
    end
  end

  defp get_expected_cluster_size do
    case Application.get_env(:libcluster, :topologies) do
      topologies when is_list(topologies) and length(topologies) > 0 ->
        {_name, config} = List.first(topologies)
        hosts = get_in(config, [:config, :hosts]) || []
        length(hosts)

      _ ->
        1
    end
  end

  defp build_simulated_topology do
    try do
      simulated_topology = OTPSupervisor.Distributed.SingleNodeSimulator.get_simulated_topology()

      # Include the current real node along with simulated nodes
      all_nodes = [Node.self() | simulated_topology.nodes]
      connected_nodes = simulated_topology.healthy_nodes

      %{
        nodes: all_nodes,
        current_node: Node.self(),
        connected_nodes: connected_nodes,
        total_nodes: length(all_nodes),
        cluster_name: get_cluster_name(),
        formation_time: DateTime.utc_now()
      }
    rescue
      _ ->
        # Fallback to real topology if simulation fails
        build_initial_topology()
    end
  end

  defp get_simulated_node_info(node, state) do
    # If it's the current real node, return real info
    if node == Node.self() do
      Map.get(state.node_info, node, collect_node_info(node))
    else
      # Check if it's a simulated node
      try do
        simulated_topology =
          OTPSupervisor.Distributed.SingleNodeSimulator.get_simulated_topology()

        if node in simulated_topology.nodes do
          # Return simulated node info
          processes = Map.get(simulated_topology.process_distribution, node, [])

          %{
            name: node,
            status:
              cond do
                node in simulated_topology.failed_nodes -> :down
                node in simulated_topology.partitioned_nodes -> :partitioned
                true -> :up
              end,
            processes: length(processes),
            # Simulated values
            memory_usage: %{total: 50_000_000, processes: 25_000_000},
            # Simulated CPU usage
            cpu_usage: 15.5,
            last_seen: DateTime.utc_now(),
            simulated: true
          }
        else
          {:error, :node_not_found}
        end
      rescue
        _ -> {:error, :node_not_found}
      end
    end
  end
end
