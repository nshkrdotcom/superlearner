defmodule OTPSupervisor.Distributed.SingleNodeSimulator do
  @moduledoc """
  Simulates distributed behavior on a single node for development.

  This module provides fake multi-node cluster behavior locally, allowing
  developers to test distributed features without the complexity of managing
  multiple actual nodes during development.
  """

  use GenServer
  require Logger

  @doc """
  Starts the Single Node Simulator.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Enable simulation with a specified number of fake nodes.
  """
  def enable_simulation(node_count \\ 3) do
    GenServer.call(__MODULE__, {:enable_simulation, node_count})
  end

  @doc """
  Disable simulation and return to normal single-node behavior.
  """
  def disable_simulation do
    GenServer.call(__MODULE__, :disable_simulation)
  end

  @doc """
  Get the list of simulated nodes.
  """
  def get_simulated_nodes do
    GenServer.call(__MODULE__, :get_simulated_nodes)
  end

  @doc """
  Simulate a node failure for testing.
  """
  def simulate_node_failure(node) do
    GenServer.call(__MODULE__, {:simulate_node_failure, node})
  end

  @doc """
  Simulate a network partition for testing.
  """
  def simulate_network_partition(nodes) do
    GenServer.call(__MODULE__, {:simulate_network_partition, nodes})
  end

  @doc """
  Check if simulation is currently enabled.
  """
  def simulation_enabled? do
    GenServer.call(__MODULE__, :simulation_enabled?)
  end

  @doc """
  Get simulated cluster topology.
  """
  def get_simulated_topology do
    GenServer.call(__MODULE__, :get_simulated_topology)
  end

  @doc """
  Simulate cross-node process execution.
  """
  def simulate_cross_node_call(target_node, module, function, args) do
    GenServer.call(__MODULE__, {:simulate_cross_node_call, target_node, module, function, args})
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    Logger.info("Starting Single Node Simulator")

    state = %{
      simulation_enabled: false,
      simulated_nodes: [],
      failed_nodes: MapSet.new(),
      partitioned_nodes: MapSet.new(),
      # Track which "node" processes belong to
      node_processes: %{},
      # Simulate network latency
      cross_node_latency: opts[:latency] || 10
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:enable_simulation, node_count}, _from, state) do
    Logger.info("Enabling single-node simulation with #{node_count} fake nodes")

    # Generate fake node names
    simulated_nodes = generate_fake_nodes(node_count)

    # Initialize process distribution
    node_processes = initialize_node_processes(simulated_nodes)

    new_state = %{
      state
      | simulation_enabled: true,
        simulated_nodes: simulated_nodes,
        failed_nodes: MapSet.new(),
        partitioned_nodes: MapSet.new(),
        node_processes: node_processes
    }

    # Notify the Tool Manager about the simulation
    notify_tool_manager(:simulation_enabled, simulated_nodes)

    {:reply, {:ok, simulated_nodes}, new_state}
  end

  @impl true
  def handle_call(:disable_simulation, _from, state) do
    Logger.info("Disabling single-node simulation")

    new_state = %{
      state
      | simulation_enabled: false,
        simulated_nodes: [],
        failed_nodes: MapSet.new(),
        partitioned_nodes: MapSet.new(),
        node_processes: %{}
    }

    # Notify the Tool Manager
    notify_tool_manager(:simulation_disabled, [])

    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call(:get_simulated_nodes, _from, state) do
    nodes =
      if state.simulation_enabled do
        state.simulated_nodes
      else
        [Node.self()]
      end

    {:reply, nodes, state}
  end

  @impl true
  def handle_call({:simulate_node_failure, node}, _from, state) do
    if node in state.simulated_nodes do
      Logger.info("Simulating failure of node: #{node}")

      new_failed_nodes = MapSet.put(state.failed_nodes, node)
      new_state = %{state | failed_nodes: new_failed_nodes}

      # Notify about node failure
      notify_tool_manager(:node_failed, node)

      {:reply, :ok, new_state}
    else
      {:reply, {:error, :node_not_found}, state}
    end
  end

  @impl true
  def handle_call({:simulate_network_partition, nodes}, _from, state) do
    Logger.info("Simulating network partition for nodes: #{inspect(nodes)}")

    valid_nodes = Enum.filter(nodes, &(&1 in state.simulated_nodes))
    new_partitioned_nodes = MapSet.union(state.partitioned_nodes, MapSet.new(valid_nodes))

    new_state = %{state | partitioned_nodes: new_partitioned_nodes}

    # Notify about partition
    notify_tool_manager(:network_partition, valid_nodes)

    {:reply, {:ok, valid_nodes}, new_state}
  end

  @impl true
  def handle_call(:simulation_enabled?, _from, state) do
    {:reply, state.simulation_enabled, state}
  end

  @impl true
  def handle_call(:get_simulated_topology, _from, state) do
    topology = build_simulated_topology(state)
    {:reply, topology, state}
  end

  @impl true
  def handle_call({:simulate_cross_node_call, target_node, module, function, args}, _from, state) do
    if state.simulation_enabled do
      result = simulate_remote_call(target_node, module, function, args, state)
      {:reply, result, state}
    else
      # Not in simulation mode, execute normally
      try do
        result = apply(module, function, args)
        {:reply, {:ok, result}, state}
      rescue
        error -> {:reply, {:error, error}, state}
      end
    end
  end

  @impl true
  def handle_info(msg, state) do
    Logger.debug("Unexpected message in SingleNodeSimulator: #{inspect(msg)}")
    {:noreply, state}
  end

  # Private helper functions

  defp generate_fake_nodes(count) do
    base_name = Node.self() |> Atom.to_string() |> String.split("@") |> List.first()
    host = Node.self() |> Atom.to_string() |> String.split("@") |> List.last()

    1..count
    |> Enum.map(fn i ->
      :"#{base_name}_sim#{i}@#{host}"
    end)
  end

  defp initialize_node_processes(simulated_nodes) do
    # Distribute current processes across simulated nodes
    current_processes = Process.list()

    simulated_nodes
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {node, index}, acc ->
      # Assign processes to nodes in round-robin fashion
      node_processes =
        current_processes
        |> Enum.with_index()
        |> Enum.filter(fn {_pid, i} -> rem(i, length(simulated_nodes)) == index end)
        |> Enum.map(fn {pid, _i} -> pid end)

      Map.put(acc, node, node_processes)
    end)
  end

  defp build_simulated_topology(state) do
    if state.simulation_enabled do
      %{
        nodes: state.simulated_nodes,
        failed_nodes: MapSet.to_list(state.failed_nodes),
        partitioned_nodes: MapSet.to_list(state.partitioned_nodes),
        healthy_nodes:
          state.simulated_nodes --
            (MapSet.to_list(state.failed_nodes) ++ MapSet.to_list(state.partitioned_nodes)),
        process_distribution: state.node_processes,
        simulation_mode: true
      }
    else
      %{
        nodes: [Node.self()],
        failed_nodes: [],
        partitioned_nodes: [],
        healthy_nodes: [Node.self()],
        process_distribution: %{Node.self() => Process.list()},
        simulation_mode: false
      }
    end
  end

  defp simulate_remote_call(target_node, module, function, args, state) do
    cond do
      target_node in state.failed_nodes ->
        {:error, {:nodedown, target_node}}

      target_node in state.partitioned_nodes ->
        {:error, {:network_partition, target_node}}

      target_node not in state.simulated_nodes ->
        {:error, {:node_not_found, target_node}}

      true ->
        # Simulate network latency
        if state.cross_node_latency > 0 do
          Process.sleep(state.cross_node_latency)
        end

        # Execute the function locally (since we're simulating)
        try do
          result = apply(module, function, args)
          {:ok, result}
        rescue
          error -> {:error, error}
        catch
          :exit, reason -> {:error, {:exit, reason}}
        end
    end
  end

  defp notify_tool_manager(event, data) do
    try do
      OTPSupervisor.Distributed.ToolManager.handle_simulation_event(event, data)
    rescue
      # Tool manager might not be available yet
      _ -> :ok
    end
  end

  # Public API for intercepting distributed calls

  @doc """
  Intercept Node.list/0 calls when simulation is enabled.
  """
  def intercept_node_list do
    if simulation_enabled?() do
      topology = get_simulated_topology()
      topology.healthy_nodes -- [Node.self()]
    else
      Node.list()
    end
  end

  @doc """
  Intercept :rpc.call/4 when simulation is enabled.
  """
  def intercept_rpc_call(node, module, function, args) do
    if simulation_enabled?() do
      simulate_cross_node_call(node, module, function, args)
    else
      :rpc.call(node, module, function, args)
    end
  end

  @doc """
  Get the node where a process "lives" in simulation.
  """
  def get_process_node(pid) do
    if simulation_enabled?() do
      topology = get_simulated_topology()

      Enum.find_value(topology.process_distribution, Node.self(), fn {node, processes} ->
        if pid in processes, do: node
      end)
    else
      Node.self()
    end
  end
end
