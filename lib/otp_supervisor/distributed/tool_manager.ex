defmodule OTPSupervisor.Distributed.ToolManager do
  @moduledoc """
  Central coordinator for all distributed development tools.

  Manages the switching between single-node and multi-node modes,
  coordinates distributed tooling components, and provides a unified
  interface for distributed debugging capabilities.
  """

  use GenServer
  require Logger

  @doc """
  Starts the Distributed Tool Manager.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Set the operation mode for distributed tooling.
  """
  def set_mode(mode) when mode in [:single_node, :multi_node] do
    GenServer.call(__MODULE__, {:set_mode, mode})
  end

  @doc """
  Get the current operation mode.
  """
  def get_mode do
    GenServer.call(__MODULE__, :get_mode)
  end

  @doc """
  Get comprehensive cluster status including mode and topology.
  """
  def get_cluster_status do
    GenServer.call(__MODULE__, :get_cluster_status)
  end

  @doc """
  Register a distributed tool component.
  """
  def register_tool(tool_module, opts \\ []) do
    GenServer.call(__MODULE__, {:register_tool, tool_module, opts})
  end

  @doc """
  Unregister a distributed tool component.
  """
  def unregister_tool(tool_module) do
    GenServer.call(__MODULE__, {:unregister_tool, tool_module})
  end

  @doc """
  List all active distributed tools.
  """
  def list_active_tools do
    GenServer.call(__MODULE__, :list_active_tools)
  end

  @doc """
  Handle simulation events from the SingleNodeSimulator.
  """
  def handle_simulation_event(event, data) do
    GenServer.cast(__MODULE__, {:simulation_event, event, data})
  end

  # GenServer callbacks

  @impl true
  def init(_opts) do
    # Determine initial mode based on cluster state
    initial_mode = determine_initial_mode()

    Logger.info("Starting Distributed Tool Manager in #{initial_mode} mode")

    # Subscribe to cluster events if LibCluster is available
    if Code.ensure_loaded?(Cluster.Events) do
      apply(Cluster.Events, :subscribe, [])
    end

    state = %{
      mode: initial_mode,
      registered_tools: %{},
      cluster_info: get_initial_cluster_info()
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:set_mode, new_mode}, _from, state) do
    Logger.info("Switching distributed tooling mode from #{state.mode} to #{new_mode}")

    # Notify all registered tools about mode change
    notify_tools_mode_change(state.registered_tools, state.mode, new_mode)

    new_state = %{state | mode: new_mode}
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call(:get_mode, _from, state) do
    {:reply, state.mode, state}
  end

  @impl true
  def handle_call(:get_cluster_status, _from, state) do
    cluster_status = %{
      mode: state.mode,
      nodes: get_cluster_nodes(),
      connected_nodes: Node.list(),
      current_node: Node.self(),
      tools: Map.keys(state.registered_tools),
      cluster_info: state.cluster_info
    }

    {:reply, cluster_status, state}
  end

  @impl true
  def handle_call({:register_tool, tool_module, opts}, _from, state) do
    Logger.debug("Registering distributed tool: #{tool_module}")

    tool_info = %{
      module: tool_module,
      opts: opts,
      registered_at: DateTime.utc_now(),
      status: :active
    }

    new_tools = Map.put(state.registered_tools, tool_module, tool_info)
    new_state = %{state | registered_tools: new_tools}

    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:unregister_tool, tool_module}, _from, state) do
    Logger.debug("Unregistering distributed tool: #{tool_module}")

    new_tools = Map.delete(state.registered_tools, tool_module)
    new_state = %{state | registered_tools: new_tools}

    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call(:list_active_tools, _from, state) do
    tools =
      state.registered_tools
      |> Enum.map(fn {module, info} ->
        Map.put(info, :module, module)
      end)

    {:reply, tools, state}
  end

  @impl true
  def handle_cast({:simulation_event, event, data}, state) do
    Logger.debug("Received simulation event: #{event} with data: #{inspect(data)}")

    # Update cluster info based on simulation events
    new_cluster_info = update_cluster_info_from_simulation(state.cluster_info, event, data)
    new_state = %{state | cluster_info: new_cluster_info}

    {:noreply, new_state}
  end

  @impl true
  def handle_info({:cluster_event, event}, state) do
    Logger.debug("Received cluster event: #{inspect(event)}")

    # Update cluster info and potentially adjust mode
    new_cluster_info = update_cluster_info(state.cluster_info, event)
    new_mode = maybe_adjust_mode(state.mode, event)

    new_state = %{state | cluster_info: new_cluster_info, mode: new_mode}

    # Notify tools if mode changed
    if new_mode != state.mode do
      notify_tools_mode_change(state.registered_tools, state.mode, new_mode)
    end

    {:noreply, new_state}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.debug("Unexpected message in ToolManager: #{inspect(msg)}")
    {:noreply, state}
  end

  # Private helper functions

  defp determine_initial_mode do
    case Node.list() do
      [] ->
        # No connected nodes, check if we're configured for clustering
        if cluster_configured?() do
          # Start in single-node mode for development
          :single_node
        else
          :single_node
        end

      _nodes ->
        # We have connected nodes
        :multi_node
    end
  end

  defp cluster_configured? do
    # Check if LibCluster is configured
    case Application.get_env(:libcluster, :topologies) do
      nil -> false
      [] -> false
      _topologies -> true
    end
  end

  defp get_initial_cluster_info do
    %{
      topology: get_cluster_nodes(),
      health: :unknown,
      last_updated: DateTime.utc_now()
    }
  end

  defp get_cluster_nodes do
    case Node.list() do
      # Single node
      [] -> [Node.self()]
      nodes -> [Node.self() | nodes]
    end
  end

  defp notify_tools_mode_change(tools, old_mode, new_mode) do
    Enum.each(tools, fn {tool_module, _info} ->
      if function_exported?(tool_module, :handle_mode_change, 2) do
        try do
          tool_module.handle_mode_change(old_mode, new_mode)
        rescue
          error ->
            Logger.warning("Tool #{tool_module} failed to handle mode change: #{inspect(error)}")
        end
      end
    end)
  end

  defp update_cluster_info(cluster_info, _event) do
    # Update cluster information based on events
    %{cluster_info | topology: get_cluster_nodes(), last_updated: DateTime.utc_now()}
  end

  defp update_cluster_info_from_simulation(cluster_info, event, data) do
    # Update cluster information based on simulation events
    case event do
      :simulation_enabled ->
        %{cluster_info | topology: data, health: :simulated, last_updated: DateTime.utc_now()}

      :simulation_disabled ->
        %{
          cluster_info
          | topology: [Node.self()],
            health: :normal,
            last_updated: DateTime.utc_now()
        }

      :node_failed ->
        %{cluster_info | health: :degraded, last_updated: DateTime.utc_now()}

      :network_partition ->
        %{cluster_info | health: :partitioned, last_updated: DateTime.utc_now()}

      _ ->
        %{cluster_info | last_updated: DateTime.utc_now()}
    end
  end

  defp maybe_adjust_mode(current_mode, event) do
    # Logic to automatically adjust mode based on cluster events
    case {current_mode, event} do
      {:single_node, {:node_up, _node}} ->
        Logger.info("Node joined cluster, considering switch to multi-node mode")
        :multi_node

      {:multi_node, {:node_down, _node}} ->
        case Node.list() do
          [] ->
            Logger.info("All nodes disconnected, switching to single-node mode")
            :single_node

          _ ->
            # Stay in multi-node mode
            current_mode
        end

      _ ->
        current_mode
    end
  end
end
