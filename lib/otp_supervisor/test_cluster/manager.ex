defmodule OTPSupervisor.TestCluster.Manager do
  @moduledoc """
  Manages the lifecycle of distributed test clusters.

  This GenServer handles:
  - Starting and stopping test node clusters
  - Health monitoring and validation
  - Code synchronization across nodes
  - Environment isolation and cleanup
  - Comprehensive error diagnostics

  Addresses critical issues from LIVE_CLUSTER_TESTING_ANALYSIS.md:
  - Distributed Erlang startup problems (:nodistribution, :not_alive)
  - Network configuration issues (127.0.0.1 vs localhost)
  - Cookie authentication failures
  - EPMD dependency management
  """

  use GenServer
  require Logger

  alias OTPSupervisor.TestCluster.{HealthChecker, HostnameResolver, PortManager, Diagnostics}

  # Dynamic test cluster configuration - no hardcoded nodes
  # Configuration is now generated dynamically using HostnameResolver and PortManager

  @default_timeout 30_000

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Start the test cluster with all configured nodes.
  """
  def start_cluster(opts \\ []) do
    GenServer.call(__MODULE__, {:start_cluster, opts}, @default_timeout)
  end

  @doc """
  Stop the test cluster and clean up all nodes.
  """
  def stop_cluster do
    GenServer.call(__MODULE__, :stop_cluster, @default_timeout)
  end

  @doc """
  Get the current status of the test cluster.
  """
  def get_status do
    GenServer.call(__MODULE__, :get_status)
  end

  @doc """
  Perform comprehensive health check on the cluster.
  """
  def health_check do
    GenServer.call(__MODULE__, :health_check, @default_timeout)
  end

  @doc """
  Clean up all test artifacts and resources.
  """
  def clean_all do
    GenServer.call(__MODULE__, :clean_all)
  end

  @doc """
  Get logs from test nodes.
  """
  def get_logs(node \\ nil) do
    GenServer.call(__MODULE__, {:get_logs, node})
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    Logger.info("Starting TestCluster.Manager")

    state = %{
      nodes: %{},
      status: :stopped,
      last_health_check: nil,
      opts: opts
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:start_cluster, opts}, _from, state) do
    Logger.info("Starting test cluster with WSL fixes...")

    case start_cluster_nodes(opts) do
      {:ok, nodes} ->
        new_state = %{state | nodes: nodes, status: :running}
        Logger.info("Test cluster started successfully with #{map_size(nodes)} nodes")
        {:reply, {:ok, nodes}, new_state}

      {:error, reason} ->
        diagnosis = Diagnostics.diagnose_startup_failure(reason)
        Logger.error("Cluster startup failed: #{diagnosis.problem}")
        Enum.each(diagnosis.solutions, &Logger.info("  • #{&1}"))

        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call(:stop_cluster, _from, state) do
    Logger.info("Stopping test cluster...")

    :ok = stop_cluster_nodes(state.nodes)
    new_state = %{state | nodes: %{}, status: :stopped}
    Logger.info("Test cluster stopped successfully")
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call(:get_status, _from, state) do
    status = build_cluster_status(state)
    {:reply, {:ok, status}, state}
  end

  @impl true
  def handle_call(:health_check, _from, state) do
    Logger.info("Running comprehensive health check...")

    {:ok, results} = HealthChecker.comprehensive_health_check(state.nodes)
    new_state = %{state | last_health_check: DateTime.utc_now()}
    {:reply, {:ok, results}, new_state}
  end

  @impl true
  def handle_call(:clean_all, _from, state) do
    Logger.info("Cleaning up all test artifacts...")

    # Stop cluster if running
    _ = stop_cluster_nodes(state.nodes)

    # Clean up any remaining artifacts
    :ok = clean_test_artifacts()
    new_state = %{state | nodes: %{}, status: :stopped}
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:get_logs, node}, _from, state) do
    case get_node_logs(state.nodes, node) do
      {:ok, logs} ->
        {:reply, {:ok, logs}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  # Private implementation

  defp start_phoenix_server(config) do
    Logger.info("Starting cluster node #{config.name} on #{config.hostname}:#{config.http_port}")

    # Start a REAL Elixir node - a separate OS process running the full application
    env = [
      {"PHX_PORT", Integer.to_string(config.http_port)},
      {"PORT", Integer.to_string(config.http_port)},
      {"MIX_ENV", "dev"},
      {"NODE_NAME", Atom.to_string(config.name)},
      {"PHX_SERVER", "true"}
    ]

    # Use elixir command to start a named node with the full application
    cmd_args = [
      "--name",
      Atom.to_string(config.name),
      "--cookie",
      Atom.to_string(config.cookie),
      "-S",
      "mix",
      "phx.server"
    ]

    # Start the node as a separate OS process using Task.async
    task =
      Task.async(fn ->
        System.cmd("elixir", cmd_args,
          env: env,
          cd: File.cwd!(),
          into: IO.stream(:stdio, :line)
        )
      end)

    # Give the node time to start
    :timer.sleep(8000)

    # Test if we can connect to the node
    case Node.ping(config.name) do
      :pong ->
        server_info = %{
          name: config.name,
          http_port: config.http_port,
          task: task,
          status: :running,
          url: "http://#{config.hostname}:#{config.http_port}",
          hostname: config.hostname
        }

        Logger.info("✅ Cluster node #{config.name} started successfully")
        {:ok, server_info}

      :pang ->
        Logger.error("Failed to connect to cluster node #{config.name}")
        Task.shutdown(task, :brutal_kill)
        {:error, {:node_connection_failed, config.name}}
    end
  end

  defp start_cluster_nodes(opts) do
    # CRITICAL: Address distributed Erlang startup issues
    with :ok <- validate_test_environment(),
         :ok <- ensure_primary_node_alive(),
         {:ok, nodes} <- provision_all_nodes(opts),
         :ok <- wait_for_cluster_formation(nodes),
         :ok <- sync_code_to_cluster(nodes),
         :ok <- validate_cluster_health(nodes) do
      {:ok, nodes}
    else
      {:error, reason} ->
        # Cleanup on failure
        cleanup_failed_nodes()
        {:error, reason}
    end
  end

  defp validate_test_environment do
    Logger.debug("Validating test environment...")

    case Diagnostics.check_prerequisites() do
      :ok ->
        Logger.debug("Environment validation passed")
        :ok

      {:error, failed_checks} ->
        Logger.error("Environment validation failed: #{inspect(failed_checks)}")
        {:error, {:environment_validation_failed, failed_checks}}
    end
  end

  defp ensure_primary_node_alive do
    # For Phoenix servers, we don't need distributed Erlang nodes
    # Just ensure we have a cookie set for any future distributed operations
    if Node.alive?() do
      Logger.info("Using current node as primary: #{Node.self()}")
      Node.set_cookie(:test_cluster_cookie)
    else
      Logger.info("Running in non-distributed mode - Phoenix servers will be standalone")
    end

    :ok
  end

  defp provision_all_nodes(opts) do
    Logger.info("Provisioning test nodes with dynamic configuration...")

    node_count =
      case opts[:node_count] do
        count when is_integer(count) and count > 0 -> count
        # Default to 2 nodes for most tests
        _ -> 2
      end

    # Use new hostname resolution and port management
    with {:ok, hostname} <- HostnameResolver.get_cluster_hostname(),
         {:ok, port_pairs} <- PortManager.find_available_ports(node_count) do
      Logger.info("Using hostname: #{hostname}, port pairs: #{inspect(port_pairs)}")

      # Generate dynamic node configurations
      node_configs = generate_node_configs(hostname, port_pairs)

      # Start actual Phoenix servers with dynamic configuration
      results =
        node_configs
        |> Enum.with_index()
        |> Enum.map(fn {config, index} ->
          node_key = :"node#{index + 1}"

          case start_phoenix_server(config) do
            {:ok, server_info} -> {node_key, {:ok, server_info}}
            {:error, reason} -> {node_key, {:error, reason}}
          end
        end)

      # Check if all servers started successfully
      failed_nodes = Enum.filter(results, fn {_, result} -> match?({:error, _}, result) end)

      if Enum.empty?(failed_nodes) do
        successful_nodes =
          results
          |> Enum.map(fn {name, {:ok, server_info}} -> {name, server_info} end)
          |> Enum.into(%{})

        Logger.info("All #{map_size(successful_nodes)} servers started successfully")
        {:ok, successful_nodes}
      else
        Logger.error("Failed to start servers: #{inspect(failed_nodes)}")
        {:error, {:node_startup_failed, failed_nodes}}
      end
    else
      {:error, reason} ->
        Logger.error("Failed to get hostname or ports: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp generate_node_configs(hostname, port_pairs) do
    port_pairs
    |> Enum.with_index()
    |> Enum.map(fn {{http_port, dist_port}, index} ->
      %{
        name: :"test_node#{index + 1}@#{hostname}",
        http_port: http_port,
        dist_port: dist_port,
        cookie: :test_cluster_cookie,
        hostname: hostname
      }
    end)
  end

  defp wait_for_cluster_formation(nodes, timeout \\ 10_000) do
    Logger.info("Waiting for servers to be ready...")

    # For Phoenix servers, just wait for them to respond to HTTP requests
    start_time = System.monotonic_time(:millisecond)
    wait_for_servers_ready(nodes, start_time, timeout)
  end

  defp wait_for_servers_ready(nodes, start_time, timeout) do
    current_time = System.monotonic_time(:millisecond)

    if current_time - start_time > timeout do
      {:error, :server_ready_timeout}
    else
      if all_servers_ready?(nodes) do
        Logger.info("All servers are ready and responding")
        :ok
      else
        :timer.sleep(1000)
        wait_for_servers_ready(nodes, start_time, timeout)
      end
    end
  end

  defp all_servers_ready?(nodes) do
    Enum.all?(nodes, fn {_name, server_info} ->
      hostname = Map.get(server_info, :hostname, "127.0.0.1")
      url = ~c"http://#{hostname}:#{server_info.http_port}/"

      case :httpc.request(:get, {url, []}, [], []) do
        {:ok, _} -> true
        {:error, _} -> false
      end
    end)
  end

  defp sync_code_to_cluster(_nodes) do
    Logger.info("Code synchronization not needed for Phoenix servers")
    :ok
  end

  defp validate_cluster_health(nodes) do
    Logger.info("Validating server health...")

    # Simple health check - just verify all servers are responding
    all_healthy =
      Enum.all?(nodes, fn {_name, server_info} ->
        hostname = Map.get(server_info, :hostname, "127.0.0.1")
        url = ~c"http://#{hostname}:#{server_info.http_port}/"

        case :httpc.request(:get, {url, []}, [], []) do
          {:ok, _} -> true
          {:error, _} -> false
        end
      end)

    if all_healthy do
      Logger.info("All servers are healthy")
      :ok
    else
      Logger.error("Some servers are not responding")
      {:error, :unhealthy_servers}
    end
  end

  defp stop_cluster_nodes(nodes) do
    Logger.info("Stopping #{map_size(nodes)} cluster nodes...")

    results =
      nodes
      |> Enum.map(fn {name, node} ->
        :ok = stop_node(node)
        {name, :ok}
      end)

    failed_stops = Enum.filter(results, fn {_, result} -> match?({:error, _}, result) end)

    if Enum.empty?(failed_stops) do
      :ok
    else
      Logger.warning("Some nodes failed to stop cleanly: #{inspect(failed_stops)}")
      # Still return :ok as we'll clean up forcefully
      :ok
    end
  end

  defp stop_node(server_info) do
    try do
      # Stop the Task that's running the Elixir node
      if Map.has_key?(server_info, :task) do
        Task.shutdown(server_info.task, :brutal_kill)
      end

      # Give it time to shut down
      :timer.sleep(1000)
      :ok
    rescue
      # Server might already be down
      _ -> :ok
    end
  end

  defp cleanup_failed_nodes do
    # Force cleanup of any partially started nodes
    Logger.warning("Cleaning up failed node startup attempts...")

    # Use PortManager to clean up test processes and ports
    PortManager.cleanup_test_processes()

    :ok
  end

  defp clean_test_artifacts do
    # Clean up any test-specific files, logs, etc.
    Logger.info("Cleaning up test artifacts...")

    # Use PortManager to clean up test processes and ports
    PortManager.cleanup_test_processes()

    # Remove any test-specific temporary files
    # Clear any test databases or ETS tables
    # Clean up log files

    :ok
  end

  defp build_cluster_status(state) do
    %{
      overall: state.status,
      nodes: build_node_statuses(state.nodes),
      last_health_check: state.last_health_check,
      uptime: calculate_uptime(state)
    }
  end

  defp build_node_statuses(nodes) do
    nodes
    |> Enum.map(fn {name, node} ->
      {name, get_node_status(node)}
    end)
    |> Enum.into(%{})
  end

  defp get_node_status(node) do
    try do
      case :rpc.call(node, Node, :self, [], 2000) do
        {:badrpc, reason} ->
          %{healthy: false, status: :unreachable, error: reason}

        ^node ->
          %{healthy: true, status: :running, http_port: get_node_http_port(node)}
      end
    rescue
      _ ->
        %{healthy: false, status: :error}
    end
  end

  defp get_node_http_port(_node) do
    # Since we're using dynamic configuration, we can't easily extract the port
    # from a static configuration. This function is used for status reporting.
    # For now, return nil - in a full implementation, we'd store this info in state
    nil
  end

  defp get_node_logs(nodes, target_node) do
    if target_node do
      # Get logs from specific node
      case Map.values(nodes)
           |> Enum.find(&(Atom.to_string(&1) |> String.contains?(target_node))) do
        nil -> {:error, :node_not_found}
        node -> {:ok, get_single_node_logs(node)}
      end
    else
      # Get logs from all nodes
      logs =
        nodes
        |> Enum.map(fn {name, node} ->
          {name, get_single_node_logs(node)}
        end)
        |> Enum.into(%{})

      {:ok, logs}
    end
  end

  defp get_single_node_logs(node) do
    # For now, return basic node info
    # In a full implementation, this would collect actual log files
    try do
      case :rpc.call(node, :erlang, :system_info, [:system_version], 2000) do
        {:badrpc, reason} -> "Node unreachable: #{inspect(reason)}"
        version -> "Node #{node} running: #{version}"
      end
    rescue
      _ -> "Failed to get logs from #{node}"
    end
  end

  defp calculate_uptime(state) do
    # Simple uptime calculation - in real implementation would track start time
    if state.status == :running do
      "Running"
    else
      "Stopped"
    end
  end
end
