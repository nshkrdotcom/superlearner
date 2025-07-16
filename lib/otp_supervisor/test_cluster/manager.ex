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
    # Check for existing cluster processes and update state if needed
    updated_state = discover_existing_cluster(state)
    status = build_cluster_status(updated_state)
    {:reply, {:ok, status}, updated_state}
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

    # CRITICAL: Ensure cookie synchronization
    current_cookie = Node.get_cookie()
    Logger.debug("Parent node cookie: #{current_cookie}")
    Logger.debug("Child node will use cookie: #{config.cookie}")

    # Only try to set cookie if we're in distributed mode
    if Node.alive?() do
      if current_cookie != config.cookie do
        Logger.info("Synchronizing parent node cookie from #{current_cookie} to #{config.cookie}")
        Node.set_cookie(config.cookie)
      end
    else
      Logger.warning("Parent node not in distributed mode - cookie sync skipped")
      Logger.info("Child nodes will use cookie: #{config.cookie}")
    end

    # Start a REAL Elixir node - a separate OS process running the full application
    env = [
      {"PHX_PORT", Integer.to_string(config.http_port)},
      {"PORT", Integer.to_string(config.http_port)},
      {"MIX_ENV", "dev"},
      {"NODE_NAME", Atom.to_string(config.name)},
      {"PHX_SERVER", "true"},
      # CRITICAL: Pass cookie via environment to ensure it's set early
      {"ERLANG_COOKIE", Atom.to_string(config.cookie)},
      # CRITICAL: Override the test config port to use our dynamic port
      {"TEST_HTTP_PORT", Integer.to_string(config.http_port)}
    ]

    # Use elixir command with proper distributed Erlang flags, then run mix
    # This is the correct way to start a distributed Mix application
    cmd_args = [
      "--name",
      Atom.to_string(config.name),
      "--cookie",
      Atom.to_string(config.cookie),
      "-S",
      "mix",
      "run",
      "--no-halt",
      "--eval",
      """
      IO.puts("Node started: \#{Node.self()}");
      IO.puts("Cookie: \#{Node.get_cookie()}");
      Application.put_env(:otp_supervisor, OtpSupervisorWeb.Endpoint, [
        http: [ip: {127, 0, 0, 1}, port: #{config.http_port}],
        server: true
      ]);
      {:ok, _} = Application.ensure_all_started(:otp_supervisor);
      IO.puts("Phoenix started on port #{config.http_port}");
      :timer.sleep(:infinity)
      """
    ]

    # Start the node as a separate OS process using Task.async
    # Use elixir executable instead of mix to properly handle distributed flags
    task =
      Task.async(fn ->
        System.cmd("elixir", cmd_args,
          env: env,
          cd: File.cwd!(),
          into: IO.stream(:stdio, :line)
        )
      end)

    # Wait for node to actually be ready instead of fixed sleep
    Logger.debug("Waiting for node #{config.name} to start and register...")

    # Test connection with retry logic - this replaces the fixed sleep
    case wait_for_node_connection(config.name, 30, 1000) do
      :ok ->
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

      {:error, reason} ->
        Logger.error("Failed to connect to cluster node #{config.name} after retries: #{reason}")
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
    # CRITICAL: We DO need distributed Erlang for cluster communication
    if Node.alive?() do
      Logger.info("Using current node as primary: #{Node.self()}")
      Node.set_cookie(:test_cluster_cookie)
    else
      Logger.info("Starting distributed Erlang for cluster management...")

      # Start distributed Erlang with a unique name
      node_name = :"cluster_manager_#{System.system_time(:millisecond)}@127.0.0.1"

      case Node.start(node_name, :longnames) do
        {:ok, _} ->
          Node.set_cookie(:test_cluster_cookie)
          Logger.info("Started distributed Erlang: #{Node.self()}")
          Logger.info("Cookie set to: #{Node.get_cookie()}")

        {:error, {:already_started, _}} ->
          Node.set_cookie(:test_cluster_cookie)
          Logger.info("Distributed Erlang already started: #{Node.self()}")

        {:error, reason} ->
          Logger.error("Failed to start distributed Erlang: #{inspect(reason)}")
          {:error, {:distributed_erlang_failed, reason}}
      end
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

  defp wait_for_cluster_formation(nodes, timeout \\ 60_000) do
    Logger.info("Waiting for servers to be ready...")

    # For Phoenix servers, just wait for them to respond to HTTP requests
    # Increased timeout to account for compilation time
    start_time = System.monotonic_time(:millisecond)
    wait_for_servers_ready(nodes, start_time, timeout)
  end

  defp wait_for_servers_ready(nodes, start_time, timeout) do
    current_time = System.monotonic_time(:millisecond)
    elapsed = current_time - start_time

    if elapsed > timeout do
      Logger.error("Server readiness timeout after #{elapsed}ms")
      {:error, :server_ready_timeout}
    else
      case check_servers_readiness(nodes) do
        {:all_ready, ready_count} ->
          Logger.info("All #{ready_count} servers are ready and responding (took #{elapsed}ms)")
          :ok

        {:partial_ready, ready_count, total_count} ->
          Logger.debug("#{ready_count}/#{total_count} servers ready, waiting...")
          # Use shorter sleep for more responsive checking
          :timer.sleep(500)
          wait_for_servers_ready(nodes, start_time, timeout)

        {:none_ready, total_count} ->
          Logger.debug("0/#{total_count} servers ready, waiting...")
          :timer.sleep(1000)
          wait_for_servers_ready(nodes, start_time, timeout)
      end
    end
  end

  defp check_servers_readiness(nodes) do
    total_count = map_size(nodes)

    ready_servers =
      nodes
      |> Enum.map(fn {name, server_info} ->
        hostname = Map.get(server_info, :hostname, "127.0.0.1")
        url = ~c"http://#{hostname}:#{server_info.http_port}/"

        case :httpc.request(:get, {url, []}, [{:timeout, 2000}], []) do
          {:ok, _} ->
            Logger.debug("Server #{name} is ready on #{hostname}:#{server_info.http_port}")
            {name, :ready}

          {:error, reason} ->
            Logger.debug("Server #{name} not ready: #{inspect(reason)}")
            {name, :not_ready}
        end
      end)

    ready_count = ready_servers |> Enum.count(fn {_, status} -> status == :ready end)

    cond do
      ready_count == total_count -> {:all_ready, ready_count}
      ready_count > 0 -> {:partial_ready, ready_count, total_count}
      true -> {:none_ready, total_count}
    end
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
      Logger.info("Stopping node #{server_info.name} on port #{server_info.http_port}")

      # Stop the Task that's running the Elixir node
      if Map.has_key?(server_info, :task) do
        Task.shutdown(server_info.task, :brutal_kill)
      end

      # Also kill any processes using the HTTP port directly
      case System.cmd("lsof", ["-ti:#{server_info.http_port}"], stderr_to_stdout: true) do
        {pids_output, 0} ->
          pids =
            pids_output
            |> String.trim()
            |> String.split("\n")
            |> Enum.reject(&(&1 == ""))

          Enum.each(pids, fn pid ->
            Logger.debug("Killing process #{pid} on port #{server_info.http_port}")
            System.cmd("kill", ["-9", pid])
          end)

        {_, _} ->
          Logger.debug("No processes found on port #{server_info.http_port} or lsof failed")
      end

      # Kill any elixir processes with the node name
      node_name = Map.get(server_info, :name, "unknown")

      case System.cmd("pkill", ["-f", "#{node_name}"], stderr_to_stdout: true) do
        {_, 0} -> Logger.debug("Killed processes matching #{node_name}")
        {_, 1} -> Logger.debug("No processes found matching #{node_name}")
        {_, _} -> Logger.debug("pkill failed for #{node_name}")
      end

      :ok
    rescue
      error ->
        Logger.warning("Error stopping node: #{inspect(error)}")
        :ok
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

  defp get_node_status(node_info) when is_map(node_info) do
    # Handle discovered nodes (which are maps with full info)
    if Map.has_key?(node_info, :discovered) and node_info.discovered do
      # This is a discovered node, try to ping it
      case Node.ping(node_info.name) do
        :pong ->
          %{
            healthy: true,
            status: :running,
            http_port: node_info.http_port,
            discovered: true
          }

        :pang ->
          %{
            healthy: false,
            status: :unreachable,
            http_port: node_info.http_port,
            discovered: true
          }
      end
    else
      # This is a regular node started by this Manager
      get_node_status_by_name(node_info.name)
    end
  end

  defp get_node_status(node_name) when is_atom(node_name) do
    # Handle node names directly
    get_node_status_by_name(node_name)
  end

  defp get_node_status_by_name(node_name) do
    try do
      case :rpc.call(node_name, Node, :self, [], 2000) do
        {:badrpc, reason} ->
          %{healthy: false, status: :unreachable, error: reason}

        ^node_name ->
          %{healthy: true, status: :running, http_port: nil}
      end
    rescue
      _ ->
        %{healthy: false, status: :error}
    end
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

  # Cluster discovery functions

  defp discover_existing_cluster(state) do
    Logger.debug("Discovering existing cluster processes...")

    # Ensure we're in distributed mode to ping nodes
    ensure_distributed_mode()

    # Get configuration for port ranges
    config = Application.get_env(:otp_supervisor, :distributed_testing, [])
    http_base = Keyword.get(config, :http_port_base, 4200)

    # Check for running processes on test ports
    discovered_nodes =
      http_base..(http_base + 5)
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {port, index}, acc ->
        case discover_node_on_port(port, index + 1) do
          {:ok, node_info} ->
            node_key = :"node#{index + 1}"
            Map.put(acc, node_key, node_info)

          :not_found ->
            acc
        end
      end)

    # Update state if we found running nodes
    if map_size(discovered_nodes) > 0 do
      Logger.info("Discovered #{map_size(discovered_nodes)} existing cluster nodes")
      %{state | nodes: discovered_nodes, status: :running}
    else
      Logger.debug("No existing cluster nodes found")
      state
    end
  end

  defp discover_node_on_port(port, node_index) do
    # Check if port is occupied
    case System.cmd("lsof", ["-ti:#{port}"], stderr_to_stdout: true) do
      {pids_output, 0} ->
        pids =
          pids_output
          |> String.trim()
          |> String.split("\n")
          |> Enum.reject(&(&1 == ""))

        if not Enum.empty?(pids) do
          # Try to determine if this is a test node
          pid = List.first(pids)

          case get_process_command(pid) do
            {:ok, cmd} ->
              if String.contains?(cmd, "test_node#{node_index}@127.0.0.1") do
                # This looks like our test node
                node_name = :"test_node#{node_index}@127.0.0.1"

                # Try to ping the node to confirm it's alive
                case Node.ping(node_name) do
                  :pong ->
                    Logger.debug("Discovered active test node: #{node_name} on port #{port}")

                    {:ok,
                     %{
                       name: node_name,
                       http_port: port,
                       status: :running,
                       url: "http://127.0.0.1:#{port}",
                       hostname: "127.0.0.1",
                       # Mark as discovered, not started by this Manager
                       discovered: true
                     }}

                  :pang ->
                    Logger.debug(
                      "Found process on port #{port} but node #{node_name} not responding"
                    )

                    :not_found
                end
              else
                Logger.debug("Process on port #{port} is not a test node")
                :not_found
              end

            {:error, _} ->
              Logger.debug("Could not get command for process on port #{port}")
              :not_found
          end
        else
          :not_found
        end

      {_, _} ->
        :not_found
    end
  end

  defp get_process_command(pid) do
    case System.cmd("ps", ["-p", pid, "-o", "cmd", "--no-headers"], stderr_to_stdout: true) do
      {output, 0} ->
        cmd = String.trim(output)
        {:ok, cmd}

      {_, _} ->
        {:error, :process_not_found}
    end
  end

  defp ensure_distributed_mode do
    unless Node.alive?() do
      Logger.debug("Starting distributed Erlang for node discovery...")
      node_name = :"discovery_#{System.system_time(:millisecond)}@127.0.0.1"

      case Node.start(node_name, :longnames) do
        {:ok, _} ->
          Node.set_cookie(:test_cluster_cookie)
          Logger.debug("Started distributed Erlang for discovery: #{Node.self()}")

        {:error, {:already_started, _}} ->
          Node.set_cookie(:test_cluster_cookie)
          Logger.debug("Using existing distributed node: #{Node.self()}")

        {:error, reason} ->
          Logger.warning("Failed to start distributed Erlang for discovery: #{inspect(reason)}")
      end
    else
      # Ensure we have the right cookie
      Node.set_cookie(:test_cluster_cookie)
    end
  end

  # Cookie synchronization helper functions

  defp wait_for_node_connection(node_name, max_retries, retry_interval) do
    wait_for_node_connection(node_name, max_retries, retry_interval, 1)
  end

  defp wait_for_node_connection(node_name, max_retries, retry_interval, attempt) do
    Logger.debug("Attempting to connect to #{node_name} (attempt #{attempt}/#{max_retries})")

    case Node.ping(node_name) do
      :pong ->
        Logger.debug("Successfully connected to #{node_name} on attempt #{attempt}")
        :ok

      :pang when attempt < max_retries ->
        Logger.debug("Connection failed, retrying in #{retry_interval}ms...")
        :timer.sleep(retry_interval)
        wait_for_node_connection(node_name, max_retries, retry_interval, attempt + 1)

      :pang ->
        Logger.error("Failed to connect to #{node_name} after #{max_retries} attempts")
        {:error, :connection_timeout}
    end
  end
end
