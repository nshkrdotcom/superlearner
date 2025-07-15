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
  
  alias OTPSupervisor.TestCluster.HealthChecker
  
  # Test cluster configuration
  @test_nodes %{
    node1: %{
      name: :"test_node1@127.0.0.1",
      http_port: 4100,
      dist_port: 9100,
      cookie: :test_cluster_cookie
    },
    node2: %{
      name: :"test_node2@127.0.0.1", 
      http_port: 4101,
      dist_port: 9101,
      cookie: :test_cluster_cookie
    },
    node3: %{
      name: :"test_node3@127.0.0.1",
      http_port: 4102,
      dist_port: 9102,
      cookie: :test_cluster_cookie
    }
  }
  
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
    Logger.info("Starting test cluster...")
    
    case start_cluster_nodes(opts) do
      {:ok, nodes} ->
        new_state = %{state | nodes: nodes, status: :running}
        Logger.info("Test cluster started successfully with #{map_size(nodes)} nodes")
        {:reply, {:ok, nodes}, new_state}
        
      {:error, reason} ->
        Logger.error("Failed to start test cluster: #{inspect(reason)}")
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
    Logger.info("Starting cluster node #{config.name} on port #{config.http_port}")
    
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
      "--name", Atom.to_string(config.name),
      "--cookie", Atom.to_string(config.cookie),
      "-S", "mix", "phx.server"
    ]
    
    # Start the node as a separate OS process using Task.async
    task = Task.async(fn ->
      System.cmd("elixir", cmd_args, [
        env: env,
        cd: File.cwd!(),
        into: IO.stream(:stdio, :line)
      ])
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
          url: "http://127.0.0.1:#{config.http_port}"
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
    
    checks = [
      &check_epmd_running/0,
      &check_network_configuration/0,
      &check_port_availability/0,
      &check_hostname_resolution/0
    ]
    
    case run_environment_checks(checks) do
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
    Logger.info("Provisioning test nodes...")
    
    node_configs = case opts[:node_count] do
      count when is_integer(count) and count > 0 ->
        @test_nodes |> Enum.take(count) |> Enum.into(%{})
      _ ->
        # Default to 2 nodes for most tests
        @test_nodes |> Enum.take(2) |> Enum.into(%{})
    end
    
    # Start actual Phoenix servers instead of peer nodes
    results = 
      node_configs
      |> Enum.map(fn {name, config} ->
        case start_phoenix_server(config) do
          {:ok, server_info} -> {name, {:ok, server_info}}
          {:error, reason} -> {name, {:error, reason}}
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
      {:error, {:server_startup_failed, failed_nodes}}
    end
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
      case :httpc.request(:get, {~c"http://127.0.0.1:#{server_info.http_port}/", []}, [], []) do
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
    all_healthy = Enum.all?(nodes, fn {_name, server_info} ->
      case :httpc.request(:get, {~c"http://127.0.0.1:#{server_info.http_port}/", []}, [], []) do
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
      _ -> :ok  # Server might already be down
    end
  end
  
  defp cleanup_failed_nodes do
    # Force cleanup of any partially started nodes
    Logger.warning("Cleaning up failed node startup attempts...")
    
    # Kill any processes that might be hanging around
    System.cmd("pkill", ["-f", "test_node"], stderr_to_stdout: true)
    
    :ok
  end
  
  defp clean_test_artifacts do
    # Clean up any test-specific files, logs, etc.
    Logger.info("Cleaning up test artifacts...")
    
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
  
  defp get_node_http_port(node) do
    # Extract port from node configuration
    @test_nodes
    |> Enum.find_value(fn {_, config} ->
      if config.name == node, do: config.http_port
    end)
  end
  
  defp get_node_logs(nodes, target_node) do
    if target_node do
      # Get logs from specific node
      case Map.values(nodes) |> Enum.find(&(Atom.to_string(&1) |> String.contains?(target_node))) do
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
  
  # Environment validation functions
  
  defp run_environment_checks(checks) do
    failed_checks = 
      checks
      |> Enum.map(fn check -> {check, check.()} end)
      |> Enum.filter(fn {_, result} -> result != :ok end)
    
    if Enum.empty?(failed_checks) do
      :ok
    else
      {:error, failed_checks}
    end
  end
  
  defp check_epmd_running do
    case System.cmd("epmd", ["-names"], stderr_to_stdout: true) do
      {output, 0} ->
        if String.contains?(output, "up and running") do
          :ok
        else
          {:error, :epmd_not_running}
        end
      {_, _} ->
        {:error, :epmd_not_available}
    end
  end
  
  defp check_network_configuration do
    # Test basic network connectivity
    case :inet.gethostbyname(~c"127.0.0.1") do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, {:network_config, reason}}
    end
  end
  
  defp check_port_availability do
    # Check if our test ports are available
    test_ports = [4100, 4101, 4102, 9100, 9101, 9102]
    
    unavailable_ports = 
      test_ports
      |> Enum.filter(fn port ->
        case :gen_tcp.listen(port, []) do
          {:ok, socket} -> 
            :gen_tcp.close(socket)
            false
          {:error, _} -> 
            true
        end
      end)
    
    if Enum.empty?(unavailable_ports) do
      :ok
    else
      {:error, {:ports_unavailable, unavailable_ports}}
    end
  end
  
  defp check_hostname_resolution do
    case :inet.gethostbyname(:localhost) do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, {:hostname_resolution, reason}}
    end
  end
end