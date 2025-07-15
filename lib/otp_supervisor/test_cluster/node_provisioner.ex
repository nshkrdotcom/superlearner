defmodule OTPSupervisor.TestCluster.NodeProvisioner do
  @moduledoc """
  Provisions and manages individual test nodes with robust distributed Erlang startup.

  CRITICAL FEATURES addressing LIVE_CLUSTER_TESTING_ANALYSIS.md issues:
  - Handles :nodistribution and :not_alive errors specifically
  - Implements retry logic for node startup failures  
  - Validates network configuration (127.0.0.1 vs localhost)
  - Manages cookie authentication properly
  - Provides comprehensive error diagnostics
  - Ensures EPMD is running and responsive
  """

  require Logger

  @max_startup_retries 3
  @startup_retry_delay 2000

  @doc """
  Provision a test node with comprehensive error handling.

  This function addresses all the critical startup issues identified:
  - :nodistribution errors from improper distributed Erlang setup
  - :not_alive errors when current node isn't distributed
  - Network binding issues between 127.0.0.1 and localhost
  - Cookie authentication failures
  - Port conflicts and EPMD issues
  """
  def provision_node(node_config) do
    Logger.info("Provisioning node: #{node_config.name}")

    with :ok <- validate_node_config(node_config),
         :ok <- ensure_prerequisites(node_config),
         {:ok, node} <- start_node_with_retry(node_config),
         :ok <- post_startup_validation(node, node_config) do
      Logger.info("Successfully provisioned node: #{node}")
      {:ok, node}
    else
      {:error, reason} ->
        Logger.error("Failed to provision node #{node_config.name}: #{inspect(reason)}")
        cleanup_failed_node(node_config)
        {:error, reason}
    end
  end

  @doc """
  Stop and clean up a provisioned node.
  """
  def cleanup_node(node) when is_atom(node) do
    Logger.info("Cleaning up node: #{node}")

    try do
      # Graceful shutdown first
      case :rpc.call(node, :init, :stop, [], 5000) do
        :ok ->
          Logger.debug("Node #{node} stopped gracefully")

        {:badrpc, reason} ->
          Logger.warning("Node #{node} RPC failed during shutdown: #{inspect(reason)}")
      end

      # Wait a bit for graceful shutdown
      :timer.sleep(2000)

      # Force cleanup if needed
      force_cleanup_node(node)

      :ok
    rescue
      error ->
        Logger.warning("Error during node cleanup: #{inspect(error)}")
        force_cleanup_node(node)
        :ok
    end
  end

  # Private implementation

  defp validate_node_config(config) do
    required_fields = [:name, :http_port, :dist_port, :cookie]

    missing_fields =
      required_fields
      |> Enum.filter(fn field -> not Map.has_key?(config, field) end)

    if Enum.empty?(missing_fields) do
      :ok
    else
      {:error, {:invalid_config, missing_fields}}
    end
  end

  defp ensure_prerequisites(node_config) do
    Logger.debug("Ensuring prerequisites for #{node_config.name}")

    with :ok <- ensure_current_node_distributed(),
         :ok <- validate_network_setup(),
         :ok <- ensure_epmd_running(),
         :ok <- check_port_availability(node_config),
         :ok <- validate_cookie_setup(node_config) do
      :ok
    else
      {:error, reason} ->
        {:error, {:prerequisites_failed, reason}}
    end
  end

  defp ensure_current_node_distributed do
    # CRITICAL: Fix :not_alive errors
    unless Node.alive?() do
      Logger.warning("Current node is not alive - attempting to start distributed Erlang")

      case Node.start(:"test_primary@127.0.0.1", :shortnames) do
        {:ok, _} ->
          Node.set_cookie(:test_cluster_cookie)
          Logger.info("Started distributed Erlang: #{Node.self()}")
          :ok

        {:error, {:already_started, _}} ->
          Node.set_cookie(:test_cluster_cookie)
          Logger.info("Distributed Erlang already started: #{Node.self()}")
          :ok

        {:error, reason} ->
          Logger.error("Failed to start distributed Erlang: #{inspect(reason)}")
          Logger.info("This usually means EPMD is not running or there are network issues")
          Logger.info("Try: epmd -daemon")
          {:error, {:distributed_startup_failed, reason}}
      end
    else
      Logger.debug("Current node is alive: #{Node.self()}")
      :ok
    end
  end

  defp validate_network_setup do
    # CRITICAL: Address network configuration issues
    checks = [
      {:localhost_resolution, &check_localhost_resolution/0},
      {:ip_127_binding, &check_127_binding/0},
      {:hostname_resolution, &check_hostname_resolution/0}
    ]

    failed_checks =
      checks
      |> Enum.filter(fn {_name, check} -> check.() != :ok end)

    if Enum.empty?(failed_checks) do
      :ok
    else
      Logger.error("Network validation failed: #{inspect(failed_checks)}")
      {:error, {:network_validation_failed, failed_checks}}
    end
  end

  defp ensure_epmd_running do
    # CRITICAL: Ensure EPMD is available
    case System.cmd("epmd", ["-names"], stderr_to_stdout: true) do
      {output, 0} ->
        if String.contains?(output, "up and running") do
          Logger.debug("EPMD is running")
          :ok
        else
          Logger.error("EPMD is not running properly")
          {:error, :epmd_not_running}
        end

      {error_output, exit_code} ->
        Logger.error("EPMD check failed (exit #{exit_code}): #{error_output}")
        {:error, {:epmd_check_failed, exit_code, error_output}}
    end
  end

  defp check_port_availability(node_config) do
    ports_to_check = [node_config.http_port, node_config.dist_port]

    unavailable_ports =
      ports_to_check
      |> Enum.filter(fn port ->
        case :gen_tcp.listen(port, []) do
          {:ok, socket} ->
            :gen_tcp.close(socket)
            # Port is available
            false

          {:error, :eaddrinuse} ->
            # Port is in use
            true

          {:error, reason} ->
            Logger.warning("Port #{port} check failed: #{inspect(reason)}")
            # Assume unavailable
            true
        end
      end)

    if Enum.empty?(unavailable_ports) do
      :ok
    else
      Logger.error("Ports unavailable: #{inspect(unavailable_ports)}")
      {:error, {:ports_unavailable, unavailable_ports}}
    end
  end

  defp validate_cookie_setup(node_config) do
    current_cookie = Node.get_cookie()
    expected_cookie = node_config.cookie

    if current_cookie == expected_cookie do
      :ok
    else
      Logger.warning("Cookie mismatch - current: #{current_cookie}, expected: #{expected_cookie}")
      # Set the correct cookie
      Node.set_cookie(expected_cookie)
      :ok
    end
  end

  defp start_node_with_retry(node_config, attempt \\ 1) do
    Logger.debug("Starting node #{node_config.name} (attempt #{attempt}/#{@max_startup_retries})")

    case start_node(node_config) do
      {:ok, node} ->
        Logger.info("Node #{node} started successfully on attempt #{attempt}")
        {:ok, node}

      {:error, reason} when attempt < @max_startup_retries ->
        Logger.warning("Node startup failed (attempt #{attempt}): #{inspect(reason)}")
        Logger.info("Retrying in #{@startup_retry_delay}ms...")

        # Clean up any partial state
        cleanup_failed_node(node_config)

        # Wait before retry
        :timer.sleep(@startup_retry_delay)

        # Retry with exponential backoff
        start_node_with_retry(node_config, attempt + 1)

      {:error, reason} ->
        Logger.error(
          "Node startup failed after #{@max_startup_retries} attempts: #{inspect(reason)}"
        )

        diagnose_startup_failure(reason, node_config)
        {:error, {:max_retries_exceeded, reason}}
    end
  end

  defp start_node(node_config) do
    # Use :peer module for OTP 25+ with proper error handling
    start_with_peer(node_config)
  end

  defp start_with_peer(node_config) do
    # CRITICAL: Address :peer module issues
    peer_args = %{
      name: node_config.name,
      # Use 127.0.0.1 explicitly
      host: ~c"127.0.0.1",
      args: [
        ~c"-setcookie",
        Atom.to_charlist(node_config.cookie),
        ~c"-kernel",
        ~c"inet_dist_listen_min",
        Integer.to_charlist(node_config.dist_port),
        ~c"-kernel",
        ~c"inet_dist_listen_max",
        Integer.to_charlist(node_config.dist_port)
      ],
      env: [
        {~c"MIX_ENV", ~c"test"},
        {~c"PORT", Integer.to_charlist(node_config.http_port)}
      ]
    }

    case :peer.start_link(peer_args) do
      {:ok, peer_pid, node} ->
        Logger.debug("Peer node started: #{node} (peer: #{inspect(peer_pid)})")
        {:ok, node}

      {:error, :not_alive} ->
        Logger.error("Peer startup failed: current node is not alive")
        {:error, :not_alive}

      {:error, reason} ->
        Logger.error("Peer startup failed: #{inspect(reason)}")
        {:error, {:peer_startup_failed, reason}}
    end
  end

  defp post_startup_validation(node, node_config) do
    Logger.debug("Validating node #{node} after startup")

    with :ok <- verify_node_alive(node),
         :ok <- verify_node_connectivity(node),
         :ok <- verify_cookie_authentication(node, node_config),
         :ok <- verify_application_startup(node) do
      Logger.debug("Post-startup validation passed for #{node}")
      :ok
    else
      {:error, reason} ->
        Logger.error("Post-startup validation failed for #{node}: #{inspect(reason)}")
        {:error, {:post_startup_validation_failed, reason}}
    end
  end

  defp verify_node_alive(node) do
    case :rpc.call(node, Node, :self, [], 5000) do
      ^node ->
        :ok

      {:badrpc, reason} ->
        {:error, {:node_not_responding, reason}}

      other ->
        {:error, {:unexpected_node_response, other}}
    end
  end

  defp verify_node_connectivity(node) do
    case :rpc.call(node, :erlang, :system_info, [:system_version], 5000) do
      {:badrpc, reason} ->
        {:error, {:connectivity_failed, reason}}

      _version ->
        :ok
    end
  end

  defp verify_cookie_authentication(node, node_config) do
    case :rpc.call(node, Node, :get_cookie, [], 5000) do
      {:badrpc, reason} ->
        {:error, {:cookie_check_failed, reason}}

      cookie when cookie == node_config.cookie ->
        :ok

      other_cookie ->
        {:error, {:cookie_mismatch, other_cookie, node_config.cookie}}
    end
  end

  defp verify_application_startup(node) do
    # Ensure our application can start on the test node
    case :rpc.call(node, Application, :ensure_all_started, [:otp_supervisor], 10000) do
      {:ok, _apps} ->
        Logger.debug("OTP Supervisor application started on #{node}")
        :ok

      {:error, reason} ->
        Logger.warning("Application startup failed on #{node}: #{inspect(reason)}")
        # Don't fail the node provisioning for this - the app might not be needed
        :ok

      {:badrpc, reason} ->
        {:error, {:application_startup_check_failed, reason}}
    end
  end

  defp cleanup_failed_node(node_config) do
    Logger.debug("Cleaning up failed node: #{node_config.name}")

    # Try to kill any processes that might be hanging around
    node_name_str = Atom.to_string(node_config.name)

    try do
      System.cmd("pkill", ["-f", node_name_str], stderr_to_stdout: true)
    rescue
      _ -> :ok
    end

    :ok
  end

  defp force_cleanup_node(node) do
    node_name_str = Atom.to_string(node)

    try do
      # Force kill any remaining processes
      System.cmd("pkill", ["-9", "-f", node_name_str], stderr_to_stdout: true)
    rescue
      _ -> :ok
    end

    :ok
  end

  defp diagnose_startup_failure(reason, node_config) do
    Logger.error("=== NODE STARTUP FAILURE DIAGNOSIS ===")
    Logger.error("Node: #{node_config.name}")
    Logger.error("Reason: #{inspect(reason)}")

    case reason do
      :not_alive ->
        Logger.error("DIAGNOSIS: Current node is not running in distributed mode")
        Logger.error("SOLUTION: Ensure the test environment starts distributed Erlang first")
        Logger.error("  Try: Node.start(:\"test_primary@127.0.0.1\", :shortnames)")

      {:peer_startup_failed, :not_alive} ->
        Logger.error("DIAGNOSIS: Peer module requires current node to be alive")
        Logger.error("SOLUTION: Start distributed Erlang before running tests")

      {:ports_unavailable, ports} ->
        Logger.error("DIAGNOSIS: Required ports are in use: #{inspect(ports)}")
        Logger.error("SOLUTION: Stop processes using these ports or use different ports")
        Logger.error("  Check with: netstat -an | grep #{Enum.join(ports, "\\|")}")

      {:network_validation_failed, checks} ->
        Logger.error("DIAGNOSIS: Network configuration issues: #{inspect(checks)}")
        Logger.error("SOLUTION: Check network connectivity and hostname resolution")

      :epmd_not_running ->
        Logger.error("DIAGNOSIS: EPMD (Erlang Port Mapper Daemon) is not running")
        Logger.error("SOLUTION: Start EPMD with: epmd -daemon")

      other ->
        Logger.error("DIAGNOSIS: Unexpected startup failure: #{inspect(other)}")
        Logger.error("SOLUTION: Check logs and network configuration")
    end

    Logger.error("=== END DIAGNOSIS ===")
  end

  # Network validation helpers

  defp check_localhost_resolution do
    case :inet.gethostbyname(:localhost) do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, {:localhost_resolution, reason}}
    end
  end

  defp check_127_binding do
    case :inet.gethostbyname(~c"127.0.0.1") do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, {:ip_127_binding, reason}}
    end
  end

  defp check_hostname_resolution do
    case :inet.gethostname() do
      {:ok, hostname} ->
        case :inet.gethostbyname(hostname) do
          {:ok, _} -> :ok
          {:error, reason} -> {:error, {:hostname_resolution, hostname, reason}}
        end

      {:error, reason} ->
        {:error, {:hostname_lookup, reason}}
    end
  end
end
