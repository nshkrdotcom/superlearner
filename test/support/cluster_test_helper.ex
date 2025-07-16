defmodule ClusterTestHelper do
  @moduledoc """
  Helper functions for distributed/cluster testing.

  Provides utilities to start/stop nodes, wait for cluster formation,
  and verify distributed functionality.

  Enhanced with WSL compatibility, reliable hostname resolution,
  improved port management, and seamless integration with automatic
  cluster management.

  ## Integration Modes

  This helper works in two modes:

  1. **Automatic Mode**: Integrates with AutoClusterManager for seamless
     cluster lifecycle management. Tests tagged with `:distributed` will
     automatically have clusters available.

  2. **Manual Mode**: Traditional explicit cluster management for backward
     compatibility and fine-grained control.

  ## Usage Examples

      # Automatic mode (recommended for new tests)
      @tag :distributed
      test "my distributed test" do
        nodes = ClusterTestHelper.get_cluster_nodes()
        # Test logic with automatic cluster
      end

      # Manual mode (backward compatibility)
      test "manual cluster test" do
        {:ok, nodes} = ClusterTestHelper.start_test_cluster(2)
        # Test logic
        ClusterTestHelper.stop_test_cluster(nodes)
      end

      # Hybrid mode (request specific cluster configuration)
      test "specific cluster requirements" do
        {:ok, nodes} = ClusterTestHelper.ensure_cluster(size: 3, timeout: 60_000)
        # Test logic with guaranteed 3-node cluster
      end
  """

  require Logger

  alias OTPSupervisor.TestCluster.{HostnameResolver, PortManager}
  alias OTPSupervisor.Testing.AutoClusterManager

  @doc """
  Start a test node with the given name suffix.
  Returns {:ok, node_name} or {:error, reason}.

  Uses dynamic hostname resolution and port management for reliability.
  """
  def start_test_node(suffix, opts \\ []) do
    Logger.debug("Starting test node with suffix: #{suffix}")

    # Use hostname resolution instead of hardcoded 127.0.0.1
    case HostnameResolver.get_cluster_hostname() do
      {:ok, hostname} ->
        # Handle suffix that might already contain "test_" prefix
        clean_suffix =
          if String.starts_with?(suffix, "test_") do
            suffix
          else
            "test_#{suffix}"
          end

        node_name = :"#{clean_suffix}@#{hostname}"
        cookie = opts[:cookie] || :test_cluster_cookie
        port = opts[:port]

        Logger.info("Starting test node: #{node_name}")

        case start_node_with_fallback(node_name, hostname, cookie, port) do
          {:ok, node} ->
            setup_node(node)
            {:ok, node}

          error ->
            Logger.error("Failed to start test node #{node_name}: #{inspect(error)}")
            error
        end

      {:error, reason} ->
        Logger.error("Failed to resolve hostname for test node: #{inspect(reason)}")
        {:error, {:hostname_resolution_failed, reason}}
    end
  end

  @doc """
  Stop a test node gracefully with enhanced cleanup.

  Uses port management for thorough cleanup and handles :peer module properly.
  """
  def stop_test_node(node) do
    case node do
      nil ->
        :ok

      node when is_atom(node) ->
        Logger.debug("Stopping test node: #{node}")

        # Get the peer PID if available
        peer_pid = Process.get({:peer_pid, node})

        # Try graceful shutdown first
        graceful_result =
          try do
            :rpc.call(node, :init, :stop, [], 5000)
            :timer.sleep(500)
            :ok
          catch
            _, reason ->
              Logger.debug("Graceful shutdown failed: #{inspect(reason)}")
              {:error, :graceful_failed}
          end

        # If graceful shutdown failed or we have a peer PID, use :peer.stop
        if graceful_result != :ok and peer_pid do
          try do
            :peer.stop(peer_pid)
            Logger.debug("Stopped peer process for node #{node}")
          catch
            _, reason ->
              Logger.debug("Peer stop failed: #{inspect(reason)}")
          end
        end

        # Clean up any ports that might be held by this node
        cleanup_node_ports(node)

        # Remove stored peer PID
        Process.delete({:peer_pid, node})

        :ok
    end
  end

  @doc """
  Wait for nodes to connect and cluster to stabilize.
  """
  def wait_for_cluster(expected_nodes, timeout \\ 5000) do
    start_time = System.monotonic_time(:millisecond)
    wait_for_cluster_loop(expected_nodes, start_time, timeout)
  end

  @doc """
  Wait for a condition to be true with timeout.
  """
  def wait_until(fun, timeout \\ 5000) do
    start_time = System.monotonic_time(:millisecond)
    wait_until_loop(fun, start_time, timeout)
  end

  @doc """
  Get the current cluster size.
  """
  def cluster_size do
    length([Node.self() | Node.list()])
  end

  @doc """
  Verify that distributed components are working.
  """
  def verify_distributed_components do
    try do
      # Test ToolManager
      cluster_status = OTPSupervisor.Distributed.ToolManager.get_cluster_status()

      # Test ClusterStateManager
      topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()

      # Basic verification
      cluster_status.mode == :multi_node and
        topology.total_nodes > 1 and
        length(cluster_status.connected_nodes) > 0
    rescue
      _ -> false
    end
  end

  @doc """
  Start multiple test nodes with automatic port allocation.

  Returns {:ok, nodes} or {:error, reason}.
  """
  def start_test_cluster(node_count, opts \\ []) do
    Logger.info("Starting test cluster with #{node_count} nodes")

    case PortManager.find_available_ports(node_count) do
      {:ok, port_pairs} ->
        start_nodes_with_ports(port_pairs, opts)

      {:error, reason} ->
        Logger.error("Failed to find available ports: #{inspect(reason)}")
        {:error, {:port_allocation_failed, reason}}
    end
  end

  @doc """
  Stop all test nodes and clean up resources.
  """
  def stop_test_cluster(nodes) when is_list(nodes) do
    Logger.info("Stopping test cluster with #{length(nodes)} nodes")

    # Stop all nodes
    Enum.each(nodes, &stop_test_node/1)

    # Clean up any remaining test processes
    PortManager.cleanup_test_processes()

    :ok
  end

  def stop_test_cluster(_), do: {:error, :invalid_nodes_list}

  @doc """
  Clean up all test-related resources.

  This is a comprehensive cleanup that should be called between test runs.
  """
  def cleanup_all_test_resources do
    Logger.info("Performing comprehensive test cleanup")

    # Stop any connected test nodes
    test_nodes = Node.list() |> Enum.filter(&String.contains?(Atom.to_string(&1), "test_"))
    Enum.each(test_nodes, &stop_test_node/1)

    # Clean up test processes
    PortManager.cleanup_test_processes()

    # Wait a bit for cleanup to complete
    :timer.sleep(1000)

    :ok
  end

  # ============================================================================
  # AUTOMATIC CLUSTER MANAGEMENT INTEGRATION
  # ============================================================================

  @doc """
  Get cluster nodes from automatic cluster management.

  This function integrates with AutoClusterManager to provide seamless
  access to cluster nodes for tests tagged with `:distributed`.

  Returns a list of connected cluster nodes, or an empty list if no
  cluster is available.

  ## Examples

      @tag :distributed
      test "my distributed test" do
        nodes = ClusterTestHelper.get_cluster_nodes()
        assert length(nodes) >= 2
        # Test logic with cluster nodes
      end
  """
  def get_cluster_nodes do
    case get_auto_cluster_info() do
      {:ok, %{cluster_active: true, nodes: nodes}} ->
        nodes

      {:ok, %{cluster_active: false}} ->
        Logger.debug("No active cluster available")
        []

      {:error, reason} ->
        Logger.debug("Failed to get cluster info: #{inspect(reason)}")
        []
    end
  end

  @doc """
  Ensure a cluster with specific requirements is available.

  This function coordinates between automatic and manual cluster management
  to ensure the test has access to a cluster meeting the specified requirements.

  ## Options

    * `:size` - Minimum cluster size required (default: 2)
    * `:timeout` - Maximum time to wait for cluster (default: 30_000ms)
    * `:force_new` - Force creation of new cluster (default: false)

  ## Examples

      test "specific cluster requirements" do
        {:ok, nodes} = ClusterTestHelper.ensure_cluster(size: 3, timeout: 60_000)
        assert length(nodes) == 3
        # Test logic with guaranteed 3-node cluster
      end
  """
  def ensure_cluster(opts \\ []) do
    size = Keyword.get(opts, :size, 2)
    timeout = Keyword.get(opts, :timeout, 30_000)
    force_new = Keyword.get(opts, :force_new, false)

    Logger.debug("Ensuring cluster with size: #{size}, timeout: #{timeout}")

    cond do
      force_new ->
        start_manual_cluster(size, opts)

      auto_cluster_suitable?(size) ->
        use_auto_cluster()

      true ->
        coordinate_cluster_creation(size, opts)
    end
  end

  @doc """
  Check if automatic cluster management is active and suitable.

  Returns `{:ok, cluster_info}` if automatic cluster is available and suitable,
  or `{:error, reason}` otherwise.
  """
  def check_auto_cluster_status(min_size \\ 1) do
    case get_auto_cluster_info() do
      {:ok, %{cluster_active: true, nodes: nodes} = info} ->
        if length(nodes) >= min_size do
          {:ok, info}
        else
          {:error, {:insufficient_size, length(nodes), min_size}}
        end

      {:ok, %{cluster_active: false}} ->
        {:error, :no_active_cluster}

      {:error, reason} ->
        {:error, {:auto_cluster_unavailable, reason}}
    end
  end

  @doc """
  Wait for cluster health and connectivity.

  Enhanced version that works with both automatic and manual clusters.
  Provides detailed error reporting and debugging information.
  """
  def wait_for_cluster_health(expected_nodes, timeout \\ 10_000) do
    Logger.debug("Waiting for cluster health: #{expected_nodes} nodes, timeout: #{timeout}ms")

    start_time = System.monotonic_time(:millisecond)

    case wait_for_cluster_health_loop(expected_nodes, start_time, timeout) do
      :ok ->
        Logger.debug("Cluster health confirmed: #{cluster_size()} nodes connected")
        :ok

      {:error, reason} ->
        diagnosis = diagnose_cluster_health_failure(expected_nodes, reason)
        Logger.error("Cluster health check failed: #{inspect(diagnosis)}")
        {:error, diagnosis}
    end
  end

  @doc """
  Coordinate cleanup between automatic and manual cluster management.

  This function ensures proper cleanup regardless of how the cluster was created,
  preventing resource leaks and conflicts between test runs.
  """
  def coordinate_cleanup do
    Logger.debug("Coordinating cluster cleanup")

    # Check if we have manual clusters to clean up
    manual_cleanup_result = cleanup_manual_clusters()

    # Check if automatic cluster needs coordination
    auto_cleanup_result = coordinate_auto_cleanup()

    case {manual_cleanup_result, auto_cleanup_result} do
      {:ok, :ok} ->
        Logger.debug("All cluster cleanup completed successfully")
        :ok

      {manual_result, auto_result} ->
        Logger.warning(
          "Cleanup completed with issues: manual=#{inspect(manual_result)}, auto=#{inspect(auto_result)}"
        )

        {:warning, {manual_result, auto_result}}
    end
  end

  @doc """
  Enhanced error reporting for cluster operations.

  Provides detailed diagnostic information when cluster operations fail,
  including suggestions for resolution and debugging information.
  """
  def diagnose_cluster_error(error, context \\ %{}) do
    base_diagnosis = %{
      error: error,
      context: context,
      timestamp: DateTime.utc_now(),
      cluster_state: get_current_cluster_state(),
      system_info: get_system_diagnostic_info()
    }

    case error do
      {:timeout, operation} ->
        Map.merge(base_diagnosis, %{
          problem: "Cluster operation timed out: #{operation}",
          likely_causes: [
            "Network connectivity issues",
            "System resource constraints",
            "Firewall blocking cluster communication",
            "Hostname resolution problems"
          ],
          suggestions: [
            "Check system resources (CPU, memory)",
            "Verify network connectivity between nodes",
            "Increase timeout values",
            "Check firewall settings"
          ]
        })

      {:port_allocation_failed, _reason} ->
        Map.merge(base_diagnosis, %{
          problem: "Failed to allocate ports for cluster nodes",
          likely_causes: [
            "Ports already in use by other processes",
            "Insufficient port range configured",
            "Permission issues with port binding"
          ],
          suggestions: [
            "Stop other development servers",
            "Configure different port ranges",
            "Check for zombie processes holding ports"
          ],
          port_info: get_port_usage_info()
        })

      {:node_connection_failed, node_errors} ->
        Map.merge(base_diagnosis, %{
          problem: "Failed to connect cluster nodes",
          failed_nodes: node_errors,
          likely_causes: [
            "Node startup failures",
            "Cookie mismatch between nodes",
            "Network connectivity issues",
            "Hostname resolution problems"
          ],
          suggestions: [
            "Check node startup logs",
            "Verify cookie configuration",
            "Test hostname resolution",
            "Check network connectivity"
          ]
        })

      _ ->
        Map.merge(base_diagnosis, %{
          problem: "Unknown cluster error: #{inspect(error)}",
          suggestions: [
            "Check cluster logs for more details",
            "Verify system resources",
            "Try manual cluster startup for debugging"
          ]
        })
    end
  end

  # Private functions

  defp start_node_with_fallback(node_name, hostname, cookie, port) do
    # Use :peer module (OTP 25+) - modern replacement for deprecated :slave
    # Fixed for proper hostname handling and cookie compatibility
    start_with_peer(node_name, hostname, cookie, port)
  end

  defp start_with_peer(node_name, hostname, cookie, port) do
    Logger.debug("Starting peer node #{node_name} on #{hostname}")

    # For :peer module, we need to separate the node name from the host
    # The node_name should be just the name part, not name@host
    node_string = Atom.to_string(node_name)

    case String.split(node_string, "@") do
      [name_part, _host_part] ->
        # Node name already contains @host, use just the name part
        actual_node_name = String.to_atom(name_part)
        host_charlist = String.to_charlist(hostname)

        Logger.debug("Using node name: #{actual_node_name}, host: #{hostname}")

        # Build args with proper cookie handling for :peer module
        base_args = [
          ~c"-setcookie",
          Atom.to_charlist(cookie),
          ~c"-kernel",
          ~c"inet_dist_listen_min",
          ~c"9100",
          ~c"-kernel",
          ~c"inet_dist_listen_max",
          ~c"9200"
        ]

        # Add port-specific args if port is provided
        args =
          if port do
            base_args ++ [~c"-env", ~c"PORT", Integer.to_charlist(port)]
          else
            base_args
          end

        peer_opts = %{
          name: actual_node_name,
          host: host_charlist,
          args: args,
          # Ensure proper connection handling
          connection: :standard_io
        }

        Logger.debug("Peer options: #{inspect(peer_opts)}")

        case :peer.start_link(peer_opts) do
          {:ok, peer_pid, actual_node} ->
            Logger.info("Successfully started peer node #{actual_node}")
            # Store peer PID for cleanup
            Process.put({:peer_pid, actual_node}, peer_pid)
            {:ok, actual_node}

          {:error, reason} ->
            Logger.error("Failed to start peer node: #{inspect(reason)}")
            {:error, reason}
        end

      [_name_only] ->
        # Node name doesn't contain @, this shouldn't happen with our construction
        Logger.error("Invalid node name format: #{node_name}")
        {:error, {:invalid_node_name, node_name}}
    end
  end

  defp start_nodes_with_ports(port_pairs, opts) do
    Logger.debug("Starting nodes with port pairs: #{inspect(port_pairs)}")

    nodes =
      port_pairs
      |> Enum.with_index()
      |> Enum.map(fn {{http_port, _dist_port}, index} ->
        suffix = "cluster_#{index + 1}"
        node_opts = Keyword.put(opts, :port, http_port)

        case start_test_node(suffix, node_opts) do
          {:ok, node} ->
            node

          {:error, reason} ->
            Logger.error("Failed to start node #{suffix}: #{inspect(reason)}")
            {:error, {suffix, reason}}
        end
      end)

    # Check if any failed
    failed = Enum.filter(nodes, &match?({:error, _}, &1))

    if Enum.empty?(failed) do
      successful_nodes = Enum.reject(nodes, &match?({:error, _}, &1))
      {:ok, successful_nodes}
    else
      # Clean up any successful nodes
      successful_nodes = Enum.reject(nodes, &match?({:error, _}, &1))
      Enum.each(successful_nodes, &stop_test_node/1)

      {:error, {:partial_startup_failed, failed}}
    end
  end

  defp cleanup_node_ports(node) do
    Logger.debug("Cleaning up ports for node: #{node}")

    # Extract hostname and try to determine which ports this node might be using
    node_string = Atom.to_string(node)

    # For test nodes, we can make educated guesses about port ranges
    # This is a best-effort cleanup
    if String.contains?(node_string, "test_") do
      # Clean up configured test port ranges instead of hardcoded ones
      config = Application.get_env(:otp_supervisor, :distributed_testing, [])
      http_base = Keyword.get(config, :http_port_base, 4200)
      dist_base = Keyword.get(config, :dist_port_base, 9200)

      test_ports = [
        http_base,
        http_base + 1,
        http_base + 2,
        http_base + 3,
        dist_base,
        dist_base + 1,
        dist_base + 2,
        dist_base + 3
      ]

      Enum.each(test_ports, fn port ->
        case PortManager.get_port_info(port) do
          {:ok, {:port_used, _info}} ->
            Logger.debug("Cleaning up port #{port} potentially used by #{node}")
            PortManager.cleanup_single_port(port)

          _ ->
            :ok
        end
      end)
    end

    :ok
  end

  defp setup_node(node) do
    # Add code paths
    :rpc.call(node, :code, :add_paths, [:code.get_path()])

    # Set cookie
    :rpc.call(node, Node, :set_cookie, [Node.get_cookie()])

    # Start essential applications
    :rpc.call(node, Application, :ensure_all_started, [:logger])

    # Try to start our application (may fail in test environment)
    try do
      :rpc.call(node, Application, :ensure_all_started, [:otp_supervisor])
    catch
      # Ignore failures in test environment
      _, _ -> :ok
    end
  end

  defp wait_for_cluster_loop(expected_nodes, start_time, timeout) do
    current_time = System.monotonic_time(:millisecond)

    if current_time - start_time > timeout do
      {:error, :timeout}
    else
      current_nodes = [Node.self() | Node.list()]

      if length(current_nodes) >= expected_nodes do
        :ok
      else
        :timer.sleep(50)
        wait_for_cluster_loop(expected_nodes, start_time, timeout)
      end
    end
  end

  defp wait_until_loop(fun, start_time, timeout) do
    current_time = System.monotonic_time(:millisecond)

    if current_time - start_time > timeout do
      {:error, :timeout}
    else
      if fun.() do
        :ok
      else
        :timer.sleep(50)
        wait_until_loop(fun, start_time, timeout)
      end
    end
  end

  # ============================================================================
  # PRIVATE INTEGRATION HELPERS
  # ============================================================================

  defp get_auto_cluster_info do
    try do
      case Process.whereis(AutoClusterManager) do
        nil ->
          {:error, :auto_cluster_manager_not_running}

        _pid ->
          AutoClusterManager.get_cluster_info()
      end
    catch
      :exit, {:noproc, _} ->
        {:error, :auto_cluster_manager_not_running}

      error ->
        {:error, {:auto_cluster_manager_error, error}}
    end
  end

  defp auto_cluster_suitable?(min_size) do
    case check_auto_cluster_status(min_size) do
      {:ok, _info} -> true
      _ -> false
    end
  end

  defp use_auto_cluster do
    case get_auto_cluster_info() do
      {:ok, %{cluster_active: true, nodes: nodes}} ->
        {:ok, nodes}

      {:ok, %{cluster_active: false}} ->
        {:error, :no_active_auto_cluster}

      {:error, reason} ->
        {:error, {:auto_cluster_unavailable, reason}}
    end
  end

  defp start_manual_cluster(size, opts) do
    Logger.debug("Starting manual cluster with size: #{size}")
    start_test_cluster(size, opts)
  end

  defp coordinate_cluster_creation(size, opts) do
    Logger.debug("Coordinating cluster creation for size: #{size}")

    # Try to request cluster from AutoClusterManager first
    requirements = %{
      needs_cluster: true,
      min_cluster_size: size,
      test_type: :manual_request
    }

    case try_request_auto_cluster(requirements) do
      {:ok, nodes} ->
        {:ok, nodes}

      {:error, _reason} ->
        # Fall back to manual cluster creation
        Logger.debug("Auto cluster request failed, falling back to manual creation")
        start_manual_cluster(size, opts)
    end
  end

  defp try_request_auto_cluster(requirements) do
    try do
      case Process.whereis(AutoClusterManager) do
        nil ->
          {:error, :auto_cluster_manager_not_available}

        _pid ->
          case AutoClusterManager.start_cluster_for_tests(requirements) do
            {:ok, %{cluster_active: true, nodes: nodes}} ->
              {:ok, nodes}

            {:ok, %{cluster_active: false}} ->
              {:error, :cluster_not_active}

            {:error, reason} ->
              {:error, reason}
          end
      end
    catch
      :exit, {:noproc, _} ->
        {:error, :auto_cluster_manager_not_available}

      error ->
        {:error, {:auto_cluster_request_failed, error}}
    end
  end

  defp cleanup_manual_clusters do
    Logger.debug("Cleaning up manual clusters")

    # Clean up any test nodes we might have started manually
    test_nodes = Node.list() |> Enum.filter(&String.contains?(Atom.to_string(&1), "test_"))

    if Enum.empty?(test_nodes) do
      :ok
    else
      Logger.debug("Stopping #{length(test_nodes)} manual test nodes")
      Enum.each(test_nodes, &stop_test_node/1)
      :ok
    end
  end

  defp coordinate_auto_cleanup do
    Logger.debug("Coordinating auto cluster cleanup")

    # We don't directly clean up auto clusters - that's the AutoClusterManager's job
    # But we can check if coordination is needed
    case get_auto_cluster_info() do
      {:ok, %{managed: true}} ->
        Logger.debug("Auto cluster is managed, no coordination needed")
        :ok

      {:ok, %{managed: false}} ->
        Logger.debug("Auto cluster is not managed by current process")
        :ok

      {:error, _reason} ->
        Logger.debug("No auto cluster to coordinate")
        :ok
    end
  end

  defp wait_for_cluster_health_loop(expected_nodes, start_time, timeout) do
    current_time = System.monotonic_time(:millisecond)

    if current_time - start_time > timeout do
      {:error, :timeout}
    else
      current_size = cluster_size()
      connected_nodes = Node.list()

      cond do
        current_size >= expected_nodes ->
          # Check if nodes are actually responsive
          if all_nodes_responsive?(connected_nodes) do
            :ok
          else
            :timer.sleep(100)
            wait_for_cluster_health_loop(expected_nodes, start_time, timeout)
          end

        true ->
          :timer.sleep(100)
          wait_for_cluster_health_loop(expected_nodes, start_time, timeout)
      end
    end
  end

  defp all_nodes_responsive?(nodes) do
    Enum.all?(nodes, fn node ->
      try do
        :rpc.call(node, :erlang, :node, [], 1000) == node
      catch
        _, _ -> false
      end
    end)
  end

  defp diagnose_cluster_health_failure(expected_nodes, reason) do
    current_size = cluster_size()
    connected_nodes = Node.list()

    %{
      expected_nodes: expected_nodes,
      current_size: current_size,
      connected_nodes: connected_nodes,
      failure_reason: reason,
      diagnosis: build_health_diagnosis(expected_nodes, current_size, connected_nodes, reason)
    }
  end

  defp build_health_diagnosis(expected, current, nodes, reason) do
    cond do
      current == 0 ->
        "No cluster nodes connected. Check if cluster startup succeeded."

      current < expected ->
        "Only #{current}/#{expected} nodes connected. Missing nodes: #{expected - current}"

      reason == :timeout ->
        "Cluster health check timed out. Nodes may be slow to respond: #{inspect(nodes)}"

      true ->
        "Unknown cluster health issue: #{inspect(reason)}"
    end
  end

  defp get_current_cluster_state do
    %{
      cluster_size: cluster_size(),
      connected_nodes: Node.list(),
      self_node: Node.self(),
      auto_cluster_info: get_auto_cluster_info()
    }
  end

  defp get_system_diagnostic_info do
    %{
      memory_usage: :erlang.memory(),
      process_count: :erlang.system_info(:process_count),
      port_count: :erlang.system_info(:port_count),
      node_cookie: Node.get_cookie(),
      otp_release: :erlang.system_info(:otp_release)
    }
  end

  defp get_port_usage_info do
    # Provide basic port usage information for diagnostics
    %{
      common_test_ports: [4100, 4101, 4102, 4103, 9100, 9101, 9102, 9103],
      note: "Port usage details require PortManager integration"
    }
  end
end
