defmodule ClusterTestHelper do
  @moduledoc """
  Helper functions for distributed/cluster testing.

  Provides utilities to start/stop nodes, wait for cluster formation,
  and verify distributed functionality.

  Enhanced with WSL compatibility, reliable hostname resolution,
  and improved port management.
  """

  require Logger

  alias OTPSupervisor.TestCluster.{HostnameResolver, PortManager}

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
      # Clean up common test port ranges
      test_ports = [4100, 4101, 4102, 4103, 9100, 9101, 9102, 9103]

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
end
