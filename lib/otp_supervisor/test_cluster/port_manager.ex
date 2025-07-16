defmodule OTPSupervisor.TestCluster.PortManager do
  @moduledoc """
  Smart port management for test clusters.

  Handles dynamic port allocation, conflict resolution, and cleanup
  for distributed test environments, particularly WSL Ubuntu 24.04.
  """

  require Logger

  @doc """
  Find available ports for the specified number of nodes.

  Returns port pairs (HTTP port, distribution port) for each node.
  """
  def find_available_ports(node_count) when is_integer(node_count) and node_count > 0 do
    # Get port configuration from the distributed testing config
    config = Application.get_env(:otp_supervisor, :distributed_testing, [])
    http_base = Keyword.get(config, :http_port_base, 4200)  # Use configured ports, default 4200
    dist_base = Keyword.get(config, :dist_port_base, 9200)  # Use configured ports, default 9200

    Logger.debug(
      "Finding #{node_count} port pairs starting from HTTP:#{http_base}, Dist:#{dist_base}"
    )

    case {find_free_ports(http_base, node_count), find_free_ports(dist_base, node_count)} do
      {{:ok, http_ports}, {:ok, dist_ports}} ->
        port_pairs = Enum.zip(http_ports, dist_ports)
        Logger.info("Found available port pairs: #{inspect(port_pairs)}")
        {:ok, port_pairs}

      {{:error, reason}, _} ->
        Logger.error("Failed to find HTTP ports: #{inspect(reason)}")
        {:error, {:no_http_ports, http_base, node_count}}

      {_, {:error, reason}} ->
        Logger.error("Failed to find distribution ports: #{inspect(reason)}")
        {:error, {:no_dist_ports, dist_base, node_count}}
    end
  end

  def find_available_ports(_), do: {:error, :invalid_node_count}

  @doc """
  Find a range of free ports starting from the given base port.
  """
  def find_free_ports(start_port, count) when is_integer(start_port) and is_integer(count) do
    # Search in a reasonable range (50 ports should be enough)
    search_range = start_port..(start_port + 50)

    free_ports =
      search_range
      |> Enum.filter(&port_free?/1)
      |> Enum.take(count)

    if length(free_ports) >= count do
      {:ok, free_ports}
    else
      {:error, {:insufficient_ports, length(free_ports), count}}
    end
  end

  @doc """
  Check if a specific port is available for binding.
  """
  def port_free?(port) when is_integer(port) do
    case :gen_tcp.listen(port, [:binary, {:active, false}, {:reuseaddr, true}]) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        true

      {:error, :eaddrinuse} ->
        false

      {:error, _other} ->
        # Other errors might indicate system issues, but port might still be usable
        false
    end
  end

  @doc """
  Get information about what process is using a specific port.
  """
  def get_port_info(port) when is_integer(port) do
    case System.cmd("netstat", ["-tulpn"], stderr_to_stdout: true) do
      {output, 0} ->
        lines = String.split(output, "\n")
        port_lines = Enum.filter(lines, &String.contains?(&1, ":#{port} "))

        case port_lines do
          [] -> {:ok, :port_free}
          [line | _] -> {:ok, {:port_used, parse_netstat_line(line)}}
        end

      {error, _} ->
        Logger.warning("Failed to get port info: #{error}")
        {:error, :netstat_failed}
    end
  end

  @doc """
  Clean up ports by terminating processes using them.

  Uses system commands to kill processes on the specified ports.
  """
  def cleanup_ports(port_pairs) when is_list(port_pairs) do
    Logger.info("Cleaning up ports: #{inspect(port_pairs)}")

    # Extract individual ports from tuples and flatten the list
    individual_ports =
      port_pairs
      |> Enum.flat_map(fn
        {http_port, dist_port} -> [http_port, dist_port]
        port when is_integer(port) -> [port]
        _ -> []
      end)
      |> Enum.uniq()

    results = Enum.map(individual_ports, &cleanup_single_port/1)

    failed_cleanups = Enum.filter(results, &match?({:error, _}, &1))

    if Enum.empty?(failed_cleanups) do
      Logger.info("Port cleanup completed successfully")
      :ok
    else
      Logger.warning("Some port cleanups failed: #{inspect(failed_cleanups)}")
      {:partial_success, failed_cleanups}
    end
  end

  def cleanup_ports(_), do: {:error, :invalid_port_list}

  @doc """
  Clean up a single port by killing the process using it.
  """
  def cleanup_single_port(port) when is_integer(port) do
    Logger.debug("Cleaning up port #{port}")

    # Try fuser first (more reliable)
    case System.cmd("fuser", ["-k", "#{port}/tcp"], stderr_to_stdout: true) do
      {_output, 0} ->
        Logger.debug("Successfully killed process on port #{port} using fuser")
        :ok

      {_error, _} ->
        # Fallback to lsof + kill
        cleanup_port_with_lsof(port)
    end
  end

  def cleanup_single_port(_), do: {:error, :invalid_port}

  @doc """
  Kill all test node processes that might be holding ports.
  """
  def cleanup_test_processes do
    Logger.info("Cleaning up test node processes")

    # Kill processes with test_node in the name
    case System.cmd("pkill", ["-f", "test_node"], stderr_to_stdout: true) do
      {_output, 0} ->
        Logger.info("Successfully killed test node processes")
        :ok

      {_output, 1} ->
        # Exit code 1 means no processes found, which is fine
        Logger.debug("No test node processes found to kill")
        :ok

      {error, code} ->
        Logger.warning("Failed to kill test processes (exit #{code}): #{error}")
        {:error, {:pkill_failed, code}}
    end
  end

  @doc """
  Check if the required port range is available.
  """
  def check_port_range_availability(start_port, count) do
    ports = Enum.to_list(start_port..(start_port + count - 1))

    availability =
      ports
      |> Enum.map(fn port -> {port, port_free?(port)} end)
      |> Enum.into(%{})

    unavailable_ports =
      availability
      |> Enum.filter(fn {_port, available} -> not available end)
      |> Enum.map(fn {port, _} -> port end)

    if Enum.empty?(unavailable_ports) do
      {:ok, :all_available}
    else
      {:error, {:ports_unavailable, unavailable_ports}}
    end
  end

  # Private helper functions

  defp cleanup_port_with_lsof(port) do
    case System.cmd("lsof", ["-ti:#{port}"], stderr_to_stdout: true) do
      {pids_output, 0} when pids_output != "" ->
        pids =
          pids_output
          |> String.trim()
          |> String.split("\n")
          |> Enum.reject(&(&1 == ""))

        kill_results = Enum.map(pids, &kill_pid/1)

        if Enum.all?(kill_results, &(&1 == :ok)) do
          Logger.debug("Successfully killed processes #{inspect(pids)} on port #{port}")
          :ok
        else
          Logger.warning("Failed to kill some processes on port #{port}")
          {:error, {:partial_kill, kill_results}}
        end

      {_output, _} ->
        Logger.debug("No processes found on port #{port} or lsof failed")
        :ok
    end
  end

  defp kill_pid(pid_string) do
    case System.cmd("kill", ["-9", pid_string], stderr_to_stdout: true) do
      {_output, 0} -> :ok
      {error, code} -> {:error, {:kill_failed, pid_string, code, error}}
    end
  end

  defp parse_netstat_line(line) do
    # Simple parsing of netstat output to extract useful info
    parts = String.split(line, ~r/\s+/)

    %{
      protocol: Enum.at(parts, 0, "unknown"),
      local_address: Enum.at(parts, 3, "unknown"),
      state: Enum.at(parts, 5, "unknown"),
      process_info: Enum.at(parts, 6, "unknown")
    }
  end
end
