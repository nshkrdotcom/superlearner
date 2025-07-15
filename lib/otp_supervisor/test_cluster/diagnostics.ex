defmodule OTPSupervisor.TestCluster.Diagnostics do
  @moduledoc """
  Essential diagnostics for WSL cluster testing.

  Provides error analysis and prerequisite checking for distributed test clusters,
  with specific focus on common WSL Ubuntu 24.04 issues.
  """

  require Logger

  @doc """
  Diagnoses startup failures and provides specific solutions.

  Takes an error reason and returns a map with problem description and actionable solutions.
  """
  def diagnose_startup_failure(error) do
    case error do
      {:ports_unavailable, ports} ->
        %{
          problem: "Ports #{inspect(ports)} are in use",
          solutions: [
            "Run: mix cluster.test clean",
            "Check: netstat -tulpn | grep #{Enum.join(ports, "\\|")}",
            "Kill: pkill -f test_node"
          ]
        }

      {:hostname_resolution_failed, _} ->
        %{
          problem: "Cannot resolve hostname for cluster nodes",
          solutions: [
            "Check: ping localhost",
            "Check: ping 127.0.0.1",
            "Try: sudo systemctl restart systemd-resolved"
          ]
        }

      {:node_connection_failed, node} ->
        %{
          problem: "Cannot connect to node #{node}",
          solutions: [
            "Check: epmd -names",
            "Start: epmd -daemon",
            "Check: ping #{extract_hostname(node)}"
          ]
        }

      {:insufficient_ports, available, needed} ->
        %{
          problem: "Only #{available} ports available, need #{needed}",
          solutions: [
            "Run: mix cluster.test clean",
            "Kill: pkill -f beam.smp",
            "Check: netstat -tulpn | grep :41"
          ]
        }

      {:node_startup_failed, failed_nodes} ->
        node_names =
          Enum.map(failed_nodes, fn
            {:error, {node, _reason}} -> node
            {_node_key, {:error, {_error_type, node}}} -> node
            {node_key, _} -> node_key
          end)

        %{
          problem: "Failed to start nodes: #{inspect(node_names)}",
          solutions: [
            "Check: epmd -names",
            "Run: mix cluster.test clean",
            "Verify: mix deps.get && mix compile"
          ]
        }

      _ ->
        %{
          problem: "Unknown cluster startup error: #{inspect(error)}",
          solutions: [
            "Run: mix cluster.test clean",
            "Check: epmd -names",
            "Restart: sudo systemctl restart systemd-resolved"
          ]
        }
    end
  end

  @doc """
  Checks system prerequisites for cluster testing.

  Returns :ok if all checks pass, or {:error, failed_checks} with details.
  """
  def check_prerequisites do
    checks = [
      check_epmd(),
      check_hostname_resolution(),
      check_basic_ports()
    ]

    failed_checks = Enum.filter(checks, fn {_name, result} -> result != :ok end)

    if Enum.empty?(failed_checks) do
      :ok
    else
      {:error, failed_checks}
    end
  end

  @doc """
  Runs comprehensive environment diagnostics and returns a report.
  """
  def environment_report do
    %{
      hostname: get_hostname_info(),
      epmd: get_epmd_info(),
      ports: get_port_info(),
      network: get_network_info(),
      system: get_system_info()
    }
  end

  # Private functions for prerequisite checks

  defp check_epmd do
    case System.cmd("epmd", ["-names"], stderr_to_stdout: true) do
      {output, 0} ->
        if String.contains?(output, "up and running") do
          {"EPMD", :ok}
        else
          {"EPMD", {:warning, "EPMD running but no registered names: #{String.trim(output)}"}}
        end

      {_output, _code} ->
        {"EPMD", {:error, "Run: epmd -daemon"}}
    end
  rescue
    _ ->
      {"EPMD", {:error, "EPMD command not found - Erlang not properly installed"}}
  end

  defp check_hostname_resolution do
    case :inet.gethostbyname(~c"localhost") do
      {:ok, _} ->
        {"Hostname", :ok}

      {:error, reason} ->
        {"Hostname", {:error, "Cannot resolve localhost: #{reason}"}}
    end
  end

  defp check_basic_ports do
    test_ports = [4100, 4101, 9100, 9101]

    case check_ports_available(test_ports) do
      [] ->
        {"Ports", :ok}

      busy_ports ->
        {"Ports", {:error, "Ports in use: #{inspect(busy_ports)}"}}
    end
  end

  defp check_ports_available(ports) do
    Enum.filter(ports, fn port ->
      case :gen_tcp.listen(port, []) do
        {:ok, socket} ->
          :gen_tcp.close(socket)
          false

        {:error, _} ->
          true
      end
    end)
  end

  # Private functions for detailed diagnostics

  defp get_hostname_info do
    %{
      system_hostname: get_system_hostname(),
      localhost_resolution: test_hostname_resolution("localhost"),
      ip_resolution: test_hostname_resolution("127.0.0.1")
    }
  end

  defp get_system_hostname do
    case :inet.gethostname() do
      {:ok, hostname} -> List.to_string(hostname)
      {:error, reason} -> {:error, reason}
    end
  end

  defp test_hostname_resolution(hostname) do
    case :inet.gethostbyname(String.to_charlist(hostname)) do
      {:ok, hostent} ->
        # hostent is a record: {hostent, name, aliases, addrtype, length, addr_list}
        name = elem(hostent, 1) |> List.to_string()
        addresses = elem(hostent, 5) |> Enum.map(&:inet.ntoa/1) |> Enum.map(&List.to_string/1)

        {:ok,
         %{
           name: name,
           addresses: addresses
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp get_epmd_info do
    case System.cmd("epmd", ["-names"], stderr_to_stdout: true) do
      {output, 0} ->
        {:ok, String.trim(output)}

      {output, code} ->
        {:error, "Exit code #{code}: #{String.trim(output)}"}
    end
  rescue
    _ ->
      {:error, "EPMD command not available"}
  end

  defp get_port_info do
    test_ports = 4100..4110 |> Enum.to_list()

    %{
      test_range: test_ports,
      available: Enum.filter(test_ports, &port_available?/1),
      busy: Enum.filter(test_ports, &(!port_available?(&1)))
    }
  end

  defp port_available?(port) do
    case :gen_tcp.listen(port, []) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        true

      {:error, _} ->
        false
    end
  end

  defp get_network_info do
    %{
      interfaces: get_network_interfaces(),
      connectivity: test_network_connectivity()
    }
  end

  defp get_network_interfaces do
    case :inet.getifaddrs() do
      {:ok, interfaces} ->
        interfaces
        |> Enum.map(fn {name, opts} ->
          {List.to_string(name), extract_interface_info(opts)}
        end)
        |> Enum.into(%{})

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp extract_interface_info(opts) do
    opts
    |> Enum.filter(fn {key, _} -> key in [:addr, :flags] end)
    |> Enum.map(fn
      {:addr, addr} -> {:addr, :inet.ntoa(addr) |> List.to_string()}
      {key, value} -> {key, value}
    end)
    |> Enum.into(%{})
  end

  defp test_network_connectivity do
    %{
      localhost: test_ping("localhost"),
      loopback: test_ping("127.0.0.1")
    }
  end

  defp test_ping(host) do
    case System.cmd("ping", ["-c", "1", "-W", "1", host], stderr_to_stdout: true) do
      {_output, 0} -> :ok
      {output, _code} -> {:error, String.trim(output)}
    end
  rescue
    _ ->
      {:error, "ping command not available"}
  end

  defp get_system_info do
    %{
      os: get_os_info(),
      erlang_version: System.version(),
      elixir_version: System.version(),
      node_name: Node.self()
    }
  end

  defp get_os_info do
    case :os.type() do
      {:unix, :linux} ->
        case System.cmd("uname", ["-a"], stderr_to_stdout: true) do
          {output, 0} -> {:linux, String.trim(output)}
          _ -> {:linux, :unknown}
        end

      {family, name} ->
        {family, name}
    end
  rescue
    _ ->
      :unknown
  end

  defp extract_hostname(node_atom) when is_atom(node_atom) do
    node_atom
    |> Atom.to_string()
    |> String.split("@")
    |> List.last()
  end

  defp extract_hostname(node_string) when is_binary(node_string) do
    node_string
    |> String.split("@")
    |> List.last()
  end
end
