# Design Document

## Overview

This design fixes the critical networking and hostname resolution issues in the mix cluster.test functionality for WSL Ubuntu 24.04 development. The solution focuses on three core problems: hostname resolution, port conflicts, and better error messages.

## Architecture

### Simplified Core Components

```
┌─────────────────────────────────────────────────────────────────┐
│                    Mix.Tasks.Cluster.Test                       │
│                    (Better error messages)                      │
├─────────────────────────────────────────────────────────────────┤
│                 TestCluster.Manager                             │
│                 (Enhanced with hostname + port logic)          │
├─────────────────────────────────────────────────────────────────┤
│  WSL Hostname Fix │  Smart Port Finder │  Simple Diagnostics   │
│  - Try hostname   │  - Find free ports │  - Show what's wrong  │
│  - Fall to 127.0.1│  - Clean conflicts │  - Give clear steps   │
│  - Use system name│  - Track usage     │  - Check EPMD/ports   │
└─────────────────────────────────────────────────────────────────┘
```

### WSL Hostname Resolution

Simple hostname resolution that works in WSL:

```elixir
defmodule OTPSupervisor.TestCluster.HostnameResolver do
  @moduledoc """
  Simple hostname resolution for WSL Ubuntu 24.04
  """
  
  def get_cluster_hostname do
    # Try strategies in order until one works
    strategies = [
      &try_system_hostname/0,
      &try_localhost/0, 
      &try_ip_address/0
    ]
    
    Enum.reduce_while(strategies, {:error, :no_hostname}, fn strategy, _acc ->
      case strategy.() do
        {:ok, hostname} -> {:halt, {:ok, hostname}}
        {:error, _} -> {:cont, {:error, :strategy_failed}}
      end
    end)
  end
  
  defp try_system_hostname do
    case :inet.gethostname() do
      {:ok, hostname} ->
        hostname_str = List.to_string(hostname)
        case :inet.gethostbyname(String.to_charlist(hostname_str)) do
          {:ok, _} -> {:ok, hostname_str}
          {:error, _} -> {:error, :hostname_not_resolvable}
        end
      {:error, reason} -> {:error, reason}
    end
  end
  
  defp try_localhost do
    case :inet.gethostbyname(~c"localhost") do
      {:ok, _} -> {:ok, "localhost"}
      {:error, _} -> {:error, :localhost_failed}
    end
  end
  
  defp try_ip_address do
    case :inet.gethostbyname(~c"127.0.0.1") do
      {:ok, _} -> {:ok, "127.0.0.1"}
      {:error, reason} -> {:error, reason}
    end
  end
end
```

### Smart Port Management

Find available ports and handle conflicts:

```elixir
defmodule OTPSupervisor.TestCluster.PortManager do
  @moduledoc """
  Simple port management for test clusters
  """
  
  def find_available_ports(node_count) do
    http_base = 4100
    dist_base = 9100
    
    case {find_free_ports(http_base, node_count), find_free_ports(dist_base, node_count)} do
      {{:ok, http_ports}, {:ok, dist_ports}} ->
        port_pairs = Enum.zip(http_ports, dist_ports)
        {:ok, port_pairs}
        
      {{:error, _}, _} ->
        {:error, {:no_http_ports, http_base, node_count}}
        
      {_, {:error, _}} ->
        {:error, {:no_dist_ports, dist_base, node_count}}
    end
  end
  
  defp find_free_ports(start_port, count) do
    free_ports = 
      start_port..(start_port + 50)
      |> Enum.filter(&port_free?/1)
      |> Enum.take(count)
    
    if length(free_ports) >= count do
      {:ok, free_ports}
    else
      {:error, {:insufficient_ports, length(free_ports), count}}
    end
  end
  
  defp port_free?(port) do
    case :gen_tcp.listen(port, []) do
      {:ok, socket} -> 
        :gen_tcp.close(socket)
        true
      {:error, _} -> 
        false
    end
  end
  
  def cleanup_ports(port_pairs) do
    # Kill any processes that might be using our test ports
    Enum.each(port_pairs, fn {http_port, dist_port} ->
      System.cmd("fuser", ["-k", "#{http_port}/tcp"], stderr_to_stdout: true)
      System.cmd("fuser", ["-k", "#{dist_port}/tcp"], stderr_to_stdout: true)
    end)
  end
end
```

### Essential Diagnostics Only

Just the diagnostics we actually need for WSL development:

```elixir
defmodule OTPSupervisor.TestCluster.Diagnostics do
  @moduledoc """
  Essential diagnostics for WSL cluster testing
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
  
  defp check_epmd do
    case System.cmd("epmd", ["-names"], stderr_to_stdout: true) do
      {output, 0} when output =~ "up and running" ->
        {"EPMD", :ok}
      _ ->
        {"EPMD", {:error, "Run: epmd -daemon"}}
    end
  end
  
  defp check_hostname_resolution do
    case :inet.gethostbyname(~c"localhost") do
      {:ok, _} -> {"Hostname", :ok}
      {:error, _} -> {"Hostname", {:error, "Cannot resolve localhost"}}
    end
  end
  
  defp check_basic_ports do
    case :gen_tcp.listen(4100, []) do
      {:ok, socket} -> 
        :gen_tcp.close(socket)
        {"Ports", :ok}
      {:error, _} -> 
        {"Ports", {:error, "Port 4100 in use"}}
    end
  end
  
  defp extract_hostname(node_atom) do
    node_atom
    |> Atom.to_string()
    |> String.split("@")
    |> List.last()
  end
end
```

## Components and Interfaces

### Enhanced TestCluster.Manager

Just fix the core issues in the existing manager:

```elixir
defmodule OTPSupervisor.TestCluster.Manager do
  use GenServer
  require Logger
  
  alias OTPSupervisor.TestCluster.{HostnameResolver, PortManager, Diagnostics}
  
  def start_cluster(opts \\ []) do
    GenServer.call(__MODULE__, {:start_cluster, opts}, 60_000)
  end
  
  def handle_call({:start_cluster, opts}, _from, state) do
    Logger.info("Starting test cluster with WSL fixes...")
    
    case start_cluster_fixed(opts) do
      {:ok, cluster_info} ->
        new_state = %{state | nodes: cluster_info.nodes, status: :running}
        {:reply, {:ok, cluster_info.nodes}, new_state}
        
      {:error, reason} ->
        diagnosis = Diagnostics.diagnose_startup_failure(reason)
        Logger.error("Cluster startup failed: #{diagnosis.problem}")
        Enum.each(diagnosis.solutions, &Logger.info("  • #{&1}"))
        
        {:reply, {:error, reason}, state}
    end
  end
  
  defp start_cluster_fixed(opts) do
    node_count = Keyword.get(opts, :node_count, 2)
    
    with {:ok, hostname} <- HostnameResolver.get_cluster_hostname(),
         {:ok, port_pairs} <- PortManager.find_available_ports(node_count),
         {:ok, nodes} <- start_nodes_with_ports(hostname, port_pairs) do
      
      {:ok, %{nodes: nodes, hostname: hostname, port_pairs: port_pairs}}
    else
      {:error, reason} -> {:error, reason}
    end
  end
  
  defp start_nodes_with_ports(hostname, port_pairs) do
    nodes = 
      port_pairs
      |> Enum.with_index()
      |> Enum.map(fn {{http_port, _dist_port}, index} ->
        node_name = :"test_node#{index + 1}@#{hostname}"
        
        case start_phoenix_server_fixed(node_name, http_port, hostname) do
          {:ok, server_info} -> {:"node#{index + 1}", server_info}
          {:error, reason} -> {:error, {node_name, reason}}
        end
      end)
    
    # Check if any failed
    failed = Enum.filter(nodes, &match?({:error, _}, &1))
    
    if Enum.empty?(failed) do
      {:ok, Enum.into(nodes, %{})}
    else
      {:error, {:node_startup_failed, failed}}
    end
  end
  
  defp start_phoenix_server_fixed(node_name, http_port, hostname) do
    Logger.info("Starting node #{node_name} on #{hostname}:#{http_port}")
    
    env = [
      {"PHX_PORT", Integer.to_string(http_port)},
      {"MIX_ENV", "dev"},
      {"PHX_SERVER", "true"}
    ]
    
    cmd_args = [
      "--name", Atom.to_string(node_name),
      "--cookie", "test_cluster_cookie",
      "-S", "mix", "phx.server"
    ]
    
    task = Task.async(fn ->
      System.cmd("elixir", cmd_args, [
        env: env,
        cd: File.cwd!(),
        into: IO.stream(:stdio, :line)
      ])
    end)
    
    # Give it time to start
    :timer.sleep(5000)
    
    # Test connection
    case Node.ping(node_name) do
      :pong ->
        {:ok, %{
          name: node_name,
          http_port: http_port,
          task: task,
          url: "http://#{hostname}:#{http_port}"
        }}
      :pang ->
        Task.shutdown(task, :brutal_kill)
        {:error, {:node_connection_failed, node_name}}
    end
  end
end
```

### Simple Mix Task Updates

Just add better error messages to the existing mix task:

```elixir
# In lib/mix/tasks/cluster/test.ex - just update the error handling
defp start_cluster do
  Mix.shell().info("🚀 Starting distributed test cluster...")
  
  # Run prerequisite check first
  case OTPSupervisor.TestCluster.Diagnostics.check_prerequisites() do
    :ok -> 
      start_cluster_after_checks()
    {:error, failed_checks} ->
      Mix.shell().error("❌ Prerequisites failed:")
      Enum.each(failed_checks, fn {name, {:error, msg}} ->
        Mix.shell().error("  • #{name}: #{msg}")
      end)
      System.halt(1)
  end
end

defp start_cluster_after_checks do
  case Manager.start_cluster() do
    {:ok, nodes} ->
      Mix.shell().info("✅ Test cluster started successfully!")
      show_cluster_info(nodes)
    {:error, reason} ->
      diagnosis = OTPSupervisor.TestCluster.Diagnostics.diagnose_startup_failure(reason)
      Mix.shell().error("❌ #{diagnosis.problem}")
      Mix.shell().info("")
      Mix.shell().info("💡 Try these solutions:")
      Enum.each(diagnosis.solutions, &Mix.shell().info("  • #{&1}"))
      System.halt(1)
  end
end
```

## Data Models

Keep it simple - just what we need:

```elixir
# Node info returned from manager
%{
  name: :"test_node1@localhost",
  http_port: 4100,
  task: #PID<0.123.0>,
  url: "http://localhost:4100"
}

# Diagnostic result
%{
  problem: "Ports [4100, 4101] are in use",
  solutions: ["Run: mix cluster.test clean", "Check: netstat -tulpn | grep 4100"]
}
```

## Error Handling

Focus on the three main error types we see:

1. **Port Conflicts** - Find free ports, show cleanup commands
2. **Hostname Issues** - Try hostname → localhost → 127.0.0.1  
3. **Node Connection** - Check EPMD, show ping commands

## Testing Strategy

Keep existing tests, just make them more reliable:

1. **Fix ClusterTestHelper** - Use the new hostname resolution
2. **Update port handling** - Use dynamic port finding
3. **Better cleanup** - Use the new port cleanup logic

This focused design solves the actual problems without adding unnecessary complexity.