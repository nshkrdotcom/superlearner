# BEAM Debugging Solutions for Cluster Issues

## Overview

This document provides BEAM-specific debugging techniques and logging strategies that Erlang engineers use to diagnose and prevent system hangs in distributed Elixir/Phoenix applications.

## BEAM Debugging Tools and Techniques

### 1. Runtime Debugging with Erlang Tools

#### a) Process Information Gathering
```elixir
# Get detailed process info when hang suspected
:erlang.process_info(pid, [:current_stacktrace, :messages, :message_queue_len, :status])

# List all processes with high message queues
Process.list()
|> Enum.map(fn pid -> 
  {pid, Process.info(pid, :message_queue_len)} 
end)
|> Enum.filter(fn {_, {:message_queue_len, len}} -> len > 1000 end)
```

#### b) System Monitor Setup
```elixir
# Monitor for long garbage collections and busy processes
:erlang.system_monitor(self(), [{:long_gc, 500}, {:busy_port, true}, {:busy_dist_port, true}])
```

#### c) Trace Busy Processes
```elixir
# Trace specific function calls
:erlang.trace_pattern({:io, :format, :_}, [{:_, [], [{:return_trace}]}], [:local])
:erlang.trace(:all, true, [:call])
```

### 2. Crash Dumps and Analysis

#### Enable Detailed Crash Dumps
```bash
# Set environment variables before starting
export ERL_CRASH_DUMP_SECONDS=30
export ERL_CRASH_DUMP_BYTES=100000000
export ERL_CRASH_DUMP=/path/to/cluster_crash.dump
```

#### Analyze Crash Dumps
```bash
# Using OTP's crashdump_viewer
erl -eval 'crashdump_viewer:start().' -run init stop

# Or use recon for live analysis
# Add to mix.exs: {:recon, "~> 2.5"}
```

### 3. SASL and Error Logger Configuration

#### Enhanced SASL Configuration
```elixir
# In config/config.exs
config :logger,
  handle_otp_reports: true,
  handle_sasl_reports: true  # Enable for debugging

config :sasl,
  sasl_error_logger: {:file, 'log/sasl.log'},
  errlog_type: :error,
  error_logger_format_depth: 100

# For distributed debugging
config :kernel,
  logger_sasl_compatible: true,
  error_logger_format_depth: 100
```

#### Custom Logger Backend for Cluster Issues
```elixir
defmodule ClusterDebugLogger do
  @behaviour :gen_event

  def init(_), do: {:ok, %{}}

  def handle_event({:error_report, _, {_, :std_error, report}}, state) do
    # Log cluster-specific errors with context
    if cluster_related?(report) do
      File.write!("cluster_errors.log", inspect(report, pretty: true) <> "\n", [:append])
    end
    {:ok, state}
  end

  defp cluster_related?(report) do
    String.contains?(inspect(report), ["test_node", "cluster", "rpc"])
  end
end
```

### 4. Distributed Erlang Debugging

#### Monitor Distribution Connections
```elixir
# Check distribution status
:net_kernel.monitor_nodes(true, [node_type: :all])

# Get detailed connection info
:net_adm.diagnostics(1)

# Check distribution buffers
:erlang.dist_ctrl_get_data_notification(:erlang.dist_ctrl_get_cid(node))
```

#### Debug Distribution Issues
```bash
# Start with debug distribution
elixir --name debug@localhost --cookie test_cluster_cookie \
  --erl "-kernel net_ticktime 60" \
  --erl "-kernel dist_nodelay true" \
  -e "IO.inspect Node.connect(:'test_node1@localhost')"
```

### 5. BEAM VM Flags for Debugging

#### Start Nodes with Debug Flags
```elixir
# In manager.ex, add to cmd_args:
debug_vm_args = [
  "+P", "10000000",           # Increase process limit
  "+Q", "1000000",            # Increase port limit
  "+K", "true",               # Enable kernel poll
  "+stbt", "db",              # Detailed scheduler bind info
  "+zdbbl", "32768",          # Distribution buffer busy limit
  "-kernel", "error_logger_format_depth", "100"
]
```

### 6. Live System Analysis

#### Using :observer
```elixir
# Start observer on running node
:observer.start()

# For remote nodes
:observer.start(node_name)
```

#### Using recon Library
```elixir
# Add to dependencies
{:recon, "~> 2.5"}

# Find processes with large message queues
:recon.proc_count(:message_queue_len, 10)

# Find processes using most memory
:recon.proc_count(:memory, 10)

# Get system info
:recon.info(self(), [:messages, :dictionary])
```

### 7. Custom Monitoring for Cluster Issues

#### Process Registry Monitor
```elixir
defmodule ClusterProcessMonitor do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(_) do
    :timer.send_interval(5000, :check_health)
    {:ok, %{}}
  end

  def handle_info(:check_health, state) do
    # Check for hung processes
    suspicious = Process.list()
    |> Enum.filter(fn pid ->
      case Process.info(pid, [:message_queue_len, :current_function]) do
        [{:message_queue_len, len}, {:current_function, {mod, _, _}}] ->
          len > 10000 or hanging_function?(mod)
        _ -> false
      end
    end)

    if length(suspicious) > 0 do
      Logger.error("Suspicious processes found: #{inspect(suspicious)}")
      dump_process_info(suspicious)
    end

    {:noreply, state}
  end

  defp hanging_function?(module) do
    module in [IO, :io, :file, :gen_tcp]
  end

  defp dump_process_info(pids) do
    Enum.each(pids, fn pid ->
      info = Process.info(pid, [
        :current_stacktrace, 
        :message_queue_len,
        :messages,
        :links,
        :monitors
      ])
      File.write!("hung_processes.log", inspect(info, pretty: true) <> "\n\n", [:append])
    end)
  end
end
```

### 8. Preventing Hangs - Best Practices

#### Timeout Everything
```elixir
# GenServer calls with timeout
GenServer.call(server, msg, 5000)

# Task with timeout
task = Task.async(fn -> expensive_operation() end)
case Task.yield(task, 5000) || Task.shutdown(task) do
  {:ok, result} -> result
  nil -> {:error, :timeout}
end
```

#### Use spawn_monitor for External Commands
```elixir
# Better than System.cmd for long-running processes
def start_node_improved(cmd_args, env) do
  port = Port.open({:spawn_executable, System.find_executable("elixir")}, [
    :binary,
    :exit_status,
    {:args, cmd_args},
    {:env, Enum.map(env, fn {k, v} -> {String.to_charlist(k), String.to_charlist(v)} end)},
    {:line, 1024}  # Line-buffered with max line size
  ])
  
  # Monitor the port
  ref = Port.monitor(port)
  
  # Set up bounded message handling
  spawn_link(fn -> 
    handle_port_messages(port, ref, [])
  end)
  
  {:ok, port}
end

defp handle_port_messages(port, ref, buffer) do
  receive do
    {^port, {:data, {:eol, line}}} ->
      # Handle line, maybe with buffer limits
      new_buffer = add_to_bounded_buffer(buffer, line, 1000)
      handle_port_messages(port, ref, new_buffer)
      
    {^port, {:exit_status, status}} ->
      Logger.info("Process exited with status: #{status}")
      
    {:DOWN, ^ref, :port, ^port, reason} ->
      Logger.error("Port died: #{inspect(reason)}")
  after
    30_000 ->
      Logger.error("Port communication timeout")
      Port.close(port)
  end
end
```

### 9. Emergency Recovery Procedures

#### When System is Hung
```bash
# 1. Get BEAM process info
kill -USR1 <beam_pid>  # Dumps current state to erl_crash.dump

# 2. Check what's blocking
strace -p <beam_pid> -f

# 3. Check file descriptors
lsof -p <beam_pid>

# 4. Force garbage collection (if accessible)
# Remote shell: erl -remsh nodename@host
:erlang.garbage_collect()

# 5. Kill specific processes
# In remote shell
Process.list() |> Enum.each(&Process.exit(&1, :kill))
```

### 10. Logging Best Practices for Clusters

#### Structured Logging
```elixir
# Use metadata for cluster context
Logger.metadata(node: Node.self(), cluster_operation: :startup)

# Log with context
Logger.info("Starting cluster node", 
  node_name: config.name,
  http_port: config.http_port,
  parent_task: inspect(self())
)
```

#### Log Aggregation
```elixir
# Configure remote syslog
config :logger, 
  backends: [:console, {LoggerSyslogBackend, :syslog}]

config :logger_syslog_backend, :syslog,
  host: "central-log-server.local",
  port: 514,
  facility: :local0,
  appid: "cluster_node"
```

## Conclusion

The key to preventing and debugging cluster hangs is:
1. Comprehensive monitoring and logging
2. Proper timeout configuration
3. Using appropriate BEAM primitives (Port vs System.cmd)
4. Understanding BEAM scheduling and message passing
5. Having emergency recovery procedures ready

These tools and techniques allow you to identify bottlenecks, trace hanging processes, and implement robust error recovery in distributed Elixir systems.