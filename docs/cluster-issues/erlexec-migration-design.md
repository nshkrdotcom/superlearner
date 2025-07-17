# Erlexec Migration Design for Cluster Process Management

## Overview

This document outlines the design changes needed to migrate from `System.cmd` with `IO.stream` to `erlexec` for managing cluster node processes. This addresses the process lock issues identified in the previous analysis.

## Why Erlexec?

Erlexec provides significant advantages over `System.cmd`:

1. **Proper Process Management**: Direct OS process control with PIDs
2. **Non-blocking I/O**: Asynchronous stdout/stderr handling
3. **Process Monitoring**: Built-in process monitoring and linking
4. **Signal Handling**: Fine-grained control over process termination
5. **Resource Cleanup**: Automatic cleanup of child processes

## Design Changes

### 1. Add Erlexec Dependency

**In `mix.exs`**:
```elixir
defp deps do
  [
    # ... existing deps
    {:erlexec, "~> 2.0"}
  ]
end
```

### 2. Update Application Configuration

**In `lib/otp_supervisor/application.ex`**:
```elixir
def start(_type, _args) do
  children = [
    # Start erlexec before other supervisors
    :exec,
    # ... rest of children
  ]
  
  opts = [strategy: :one_for_one, name: OTPSupervisor.Supervisor]
  Supervisor.start_link(children, opts)
end
```

### 3. Refactor TestCluster.Manager

Replace the problematic `start_phoenix_server/1` function:

```elixir
defp start_phoenix_server(config) do
  Logger.info("Starting cluster node #{config.name} on #{config.hostname}:#{config.http_port}")

  # Environment setup
  env = [
    {"PHX_PORT", Integer.to_string(config.http_port)},
    {"PORT", Integer.to_string(config.http_port)},
    {"MIX_ENV", "dev"},
    {"NODE_NAME", Atom.to_string(config.name)},
    {"PHX_SERVER", "true"},
    {"ERLANG_COOKIE", Atom.to_string(config.cookie)},
    {"TEST_HTTP_PORT", Integer.to_string(config.http_port)}
  ]

  # Build command arguments
  cmd = "elixir"
  args = [
    "--name", Atom.to_string(config.name),
    "--cookie", Atom.to_string(config.cookie),
    "-S", "mix", "run", "--no-halt"
  ]

  # Start with erlexec
  case :exec.run([cmd | args], [
    {:env, env},
    {:cd, File.cwd!()},
    :monitor,
    :stdout,
    :stderr,
    {:stdout, {:send, self()}},
    {:stderr, {:send, self()}},
    {:kill_timeout, 5}  # 5 seconds before SIGKILL
  ]) do
    {:ok, pid, os_pid} ->
      # Store process info
      server_info = %{
        name: config.name,
        http_port: config.http_port,
        exec_pid: pid,        # Erlang process monitoring the OS process
        os_pid: os_pid,       # Actual OS process ID
        status: :running,
        url: "http://#{config.hostname}:#{config.http_port}",
        hostname: config.hostname,
        output_buffer: :queue.new(),  # Bounded buffer for output
        max_buffer_size: 1000  # Keep last 1000 lines
      }

      # Start output handler
      {:ok, handler_pid} = start_output_handler(server_info)
      
      server_info = Map.put(server_info, :output_handler, handler_pid)
      
      # Wait for node connection
      case wait_for_node_connection(config.name, 30, 1000) do
        :ok ->
          Logger.info("✅ Cluster node #{config.name} started successfully")
          {:ok, server_info}

        {:error, reason} ->
          Logger.error("Failed to connect to cluster node #{config.name}: #{reason}")
          :exec.stop(os_pid)
          {:error, {:node_connection_failed, config.name}}
      end

    {:error, reason} ->
      Logger.error("Failed to start cluster node #{config.name}: #{inspect(reason)}")
      {:error, reason}
  end
end
```

### 4. Implement Output Handler

```elixir
defp start_output_handler(server_info) do
  Task.start_link(fn ->
    handle_process_output(server_info)
  end)
end

defp handle_process_output(server_info) do
  receive do
    {:stdout, os_pid, data} when os_pid == server_info.os_pid ->
      # Handle stdout with bounded buffer
      handle_output_data(server_info, :stdout, data)
      handle_process_output(server_info)

    {:stderr, os_pid, data} when os_pid == server_info.os_pid ->
      # Handle stderr with bounded buffer
      handle_output_data(server_info, :stderr, data)
      handle_process_output(server_info)

    {:DOWN, os_pid, :process, _pid, reason} when os_pid == server_info.os_pid ->
      Logger.info("Cluster node #{server_info.name} exited: #{inspect(reason)}")
      # Cleanup handled by erlexec

    _ ->
      handle_process_output(server_info)
  end
end

defp handle_output_data(server_info, stream, data) do
  # Log with rate limiting
  lines = String.split(data, "\n", trim: true)
  
  Enum.each(lines, fn line ->
    Logger.debug("[#{server_info.name}:#{stream}] #{line}")
  end)
  
  # Optional: Store in bounded buffer for debugging
  # update_output_buffer(server_info, stream, lines)
end
```

### 5. Update Stop Functions

```elixir
defp stop_node(server_info) do
  Logger.info("Stopping node #{server_info.name} (OS PID: #{server_info.os_pid})")
  
  # Use erlexec to stop the process gracefully
  case :exec.stop(server_info.os_pid) do
    :ok ->
      Logger.info("Node #{server_info.name} stopped successfully")
      :ok
      
    {:error, :no_process} ->
      # Process already dead
      Logger.info("Node #{server_info.name} was already stopped")
      :ok
      
    {:error, reason} ->
      Logger.error("Failed to stop node #{server_info.name}: #{inspect(reason)}")
      # Try force kill
      :exec.kill(server_info.os_pid, 9)
      :ok
  end
end
```

### 6. Enhanced Monitoring

```elixir
defp monitor_cluster_health(state) do
  # Check all managed processes
  Enum.each(state.nodes, fn {name, node_info} ->
    case :exec.status(node_info.os_pid) do
      {:status, status} ->
        # Process is still running
        if status != node_info.last_status do
          Logger.info("Node #{name} status changed: #{inspect(status)}")
          # Update state
        end
        
      {:error, :no_process} ->
        # Process died unexpectedly
        Logger.error("Node #{name} died unexpectedly!")
        # Handle cleanup and potential restart
    end
  end)
end
```

### 7. Configuration Options

Add erlexec-specific configuration:

```elixir
# In config/config.exs
config :erlexec,
  # Limit max number of child processes
  max_children: 100,
  # Enable debug logging
  debug: false,
  # Custom port program path (if needed)
  # port_path: "/path/to/exec-port"
```

### 8. Error Recovery

Implement better error recovery with erlexec:

```elixir
defp handle_node_crash(node_name, exit_reason, state) do
  case exit_reason do
    {:exit_status, 0} ->
      # Normal exit
      Logger.info("Node #{node_name} exited normally")
      
    {:exit_status, code} ->
      # Abnormal exit
      Logger.error("Node #{node_name} crashed with exit code: #{code}")
      maybe_restart_node(node_name, state)
      
    {:signal, signal, _core_dump} ->
      # Killed by signal
      Logger.error("Node #{node_name} killed by signal: #{signal}")
      maybe_restart_node(node_name, state)
  end
end

defp maybe_restart_node(node_name, state) do
  node_info = Map.get(state.nodes, node_name)
  
  if node_info && node_info.restart_count < 3 do
    Logger.info("Attempting to restart node #{node_name}")
    # Implement restart logic with backoff
  else
    Logger.error("Node #{node_name} exceeded restart limit")
  end
end
```

## Migration Steps

1. **Add erlexec dependency** to mix.exs
2. **Start :exec application** in your supervision tree
3. **Create feature branch** for migration
4. **Update TestCluster.Manager** with new implementation
5. **Test with single node** first
6. **Test with full cluster** including error scenarios
7. **Update documentation** and tests

## Benefits of This Design

1. **No More Deadlocks**: Asynchronous I/O prevents buffer overflow
2. **Better Process Control**: Direct OS process management
3. **Proper Cleanup**: Automatic child process termination
4. **Enhanced Monitoring**: Real-time process status
5. **Graceful Shutdown**: Configurable termination timeouts
6. **Error Recovery**: Built-in restart capabilities

## Testing the Migration

```elixir
# Test graceful shutdown
test "cluster nodes stop cleanly" do
  {:ok, nodes} = Manager.start_cluster(node_count: 2)
  assert :ok = Manager.stop_cluster()
  
  # Verify no orphaned processes
  :timer.sleep(1000)
  assert [] = :exec.which_processes()
end

# Test error handling
test "handles node crash gracefully" do
  {:ok, nodes} = Manager.start_cluster(node_count: 1)
  [{_, node_info}] = nodes
  
  # Kill the process
  :exec.kill(node_info.os_pid, 9)
  
  # Should detect and handle crash
  assert_receive {:node_crashed, _}, 5000
end
```

## Conclusion

Migrating to erlexec solves the fundamental issues with `System.cmd` and provides a robust foundation for cluster process management. The bounded output handling, proper process monitoring, and graceful shutdown capabilities ensure the system remains stable even under error conditions.