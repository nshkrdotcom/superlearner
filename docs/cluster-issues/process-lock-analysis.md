# Cluster Process Lock Issue Analysis

## Executive Summary

When running `mix cluster.test start`, the system experiences process locks and hangs when errors occur, particularly related to Phoenix LiveView pages. This analysis identifies the root causes and provides solutions.

## Root Causes Identified

### 1. System.cmd with IO.stream Deadlock

**Location**: `lib/otp_supervisor/test_cluster/manager.ex:225-231`

```elixir
task = Task.async(fn ->
  System.cmd("elixir", cmd_args,
    env: env,
    cd: File.cwd!(),
    into: IO.stream(:stdio, :line)
  )
end)
```

**Problem**: 
- `IO.stream(:stdio, :line)` creates a blocking stream that waits for lines from the spawned process
- If the spawned elixir process produces continuous output or gets stuck, the Task blocks indefinitely
- No timeout mechanism exists on the stream itself
- Buffer overflow can occur if output is produced faster than consumed

**Why This Causes Hangs**:
1. The spawned elixir process runs with `:timer.sleep(:infinity)` 
2. Any error output or unexpected logging can fill the pipe buffer
3. When the buffer is full, the child process blocks on write
4. The parent Task blocks on read, creating a deadlock

### 2. Inadequate Process Cleanup

**Issue**: `Task.shutdown(task, :brutal_kill)` doesn't guarantee OS process termination

**Location**: `lib/otp_supervisor/test_cluster/manager.ex:253`

The Task is killed, but the underlying OS process (elixir) continues running because:
- `System.cmd` spawns a separate OS process
- Killing the Task only stops the Erlang process monitoring it
- The OS process becomes orphaned

### 3. LiveView Error Cascades

**Observed Pattern**:
1. LiveView page encounters an error (e.g., in `supervisor_live.ex`)
2. Error handling tries to log extensively
3. Logs are sent to stdout/stderr
4. The pipe buffer between parent and child process fills up
5. Child process blocks trying to write more logs
6. Parent Task blocks trying to read from the pipe
7. System appears hung

### 4. No Timeout on Long-Running Operations

Several operations lack proper timeouts:
- RPC calls use fixed 5-second timeouts (may be too long)
- No overall timeout on cluster startup
- LiveView refresh loops continue even during errors

## Specific Vulnerabilities

### In TestCluster.Manager

1. **Synchronous IO Handling**: 
   - Line 229: `into: IO.stream(:stdio, :line)` has no backpressure control
   - No mechanism to drop output if buffer fills

2. **Incomplete Error Recovery**:
   - `cleanup_failed_nodes` only calls `PortManager.cleanup_test_processes()`
   - Doesn't track actual OS PIDs for reliable cleanup

3. **Race Conditions**:
   - `wait_for_node_connection` might succeed before process fully initializes
   - Phoenix startup errors after connection check won't be caught

### In Mix Task (cluster.test)

1. **Force Stop Limitations**:
   - `pkill -f` patterns might miss processes with altered command lines
   - No verification that processes actually terminated

2. **Port Cleanup Issues**:
   - `lsof` and `kill -9` approach is platform-specific
   - May leave sockets in TIME_WAIT state

## Why Mix-Based Startup Differs from Manual Scripts

1. **Process Hierarchy**: 
   - Mix task creates deeper process tree
   - More intermediate processes that can orphan children

2. **Output Handling**:
   - Manual scripts likely use different output redirection
   - Mix captures and processes output differently

3. **Error Propagation**:
   - Mix task error handling might prevent proper cleanup
   - Signal handling differs between interactive and Mix contexts

## Recommendations

### Immediate Fixes

1. **Replace System.cmd with Port**:
```elixir
# Instead of System.cmd with IO.stream
port = Port.open({:spawn_executable, System.find_executable("elixir")},
  [:binary, :exit_status, args: cmd_args, env: env])
```

2. **Add Output Buffer Management**:
```elixir
# Implement a bounded buffer that drops old messages
defmodule BoundedBuffer do
  def start_link(max_size) do
    # Implementation with size limits
  end
end
```

3. **Track OS PIDs Directly**:
```elixir
# Get PID from spawned process
{:os_pid, os_pid} = Port.info(port, :os_pid)
# Store for reliable cleanup
```

### Long-term Solutions

1. **Use Erlang Distribution Instead of OS Processes**:
   - Start nodes using `:slave` or `:peer` modules
   - Better integration with BEAM supervision

2. **Implement Circuit Breakers**:
   - Add circuit breakers for cluster operations
   - Fail fast when errors detected

3. **Add Comprehensive Health Monitoring**:
   - Monitor process memory/CPU usage
   - Detect hung processes proactively

4. **Improve Error Boundaries**:
   - Add try-catch blocks around all LiveView operations
   - Implement error recovery strategies

## Testing the Fix

1. **Reproduce the Issue**:
   ```bash
   # Introduce an error in supervisor_live.ex
   # Run: mix cluster.test start
   # Observe the hang
   ```

2. **Verify Fix**:
   ```bash
   # Apply recommended changes
   # Run same error scenario
   # Ensure proper cleanup and no hangs
   ```

## Conclusion

The primary issue is the use of `System.cmd` with `IO.stream` for long-running processes, combined with inadequate process cleanup. When Phoenix LiveView errors produce excessive output, the pipe buffer fills and creates a deadlock. The fix requires better process management and output handling strategies.