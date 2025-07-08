# OTP Testing Standards

## Overview

This document establishes standards for writing OTP-compliant tests in the OTP Supervisor Educational Tool project. These standards ensure tests are reliable, maintainable, and follow OTP principles.

## Core Principles

### 1. No Process.sleep/1 in Tests

❌ **Wrong:**
```elixir
test "async operation" do
  GenServer.cast(pid, :do_something)
  Process.sleep(100)  # Flaky and un-OTP-like
  assert some_condition()
end
```

✅ **Correct:**
```elixir
test "async operation" do
  GenServer.cast(pid, :do_something)
  # Use synchronous call as synchronization point
  result = GenServer.call(pid, :get_state)
  assert some_condition(result)
end
```

**Why:** GenServer guarantees FIFO message processing. A synchronous call will only be processed after all previous casts are complete.

### 2. Leverage GenServer Message Ordering

GenServer processes messages in strict FIFO order:

```elixir
test "message ordering guarantee" do
  GenServer.cast(pid, :msg1)     # Async
  GenServer.cast(pid, :msg2)     # Async  
  GenServer.cast(pid, :msg3)     # Async
  result = GenServer.call(pid, :get_state)  # Sync - waits for all above
  
  # All casts are guaranteed to be processed before this call returns
  assert result.count == 3
end
```

### 3. Proper Process Monitoring

❌ **Wrong:**
```elixir
test "process crash" do
  kill_process(pid)
  Process.sleep(50)  # Hope it crashed
  refute Process.alive?(pid)
end
```

✅ **Correct:**
```elixir
test "process crash" do
  ref = Process.monitor(pid)
  kill_process(pid)
  
  receive do
    {:DOWN, ^ref, :process, ^pid, _reason} -> :ok
  after
    1000 -> flunk("Process did not terminate")
  end
  
  refute Process.alive?(pid)
end
```

### 4. Supervisor Restart Testing

❌ **Wrong:**
```elixir
test "supervisor restart" do
  original_pid = Process.whereis(:worker)
  Process.exit(original_pid, :kill)
  Process.sleep(100)  # Hope it restarted
  new_pid = Process.whereis(:worker)
  assert new_pid != original_pid
end
```

✅ **Correct:**
```elixir
test "supervisor restart" do
  original_pid = Process.whereis(:worker)
  ref = Process.monitor(original_pid)
  
  Process.exit(original_pid, :kill)
  
  # Wait for crash
  receive do
    {:DOWN, ^ref, :process, ^original_pid, _reason} -> :ok
  after
    1000 -> flunk("Process did not terminate")
  end
  
  # Poll for restart (supervisor may take time)
  new_pid = Enum.reduce_while(1..100, nil, fn _i, _acc ->
    case Process.whereis(:worker) do
      nil -> {:cont, nil}
      pid when pid != original_pid -> {:halt, pid}
      ^original_pid -> {:cont, nil}  # Shouldn't happen
    end
  end)
  
  assert new_pid != original_pid
  assert Process.alive?(new_pid)
end
```

### 5. Concurrent Operations Testing

Use Task.async/await for controlled concurrency:

```elixir
test "concurrent operations" do
  tasks = for i <- 1..10 do
    Task.async(fn ->
      for j <- 1..50 do
        GenServer.cast(pid, {:increment, i, j})
      end
    end)
  end
  
  # Wait for all tasks to complete
  Enum.each(tasks, &Task.await/1)
  
  # Use sync call to ensure all casts processed
  final_count = GenServer.call(pid, :get_count)
  assert final_count == 500  # 10 tasks * 50 increments
end
```

### 6. Error Testing

Test both success and failure paths:

```elixir
test "handles invalid input" do
  # Test that invalid input returns proper error
  assert {:error, :invalid_input} = MyGenServer.do_something(pid, :invalid)
  
  # Verify process is still alive and functional
  assert {:ok, result} = MyGenServer.do_something(pid, :valid)
  assert result == :expected_value
end
```

### 7. State Verification

Always verify state through the public API:

❌ **Wrong:**
```elixir
test "internal state" do
  state = :sys.get_state(pid)  # Accessing internals
  assert state.counter == 5
end
```

✅ **Correct:**
```elixir
test "state management" do
  result = MyGenServer.get_counter(pid)  # Public API
  assert result == 5
end
```

## Common Patterns

### Setup and Cleanup

```elixir
defmodule MyWorkerTest do
  use ExUnit.Case, async: true
  
  setup do
    {:ok, pid} = MyWorker.start_link(name: :test_worker)
    
    on_exit(fn ->
      if Process.alive?(pid) do
        GenServer.stop(pid)
      end
    end)
    
    {:ok, pid: pid}
  end
  
  test "worker functionality", %{pid: pid} do
    # Test using pid from setup
  end
end
```

### Helper Functions

Create reusable, OTP-compliant helpers:

```elixir
defp send_messages_and_verify(pid, count) do
  for i <- 1..count do
    GenServer.cast(pid, {:message, i})
  end
  
  # Sync call ensures all casts processed
  final_count = GenServer.call(pid, :get_message_count)
  assert final_count == count
  final_count
end

defp wait_for_process_registration(name, timeout \\ 1000) do
  start_time = System.monotonic_time(:millisecond)
  
  Enum.reduce_while(Stream.cycle([1]), nil, fn _i, _acc ->
    if System.monotonic_time(:millisecond) - start_time > timeout do
      {:halt, nil}
    else
      case Process.whereis(name) do
        nil -> {:cont, nil}
        pid -> {:halt, pid}
      end
    end
  end)
end
```

## Testing Anti-Patterns

### ❌ Avoid These

1. **Timing Dependencies**: Never rely on sleep or timing
2. **Internal State Access**: Don't use `:sys.get_state/1` in tests
3. **Process Polling Without Timeout**: Always have escape conditions
4. **Race Conditions**: Don't assume async operations complete immediately
5. **Hardcoded PIDs**: Always get PIDs dynamically
6. **Ignoring Process Links**: Be aware of process relationships in tests

### ✅ Good Practices

1. **Deterministic Tests**: Tests should pass consistently
2. **Public API Only**: Test through the public interface
3. **Proper Error Handling**: Test both success and failure cases
4. **Resource Cleanup**: Always clean up processes
5. **Educational Value**: Tests should demonstrate OTP concepts
6. **Clear Assertions**: Make test intentions obvious

## Example: Complete OTP-Compliant Test

```elixir
defmodule MyWorkerTest do
  use ExUnit.Case, async: true
  
  alias MyApp.MyWorker
  
  describe "basic functionality" do
    test "worker lifecycle" do
      # Start worker
      {:ok, pid} = MyWorker.start_link(name: :test_worker)
      assert Process.alive?(pid)
      assert Process.whereis(:test_worker) == pid
      
      # Test functionality using public API
      assert :ok = MyWorker.do_work(:test_worker, :some_task)
      assert {:ok, result} = MyWorker.get_result(:test_worker)
      assert result == :expected_value
      
      # Test error handling
      assert {:error, :invalid} = MyWorker.do_work(:test_worker, :invalid_task)
      
      # Verify worker still functional after error
      assert {:ok, _} = MyWorker.get_result(:test_worker)
      
      # Clean shutdown
      assert :ok = GenServer.stop(pid)
      refute Process.alive?(pid)
    end
  end
  
  describe "supervision integration" do
    test "restart behavior" do
      children = [{MyWorker, name: :supervised_worker}]
      {:ok, sup_pid} = Supervisor.start_link(children, strategy: :one_for_one)
      
      original_pid = Process.whereis(:supervised_worker)
      ref = Process.monitor(original_pid)
      
      # Cause crash
      Process.exit(original_pid, :kill)
      
      # Wait for crash
      receive do
        {:DOWN, ^ref, :process, ^original_pid, :killed} -> :ok
      after
        1000 -> flunk("Process did not crash")
      end
      
      # Wait for restart
      new_pid = wait_for_process_registration(:supervised_worker)
      assert new_pid != original_pid
      assert Process.alive?(new_pid)
      
      # Verify fresh state
      assert {:ok, initial_state} = MyWorker.get_result(:supervised_worker)
      assert initial_state == :initial_value
      
      # Cleanup
      Supervisor.stop(sup_pid)
    end
  end
end
```

## Key Takeaways

1. **Trust OTP**: Use OTP guarantees instead of timing
2. **Synchronous Calls**: Use them as synchronization points
3. **Monitor Everything**: Use proper process monitoring
4. **Test Real Scenarios**: Test how the system actually behaves
5. **Stay Deterministic**: Tests should never be flaky
6. **Educational First**: Tests should teach OTP concepts

Following these standards ensures reliable, maintainable tests that demonstrate proper OTP patterns and serve as educational examples.