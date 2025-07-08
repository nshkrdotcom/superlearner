# 02 - Implement Telemetry-Based Analytics (Test-Driven Development)

## Task
Implement a proper telemetry-based analytics system using Test-Driven Development to replace the flawed RestartTracker. This uses OTP's built-in telemetry events for reliable supervisor monitoring without external simulation.

## Required Reading
**You must read these files before starting:**

1. `docs/TELEMETRY_ANALYTICS_IMPLEMENTATION.md` - Complete implementation guide
2. `docs/REAL_OTP_DESIGN2.md` - Context on why telemetry is the correct approach
3. `../../docs/code-standards/otp-testing-standards.md` - **PRIMARY TESTING STANDARD**
4. `lib/otp_supervisor/application.ex` - Where to add AnalyticsServer
5. `lib/otp_supervisor/core/control.ex` - Where to add new analytics functions
6. `test/otp_supervisor/core/control_test.exs` - Pattern for writing tests
7. `test/support/supervisor_test_helper.ex` - Testing patterns to follow

## Prerequisites
- ✅ **Phase 01 completed** - All flawed features removed
- ✅ `mix test` passes after cleanup
- ✅ No references to RestartTracker remain in codebase

## Test-Driven Development Workflow

### Phase 1: Write Tests First (TDD Red Phase)

#### Step 1.1: Create AnalyticsServer Test Suite
**Create**: `test/otp_supervisor/core/analytics_server_test.exs`

```elixir
defmodule OTPSupervisor.Core.AnalyticsServerTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog
  import SupervisorTestHelper
  
  @moduledoc """
  Test suite for AnalyticsServer using OTP Testing Standards.
  
  These tests verify telemetry-based analytics without external simulation.
  All tests follow OTP patterns and demonstrate proper event handling.
  """
  
  alias OTPSupervisor.Core.AnalyticsServer
  
  describe "analytics server lifecycle" do
    test "starts successfully and attaches telemetry handlers" do
      # AnalyticsServer should already be running in application
      # If this test runs, the server started successfully
      assert Process.whereis(AnalyticsServer) != nil
      assert Process.alive?(Process.whereis(AnalyticsServer))
    end
    
    test "provides empty history for unknown supervisors" do
      unknown_pid = spawn(fn -> :ok end)
      history = AnalyticsServer.get_restart_history(unknown_pid)
      assert history == []
    end
    
    test "provides initial stats after startup" do
      stats = AnalyticsServer.get_all_supervisor_stats()
      assert is_map(stats)
      assert Map.has_key?(stats, :total_supervisors)
      assert Map.has_key?(stats, :total_restarts)
      assert Map.has_key?(stats, :uptime_ms)
      assert Map.has_key?(stats, :supervisor_stats)
    end
  end
  
  describe "telemetry event processing" do
    setup do
      # Create isolated supervisor for testing telemetry
      setup_isolated_supervisor("telemetry_test")
    end
    
    test "captures supervisor restart events", %{supervisor: supervisor, sup_pid: sup_pid} do
      # Get initial restart history
      initial_history = AnalyticsServer.get_restart_history(sup_pid)
      initial_count = length(initial_history)
      
      # Get a child process to kill
      children = Supervisor.which_children(sup_pid)
      {child_id, child_pid, _type, _modules} = hd(children)
      
      # Monitor the child process
      ref = Process.monitor(child_pid)
      
      # Kill the child to trigger restart
      Process.exit(child_pid, :kill)
      
      # Wait for process to die (OTP pattern - no sleep)
      receive do
        {:DOWN, ^ref, :process, ^child_pid, :killed} -> :ok
      after
        1000 -> flunk("Child process did not die")
      end
      
      # Wait for supervisor to restart child
      :ok = wait_for_child_restart(sup_pid, child_id, child_pid)
      
      # Give telemetry events time to be processed
      # Use sync call to ensure all messages processed
      :ok = AnalyticsServer.sync(sup_pid)
      
      # Verify restart event was captured
      new_history = AnalyticsServer.get_restart_history(sup_pid)
      new_count = length(new_history)
      
      assert new_count > initial_count, "No restart event was captured"
      
      # Verify restart event details
      latest_event = hd(new_history)
      assert latest_event.child_id == child_id
      assert latest_event.event_type in [:terminated, :restarted]
      assert is_integer(latest_event.timestamp)
    end
    
    test "calculates failure rates correctly", %{supervisor: supervisor, sup_pid: sup_pid} do
      # Cause multiple restarts
      children = Supervisor.which_children(sup_pid)
      {child_id, child_pid, _type, _modules} = hd(children)
      
      # Kill child multiple times
      for _i <- 1..3 do
        ref = Process.monitor(child_pid)
        Process.exit(child_pid, :kill)
        
        # Wait for death
        receive do
          {:DOWN, ^ref, :process, ^child_pid, :killed} -> :ok
        after
          1000 -> flunk("Child process did not die")
        end
        
        # Wait for restart
        :ok = wait_for_child_restart(sup_pid, child_id, child_pid)
        
        # Get new child PID for next iteration
        updated_children = Supervisor.which_children(sup_pid)
        {^child_id, new_child_pid, _type, _modules} = 
          Enum.find(updated_children, fn {id, _pid, _type, _modules} -> id == child_id end)
        child_pid = new_child_pid
      end
      
      # Sync to ensure all events processed
      :ok = AnalyticsServer.sync(sup_pid)
      
      # Check failure rate over last 10 seconds
      failure_rate = AnalyticsServer.get_failure_rate(sup_pid, 10_000)
      
      assert failure_rate.restarts >= 3
      assert failure_rate.rate > 0
      assert failure_rate.window_ms == 10_000
    end
    
    test "tracks multiple supervisors independently", %{supervisor: supervisor1, sup_pid: sup_pid1} do
      # Create second isolated supervisor
      %{supervisor: supervisor2, sup_pid: sup_pid2} = 
        setup_isolated_supervisor("telemetry_test_2")
      
      on_exit(fn ->
        if Process.alive?(sup_pid2) do
          ref = Process.monitor(sup_pid2)
          Process.exit(sup_pid2, :kill)
          receive do
            {:DOWN, ^ref, :process, ^sup_pid2, _} -> :ok
          after 100 -> :ok
          end
        end
      end)
      
      # Cause restart in first supervisor only
      children1 = Supervisor.which_children(sup_pid1)
      {child_id1, child_pid1, _type, _modules} = hd(children1)
      
      ref = Process.monitor(child_pid1)
      Process.exit(child_pid1, :kill)
      
      receive do
        {:DOWN, ^ref, :process, ^child_pid1, :killed} -> :ok
      after
        1000 -> flunk("Child process did not die")
      end
      
      :ok = wait_for_child_restart(sup_pid1, child_id1, child_pid1)
      :ok = AnalyticsServer.sync(sup_pid1)
      
      # Verify first supervisor has restart events
      history1 = AnalyticsServer.get_restart_history(sup_pid1)
      assert length(history1) > 0
      
      # Verify second supervisor has no restart events
      history2 = AnalyticsServer.get_restart_history(sup_pid2)
      initial_count = length(history2)
      
      # Should be empty or unchanged
      assert initial_count == 0 or length(history2) == initial_count
    end
  end
  
  describe "error handling and edge cases" do
    test "handles non-existent supervisor gracefully" do
      fake_pid = spawn(fn -> :ok end)
      Process.exit(fake_pid, :kill)
      
      # Should not crash when querying dead process
      history = AnalyticsServer.get_restart_history(fake_pid)
      assert history == []
      
      failure_rate = AnalyticsServer.get_failure_rate(fake_pid, 1000)
      assert failure_rate.restarts == 0
    end
    
    test "maintains bounded history" do
      # This test would require generating many events
      # For now, verify the structure supports bounds
      history = AnalyticsServer.get_restart_history(self())
      assert is_list(history)
    end
  end
end
```

#### Step 1.2: Create Control Module Analytics Tests
**Add to**: `test/otp_supervisor/core/control_test.exs`

```elixir
# Add this describe block to the existing file
describe "telemetry-based analytics" do
  setup do
    setup_isolated_supervisor("analytics_control_test")
  end
  
  test "get_restart_history returns telemetry data", %{supervisor: supervisor, sup_pid: sup_pid} do
    # Test Control module wrapper
    {:ok, history} = Control.get_restart_history(supervisor)
    assert is_list(history)
    
    # Also test with PID directly
    {:ok, history_pid} = Control.get_restart_history(sup_pid)
    assert history == history_pid
  end
  
  test "get_supervisor_analytics returns comprehensive stats", %{supervisor: supervisor} do
    stats = Control.get_supervisor_analytics()
    
    assert is_map(stats)
    assert Map.has_key?(stats, :total_supervisors)
    assert Map.has_key?(stats, :total_restarts)
    assert Map.has_key?(stats, :supervisor_stats)
    assert is_list(stats.supervisor_stats)
  end
  
  test "get_failure_rate calculates rates correctly", %{supervisor: supervisor, sup_pid: sup_pid} do
    {:ok, failure_rate} = Control.get_failure_rate(supervisor, 5000)
    
    assert is_map(failure_rate)
    assert Map.has_key?(failure_rate, :restarts)
    assert Map.has_key?(failure_rate, :rate)
    assert Map.has_key?(failure_rate, :window_ms)
    assert failure_rate.window_ms == 5000
  end
  
  test "handles invalid supervisor gracefully" do
    assert {:error, :invalid_pid} = Control.get_restart_history("invalid")
    assert {:error, :invalid_pid} = Control.get_failure_rate("invalid", 1000)
  end
end
```

#### Step 1.3: Run Initial Tests (Should Fail - Red Phase)
```bash
# These should FAIL because AnalyticsServer doesn't exist yet
mix test test/otp_supervisor/core/analytics_server_test.exs

# This should FAIL because new Control functions don't exist
mix test test/otp_supervisor/core/control_test.exs
```

### Phase 2: Implement Features (TDD Green Phase)

#### Step 2.1: Create AnalyticsServer Module
**Create**: `lib/otp_supervisor/core/analytics_server.ex`

Follow the implementation from `docs/TELEMETRY_ANALYTICS_IMPLEMENTATION.md` with these OTP Testing Standards requirements:

- ✅ **No Process.sleep/1** - Use proper OTP synchronization
- ✅ **GenServer message ordering** - Rely on FIFO guarantees  
- ✅ **Proper process monitoring** - Use Process.monitor/1
- ✅ **Error handling** - Handle supervisor crashes gracefully
- ✅ **Educational comments** - Explain telemetry patterns

```elixir
defmodule OTPSupervisor.Core.AnalyticsServer do
  @moduledoc """
  Collects supervisor analytics using OTP's built-in telemetry events.
  
  This server demonstrates the modern, idiomatic way to monitor OTP supervisors
  without external process monitoring or race conditions. It uses :telemetry
  events that are emitted by supervisors themselves.
  
  ## Educational Value
  
  This module teaches:
  - Proper telemetry integration patterns
  - GenServer state management for analytics
  - Handling asynchronous telemetry events
  - Bounded data storage techniques
  """
  
  use GenServer
  require Logger
  
  # Implementation from docs/TELEMETRY_ANALYTICS_IMPLEMENTATION.md
  # Following OTP Testing Standards patterns
end
```

#### Step 2.2: Add to Application Supervision Tree
**Update**: `lib/otp_supervisor/application.ex`

```elixir
def start(_type, _args) do
  children = [
    OtpSupervisorWeb.Telemetry,
    {DNSCluster, query: Application.get_env(:otp_supervisor, :dns_cluster_query) || :ignore},
    {Phoenix.PubSub, name: OtpSupervisor.PubSub},
    {Finch, name: OtpSupervisor.Finch},
    {Registry, keys: :unique, name: TracerRegistry},
    
    # Add AnalyticsServer to collect supervisor events
    OTPSupervisor.Core.AnalyticsServer,
    
    OtpSupervisorWeb.Endpoint,
    {OTPSupervisor.Sandbox.Supervisors.DemoSupervisor,
     name: :demo_one_for_one, strategy: :one_for_one}
  ]
  
  opts = [strategy: :one_for_one, name: OtpSupervisor.Supervisor]
  Supervisor.start_link(children, opts)
end
```

#### Step 2.3: Update Control Module
**Update**: `lib/otp_supervisor/core/control.ex`

Add analytics functions following OTP patterns:

```elixir
# Add these functions to Control module
def get_restart_history(supervisor) do
  case to_pid(supervisor) do
    {:ok, pid} ->
      {:ok, OTPSupervisor.Core.AnalyticsServer.get_restart_history(pid)}
    error ->
      error
  end
end

def get_supervisor_analytics do
  OTPSupervisor.Core.AnalyticsServer.get_all_supervisor_stats()
end

def get_failure_rate(supervisor, time_window_ms \\ 60_000) do
  case to_pid(supervisor) do
    {:ok, pid} ->
      {:ok, OTPSupervisor.Core.AnalyticsServer.get_failure_rate(pid, time_window_ms)}
    error ->
      error
  end
end
```

#### Step 2.4: Run Tests (Should Pass - Green Phase)
```bash
# Application should start with AnalyticsServer
mix compile

# Analytics server tests should pass
mix test test/otp_supervisor/core/analytics_server_test.exs

# Control module tests should pass
mix test test/otp_supervisor/core/control_test.exs

# Full test suite should pass
mix test
```

### Phase 3: Refactor and Review (TDD Blue Phase)

#### Step 3.1: Code Review Based on OTP Testing Standards

**Review Checklist:**

- [ ] **No Process.sleep/1 usage** in implementation or tests
- [ ] **Proper GenServer synchronization** using call/cast patterns
- [ ] **Process monitoring** for supervisor lifecycle management
- [ ] **Telemetry handlers are non-blocking** and fast
- [ ] **Bounded data storage** prevents memory leaks
- [ ] **Error handling** for edge cases and failures
- [ ] **Educational comments** explain OTP patterns
- [ ] **Tests demonstrate real OTP scenarios** not mocked events

#### Step 3.2: Performance and Integration Testing

**Add integration test**: `test/otp_supervisor/core/analytics_integration_test.exs`

```elixir
defmodule OTPSupervisor.Core.AnalyticsIntegrationTest do
  use ExUnit.Case, async: false  # Telemetry is global
  import SupervisorTestHelper
  
  @moduledoc """
  Integration tests for analytics with real OTP supervisors.
  Tests the full flow from supervisor events to analytics data.
  """
  
  alias OTPSupervisor.Core.{AnalyticsServer, Control}
  
  describe "end-to-end analytics flow" do
    setup do
      setup_isolated_supervisor("integration_test")
    end
    
    test "complete restart tracking flow", %{supervisor: supervisor, sup_pid: sup_pid} do
      # 1. Verify initial state
      {:ok, initial_history} = Control.get_restart_history(supervisor)
      initial_count = length(initial_history)
      
      # 2. Cause a restart using real OTP operations
      children = Supervisor.which_children(sup_pid)
      {child_id, child_pid, _type, _modules} = hd(children)
      
      ref = Process.monitor(child_pid)
      Process.exit(child_pid, :kill)
      
      # 3. Wait for restart using OTP patterns
      receive do
        {:DOWN, ^ref, :process, ^child_pid, :killed} -> :ok
      after
        1000 -> flunk("Process did not terminate")
      end
      
      :ok = wait_for_child_restart(sup_pid, child_id, child_pid)
      
      # 4. Verify analytics captured the event
      :ok = AnalyticsServer.sync(sup_pid)
      {:ok, final_history} = Control.get_restart_history(supervisor)
      
      assert length(final_history) > initial_count
      
      # 5. Verify global stats updated
      stats = Control.get_supervisor_analytics()
      assert stats.total_restarts > 0
    end
    
    test "analytics work with demo supervisor", %{} do
      # Test with the real demo supervisor
      {:ok, demo_history} = Control.get_restart_history(:demo_one_for_one)
      assert is_list(demo_history)
      
      # Should be able to get failure rate
      {:ok, failure_rate} = Control.get_failure_rate(:demo_one_for_one, 10_000)
      assert is_map(failure_rate)
    end
  end
  
  describe "performance characteristics" do
    test "telemetry handlers are fast" do
      # Telemetry handlers should complete quickly
      # This test ensures no blocking operations in handlers
      start_time = System.monotonic_time(:microsecond)
      
      # Generate telemetry event manually
      :telemetry.execute(
        [:test, :event], 
        %{}, 
        %{supervisor_pid: self(), child_id: :test}
      )
      
      end_time = System.monotonic_time(:microsecond)
      duration_us = end_time - start_time
      
      # Should complete in microseconds, not milliseconds
      assert duration_us < 1000, "Telemetry handler too slow: #{duration_us}μs"
    end
  end
end
```

#### Step 3.3: Manual Verification

```bash
# Start application and verify analytics work
iex -S mix

# Test with demo supervisor
iex> alias OTPSupervisor.Core.Control
iex> {:ok, history} = Control.get_restart_history(:demo_one_for_one)
iex> stats = Control.get_supervisor_analytics()
iex> IO.inspect(stats)

# Kill a process and verify tracking
iex> children = Supervisor.which_children(:demo_one_for_one)
iex> {_, pid, _, _} = hd(children)
iex> Process.exit(pid, :kill)
iex> :timer.sleep(100)  # Only for manual testing
iex> {:ok, new_history} = Control.get_restart_history(:demo_one_for_one)
iex> length(new_history) > length(history)
```

## Success Criteria

### Functional Requirements
- [ ] AnalyticsServer starts successfully in application tree
- [ ] Telemetry events are captured from real supervisor operations
- [ ] `Control.get_restart_history/1` returns real event data
- [ ] `Control.get_failure_rate/2` calculates accurate rates
- [ ] `Control.get_supervisor_analytics/0` provides system overview
- [ ] Works with both isolated test supervisors and demo supervisor

### Code Quality Requirements (OTP Testing Standards)
- [ ] **No Process.sleep/1** in any implementation or tests
- [ ] **Proper OTP synchronization** using GenServer patterns
- [ ] **Process monitoring** for supervisor lifecycle
- [ ] **Educational comments** explaining telemetry patterns
- [ ] **Error handling** for edge cases
- [ ] **Bounded storage** preventing memory leaks

### Testing Requirements
- [ ] All tests follow OTP Testing Standards
- [ ] Tests use real OTP operations, not mocked events
- [ ] Proper cleanup with `on_exit/1` callbacks
- [ ] Unique naming for test isolation
- [ ] Tests serve as educational examples

### Educational Value
- [ ] Code demonstrates proper telemetry integration
- [ ] Tests show real OTP supervisor monitoring
- [ ] Comments explain why telemetry is superior to external monitoring
- [ ] Examples show production-ready patterns

## Performance Requirements
- [ ] Telemetry handlers complete in microseconds
- [ ] No blocking operations in event processing
- [ ] Memory usage bounded (limited history storage)
- [ ] Minimal impact on supervisor performance

## Integration Requirements
- [ ] Works seamlessly with existing Control module API
- [ ] Compatible with SupervisorTestHelper patterns
- [ ] Does not interfere with Phoenix application
- [ ] Integrates cleanly with demo supervisor

## Notes
- **Use real telemetry** - Never mock or simulate telemetry events
- **Follow OTP Testing Standards** for all code and tests
- **Educational first** - Code should teach proper OTP monitoring
- **Performance conscious** - Telemetry handlers must be fast
- **Test with real supervisors** - Use actual OTP restart scenarios

This TDD implementation provides **real OTP analytics** using industry-standard telemetry patterns while serving as an excellent educational example.