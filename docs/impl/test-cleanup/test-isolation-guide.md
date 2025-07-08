# Test Isolation Guide for OTP Supervisor Educational Tool

## Overview

This guide provides essential patterns and practices for writing isolated tests when working with OTP supervisors and processes. Proper test isolation is crucial to prevent test interference and ensure deterministic, reliable test execution.

## Core Principle: Test Independence

Every test must be **completely independent** and should not affect or be affected by other tests. This is especially critical when testing supervisors and processes that use global names.

## Test Categories

### 1. Destructive Tests (Modify State)
Tests that kill processes, modify state, or alter supervisor behavior **MUST** use isolated supervisors.

**Examples:**
- Killing processes
- Testing restart behavior
- Modifying process state
- Testing supervisor crashes

### 2. Read-Only Tests (Safe to Share)
Tests that only read state without modifications can use the shared demo supervisor.

**Examples:**
- Listing supervisors
- Reading process info
- Displaying supervision trees
- Checking process existence

### 3. Error Tests (Special Isolation)
Tests for error scenarios should use temporary supervisors designed to fail.

## Essential Test Helper: SupervisorTestHelper

The project includes `test/support/supervisor_test_helper.ex` which provides isolated supervisor instances:

### For Destructive Tests:
```elixir
describe "process killing tests" do
  setup do
    SupervisorTestHelper.setup_isolated_supervisor("kill_tests")
  end
  
  test "kills and restarts process", %{supervisor: supervisor} do
    # Safe to kill - isolated supervisor
    {:ok, children} = Control.get_supervision_tree(supervisor)
    worker_pid = SupervisorTestHelper.extract_first_worker_pid(children)
    Process.exit(worker_pid, :kill)
    # Process will restart with new PID
  end
end
```

### For Read-Only Tests:
```elixir
describe "read-only operations" do
  setup do
    SupervisorTestHelper.get_demo_supervisor()
  end
  
  test "lists supervisor children", %{supervisor: supervisor} do
    # Safe to read - no mutations
    {:ok, children} = Control.get_supervision_tree(supervisor)
    assert length(children) > 0
  end
end
```

## Critical Patterns to Follow

### 1. Unique Process Names
When creating processes in tests, always use unique names:
```elixir
unique_id = :erlang.unique_integer([:positive])
counter_name = :"test_counter_#{unique_id}"
```

### 2. Proper Cleanup
Always ensure processes are cleaned up after tests:
```elixir
on_exit(fn ->
  if Process.alive?(pid) do
    Process.exit(pid, :kill)
    Process.sleep(10)  # Allow cleanup
  end
end)
```

### 3. Avoid Process.sleep for Synchronization
Use proper synchronization instead of sleep:
```elixir
# Bad - race condition prone
Process.exit(pid, :kill)
Process.sleep(100)

# Good - deterministic
Process.exit(pid, :kill)
:ok = SupervisorTestHelper.wait_for_restart(supervisor)
```

**CRITICAL**: If helpers don't work immediately, **debug the helper usage**, don't fall back to sleep. Common issues:
- Using supervisor name instead of supervisor PID
- Helper expects different supervisor type than you're using
- Race condition in test setup, not synchronization

### 4. Handle PID String Conversions
Control module returns PIDs as strings. Convert them properly:
```elixir
pid_string = "#PID<0.123.0>"
actual_pid = SupervisorTestHelper.extract_pid_from_string(pid_string)
```

## Anti-Patterns to Avoid

### ❌ Shared Mutable State
```elixir
# WRONG - affects other tests
Counter.increment(:counter_1)  # Global name!
```

### ❌ Conditional Test Skipping
```elixir
# WRONG - hides failures
if supervisor == nil do
  assert true  # Silent skip!
else
  # actual test
end
```

### ❌ Assuming Initial State
```elixir
# WRONG - other tests may have changed it
assert Counter.get_value(:counter_1) == 0
```

### ❌ Global Process Names in Tests
```elixir
# WRONG - conflicts with other tests
{:ok, _} = Counter.start_link(name: :my_counter)
```

## LiveView Test Considerations

When testing LiveView components that interact with supervisors:

1. **Use Isolated Supervisors** for any test that triggers process operations
2. **Mock Supervisor Operations** when testing UI behavior only
3. **Test Real Integration** separately with proper isolation

```elixir
test "kill button works", %{conn: conn} do
  # Setup isolated supervisor
  %{supervisor: supervisor} = SupervisorTestHelper.setup_isolated_supervisor("lv_test")
  
  # Test LiveView with isolated supervisor
  {:ok, view, _html} = live(conn, "/supervisors?supervisor=#{supervisor}")
  
  # Safe to kill processes - they're isolated
  view |> element("[data-test-kill-button]") |> render_click()
end
```

## Test Organization

### Group by Behavior Type
```elixir
describe "destructive supervisor operations" do
  setup do
    SupervisorTestHelper.setup_isolated_supervisor("destructive")
  end
  # All tests that modify state
end

describe "read-only supervisor operations" do  
  setup do
    SupervisorTestHelper.get_demo_supervisor()
  end
  # All tests that only read
end
```

### Use Descriptive Test Names
```elixir
test "supervisor restarts killed child with one_for_one strategy" do
  # Clear intent
end
```

## Debugging Test Failures

### Check for Isolation Issues
1. Run single test in isolation: `mix test path/to/test.exs:LINE`
2. Look for global process names
3. Verify cleanup is happening
4. Check for race conditions

### Common Symptoms of Poor Isolation
- Tests pass individually but fail together
- Intermittent failures
- "Process already registered" errors
- Unexpected process states

## Summary Checklist

Before submitting test code, verify:

- [ ] Destructive tests use `setup_isolated_supervisor/1`
- [ ] Read-only tests use `get_demo_supervisor/0`
- [ ] No hardcoded global process names
- [ ] Proper cleanup with `on_exit/1`
- [ ] No `Process.sleep/1` for synchronization
- [ ] Tests pass when run in any order
- [ ] Tests pass when run in parallel
- [ ] Clear test names describe the scenario

Following these patterns ensures reliable, maintainable tests that properly demonstrate OTP supervisor behavior without interference.