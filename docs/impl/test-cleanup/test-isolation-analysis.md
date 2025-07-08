# SupervisorLive Test Isolation Analysis

## Problem Statement

The SupervisorLive tests are failing due to **supervision tree crossover** - multiple tests interfering with each other by accessing the same globally named processes. This violates test isolation principles and causes non-deterministic failures.

## Root Cause Analysis

### Primary Conflict: Shared Global Supervisor

The main culprit is the `:demo_one_for_one` supervisor that starts with the application and creates globally named children:

```elixir
# DemoSupervisor creates these GLOBAL processes:
:counter_1    # With initial value 0
:counter_2    # With initial value 100  
:printer_1    # With ID "printer-1"
```

**Multiple tests access these same processes simultaneously**, causing:
- Race conditions when killing/restarting processes
- State mutations affecting other tests (counter increments)
- Timing dependencies on supervisor restart cycles

### Detailed Conflict Mapping

#### Category 1: DESTRUCTIVE Tests (Kill/Modify Processes)
These tests **MUST** use isolated supervisors:

1. **auto-refresh timer functionality** - Kills child processes
2. **handling of processes that die during display** - Kills processes  
3. **process killing via kill button** - Kills processes via UI
4. **UI updates after process kills** - Kills processes and checks UI
5. **process state changes reflected** - Increments counters (mutates state)
6. **handling of processes with missing info** - Kills selected processes
7. **concurrent user interactions** - Kills processes in multi-user scenario

#### Category 2: READ-ONLY Tests (Safe to share)
These can potentially use the shared supervisor:

1. **supervisor live view loads** - Only renders page
2. **can select a supervisor** - Only displays children
3. **process details formatting** - Only reads process info
4. **connection establishment** - Only tests WebSocket setup
5. **supervisor list updates** - Only checks supervisor detection

#### Category 3: ERROR HANDLING Tests (State Independent)
These should use isolated setups or mocks:

1. **invalid supervisor selection** - Tests error paths
2. **malformed PID selection** - Tests error handling
3. **supervisor that crashes during inspection** - Needs controlled crash

## Current Broken Patterns

### Anti-Pattern 1: Conditional Test Skipping
```elixir
if supervisor == nil do
  assert true  # TERRIBLE - silent skip
else
  # actual test
end
```

### Anti-Pattern 2: Shared Mutable State
```elixir
# Multiple tests doing this simultaneously:
Counter.increment(:counter_1)  # RACE CONDITION
Process.exit(counter_pid, :kill)  # INTERFERES WITH OTHER TESTS
```

### Anti-Pattern 3: Implicit Dependencies
```elixir
# Assuming counter_1 starts at 0, but other tests may have incremented it
assert Counter.get_value(:counter_1) == 0  # FLAKY
```

## Correct Isolation Pattern

The codebase already shows the RIGHT way in some tests:

```elixir
describe "isolated test group" do
  setup do
    # UNIQUE name prevents conflicts
    supervisor_name = :"test_supervisor_#{:erlang.unique_integer([:positive])}"
    {:ok, sup_pid} = DemoSupervisor.start_link(name: supervisor_name)
    
    # PROPER cleanup
    on_exit(fn ->
      if Process.alive?(sup_pid) do
        Process.exit(sup_pid, :kill)
      end
    end)
    
    %{supervisor: supervisor_name, sup_pid: sup_pid}
  end
  
  test "isolated test", %{supervisor: supervisor} do
    # Uses isolated supervisor - no conflicts!
  end
end
```

## Solution Strategy

### Immediate Fix: Test Helper System

Create a test helper module that provides isolated supervisor instances:

```elixir
defmodule SupervisorTestHelper do
  @doc "Creates isolated supervisor for destructive tests"
  def setup_isolated_supervisor(test_name \\ "") do
    supervisor_name = :"test_sup_#{test_name}_#{:erlang.unique_integer([:positive])}"
    {:ok, sup_pid} = DemoSupervisor.start_link(name: supervisor_name)
    
    ExUnit.Callbacks.on_exit(fn ->
      if Process.alive?(sup_pid) do
        Process.exit(sup_pid, :kill)
      end
    end)
    
    %{supervisor: supervisor_name, sup_pid: sup_pid}
  end
  
  @doc "Gets read-only access to demo supervisor"
  def get_demo_supervisor do
    case Control.get_supervision_tree(:demo_one_for_one) do
      {:ok, _} -> %{supervisor: :demo_one_for_one}
      {:error, _} -> raise "Demo supervisor not available for read-only tests"
    end
  end
end
```

### Test Classification and Fixes

#### For DESTRUCTIVE tests:
```elixir
describe "process killing tests" do
  setup do
    SupervisorTestHelper.setup_isolated_supervisor("killing")
  end
  
  test "kill process test", %{supervisor: supervisor} do
    # Safe to kill - it's isolated!
  end
end
```

#### For READ-ONLY tests:
```elixir
describe "read-only tests" do
  setup do
    SupervisorTestHelper.get_demo_supervisor()
  end
  
  test "read-only test", %{supervisor: supervisor} do
    # Safe to read - no mutations!
  end
end
```

## Elixir Testing Standards

This follows standard Elixir testing practices:

1. **Test Isolation**: Each test should be independent
2. **Proper Setup/Teardown**: Use `setup` and `on_exit` for resource management
3. **Unique Naming**: Use `unique_integer/1` to avoid name conflicts
4. **Explicit Dependencies**: Make test requirements explicit in setup

## Implementation Plan

1. **Create** `test/support/supervisor_test_helper.ex`
2. **Categorize** all existing tests into DESTRUCTIVE vs READ-ONLY
3. **Convert** destructive tests to use isolated supervisors
4. **Verify** read-only tests can safely share the demo supervisor
5. **Remove** all conditional skipping and anti-patterns
6. **Run** tests to ensure no more conflicts

This will result in **deterministic, isolated, robust tests** that follow Elixir community standards.