# 03 - Implement Sandbox Manager (Test-Driven Development)

## Task
Implement a proper SandboxManager for real OTP supervisor lifecycle management using Test-Driven Development. This replaces the flawed SupervisorController with genuine OTP operations for creating, destroying, and managing sandbox supervision trees.

## Required Reading
**You must read these files before starting:**

1. `docs/SANDBOX_MANAGER_IMPLEMENTATION.md` - Complete implementation guide
2. `docs/REAL_OTP_DESIGN2.md` - Context on why real lifecycle management is correct
3. `../../docs/code-standards/otp-testing-standards.md` - **PRIMARY TESTING STANDARD**
4. `lib/otp_supervisor/application.ex` - Where to add SandboxManager
5. `lib/otp_supervisor/core/control.ex` - Where to add sandbox management functions
6. `lib/otp_supervisor/sandbox/supervisors/demo_supervisor.ex` - Pattern for supervisor implementation
7. `test/support/test_demo_supervisor.ex` - Pattern for test supervisors
8. `test/support/supervisor_test_helper.ex` - Testing patterns to follow

## Prerequisites
- ✅ **Phase 01 completed** - All flawed features removed
- ✅ **Phase 02 completed** - Telemetry analytics implemented and working
- ✅ `mix test` passes completely
- ✅ No references to SupervisorController remain in codebase

## Test-Driven Development Workflow

### Phase 1: Write Tests First (TDD Red Phase)

#### Step 1.1: Create SandboxManager Test Suite
**Create**: `test/otp_supervisor/core/sandbox_manager_test.exs`

```elixir
defmodule OTPSupervisor.Core.SandboxManagerTest do
  use ExUnit.Case, async: true
  import SupervisorTestHelper
  
  @moduledoc """
  Test suite for SandboxManager using OTP Testing Standards.
  
  These tests verify real OTP supervisor lifecycle management without
  simulation or external hacks. All tests follow OTP patterns and
  demonstrate proper sandbox isolation.
  """
  
  alias OTPSupervisor.Core.SandboxManager
  alias OTPSupervisor.Sandbox.TestDemoSupervisor
  
  describe "sandbox manager lifecycle" do
    test "starts successfully and initializes ETS table" do
      # SandboxManager should already be running in application
      # If this test runs, the manager started successfully
      assert Process.whereis(SandboxManager) != nil
      assert Process.alive?(Process.whereis(SandboxManager))
      
      # ETS table should exist
      assert :ets.info(:sandboxes) != :undefined
    end
    
    test "provides empty sandbox list initially" do
      sandboxes = SandboxManager.list_sandboxes()
      assert is_list(sandboxes)
      # May not be empty if other tests created sandboxes
    end
  end
  
  describe "sandbox creation and destruction" do
    test "creates sandbox with unique naming" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_create_#{unique_id}"
      
      # Create sandbox
      {:ok, sandbox_info} = SandboxManager.create_sandbox(
        sandbox_id, 
        TestDemoSupervisor, 
        [strategy: :one_for_one]
      )
      
      # Verify sandbox info structure
      assert sandbox_info.id == sandbox_id
      assert sandbox_info.supervisor_module == TestDemoSupervisor
      assert is_pid(sandbox_info.supervisor_pid)
      assert Process.alive?(sandbox_info.supervisor_pid)
      assert sandbox_info.opts[:strategy] == :one_for_one
      assert is_integer(sandbox_info.created_at)
      assert sandbox_info.restart_count == 0
      
      # Verify children are running
      children = Supervisor.which_children(sandbox_info.supervisor_pid)
      assert length(children) == 3  # Should have 2 counters + 1 printer
      
      # Verify unique child naming
      child_names = Enum.map(children, fn {id, _pid, _type, _modules} -> id end)
      assert :counter_1 in child_names
      assert :counter_2 in child_names
      assert :printer_1 in child_names
      
      # Cleanup
      :ok = SandboxManager.destroy_sandbox(sandbox_id)
    end
    
    test "destroys sandbox completely" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_destroy_#{unique_id}"
      
      # Create sandbox
      {:ok, sandbox_info} = SandboxManager.create_sandbox(
        sandbox_id,
        TestDemoSupervisor,
        [strategy: :one_for_all]
      )
      
      supervisor_pid = sandbox_info.supervisor_pid
      
      # Monitor supervisor for death
      ref = Process.monitor(supervisor_pid)
      
      # Destroy sandbox
      :ok = SandboxManager.destroy_sandbox(sandbox_id)
      
      # Wait for supervisor to die (OTP pattern - no sleep)
      receive do
        {:DOWN, ^ref, :process, ^supervisor_pid, _reason} -> :ok
      after
        1000 -> flunk("Supervisor did not terminate")
      end
      
      # Verify supervisor is dead
      refute Process.alive?(supervisor_pid)
      
      # Verify sandbox removed from manager
      {:error, :not_found} = SandboxManager.get_sandbox_info(sandbox_id)
    end
    
    test "prevents duplicate sandbox IDs" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_duplicate_#{unique_id}"
      
      # Create first sandbox
      {:ok, _sandbox_info} = SandboxManager.create_sandbox(
        sandbox_id,
        TestDemoSupervisor
      )
      
      # Attempt to create duplicate
      {:error, {:already_exists, existing_info}} = SandboxManager.create_sandbox(
        sandbox_id,
        TestDemoSupervisor
      )
      
      assert existing_info.id == sandbox_id
      
      # Cleanup
      :ok = SandboxManager.destroy_sandbox(sandbox_id)
    end
  end
  
  describe "sandbox restart functionality" do
    test "restarts sandbox with same configuration" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_restart_#{unique_id}"
      
      # Create sandbox with specific configuration
      {:ok, original_info} = SandboxManager.create_sandbox(
        sandbox_id,
        TestDemoSupervisor,
        [strategy: :rest_for_one, custom_opt: :test_value]
      )
      
      original_pid = original_info.supervisor_pid
      original_opts = original_info.opts
      
      # Monitor original supervisor
      ref = Process.monitor(original_pid)
      
      # Restart sandbox
      {:ok, restarted_info} = SandboxManager.restart_sandbox(sandbox_id)
      
      # Wait for original supervisor to die
      receive do
        {:DOWN, ^ref, :process, ^original_pid, _reason} -> :ok
      after
        1000 -> flunk("Original supervisor did not terminate")
      end
      
      # Verify new supervisor is different
      new_pid = restarted_info.supervisor_pid
      assert new_pid != original_pid
      assert Process.alive?(new_pid)
      
      # Verify configuration preserved
      assert restarted_info.id == sandbox_id
      assert restarted_info.supervisor_module == TestDemoSupervisor
      assert restarted_info.opts[:strategy] == :rest_for_one
      assert restarted_info.opts[:custom_opt] == :test_value
      assert restarted_info.restart_count == 1
      
      # Verify children are running in new supervisor
      children = Supervisor.which_children(new_pid)
      assert length(children) == 3
      
      # Cleanup
      :ok = SandboxManager.destroy_sandbox(sandbox_id)
    end
    
    test "handles restart of non-existent sandbox" do
      {:error, :not_found} = SandboxManager.restart_sandbox("non_existent")
    end
  end
  
  describe "sandbox introspection" do
    test "provides sandbox information" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_info_#{unique_id}"
      
      # Create sandbox
      {:ok, _created_info} = SandboxManager.create_sandbox(
        sandbox_id,
        TestDemoSupervisor,
        [strategy: :one_for_one]
      )
      
      # Get sandbox info
      {:ok, sandbox_info} = SandboxManager.get_sandbox_info(sandbox_id)
      
      assert sandbox_info.id == sandbox_id
      assert sandbox_info.supervisor_module == TestDemoSupervisor
      assert is_pid(sandbox_info.supervisor_pid)
      assert Process.alive?(sandbox_info.supervisor_pid)
      
      # Cleanup
      :ok = SandboxManager.destroy_sandbox(sandbox_id)
    end
    
    test "provides sandbox PID lookup" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_pid_#{unique_id}"
      
      # Create sandbox
      {:ok, sandbox_info} = SandboxManager.create_sandbox(
        sandbox_id,
        TestDemoSupervisor
      )
      
      # Get PID via lookup
      {:ok, pid} = SandboxManager.get_sandbox_pid(sandbox_id)
      assert pid == sandbox_info.supervisor_pid
      
      # Cleanup
      :ok = SandboxManager.destroy_sandbox(sandbox_id)
    end
    
    test "lists all active sandboxes" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id_1 = "test_list_1_#{unique_id}"
      sandbox_id_2 = "test_list_2_#{unique_id}"
      
      # Create multiple sandboxes
      {:ok, _info1} = SandboxManager.create_sandbox(sandbox_id_1, TestDemoSupervisor)
      {:ok, _info2} = SandboxManager.create_sandbox(sandbox_id_2, TestDemoSupervisor)
      
      # List sandboxes
      sandboxes = SandboxManager.list_sandboxes()
      sandbox_ids = Enum.map(sandboxes, & &1.id)
      
      assert sandbox_id_1 in sandbox_ids
      assert sandbox_id_2 in sandbox_ids
      
      # Cleanup
      :ok = SandboxManager.destroy_sandbox(sandbox_id_1)
      :ok = SandboxManager.destroy_sandbox(sandbox_id_2)
    end
  end
  
  describe "supervisor crash handling" do
    test "detects and cleans up crashed supervisors" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_crash_#{unique_id}"
      
      # Create sandbox
      {:ok, sandbox_info} = SandboxManager.create_sandbox(
        sandbox_id,
        TestDemoSupervisor
      )
      
      supervisor_pid = sandbox_info.supervisor_pid
      
      # Monitor the supervisor
      ref = Process.monitor(supervisor_pid)
      
      # Kill the supervisor directly
      Process.exit(supervisor_pid, :kill)
      
      # Wait for supervisor to die
      receive do
        {:DOWN, ^ref, :process, ^supervisor_pid, :killed} -> :ok
      after
        1000 -> flunk("Supervisor did not die")
      end
      
      # Give SandboxManager time to process DOWN message
      # Use a small delay and then check - this is acceptable for crash testing
      :timer.sleep(50)
      
      # Verify sandbox was cleaned up
      {:error, :not_found} = SandboxManager.get_sandbox_info(sandbox_id)
      
      # Verify not in sandbox list
      sandboxes = SandboxManager.list_sandboxes()
      sandbox_ids = Enum.map(sandboxes, & &1.id)
      refute sandbox_id in sandbox_ids
    end
  end
  
  describe "concurrent operations" do
    test "handles multiple concurrent sandbox operations" do
      unique_id = :erlang.unique_integer([:positive])
      
      # Create multiple sandboxes concurrently
      tasks = for i <- 1..5 do
        Task.async(fn ->
          sandbox_id = "concurrent_#{i}_#{unique_id}"
          {:ok, info} = SandboxManager.create_sandbox(sandbox_id, TestDemoSupervisor)
          {sandbox_id, info}
        end)
      end
      
      # Wait for all creations to complete
      results = Enum.map(tasks, &Task.await/1)
      
      # Verify all sandboxes were created
      assert length(results) == 5
      
      # Cleanup concurrently
      cleanup_tasks = for {sandbox_id, _info} <- results do
        Task.async(fn ->
          :ok = SandboxManager.destroy_sandbox(sandbox_id)
        end)
      end
      
      # Wait for all cleanups
      Enum.each(cleanup_tasks, &Task.await/1)
    end
  end
end
```

#### Step 1.2: Create Enhanced TestDemoSupervisor Tests
**Create**: `test/otp_supervisor/sandbox/test_demo_supervisor_test.exs`

```elixir
defmodule OTPSupervisor.Sandbox.TestDemoSupervisorTest do
  use ExUnit.Case, async: true
  import SupervisorTestHelper
  
  @moduledoc """
  Tests for enhanced TestDemoSupervisor with unique naming support.
  Verifies proper isolation and configuration options.
  """
  
  alias OTPSupervisor.Sandbox.TestDemoSupervisor
  
  describe "enhanced supervisor functionality" do
    test "supports unique naming for isolation" do
      unique_id_1 = :erlang.unique_integer([:positive])
      unique_id_2 = :erlang.unique_integer([:positive])
      
      # Start two supervisors with different unique IDs
      {:ok, sup1} = TestDemoSupervisor.start_link(
        name: :"test_sup_#{unique_id_1}",
        unique_id: unique_id_1,
        strategy: :one_for_one
      )
      
      {:ok, sup2} = TestDemoSupervisor.start_link(
        name: :"test_sup_#{unique_id_2}",
        unique_id: unique_id_2,
        strategy: :one_for_all
      )
      
      # Verify children have unique names
      children1 = Supervisor.which_children(sup1)
      children2 = Supervisor.which_children(sup2)
      
      # Extract child process names from registered processes
      child_names_1 = get_child_names(unique_id_1)
      child_names_2 = get_child_names(unique_id_2)
      
      # Verify names are different
      assert child_names_1 != child_names_2
      
      # Verify no name conflicts
      for name1 <- child_names_1 do
        refute name1 in child_names_2
      end
      
      # Cleanup
      on_exit(fn ->
        if Process.alive?(sup1), do: Supervisor.stop(sup1)
        if Process.alive?(sup2), do: Supervisor.stop(sup2)
      end)
    end
    
    test "supports different supervisor strategies" do
      unique_id = :erlang.unique_integer([:positive])
      
      # Test different strategies
      strategies = [:one_for_one, :one_for_all, :rest_for_one]
      
      for strategy <- strategies do
        {:ok, sup_pid} = TestDemoSupervisor.start_link(
          name: :"test_strategy_#{strategy}_#{unique_id}",
          unique_id: unique_id,
          strategy: strategy
        )
        
        # Verify supervisor started with correct strategy
        children = Supervisor.which_children(sup_pid)
        assert length(children) == 3
        
        # Cleanup
        Supervisor.stop(sup_pid)
      end
    end
    
    test "worker processes are functional" do
      unique_id = :erlang.unique_integer([:positive])
      
      {:ok, sup_pid} = TestDemoSupervisor.start_link(
        name: :"test_workers_#{unique_id}",
        unique_id: unique_id
      )
      
      counter_name = :"counter_1_#{unique_id}"
      printer_name = :"printer_1_#{unique_id}"
      
      # Test counter functionality
      original_value = OTPSupervisor.Sandbox.Workers.Counter.get_value(counter_name)
      OTPSupervisor.Sandbox.Workers.Counter.increment(counter_name)
      new_value = OTPSupervisor.Sandbox.Workers.Counter.get_value(counter_name)
      assert new_value == original_value + 1
      
      # Test printer functionality
      assert :ok = OTPSupervisor.Sandbox.Workers.Printer.print_message(
        printer_name, 
        "test message"
      )
      
      # Cleanup
      Supervisor.stop(sup_pid)
    end
  end
  
  defp get_child_names(unique_id) do
    [
      :"counter_1_#{unique_id}",
      :"counter_2_#{unique_id}",
      :"printer_1_#{unique_id}"
    ]
  end
end
```

#### Step 1.3: Create Control Module Sandbox Tests  
**Add to**: `test/otp_supervisor/core/control_test.exs`

```elixir
# Add this describe block to the existing file
describe "sandbox management" do
  test "creates sandbox with automatic ID generation" do
    {:ok, sandbox_info} = Control.create_sandbox(
      OTPSupervisor.Sandbox.TestDemoSupervisor,
      [strategy: :one_for_one]
    )
    
    assert is_binary(sandbox_info.id)
    assert sandbox_info.supervisor_module == OTPSupervisor.Sandbox.TestDemoSupervisor
    assert is_pid(sandbox_info.supervisor_pid)
    assert Process.alive?(sandbox_info.supervisor_pid)
    
    # Cleanup
    :ok = Control.destroy_sandbox(sandbox_info.id)
  end
  
  test "destroys sandbox by ID" do
    {:ok, sandbox_info} = Control.create_sandbox(OTPSupervisor.Sandbox.TestDemoSupervisor)
    supervisor_pid = sandbox_info.supervisor_pid
    
    # Monitor for death
    ref = Process.monitor(supervisor_pid)
    
    :ok = Control.destroy_sandbox(sandbox_info.id)
    
    # Wait for supervisor to die
    receive do
      {:DOWN, ^ref, :process, ^supervisor_pid, _reason} -> :ok
    after
      1000 -> flunk("Supervisor did not terminate")
    end
    
    refute Process.alive?(supervisor_pid)
  end
  
  test "restarts sandbox preserving configuration" do
    {:ok, original_info} = Control.create_sandbox(
      OTPSupervisor.Sandbox.TestDemoSupervisor,
      [strategy: :rest_for_one, test_opt: :value]
    )
    
    original_pid = original_info.supervisor_pid
    
    {:ok, restarted_info} = Control.restart_sandbox(original_info.id)
    
    assert restarted_info.supervisor_pid != original_pid
    assert restarted_info.opts[:strategy] == :rest_for_one
    assert restarted_info.opts[:test_opt] == :value
    assert restarted_info.restart_count == 1
    
    # Cleanup
    :ok = Control.destroy_sandbox(restarted_info.id)
  end
  
  test "lists active sandboxes" do
    {:ok, sandbox1} = Control.create_sandbox(OTPSupervisor.Sandbox.TestDemoSupervisor)
    {:ok, sandbox2} = Control.create_sandbox(OTPSupervisor.Sandbox.TestDemoSupervisor)
    
    sandboxes = Control.list_sandboxes()
    sandbox_ids = Enum.map(sandboxes, & &1.id)
    
    assert sandbox1.id in sandbox_ids
    assert sandbox2.id in sandbox_ids
    
    # Cleanup
    :ok = Control.destroy_sandbox(sandbox1.id)
    :ok = Control.destroy_sandbox(sandbox2.id)
  end
  
  test "gets sandbox information" do
    {:ok, created_info} = Control.create_sandbox(OTPSupervisor.Sandbox.TestDemoSupervisor)
    
    {:ok, retrieved_info} = Control.get_sandbox_info(created_info.id)
    
    assert retrieved_info.id == created_info.id
    assert retrieved_info.supervisor_pid == created_info.supervisor_pid
    
    # Cleanup
    :ok = Control.destroy_sandbox(created_info.id)
  end
  
  test "handles non-existent sandbox gracefully" do
    assert {:error, :not_found} = Control.destroy_sandbox("non_existent")
    assert {:error, :not_found} = Control.restart_sandbox("non_existent")
    assert {:error, :not_found} = Control.get_sandbox_info("non_existent")
  end
end
```

#### Step 1.4: Run Initial Tests (Should Fail - Red Phase)
```bash
# These should FAIL because SandboxManager doesn't exist yet
mix test test/otp_supervisor/core/sandbox_manager_test.exs

# This should FAIL because TestDemoSupervisor enhancements don't exist
mix test test/otp_supervisor/sandbox/test_demo_supervisor_test.exs

# This should FAIL because new Control functions don't exist
mix test test/otp_supervisor/core/control_test.exs
```

### Phase 2: Implement Features (TDD Green Phase)

#### Step 2.1: Create SandboxManager Module
**Create**: `lib/otp_supervisor/core/sandbox_manager.ex`

Follow the implementation from `docs/SANDBOX_MANAGER_IMPLEMENTATION.md` with these OTP Testing Standards requirements:

- ✅ **No Process.sleep/1** - Use proper OTP synchronization
- ✅ **GenServer message ordering** - Rely on FIFO guarantees
- ✅ **Proper process monitoring** - Use Process.monitor/1 for supervisor lifecycle
- ✅ **ETS for fast lookup** - Use ETS tables for sandbox metadata
- ✅ **Graceful cleanup** - Handle supervisor crashes properly
- ✅ **Comprehensive documentation** - Explain lifecycle management patterns

#### Step 2.2: Enhance TestDemoSupervisor
**Update**: `test/support/test_demo_supervisor.ex`

Add unique naming support and configuration options:

```elixir
defmodule OTPSupervisor.Sandbox.TestDemoSupervisor do
  @moduledoc """
  Production-grade demo supervisor with unique naming for sandbox isolation.
  
  This supervisor provides proper OTP patterns for dynamic supervisor
  creation with unique child naming to prevent conflicts between multiple
  instances running concurrently.
  """
  
  use Supervisor
  
  def start_link(opts) do
    strategy = Keyword.get(opts, :strategy, :one_for_one)
    unique_id = Keyword.get(opts, :unique_id, :erlang.unique_integer([:positive]))
    name = Keyword.get(opts, :name, :"test_demo_supervisor_#{unique_id}")
    
    Supervisor.start_link(__MODULE__, {strategy, unique_id}, name: name)
  end
  
  @impl true
  def init({strategy, unique_id}) do
    children = [
      %{
        id: :counter_1,
        start: {OTPSupervisor.Sandbox.Workers.Counter, :start_link,
                [[name: :"counter_1_#{unique_id}", initial_value: 0]]}
      },
      %{
        id: :counter_2,  
        start: {OTPSupervisor.Sandbox.Workers.Counter, :start_link,
                [[name: :"counter_2_#{unique_id}", initial_value: 100]]}
      },
      %{
        id: :printer_1,
        start: {OTPSupervisor.Sandbox.Workers.Printer, :start_link,
                [[name: :"printer_1_#{unique_id}", id: "printer-#{unique_id}"]]}
      }
    ]
    
    Supervisor.init(children, strategy: strategy)
  end
end
```

#### Step 2.3: Add to Application Supervision Tree
**Update**: `lib/otp_supervisor/application.ex`

```elixir
def start(_type, _args) do
  children = [
    OtpSupervisorWeb.Telemetry,
    {DNSCluster, query: Application.get_env(:otp_supervisor, :dns_cluster_query) || :ignore},
    {Phoenix.PubSub, name: OtpSupervisor.PubSub},
    {Finch, name: OtpSupervisor.Finch},
    {Registry, keys: :unique, name: TracerRegistry},
    
    # Analytics and sandbox management
    OTPSupervisor.Core.AnalyticsServer,
    OTPSupervisor.Core.SandboxManager,
    
    OtpSupervisorWeb.Endpoint,
    {OTPSupervisor.Sandbox.Supervisors.DemoSupervisor,
     name: :demo_one_for_one, strategy: :one_for_one}
  ]
  
  opts = [strategy: :one_for_one, name: OtpSupervisor.Supervisor]
  Supervisor.start_link(children, opts)
end
```

#### Step 2.4: Update Control Module
**Update**: `lib/otp_supervisor/core/control.ex`

Add sandbox management functions:

```elixir
# Add these functions to Control module
def create_sandbox(supervisor_module, opts \\ []) do
  sandbox_id = "sandbox_#{:erlang.unique_integer([:positive])}"
  OTPSupervisor.Core.SandboxManager.create_sandbox(sandbox_id, supervisor_module, opts)
end

def destroy_sandbox(sandbox_id) do
  OTPSupervisor.Core.SandboxManager.destroy_sandbox(sandbox_id)
end

def restart_sandbox(sandbox_id) do
  OTPSupervisor.Core.SandboxManager.restart_sandbox(sandbox_id)
end

def list_sandboxes do
  OTPSupervisor.Core.SandboxManager.list_sandboxes()
end

def get_sandbox_info(sandbox_id) do
  OTPSupervisor.Core.SandboxManager.get_sandbox_info(sandbox_id)
end
```

#### Step 2.5: Run Tests (Should Pass - Green Phase)
```bash
# Application should start with SandboxManager
mix compile

# Sandbox manager tests should pass
mix test test/otp_supervisor/core/sandbox_manager_test.exs

# Enhanced supervisor tests should pass
mix test test/otp_supervisor/sandbox/test_demo_supervisor_test.exs

# Control module tests should pass
mix test test/otp_supervisor/core/control_test.exs

# Full test suite should pass
mix test
```

### Phase 3: Refactor and Review (TDD Blue Phase)

#### Step 3.1: Code Review Based on OTP Testing Standards

**Review Checklist:**

- [ ] **No Process.sleep/1 usage** in implementation or tests (except supervised crash detection)
- [ ] **Proper GenServer synchronization** using call/cast patterns
- [ ] **Process monitoring** for supervisor lifecycle management  
- [ ] **ETS table management** for fast sandbox lookup
- [ ] **Graceful cleanup** with proper resource management
- [ ] **Unique naming** prevents conflicts between sandboxes
- [ ] **Error handling** for edge cases and failures
- [ ] **Comprehensive documentation** explain lifecycle management patterns
- [ ] **Tests demonstrate real OTP scenarios** not simulated behavior

#### Step 3.2: Integration and Performance Testing

**Add integration test**: `test/otp_supervisor/core/sandbox_integration_test.exs`

```elixir
defmodule OTPSupervisor.Core.SandboxIntegrationTest do
  use ExUnit.Case, async: true
  import SupervisorTestHelper
  
  @moduledoc """
  Integration tests for sandbox management with analytics and control systems.
  Tests the full integration between SandboxManager, AnalyticsServer, and Control.
  """
  
  alias OTPSupervisor.Core.{SandboxManager, Control, AnalyticsServer}
  alias OTPSupervisor.Sandbox.TestDemoSupervisor
  
  describe "end-to-end sandbox lifecycle with analytics" do
    test "sandbox operations are tracked by analytics" do
      # Create sandbox via Control module
      {:ok, sandbox_info} = Control.create_sandbox(TestDemoSupervisor, [strategy: :one_for_one])
      sandbox_pid = sandbox_info.supervisor_pid
      
      # Verify analytics can track this supervisor
      {:ok, initial_history} = Control.get_restart_history(sandbox_pid)
      assert is_list(initial_history)
      
      # Cause a restart in the sandbox
      children = Supervisor.which_children(sandbox_pid)
      {child_id, child_pid, _type, _modules} = hd(children)
      
      ref = Process.monitor(child_pid)
      Process.exit(child_pid, :kill)
      
      # Wait for restart using OTP patterns
      receive do
        {:DOWN, ^ref, :process, ^child_pid, :killed} -> :ok
      after
        1000 -> flunk("Child process did not terminate")
      end
      
      :ok = wait_for_child_restart(sandbox_pid, child_id, child_pid)
      
      # Verify analytics captured the event
      :ok = AnalyticsServer.sync(sandbox_pid)
      {:ok, final_history} = Control.get_restart_history(sandbox_pid)
      
      assert length(final_history) > length(initial_history)
      
      # Cleanup
      :ok = Control.destroy_sandbox(sandbox_info.id)
    end
    
    test "multiple sandboxes work independently" do
      # Create multiple sandboxes with different strategies
      {:ok, sandbox1} = Control.create_sandbox(TestDemoSupervisor, [strategy: :one_for_one])
      {:ok, sandbox2} = Control.create_sandbox(TestDemoSupervisor, [strategy: :one_for_all])
      
      # Verify they're listed
      sandboxes = Control.list_sandboxes()
      sandbox_ids = Enum.map(sandboxes, & &1.id)
      assert sandbox1.id in sandbox_ids
      assert sandbox2.id in sandbox_ids
      
      # Verify they have different PIDs
      assert sandbox1.supervisor_pid != sandbox2.supervisor_pid
      
      # Verify both are functional
      children1 = Supervisor.which_children(sandbox1.supervisor_pid)
      children2 = Supervisor.which_children(sandbox2.supervisor_pid)
      assert length(children1) == 3
      assert length(children2) == 3
      
      # Cleanup
      :ok = Control.destroy_sandbox(sandbox1.id)
      :ok = Control.destroy_sandbox(sandbox2.id)
    end
  end
  
  describe "sandbox resilience and error handling" do
    test "survives sandbox supervisor crashes" do
      # Create sandbox
      {:ok, sandbox_info} = Control.create_sandbox(TestDemoSupervisor)
      supervisor_pid = sandbox_info.supervisor_pid
      sandbox_id = sandbox_info.id
      
      # Kill the sandbox supervisor directly
      ref = Process.monitor(supervisor_pid)
      Process.exit(supervisor_pid, :kill)
      
      # Wait for death
      receive do
        {:DOWN, ^ref, :process, ^supervisor_pid, :killed} -> :ok
      after
        1000 -> flunk("Supervisor did not die")
      end
      
      # Give SandboxManager time to clean up
      :timer.sleep(50)
      
      # Verify cleanup happened
      {:error, :not_found} = Control.get_sandbox_info(sandbox_id)
      
      # Verify SandboxManager is still functional
      {:ok, new_sandbox} = Control.create_sandbox(TestDemoSupervisor)
      assert Process.alive?(new_sandbox.supervisor_pid)
      
      # Cleanup
      :ok = Control.destroy_sandbox(new_sandbox.id)
    end
  end
end
```

#### Step 3.3: Manual Verification

```bash
# Start application and verify sandbox management works
iex -S mix

# Test sandbox lifecycle
iex> alias OTPSupervisor.Core.Control
iex> alias OTPSupervisor.Sandbox.TestDemoSupervisor

# Create sandbox
iex> {:ok, info} = Control.create_sandbox(TestDemoSupervisor, [strategy: :one_for_one])
iex> Process.alive?(info.supervisor_pid)
true

# Verify children
iex> Supervisor.which_children(info.supervisor_pid)
# Should show 3 children

# List sandboxes
iex> Control.list_sandboxes()

# Restart sandbox
iex> {:ok, restarted} = Control.restart_sandbox(info.id)
iex> restarted.restart_count
1

# Destroy sandbox
iex> :ok = Control.destroy_sandbox(info.id)
iex> Process.alive?(info.supervisor_pid)
false
```

## Success Criteria

### Functional Requirements
- [ ] SandboxManager starts successfully in application tree
- [ ] Can create multiple sandboxes without naming conflicts
- [ ] Sandbox destruction properly cleans up all processes  
- [ ] Sandbox restart preserves original configuration
- [ ] Supervisor crashes are detected and cleaned up automatically
- [ ] Control module provides clean API for sandbox operations

### Code Quality Requirements (OTP Testing Standards)
- [ ] **No Process.sleep/1** except for supervisor crash detection
- [ ] **Proper OTP synchronization** using GenServer patterns
- [ ] **Process monitoring** for supervisor lifecycle management
- [ ] **ETS table management** for fast lookups
- [ ] **Comprehensive documentation** explaining lifecycle patterns  
- [ ] **Error handling** for edge cases and failures
- [ ] **Resource cleanup** with proper teardown

### Testing Requirements  
- [ ] All tests follow OTP Testing Standards
- [ ] Tests use real OTP supervisor operations
- [ ] Proper cleanup with `on_exit/1` callbacks
- [ ] Unique naming for test isolation
- [ ] Tests demonstrate supervisor lifecycle patterns
- [ ] Integration tests verify end-to-end functionality

### Production Value
- [ ] Code demonstrates proper OTP supervisor lifecycle management
- [ ] Tests show real-world patterns for managing subsystems
- [ ] Comments explain why lifecycle management is superior to simulation
- [ ] Examples show production-ready patterns for dynamic supervisors

## Performance Requirements
- [ ] ETS lookups are fast for sandbox metadata
- [ ] No blocking operations in normal sandbox operations
- [ ] Concurrent sandbox operations work correctly
- [ ] Memory usage is bounded (no supervisor leaks)

## Integration Requirements
- [ ] Works seamlessly with existing Control module API
- [ ] Compatible with AnalyticsServer for tracking sandbox events
- [ ] Does not interfere with Phoenix application or demo supervisor
- [ ] Integrates cleanly with SupervisorTestHelper patterns

## Notes
- **Use real OTP operations** - Never simulate supervisor lifecycle
- **Follow OTP Testing Standards** for all code and tests
- **Production focused** - Code should provide robust lifecycle management
- **Test with real supervisors** - Use actual OTP start/stop scenarios
- **Handle edge cases** - Supervisor crashes, invalid inputs, concurrent access

This TDD implementation provides **real OTP supervisor lifecycle management** using genuine patterns from production applications for managing dynamic supervision trees and subsystems.