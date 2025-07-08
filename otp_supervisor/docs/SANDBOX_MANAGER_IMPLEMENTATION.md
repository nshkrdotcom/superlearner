# SandboxManager Implementation

## Overview

Replace the flawed `SupervisorController` with a proper `SandboxManager` that uses real OTP lifecycle operations to manage sandbox supervision trees. No more fake "pause" simulation.

## The Problem with Current SupervisorController

```elixir
# FLAWED - Destructive simulation of "pause"
def handle_call(:pause, _from, state) do
  # This DESTROYS the supervision tree!
  children = Supervisor.which_children(state.supervisor_pid)
  Enum.each(children, fn {id, pid, _type, _modules} ->
    Supervisor.terminate_child(state.supervisor_pid, id)  # Kill children
    Supervisor.delete_child(state.supervisor_pid, id)     # Delete specs
  end)
  
  # Store specs in GenServer state (fragile!)
  {:reply, :ok, %{state | paused: true, original_children: child_specs}}
end
```

**Problems:**
- Not "pausing" - actually destroying and recreating supervision trees
- Fragile state management outside the supervisor
- Other processes can break the "paused" illusion by starting children
- Teaches destructive patterns instead of proper OTP lifecycle management

## The Right Way: SandboxManager

### 1. SandboxManager Implementation

```elixir
defmodule OTPSupervisor.Core.SandboxManager do
  @moduledoc """
  Manages the lifecycle of sandbox supervision trees.
  
  This manager provides a clean API for starting, stopping, and reconfiguring
  entire sandbox supervision trees using proper OTP lifecycle operations.
  This is a real-world pattern for managing subsystems in OTP applications.
  """
  
  use GenServer
  require Logger
  
  # Public API
  
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def create_sandbox(sandbox_id, supervisor_module, opts \\ []) do
    GenServer.call(__MODULE__, {:create_sandbox, sandbox_id, supervisor_module, opts})
  end
  
  def destroy_sandbox(sandbox_id) do
    GenServer.call(__MODULE__, {:destroy_sandbox, sandbox_id})
  end
  
  def restart_sandbox(sandbox_id) do
    GenServer.call(__MODULE__, {:restart_sandbox, sandbox_id})
  end
  
  def get_sandbox_info(sandbox_id) do
    GenServer.call(__MODULE__, {:get_sandbox_info, sandbox_id})
  end
  
  def list_sandboxes do
    GenServer.call(__MODULE__, :list_sandboxes)
  end
  
  def get_sandbox_pid(sandbox_id) do
    GenServer.call(__MODULE__, {:get_sandbox_pid, sandbox_id})
  end
  
  # GenServer Callbacks
  
  @impl true
  def init(_opts) do
    # Use ETS table for fast sandbox lookup
    :ets.new(:sandboxes, [:named_table, :set, :protected])
    
    state = %{
      sandboxes: %{},
      next_id: 1
    }
    
    Logger.info("SandboxManager started")
    {:ok, state}
  end
  
  @impl true
  def handle_call({:create_sandbox, sandbox_id, supervisor_module, opts}, _from, state) do
    case Map.get(state.sandboxes, sandbox_id) do
      nil ->
        # Create new sandbox
        case start_sandbox_supervisor(sandbox_id, supervisor_module, opts) do
          {:ok, pid, full_opts} ->
            sandbox_info = %{
              id: sandbox_id,
              supervisor_module: supervisor_module,
              supervisor_pid: pid,
              opts: full_opts,
              created_at: System.system_time(:millisecond),
              restart_count: 0
            }
            
            # Monitor the supervisor
            ref = Process.monitor(pid)
            
            # Store in ETS for fast lookup
            :ets.insert(:sandboxes, {sandbox_id, sandbox_info})
            
            new_sandboxes = Map.put(state.sandboxes, sandbox_id, {sandbox_info, ref})
            
            Logger.info("Created sandbox #{sandbox_id} with PID #{inspect(pid)}")
            {:reply, {:ok, sandbox_info}, %{state | sandboxes: new_sandboxes}}
            
          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end
        
      {existing_info, _ref} ->
        {:reply, {:error, {:already_exists, existing_info}}, state}
    end
  end
  
  @impl true
  def handle_call({:destroy_sandbox, sandbox_id}, _from, state) do
    case Map.get(state.sandboxes, sandbox_id) do
      {sandbox_info, ref} ->
        # Stop monitoring
        Process.demonitor(ref, [:flush])
        
        # Gracefully stop the supervisor
        case stop_sandbox_supervisor(sandbox_info.supervisor_pid) do
          :ok ->
            # Remove from state and ETS
            :ets.delete(:sandboxes, sandbox_id)
            new_sandboxes = Map.delete(state.sandboxes, sandbox_id)
            
            Logger.info("Destroyed sandbox #{sandbox_id}")
            {:reply, :ok, %{state | sandboxes: new_sandboxes}}
            
          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end
        
      nil ->
        {:reply, {:error, :not_found}, state}
    end
  end
  
  @impl true
  def handle_call({:restart_sandbox, sandbox_id}, _from, state) do
    case Map.get(state.sandboxes, sandbox_id) do
      {sandbox_info, ref} ->
        # Stop current supervisor
        Process.demonitor(ref, [:flush])
        stop_sandbox_supervisor(sandbox_info.supervisor_pid)
        
        # Start new supervisor with same configuration
        case start_sandbox_supervisor(sandbox_id, sandbox_info.supervisor_module, sandbox_info.opts) do
          {:ok, new_pid, _opts} ->
            # Update sandbox info
            updated_info = %{sandbox_info | 
              supervisor_pid: new_pid,
              restart_count: sandbox_info.restart_count + 1
            }
            
            # Monitor new supervisor
            new_ref = Process.monitor(new_pid)
            
            # Update state and ETS
            :ets.insert(:sandboxes, {sandbox_id, updated_info})
            new_sandboxes = Map.put(state.sandboxes, sandbox_id, {updated_info, new_ref})
            
            Logger.info("Restarted sandbox #{sandbox_id} with new PID #{inspect(new_pid)}")
            {:reply, {:ok, updated_info}, %{state | sandboxes: new_sandboxes}}
            
          {:error, reason} ->
            # Remove failed sandbox
            :ets.delete(:sandboxes, sandbox_id)
            new_sandboxes = Map.delete(state.sandboxes, sandbox_id)
            {:reply, {:error, reason}, %{state | sandboxes: new_sandboxes}}
        end
        
      nil ->
        {:reply, {:error, :not_found}, state}
    end
  end
  
  @impl true
  def handle_call({:get_sandbox_info, sandbox_id}, _from, state) do
    case :ets.lookup(:sandboxes, sandbox_id) do
      [{^sandbox_id, sandbox_info}] ->
        {:reply, {:ok, sandbox_info}, state}
      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end
  
  @impl true
  def handle_call({:get_sandbox_pid, sandbox_id}, _from, state) do
    case :ets.lookup(:sandboxes, sandbox_id) do
      [{^sandbox_id, sandbox_info}] ->
        {:reply, {:ok, sandbox_info.supervisor_pid}, state}
      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end
  
  @impl true
  def handle_call(:list_sandboxes, _from, state) do
    sandboxes = :ets.tab2list(:sandboxes)
    sandbox_list = Enum.map(sandboxes, fn {_id, info} -> info end)
    {:reply, sandbox_list, state}
  end
  
  @impl true
  def handle_info({:DOWN, ref, :process, pid, reason}, state) do
    # Find which sandbox died
    case find_sandbox_by_ref(state.sandboxes, ref) do
      {sandbox_id, sandbox_info} ->
        Logger.warn("Sandbox #{sandbox_id} supervisor died: #{inspect(reason)}")
        
        # Remove from state and ETS
        :ets.delete(:sandboxes, sandbox_id)
        new_sandboxes = Map.delete(state.sandboxes, sandbox_id)
        
        # Optionally implement auto-restart logic here
        {:noreply, %{state | sandboxes: new_sandboxes}}
        
      nil ->
        {:noreply, state}
    end
  end
  
  # Private Functions
  
  defp start_sandbox_supervisor(sandbox_id, supervisor_module, opts) do
    # Create unique name for this sandbox
    unique_id = :erlang.unique_integer([:positive])
    sandbox_name = :"sandbox_#{sandbox_id}_#{unique_id}"
    
    # Merge options with sandbox-specific configuration
    full_opts = Keyword.merge(opts, [
      name: sandbox_name,
      unique_id: unique_id,
      sandbox_id: sandbox_id
    ])
    
    case supervisor_module.start_link(full_opts) do
      {:ok, pid} -> 
        {:ok, pid, full_opts}
      {:error, reason} -> 
        {:error, reason}
    end
  end
  
  defp stop_sandbox_supervisor(supervisor_pid) do
    try do
      # Give the supervisor 5 seconds to shut down gracefully
      case Supervisor.stop(supervisor_pid, :normal, 5000) do
        :ok -> :ok
        {:error, reason} -> {:error, reason}
      end
    catch
      :exit, reason -> {:error, reason}
    end
  end
  
  defp find_sandbox_by_ref(sandboxes, target_ref) do
    Enum.find_value(sandboxes, fn {sandbox_id, {sandbox_info, ref}} ->
      if ref == target_ref do
        {sandbox_id, sandbox_info}
      else
        nil
      end
    end)
  end
end
```

### 2. Integration with Control Module

```elixr
# lib/otp_supervisor/core/control.ex

# Replace the flawed supervisor controller functions
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

# Remove these flawed functions:
# - pause_supervisor/1
# - resume_supervisor/1  
# - paused?/1
```

### 3. TestDemoSupervisor Enhancement

```elixir
defmodule OTPSupervisor.Sandbox.TestDemoSupervisor do
  @moduledoc """
  Enhanced demo supervisor that works with SandboxManager.
  
  This supervisor supports unique child naming for isolation
  and can be configured with different strategies dynamically.
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

### 4. Testing the SandboxManager

```elixir
defmodule OTPSupervisor.Core.SandboxManagerTest do
  use ExUnit.Case, async: true
  
  alias OTPSupervisor.Core.SandboxManager
  alias OTPSupervisor.Sandbox.TestDemoSupervisor
  
  test "creates and destroys sandboxes" do
    # Create sandbox
    {:ok, sandbox_info} = SandboxManager.create_sandbox(
      "test_sandbox", 
      TestDemoSupervisor, 
      [strategy: :one_for_one]
    )
    
    assert sandbox_info.id == "test_sandbox"
    assert Process.alive?(sandbox_info.supervisor_pid)
    
    # Verify children are running
    children = Supervisor.which_children(sandbox_info.supervisor_pid)
    assert length(children) == 3
    
    # Destroy sandbox
    :ok = SandboxManager.destroy_sandbox("test_sandbox")
    
    # Verify supervisor is stopped
    refute Process.alive?(sandbox_info.supervisor_pid)
    
    # Verify sandbox is removed
    {:error, :not_found} = SandboxManager.get_sandbox_info("test_sandbox")
  end
  
  test "restarts sandboxes with same configuration" do
    # Create sandbox
    {:ok, original_info} = SandboxManager.create_sandbox(
      "restart_test", 
      TestDemoSupervisor,
      [strategy: :one_for_all]
    )
    
    original_pid = original_info.supervisor_pid
    
    # Restart sandbox
    {:ok, restarted_info} = SandboxManager.restart_sandbox("restart_test")
    
    # Verify new PID and incremented restart count
    assert restarted_info.supervisor_pid != original_pid
    assert restarted_info.restart_count == 1
    assert restarted_info.opts[:strategy] == :one_for_all
    
    # Cleanup
    SandboxManager.destroy_sandbox("restart_test")
  end
  
  test "handles supervisor crashes gracefully" do
    # Create sandbox
    {:ok, sandbox_info} = SandboxManager.create_sandbox(
      "crash_test",
      TestDemoSupervisor
    )
    
    # Kill the supervisor process
    Process.exit(sandbox_info.supervisor_pid, :kill)
    
    # Wait for DOWN message to be processed
    :timer.sleep(50)
    
    # Verify sandbox was cleaned up
    {:error, :not_found} = SandboxManager.get_sandbox_info("crash_test")
  end
end
```

## Benefits of This Approach

1. **Real OTP Operations**: Uses actual supervisor start/stop, not simulation
2. **Proper Lifecycle Management**: Demonstrates real-world subsystem management patterns
3. **Clean State Management**: All state is properly maintained in the manager
4. **Fault Tolerance**: Handles supervisor crashes gracefully
5. **Educational Value**: Shows proper OTP lifecycle patterns
6. **Production Ready**: This pattern is used in real OTP applications

## Migration Plan

1. **Add SandboxManager** to application supervision tree
2. **Enhance TestDemoSupervisor** with unique naming support
3. **Update Control module** to use SandboxManager instead of SupervisorController
4. **Remove SupervisorController** module completely
5. **Update tests** to use new sandbox lifecycle API
6. **Update LiveView** to work with sandbox management instead of pause/resume

This gives us **real OTP supervisor lifecycle management** without any simulation or fake state manipulation.