# Real OTP Design: How to Build Proper Supervisor Features

## Current Architecture Analysis

### What We Have Right ✅
- **Real sandbox supervision tree**: `DemoSupervisor` is properly integrated into the main application supervision tree at `application.ex:23-24`
- **Isolated from Phoenix**: The sandbox runs under the main supervisor but doesn't affect Phoenix/web processes
- **Real OTP workers**: Counter and Printer are actual GenServers with proper OTP behaviors
- **Registry for process tracking**: Using `TracerRegistry` for legitimate process discovery

### What's Wrong ❌

#### 1. RestartTracker - External Monitoring Antipattern

**Current Flawed Implementation:**
```elixir
# lib/otp_supervisor/core/restart_tracker.ex
def init(supervisor_pid) do
  # Monitors processes FROM OUTSIDE the supervisor
  children = get_supervisor_children(supervisor_pid)
  monitored_children = monitor_children(children)
end

def handle_info({:DOWN, ref, :process, pid, reason}, state) do
  # Detects restart by polling supervisor state
  current_children = get_supervisor_children(state.supervisor_pid)
  case find_restarted_child(child_id, current_children) do
end
```

**Problems:**
- External monitoring creates race conditions
- Can't detect all restart events reliably  
- Doesn't work with dynamic supervisors
- Simulates behavior instead of integrating with OTP

#### 2. SupervisorController - Fake Pause/Resume

**Current Flawed Implementation:**
```elixir
# lib/otp_supervisor/core/supervisor_controller.ex
def handle_call(:pause, _from, state) do
  # SIMULATES pausing by terminating all children
  children = Supervisor.which_children(state.supervisor_pid)
  Enum.each(children, fn {id, pid, _type, _modules} ->
    Supervisor.terminate_child(state.supervisor_pid, id)
    Supervisor.delete_child(state.supervisor_pid, id)
  end)
end
```

**Problems:**
- Not real pause - just empties the supervisor
- Breaks if other processes start children
- Loses supervisor state consistency
- Simulates behavior instead of building real OTP features

## How To Build It Right

### 1. Restart Tracking - Custom Supervisor Behavior

**Replace RestartTracker with a proper custom supervisor:**

```elixir
defmodule OTPSupervisor.Sandbox.TrackingSupervisor do
  @moduledoc """
  A supervisor that tracks restart events natively in its callbacks.
  """
  use Supervisor
  
  # Registry for storing restart history
  def start_link(opts) do
    name = Keyword.fetch!(opts, :name)
    children = Keyword.fetch!(opts, :children)
    strategy = Keyword.get(opts, :strategy, :one_for_one)
    
    # Register restart history storage
    :ets.new(:"#{name}_restarts", [:public, :named_table, :ordered_set])
    
    Supervisor.start_link(__MODULE__, {children, strategy}, name: name)
  end
  
  @impl true
  def init({children, strategy}) do
    Supervisor.init(children, strategy: strategy)
  end
  
  # Override restart_child to track events
  def restart_child(supervisor, child_id) do
    case Supervisor.restart_child(supervisor, child_id) do
      {:ok, child} = result ->
        record_restart_event(supervisor, child_id, :manual_restart, nil, child)
        result
      error -> 
        error
    end
  end
  
  # Track automatic restarts by overriding handle_info
  def handle_info({:EXIT, pid, reason}, state) do
    # Find which child died
    case find_child_by_pid(state, pid) do
      {:ok, child_id} ->
        # Let supervisor handle restart
        result = super({:EXIT, pid, reason}, state)
        
        # Track the restart after it happens
        case result do
          {:noreply, new_state} ->
            case find_child_by_id(new_state, child_id) do
              {:ok, new_pid} when new_pid != pid ->
                record_restart_event(self(), child_id, reason, pid, new_pid)
              _ -> :ok
            end
        end
        
        result
      :not_found ->
        super({:EXIT, pid, reason}, state)
    end
  end
  
  defp record_restart_event(supervisor, child_id, reason, old_pid, new_pid) do
    table_name = :"#{supervisor}_restarts"
    timestamp = System.system_time(:millisecond)
    
    event = {timestamp, child_id, reason, old_pid, new_pid}
    :ets.insert(table_name, event)
  end
  
  def get_restart_history(supervisor) do
    table_name = :"#{supervisor}_restarts"
    :ets.tab2list(table_name)
  end
end
```

### 2. Real Supervisor Control - Built Into Supervisor

**Replace SupervisorController with proper supervisor extensions:**

```elixir
defmodule OTPSupervisor.Sandbox.ControllableSupervisor do
  @moduledoc """
  A supervisor that can be paused/resumed through proper OTP mechanisms.
  """
  use GenServer
  
  # This implements a supervisor using GenServer to have full control
  # over restart behavior without simulating anything
  
  def start_link(opts) do
    name = Keyword.fetch!(opts, :name)
    children = Keyword.fetch!(opts, :children)
    strategy = Keyword.get(opts, :strategy, :one_for_one)
    
    GenServer.start_link(__MODULE__, {children, strategy}, name: name)
  end
  
  def pause(supervisor) do
    GenServer.call(supervisor, :pause)
  end
  
  def resume(supervisor) do
    GenServer.call(supervisor, :resume)
  end
  
  def which_children(supervisor) do
    GenServer.call(supervisor, :which_children)
  end
  
  @impl true
  def init({children, strategy}) do
    # Start with normal supervision
    state = %{
      children: children,
      strategy: strategy,
      paused: false,
      active_children: %{},
      child_specs: %{}
    }
    
    # Start all children
    {:ok, start_children(state)}
  end
  
  @impl true
  def handle_call(:pause, _from, state) do
    if state.paused do
      {:reply, :already_paused, state}
    else
      # Stop monitoring and terminate children
      new_state = stop_all_children(state)
      {:reply, :ok, %{new_state | paused: true}}
    end
  end
  
  @impl true
  def handle_call(:resume, _from, state) do
    if state.paused do
      # Restart all children
      new_state = start_children(%{state | paused: false})
      {:reply, :ok, new_state}
    else
      {:reply, :not_paused, state}
    end
  end
  
  @impl true
  def handle_call(:which_children, _from, state) do
    children = Map.to_list(state.active_children)
    {:reply, children, state}
  end
  
  @impl true
  def handle_info({:DOWN, ref, :process, pid, reason}, state) do
    if state.paused do
      # Don't restart when paused - just clean up
      new_active = remove_child_by_pid(state.active_children, pid)
      {:noreply, %{state | active_children: new_active}}
    else
      # Normal restart logic based on strategy
      case find_child_by_pid(state.active_children, pid) do
        {:ok, child_id} ->
          new_state = restart_child_by_strategy(state, child_id, reason)
          {:noreply, new_state}
        :not_found ->
          {:noreply, state}
      end
    end
  end
  
  # Real restart logic based on OTP supervisor strategies
  defp restart_child_by_strategy(state, failed_child_id, reason) do
    case state.strategy do
      :one_for_one ->
        restart_single_child(state, failed_child_id)
      :one_for_all ->
        restart_all_children(state)
      :rest_for_one ->
        restart_from_child(state, failed_child_id)
    end
  end
  
  # Implement proper OTP restart strategies...
end
```

### 3. Message Tracing - Proper OTP Integration

**Instead of external message tracing, build it into GenServers:**

```elixir
defmodule OTPSupervisor.Sandbox.TrackedGenServer do
  @moduledoc """
  A GenServer behaviour that automatically tracks messages.
  """
  
  defmacro __using__(opts) do
    quote do
      use GenServer
      @traced_calls Keyword.get(unquote(opts), :trace_calls, true)
      @traced_casts Keyword.get(unquote(opts), :trace_casts, true)
      @traced_info Keyword.get(unquote(opts), :trace_info, false)
      
      # Override GenServer callbacks to add tracing
      def handle_call(request, from, state) do
        if @traced_calls do
          OTPSupervisor.Core.MessageTracer.trace_call(self(), request, from)
        end
        
        super(request, from, state)
      end
      
      def handle_cast(request, state) do
        if @traced_casts do
          OTPSupervisor.Core.MessageTracer.trace_cast(self(), request)
        end
        
        super(request, state)
      end
      
      defoverridable handle_call: 3, handle_cast: 2
    end
  end
end
```

## The Right Architecture

### Sandbox Supervision Tree Structure

```
OtpSupervisor.Supervisor (main app)
├── OtpSupervisorWeb.Endpoint (Phoenix)
├── Registry (TracerRegistry)
└── OTPSupervisor.Sandbox.Root
    ├── OTPSupervisor.Sandbox.TrackingSupervisor (demo_tracking)
    │   ├── Counter (with restart tracking)
    │   ├── Counter (with restart tracking)  
    │   └── Printer (with restart tracking)
    ├── OTPSupervisor.Sandbox.ControllableSupervisor (demo_controllable)
    │   ├── Counter (pausable)
    │   └── Printer (pausable)
    └── OTPSupervisor.Sandbox.DynamicSupervisor (demo_dynamic)
        └── (dynamically added children)
```

### Key Principles

1. **No External Simulation**: Build features INTO supervisors, not around them
2. **Real OTP Patterns**: Use proper GenServer/Supervisor callbacks and behaviors  
3. **Registry for Discovery**: Use registry pattern for legitimate process discovery
4. **Sandbox Isolation**: Keep sandbox completely separate from Phoenix processes
5. **Educational Value**: Show how OTP really works, not fake implementations

### Implementation Plan

1. **Replace RestartTracker** with `TrackingSupervisor` that natively tracks restarts
2. **Replace SupervisorController** with `ControllableSupervisor` that implements real pause/resume
3. **Add TrackedGenServer** behavior for proper message tracing integration
4. **Create multiple sandbox supervisor types** to demonstrate different OTP patterns
5. **Keep registry pattern** for process discovery (this is legitimate OTP)

This gives us a **real OTP application** that demonstrates **actual OTP capabilities** without any simulation or external hacks.