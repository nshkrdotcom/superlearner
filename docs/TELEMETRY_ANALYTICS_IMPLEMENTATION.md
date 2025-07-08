# Telemetry-Based Analytics Implementation

## Overview

Replace the flawed `RestartTracker` with a proper telemetry-based analytics system that hooks into OTP's built-in event system. This is the modern, idiomatic way to collect supervisor analytics.

## The Problem with Current RestartTracker

```elixir
# FLAWED - External monitoring creates race conditions
def init(supervisor_pid) do
  children = get_supervisor_children(supervisor_pid)  # Snapshot at one point in time
  monitored_children = monitor_children(children)     # Misses future children
end

def handle_info({:DOWN, ref, :process, pid, reason}, state) do
  # Trying to guess what happened by polling supervisor state
  current_children = get_supervisor_children(state.supervisor_pid)
  case find_restarted_child(child_id, current_children) do
```

**Problems:**
- External observer trying to guess supervisor internals
- Race conditions between child death and restart detection
- Blind to dynamic supervisor children added after initialization
- Violates OTP principle: supervisor is source of truth

## The Right Way: Telemetry Integration

### 1. AnalyticsServer Implementation

```elixir
defmodule OTPSupervisor.Core.AnalyticsServer do
  @moduledoc """
  Collects supervisor analytics using OTP's built-in telemetry events.
  
  This server hooks into the standard supervisor telemetry events to track
  restart patterns, failure rates, and supervisor health metrics across
  the entire application.
  """
  
  use GenServer
  require Logger
  
  # Public API
  
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def get_restart_history(supervisor_pid) when is_pid(supervisor_pid) do
    GenServer.call(__MODULE__, {:get_restart_history, supervisor_pid})
  end
  
  def get_all_supervisor_stats do
    GenServer.call(__MODULE__, :get_all_stats)
  end
  
  def get_failure_rate(supervisor_pid, time_window_ms \\ 60_000) do
    GenServer.call(__MODULE__, {:get_failure_rate, supervisor_pid, time_window_ms})
  end
  
  # GenServer Callbacks
  
  @impl true
  def init(_opts) do
    # Attach to all supervisor telemetry events
    attach_telemetry_handlers()
    
    state = %{
      # Map of supervisor_pid -> list of restart events
      restart_history: %{},
      # Map of supervisor_pid -> supervisor metadata  
      supervisor_info: %{},
      # Global stats
      total_restarts: 0,
      start_time: System.system_time(:millisecond)
    }
    
    Logger.info("AnalyticsServer started - monitoring all supervisor events")
    {:ok, state}
  end
  
  @impl true
  def handle_call({:get_restart_history, supervisor_pid}, _from, state) do
    history = Map.get(state.restart_history, supervisor_pid, [])
    {:reply, history, state}
  end
  
  @impl true
  def handle_call(:get_all_stats, _from, state) do
    stats = %{
      total_supervisors: map_size(state.supervisor_info),
      total_restarts: state.total_restarts,
      uptime_ms: System.system_time(:millisecond) - state.start_time,
      supervisor_stats: calculate_supervisor_stats(state)
    }
    {:reply, stats, state}
  end
  
  @impl true
  def handle_call({:get_failure_rate, supervisor_pid, time_window_ms}, _from, state) do
    now = System.system_time(:millisecond)
    cutoff_time = now - time_window_ms
    
    recent_restarts = 
      state.restart_history
      |> Map.get(supervisor_pid, [])
      |> Enum.count(fn event -> event.timestamp >= cutoff_time end)
    
    rate = recent_restarts / (time_window_ms / 1000)  # restarts per second
    {:reply, %{restarts: recent_restarts, rate: rate, window_ms: time_window_ms}, state}
  end
  
  @impl true
  def handle_cast({:supervisor_event, event_name, measurements, metadata}, state) do
    new_state = process_supervisor_event(event_name, measurements, metadata, state)
    {:noreply, new_state}
  end
  
  # Private Functions
  
  defp attach_telemetry_handlers do
    # Attach to supervisor child lifecycle events
    :ok = :telemetry.attach_many(
      "analytics-supervisor-events",
      [
        [:supervisor, :init],
        [:supervisor, :child, :start],
        [:supervisor, :child, :start_error], 
        [:supervisor, :child, :terminate],
        [:supervisor, :child, :restart]
      ],
      &__MODULE__.handle_telemetry_event/4,
      self()
    )
  end
  
  # This function is called by telemetry when events occur
  def handle_telemetry_event(event_name, measurements, metadata, analytics_server_pid) do
    GenServer.cast(analytics_server_pid, {:supervisor_event, event_name, measurements, metadata})
  end
  
  defp process_supervisor_event([:supervisor, :init], _measurements, metadata, state) do
    supervisor_pid = metadata.supervisor_pid
    
    info = %{
      name: metadata.name,
      strategy: metadata.strategy,
      max_restarts: metadata.max_restarts,
      max_seconds: metadata.max_seconds,
      init_time: System.system_time(:millisecond)
    }
    
    %{state | supervisor_info: Map.put(state.supervisor_info, supervisor_pid, info)}
  end
  
  defp process_supervisor_event([:supervisor, :child, :start], _measurements, metadata, state) do
    # Child started successfully - could track this for health metrics
    state
  end
  
  defp process_supervisor_event([:supervisor, :child, :start_error], _measurements, metadata, state) do
    # Child failed to start - this is a restart failure
    record_restart_event(metadata, :start_error, state)
  end
  
  defp process_supervisor_event([:supervisor, :child, :terminate], _measurements, metadata, state) do
    # Child terminated - check if it will be restarted
    if metadata.shutdown != :shutdown do
      record_restart_event(metadata, :terminated, state)
    else
      state
    end
  end
  
  defp process_supervisor_event([:supervisor, :child, :restart], _measurements, metadata, state) do
    # Child successfully restarted
    record_restart_event(metadata, :restarted, state)
  end
  
  defp record_restart_event(metadata, event_type, state) do
    supervisor_pid = metadata.supervisor_pid
    
    event = %{
      timestamp: System.system_time(:millisecond),
      event_type: event_type,
      child_id: metadata.child_id,
      child_pid: metadata.child_pid,
      reason: Map.get(metadata, :reason),
      shutdown: Map.get(metadata, :shutdown),
      supervisor_name: get_supervisor_name(supervisor_pid, state)
    }
    
    # Add to history (keep last 1000 events per supervisor)
    current_history = Map.get(state.restart_history, supervisor_pid, [])
    new_history = [event | current_history] |> Enum.take(1000)
    
    %{state | 
      restart_history: Map.put(state.restart_history, supervisor_pid, new_history),
      total_restarts: state.total_restarts + 1
    }
  end
  
  defp get_supervisor_name(supervisor_pid, state) do
    case Map.get(state.supervisor_info, supervisor_pid) do
      %{name: name} -> name
      _ -> inspect(supervisor_pid)
    end
  end
  
  defp calculate_supervisor_stats(state) do
    Enum.map(state.restart_history, fn {supervisor_pid, events} ->
      recent_events = Enum.take(events, 10)
      
      %{
        supervisor_pid: supervisor_pid,
        supervisor_name: get_supervisor_name(supervisor_pid, state),
        total_restarts: length(events),
        recent_restarts: length(recent_events),
        last_restart: if(events != [], do: hd(events).timestamp, else: nil)
      }
    end)
  end
end
```

### 2. Integration with Application

```elixir
# lib/otp_supervisor/application.ex
def start(_type, _args) do
  children = [
    OtpSupervisorWeb.Telemetry,
    {DNSCluster, query: Application.get_env(:otp_supervisor, :dns_cluster_query) || :ignore},
    {Phoenix.PubSub, name: OtpSupervisor.PubSub},
    {Finch, name: OtpSupervisor.Finch},
    {Registry, keys: :unique, name: TracerRegistry},
    
    # Add our analytics server
    OTPSupervisor.Core.AnalyticsServer,
    
    OtpSupervisorWeb.Endpoint,
    {OTPSupervisor.Sandbox.Supervisors.DemoSupervisor,
     name: :demo_one_for_one, strategy: :one_for_one}
  ]
  
  opts = [strategy: :one_for_one, name: OtpSupervisor.Supervisor]
  Supervisor.start_link(children, opts)
end
```

### 3. Update Control Module

```elixir
# lib/otp_supervisor/core/control.ex

# Replace the flawed restart tracking functions
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

### 4. Testing the Analytics

```elixir
defmodule OTPSupervisor.Core.AnalyticsTest do
  use ExUnit.Case, async: false  # Telemetry is global
  
  alias OTPSupervisor.Core.AnalyticsServer
  alias OTPSupervisor.Core.Control
  
  test "tracks restart events via telemetry" do
    # Get initial stats
    initial_stats = AnalyticsServer.get_all_supervisor_stats()
    
    # Create a test supervisor
    {:ok, sup_pid} = TestDemoSupervisor.start_link(name: :telemetry_test, unique_id: 123)
    
    # Kill a child process
    children = Supervisor.which_children(sup_pid)
    {_id, child_pid, _type, _modules} = hd(children)
    Process.exit(child_pid, :kill)
    
    # Wait for telemetry events to be processed
    :timer.sleep(50)
    
    # Check analytics
    {:ok, history} = Control.get_restart_history(sup_pid)
    assert length(history) > 0
    
    restart_event = hd(history)
    assert restart_event.event_type in [:terminated, :restarted]
    assert restart_event.child_pid == child_pid
    
    # Check failure rate
    {:ok, failure_rate} = Control.get_failure_rate(sup_pid, 10_000)
    assert failure_rate.restarts >= 1
  end
end
```

## Benefits of This Approach

1. **Real OTP Integration**: Uses OTP's built-in telemetry system
2. **No Race Conditions**: Events come directly from supervisor internals
3. **Dynamic Supervisor Support**: Automatically tracks all children regardless of when they're added
4. **Non-Invasive**: No changes needed to existing supervisor code
5. **Production Ready**: Telemetry is used in real production systems
6. **Educational**: Shows proper OTP monitoring patterns

## Migration Plan

1. **Add AnalyticsServer** to application supervision tree
2. **Update Control module** to use new analytics functions  
3. **Remove RestartTracker** module completely
4. **Update tests** to use new telemetry-based system
5. **Update LiveView** to display analytics from new system

This gives us **real OTP analytics** without any external simulation or monitoring hacks.