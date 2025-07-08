# Phase 1: Enhanced Prompts for OTP Supervisor Production Tool - FULLY CONTEXTUALIZED

## Overview

This document contains five comprehensive Test-Driven Development (TDD) prompts for enhancing the OTP Supervisor Production Tool. Each prompt includes ALL required reading content and context needed to execute the prompt from scratch without external dependencies.

---

## Prompt 1: Advanced Process Monitoring with TDD

**Objective**: Implement comprehensive process monitoring capabilities with real-time metrics collection and historical data tracking.

### Required Reading Phase

Before writing any code, you MUST read and understand:

#### 1. Test Design Standards (docsDesign/code-standards/test-design-reference.md)

**TDD Methodology Requirements:**
- Write failing tests FIRST before any implementation
- Use descriptive test names that explain behavior
- Follow AAA pattern: Arrange, Act, Assert
- Test isolation: Each test must be independent
- Use setup/teardown for consistent test state
- Minimum 95% test coverage for all new code

**Test Isolation Patterns:**
```elixir
# Good - Isolated test setup
setup do
  supervisor_name = :"test_supervisor_#{:erlang.unique_integer([:positive])}"
  {:ok, sup_pid} = TestSupervisor.start_link(name: supervisor_name)
  
  on_exit(fn ->
    if Process.alive?(sup_pid), do: Process.exit(sup_pid, :kill)
  end)
  
  %{supervisor: supervisor_name, sup_pid: sup_pid}
end

# Bad - Shared global state
setup do
  supervisor = :demo_one_for_one  # Global shared supervisor!
  %{supervisor: supervisor}
end
```

**Test Structure Guidelines:**
- Group related tests with `describe` blocks
- Use `async: true` only when tests don't share global state
- Include edge cases and error conditions
- Test both success and failure scenarios

#### 2. Existing Process Monitoring (lib/otp_supervisor/core/control.ex)

**Current Implementation Analysis:**
```elixir
# Current process monitoring capabilities:

@doc """
Lists all processes in the system with optional filtering.
"""
def list_all_processes(opts \\ []) do
  filter = Keyword.get(opts, :filter, :all)
  limit = Keyword.get(opts, :limit, :infinity)

  processes =
    Process.list()
    |> Stream.map(&process_info_with_type/1)
    |> Stream.filter(&filter_process(&1, filter))

  limited_processes =
    case limit do
      :infinity -> Enum.to_list(processes)
      n when is_integer(n) and n > 0 -> Enum.take(processes, n)
    end

  Enum.sort_by(limited_processes, & &1.name)
end

@doc """
Gets detailed information about a process.
"""
def get_process_info(pid) when is_pid(pid) do
  keys = [
    :memory,
    :message_queue_len,
    :status,
    :heap_size,
    :stack_size,
    :reductions,
    :current_function
  ]

  case Process.info(pid, keys) do
    nil -> {:error, :process_dead}
    info -> {:ok, Map.new(info)}
  end
end

@doc """
Builds a complete process relationship graph.
"""
def build_process_graph do
  processes = list_all_processes()

  %{
    processes: processes,
    links: build_link_graph(processes),
    monitors: build_monitor_graph(processes)
  }
end
```

**Key Data Structures:**
- Process info maps include: `%{pid: string, name: atom, type: atom}`
- Process types: `:supervisor`, `:genserver`, `:worker`, `:dead`
- Process relationships tracked through `:links` and `:monitors`

#### 3. Performance Metrics Pattern (lib/otp_supervisor_web/live/system_dashboard_live.ex)

**Current Real-Time Update Mechanism:**
```elixir
@impl true
def mount(_params, _session, socket) do
  if connected?(socket) do
    :timer.send_interval(1000, self(), :update_metrics)
  end
  
  socket =
    socket
    |> assign(:page_title, "System Dashboard")
    |> assign(:system_metrics, get_system_metrics())
    |> assign(:anomalies, detect_anomalies())
    
  {:ok, socket}
end

@impl true
def handle_info(:update_metrics, socket) do
  socket =
    socket
    |> assign(:system_metrics, get_system_metrics())
    |> assign(:anomalies, detect_anomalies())
    
  {:noreply, socket}
end

defp get_system_metrics do
  %{
    total_processes: length(Process.list()),
    memory_usage: :erlang.memory(:total),
    message_queue_lengths: get_queue_lengths(),
    cpu_usage: get_cpu_usage(),
    supervision_health: calculate_supervision_health()
  }
end
```

**LiveView Integration Patterns:**
- Use `connected?/1` to avoid timer setup during initial render
- Update metrics every 1000ms for real-time feel without overwhelming
- Handle errors gracefully in metrics collection
- Use assigns for state management

### Test-First Implementation Phase

Write comprehensive failing tests that cover:

#### 1. Process Metrics Collection
```elixir
defmodule OTPSupervisor.Core.ProcessMonitorTest do
  use ExUnit.Case, async: true
  alias OTPSupervisor.Core.ProcessMonitor
  
  describe "process metrics collection" do
    test "collects comprehensive process metrics including memory, reductions, and message queue stats" do
      {:ok, monitor} = ProcessMonitor.start_link([])
      
      # Create a test process
      {:ok, test_pid} = Agent.start_link(fn -> 0 end)
      
      metrics = ProcessMonitor.collect_metrics(test_pid)
      
      assert %{
        memory: memory,
        reductions: reductions,
        message_queue_len: queue_len,
        timestamp: timestamp
      } = metrics
      
      assert is_integer(memory) and memory > 0
      assert is_integer(reductions) and reductions >= 0  
      assert is_integer(queue_len) and queue_len >= 0
      assert is_integer(timestamp)
    end
    
    test "tracks process metrics over time with configurable sampling intervals" do
      {:ok, monitor} = ProcessMonitor.start_link(sampling_interval: 100)
      {:ok, test_pid} = Agent.start_link(fn -> 0 end)
      
      ProcessMonitor.start_tracking(monitor, test_pid)
      
      # Wait for multiple samples
      Process.sleep(250)
      
      history = ProcessMonitor.get_history(monitor, test_pid)
      assert length(history) >= 2
      
      # Verify chronological order
      timestamps = Enum.map(history, & &1.timestamp)
      assert timestamps == Enum.sort(timestamps)
    end
    
    test "handles edge cases like process death during metrics collection" do
      {:ok, monitor} = ProcessMonitor.start_link([])
      {:ok, test_pid} = Agent.start_link(fn -> 0 end)
      
      ProcessMonitor.start_tracking(monitor, test_pid)
      
      # Kill the process
      Process.exit(test_pid, :kill)
      Process.sleep(50)
      
      # Should handle gracefully
      assert {:error, :process_dead} = ProcessMonitor.collect_metrics(test_pid)
      
      # Should clean up tracking
      assert [] = ProcessMonitor.get_tracked_processes(monitor)
    end
  end
end
```

#### 2. Real-Time Monitoring
```elixir
describe "real-time monitoring" do
  test "provides real-time process monitoring with subscribe/unsubscribe functionality" do
    {:ok, monitor} = ProcessMonitor.start_link([])
    {:ok, test_pid} = Agent.start_link(fn -> 0 end)
    
    # Subscribe to updates
    :ok = ProcessMonitor.subscribe(monitor, test_pid)
    
    # Start tracking
    ProcessMonitor.start_tracking(monitor, test_pid)
    
    # Should receive update messages
    assert_receive {:process_metrics, ^test_pid, metrics}, 1000
    assert is_map(metrics)
  end
  
  test "broadcasts process state changes to subscribed observers" do
    {:ok, monitor} = ProcessMonitor.start_link([])
    {:ok, test_pid} = Agent.start_link(fn -> 0 end)
    
    ProcessMonitor.subscribe(monitor, test_pid)
    ProcessMonitor.start_tracking(monitor, test_pid)
    
    # Change process state
    Agent.update(test_pid, &(&1 + 1))
    
    # Should broadcast state change
    assert_receive {:process_state_change, ^test_pid, _old_state, _new_state}, 1000
  end
  
  test "handles high-frequency updates without overwhelming subscribers" do
    {:ok, monitor} = ProcessMonitor.start_link(sampling_interval: 10)
    {:ok, test_pid} = Agent.start_link(fn -> 0 end)
    
    ProcessMonitor.subscribe(monitor, test_pid)
    ProcessMonitor.start_tracking(monitor, test_pid)
    
    # Generate rapid state changes
    for _ <- 1..100, do: Agent.update(test_pid, &(&1 + 1))
    
    # Should throttle updates
    message_count = count_messages_in_timeframe(500)
    assert message_count < 50  # Should be throttled
  end
end
```

#### 3. Historical Data Management
```elixir
describe "historical data management" do
  test "maintains historical process metrics with configurable retention periods" do
    {:ok, monitor} = ProcessMonitor.start_link(retention_hours: 1)
    {:ok, test_pid} = Agent.start_link(fn -> 0 end)
    
    ProcessMonitor.start_tracking(monitor, test_pid)
    
    # Simulate old data
    old_timestamp = System.system_time(:millisecond) - 2 * 60 * 60 * 1000  # 2 hours ago
    ProcessMonitor.add_historical_data(monitor, test_pid, %{timestamp: old_timestamp})
    
    # Trigger cleanup
    ProcessMonitor.cleanup_old_data(monitor)
    
    history = ProcessMonitor.get_history(monitor, test_pid)
    
    # Old data should be pruned
    assert Enum.all?(history, fn metric ->
      (System.system_time(:millisecond) - metric.timestamp) < 60 * 60 * 1000
    end)
  end
  
  test "provides efficient querying of historical data by process, time range, and metric type" do
    {:ok, monitor} = ProcessMonitor.start_link([])
    {:ok, test_pid} = Agent.start_link(fn -> 0 end)
    
    # Add sample data
    now = System.system_time(:millisecond)
    sample_data = [
      %{timestamp: now - 300_000, memory: 1000, type: :memory},
      %{timestamp: now - 200_000, memory: 2000, type: :memory},
      %{timestamp: now - 100_000, reductions: 500, type: :reductions}
    ]
    
    for data <- sample_data do
      ProcessMonitor.add_historical_data(monitor, test_pid, data)
    end
    
    # Query by time range
    recent_data = ProcessMonitor.query_history(
      monitor, 
      test_pid, 
      time_range: {now - 250_000, now}
    )
    assert length(recent_data) == 2
    
    # Query by metric type
    memory_data = ProcessMonitor.query_history(
      monitor,
      test_pid,
      metric_type: :memory
    )
    assert length(memory_data) == 2
  end
  
  test "automatically prunes old data to prevent memory leaks" do
    {:ok, monitor} = ProcessMonitor.start_link(
      retention_hours: 1,
      cleanup_interval: 100
    )
    
    {:ok, test_pid} = Agent.start_link(fn -> 0 end)
    
    # Add old data that should be pruned
    old_timestamp = System.system_time(:millisecond) - 2 * 60 * 60 * 1000
    ProcessMonitor.add_historical_data(monitor, test_pid, %{timestamp: old_timestamp})
    
    # Wait for automatic cleanup
    Process.sleep(150)
    
    history = ProcessMonitor.get_history(monitor, test_pid)
    assert Enum.empty?(history)
  end
end
```

### Implementation Requirements

Create `lib/otp_supervisor/core/process_monitor.ex` with:

```elixir
defmodule OTPSupervisor.Core.ProcessMonitor do
  @moduledoc """
  Provides comprehensive process monitoring with real-time metrics collection 
  and historical data tracking.
  
  This GenServer coordinates monitoring activities for multiple processes,
  collects metrics at configurable intervals, and maintains historical data
  with automatic cleanup.
  """
  
  use GenServer
  require Logger
  
  # Client API
  
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def collect_metrics(pid) when is_pid(pid) do
    GenServer.call(__MODULE__, {:collect_metrics, pid})
  end
  
  def start_tracking(monitor \\ __MODULE__, pid) when is_pid(pid) do
    GenServer.call(monitor, {:start_tracking, pid})
  end
  
  def stop_tracking(monitor \\ __MODULE__, pid) when is_pid(pid) do
    GenServer.call(monitor, {:stop_tracking, pid})
  end
  
  def subscribe(monitor \\ __MODULE__, pid) when is_pid(pid) do
    GenServer.call(monitor, {:subscribe, pid})
  end
  
  def get_history(monitor \\ __MODULE__, pid) when is_pid(pid) do
    GenServer.call(monitor, {:get_history, pid})
  end
  
  # Server Implementation
  
  @impl true
  def init(opts) do
    sampling_interval = Keyword.get(opts, :sampling_interval, 1000)
    retention_hours = Keyword.get(opts, :retention_hours, 24)
    cleanup_interval = Keyword.get(opts, :cleanup_interval, 60_000)
    
    # Schedule periodic tasks
    :timer.send_interval(sampling_interval, :collect_metrics)
    :timer.send_interval(cleanup_interval, :cleanup_old_data)
    
    state = %{
      tracked_processes: %{},
      subscribers: %{},
      historical_data: %{},
      sampling_interval: sampling_interval,
      retention_ms: retention_hours * 60 * 60 * 1000
    }
    
    {:ok, state}
  end
  
  @impl true
  def handle_call({:collect_metrics, pid}, _from, state) do
    case collect_process_metrics(pid) do
      {:ok, metrics} ->
        {:reply, metrics, state}
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
  
  # ... additional implementation
end
```

### Compliance Review Phase

Verify that your implementation:
- ✅ Uses only standard Erlang/OTP process introspection APIs (`:erlang.process_info/2`)
- ✅ Follows proper GenServer patterns with supervision  
- ✅ Implements robust error handling for process death scenarios
- ✅ Maintains test coverage above 95% for all public functions
- ✅ Documents all public APIs with `@doc` and `@spec` annotations

---

## Prompt 2: Enhanced Supervisor Analytics with TDD

**Objective**: Implement advanced supervisor analytics including restart pattern analysis, failure prediction, and performance optimization recommendations.

### Required Reading Phase

#### 1. Existing Analytics (lib/otp_supervisor/core/control.ex)

**Current Supervisor Operations:**
```elixir
@doc """
Lists all registered supervisors in the system.
"""
def list_supervisors do
  Process.registered()
  |> Enum.filter(&is_supervisor?/1)
  |> Enum.map(&format_supervisor_info/1)
end

@doc """
Gets the supervision tree for a given supervisor.
"""
def get_supervision_tree(supervisor_name) when is_atom(supervisor_name) do
  case Process.whereis(supervisor_name) do
    nil -> {:error, :not_found}
    pid -> get_supervision_tree(pid)
  end
end

def get_supervision_tree(supervisor_pid) when is_pid(supervisor_pid) do
  try do
    children = Supervisor.which_children(supervisor_pid)
    {:ok, format_children(children)}
  rescue
    ArgumentError -> {:error, :not_supervisor}
    FunctionClauseError -> {:error, :not_supervisor}
    e -> {:error, Exception.message(e)}
  end
end

# Helper function to identify supervisors
defp is_supervisor_pid?(pid) when is_pid(pid) do
  case Process.info(pid, :dictionary) do
    nil -> false
    {:dictionary, dict} ->
      initial_call = Keyword.get(dict, :"$initial_call", false)
      case initial_call do
        {mod, _, _} when mod in [:supervisor, Supervisor, DynamicSupervisor, PartitionSupervisor, Task.Supervisor] ->
          true
        _ -> false
      end
  end
end
```

**Supervision Tree Traversal Patterns:**
- Use `Supervisor.which_children/1` for immediate children
- Recursively traverse tree for deep analysis
- Handle supervisor/worker type detection
- Format child information consistently

#### 2. LiveView Analytics Display (lib/otp_supervisor_web/live/supervisor_live.ex)

**Current Presentation Patterns:**
```elixir
@impl true
def mount(_params, _session, socket) do
  if connected?(socket) do
    :timer.send_interval(@refresh_interval, self(), :refresh)
  end

  {:ok,
   socket
   |> assign(:supervisors, Control.list_supervisors())
   |> assign(:selected_supervisor, nil)
   |> assign(:children, [])
   |> assign(:selected_process, nil)
   |> assign(:process_info, nil)}
end

@impl true
def handle_event("select_supervisor", %{"name" => name}, socket) do
  {:noreply,
   socket
   |> select_supervisor(name)
   |> push_patch(to: ~p"/supervisors?supervisor=#{name}")}
end

@impl true
def handle_event("kill_process", %{"pid" => pid_string}, socket) do
  case Control.kill_process(pid_string) do
    :ok ->
      send(self(), :refresh)
      {:noreply, put_flash(socket, :info, "Process killed: #{pid_string}")}
    {:error, :invalid_pid} ->
      {:noreply, put_flash(socket, :error, "Invalid PID: #{pid_string}")}
  end
end

@impl true
def handle_info(:refresh, socket) do
  socket = socket |> assign(:supervisors, Control.list_supervisors())
  
  socket = if socket.assigns.selected_supervisor do
    refresh_children(socket)
  else
    socket
  end
  
  {:noreply, socket}
end
```

**Real-Time Update Mechanisms:**
- Timer-based refresh every 1000ms
- Event-driven updates on user actions
- Immediate refresh after process operations
- Error handling with user feedback

#### 3. Test Infrastructure (test/support/supervisor_test_helper.ex)

**Current Test Helper Patterns:**
```elixir
defmodule SupervisorTestHelper do
  @moduledoc """
  Helper functions for testing supervisor behavior with proper isolation.
  """

  def setup_isolated_supervisor(base_name) do
    unique_id = :erlang.unique_integer([:positive])
    supervisor_name = :"#{base_name}_supervisor_#{unique_id}"
    
    {:ok, sup_pid} = OTPSupervisor.Sandbox.Supervisors.DemoSupervisor.start_link(
      name: supervisor_name,
      strategy: :one_for_one
    )
    
    on_exit(fn ->
      if Process.alive?(sup_pid) do
        Process.exit(sup_pid, :kill)
      end
    end)
    
    %{supervisor: supervisor_name, sup_pid: sup_pid}
  end

  def wait_for_process_restart(supervisor, old_pid) do
    wait_for_condition(fn ->
      case Control.get_supervision_tree(supervisor) do
        {:ok, children} ->
          current_pids = Enum.map(children, &(&1.pid))
          inspect(old_pid) not in current_pids
        _ -> false
      end
    end, 1000)
  end

  defp wait_for_condition(condition_fn, timeout_ms) do
    end_time = System.monotonic_time(:millisecond) + timeout_ms
    wait_for_condition_loop(condition_fn, end_time)
  end
end
```

**Isolation Patterns:**
- Unique supervisor names using `:erlang.unique_integer/1`
- Proper cleanup with `on_exit/1` callbacks
- Wait helpers for asynchronous operations
- Conditional waiting with timeouts

### Test-First Implementation Phase

Write failing tests for:

#### 1. Restart Pattern Analysis
```elixir
describe "restart pattern analysis" do
  setup do
    SupervisorTestHelper.setup_isolated_supervisor("analytics")
  end

  test "analyzes restart patterns to identify problematic children", %{supervisor: supervisor} do
    analytics = SupervisorAnalytics.start_link(supervisor: supervisor)
    
    # Simulate multiple restarts of same child
    {:ok, children} = Control.get_supervision_tree(supervisor)
    problem_child = List.first(children)
    
    # Cause 3 restarts
    for _ <- 1..3 do
      Control.kill_process(problem_child.pid)
      SupervisorTestHelper.wait_for_process_restart(supervisor, problem_child.pid)
    end
    
    patterns = SupervisorAnalytics.analyze_restart_patterns(analytics)
    
    assert %{
      problematic_children: problematic,
      restart_frequency: frequency
    } = patterns
    
    assert length(problematic) > 0
    assert frequency > 0
  end
  
  test "calculates restart frequency and intensity metrics", %{supervisor: supervisor} do
    analytics = SupervisorAnalytics.start_link(supervisor: supervisor)
    
    # Record baseline
    baseline = SupervisorAnalytics.get_metrics(analytics)
    
    # Cause restarts
    {:ok, children} = Control.get_supervision_tree(supervisor)
    child = List.first(children)
    
    start_time = System.system_time(:millisecond)
    Control.kill_process(child.pid)
    SupervisorTestHelper.wait_for_process_restart(supervisor, child.pid)
    
    metrics = SupervisorAnalytics.get_metrics(analytics)
    
    assert %{
      restart_count: count,
      restart_frequency: frequency,
      average_restart_time: avg_time
    } = metrics
    
    assert count > baseline.restart_count
    assert frequency > 0
    assert is_number(avg_time)
  end
  
  test "detects restart storms and provides early warnings", %{supervisor: supervisor} do
    analytics = SupervisorAnalytics.start_link(
      supervisor: supervisor,
      storm_threshold: 3,
      storm_window_ms: 1000
    )
    
    {:ok, children} = Control.get_supervision_tree(supervisor)
    child = List.first(children)
    
    # Cause rapid restarts (storm)
    for _ <- 1..4 do
      Control.kill_process(child.pid)
      SupervisorTestHelper.wait_for_process_restart(supervisor, child.pid)
    end
    
    warnings = SupervisorAnalytics.get_warnings(analytics)
    
    assert Enum.any?(warnings, fn warning ->
      warning.type == :restart_storm
    end)
  end
end
```

#### 2. Performance Analytics
```elixir
describe "performance analytics" do
  test "measures supervisor overhead and child startup times", %{supervisor: supervisor} do
    analytics = SupervisorAnalytics.start_link(supervisor: supervisor)
    
    # Measure startup time
    {:ok, children} = Control.get_supervision_tree(supervisor)
    child = List.first(children)
    
    start_time = System.monotonic_time(:microsecond)
    Control.kill_process(child.pid)
    SupervisorTestHelper.wait_for_process_restart(supervisor, child.pid)
    restart_time = System.monotonic_time(:microsecond) - start_time
    
    performance = SupervisorAnalytics.get_performance_metrics(analytics)
    
    assert %{
      average_startup_time: avg_startup,
      supervisor_overhead: overhead,
      restart_efficiency: efficiency
    } = performance
    
    assert is_number(avg_startup) and avg_startup > 0
    assert is_number(overhead) and overhead >= 0
    assert is_number(efficiency) and efficiency > 0
  end
  
  test "analyzes memory usage patterns across supervision trees", %{supervisor: supervisor} do
    analytics = SupervisorAnalytics.start_link(supervisor: supervisor)
    
    # Collect baseline memory usage
    baseline_memory = SupervisorAnalytics.get_memory_analysis(analytics)
    
    # Cause memory pressure
    {:ok, children} = Control.get_supervision_tree(supervisor)
    for child <- children do
      # Simulate memory-intensive operation
      send(child.pid, {:allocate_memory, 1024 * 1024})  # 1MB
    end
    
    Process.sleep(100)  # Allow memory allocation
    
    current_memory = SupervisorAnalytics.get_memory_analysis(analytics)
    
    assert %{
      total_memory: total,
      memory_per_child: per_child,
      memory_growth_rate: growth_rate
    } = current_memory
    
    assert total > baseline_memory.total_memory
    assert is_list(per_child)
    assert is_number(growth_rate)
  end
  
  test "provides performance recommendations based on metrics", %{supervisor: supervisor} do
    analytics = SupervisorAnalytics.start_link(supervisor: supervisor)
    
    # Simulate performance issues
    # High restart frequency
    {:ok, children} = Control.get_supervision_tree(supervisor)
    child = List.first(children)
    
    for _ <- 1..5 do
      Control.kill_process(child.pid)
      SupervisorTestHelper.wait_for_process_restart(supervisor, child.pid)
    end
    
    recommendations = SupervisorAnalytics.get_recommendations(analytics)
    
    assert is_list(recommendations)
    assert Enum.any?(recommendations, fn rec ->
      rec.type == :high_restart_frequency and
      rec.severity in [:warning, :critical]
    end)
  end
end
```

#### 3. Failure Prediction
```elixir
describe "failure prediction" do
  test "identifies supervisors at risk of restart storms", %{supervisor: supervisor} do
    analytics = SupervisorAnalytics.start_link(supervisor: supervisor)
    
    # Create restart pattern that indicates risk
    {:ok, children} = Control.get_supervision_tree(supervisor)
    child = List.first(children)
    
    # Pattern: increasing restart frequency
    for i <- 1..3 do
      Control.kill_process(child.pid)
      SupervisorTestHelper.wait_for_process_restart(supervisor, child.pid)
      Process.sleep(1000 - (i * 200))  # Decreasing intervals
    end
    
    risk_assessment = SupervisorAnalytics.assess_storm_risk(analytics)
    
    assert %{
      risk_level: risk,
      confidence: confidence,
      indicators: indicators
    } = risk_assessment
    
    assert risk in [:low, :medium, :high, :critical]
    assert confidence >= 0.0 and confidence <= 1.0
    assert is_list(indicators)
  end
  
  test "predicts likely failure points based on historical data", %{supervisor: supervisor} do
    analytics = SupervisorAnalytics.start_link(supervisor: supervisor)
    
    # Build historical pattern
    {:ok, children} = Control.get_supervision_tree(supervisor)
    
    # Simulate pattern where one child fails more often
    frequent_failure_child = List.first(children)
    occasional_failure_child = List.last(children)
    
    # Create failure pattern
    for _ <- 1..4 do
      Control.kill_process(frequent_failure_child.pid)
      SupervisorTestHelper.wait_for_process_restart(supervisor, frequent_failure_child.pid)
    end
    
    Control.kill_process(occasional_failure_child.pid)
    SupervisorTestHelper.wait_for_process_restart(supervisor, occasional_failure_child.pid)
    
    predictions = SupervisorAnalytics.predict_failure_points(analytics)
    
    assert is_list(predictions)
    
    frequent_prediction = Enum.find(predictions, fn p ->
      p.child_id == frequent_failure_child.id
    end)
    
    occasional_prediction = Enum.find(predictions, fn p ->
      p.child_id == occasional_failure_child.id
    end)
    
    assert frequent_prediction.failure_probability > occasional_prediction.failure_probability
  end
  
  test "provides actionable recommendations for supervision strategy adjustments", %{supervisor: supervisor} do
    analytics = SupervisorAnalytics.start_link(supervisor: supervisor)
    
    # Simulate scenario requiring strategy change
    # All children failing together suggests one_for_all might be better
    {:ok, children} = Control.get_supervision_tree(supervisor)
    
    # Kill all children simultaneously multiple times
    for _ <- 1..2 do
      for child <- children do
        Control.kill_process(child.pid)
      end
      SupervisorTestHelper.wait_for_supervisor_restart(supervisor)
    end
    
    strategy_recommendations = SupervisorAnalytics.recommend_strategy_changes(analytics)
    
    assert is_list(strategy_recommendations)
    
    strategy_rec = Enum.find(strategy_recommendations, fn rec ->
      rec.type == :strategy_change
    end)
    
    assert strategy_rec != nil
    assert strategy_rec.current_strategy == :one_for_one
    assert strategy_rec.recommended_strategy in [:one_for_all, :rest_for_one]
    assert is_binary(strategy_rec.reasoning)
  end
end
```

### Implementation Requirements

Create `lib/otp_supervisor/core/supervisor_analytics.ex` with:

```elixir
defmodule OTPSupervisor.Core.SupervisorAnalytics do
  @moduledoc """
  Provides advanced supervisor analytics including restart pattern analysis,
  failure prediction, and performance optimization recommendations.
  
  This module monitors supervisor behavior without interfering with normal
  operation, using only observer patterns and statistical analysis.
  """
  
  use GenServer
  require Logger
  
  # Client API
  
  def start_link(opts) do
    supervisor = Keyword.fetch!(opts, :supervisor)
    GenServer.start_link(__MODULE__, opts, name: via_tuple(supervisor))
  end
  
  def analyze_restart_patterns(analytics_pid) when is_pid(analytics_pid) do
    GenServer.call(analytics_pid, :analyze_restart_patterns)
  end
  
  def get_metrics(analytics_pid) when is_pid(analytics_pid) do
    GenServer.call(analytics_pid, :get_metrics)
  end
  
  def assess_storm_risk(analytics_pid) when is_pid(analytics_pid) do
    GenServer.call(analytics_pid, :assess_storm_risk)
  end
  
  # Server Implementation
  
  @impl true
  def init(opts) do
    supervisor = Keyword.fetch!(opts, :supervisor)
    storm_threshold = Keyword.get(opts, :storm_threshold, 5)
    storm_window_ms = Keyword.get(opts, :storm_window_ms, 60_000)
    
    # Set up monitoring without interfering with supervisor
    Process.monitor(supervisor)
    
    state = %{
      supervisor: supervisor,
      restart_events: [],
      performance_metrics: %{},
      storm_threshold: storm_threshold,
      storm_window_ms: storm_window_ms,
      last_analysis: System.system_time(:millisecond)
    }
    
    # Schedule periodic analysis
    :timer.send_interval(5000, :analyze)
    
    {:ok, state}
  end
  
  # ... implementation continues
end
```

### Compliance Review Phase

Ensure your implementation:
- ✅ Never interferes with normal supervisor operation
- ✅ Uses only observer patterns, never controller patterns  
- ✅ Provides actionable insights backed by data
- ✅ Maintains comprehensive test coverage
- ✅ Includes detailed documentation and examples

---

## Prompt 3: Interactive Process Debugging Tools with TDD

**Objective**: Create sophisticated debugging tools for process inspection, message tracing, and state analysis for production OTP monitoring.

### Required Reading Phase

#### 1. Message Tracing (lib/otp_supervisor/core/message_tracer.ex)

**Current Tracing Implementation:**
```elixir
defmodule OTPSupervisor.Core.MessageTracer do
  @moduledoc """
  Provides message tracing capabilities for debugging message flow in OTP systems.
  """
  
  use GenServer
  
  def trace_messages(pid, opts \\ []) when is_pid(pid) do
    max_messages = Keyword.get(opts, :max_messages, 100)
    
    case GenServer.start(__MODULE__, {pid, max_messages}) do
      {:ok, tracer_pid} ->
        :erlang.trace(pid, true, [:send, :receive, {tracer_pid, []}])
        {:ok, tracer_pid}
      error -> error
    end
  end
  
  def get_message_history(pid) do
    case Registry.lookup(TracerRegistry, pid) do
      [{tracer_pid, _}] -> GenServer.call(tracer_pid, :get_messages)
      [] -> []
    end
  end
  
  def stop_tracing(pid) do
    case Registry.lookup(TracerRegistry, pid) do
      [{tracer_pid, _}] ->
        GenServer.stop(tracer_pid)
        :ok
      [] -> :ok
    end
  end
  
  # Server callbacks
  def init({traced_pid, max_messages}) do
    Registry.register(TracerRegistry, traced_pid, nil)
    {:ok, %{traced_pid: traced_pid, messages: [], max_messages: max_messages}}
  end
  
  def handle_info({:trace, pid, :send, message, to}, state) do
    new_message = %{
      timestamp: System.system_time(:millisecond),
      direction: :outgoing,
      content: message,
      to: to
    }
    {:noreply, add_message(state, new_message)}
  end
  
  def handle_info({:trace, pid, :receive, message}, state) do
    new_message = %{
      timestamp: System.system_time(:millisecond),
      direction: :incoming,
      content: message
    }
    {:noreply, add_message(state, new_message)}
  end
end
```

**Current Capabilities:**
- Basic send/receive tracing using `:erlang.trace/3`
- Message history with timestamps
- Registry-based tracer management
- Bounded message storage

**Performance Considerations:**
- Tracing has performance impact - use judiciously
- Message storage is bounded to prevent memory issues
- Registry lookup for efficient tracer management

#### 2. Process State Access (lib/otp_supervisor/core/control.ex)

**Current State Inspection:**
```elixir
@doc """
Gets the state of a GenServer process safely.
"""
def get_process_state(pid) when is_pid(pid) do
  try do
    state = :sys.get_state(pid, 100)
    {:ok, state}
  rescue
    _ -> {:error, :not_a_genserver}
  catch
    :exit, _ -> {:error, :not_a_genserver}
  end
end
```

**Safe State Access Patterns:**
- Use `:sys.get_state/2` with timeout for safety
- Handle non-GenServer processes gracefully
- Timeout prevents hanging on unresponsive processes
- Comprehensive error handling for all failure modes

#### 3. Web Interface Patterns (lib/otp_supervisor_web/live/)

**LiveView Organization Patterns:**
```elixir
# supervisor_live.ex structure:
defmodule OtpSupervisorWeb.SupervisorLive do
  use OtpSupervisorWeb, :live_view
  
  # State management
  @impl true
  def mount(_params, _session, socket) do
    # Initialize socket state
  end
  
  # URL parameter handling
  @impl true  
  def handle_params(params, _url, socket) do
    # Handle route parameters
  end
  
  # User interactions
  @impl true
  def handle_event("select_supervisor", %{"name" => name}, socket) do
    # Handle user selections
  end
  
  # Real-time updates
  @impl true
  def handle_info(:refresh, socket) do
    # Handle timer-based updates
  end
end
```

**User Interaction Patterns:**
- Event-driven state updates
- URL parameter integration for deep linking
- Flash messages for user feedback
- Graceful error handling with user-friendly messages

### Test-First Implementation Phase

#### 1. Advanced Message Tracing
```elixir
describe "advanced message tracing" do
  setup do
    SupervisorTestHelper.setup_isolated_supervisor("debug_tracing")
  end

  test "traces messages with filtering by type, sender, and content patterns", %{supervisor: supervisor} do
    {:ok, children} = Control.get_supervision_tree(supervisor)
    target_child = List.first(children)
    {:ok, target_pid} = Control.to_pid(target_child.pid)
    
    # Start tracing with filters
    {:ok, debugger} = ProcessDebugger.start_link(target_pid)
    
    :ok = ProcessDebugger.start_message_tracing(debugger, 
      filters: [
        {:message_type, :cast},
        {:sender_pattern, ~r/counter/},
        {:content_pattern, ~r/increment/}
      ]
    )
    
    # Send various messages
    GenServer.cast(target_pid, :increment)  # Should be traced
    GenServer.call(target_pid, :get_value)  # Should be filtered out
    GenServer.cast(target_pid, :decrement)  # Should be filtered out
    send(target_pid, {:custom, :message})   # Should be filtered out
    
    Process.sleep(50)  # Allow tracing
    
    traced_messages = ProcessDebugger.get_traced_messages(debugger)
    
    assert length(traced_messages) == 1
    assert List.first(traced_messages).content == :increment
  end
  
  test "provides message flow visualization between processes", %{supervisor: supervisor} do
    {:ok, children} = Control.get_supervision_tree(supervisor)
    
    # Set up multi-process tracing
    child_pids = Enum.map(children, fn child ->
      {:ok, pid} = Control.to_pid(child.pid)
      pid
    end)
    
    {:ok, debugger} = ProcessDebugger.start_link()
    
    for pid <- child_pids do
      ProcessDebugger.add_trace_target(debugger, pid)
    end
    
    ProcessDebugger.start_flow_tracing(debugger)
    
    # Create message flow
    [pid1, pid2 | _] = child_pids
    send(pid1, {:forward_to, pid2, :test_message})
    send(pid2, {:respond_to, pid1, :acknowledgment})
    
    Process.sleep(50)
    
    flow_diagram = ProcessDebugger.get_message_flow(debugger)
    
    assert %{
      nodes: nodes,
      edges: edges,
      timeline: timeline
    } = flow_diagram
    
    assert length(nodes) >= 2
    assert length(edges) >= 2
    assert is_list(timeline)
  end
  
  test "handles high-volume message tracing without performance degradation", %{supervisor: supervisor} do
    {:ok, children} = Control.get_supervision_tree(supervisor)
    target_child = List.first(children)
    {:ok, target_pid} = Control.to_pid(target_child.pid)
    
    {:ok, debugger} = ProcessDebugger.start_link(target_pid, 
      buffer_size: 1000,
      sampling_rate: 0.1  # Sample 10% of messages
    )
    
    ProcessDebugger.start_message_tracing(debugger)
    
    # Generate high message volume
    start_time = System.monotonic_time(:microsecond)
    
    for _ <- 1..10_000 do
      GenServer.cast(target_pid, :increment)
    end
    
    end_time = System.monotonic_time(:microsecond)
    elapsed_ms = (end_time - start_time) / 1000
    
    # Should complete within reasonable time even with tracing
    assert elapsed_ms < 1000  # Less than 1 second
    
    traced_messages = ProcessDebugger.get_traced_messages(debugger)
    
    # Should have sampled messages, not all
    assert length(traced_messages) < 10_000
    assert length(traced_messages) > 0
  end
end
```

#### 2. State Inspection Tools
```elixir
describe "state inspection tools" do
  test "provides deep inspection of GenServer state with type information", %{supervisor: supervisor} do
    {:ok, children} = Control.get_supervision_tree(supervisor)
    counter_child = Enum.find(children, &(&1.id == :counter_1))
    {:ok, counter_pid} = Control.to_pid(counter_child.pid)
    
    {:ok, debugger} = ProcessDebugger.start_link(counter_pid)
    
    # Modify state
    GenServer.cast(counter_pid, :increment)
    GenServer.cast(counter_pid, :increment)
    
    state_inspection = ProcessDebugger.inspect_state(debugger)
    
    assert %{
      current_state: state,
      state_type: type_info,
      state_size: size,
      state_structure: structure
    } = state_inspection
    
    assert state.value == 2
    assert type_info == :map
    assert is_integer(size) and size > 0
    assert is_map(structure)
  end
  
  test "tracks state changes over time with diff capabilities", %{supervisor: supervisor} do
    {:ok, children} = Control.get_supervision_tree(supervisor)
    counter_child = Enum.find(children, &(&1.id == :counter_1))
    {:ok, counter_pid} = Control.to_pid(counter_child.pid)
    
    {:ok, debugger} = ProcessDebugger.start_link(counter_pid)
    ProcessDebugger.start_state_tracking(debugger)
    
    # Capture initial state
    initial_state = ProcessDebugger.get_current_state(debugger)
    
    # Make changes
    GenServer.cast(counter_pid, :increment)
    Process.sleep(10)
    GenServer.cast(counter_pid, :increment)
    Process.sleep(10)
    
    state_history = ProcessDebugger.get_state_history(debugger)
    
    assert length(state_history) >= 3  # Initial + 2 changes
    
    # Test diff capabilities
    diff = ProcessDebugger.diff_states(debugger, 0, 2)  # Compare first and last
    
    assert %{
      before: before_state,
      after: after_state,
      changes: changes
    } = diff
    
    assert before_state.value == 0
    assert after_state.value == 2
    assert Enum.any?(changes, fn change ->
      change.field == :value and change.type == :modified
    end)
  end
  
  test "safely handles complex data structures and circular references", %{supervisor: supervisor} do
    # Create a process with complex state
    complex_state = %{
      simple_value: 42,
      list_data: [1, 2, 3, %{nested: "value"}],
      tuple_data: {:ok, %{status: :active}},
      large_binary: :crypto.strong_rand_bytes(1024),
      circular_ref: nil  # Will be set to self-reference
    }
    
    # Add circular reference
    complex_state = %{complex_state | circular_ref: complex_state}
    
    {:ok, test_pid} = Agent.start_link(fn -> complex_state end)
    
    {:ok, debugger} = ProcessDebugger.start_link(test_pid)
    
    # Should handle complex state without crashing
    state_inspection = ProcessDebugger.inspect_state(debugger)
    
    assert %{
      current_state: state,
      state_type: type_info,
      serialization_info: serial_info
    } = state_inspection
    
    # Should detect circular reference
    assert serial_info.has_circular_references == true
    assert serial_info.serializable == false
    
    # Should still provide useful information
    assert is_map(state)
    assert state.simple_value == 42
  end
end
```

#### 3. Interactive Debugging Interface
```elixir
describe "interactive debugging interface" do
  test "allows real-time interaction with processes through web interface", %{supervisor: supervisor} do
    {:ok, view, _html} = live(build_conn(), "/debug")
    
    # Select a process for debugging
    {:ok, children} = Control.get_supervision_tree(supervisor)
    target_child = List.first(children)
    
    html = view
    |> element("#process-selector")
    |> render_click(%{pid: target_child.pid})
    
    assert html =~ "Process Debug Console"
    assert html =~ target_child.pid
    
    # Send interactive commands
    html = view
    |> form("#debug-command", %{
      command_type: "cast",
      command_content: "increment"
    })
    |> render_submit()
    
    assert html =~ "Command sent successfully"
    
    # Verify state change
    updated_html = render(view)
    assert updated_html =~ "value: 1"  # State should show increment
  end
  
  test "provides safe process manipulation tools with undo capabilities", %{supervisor: supervisor} do
    {:ok, children} = Control.get_supervision_tree(supervisor)
    counter_child = Enum.find(children, &(&1.id == :counter_1))
    {:ok, counter_pid} = Control.to_pid(counter_child.pid)
    
    {:ok, debugger} = ProcessDebugger.start_link(counter_pid)
    
    # Enable operation recording
    ProcessDebugger.enable_operation_recording(debugger)
    
    # Perform operations
    ProcessDebugger.safe_cast(debugger, :increment)
    ProcessDebugger.safe_cast(debugger, :increment)
    
    current_state = ProcessDebugger.get_current_state(debugger)
    assert current_state.value == 2
    
    # Test undo capability
    ProcessDebugger.undo_last_operation(debugger)
    
    undone_state = ProcessDebugger.get_current_state(debugger)
    assert undone_state.value == 1
    
    # Test undo history
    operation_history = ProcessDebugger.get_operation_history(debugger)
    
    assert length(operation_history) >= 2
    assert List.first(operation_history).type == :undo
    assert List.last(operation_history).type == :cast
  end
  
  test "includes comprehensive debugging context and operation history", %{supervisor: supervisor} do
    {:ok, children} = Control.get_supervision_tree(supervisor)
    target_child = List.first(children)
    {:ok, target_pid} = Control.to_pid(target_child.pid)
    
    {:ok, debugger} = ProcessDebugger.start_link(target_pid)
    
    # Enable comprehensive logging
    ProcessDebugger.set_debug_level(debugger, :verbose)
    
    # Perform various operations
    ProcessDebugger.safe_cast(debugger, :increment)
    ProcessDebugger.safe_call(debugger, :get_value)
    ProcessDebugger.inspect_mailbox(debugger)
    ProcessDebugger.trace_messages(debugger, duration: 100)
    
    debug_context = ProcessDebugger.get_debug_context(debugger)
    
    assert %{
      process_info: proc_info,
      operation_history: op_history,
      performance_metrics: perf_metrics,
      debugging_session: session_info
    } = debug_context
    
    assert is_map(proc_info)
    assert length(op_history) >= 4
    assert is_map(perf_metrics)
    assert session_info.debug_level == :verbose
    
    # Verify operation details
    cast_op = Enum.find(op_history, &(&1.type == :cast))
    assert cast_op.command == :increment
    assert is_integer(cast_op.timestamp)
    assert cast_op.success == true
  end
end
```

### Implementation Requirements

Develop `lib/otp_supervisor/core/process_debugger.ex`:

```elixir
defmodule OTPSupervisor.Core.ProcessDebugger do
  @moduledoc """
  Provides sophisticated debugging tools for process inspection, message tracing,
  and state analysis for production OTP monitoring.
  
  This module enables safe interaction with running processes without compromising
  system stability. All operations include comprehensive logging and safety checks.
  """
  
  use GenServer
  require Logger
  
  # Client API
  
  def start_link(target_pid, opts \\ []) when is_pid(target_pid) do
    GenServer.start_link(__MODULE__, {target_pid, opts})
  end
  
  def start_message_tracing(debugger, opts \\ []) do
    GenServer.call(debugger, {:start_message_tracing, opts})
  end
  
  def inspect_state(debugger) do
    GenServer.call(debugger, :inspect_state)
  end
  
  def safe_cast(debugger, message) do
    GenServer.call(debugger, {:safe_cast, message})
  end
  
  def enable_operation_recording(debugger) do
    GenServer.call(debugger, :enable_operation_recording)
  end
  
  # Server Implementation
  
  @impl true
  def init({target_pid, opts}) do
    Process.monitor(target_pid)
    
    state = %{
      target_pid: target_pid,
      message_tracer: nil,
      state_history: [],
      operation_history: [],
      debug_level: Keyword.get(opts, :debug_level, :normal),
      recording_enabled: false,
      safety_checks: Keyword.get(opts, :safety_checks, true)
    }
    
    {:ok, state}
  end
  
  @impl true
  def handle_call({:start_message_tracing, opts}, _from, state) do
    filters = Keyword.get(opts, :filters, [])
    
    case MessageTracer.trace_messages(state.target_pid, opts) do
      {:ok, tracer_pid} ->
        new_state = %{state | message_tracer: tracer_pid}
        {:reply, :ok, new_state}
      error ->
        {:reply, error, state}
    end
  end
  
  @impl true
  def handle_call(:inspect_state, _from, state) do
    case safely_get_state(state.target_pid) do
      {:ok, current_state} ->
        inspection = %{
          current_state: current_state,
          state_type: get_state_type(current_state),
          state_size: get_state_size(current_state),
          state_structure: analyze_structure(current_state),
          serialization_info: check_serialization(current_state)
        }
        {:reply, inspection, state}
      error ->
        {:reply, error, state}
    end
  end
  
  @impl true
  def handle_call({:safe_cast, message}, _from, state) do
    if state.safety_checks do
      case validate_safe_operation(:cast, message, state.target_pid) do
        :ok ->
          perform_safe_cast(message, state)
        {:error, reason} ->
          {:reply, {:error, reason}, state}
      end
    else
      perform_safe_cast(message, state)
    end
  end
  
  # ... additional implementation
end
```

### Compliance Review Phase

Verify that:
- ✅ Debugging operations never compromise system stability
- ✅ All tools include comprehensive operation logging and audit trails
- ✅ Performance impact is minimized and measurable
- ✅ Safety mechanisms prevent accidental process termination
- ✅ Test coverage demonstrates safe operation under all conditions

---

## Prompt 4: API Migration and Cleanup with TDD

**Objective**: Remove simulated/fake functionality and implement proper OTP patterns while maintaining API compatibility where possible.

### Required Reading Phase

#### 1. Current API Implementation

**Complete API Structure:**
```elixir
# lib/otp_supervisor_web/controllers/api/v1/supervisor_controller.ex
defmodule OtpSupervisorWeb.Api.V1.SupervisorController do
  use OtpSupervisorWeb, :controller
  
  alias OTPSupervisor.Core.Control
  
  def index(conn, _params) do
    supervisors = Control.list_supervisors()
    json(conn, %{data: supervisors})
  end
  
  def show(conn, %{"name" => supervisor_name}) do
    supervisor_atom = String.to_existing_atom(supervisor_name)
    
    case Control.get_supervision_tree(supervisor_atom) do
      {:ok, children} ->
        supervisor_info = %{
          name: supervisor_name,
          children: children,
          child_count: length(children)
        }
        json(conn, %{data: supervisor_info})
      
      {:error, reason} ->
        conn
        |> put_status(404)
        |> json(%{error: %{message: "Supervisor not found", reason: inspect(reason)}})
    end
  end
  
  def analytics(conn, %{"name" => supervisor_name}) do
    supervisor_atom = String.to_existing_atom(supervisor_name)
    
    case Control.to_pid(supervisor_atom) do
      {:ok, pid} ->
        analytics = %{
          restart_history: Control.get_restart_history(pid),
          failure_rate: Control.get_failure_rate(pid),
          supervisor_stats: Control.get_supervisor_analytics()
        }
        json(conn, %{data: analytics})
      
      {:error, reason} ->
        conn
        |> put_status(404)
        |> json(%{error: %{message: "Supervisor not found", reason: inspect(reason)}})
    end
  end
  
  def change_strategy(conn, %{"name" => _supervisor_name, "strategy" => _new_strategy}) do
    # This endpoint would be harmful - return deprecation
    conn
    |> put_status(410)
    |> json(%{
      error: %{
        code: "endpoint_removed",
        message: "Strategy change at runtime is not supported - would be harmful",
        alternative: "Use sandbox management: POST /api/v1/sandboxes"
      }
    })
  end
  
  def simulate_failure(conn, %{"name" => supervisor_name, "child_id" => child_id, "reason" => reason}) do
    supervisor_atom = String.to_existing_atom(supervisor_name)
    
    case Control.get_supervision_tree(supervisor_atom) do
      {:ok, children} ->
        child = Enum.find(children, &(&1.id == String.to_atom(child_id)))
        
        if child do
          case Control.to_pid(child.pid) do
            {:ok, pid} ->
              Control.simulate_crash(pid, String.to_atom(reason))
              json(conn, %{data: %{status: "failure_simulated", child_id: child_id}})
            error ->
              conn
              |> put_status(400)
              |> json(%{error: %{message: "Invalid child PID", reason: inspect(error)}})
          end
        else
          conn
          |> put_status(404)
          |> json(%{error: %{message: "Child not found"}})
        end
      
      {:error, reason} ->
        conn
        |> put_status(404)
        |> json(%{error: %{message: "Supervisor not found", reason: inspect(reason)}})
    end
  end
end
```

**Process Controller Implementation:**
```elixir
# lib/otp_supervisor_web/controllers/api/v1/process_controller.ex
defmodule OtpSupervisorWeb.Api.V1.ProcessController do
  use OtpSupervisorWeb, :controller
  
  alias OTPSupervisor.Core.Control
  
  def index(conn, params) do
    filter = Map.get(params, "type", "all") |> String.to_atom()
    limit = case Map.get(params, "limit") do
      nil -> :infinity
      limit_str -> String.to_integer(limit_str)
    end
    
    processes = Control.list_all_processes(filter: filter, limit: limit)
    
    json(conn, %{
      data: processes,
      meta: %{
        total: length(processes),
        filter: filter,
        limit: limit
      }
    })
  end
  
  def show(conn, %{"pid" => pid_string}) do
    case Control.to_pid(pid_string) do
      {:ok, pid} ->
        case Control.get_process_info(pid) do
          {:ok, info} ->
            process_data = %{
              pid: pid_string,
              info: info,
              state: get_state_if_genserver(pid),
              links: get_process_links(pid),
              monitors: get_process_monitors(pid)
            }
            json(conn, %{data: process_data})
          
          {:error, :process_dead} ->
            conn
            |> put_status(404)
            |> json(%{error: %{message: "Process not found or dead"}})
        end
      
      {:error, reason} ->
        conn
        |> put_status(400)
        |> json(%{error: %{message: "Invalid PID format", reason: inspect(reason)}})
    end
  end
  
  def get_state(conn, %{"pid" => pid_string}) do
    case Control.to_pid(pid_string) do
      {:ok, pid} ->
        case Control.get_process_state(pid) do
          {:ok, state} ->
            json(conn, %{data: %{state: state, type: "genserver"}})
          
          {:error, :not_a_genserver} ->
            conn
            |> put_status(422)
            |> json(%{error: %{message: "Not a GenServer process"}})
        end
      
      {:error, reason} ->
        conn
        |> put_status(400)
        |> json(%{error: %{message: "Invalid PID format", reason: inspect(reason)}})
    end
  end
  
  # ... additional endpoints
end
```

#### 2. Router Configuration (lib/otp_supervisor_web/router.ex)

**Current API Routes:**
```elixir
# API routes
scope "/api", OtpSupervisorWeb.Api, as: :api do
  pipe_through :api

  scope "/v1", V1, as: :v1 do
    get "/processes", ProcessController, :index
    get "/processes/:pid", ProcessController, :show
    get "/processes/:pid/state", ProcessController, :get_state
    get "/processes/:pid/messages", ProcessController, :get_messages
    post "/processes/:pid/trace", ProcessController, :start_trace
    delete "/processes/:pid/trace", ProcessController, :stop_trace
    post "/processes/:pid/message", ProcessController, :send_message

    get "/system/health", SystemController, :health
    get "/system/graph", SystemController, :graph
    get "/system/bottlenecks", SystemController, :bottlenecks
    get "/system/anomalies", SystemController, :anomalies

    get "/supervisors", SupervisorController, :index
    get "/supervisors/:name", SupervisorController, :show
    get "/supervisors/:name/analytics", SupervisorController, :analytics
    put "/supervisors/:name/strategy", SupervisorController, :change_strategy
    post "/supervisors/:name/simulate-failure", SupervisorController, :simulate_failure
  end
end
```

#### 3. Proper OTP Patterns Analysis

**What's Already Correct:**
- ✅ **Telemetry-based analytics** - Uses proper OTP telemetry events
- ✅ **Real sandbox management** - `SandboxManager` with proper supervisor lifecycle  
- ✅ **Safe state inspection** - Uses `:sys.get_state/2` properly
- ✅ **Process introspection** - Uses standard Erlang APIs only

**What Never Existed (Good!):**
- ❌ External restart tracking (never implemented)
- ❌ Pause/resume supervisor functionality (never implemented)
- ❌ External supervisor state monitoring (never implemented)

**Analytics Server Implementation (Already Proper):**
```elixir
# lib/otp_supervisor/core/analytics_server.ex - ALREADY CORRECT
defmodule OTPSupervisor.Core.AnalyticsServer do
  @moduledoc """
  Collects supervisor analytics using OTP's built-in telemetry events.
  This server provides production-grade monitoring without external process monitoring.
  """
  
  use GenServer
  
  # Uses proper telemetry integration
  def get_restart_history(supervisor_pid) when is_pid(supervisor_pid) do
    GenServer.call(__MODULE__, {:get_restart_history, supervisor_pid})
  end
  
  # Provides real analytics without harmful patterns
  def get_failure_rate(supervisor_pid, time_window_ms \\ 60_000) do
    GenServer.call(__MODULE__, {:get_failure_rate, supervisor_pid, time_window_ms})
  end
end
```

**Sandbox Manager Implementation (Already Proper):**
```elixir
# lib/otp_supervisor/core/sandbox_manager.ex - ALREADY CORRECT
defmodule OTPSupervisor.Core.SandboxManager do
  @moduledoc """
  Manages isolated supervisor sandboxes for safe experimentation.
  Uses proper OTP supervisor lifecycle management.
  """
  
  use GenServer
  
  def create_sandbox(sandbox_id, supervisor_module, opts \\ []) do
    GenServer.call(__MODULE__, {:create_sandbox, sandbox_id, supervisor_module, opts})
  end
  
  def destroy_sandbox(sandbox_id) do
    GenServer.call(__MODULE__, {:destroy_sandbox, sandbox_id})
  end
end
```

### Test-First Implementation Phase

Since the harmful patterns **never existed**, the focus is on testing that the proper patterns work correctly:

#### 1. API Endpoint Verification Tests
```elixir
describe "API compatibility verification" do
  test "analytics endpoint returns telemetry-based data in compatible format" do
    supervisor = :demo_one_for_one
    
    conn = build_conn()
    |> get("/api/v1/supervisors/demo_one_for_one/analytics")
    
    assert %{
      "data" => %{
        "restart_history" => history,
        "failure_rate" => rate,
        "supervisor_stats" => stats
      }
    } = json_response(conn, 200)
    
    assert is_list(history)
    assert is_map(rate)
    assert is_map(stats)
  end
  
  test "removed endpoints return 410 Gone with proper alternatives" do
    conn = build_conn()
    |> put("/api/v1/supervisors/demo_one_for_one/strategy", %{"strategy" => "one_for_all"})
    
    assert %{
      "error" => %{
        "code" => "endpoint_removed",
        "message" => message,
        "alternative" => alternative
      }
    } = json_response(conn, 410)
    
    assert message =~ "not supported"
    assert alternative =~ "sandbox"
  end
  
  test "existing endpoints maintain backward compatibility" do
    conn = build_conn()
    |> get("/api/v1/supervisors")
    
    assert %{"data" => supervisors} = json_response(conn, 200)
    assert is_list(supervisors)
    
    # Verify expected fields are present
    supervisor = List.first(supervisors)
    assert Map.has_key?(supervisor, "name")
    assert Map.has_key?(supervisor, "pid")
    assert Map.has_key?(supervisor, "alive")
  end
end
```

#### 2. Sandbox Management API Tests
```elixir
describe "sandbox management API" do
  test "sandbox creation creates real supervisors with proper lifecycle" do
    conn = build_conn()
    |> post("/api/v1/sandboxes", %{
      "supervisor_module" => "OTPSupervisor.Sandbox.Supervisors.DemoSupervisor",
      "strategy" => "one_for_one"
    })
    
    assert %{
      "data" => %{
        "sandbox_id" => sandbox_id,
        "supervisor_pid" => supervisor_pid,
        "status" => "active"
      }
    } = json_response(conn, 201)
    
    assert is_binary(sandbox_id)
    assert is_binary(supervisor_pid)
    
    # Verify real supervisor was created
    {:ok, pid} = Control.to_pid(supervisor_pid)
    assert Process.alive?(pid)
    
    # Cleanup
    delete(build_conn(), "/api/v1/sandboxes/#{sandbox_id}")
  end
  
  test "sandbox operations maintain isolation and proper cleanup" do
    # Create sandbox
    create_conn = post(build_conn(), "/api/v1/sandboxes", %{
      "supervisor_module" => "OTPSupervisor.Sandbox.Supervisors.DemoSupervisor"
    })
    
    %{"data" => %{"sandbox_id" => sandbox_id}} = json_response(create_conn, 201)
    
    # Verify sandbox exists
    get_conn = get(build_conn(), "/api/v1/sandboxes/#{sandbox_id}")
    assert %{"data" => sandbox_info} = json_response(get_conn, 200)
    assert sandbox_info["status"] == "active"
    
    # Destroy sandbox
    delete_conn = delete(build_conn(), "/api/v1/sandboxes/#{sandbox_id}")
    assert %{"data" => %{"status" => "destroyed"}} = json_response(delete_conn, 200)
    
    # Verify cleanup
    get_after_delete = get(build_conn(), "/api/v1/sandboxes/#{sandbox_id}")
    assert json_response(get_after_delete, 404)
  end
  
  test "sandbox restart maintains configuration" do
    # Create sandbox
    create_conn = post(build_conn(), "/api/v1/sandboxes", %{
      "supervisor_module" => "OTPSupervisor.Sandbox.Supervisors.DemoSupervisor",
      "strategy" => "one_for_one"
    })
    
    %{"data" => %{"sandbox_id" => sandbox_id, "supervisor_pid" => original_pid}} = 
      json_response(create_conn, 201)
    
    # Restart sandbox
    restart_conn = put(build_conn(), "/api/v1/sandboxes/#{sandbox_id}/restart")
    
    assert %{
      "data" => %{
        "sandbox_id" => ^sandbox_id,
        "supervisor_pid" => new_pid,
        "restart_count" => 1
      }
    } = json_response(restart_conn, 200)
    
    # Should have new PID but same configuration
    assert new_pid != original_pid
    
    # Cleanup
    delete(build_conn(), "/api/v1/sandboxes/#{sandbox_id}")
  end
end
```

#### 3. Telemetry Integration Tests
```elixir
describe "telemetry integration verification" do
  test "analytics use real telemetry events without external monitoring" do
    supervisor = :demo_one_for_one
    
    # Get baseline analytics
    initial_analytics = Control.get_supervisor_analytics()
    
    # Generate telemetry events by causing restart
    {:ok, children} = Control.get_supervision_tree(supervisor)
    child = List.first(children)
    {:ok, child_pid} = Control.to_pid(child.pid)
    
    # Kill process to generate restart event
    Control.kill_process(child_pid)
    
    # Wait for restart and telemetry processing
    Process.sleep(100)
    
    # Get updated analytics
    updated_analytics = Control.get_supervisor_analytics()
    
    # Should show change from telemetry events
    assert updated_analytics.total_restarts > initial_analytics.total_restarts
  end
  
  test "restart history comes from telemetry events not external tracking" do
    supervisor = :demo_one_for_one
    
    case Control.to_pid(supervisor) do
      {:ok, supervisor_pid} ->
        # Get initial history
        initial_history = Control.get_restart_history(supervisor_pid)
        initial_count = length(initial_history)
        
        # Cause restart
        {:ok, children} = Control.get_supervision_tree(supervisor)
        child = List.first(children)
        {:ok, child_pid} = Control.to_pid(child.pid)
        
        Control.kill_process(child_pid)
        Process.sleep(100)  # Allow telemetry processing
        
        # Get updated history
        updated_history = Control.get_restart_history(supervisor_pid)
        
        # Should have new entry from telemetry
        assert length(updated_history) > initial_count
        
        # Verify entry structure (from telemetry)
        latest_entry = List.first(updated_history)
        assert Map.has_key?(latest_entry, :timestamp)
        assert Map.has_key?(latest_entry, :child_id)
        assert Map.has_key?(latest_entry, :reason)
    end
  end
  
  test "no external supervisor monitoring patterns exist" do
    # Verify no harmful patterns in codebase
    assert Code.ensure_loaded?(OTPSupervisor.Core.Control)
    refute Code.ensure_loaded?(OTPSupervisor.Core.RestartTracker)  # Should not exist
    refute Code.ensure_loaded?(OTPSupervisor.Core.SupervisorController)  # Should not exist
    
    # Verify Control module doesn't have harmful functions
    control_functions = OTPSupervisor.Core.Control.__info__(:functions)
    
    refute {:pause_supervisor, 1} in control_functions
    refute {:resume_supervisor, 1} in control_functions
    refute {:start_restart_tracking, 1} in control_functions
  end
end
```

### Implementation Requirements

Since harmful patterns never existed, the implementation focuses on **completing the proper patterns**:

#### 1. Add Missing Sandbox API Controller
```elixir
# lib/otp_supervisor_web/controllers/api/v1/sandbox_controller.ex
defmodule OtpSupervisorWeb.Api.V1.SandboxController do
  use OtpSupervisorWeb, :controller
  
  alias OTPSupervisor.Core.Control
  
  def create(conn, %{"supervisor_module" => module_name} = params) do
    try do
      module = String.to_existing_atom(module_name)
      strategy = Map.get(params, "strategy", "one_for_one") |> String.to_atom()
      
      case Control.create_sandbox(module, strategy: strategy) do
        {:ok, sandbox_info} ->
          conn
          |> put_status(201)
          |> json(%{data: sandbox_info})
        
        {:error, reason} ->
          conn
          |> put_status(422)
          |> json(%{error: %{message: "Failed to create sandbox", reason: inspect(reason)}})
      end
    rescue
      ArgumentError ->
        conn
        |> put_status(400)
        |> json(%{error: %{message: "Invalid supervisor module"}})
    end
  end
  
  def index(conn, _params) do
    sandboxes = Control.list_sandboxes()
    json(conn, %{data: sandboxes})
  end
  
  def show(conn, %{"id" => sandbox_id}) do
    case Control.get_sandbox_info(sandbox_id) do
      {:ok, sandbox_info} ->
        json(conn, %{data: sandbox_info})
      
      {:error, :not_found} ->
        conn
        |> put_status(404)
        |> json(%{error: %{message: "Sandbox not found"}})
    end
  end
  
  def restart(conn, %{"id" => sandbox_id}) do
    case Control.restart_sandbox(sandbox_id) do
      {:ok, sandbox_info} ->
        json(conn, %{data: sandbox_info})
      
      {:error, :not_found} ->
        conn
        |> put_status(404)
        |> json(%{error: %{message: "Sandbox not found"}})
      
      {:error, reason} ->
        conn
        |> put_status(422)
        |> json(%{error: %{message: "Failed to restart sandbox", reason: inspect(reason)}})
    end
  end
  
  def destroy(conn, %{"id" => sandbox_id}) do
    case Control.destroy_sandbox(sandbox_id) do
      :ok ->
        json(conn, %{data: %{status: "destroyed", sandbox_id: sandbox_id}})
      
      {:error, :not_found} ->
        conn
        |> put_status(404)
        |> json(%{error: %{message: "Sandbox not found"}})
    end
  end
end
```

#### 2. Update Router with Sandbox Routes
```elixir
# Add to lib/otp_supervisor_web/router.ex
scope "/v1", V1, as: :v1 do
  # ... existing routes ...
  
  # Sandbox management routes
  post "/sandboxes", SandboxController, :create
  get "/sandboxes", SandboxController, :index
  get "/sandboxes/:id", SandboxController, :show
  put "/sandboxes/:id/restart", SandboxController, :restart
  delete "/sandboxes/:id", SandboxController, :destroy
end
```

### Compliance Review Phase

Verify your implementation:

1. **API Compatibility**:
   - ✅ Non-breaking endpoints work identically
   - ✅ Breaking changes return proper HTTP status codes (410 Gone)
   - ✅ New endpoints follow existing API patterns
   - ✅ All responses include migration guidance where applicable

2. **OTP Correctness**:
   - ✅ No external monitoring of supervisor internals
   - ✅ All supervisor operations use proper OTP APIs
   - ✅ Telemetry integration follows OTP best practices
   - ✅ Sandbox management demonstrates real supervisor patterns

3. **Production Value**:
   - ✅ Clear documentation of proper OTP patterns
   - ✅ Examples show real supervisor lifecycle management
   - ✅ Professional context in error messages and API responses

---

## Prompt 5: REST API for External Tool Integration with TDD

**Objective**: Verify and enhance the existing comprehensive REST API for external tools, ensuring it works perfectly with the proper OTP patterns.

### Required Reading Phase

#### 1. Current API Implementation Verification

**Complete Process Controller:**
```elixir
# lib/otp_supervisor_web/controllers/api/v1/process_controller.ex
# [Full implementation as shown above in Prompt 4]
```

**System Controller Implementation:**
```elixir
# lib/otp_supervisor_web/controllers/api/v1/system_controller.ex
defmodule OtpSupervisorWeb.Api.V1.SystemController do
  use OtpSupervisorWeb, :controller
  
  alias OTPSupervisor.Core.Control
  alias OTPSupervisor.Core.SystemAnalyzer
  
  def health(conn, _params) do
    health_data = %{
      status: "healthy",
      timestamp: System.system_time(:millisecond),
      system_info: %{
        total_processes: length(Process.list()),
        total_supervisors: length(Control.list_supervisors()),
        memory_usage: :erlang.memory(:total),
        uptime: :erlang.statistics(:wall_clock) |> elem(0)
      }
    }
    
    json(conn, %{data: health_data})
  end
  
  def graph(conn, _params) do
    process_graph = Control.build_process_graph()
    json(conn, %{data: process_graph})
  end
  
  def bottlenecks(conn, _params) do
    bottlenecks = SystemAnalyzer.detect_bottlenecks()
    json(conn, %{data: bottlenecks})
  end
  
  def anomalies(conn, _params) do
    anomalies = SystemAnalyzer.detect_anomalies()
    json(conn, %{data: anomalies})
  end
end
```

#### 2. Core Control Module Integration

**Verify all API dependencies exist in Control module:**
```elixir
# These functions must exist in lib/otp_supervisor/core/control.ex:

# Process operations
def list_all_processes(opts \\ [])  # ✅ EXISTS
def get_process_info(pid)           # ✅ EXISTS  
def get_process_state(pid)          # ✅ EXISTS
def kill_process(pid)               # ✅ EXISTS
def to_pid(pid_string)              # ✅ EXISTS
def build_process_graph()           # ✅ EXISTS

# Supervisor operations
def list_supervisors()              # ✅ EXISTS
def get_supervision_tree(supervisor) # ✅ EXISTS
def get_restart_history(supervisor)  # ✅ EXISTS (telemetry-based)
def get_supervisor_analytics()       # ✅ EXISTS
def simulate_crash(pid, reason, opts) # ✅ EXISTS

# Sandbox operations
def create_sandbox(module, opts)     # ✅ EXISTS
def destroy_sandbox(sandbox_id)     # ✅ EXISTS
def restart_sandbox(sandbox_id)     # ✅ EXISTS
def list_sandboxes()                # ✅ EXISTS
def get_sandbox_info(sandbox_id)    # ✅ EXISTS
```

### Test-First Implementation Phase

Write comprehensive tests ensuring API works perfectly with cleaned-up backend:

#### 1. Complete API Integration Tests
```elixir
defmodule OtpSupervisorWeb.Api.V1.FullIntegrationTest do
  use OtpSupervisorWeb.ConnCase, async: false
  
  describe "complete API workflow integration" do
    test "full workflow: process discovery -> inspection -> manipulation -> cleanup" do
      # Step 1: Discover processes
      conn = build_conn() |> get("/api/v1/processes")
      assert %{"data" => processes} = json_response(conn, 200)
      assert length(processes) > 0
      
      # Step 2: Filter for GenServer processes
      conn = build_conn() |> get("/api/v1/processes?type=genserver")
      assert %{"data" => genservers} = json_response(conn, 200)
      genserver = List.first(genservers)
      
      # Step 3: Inspect specific process
      conn = build_conn() |> get("/api/v1/processes/#{genserver["pid"]}")
      assert %{"data" => process_details} = json_response(conn, 200)
      assert Map.has_key?(process_details, "info")
      assert Map.has_key?(process_details, "links")
      
      # Step 4: Get process state
      conn = build_conn() |> get("/api/v1/processes/#{genserver["pid"]}/state")
      assert %{"data" => %{"state" => state}} = json_response(conn, 200)
      
      # Step 5: Send message to process
      conn = build_conn()
      |> post("/api/v1/processes/#{genserver["pid"]}/message", %{
        "message" => %{"type" => "cast", "content" => "increment"}
      })
      assert %{"data" => %{"status" => "message_sent"}} = json_response(conn, 200)
      
      # Step 6: Verify state change
      conn = build_conn() |> get("/api/v1/processes/#{genserver["pid"]}/state")
      assert %{"data" => %{"state" => new_state}} = json_response(conn, 200)
      # State should have changed (if it's a counter)
    end
    
    test "supervisor management workflow with real OTP operations" do
      # Step 1: List supervisors
      conn = build_conn() |> get("/api/v1/supervisors")
      assert %{"data" => supervisors} = json_response(conn, 200)
      supervisor = List.first(supervisors)
      
      # Step 2: Get supervisor details
      conn = build_conn() |> get("/api/v1/supervisors/#{supervisor["name"]}")
      assert %{"data" => supervisor_details} = json_response(conn, 200)
      assert Map.has_key?(supervisor_details, "children")
      
      # Step 3: Get analytics (telemetry-based)
      conn = build_conn() |> get("/api/v1/supervisors/#{supervisor["name"]}/analytics")
      assert %{"data" => analytics} = json_response(conn, 200)
      assert Map.has_key?(analytics, "restart_history")
      assert Map.has_key?(analytics, "failure_rate")
      
      # Step 4: Simulate failure
      child = List.first(supervisor_details["children"])
      conn = build_conn()
      |> post("/api/v1/supervisors/#{supervisor["name"]}/simulate-failure", %{
        "child_id" => child["id"],
        "reason" => "kill"
      })
      assert %{"data" => %{"status" => "failure_simulated"}} = json_response(conn, 200)
      
      # Step 5: Verify restart occurred (check analytics again)
      Process.sleep(100)  # Allow restart and telemetry processing
      conn = build_conn() |> get("/api/v1/supervisors/#{supervisor["name"]}/analytics")
      assert %{"data" => updated_analytics} = json_response(conn, 200)
      # Should show updated restart history
    end
    
    test "sandbox lifecycle management demonstrates proper OTP patterns" do
      # Step 1: Create sandbox
      conn = build_conn()
      |> post("/api/v1/sandboxes", %{
        "supervisor_module" => "OTPSupervisor.Sandbox.Supervisors.DemoSupervisor",
        "strategy" => "one_for_one"
      })
      assert %{"data" => sandbox} = json_response(conn, 201)
      sandbox_id = sandbox["sandbox_id"]
      
      # Step 2: Verify sandbox is active
      conn = build_conn() |> get("/api/v1/sandboxes/#{sandbox_id}")
      assert %{"data" => %{"status" => "active"}} = json_response(conn, 200)
      
      # Step 3: List all sandboxes
      conn = build_conn() |> get("/api/v1/sandboxes")
      assert %{"data" => sandboxes} = json_response(conn, 200)
      assert Enum.any?(sandboxes, &(&1["sandbox_id"] == sandbox_id))
      
      # Step 4: Restart sandbox
      conn = build_conn() |> put("/api/v1/sandboxes/#{sandbox_id}/restart")
      assert %{"data" => restarted_sandbox} = json_response(conn, 200)
      assert restarted_sandbox["restart_count"] == 1
      
      # Step 5: Destroy sandbox
      conn = build_conn() |> delete("/api/v1/sandboxes/#{sandbox_id}")
      assert %{"data" => %{"status" => "destroyed"}} = json_response(conn, 200)
      
      # Step 6: Verify cleanup
      conn = build_conn() |> get("/api/v1/sandboxes/#{sandbox_id}")
      assert json_response(conn, 404)
    end
  end
  
  describe "system-wide API operations" do
    test "system health monitoring provides comprehensive metrics" do
      conn = build_conn() |> get("/api/v1/system/health")
      
      assert %{
        "data" => %{
          "status" => "healthy",
          "timestamp" => timestamp,
          "system_info" => %{
            "total_processes" => process_count,
            "total_supervisors" => supervisor_count,
            "memory_usage" => memory,
            "uptime" => uptime
          }
        }
      } = json_response(conn, 200)
      
      assert is_integer(timestamp)
      assert is_integer(process_count) and process_count > 0
      assert is_integer(supervisor_count) and supervisor_count > 0
      assert is_integer(memory) and memory > 0
      assert is_integer(uptime) and uptime > 0
    end
    
    test "process relationship graph provides complete system topology" do
      conn = build_conn() |> get("/api/v1/system/graph")
      
      assert %{
        "data" => %{
          "processes" => processes,
          "links" => links,
          "monitors" => monitors
        }
      } = json_response(conn, 200)
      
      assert is_list(processes) and length(processes) > 0
      assert is_list(links)
      assert is_list(monitors)
      
      # Verify structure
      process = List.first(processes)
      assert Map.has_key?(process, "pid")
      assert Map.has_key?(process, "type")
    end
    
    test "anomaly detection provides actionable insights" do
      conn = build_conn() |> get("/api/v1/system/anomalies")
      
      assert %{"data" => anomalies} = json_response(conn, 200)
      assert is_list(anomalies)
      
      # If anomalies exist, verify structure
      if length(anomalies) > 0 do
        anomaly = List.first(anomalies)
        assert Map.has_key?(anomaly, "type")
        assert Map.has_key?(anomaly, "severity")
        assert Map.has_key?(anomaly, "description")
      end
    end
  end
end
```

#### 2. Error Handling and Edge Cases
```elixir
describe "comprehensive error handling" do
  test "invalid PIDs return consistent error format" do
    invalid_pids = ["invalid", "#PID<invalid>", "not-a-pid", ""]
    
    for invalid_pid <- invalid_pids do
      conn = build_conn() |> get("/api/v1/processes/#{invalid_pid}")
      
      assert %{
        "error" => %{
          "message" => message,
          "reason" => _reason
        }
      } = json_response(conn, 400)
      
      assert message =~ "Invalid PID"
    end
  end
  
  test "dead processes return 404 with helpful message" do
    # Create and kill a process
    {:ok, test_pid} = Agent.start_link(fn -> 0 end)
    pid_string = inspect(test_pid)
    Process.exit(test_pid, :kill)
    Process.sleep(10)  # Ensure process is dead
    
    conn = build_conn() |> get("/api/v1/processes/#{pid_string}")
    
    assert %{
      "error" => %{
        "message" => "Process not found or dead"
      }
    } = json_response(conn, 404)
  end
  
  test "non-existent supervisors return 404 with proper error" do
    conn = build_conn() |> get("/api/v1/supervisors/non_existent_supervisor")
    
    assert %{
      "error" => %{
        "message" => "Supervisor not found",
        "reason" => _reason
      }
    } = json_response(conn, 404)
  end
  
  test "malformed request bodies return 400 with validation errors" do
    # Missing required fields
    conn = build_conn()
    |> post("/api/v1/sandboxes", %{
      "invalid_field" => "value"
    })
    
    assert %{
      "error" => %{
        "message" => message
      }
    } = json_response(conn, 400)
    
    assert message =~ "supervisor_module"
  end
end
```

#### 3. Performance and Scalability Tests
```elixir
describe "API performance and scalability" do
  test "process listing scales efficiently with large numbers of processes" do
    # Create many test processes
    test_pids = for _ <- 1..100 do
      {:ok, pid} = Agent.start_link(fn -> 0 end)
      pid
    end
    
    # Test performance
    start_time = System.monotonic_time(:microsecond)
    
    conn = build_conn() |> get("/api/v1/processes")
    assert %{"data" => processes} = json_response(conn, 200)
    
    end_time = System.monotonic_time(:microsecond)
    elapsed_ms = (end_time - start_time) / 1000
    
    # Should complete within reasonable time
    assert elapsed_ms < 1000  # Less than 1 second
    assert length(processes) >= 100
    
    # Cleanup
    for pid <- test_pids, do: Process.exit(pid, :kill)
  end
  
  test "pagination works correctly for large result sets" do
    conn = build_conn() |> get("/api/v1/processes?limit=10")
    
    assert %{
      "data" => processes,
      "meta" => %{
        "total" => total,
        "limit" => 10
      }
    } = json_response(conn, 200)
    
    assert length(processes) <= 10
    assert is_integer(total)
  end
  
  test "concurrent API requests don't interfere with each other" do
    # Make multiple concurrent requests
    tasks = for i <- 1..10 do
      Task.async(fn ->
        conn = build_conn() |> get("/api/v1/system/health")
        {i, json_response(conn, 200)}
      end)
    end
    
    results = Task.await_many(tasks, 5000)
    
    # All should succeed
    assert length(results) == 10
    
    for {i, response} <- results do
      assert %{"data" => %{"status" => "healthy"}} = response
    end
  end
end
```

### Implementation Requirements

Complete any missing API endpoints and ensure full integration:

#### 1. Enhance ProcessController with Missing Features
```elixir
# Add to lib/otp_supervisor_web/controllers/api/v1/process_controller.ex

def get_messages(conn, %{"pid" => pid_string}) do
  case Control.to_pid(pid_string) do
    {:ok, pid} ->
      case Process.info(pid, :messages) do
        {:messages, messages} ->
          json(conn, %{
            data: %{
              pid: pid_string,
              message_count: length(messages),
              messages: Enum.take(messages, 10)  # Limit for safety
            }
          })
        
        nil ->
          conn
          |> put_status(404)
          |> json(%{error: %{message: "Process not found or dead"}})
      end
    
    {:error, reason} ->
      conn
      |> put_status(400)
      |> json(%{error: %{message: "Invalid PID format", reason: inspect(reason)}})
  end
end

def start_trace(conn, %{"pid" => pid_string} = params) do
  case Control.to_pid(pid_string) do
    {:ok, pid} ->
      max_messages = Map.get(params, "max_messages", 100)
      duration = Map.get(params, "duration", 60)
      
      case MessageTracer.trace_messages(pid, max_messages: max_messages) do
        {:ok, tracer_pid} ->
          # Schedule automatic stop
          Process.send_after(self(), {:stop_trace, pid}, duration * 1000)
          
          json(conn, %{
            data: %{
              status: "tracing_started",
              tracer_pid: inspect(tracer_pid),
              max_messages: max_messages,
              duration: duration
            }
          })
        
        {:error, reason} ->
          conn
          |> put_status(422)
          |> json(%{error: %{message: "Failed to start tracing", reason: inspect(reason)}})
      end
    
    {:error, reason} ->
      conn
      |> put_status(400)
      |> json(%{error: %{message: "Invalid PID format", reason: inspect(reason)}})
  end
end

def send_message(conn, %{"pid" => pid_string, "message" => message_params}) do
  case Control.to_pid(pid_string) do
    {:ok, pid} ->
      message_type = Map.get(message_params, "type")
      content = Map.get(message_params, "content")
      
      result = case message_type do
        "cast" ->
          GenServer.cast(pid, String.to_atom(content))
          {:ok, "cast"}
        
        "call" ->
          try do
            GenServer.call(pid, String.to_atom(content), 5000)
            {:ok, "call"}
          rescue
            _ -> {:error, "call_failed"}
          catch
            :exit, _ -> {:error, "call_timeout"}
          end
        
        "send" ->
          send(pid, String.to_atom(content))
          {:ok, "send"}
        
        _ ->
          {:error, "invalid_message_type"}
      end
      
      case result do
        {:ok, type} ->
          json(conn, %{data: %{status: "message_sent", message_type: type}})
        
        {:error, reason} ->
          conn
          |> put_status(422)
          |> json(%{error: %{message: "Failed to send message", code: reason}})
      end
    
    {:error, reason} ->
      conn
      |> put_status(400)
      |> json(%{error: %{message: "Invalid PID format", reason: inspect(reason)}})
  end
end
```

### Compliance Review Phase

Ensure the API:
- ✅ Works correctly with real OTP patterns (no simulated functionality)
- ✅ Maintains backward compatibility where possible
- ✅ Provides clear error messages and status codes
- ✅ Demonstrates proper OTP production patterns
- ✅ Has comprehensive test coverage for all endpoints
- ✅ Handles edge cases and error conditions gracefully
- ✅ Performs well under load
- ✅ Provides complete functionality for external tool integration

---

## Summary: Comprehensive TDD Context

Each prompt now includes:
- **Complete required reading** with actual file contents and patterns
- **Full context** for understanding existing implementations  
- **Comprehensive test specifications** following TDD methodology
- **Implementation requirements** with detailed code examples
- **Compliance verification** ensuring OTP best practices

All prompts are now **fully self-contained** and can be executed independently with complete context.