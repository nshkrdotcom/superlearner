# Phase 1 Enhanced Prompts - Revised TDD Edition

## Overview

This document provides comprehensive, test-driven development (TDD) prompts for enhancing the OTP Supervisor Educational Tool. Each prompt is fully self-contained and follows strict TDD methodology to ensure robust, maintainable code that serves as an excellent educational resource.

Each prompt includes complete context, required reading materials, and detailed step-by-step TDD processes to enable successful execution by any AI context without prior project knowledge.

---

## Prompt 1: Enhanced Process Introspection with TDD

### Objective
Enhance the core control module with comprehensive process introspection capabilities using test-driven development.

### Required Reading
- **Test Standards**: `docs/code-standards/test-design-reference.md` (Complete TDD methodology and test patterns)
- **Source Code**: `lib/otp_supervisor/core/control.ex` (Current implementation patterns)
- **Source Code**: `test/otp_supervisor/core/control_test.exs` (Existing test patterns)
- **Foundation**: `docs/foundational-concepts/foundation-requirements.md` (Process fundamentals section)

### TDD Development Process

#### Step 1: Write Failing Tests
Based on the test design reference, create comprehensive tests for new introspection features:

```elixir
# In test/otp_supervisor/core/control_test.exs

describe "process introspection" do
  setup do
    SupervisorTestHelper.setup_isolated_supervisor("introspection")
  end

  test "list_all_processes/1 returns all processes with filtering", %{supervisor: supervisor} do
    processes = Control.list_all_processes(filter: :supervisor)
    assert is_list(processes)
    assert length(processes) > 0
    
    # Test each process has required fields
    Enum.each(processes, fn process ->
      assert Map.has_key?(process, :pid)
      assert Map.has_key?(process, :name)
      assert Map.has_key?(process, :type)
    end)
  end

  test "get_process_state/1 extracts GenServer state safely", %{supervisor: supervisor} do
    {:ok, counter_pid} = start_supervised({Counter, name: :"test_counter_#{:erlang.unique_integer([:positive])}"})
    
    result = Control.get_process_state(counter_pid)
    assert {:ok, state} = result
    assert is_map(state) or is_atom(state) or is_number(state)
  end

  test "get_process_state/1 handles non-GenServer processes", %{supervisor: supervisor} do
    {:ok, task_pid} = Task.start(fn -> Process.sleep(1000) end)
    
    result = Control.get_process_state(task_pid)
    assert {:error, :not_a_genserver} = result
  end

  test "build_process_graph/0 creates complete relationship graph" do
    graph = Control.build_process_graph()
    assert is_map(graph)
    assert Map.has_key?(graph, :processes)
    assert Map.has_key?(graph, :links)
    assert Map.has_key?(graph, :monitors)
  end
end
```

#### Step 2: Implement Features
Write minimum code to make tests pass:

```elixir
# In lib/otp_supervisor/core/control.ex

def list_all_processes(opts \\ []) do
  filter = Keyword.get(opts, :filter, :all)
  
  Process.list()
  |> Enum.map(&process_info_with_type/1)
  |> Enum.filter(&filter_process(&1, filter))
  |> Enum.sort_by(& &1.name)
end

def get_process_state(pid) when is_pid(pid) do
  try do
    state = :sys.get_state(pid)
    {:ok, state}
  rescue
    _ -> {:error, :not_a_genserver}
  end
end

def build_process_graph do
  processes = list_all_processes()
  
  %{
    processes: processes,
    links: build_link_graph(processes),
    monitors: build_monitor_graph(processes)
  }
end
```

#### Step 3: Run All Tests
Execute complete test suite to ensure no regressions:
```bash
mix test
```

#### Step 4: Conduct Critical Compliance Review
Review implementation against all standards:
- ✅ Does it satisfy all test requirements?
- ✅ Does it follow OTP patterns from foundation requirements?
- ✅ Does it use proper error handling patterns?
- ✅ Are all functions documented with examples?
- ✅ Does it maintain educational value?

#### Step 5: Refactor and Enhance
Based on compliance review, improve code quality:
- Add comprehensive documentation
- Optimize performance for large process lists
- Add property-based tests for edge cases
- Ensure thread safety

---

## Prompt 2: Message Flow Analysis System with TDD

### Objective
Create a comprehensive message tracing and analysis system for debugging message flow in OTP systems.

### Required Reading
- **Test Standards**: `docs/code-standards/test-design-reference.md` (Message handling test patterns)
- **Source Code**: `lib/otp_supervisor/core/control.ex` (Integration patterns)
- **Source Code**: `lib/otp_supervisor/sandbox/workers/counter.ex` (GenServer message patterns)
- **Foundation**: `docs/foundational-concepts/foundation-requirements.md` (Message passing section)

### TDD Development Process

#### Step 1: Write Failing Tests
Create comprehensive test suite for message tracing:

```elixir
# In test/otp_supervisor/core/message_tracer_test.exs

defmodule OtpSupervisor.Core.MessageTracerTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog
  
  alias OtpSupervisor.Core.MessageTracer
  alias OtpSupervisor.Sandbox.Workers.Counter
  
  describe "message tracing" do
    setup do
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"test_counter_#{unique_id}"
      {:ok, counter_pid} = start_supervised({Counter, name: counter_name})
      
      on_exit(fn ->
        MessageTracer.stop_tracing(counter_pid)
      end)
      
      {:ok, counter_pid: counter_pid, counter_name: counter_name}
    end

    test "trace_messages/2 starts tracing successfully", %{counter_pid: counter_pid} do
      result = MessageTracer.trace_messages(counter_pid, max_messages: 10)
      assert {:ok, tracer_pid} = result
      assert Process.alive?(tracer_pid)
    end

    test "captures GenServer calls and casts", %{counter_pid: counter_pid} do
      {:ok, _tracer} = MessageTracer.trace_messages(counter_pid, max_messages: 5)
      
      # Generate some messages
      GenServer.cast(counter_pid, :increment)
      GenServer.call(counter_pid, :get_value)
      GenServer.cast(counter_pid, :increment)
      
      # Allow time for message processing
      Process.sleep(50)
      
      messages = MessageTracer.get_message_history(counter_pid)
      assert length(messages) >= 2
      
      # Verify message structure
      Enum.each(messages, fn msg ->
        assert Map.has_key?(msg, :timestamp)
        assert Map.has_key?(msg, :direction)
        assert Map.has_key?(msg, :content)
      end)
    end

    test "stops tracing properly", %{counter_pid: counter_pid} do
      {:ok, tracer_pid} = MessageTracer.trace_messages(counter_pid, max_messages: 5)
      
      result = MessageTracer.stop_tracing(counter_pid)
      assert :ok = result
      
      # Verify tracer process is cleaned up
      refute Process.alive?(tracer_pid)
    end
  end

  describe "message analysis" do
    test "analyze_message_patterns/1 identifies common patterns" do
      # Create mock message history
      messages = [
        %{content: {:cast, :increment}, timestamp: 1000},
        %{content: {:call, :get_value}, timestamp: 1001},
        %{content: {:cast, :increment}, timestamp: 1002},
        %{content: {:cast, :increment}, timestamp: 1003}
      ]
      
      patterns = MessageTracer.analyze_message_patterns(messages)
      
      assert Map.has_key?(patterns, :cast_frequency)
      assert Map.has_key?(patterns, :call_frequency)
      assert patterns.cast_frequency > patterns.call_frequency
    end
  end
end
```

#### Step 2: Implement Message Tracer
Write minimum viable implementation:

```elixir
# In lib/otp_supervisor/core/message_tracer.ex

defmodule OtpSupervisor.Core.MessageTracer do
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
      error ->
        error
    end
  end
  
  def get_message_history(pid) do
    case Registry.lookup(TracerRegistry, pid) do
      [{tracer_pid, _}] ->
        GenServer.call(tracer_pid, :get_messages)
      [] ->
        []
    end
  end
  
  def stop_tracing(pid) do
    case Registry.lookup(TracerRegistry, pid) do
      [{tracer_pid, _}] ->
        GenServer.stop(tracer_pid)
        :ok
      [] ->
        :ok
    end
  end
  
  # GenServer callbacks
  
  def init({traced_pid, max_messages}) do
    Registry.register(TracerRegistry, traced_pid, nil)
    {:ok, %{traced_pid: traced_pid, messages: [], max_messages: max_messages}}
  end
  
  def handle_call(:get_messages, _from, state) do
    {:reply, Enum.reverse(state.messages), state}
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
  
  defp add_message(state, message) do
    new_messages = [message | state.messages]
    |> Enum.take(state.max_messages)
    
    %{state | messages: new_messages}
  end
end
```

#### Step 3: Run All Tests
Execute complete test suite including integration tests.

#### Step 4: Conduct Critical Compliance Review
- ✅ Does message tracing work without affecting traced processes?
- ✅ Are all edge cases handled (process death, invalid PIDs)?
- ✅ Is the API consistent with existing patterns?
- ✅ Does it provide educational value about message passing?

#### Step 5: Refactor and Enhance
Add comprehensive documentation, error handling, and performance optimizations.

---

## Prompt 3: Advanced Supervisor Control with TDD

### Objective
Enhance supervisor control capabilities with runtime manipulation, analytics, and failure simulation using TDD.

### Required Reading
- **Test Standards**: `docs/code-standards/test-design-reference.md` (Supervisor testing patterns)
- **Source Code**: `lib/otp_supervisor/core/control.ex` (Current supervisor operations)
- **Source Code**: `lib/otp_supervisor/sandbox/supervisors/demo_supervisor.ex` (Supervisor patterns)
- **Foundation**: `docs/foundational-concepts/foundation-requirements.md` (Supervisor behavior section)

### TDD Development Process

#### Step 1: Write Failing Tests
Create comprehensive test suite for advanced supervisor control:

```elixir
# In test/otp_supervisor/core/control_test.exs (additional describe blocks)

describe "supervisor analytics" do
  setup do
    SupervisorTestHelper.setup_isolated_supervisor("analytics")
  end

  test "get_restart_history/1 tracks all restarts with details", %{supervisor: supervisor} do
    # Initialize history tracking
    Control.start_restart_tracking(supervisor)
    
    # Generate some restarts
    children = Control.get_supervision_tree(supervisor)
    child_pid = children |> List.first() |> Map.get(:pid)
    
    Control.kill_process(child_pid)
    SupervisorTestHelper.wait_for_process_restart(supervisor, child_pid)
    
    history = Control.get_restart_history(supervisor)
    
    assert is_list(history)
    assert length(history) > 0
    
    restart_event = List.first(history)
    assert Map.has_key?(restart_event, :timestamp)
    assert Map.has_key?(restart_event, :child_id)
    assert Map.has_key?(restart_event, :reason)
    assert Map.has_key?(restart_event, :old_pid)
    assert Map.has_key?(restart_event, :new_pid)
  end

  test "calculate_restart_intensity/1 computes current intensity", %{supervisor: supervisor} do
    intensity = Control.calculate_restart_intensity(supervisor)
    
    assert is_number(intensity)
    assert intensity >= 0.0
  end

  test "predict_restart_storm/1 warns of approaching limits", %{supervisor: supervisor} do
    # This test would require multiple rapid restarts
    prediction = Control.predict_restart_storm(supervisor)
    
    assert is_map(prediction)
    assert Map.has_key?(prediction, :risk_level)
    assert Map.has_key?(prediction, :current_intensity)
    assert Map.has_key?(prediction, :threshold)
  end
end

describe "runtime supervisor manipulation" do
  setup do
    SupervisorTestHelper.setup_isolated_supervisor("manipulation")
  end

  test "pause_supervisor/1 temporarily prevents restarts", %{supervisor: supervisor} do
    # Pause the supervisor
    result = Control.pause_supervisor(supervisor)
    assert :ok = result
    
    # Kill a child - should not restart
    children = Control.get_supervision_tree(supervisor)
    child_pid = children |> List.first() |> Map.get(:pid)
    
    Control.kill_process(child_pid)
    Process.sleep(100)  # Wait for potential restart
    
    # Verify child was not restarted
    new_children = Control.get_supervision_tree(supervisor)
    assert length(new_children) == length(children) - 1
  end

  test "resume_supervisor/1 re-enables supervision", %{supervisor: supervisor} do
    # Pause then resume
    Control.pause_supervisor(supervisor)
    result = Control.resume_supervisor(supervisor)
    assert :ok = result
    
    # Kill a child - should restart
    children = Control.get_supervision_tree(supervisor)
    child_pid = children |> List.first() |> Map.get(:pid)
    
    Control.kill_process(child_pid)
    SupervisorTestHelper.wait_for_process_restart(supervisor, child_pid)
    
    new_children = Control.get_supervision_tree(supervisor)
    assert length(new_children) == length(children)
  end
end

describe "failure simulation" do
  setup do
    SupervisorTestHelper.setup_isolated_supervisor("simulation")
  end

  test "simulate_crash/3 crashes child with specific reason", %{supervisor: supervisor} do
    children = Control.get_supervision_tree(supervisor)
    child_pid = children |> List.first() |> Map.get(:pid)
    
    capture_log(fn ->
      result = Control.simulate_crash(child_pid, :custom_reason, delay: 0)
      assert :ok = result
    end)
    
    # Verify process was killed
    refute Process.alive?(child_pid)
  end

  test "simulate_memory_pressure/2 increases process memory usage", %{supervisor: supervisor} do
    children = Control.get_supervision_tree(supervisor)
    child_pid = children |> List.first() |> Map.get(:pid)
    
    initial_memory = Process.info(child_pid, :memory) |> elem(1)
    
    result = Control.simulate_memory_pressure(child_pid, mb: 1)
    assert :ok = result
    
    # Allow time for memory allocation
    Process.sleep(100)
    
    new_memory = Process.info(child_pid, :memory) |> elem(1)
    assert new_memory > initial_memory
  end
end
```

#### Step 2: Implement Advanced Control Features
Write minimum viable implementation for each feature:

```elixir
# In lib/otp_supervisor/core/control.ex (additions)

@doc """
Starts tracking restart events for a supervisor.
"""
def start_restart_tracking(supervisor) do
  case to_pid(supervisor) do
    {:ok, pid} ->
      RestartTracker.start_tracking(pid)
    error ->
      error
  end
end

@doc """
Gets the restart history for a supervisor.
"""
def get_restart_history(supervisor) do
  case to_pid(supervisor) do
    {:ok, pid} ->
      RestartTracker.get_history(pid)
    {:error, _} ->
      []
  end
end

@doc """
Calculates the current restart intensity for a supervisor.
"""
def calculate_restart_intensity(supervisor) do
  case to_pid(supervisor) do
    {:ok, pid} ->
      history = RestartTracker.get_history(pid)
      calculate_intensity(history)
    {:error, _} ->
      0.0
  end
end

@doc """
Temporarily pauses supervision for a supervisor.
"""
def pause_supervisor(supervisor) do
  case to_pid(supervisor) do
    {:ok, pid} ->
      SupervisorController.pause(pid)
    error ->
      error
  end
end

@doc """
Simulates a crash with a specific reason.
"""
def simulate_crash(pid, reason, opts \\ []) when is_pid(pid) do
  delay = Keyword.get(opts, :delay, 0)
  
  if delay > 0 do
    Process.send_after(self(), {:crash, pid, reason}, delay)
  else
    Process.exit(pid, reason)
  end
  
  :ok
end

@doc """
Simulates memory pressure on a process.
"""
def simulate_memory_pressure(pid, opts \\ []) when is_pid(pid) do
  mb = Keyword.get(opts, :mb, 1)
  
  # Send message to process to allocate memory
  send(pid, {:simulate_memory_pressure, mb})
  :ok
end

defp calculate_intensity(history) do
  now = System.system_time(:millisecond)
  recent_restarts = Enum.filter(history, fn event ->
    (now - event.timestamp) < 60_000  # Last minute
  end)
  
  length(recent_restarts) / 60.0
end
```

#### Step 3: Run All Tests
Execute complete test suite including new advanced features.

#### Step 4: Conduct Critical Compliance Review
- ✅ Do all supervisor manipulations maintain OTP compliance?
- ✅ Are failure simulations safe and controllable?
- ✅ Does restart tracking provide valuable insights?
- ✅ Are all edge cases handled properly?

#### Step 5: Refactor and Enhance
Add robust error handling, comprehensive documentation, and performance optimizations.

---

## Prompt 4: System Analysis Dashboard with TDD

### Objective
Create a comprehensive LiveView dashboard for system-wide analysis and debugging using TDD principles.

### Required Reading
- **Test Standards**: `docs/code-standards/test-design-reference.md` (LiveView testing patterns)
- **Source Code**: `lib/otp_supervisor_web/live/supervisor_live.ex` (LiveView patterns)
- **Source Code**: `lib/otp_supervisor_web/router.ex` (Routing patterns)
- **Foundation**: `docs/foundational-concepts/foundation-requirements.md` (Process inspection section)

### TDD Development Process

#### Step 1: Write Failing Tests
Create comprehensive test suite for dashboard functionality:

```elixir
# In test/otp_supervisor_web/live/system_dashboard_live_test.exs

defmodule OtpSupervisorWeb.SystemDashboardLiveTest do
  use OtpSupervisorWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  
  describe "system dashboard" do
    setup %{conn: conn} do
      %{supervisor: supervisor} = SupervisorTestHelper.setup_isolated_supervisor("dashboard")
      {:ok, conn: conn, supervisor: supervisor}
    end

    test "renders system metrics overview", %{conn: conn} do
      {:ok, view, html} = live(conn, "/system")
      
      assert html =~ "System Overview"
      assert html =~ "Total Processes"
      assert html =~ "Memory Usage"
      assert html =~ "Message Queues"
      assert html =~ "CPU Usage"
      assert html =~ "Supervision Health"
    end

    test "displays real-time process count", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/system")
      
      # Get initial count
      initial_html = render(view)
      assert initial_html =~ ~r/Total Processes:.*\d+/
      
      # Create a new process
      unique_id = :erlang.unique_integer([:positive])
      {:ok, _pid} = start_supervised({Agent, fn -> 0 end}, id: :"test_agent_#{unique_id}")
      
      # Dashboard should update automatically
      assert_receive {:phoenix_live_view, :updated, _}
      
      updated_html = render(view)
      assert updated_html =~ "Total Processes"
    end

    test "shows anomaly detection results", %{conn: conn} do
      {:ok, view, html} = live(conn, "/system")
      
      assert html =~ "Anomaly Detection"
      assert html =~ "Memory Growth"
      assert html =~ "Growing Queues"
      assert html =~ "Unsupervised Processes"
    end

    test "provides process search functionality", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/system")
      
      # Search for supervisor processes
      html = view
      |> form("#process-search", %{search: "supervisor"})
      |> render_submit()
      
      assert html =~ "Search Results"
      assert html =~ supervisor
    end

    test "enables bulk operations on processes", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/system")
      
      # Select processes for bulk operation
      html = view
      |> element("#bulk-operations")
      |> render_click()
      
      assert html =~ "Bulk Operations"
      assert html =~ "Selected Processes"
    end

    test "exports system report as JSON", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/system")
      
      # Trigger export
      html = view
      |> element("#export-json")
      |> render_click()
      
      assert html =~ "Export Complete"
    end
  end

  describe "process relationship explorer" do
    test "displays process links and monitors", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/system")
      
      # Navigate to relationship explorer
      html = view
      |> element("#relationship-explorer")
      |> render_click()
      
      assert html =~ "Process Relationships"
      assert html =~ "Links"
      assert html =~ "Monitors"
    end
  end
end
```

#### Step 2: Implement Dashboard LiveView
Create the system dashboard with comprehensive features:

```elixir
# In lib/otp_supervisor_web/live/system_dashboard_live.ex

defmodule OtpSupervisorWeb.SystemDashboardLive do
  use OtpSupervisorWeb, :live_view
  
  alias OtpSupervisor.Core.Control
  alias OtpSupervisor.Core.SystemAnalyzer
  
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
      |> assign(:search_results, [])
      |> assign(:selected_processes, [])
      
    {:ok, socket}
  end
  
  @impl true
  def handle_event("search_processes", %{"search" => query}, socket) do
    results = SystemAnalyzer.search_processes(query)
    {:noreply, assign(socket, :search_results, results)}
  end
  
  def handle_event("toggle_process_selection", %{"pid" => pid_string}, socket) do
    selected = socket.assigns.selected_processes
    
    new_selected = 
      if pid_string in selected do
        List.delete(selected, pid_string)
      else
        [pid_string | selected]
      end
    
    {:noreply, assign(socket, :selected_processes, new_selected)}
  end
  
  def handle_event("bulk_kill_processes", _params, socket) do
    selected = socket.assigns.selected_processes
    
    Enum.each(selected, fn pid_string ->
      case Control.to_pid(pid_string) do
        {:ok, pid} -> Control.kill_process(pid)
        _ -> :ok
      end
    end)
    
    {:noreply, 
     socket
     |> assign(:selected_processes, [])
     |> put_flash(:info, "Killed #{length(selected)} processes")
    }
  end
  
  def handle_event("export_system_report", _params, socket) do
    report = SystemAnalyzer.generate_system_report()
    
    # In a real implementation, this would trigger a download
    {:noreply, put_flash(socket, :info, "System report exported")}
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
  
  defp detect_anomalies do
    SystemAnalyzer.detect_anomalies()
  end
  
  defp get_queue_lengths do
    Process.list()
    |> Enum.map(fn pid ->
      case Process.info(pid, :message_queue_len) do
        {:message_queue_len, len} -> len
        _ -> 0
      end
    end)
    |> Enum.sum()
  end
  
  defp get_cpu_usage do
    # Simplified CPU usage calculation
    :cpu_sup.avg1() / 256.0
  end
  
  defp calculate_supervision_health do
    supervisors = Control.list_supervisors()
    
    if length(supervisors) > 0 do
      healthy_count = Enum.count(supervisors, &supervisor_healthy?/1)
      (healthy_count / length(supervisors)) * 100
    else
      100.0
    end
  end
  
  defp supervisor_healthy?(supervisor) do
    case Control.get_supervision_tree(supervisor) do
      {:ok, _children} -> true
      _ -> false
    end
  end
end
```

#### Step 3: Create Dashboard Template
Create comprehensive template with interactive features:

```elixir
# In lib/otp_supervisor_web/live/system_dashboard_live.html.heex

<div class="min-h-screen bg-gray-50">
  <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
    <div class="mb-8">
      <h1 class="text-3xl font-bold text-gray-900">System Dashboard</h1>
      <p class="text-gray-600">Real-time system analysis and debugging</p>
    </div>
    
    <!-- System Metrics Overview -->
    <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-5 gap-6 mb-8">
      <div class="bg-white rounded-lg shadow p-6">
        <div class="flex items-center">
          <div class="flex-shrink-0">
            <div class="w-8 h-8 bg-blue-500 rounded-full flex items-center justify-center">
              <span class="text-white font-bold">P</span>
            </div>
          </div>
          <div class="ml-4">
            <p class="text-sm font-medium text-gray-600">Total Processes</p>
            <p class="text-2xl font-semibold text-gray-900"><%= @system_metrics.total_processes %></p>
          </div>
        </div>
      </div>
      
      <div class="bg-white rounded-lg shadow p-6">
        <div class="flex items-center">
          <div class="flex-shrink-0">
            <div class="w-8 h-8 bg-green-500 rounded-full flex items-center justify-center">
              <span class="text-white font-bold">M</span>
            </div>
          </div>
          <div class="ml-4">
            <p class="text-sm font-medium text-gray-600">Memory Usage</p>
            <p class="text-2xl font-semibold text-gray-900"><%= format_bytes(@system_metrics.memory_usage) %></p>
          </div>
        </div>
      </div>
      
      <div class="bg-white rounded-lg shadow p-6">
        <div class="flex items-center">
          <div class="flex-shrink-0">
            <div class="w-8 h-8 bg-yellow-500 rounded-full flex items-center justify-center">
              <span class="text-white font-bold">Q</span>
            </div>
          </div>
          <div class="ml-4">
            <p class="text-sm font-medium text-gray-600">Message Queues</p>
            <p class="text-2xl font-semibold text-gray-900"><%= @system_metrics.message_queue_lengths %></p>
          </div>
        </div>
      </div>
      
      <div class="bg-white rounded-lg shadow p-6">
        <div class="flex items-center">
          <div class="flex-shrink-0">
            <div class="w-8 h-8 bg-red-500 rounded-full flex items-center justify-center">
              <span class="text-white font-bold">C</span>
            </div>
          </div>
          <div class="ml-4">
            <p class="text-sm font-medium text-gray-600">CPU Usage</p>
            <p class="text-2xl font-semibold text-gray-900"><%= Float.round(@system_metrics.cpu_usage, 1) %>%</p>
          </div>
        </div>
      </div>
      
      <div class="bg-white rounded-lg shadow p-6">
        <div class="flex items-center">
          <div class="flex-shrink-0">
            <div class="w-8 h-8 bg-purple-500 rounded-full flex items-center justify-center">
              <span class="text-white font-bold">S</span>
            </div>
          </div>
          <div class="ml-4">
            <p class="text-sm font-medium text-gray-600">Supervision Health</p>
            <p class="text-2xl font-semibold text-gray-900"><%= Float.round(@system_metrics.supervision_health, 1) %>%</p>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Anomaly Detection -->
    <div class="bg-white rounded-lg shadow mb-8">
      <div class="px-6 py-4 border-b border-gray-200">
        <h2 class="text-lg font-semibold text-gray-900">Anomaly Detection</h2>
      </div>
      <div class="p-6">
        <%= if length(@anomalies) > 0 do %>
          <div class="space-y-4">
            <%= for anomaly <- @anomalies do %>
              <div class="flex items-start space-x-3">
                <div class="flex-shrink-0">
                  <div class="w-6 h-6 bg-red-100 rounded-full flex items-center justify-center">
                    <span class="text-red-600 text-sm">!</span>
                  </div>
                </div>
                <div class="min-w-0 flex-1">
                  <p class="text-sm font-medium text-gray-900"><%= anomaly.type %></p>
                  <p class="text-sm text-gray-500"><%= anomaly.description %></p>
                </div>
              </div>
            <% end %>
          </div>
        <% else %>
          <p class="text-gray-500">No anomalies detected</p>
        <% end %>
      </div>
    </div>
    
    <!-- Process Search and Operations -->
    <div class="bg-white rounded-lg shadow mb-8">
      <div class="px-6 py-4 border-b border-gray-200">
        <h2 class="text-lg font-semibold text-gray-900">Process Search</h2>
      </div>
      <div class="p-6">
        <.form for={%{}} id="process-search" phx-submit="search_processes">
          <div class="flex space-x-4">
            <div class="flex-1">
              <input type="text" name="search" placeholder="Search processes..." 
                     class="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500">
            </div>
            <button type="submit" class="px-4 py-2 bg-blue-500 text-white rounded-md hover:bg-blue-600">
              Search
            </button>
          </div>
        </.form>
        
        <%= if length(@search_results) > 0 do %>
          <div class="mt-6">
            <h3 class="text-sm font-medium text-gray-900 mb-4">Search Results</h3>
            <div class="space-y-2">
              <%= for process <- @search_results do %>
                <div class="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
                  <div class="flex items-center space-x-3">
                    <input type="checkbox" phx-click="toggle_process_selection" 
                           phx-value-pid={process.pid} 
                           checked={process.pid in @selected_processes}>
                    <div>
                      <p class="text-sm font-medium text-gray-900"><%= process.name || "Unnamed" %></p>
                      <p class="text-sm text-gray-500">PID: <%= process.pid %></p>
                    </div>
                  </div>
                  <div class="text-sm text-gray-500">
                    <%= process.type %>
                  </div>
                </div>
              <% end %>
            </div>
          </div>
        <% end %>
        
        <%= if length(@selected_processes) > 0 do %>
          <div class="mt-6 flex space-x-4">
            <button phx-click="bulk_kill_processes" 
                    class="px-4 py-2 bg-red-500 text-white rounded-md hover:bg-red-600">
              Kill Selected (<%= length(@selected_processes) %>)
            </button>
          </div>
        <% end %>
      </div>
    </div>
    
    <!-- Export Controls -->
    <div class="bg-white rounded-lg shadow">
      <div class="px-6 py-4 border-b border-gray-200">
        <h2 class="text-lg font-semibold text-gray-900">Export & Reports</h2>
      </div>
      <div class="p-6">
        <div class="flex space-x-4">
          <button phx-click="export_system_report" id="export-json"
                  class="px-4 py-2 bg-green-500 text-white rounded-md hover:bg-green-600">
            Export System Report (JSON)
          </button>
          <button class="px-4 py-2 bg-blue-500 text-white rounded-md hover:bg-blue-600">
            Export Process Tree (DOT)
          </button>
        </div>
      </div>
    </div>
  </div>
</div>
```

#### Step 4: Add Route Configuration
Update router to include system dashboard:

```elixir
# In lib/otp_supervisor_web/router.ex (addition)

scope "/", OtpSupervisorWeb do
  pipe_through :browser

  get "/", PageController, :home
  live "/supervisors", SupervisorLive
  live "/system", SystemDashboardLive  # Add this line
end
```

#### Step 5: Run All Tests
Execute complete test suite including LiveView integration tests.

#### Step 6: Conduct Critical Compliance Review
- ✅ Does the dashboard provide real-time updates without performance issues?
- ✅ Are all interactive features working correctly?
- ✅ Does the anomaly detection provide valuable insights?
- ✅ Are bulk operations safe and well-controlled?
- ✅ Does the export functionality work as expected?

#### Step 7: Refactor and Enhance
Add comprehensive error handling, performance optimizations, and enhanced visualizations.

---

## Prompt 5: REST API for External Tool Integration with TDD

### Objective
Create a comprehensive REST API that enables external tools to integrate with the OTP supervisor system using TDD.

### Required Reading
- **Test Standards**: `docs/code-standards/test-design-reference.md` (Controller testing patterns)
- **Source Code**: `lib/otp_supervisor_web/controllers/` (Controller patterns)
- **Source Code**: `lib/otp_supervisor_web/router.ex` (API routing patterns)
- **Source Code**: `lib/otp_supervisor/core/control.ex` (Core API integration)

### TDD Development Process

#### Step 1: Write Failing Tests
Create comprehensive API test suite:

```elixir
# In test/otp_supervisor_web/controllers/api/v1/process_controller_test.exs

defmodule OtpSupervisorWeb.Api.V1.ProcessControllerTest do
  use OtpSupervisorWeb.ConnCase, async: true
  
  alias OtpSupervisor.Sandbox.Workers.Counter
  
  describe "GET /api/v1/processes" do
    test "lists all processes with default pagination", %{conn: conn} do
      conn = get(conn, "/api/v1/processes")
      
      assert %{
        "data" => processes,
        "meta" => %{
          "total" => total,
          "page" => 1,
          "per_page" => 50
        }
      } = json_response(conn, 200)
      
      assert is_list(processes)
      assert is_integer(total)
      assert total > 0
    end
    
    test "supports filtering by process type", %{conn: conn} do
      conn = get(conn, "/api/v1/processes", type: "supervisor")
      
      assert %{"data" => processes} = json_response(conn, 200)
      
      # All returned processes should be supervisors
      Enum.each(processes, fn process ->
        assert process["type"] == "supervisor"
      end)
    end
    
    test "supports pagination", %{conn: conn} do
      conn = get(conn, "/api/v1/processes", page: 2, per_page: 10)
      
      assert %{
        "data" => processes,
        "meta" => %{
          "page" => 2,
          "per_page" => 10
        }
      } = json_response(conn, 200)
      
      assert length(processes) <= 10
    end
  end
  
  describe "GET /api/v1/processes/:pid" do
    setup do
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"api_test_counter_#{unique_id}"
      {:ok, pid} = start_supervised({Counter, name: counter_name})
      
      {:ok, pid: pid, counter_name: counter_name}
    end
    
    test "returns detailed process information", %{conn: conn, pid: pid} do
      pid_string = inspect(pid)
      conn = get(conn, "/api/v1/processes/#{pid_string}")
      
      assert %{
        "data" => %{
          "pid" => ^pid_string,
          "name" => name,
          "type" => type,
          "memory" => memory,
          "message_queue_len" => queue_len,
          "links" => links,
          "monitors" => monitors
        }
      } = json_response(conn, 200)
      
      assert is_binary(name) or is_nil(name)
      assert is_binary(type)
      assert is_integer(memory)
      assert is_integer(queue_len)
      assert is_list(links)
      assert is_list(monitors)
    end
    
    test "returns 404 for non-existent process", %{conn: conn} do
      fake_pid = "<0.99999.0>"
      conn = get(conn, "/api/v1/processes/#{fake_pid}")
      
      assert %{
        "error" => %{
          "message" => "Process not found",
          "code" => "process_not_found"
        }
      } = json_response(conn, 404)
    end
  end
  
  describe "GET /api/v1/processes/:pid/state" do
    setup do
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"api_test_counter_#{unique_id}"
      {:ok, pid} = start_supervised({Counter, name: counter_name})
      
      {:ok, pid: pid, counter_name: counter_name}
    end
    
    test "returns GenServer state", %{conn: conn, pid: pid} do
      # Set a known state
      GenServer.cast(pid, :increment)
      GenServer.cast(pid, :increment)
      
      pid_string = inspect(pid)
      conn = get(conn, "/api/v1/processes/#{pid_string}/state")
      
      assert %{
        "data" => %{
          "state" => state,
          "type" => "genserver"
        }
      } = json_response(conn, 200)
      
      assert state == 2
    end
    
    test "returns error for non-GenServer process", %{conn: conn} do
      {:ok, task_pid} = Task.start(fn -> Process.sleep(1000) end)
      
      pid_string = inspect(task_pid)
      conn = get(conn, "/api/v1/processes/#{pid_string}/state")
      
      assert %{
        "error" => %{
          "message" => "Not a GenServer process",
          "code" => "not_genserver"
        }
      } = json_response(conn, 422)
    end
  end
  
  describe "POST /api/v1/processes/:pid/trace" do
    setup do
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"api_test_counter_#{unique_id}"
      {:ok, pid} = start_supervised({Counter, name: counter_name})
      
      {:ok, pid: pid, counter_name: counter_name}
    end
    
    test "starts message tracing", %{conn: conn, pid: pid} do
      pid_string = inspect(pid)
      
      conn = post(conn, "/api/v1/processes/#{pid_string}/trace", %{
        "max_messages" => 10,
        "duration" => 30
      })
      
      assert %{
        "data" => %{
          "status" => "tracing_started",
          "tracer_pid" => tracer_pid,
          "max_messages" => 10,
          "duration" => 30
        }
      } = json_response(conn, 200)
      
      assert is_binary(tracer_pid)
    end
  end
  
  describe "POST /api/v1/processes/:pid/message" do
    setup do
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"api_test_counter_#{unique_id}"
      {:ok, pid} = start_supervised({Counter, name: counter_name})
      
      {:ok, pid: pid, counter_name: counter_name}
    end
    
    test "sends message to process", %{conn: conn, pid: pid} do
      pid_string = inspect(pid)
      
      conn = post(conn, "/api/v1/processes/#{pid_string}/message", %{
        "message" => %{
          "type" => "cast",
          "content" => "increment"
        }
      })
      
      assert %{
        "data" => %{
          "status" => "message_sent",
          "message_type" => "cast"
        }
      } = json_response(conn, 200)
      
      # Verify message was processed
      value = GenServer.call(pid, :get_value)
      assert value == 1
    end
  end
end
```

#### Step 2: Implement Process API Controller
Create the REST API controller:

```elixir
# In lib/otp_supervisor_web/controllers/api/v1/process_controller.ex

defmodule OtpSupervisorWeb.Api.V1.ProcessController do
  use OtpSupervisorWeb, :controller
  
  alias OtpSupervisor.Core.Control
  alias OtpSupervisor.Core.MessageTracer
  
  def index(conn, params) do
    page = String.to_integer(params["page"] || "1")
    per_page = String.to_integer(params["per_page"] || "50")
    type_filter = params["type"]
    
    all_processes = Control.list_all_processes()
    
    filtered_processes = 
      if type_filter do
        Enum.filter(all_processes, fn process ->
          process.type == type_filter
        end)
      else
        all_processes
      end
    
    total = length(filtered_processes)
    
    paginated_processes = 
      filtered_processes
      |> Enum.drop((page - 1) * per_page)
      |> Enum.take(per_page)
    
    conn
    |> put_status(200)
    |> json(%{
      data: paginated_processes,
      meta: %{
        total: total,
        page: page,
        per_page: per_page,
        total_pages: ceil(total / per_page)
      }
    })
  end
  
  def show(conn, %{"pid" => pid_string}) do
    case Control.to_pid(pid_string) do
      {:ok, pid} ->
        case Control.get_process_info(pid) do
          {:ok, process_info} ->
            conn
            |> put_status(200)
            |> json(%{data: process_info})
          
          {:error, :not_found} ->
            conn
            |> put_status(404)
            |> json(%{
              error: %{
                message: "Process not found",
                code: "process_not_found"
              }
            })
        end
      
      {:error, _} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid PID format",
            code: "invalid_pid"
          }
        })
    end
  end
  
  def get_state(conn, %{"pid" => pid_string}) do
    case Control.to_pid(pid_string) do
      {:ok, pid} ->
        case Control.get_process_state(pid) do
          {:ok, state} ->
            conn
            |> put_status(200)
            |> json(%{
              data: %{
                state: state,
                type: "genserver"
              }
            })
          
          {:error, :not_a_genserver} ->
            conn
            |> put_status(422)
            |> json(%{
              error: %{
                message: "Not a GenServer process",
                code: "not_genserver"
              }
            })
        end
      
      {:error, _} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid PID format",
            code: "invalid_pid"
          }
        })
    end
  end
  
  def start_trace(conn, %{"pid" => pid_string} = params) do
    case Control.to_pid(pid_string) do
      {:ok, pid} ->
        max_messages = params["max_messages"] || 100
        duration = params["duration"] || 60
        
        case MessageTracer.trace_messages(pid, max_messages: max_messages) do
          {:ok, tracer_pid} ->
            # Schedule automatic stop
            Process.send_after(self(), {:stop_trace, pid}, duration * 1000)
            
            conn
            |> put_status(200)
            |> json(%{
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
            |> json(%{
              error: %{
                message: "Failed to start tracing",
                code: "trace_failed",
                reason: inspect(reason)
              }
            })
        end
      
      {:error, _} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid PID format",
            code: "invalid_pid"
          }
        })
    end
  end
  
  def send_message(conn, %{"pid" => pid_string, "message" => message_params}) do
    case Control.to_pid(pid_string) do
      {:ok, pid} ->
        message_type = message_params["type"]
        content = message_params["content"]
        
        result = case message_type do
          "cast" ->
            GenServer.cast(pid, String.to_atom(content))
            {:ok, "cast"}
          
          "call" ->
            try do
              GenServer.call(pid, String.to_atom(content))
              {:ok, "call"}
            rescue
              _ -> {:error, "call_failed"}
            end
          
          "send" ->
            send(pid, String.to_atom(content))
            {:ok, "send"}
          
          _ ->
            {:error, "invalid_message_type"}
        end
        
        case result do
          {:ok, type} ->
            conn
            |> put_status(200)
            |> json(%{
              data: %{
                status: "message_sent",
                message_type: type
              }
            })
          
          {:error, reason} ->
            conn
            |> put_status(422)
            |> json(%{
              error: %{
                message: "Failed to send message",
                code: reason
              }
            })
        end
      
      {:error, _} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid PID format",
            code: "invalid_pid"
          }
        })
    end
  end
end
```

#### Step 3: Add API Routes
Update router configuration:

```elixir
# In lib/otp_supervisor_web/router.ex (add API pipeline and routes)

pipeline :api do
  plug :accepts, ["json"]
  plug :put_secure_browser_headers
end

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
    post "/supervisors/:name/pause", SupervisorController, :pause
    post "/supervisors/:name/resume", SupervisorController, :resume
    put "/supervisors/:name/strategy", SupervisorController, :change_strategy
    post "/supervisors/:name/simulate-failure", SupervisorController, :simulate_failure
  end
end
```

#### Step 4: Run All Tests
Execute complete test suite including API integration tests.

#### Step 5: Conduct Critical Compliance Review
- ✅ Does the API follow RESTful conventions?
- ✅ Are all endpoints properly authenticated and authorized?
- ✅ Does error handling provide meaningful responses?
- ✅ Are all API responses consistently formatted?
- ✅ Does the API handle edge cases gracefully?

#### Step 6: Refactor and Enhance
Add comprehensive documentation, rate limiting, and OpenAPI specifications.

---

## Success Criteria for Enhanced Phase 1

After completing all revised prompts (1-5), the system will provide:

### Technical Achievements
- [ ] **Comprehensive Process Introspection**: Deep inspection of all processes with state access
- [ ] **Message Flow Analysis**: Complete message tracing and analysis capabilities  
- [ ] **Advanced Supervisor Control**: Runtime manipulation and analytics
- [ ] **Real-time System Dashboard**: Professional monitoring interface
- [ ] **Complete REST API**: External tool integration capabilities

### Educational Outcomes
- [ ] **Hands-on Learning**: Interactive exploration of OTP concepts
- [ ] **Visual Understanding**: Clear representation of abstract concepts
- [ ] **Safe Experimentation**: Controlled environment for testing scenarios
- [ ] **Real-world Relevance**: Production-applicable patterns and practices

### Quality Standards
- [ ] **Test Coverage**: > 95% test coverage with comprehensive TDD approach
- [ ] **Documentation**: Complete API documentation with examples
- [ ] **Performance**: Real-time updates without system degradation
- [ ] **Error Handling**: Robust error handling and user feedback
- [ ] **Code Quality**: Clean, maintainable, well-documented code

### Deliverables
- [ ] **Professional debugging interface** at `/supervisors`
- [ ] **System analysis dashboard** at `/system`
- [ ] **Complete REST API** at `/api/v1/*`
- [ ] **Comprehensive test suite** with TDD methodology
- [ ] **Educational documentation** and examples

## Implementation Notes

### TDD Methodology Requirements
Each prompt **must** follow the complete TDD cycle:
1. **Write failing tests first** - No implementation without tests
2. **Implement minimum viable solution** - Just enough to pass tests
3. **Run complete test suite** - Ensure no regressions
4. **Critical compliance review** - Verify all requirements met
5. **Refactor and enhance** - Improve code quality and documentation

### Educational Integration
Every feature must maintain educational value:
- Clear documentation with examples
- Inline explanations of OTP concepts
- Progressive complexity in demonstrations
- Real-world applicability

### Quality Assurance
All implementations must meet production standards:
- Comprehensive error handling
- Performance optimization
- Security considerations
- Maintainable code structure

---

This revised prompt set transforms the basic educational tool into a comprehensive, production-ready OTP introspection platform while maintaining its core educational mission through rigorous test-driven development practices.