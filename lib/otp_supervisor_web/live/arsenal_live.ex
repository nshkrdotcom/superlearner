defmodule OtpSupervisorWeb.Live.ArsenalLive do
  use Phoenix.LiveView

  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OtpSupervisorWeb.Components.Layout.TerminalPanelLayout
  alias OtpSupervisorWeb.Components.Widgets.OperationGridWidget
  alias OtpSupervisorWeb.Components.Widgets.ExecutionPanelWidget

  @moduledoc """
  Arsenal command center for OTP operations.

  Refactored to use LiveComponents for better reusability and maintainability.
  """

  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(5000, self(), :update_operations)
      Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "arsenal_updates")
    end

    {:ok,
     socket
     |> assign(:page_title, "Arsenal Command Center")
     |> assign(:current_page, "arsenal")
     |> assign(:selected_operation, nil)
     |> assign(:execution_history, [])
     |> assign(:show_execution_panel, false)
     |> load_arsenal_data()}
  end

  def handle_info(:update_operations, socket) do
    {:noreply, update_operations_data(socket)}
  end

  def handle_info({:arsenal_update, data}, socket) do
    {:noreply, handle_arsenal_update(socket, data)}
  end

  def handle_info({:operation_completed, execution_id}, socket) do
    updated_history =
      Enum.map(socket.assigns.execution_history, fn entry ->
        if entry.id == execution_id do
          %{
            entry
            | status: :completed,
              completed_at: DateTime.utc_now(),
              result: :success,
              output: "Operation completed successfully"
          }
        else
          entry
        end
      end)

    {:noreply, assign(socket, :execution_history, updated_history)}
  end

  def handle_info({:operation_selected, operation}, socket) do
    {:noreply,
     socket
     |> assign(:selected_operation, operation)
     |> assign(:show_execution_panel, true)}
  end

  def handle_info({:toggle_operation_status, operation_id}, socket) do
    operations =
      Enum.map(socket.assigns.operations, fn op ->
        if op.id == operation_id do
          new_status =
            case op.status do
              :active -> :inactive
              :inactive -> :active
              :planned -> :active
              _ -> op.status
            end

          %{op | status: new_status}
        else
          op
        end
      end)

    {:noreply, assign(socket, :operations, operations)}
  end

  def handle_info({:execute_operation, operation_id}, socket) do
    # For now, just add to execution history
    execution_entry = %{
      id: :crypto.strong_rand_bytes(16) |> Base.encode64(),
      operation_id: operation_id,
      status: :executing,
      started_at: DateTime.utc_now(),
      completed_at: nil,
      result: nil,
      output: "Operation started..."
    }

    updated_history = [execution_entry | socket.assigns.execution_history]

    {:noreply, assign(socket, :execution_history, updated_history)}
  end

  def render(assigns) do
    ~H"""
    <div class="fixed inset-0 bg-gray-900 text-green-400 flex flex-col">
      <!-- Status Bar -->
      <.live_component
        module={TerminalStatusBar}
        id="arsenal-status-bar"
        title="Arsenal Command Center"
        metrics={status_bar_metrics(assigns)}
        navigation_links={TerminalNavigationLinks.page_navigation_links("arsenal", %{})}
      />
      
    <!-- Main Layout -->
      <.live_component
        module={TerminalPanelLayout}
        id="arsenal-panel-layout"
        layout_type={if(@show_execution_panel, do: :two_panel, else: :stacked)}
        panels={arsenal_panels(assigns)}
        gap="gap-4"
        padding="p-4"
      />
    </div>
    """
  end

  # Event handlers

  def handle_event("select_operation", %{"operation_id" => operation_id}, socket) do
    operation = find_operation(operation_id, socket.assigns.operations)

    {:noreply,
     socket
     |> assign(:selected_operation, operation)
     |> assign(:show_execution_panel, true)}
  end

  def handle_event("execute_operation", %{"operation_id" => operation_id}, socket) do
    operation = find_operation(operation_id, socket.assigns.operations)

    if operation && operation.status == :active do
      # Here you would call your operation execution API
      execution_entry = %{
        id: "exec_#{:rand.uniform(10000)}",
        operation_id: operation_id,
        operation_name: operation.name,
        status: :running,
        started_at: DateTime.utc_now(),
        completed_at: nil,
        result: nil,
        output: "Starting execution..."
      }

      new_history = [execution_entry | socket.assigns.execution_history]

      # Simulate completion after 3 seconds
      Process.send_after(self(), {:operation_completed, execution_entry.id}, 3000)

      {:noreply,
       socket
       |> assign(:execution_history, new_history)
       |> put_flash(:info, "Operation #{operation.name} started")}
    else
      {:noreply, put_flash(socket, :error, "Operation cannot be executed")}
    end
  end

  def handle_event("toggle_operation_status", %{"operation_id" => operation_id}, socket) do
    operations =
      Enum.map(socket.assigns.operations, fn op ->
        if op.id == operation_id do
          new_status =
            case op.status do
              :active -> :inactive
              :inactive -> :active
              :planned -> :active
              _ -> op.status
            end

          %{op | status: new_status}
        else
          op
        end
      end)

    {:noreply,
     socket
     |> assign(:operations, operations)
     |> update_operation_counts()}
  end

  def handle_event("clear_history", _params, socket) do
    {:noreply, assign(socket, :execution_history, [])}
  end

  def handle_event("close_execution_panel", _params, socket) do
    {:noreply,
     socket
     |> assign(:selected_operation, nil)
     |> assign(:show_execution_panel, false)}
  end

  def handle_event("refresh_operations", _params, socket) do
    {:noreply, update_operations_data(socket)}
  end

  # Private functions

  defp load_arsenal_data(socket) do
    operations = get_operations()

    socket
    |> assign(:operations, operations)
    |> assign(:operation_stats, get_operation_stats(operations))
    |> update_operation_counts()
  end

  defp update_operations_data(socket) do
    operations = get_operations()

    socket
    |> assign(:operations, operations)
    |> assign(:operation_stats, get_operation_stats(operations))
    |> update_operation_counts()
  end

  defp update_operation_counts(socket) do
    operations = socket.assigns.operations

    socket
    |> assign(:active_operations_count, count_operations_by_status(operations, :active))
    |> assign(:planned_operations_count, count_operations_by_status(operations, :planned))
    |> assign(:inactive_operations_count, count_operations_by_status(operations, :inactive))
  end

  defp handle_arsenal_update(socket, %{type: :operations_update, data: operations}) do
    socket
    |> assign(:operations, operations)
    |> assign(:operation_stats, get_operation_stats(operations))
    |> update_operation_counts()
  end

  defp handle_arsenal_update(socket, %{type: :execution_update, data: execution}) do
    updated_history = [execution | socket.assigns.execution_history]
    assign(socket, :execution_history, updated_history)
  end

  defp handle_arsenal_update(socket, _data), do: socket

  defp status_bar_metrics(assigns) do
    [
      %{label: "Active", value: "#{assigns.active_operations_count}"},
      %{label: "Planned", value: "#{assigns.planned_operations_count}"},
      %{label: "Inactive", value: "#{assigns.inactive_operations_count}"},
      %{
        label: "Executing",
        value: "#{count_executions_by_status(assigns.execution_history, :running)}"
      }
    ]
  end

  defp arsenal_panels(assigns) do
    # Operation Grid Widget (always shown)
    operations_grid = %{
      title: "Operations Grid",
      component: OperationGridWidget,
      assigns: %{
        id: "operations-grid",
        operations: operation_grid_data(assigns),
        selected_operation: assigns.selected_operation,
        grid_size: "grid-cols-8",
        show_status: true,
        interactive: true,
        filters: %{},
        compact_mode: false
      }
    }

    if assigns.show_execution_panel do
      # When showing execution panel, show both panels side by side
      execution_panel = %{
        title: "Execution Panel",
        component: ExecutionPanelWidget,
        assigns: %{
          id: "execution-panel",
          executions: execution_panel_data(assigns),
          live_output: [],
          show_output: true,
          max_history: 50,
          auto_scroll: true,
          compact_mode: false
        }
      }

      [operations_grid, execution_panel]
    else
      # Only show operations grid when execution panel is hidden
      [operations_grid]
    end
  end

  # Mock data functions (replace with real implementations)

  defp get_operations do
    # Get real Arsenal operations from the registry
    real_operations =
      case OTPSupervisor.Core.Arsenal.list_operations() do
        operations when is_list(operations) -> operations
        _ -> []
      end

    # Map real operations to the expected format
    real_ops =
      Enum.map(real_operations, fn op ->
        %{
          id: op.module |> to_string() |> String.replace("Elixir.", ""),
          name: op.module |> to_string(),
          description: Map.get(op, :description, "Arsenal operation"),
          category: categorize_operation(op.module),
          priority: Map.get(op, :priority, :medium),
          status: :active,
          icon: get_operation_icon(op.module),
          execution_count: Enum.random(0..100),
          last_executed: DateTime.add(DateTime.utc_now(), -:rand.uniform(86400), :second)
        }
      end)

    # If we have fewer than 200 operations, add comprehensive test data to fill the grid
    if length(real_ops) < 200 do
      test_ops = generate_test_operations(200 - length(real_ops))
      real_ops ++ test_ops
    else
      real_ops
    end
  end

  defp categorize_operation(module) do
    module_name = to_string(module)

    cond do
      String.contains?(module_name, "Process") -> "Process"
      String.contains?(module_name, "Supervisor") -> "Supervisor"
      String.contains?(module_name, "Trace") -> "Debug"
      String.contains?(module_name, "Kill") -> "Process"
      String.contains?(module_name, "List") -> "System"
      String.contains?(module_name, "Send") -> "Network"
      true -> "System"
    end
  end

  defp get_operation_icon(module) do
    module_name = to_string(module)

    cond do
      String.contains?(module_name, "Process") -> "🔍"
      String.contains?(module_name, "Kill") -> "⚡"
      String.contains?(module_name, "Trace") -> "🔧"
      String.contains?(module_name, "List") -> "📊"
      String.contains?(module_name, "Send") -> "📡"
      true -> "⚙️"
    end
  end

  defp generate_test_operations(count) do
    # Comprehensive operations from the Arsenal manual
    operation_names = [
      # Process Lifecycle Management
      "StartProcess",
      "StartLinkProcess",
      "StartMonitorProcess",
      "SpawnProcess",
      "SpawnLinkProcess",
      "SpawnMonitorProcess",
      "KillProcess",
      "ExitProcess",
      "ShutdownProcess",
      "TerminateProcessTree",
      "HibernateProcess",
      "WakeProcess",
      "SuspendProcess",
      "ResumeProcess",
      "PauseProcessScheduling",
      "ResumeProcessScheduling",
      "SetProcessPriority",
      "MigrateProcess",
      "PinProcessToScheduler",
      "UnpinProcessFromScheduler",
      "GarbageCollectProcess",
      "CompactProcessHeap",

      # Supervisor Management
      "StartSupervisor",
      "StartSupervisorLink",
      "StopSupervisor",
      "RestartSupervisor",
      "PauseSupervisor",
      "ResumeSupervisor",
      "WhichChildren",
      "CountChildren",
      "DeleteChild",
      "RestartChild",
      "TerminateChild",
      "ChangeSupervisorStrategy",
      "ChangeSupervisorIntensity",
      "ChangeSupervisorPeriod",
      "GetSupervisorFlags",
      "SetSupervisorFlags",
      "ReplaceChildSpec",
      "AddDynamicChild",
      "StartTemporaryChild",
      "StartTransientChild",
      "StartPermanentChild",

      # System Introspection
      "GetProcessInfo",
      "GetProcessState",
      "GetProcessBacktrace",
      "GetProcessDictionary",
      "GetProcessMemoryInfo",
      "GetProcessLinks",
      "GetProcessMonitors",
      "GetProcessTrapExit",
      "GetProcessRegisteredName",
      "GetProcessGroupLeader",
      "ListAllProcesses",
      "ListRegisteredProcesses",
      "ListSupervisorProcesses",
      "ListGenServerProcesses",
      "ListGenEventProcesses",
      "ListGenStateMProcesses",
      "ListApplicationProcesses",
      "GetSystemInfo",
      "GetSchedulerInfo",
      "GetMemoryInfo",
      "BuildProcessTree",
      "BuildSupervisionTree",
      "FindProcessAncestors",
      "FindProcessDescendants",
      "GetProcessDependencies",
      "AnalyzeProcessRelationships",
      "FindProcessCycles",
      "GetIsolatedProcesses",

      # Message Passing and Communication
      "SendMessage",
      "SendMessageAfter",
      "SendExitSignal",
      "CastMessage",
      "CallProcess",
      "CallProcessSync",
      "MultiCallProcesses",
      "BroadcastMessage",
      "SelectiveSend",
      "GetMessageQueueLength",
      "FlushMessageQueue",
      "PeekMessageQueue",
      "DrainMessageQueue",
      "FilterMessageQueue",
      "PrioritizeMessageQueue",
      "GetMessageQueueStats",
      "InspectMailbox",
      "CountMailboxMessages",
      "SearchMailbox",
      "CompactMailbox",
      "AnalyzeMailboxPatterns",

      # Process Registry and Naming
      "RegisterProcess",
      "UnregisterProcess",
      "WhereisProcess",
      "RegisteredNames",
      "RegisterProcessGlobally",
      "UnregisterProcessGlobally",
      "GlobalWhereisName",
      "RegisterProcessWithMetadata",
      "UpdateProcessMetadata",
      "SearchRegistryByMetadata",
      "ListRegistryContents",
      "CreateProcessGroup",
      "JoinProcessGroup",
      "LeaveProcessGroup",
      "ListProcessGroups",
      "BroadcastToGroup",

      # Tracing and Debugging
      "TraceProcess",
      "TraceCalls",
      "TraceMessages",
      "TraceGarbageCollection",
      "TraceProcessEvents",
      "StopTracing",
      "GetTraceData",
      "ClearTraceData",
      "SetDebugFlags",
      "GetDebugInfo",
      "EnableSysDebug",
      "DisableSysDebug",
      "GetSysDebugLog",
      "TraceFunctionCalls",
      "SetBreakpoint",
      "RemoveBreakpoint",
      "StepExecution",
      "ProfileProcess",
      "ProfileFunctionCalls",
      "MeasureExecutionTime",
      "GetReductionCount",
      "BenchmarkOperation",
      "AnalyzeHotspots",
      "MemoryProfiling",

      # State Management
      "GetGenServerState",
      "SetGenServerState",
      "UpdateGenServerState",
      "BackupGenServerState",
      "RestoreGenServerState",
      "DiffGenServerStates",
      "ValidateGenServerState",
      "CompressGenServerState",
      "PersistProcessState",
      "LoadProcessState",
      "CheckpointProcessState",
      "RollbackProcessState",
      "MigrateProcessState",
      "ReplicateProcessState",

      # Error Handling and Recovery
      "GetLastError",
      "GetErrorHistory",
      "GetCrashDump",
      "AnalyzeErrorPatterns",
      "PredictErrorLikelihood",
      "GetErrorContext",
      "AutoRestartOnFailure",
      "DisableAutoRestart",
      "SetRestartStrategy",
      "CreateRecoveryPlan",
      "ExecuteRecoveryPlan",
      "TestRecoveryScenario",
      "SimulateProcessCrash",
      "SimulateNetworkPartition",
      "SimulateResourceExhaustion",
      "SimulateTimeout",
      "InjectFault",
      "ChaosMonkey",

      # Application Management
      "StartApplication",
      "StopApplication",
      "RestartApplication",
      "GetApplicationInfo",
      "ListApplications",
      "WhichApplications",
      "ApplicationControllerInfo",
      "GetApplicationEnv",
      "SetApplicationEnv",
      "UnsetApplicationEnv",
      "GetAllApplicationEnv",
      "LoadApplicationConfig",
      "ReloadApplicationConfig",

      # Hot Code Reloading
      "LoadModule",
      "ReloadModule",
      "PurgeModule",
      "SoftPurgeModule",
      "CheckModuleCompatibility",
      "GetModuleInfo",
      "ListLoadedModules",
      "UpgradeProcessCode",
      "DowngradeProcessCode",
      "SuspendForUpgrade",
      "ResumeAfterUpgrade",
      "RollbackCodeUpgrade",
      "ValidateCodeUpgrade",

      # Distributed Operations
      "ConnectNode",
      "DisconnectNode",
      "ListConnectedNodes",
      "PingNode",
      "MonitorNode",
      "DemonitorNode",
      "SpawnProcessOnNode",
      "MigrateProcessToNode",
      "ReplicateProcessToNodes",
      "FindProcessOnNodes",
      "BroadcastToAllNodes",
      "GatherFromAllNodes",

      # Resource Management
      "GetProcessMemoryUsage",
      "ForceGarbageCollection",
      "SetMemoryLimit",
      "MonitorMemoryUsage",
      "OptimizeMemoryUsage",
      "DetectMemoryLeaks",
      "SetProcessHeapSize",
      "SetProcessStackSize",
      "SetMessageQueueLimit",
      "MonitorResourceUsage",
      "EnforceResourceLimits",
      "GetResourceQuotas",

      # Performance Monitoring
      "GetProcessStatistics",
      "GetSystemStatistics",
      "MonitorProcessPerformance",
      "GetSchedulerUtilization",
      "GetIOStatistics",
      "GetNetworkStatistics",
      "AnalyzePerformanceTrends",
      "IdentifyBottlenecks",
      "SuggestOptimizations",
      "BenchmarkSystemOperations",
      "ProfileSystemCalls",
      "AnalyzeContention",

      # Advanced OTP Patterns
      "ImplementBackpressure",
      "SetupCircuitBreaker",
      "ImplementRateLimiting",
      "SetupBulkheading",
      "ImplementGracefulDegradation",
      "SetupHealthChecks",
      "ImplementSupervisionBridge",
      "SetupSupervisorHierarchy",
      "ImplementDynamicSupervision",
      "SetupSupervisorPools",
      "ImplementCircuitBreakerSupervisor",
      "SetupRestartStrategies",
      "CreateWorkerPool",
      "ResizeWorkerPool",
      "BalancePoolLoad",
      "MonitorPoolHealth",
      "ImplementPoolBackpressure",
      "SetupPoolOverflow"
    ]

    categories = [
      "Process",
      "Supervisor",
      "System",
      "Network",
      "Debug",
      "State",
      "Error",
      "Application",
      "Code",
      "Distributed",
      "Resource",
      "Performance",
      "Pattern",
      "Security",
      "Test"
    ]

    priorities = [:low, :medium, :high, :critical]
    statuses = [:active, :planned, :inactive]

    icons = [
      "⚡",
      "🔧",
      "🔍",
      "📊",
      "🛠️",
      "⚙️",
      "🔄",
      "📈",
      "🔐",
      "🌐",
      "💾",
      "🔋",
      "📡",
      "🎯",
      "🚀",
      "🔥",
      "⚠️",
      "💡",
      "🏗️",
      "🔒",
      "🎮",
      "🧪",
      "⏰",
      "🔗",
      "📦",
      "🎪",
      "🏃",
      "🎨",
      "🔌",
      "🎯"
    ]

    for i <- 1..count do
      operation_name = Enum.at(operation_names, rem(i - 1, length(operation_names)))

      %{
        id: "test_op_#{i}",
        name: operation_name,
        description: "Arsenal operation: #{operation_name}",
        category: Enum.random(categories),
        priority: Enum.random(priorities),
        status: Enum.random(statuses),
        icon: Enum.random(icons),
        execution_count: :rand.uniform(100),
        last_executed: DateTime.add(DateTime.utc_now(), -:rand.uniform(86400), :second)
      }
    end
  end

  defp get_operation_stats(operations) do
    total = length(operations)
    success_rate = 85.0 + :rand.uniform(15)
    avg_execution_time = 50 + :rand.uniform(200)
    executions_24h = :rand.uniform(50)

    %{
      total: total,
      success_rate: success_rate,
      avg_execution_time: avg_execution_time,
      executions_24h: executions_24h
    }
  end

  defp count_operations_by_status(operations, status) do
    Enum.count(operations, &(&1.status == status))
  end

  defp count_executions_by_status(executions, status) do
    Enum.count(executions, &(&1.status == status))
  end

  defp find_operation(operation_id, operations) do
    Enum.find(operations, &(&1.id == operation_id))
  end

  # New data transformation functions for specialized widgets

  defp operation_grid_data(assigns) do
    assigns.operations
  end

  defp execution_panel_data(assigns) do
    assigns.execution_history
  end
end
