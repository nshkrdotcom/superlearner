# SANDBOX V2 Implementation Guide

## Overview

The SANDBOX V2 system provides application-level process isolation for safe experimentation with OTP supervisors and processes. This guide covers the complete implementation, from core architecture to practical usage patterns.

## Core Architecture

### System Components

```
┌─────────────────────────────────────────────────────────────┐
│                     Host Application                         │
│  ┌─────────────────┐  ┌──────────────┐  ┌───────────────┐  │
│  │ SandboxManager  │  │ Control API  │  │ Analytics     │  │
│  │ (GenServer)     │  │              │  │               │  │
│  └─────────────────┘  └──────────────┘  └───────────────┘  │
└────────────────────────────┬────────────────────────────────┘
                             │
┌────────────────────────────┴────────────────────────────────┐
│                   Sandbox Applications                       │
│  ┌─────────────────┐  ┌──────────────┐  ┌───────────────┐  │
│  │   Sandbox A     │  │   Sandbox B  │  │   Sandbox C   │  │
│  │  (Application)  │  │ (Application)│  │ (Application) │  │
│  └─────────────────┘  └──────────────┘  └───────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### Key Design Principles

1. **Application Isolation**: Each sandbox runs as a separate OTP application
2. **Resource Management**: Controlled memory and process limits
3. **Hot Reloading**: Dynamic code compilation and loading
4. **State Persistence**: Optional state snapshots for experiment continuity
5. **Monitoring Integration**: Full observability into sandbox behavior

## Implementation Details

### 1. SandboxManager GenServer

```elixir
defmodule OtpSupervisor.Core.SandboxManager do
  use GenServer
  require Logger

  @ets_table :sandbox_registry
  
  defstruct [
    :sandbox_id,
    :app_name,
    :supervisor_pid,
    :start_time,
    :config,
    :status,
    :resource_limits,
    :analytics_data
  ]

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    :ets.new(@ets_table, [:named_table, :public, :set])
    Process.flag(:trap_exit, true)
    {:ok, %{sandboxes: %{}, next_id: 1}}
  end

  # Public API
  def create_sandbox(config) do
    GenServer.call(__MODULE__, {:create_sandbox, config})
  end

  def destroy_sandbox(sandbox_id) do
    GenServer.call(__MODULE__, {:destroy_sandbox, sandbox_id})
  end

  def list_sandboxes do
    GenServer.call(__MODULE__, :list_sandboxes)
  end

  def get_sandbox(sandbox_id) do
    GenServer.call(__MODULE__, {:get_sandbox, sandbox_id})
  end

  # GenServer Callbacks
  def handle_call({:create_sandbox, config}, _from, state) do
    case create_sandbox_impl(config, state.next_id) do
      {:ok, sandbox} ->
        new_state = %{
          state |
          sandboxes: Map.put(state.sandboxes, sandbox.sandbox_id, sandbox),
          next_id: state.next_id + 1
        }
        :ets.insert(@ets_table, {sandbox.sandbox_id, sandbox})
        {:reply, {:ok, sandbox}, new_state}
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:destroy_sandbox, sandbox_id}, _from, state) do
    case Map.get(state.sandboxes, sandbox_id) do
      nil ->
        {:reply, {:error, :not_found}, state}
      
      sandbox ->
        :ok = destroy_sandbox_impl(sandbox)
        new_state = %{state | sandboxes: Map.delete(state.sandboxes, sandbox_id)}
        :ets.delete(@ets_table, sandbox_id)
        {:reply, :ok, new_state}
    end
  end

  def handle_call(:list_sandboxes, _from, state) do
    sandboxes = Map.values(state.sandboxes)
    {:reply, {:ok, sandboxes}, state}
  end

  def handle_call({:get_sandbox, sandbox_id}, _from, state) do
    case Map.get(state.sandboxes, sandbox_id) do
      nil -> {:reply, {:error, :not_found}, state}
      sandbox -> {:reply, {:ok, sandbox}, state}
    end
  end

  # Implementation Functions
  defp create_sandbox_impl(config, sandbox_id) do
    app_name = :"sandbox_#{sandbox_id}"
    
    with {:ok, code} <- compile_sandbox_code(config),
         {:ok, supervisor_pid} <- start_sandbox_application(app_name, code, config),
         {:ok, analytics} <- setup_analytics(sandbox_id) do
      
      sandbox = %__MODULE__{
        sandbox_id: sandbox_id,
        app_name: app_name,
        supervisor_pid: supervisor_pid,
        start_time: DateTime.utc_now(),
        config: config,
        status: :running,
        resource_limits: config[:resource_limits] || default_limits(),
        analytics_data: analytics
      }
      
      Logger.info("Created sandbox #{sandbox_id} with app #{app_name}")
      {:ok, sandbox}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp destroy_sandbox_impl(sandbox) do
    # Stop application
    :application.stop(sandbox.app_name)
    :application.unload(sandbox.app_name)
    
    # Clean up analytics
    cleanup_analytics(sandbox.sandbox_id)
    
    # Force cleanup any remaining processes
    cleanup_processes(sandbox.supervisor_pid)
    
    Logger.info("Destroyed sandbox #{sandbox.sandbox_id}")
    :ok
  end

  defp default_limits do
    %{
      max_processes: 100,
      max_memory: 50 * 1024 * 1024,  # 50MB
      max_runtime: 300_000  # 5 minutes
    }
  end
end
```

### 2. Dynamic Code Compilation

```elixir
defmodule OtpSupervisor.Core.CodeCompiler do
  @moduledoc """
  Handles dynamic compilation of sandbox code
  """

  def compile_sandbox_code(config) do
    case config[:code_type] do
      :module_source -> compile_from_source(config[:source])
      :beam_files -> load_beam_files(config[:beam_files])
      :git_repo -> compile_from_repo(config[:repo_url], config[:commit])
      _ -> {:error, :invalid_code_type}
    end
  end

  defp compile_from_source(source_code) do
    try do
      # Create temporary directory
      temp_dir = create_temp_dir()
      
      # Write source files
      source_files = write_source_files(temp_dir, source_code)
      
      # Compile with elixir compiler
      case System.cmd("elixirc", ["-o", temp_dir | source_files]) do
        {_output, 0} ->
          beam_files = list_beam_files(temp_dir)
          {:ok, %{beam_files: beam_files, temp_dir: temp_dir}}
        
        {error, _code} ->
          File.rm_rf!(temp_dir)
          {:error, {:compilation_failed, error}}
      end
    rescue
      error -> {:error, {:compilation_error, error}}
    end
  end

  defp load_beam_files(beam_files) do
    try do
      loaded_modules = 
        beam_files
        |> Enum.map(fn {module, beam_data} ->
          {:module, module} = :code.load_binary(module, ~c"sandbox", beam_data)
          module
        end)
      
      {:ok, %{modules: loaded_modules}}
    rescue
      error -> {:error, {:load_error, error}}
    end
  end

  defp compile_from_repo(repo_url, commit) do
    try do
      temp_dir = create_temp_dir()
      
      # Clone repository
      case System.cmd("git", ["clone", repo_url, temp_dir]) do
        {_output, 0} ->
          # Checkout specific commit
          System.cmd("git", ["checkout", commit], cd: temp_dir)
          
          # Compile with mix
          case System.cmd("mix", ["compile"], cd: temp_dir) do
            {_output, 0} ->
              beam_files = find_compiled_beams(temp_dir)
              {:ok, %{beam_files: beam_files, temp_dir: temp_dir}}
            
            {error, _code} ->
              File.rm_rf!(temp_dir)
              {:error, {:compilation_failed, error}}
          end
        
        {error, _code} ->
          File.rm_rf!(temp_dir)
          {:error, {:git_clone_failed, error}}
      end
    rescue
      error -> {:error, {:repo_error, error}}
    end
  end

  defp create_temp_dir do
    temp_base = System.tmp_dir!()
    temp_dir = Path.join(temp_base, "sandbox_#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(temp_dir)
    temp_dir
  end
end
```

### 3. Application Lifecycle Management

```elixir
defmodule OtpSupervisor.Core.ApplicationManager do
  @moduledoc """
  Manages sandbox application lifecycle
  """

  def start_sandbox_application(app_name, compiled_code, config) do
    # Define application specification
    app_spec = build_app_spec(app_name, compiled_code, config)
    
    # Load application
    :application.load(app_spec)
    
    # Start application with supervision
    case :application.start(app_name, :temporary) do
      :ok ->
        {:ok, supervisor_pid} = get_app_supervisor(app_name)
        setup_monitoring(supervisor_pid, app_name)
        {:ok, supervisor_pid}
      
      {:error, reason} ->
        :application.unload(app_name)
        {:error, reason}
    end
  end

  defp build_app_spec(app_name, compiled_code, config) do
    {app_name, [
      description: "Sandbox Application #{app_name}",
      vsn: "1.0.0",
      modules: compiled_code[:modules] || [],
      registered: [],
      applications: [:kernel, :stdlib] ++ (config[:dependencies] || []),
      mod: {config[:main_module] || SandboxApp, []},
      env: config[:env] || []
    ]}
  end

  defp setup_monitoring(supervisor_pid, app_name) do
    # Monitor supervisor process
    ref = Process.monitor(supervisor_pid)
    
    # Store monitoring info
    :ets.insert(:sandbox_monitors, {app_name, {supervisor_pid, ref}})
    
    # Set up resource monitoring
    spawn_link(fn -> resource_monitor_loop(supervisor_pid, app_name) end)
  end

  defp resource_monitor_loop(supervisor_pid, app_name) do
    Process.sleep(1000)
    
    case Process.alive?(supervisor_pid) do
      true ->
        check_resource_limits(supervisor_pid, app_name)
        resource_monitor_loop(supervisor_pid, app_name)
      
      false ->
        :ok
    end
  end

  defp check_resource_limits(supervisor_pid, app_name) do
    children = get_all_children(supervisor_pid)
    
    # Check process count
    if length(children) > get_process_limit(app_name) do
      Logger.warn("Sandbox #{app_name} exceeded process limit")
      notify_limit_exceeded(app_name, :process_limit)
    end
    
    # Check memory usage
    total_memory = children |> Enum.map(&process_memory/1) |> Enum.sum()
    if total_memory > get_memory_limit(app_name) do
      Logger.warn("Sandbox #{app_name} exceeded memory limit")
      notify_limit_exceeded(app_name, :memory_limit)
    end
  end
end
```

### 4. Control API Integration

```elixir
defmodule OtpSupervisor.Core.Control do
  @moduledoc """
  High-level control interface for sandbox operations
  """

  alias OtpSupervisor.Core.SandboxManager

  def create_experiment(experiment_config) do
    # Validate configuration
    with {:ok, validated_config} <- validate_config(experiment_config),
         {:ok, sandbox} <- SandboxManager.create_sandbox(validated_config),
         {:ok, _analytics} <- start_analytics_collection(sandbox.sandbox_id) do
      
      {:ok, %{
        sandbox_id: sandbox.sandbox_id,
        status: :running,
        app_name: sandbox.app_name,
        created_at: sandbox.start_time
      }}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def run_experiment_step(sandbox_id, step_config) do
    with {:ok, sandbox} <- SandboxManager.get_sandbox(sandbox_id),
         {:ok, result} <- execute_step(sandbox, step_config) do
      
      # Record step execution
      record_step_analytics(sandbox_id, step_config, result)
      
      {:ok, result}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def get_experiment_state(sandbox_id) do
    with {:ok, sandbox} <- SandboxManager.get_sandbox(sandbox_id) do
      state = %{
        sandbox_id: sandbox_id,
        status: sandbox.status,
        runtime: DateTime.diff(DateTime.utc_now(), sandbox.start_time),
        processes: get_sandbox_processes(sandbox.supervisor_pid),
        memory_usage: get_sandbox_memory(sandbox.supervisor_pid),
        supervision_tree: get_supervision_tree(sandbox.supervisor_pid)
      }
      
      {:ok, state}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def snapshot_experiment(sandbox_id, snapshot_name) do
    with {:ok, sandbox} <- SandboxManager.get_sandbox(sandbox_id),
         {:ok, snapshot} <- create_state_snapshot(sandbox, snapshot_name) do
      
      store_snapshot(sandbox_id, snapshot_name, snapshot)
      {:ok, %{snapshot_name: snapshot_name, created_at: DateTime.utc_now()}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def restore_experiment(sandbox_id, snapshot_name) do
    with {:ok, sandbox} <- SandboxManager.get_sandbox(sandbox_id),
         {:ok, snapshot} <- load_snapshot(sandbox_id, snapshot_name),
         {:ok, _result} <- restore_state_snapshot(sandbox, snapshot) do
      
      {:ok, %{restored_from: snapshot_name, restored_at: DateTime.utc_now()}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def destroy_experiment(sandbox_id) do
    with {:ok, _result} <- SandboxManager.destroy_sandbox(sandbox_id) do
      cleanup_analytics(sandbox_id)
      cleanup_snapshots(sandbox_id)
      {:ok, %{destroyed_at: DateTime.utc_now()}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Implementation helpers
  defp execute_step(sandbox, step_config) do
    case step_config[:type] do
      :message_send ->
        send_message_to_process(sandbox, step_config)
      
      :process_spawn ->
        spawn_process_in_sandbox(sandbox, step_config)
      
      :supervisor_restart ->
        restart_supervisor_child(sandbox, step_config)
      
      :code_reload ->
        reload_code_in_sandbox(sandbox, step_config)
      
      _ ->
        {:error, :unknown_step_type}
    end
  end

  defp send_message_to_process(sandbox, config) do
    target = config[:target]
    message = config[:message]
    
    case find_process_by_name(sandbox.supervisor_pid, target) do
      {:ok, pid} ->
        send(pid, message)
        {:ok, %{sent_to: pid, message: message}}
      
      {:error, reason} ->
        {:error, reason}
    end
  end
end
```

### 5. Analytics and Monitoring

```elixir
defmodule OtpSupervisor.Core.Analytics do
  use GenServer

  def start_link(sandbox_id) do
    GenServer.start_link(__MODULE__, sandbox_id, name: via_tuple(sandbox_id))
  end

  def record_event(sandbox_id, event_type, event_data) do
    GenServer.cast(via_tuple(sandbox_id), {:record_event, event_type, event_data})
  end

  def get_metrics(sandbox_id) do
    GenServer.call(via_tuple(sandbox_id), :get_metrics)
  end

  def init(sandbox_id) do
    state = %{
      sandbox_id: sandbox_id,
      events: [],
      start_time: DateTime.utc_now(),
      metrics: %{
        process_spawns: 0,
        process_exits: 0,
        messages_sent: 0,
        supervisor_restarts: 0,
        memory_peak: 0
      }
    }
    
    # Schedule periodic metric collection
    :timer.send_interval(1000, :collect_metrics)
    
    {:ok, state}
  end

  def handle_cast({:record_event, event_type, event_data}, state) do
    event = %{
      timestamp: DateTime.utc_now(),
      type: event_type,
      data: event_data
    }
    
    new_state = %{
      state |
      events: [event | state.events],
      metrics: update_metrics(state.metrics, event_type, event_data)
    }
    
    {:noreply, new_state}
  end

  def handle_call(:get_metrics, _from, state) do
    metrics = %{
      runtime_seconds: DateTime.diff(DateTime.utc_now(), state.start_time),
      event_count: length(state.events),
      metrics: state.metrics,
      recent_events: Enum.take(state.events, 10)
    }
    
    {:reply, {:ok, metrics}, state}
  end

  def handle_info(:collect_metrics, state) do
    # Collect current sandbox metrics
    current_metrics = collect_sandbox_metrics(state.sandbox_id)
    
    # Update peak memory if necessary
    new_metrics = %{
      state.metrics |
      memory_peak: max(state.metrics.memory_peak, current_metrics.memory_usage)
    }
    
    {:noreply, %{state | metrics: new_metrics}}
  end

  defp via_tuple(sandbox_id) do
    {:via, Registry, {SandboxAnalytics, sandbox_id}}
  end
end
```

## Usage Examples

### 1. Basic Sandbox Creation

```elixir
# Create a simple GenServer sandbox
config = %{
  code_type: :module_source,
  source: %{
    "my_server.ex" => """
    defmodule MyServer do
      use GenServer
      
      def start_link(init_arg) do
        GenServer.start_link(__MODULE__, init_arg, name: __MODULE__)
      end
      
      def init(state) do
        {:ok, state}
      end
      
      def handle_call(:get_state, _from, state) do
        {:reply, state, state}
      end
    end
    """
  },
  main_module: MyServer,
  resource_limits: %{max_processes: 10, max_memory: 10_000_000}
}

{:ok, experiment} = OtpSupervisor.Core.Control.create_experiment(config)
```

### 2. Running Experiment Steps

```elixir
# Send a message to the GenServer
step_config = %{
  type: :message_send,
  target: "MyServer",
  message: {:call, :get_state}
}

{:ok, result} = OtpSupervisor.Core.Control.run_experiment_step(
  experiment.sandbox_id, 
  step_config
)
```

### 3. Monitoring and Analytics

```elixir
# Get current experiment state
{:ok, state} = OtpSupervisor.Core.Control.get_experiment_state(experiment.sandbox_id)

# Create a snapshot
{:ok, snapshot} = OtpSupervisor.Core.Control.snapshot_experiment(
  experiment.sandbox_id, 
  "after_first_message"
)

# View analytics
{:ok, metrics} = OtpSupervisor.Core.Analytics.get_metrics(experiment.sandbox_id)
```

## Advanced Features

### 1. Hot Code Reloading

```elixir
def reload_module_in_sandbox(sandbox_id, module_name, new_source) do
  with {:ok, compiled} <- compile_module(new_source),
       {:ok, sandbox} <- SandboxManager.get_sandbox(sandbox_id) do
    
    # Purge old version
    :code.purge(module_name)
    
    # Load new version
    {:module, ^module_name} = :code.load_binary(module_name, ~c"sandbox", compiled)
    
    # Notify processes of code change
    notify_code_change(sandbox.supervisor_pid, module_name)
    
    {:ok, :reloaded}
  end
end
```

### 2. State Persistence

```elixir
defmodule OtpSupervisor.Core.StatePersistence do
  def create_snapshot(sandbox) do
    processes = get_all_processes(sandbox.supervisor_pid)
    
    snapshot = %{
      processes: Enum.map(processes, &capture_process_state/1),
      ets_tables: capture_ets_tables(sandbox.supervisor_pid),
      supervision_tree: capture_supervision_tree(sandbox.supervisor_pid),
      timestamp: DateTime.utc_now()
    }
    
    {:ok, snapshot}
  end

  def restore_snapshot(sandbox, snapshot) do
    # Stop all current processes
    stop_all_processes(sandbox.supervisor_pid)
    
    # Restore supervision tree
    restore_supervision_tree(sandbox.supervisor_pid, snapshot.supervision_tree)
    
    # Restore process states
    Enum.each(snapshot.processes, &restore_process_state/1)
    
    # Restore ETS tables
    restore_ets_tables(snapshot.ets_tables)
    
    {:ok, :restored}
  end
end
```

### 3. Distributed Sandbox Support

```elixir
defmodule OtpSupervisor.Core.DistributedSandbox do
  def create_distributed_sandbox(nodes, config) do
    # Create sandbox on each node
    sandbox_results = 
      nodes
      |> Enum.map(fn node ->
        :rpc.call(node, SandboxManager, :create_sandbox, [config])
      end)
    
    # Link sandboxes together
    link_distributed_sandboxes(sandbox_results)
    
    {:ok, %{distributed_sandbox_id: generate_id(), nodes: nodes}}
  end

  defp link_distributed_sandboxes(sandboxes) do
    # Set up inter-node communication
    # Configure distributed supervision
    # Establish monitoring between nodes
  end
end
```

## Integration with Cinema Debugger

The SANDBOX V2 system integrates seamlessly with the Cinema Debugger by providing:

1. **Instrumentation Hooks**: Automatic event capture for visualization
2. **State Inspection**: Real-time process and supervisor state access  
3. **Execution Control**: Pause, step, and replay capabilities
4. **Isolation**: Safe environment for debugging without affecting production

```elixir
# Enable Cinema Debugger integration
config = %{
  # ... standard sandbox config ...
  cinema_debugger: %{
    enabled: true,
    capture_events: [:process_spawn, :message_send, :state_change],
    visualization_endpoint: "ws://localhost:4000/cinema"
  }
}

{:ok, experiment} = OtpSupervisor.Core.Control.create_experiment(config)
```

## Best Practices

### 1. Resource Management
- Always set appropriate resource limits
- Monitor memory usage regularly
- Use timeouts for long-running experiments

### 2. Code Safety
- Validate all dynamic code before compilation
- Use separate compilation environments
- Implement code review for sandbox modules

### 3. Monitoring
- Enable comprehensive analytics
- Set up alerts for resource violations
- Regular snapshot creation for important states

### 4. Performance
- Use ETS for fast sandbox lookup
- Implement efficient state serialization
- Batch analytics collection

## Conclusion

SANDBOX V2 provides a robust foundation for safe OTP experimentation. Its application-based isolation, comprehensive monitoring, and integration capabilities make it ideal for educational tools, debugging systems, and research platforms. The architecture supports both simple scripted experiments and complex distributed scenarios while maintaining safety and observability throughout.