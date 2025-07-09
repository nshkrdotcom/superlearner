# Distributed Sandbox Architecture Plan

## Executive Summary

This document outlines the architectural design for implementing multi-node distributed sandboxed environments in the OTP Supervisor Educational Platform using Horde for distributed process management. The design maintains the system's educational focus while adding robust multi-node capabilities.

## Current System Analysis

### Architecture Overview
The superlearner project implements a sophisticated OTP educational platform with three major innovations:

1. **Arsenal System** - Metaprogrammed REST API framework with dynamic routing
2. **Sandbox Manager** - Isolated OTP application environments  
3. **Multi-layered Web UI** - Four LiveView pages with terminal-style components

### Key Components
- **Control Module**: Central API for process management (`lib/otp_supervisor/core/control.ex`)
- **Sandbox Manager**: Dynamic application lifecycle management (`lib/otp_supervisor/core/sandbox_manager.ex`)
- **Arsenal Operations**: Protocol-driven HTTP endpoints (`lib/otp_supervisor/core/arsenal/`)
- **LiveView Components**: Modular UI widgets (`lib/otp_supervisor_web/live/`)

## Distributed Architecture Design

### Phase 1: Foundation (Weeks 1-2)
**Goal**: Establish distributed cluster foundation with Horde integration

#### 1.1 Cluster Management
```elixir
# New file: lib/otp_supervisor/distributed/cluster_manager.ex
defmodule OTPSupervisor.Distributed.ClusterManager do
  use GenServer
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(_opts) do
    children = [
      {Horde.Registry, [name: DistributedSandboxRegistry, keys: :unique]},
      {Horde.DynamicSupervisor, [
        name: DistributedSandboxSupervisor,
        strategy: :one_for_one,
        distribution_strategy: Horde.UniformDistribution
      ]}
    ]
    
    {:ok, %{cluster_members: [], node_health: %{}}}
  end
  
  def get_cluster_status do
    GenServer.call(__MODULE__, :get_cluster_status)
  end
  
  def handle_call(:get_cluster_status, _from, state) do
    cluster_nodes = [Node.self() | Node.list()]
    
    cluster_status = %{
      nodes: cluster_nodes,
      total_nodes: length(cluster_nodes),
      healthy_nodes: count_healthy_nodes(cluster_nodes),
      cluster_health: calculate_cluster_health(cluster_nodes)
    }
    
    {:reply, cluster_status, state}
  end
end
```

#### 1.2 LibCluster Configuration (WSL Port-Based Development)
```elixir
# config/config.exs
config :libcluster,
  topologies: [
    sandbox_cluster: [
      strategy: Cluster.Strategy.Epmd,
      config: [
        hosts: [:"superlearner@localhost", :"superlearner2@localhost"],
        polling_interval: 3_000,
        connect: :all_visible,
        hidden: false
      ]
    ]
  ]

# Node 1 configuration (config/dev.exs)
config :superlearner, 
  node_name: :"superlearner@localhost",
  port: 4000

config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [port: 4000],
  debug_errors: true,
  code_reloader: true,
  check_origin: false

# Node 2 configuration (config/dev2.exs - create this file)
import_config "dev.exs"

config :superlearner, 
  node_name: :"superlearner2@localhost",
  port: 4001

config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [port: 4001],
  debug_errors: true,
  code_reloader: true,
  check_origin: false
```

### Phase 2: Distributed Sandbox Management (Weeks 3-4)
**Goal**: Implement distributed sandbox lifecycle with intelligent node placement

#### 2.1 Enhanced Sandbox Manager
```elixir
# Enhanced: lib/otp_supervisor/distributed/sandbox_manager.ex
defmodule OTPSupervisor.Distributed.SandboxManager do
  @behaviour OTPSupervisor.Core.SandboxManager
  
  def create_sandbox(sandbox_id, module_or_app, opts \\ []) do
    node_preference = Keyword.get(opts, :node, :auto)
    resource_requirements = Keyword.get(opts, :resources, %{})
    
    case select_optimal_node(node_preference, resource_requirements) do
      {:ok, target_node} ->
        sandbox_spec = build_distributed_sandbox_spec(sandbox_id, module_or_app, opts)
        
        case Horde.DynamicSupervisor.start_child(
          DistributedSandboxSupervisor,
          {DistributedSandboxInstance, [sandbox_spec, target_node]}
        ) do
          {:ok, pid} ->
            register_distributed_sandbox(sandbox_id, pid, target_node)
            notify_cluster_sandbox_created(sandbox_id, target_node)
            {:ok, %{sandbox_id: sandbox_id, pid: pid, node: target_node}}
          
          {:error, reason} ->
            {:error, {:sandbox_creation_failed, reason}}
        end
      
      {:error, reason} ->
        {:error, {:node_selection_failed, reason}}
    end
  end
  
  def destroy_sandbox(sandbox_id) do
    case Horde.Registry.lookup(DistributedSandboxRegistry, sandbox_id) do
      [{pid, metadata}] ->
        target_node = metadata.node
        
        case :rpc.call(target_node, GenServer, :stop, [pid, :shutdown]) do
          {:badrpc, reason} ->
            {:error, {:rpc_failed, reason}}
          
          :ok ->
            Horde.Registry.unregister(DistributedSandboxRegistry, sandbox_id)
            notify_cluster_sandbox_destroyed(sandbox_id, target_node)
            :ok
        end
      
      [] ->
        {:error, :sandbox_not_found}
    end
  end
  
  def list_sandboxes do
    case Horde.Registry.select(DistributedSandboxRegistry, [{{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}]) do
      sandboxes ->
        distributed_sandboxes = Enum.map(sandboxes, fn {sandbox_id, pid, metadata} ->
          %{
            id: sandbox_id,
            pid: pid,
            node: metadata.node,
            created_at: metadata.created_at,
            status: get_sandbox_status(pid, metadata.node),
            resource_usage: get_sandbox_resources(pid, metadata.node)
          }
        end)
        
        {:ok, distributed_sandboxes}
      
      error ->
        {:error, error}
    end
  end
  
  defp select_optimal_node(:auto, resource_requirements) do
    cluster_nodes = [Node.self() | Node.list()]
    
    node_scores = Task.async_stream(cluster_nodes, fn node ->
      case :rpc.call(node, __MODULE__, :get_node_capacity, []) do
        {:badrpc, _} -> {node, 0}
        capacity -> {node, calculate_node_score(capacity, resource_requirements)}
      end
    end, timeout: 2_000)
    
    optimal_node = 
      node_scores
      |> Enum.reduce({nil, 0}, fn {:ok, {node, score}}, {best_node, best_score} ->
        if score > best_score, do: {node, score}, else: {best_node, best_score}
      end)
      |> elem(0)
    
    case optimal_node do
      nil -> {:error, :no_available_nodes}
      node -> {:ok, node}
    end
  end
  
  defp select_optimal_node(specific_node, _) when is_atom(specific_node) do
    if specific_node in ([Node.self() | Node.list()]) do
      {:ok, specific_node}
    else
      {:error, :node_not_available}
    end
  end
end
```

#### 2.2 Distributed Sandbox Instance
```elixir
# New file: lib/otp_supervisor/distributed/sandbox_instance.ex
defmodule OTPSupervisor.Distributed.SandboxInstance do
  use GenServer
  
  def start_link([sandbox_spec, target_node]) do
    GenServer.start_link(__MODULE__, {sandbox_spec, target_node})
  end
  
  def init({sandbox_spec, target_node}) do
    case Node.self() do
      ^target_node ->
        # We're on the target node, start the sandbox
        case start_local_sandbox(sandbox_spec) do
          {:ok, app_pid} ->
            monitor_ref = Process.monitor(app_pid)
            
            state = %{
              sandbox_spec: sandbox_spec,
              app_pid: app_pid,
              node: target_node,
              monitor_ref: monitor_ref,
              status: :running,
              created_at: DateTime.utc_now()
            }
            
            {:ok, state}
          
          {:error, reason} ->
            {:stop, {:sandbox_start_failed, reason}}
        end
      
      _ ->
        # We're on the wrong node, this shouldn't happen with Horde
        {:stop, {:wrong_node, Node.self(), target_node}}
    end
  end
  
  def handle_info({:DOWN, ref, :process, _pid, reason}, %{monitor_ref: ref} = state) do
    # Local sandbox process died, decide whether to restart locally or migrate
    case should_restart_locally(reason, state) do
      true ->
        case start_local_sandbox(state.sandbox_spec) do
          {:ok, new_app_pid} ->
            new_monitor_ref = Process.monitor(new_app_pid)
            new_state = %{state | app_pid: new_app_pid, monitor_ref: new_monitor_ref}
            {:noreply, new_state}
          
          {:error, restart_reason} ->
            {:stop, {:restart_failed, restart_reason}, state}
        end
      
      false ->
        # Request migration to another node
        {:stop, {:migration_required, reason}, state}
    end
  end
  
  defp start_local_sandbox(sandbox_spec) do
    # Use existing sandbox creation logic from current SandboxManager
    OTPSupervisor.Core.SandboxManager.create_sandbox(
      sandbox_spec.id,
      sandbox_spec.module_or_app,
      sandbox_spec.opts
    )
  end
end
```

### Phase 3: Arsenal Distribution (Weeks 5-6)
**Goal**: Extend Arsenal operations for distributed environments

#### 3.1 Distributed Arsenal Operations
```elixir
# New file: lib/otp_supervisor/distributed/arsenal/operations/list_distributed_sandboxes.ex
defmodule OTPSupervisor.Distributed.Arsenal.Operations.ListDistributedSandboxes do
  use OTPSupervisor.Core.Arsenal.Operation
  
  def rest_config do
    %{
      method: :get,
      path: "/api/v1/distributed/sandboxes",
      summary: "List sandboxes across all cluster nodes",
      parameters: [
        %{
          name: :node,
          type: :string,
          required: false,
          description: "Filter by specific node",
          location: :query
        },
        %{
          name: :status,
          type: :string,
          required: false,
          description: "Filter by sandbox status",
          location: :query
        }
      ],
      responses: %{
        200 => %{
          description: "Distributed sandbox list",
          schema: %{
            type: :object,
            properties: %{
              cluster_nodes: %{type: :array},
              total_sandboxes: %{type: :integer},
              sandboxes: %{type: :array}
            }
          }
        }
      }
    }
  end
  
  def validate_params(params) do
    validated = %{}
    
    validated = case Map.get(params, "node") do
      nil -> validated
      node_string -> 
        case String.to_existing_atom(node_string) do
          node_atom -> Map.put(validated, "node", node_atom)
        rescue
          ArgumentError -> {:error, {:invalid_parameter, :node, "invalid node name"}}
        end
    end
    
    validated = case Map.get(params, "status") do
      nil -> validated
      status when status in ["running", "stopped", "error"] -> 
        Map.put(validated, "status", String.to_atom(status))
      _ -> 
        {:error, {:invalid_parameter, :status, "must be running, stopped, or error"}}
    end
    
    case validated do
      %{} -> {:ok, validated}
      error -> error
    end
  end
  
  def execute(params) do
    cluster_nodes = [Node.self() | Node.list()]
    node_filter = Map.get(params, "node")
    status_filter = Map.get(params, "status")
    
    target_nodes = case node_filter do
      nil -> cluster_nodes
      specific_node -> [specific_node]
    end
    
    node_results = Task.async_stream(target_nodes, fn node ->
      case :rpc.call(node, OTPSupervisor.Distributed.SandboxManager, :list_local_sandboxes, []) do
        {:badrpc, reason} -> {node, {:error, reason}}
        {:ok, sandboxes} -> {node, {:ok, sandboxes}}
        error -> {node, {:error, error}}
      end
    end, timeout: 5_000)
    
    {successful_nodes, failed_nodes, all_sandboxes} = 
      Enum.reduce(node_results, {[], [], []}, fn 
        {:ok, {node, {:ok, sandboxes}}}, {success, failed, all_sandboxes} ->
          node_sandboxes = Enum.map(sandboxes, &Map.put(&1, :node, node))
          {[node | success], failed, all_sandboxes ++ node_sandboxes}
        
        {:ok, {node, {:error, reason}}}, {success, failed, all_sandboxes} ->
          {success, [{node, reason} | failed], all_sandboxes}
      end)
    
    filtered_sandboxes = case status_filter do
      nil -> all_sandboxes
      status -> Enum.filter(all_sandboxes, &(&1.status == status))
    end
    
    result = %{
      cluster_info: %{
        total_nodes: length(cluster_nodes),
        queried_nodes: length(target_nodes),
        successful_nodes: length(successful_nodes),
        failed_nodes: length(failed_nodes)
      },
      sandboxes: filtered_sandboxes,
      total_sandboxes: length(filtered_sandboxes),
      node_failures: failed_nodes
    }
    
    {:ok, result}
  end
end
```

#### 3.2 Cross-Node Process Management
```elixir
# New file: lib/otp_supervisor/distributed/arsenal/operations/cross_node_process_management.ex
defmodule OTPSupervisor.Distributed.Arsenal.Operations.CrossNodeProcessManagement do
  use OTPSupervisor.Core.Arsenal.Operation
  
  def rest_config do
    %{
      method: :delete,
      path: "/api/v1/distributed/processes/:node/:pid",
      summary: "Terminate process on specific node",
      parameters: [
        %{
          name: :node,
          type: :string,
          required: true,
          description: "Target node name",
          location: :path
        },
        %{
          name: :pid,
          type: :string,
          required: true,
          description: "Process ID",
          location: :path
        },
        %{
          name: :reason,
          type: :string,
          required: false,
          description: "Termination reason",
          location: :query
        }
      ]
    }
  end
  
  def validate_params(%{"node" => node_string, "pid" => pid_string} = params) do
    with {:ok, node_atom} <- parse_node(node_string),
         {:ok, pid} <- parse_pid(pid_string) do
      
      reason = case Map.get(params, "reason") do
        nil -> :shutdown
        reason_string -> String.to_atom(reason_string)
      end
      
      {:ok, %{
        "node" => node_atom,
        "pid" => pid,
        "reason" => reason
      }}
    else
      {:error, error} -> {:error, error}
    end
  end
  
  def execute(%{"node" => target_node, "pid" => pid, "reason" => reason}) do
    case target_node in [Node.self() | Node.list()] do
      true ->
        case :rpc.call(target_node, Process, :exit, [pid, reason]) do
          {:badrpc, rpc_reason} ->
            {:error, {:rpc_failed, rpc_reason}}
          
          true ->
            {:ok, %{
              terminated: true,
              pid: inspect(pid),
              node: target_node,
              reason: reason,
              timestamp: DateTime.utc_now()
            }}
          
          false ->
            {:error, :process_not_found}
        end
      
      false ->
        {:error, :node_not_available}
    end
  end
  
  defp parse_node(node_string) do
    try do
      node_atom = String.to_existing_atom(node_string)
      {:ok, node_atom}
    rescue
      ArgumentError -> {:error, {:invalid_parameter, :node, "invalid node name"}}
    end
  end
  
  defp parse_pid(pid_string) do
    case OTPSupervisor.Core.Control.to_pid(pid_string) do
      {:ok, pid} -> {:ok, pid}
      {:error, reason} -> {:error, {:invalid_parameter, :pid, reason}}
    end
  end
end
```

### Phase 4: Real-time Distributed Monitoring (Weeks 7-8)
**Goal**: Implement cluster-wide monitoring and visualization

#### 4.1 Cluster Telemetry System
```elixir
# New file: lib/otp_supervisor/distributed/cluster_telemetry.ex
defmodule OTPSupervisor.Distributed.ClusterTelemetry do
  use GenServer
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(_opts) do
    Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "cluster_metrics")
    :timer.send_interval(2_000, :collect_cluster_metrics)
    :timer.send_interval(30_000, :health_check)
    
    {:ok, %{
      cluster_metrics: %{},
      node_health: %{},
      metric_history: []
    }}
  end
  
  def handle_info(:collect_cluster_metrics, state) do
    cluster_nodes = [Node.self() | Node.list()]
    
    metrics_tasks = Task.async_stream(cluster_nodes, fn node ->
      case :rpc.call(node, __MODULE__, :get_local_metrics, [], 2_000) do
        {:badrpc, reason} -> 
          {node, %{status: :unreachable, error: reason, timestamp: DateTime.utc_now()}}
        
        metrics -> 
          {node, Map.put(metrics, :timestamp, DateTime.utc_now())}
      end
    end, timeout: 3_000)
    
    updated_metrics = 
      metrics_tasks
      |> Enum.reduce(%{}, fn {:ok, {node, metrics}}, acc ->
        Map.put(acc, node, metrics)
      end)
    
    # Add metrics to history (keep last 100 entries)
    new_history = [updated_metrics | state.metric_history] |> Enum.take(100)
    
    # Broadcast cluster-wide metrics
    Phoenix.PubSub.broadcast(
      OtpSupervisor.PubSub,
      "cluster_metrics",
      {:cluster_metrics_update, updated_metrics}
    )
    
    {:noreply, %{
      state | 
      cluster_metrics: updated_metrics,
      metric_history: new_history
    }}
  end
  
  def handle_info(:health_check, state) do
    cluster_health = analyze_cluster_health(state.cluster_metrics)
    
    Phoenix.PubSub.broadcast(
      OtpSupervisor.PubSub,
      "cluster_health",
      {:cluster_health_update, cluster_health}
    )
    
    {:noreply, %{state | node_health: cluster_health}}
  end
  
  def get_local_metrics do
    %{
      node: Node.self(),
      status: :healthy,
      system_info: %{
        process_count: length(Process.list()),
        memory_usage: :erlang.memory(),
        cpu_usage: get_cpu_usage(),
        uptime: :erlang.system_info(:uptime)
      },
      sandbox_info: %{
        total_sandboxes: count_local_sandboxes(),
        active_sandboxes: count_active_sandboxes(),
        memory_per_sandbox: get_average_sandbox_memory()
      },
      network_info: %{
        connected_nodes: length(Node.list()),
        cluster_size: length([Node.self() | Node.list()])
      }
    }
  end
  
  defp analyze_cluster_health(cluster_metrics) do
    Enum.reduce(cluster_metrics, %{}, fn {node, metrics}, acc ->
      health_status = case metrics.status do
        :unreachable -> :critical
        _ -> 
          cond do
            metrics.system_info.process_count > 100_000 -> :warning
            metrics.system_info.memory_usage.total > 2_000_000_000 -> :warning
            true -> :healthy
          end
      end
      
      Map.put(acc, node, health_status)
    end)
  end
end
```

#### 4.2 Distributed Dashboard LiveView
```elixir
# New file: lib/otp_supervisor_web/live/distributed_dashboard_live.ex
defmodule OtpSupervisorWeb.Live.DistributedDashboardLive do
  use Phoenix.LiveView
  
  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OtpSupervisorWeb.Components.Layout.TerminalPanelLayout
  alias OtpSupervisorWeb.Components.Widgets.SystemMetricsWidget
  alias OtpSupervisorWeb.Components.Widgets.ProcessListWidget
  
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "cluster_metrics")
      Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "cluster_health")
      :timer.send_interval(5_000, :refresh_cluster_data)
    end
    
    {:ok,
     socket
     |> assign(:page_title, "Distributed Cluster Dashboard")
     |> assign(:current_page, "distributed")
     |> assign(:cluster_nodes, [])
     |> assign(:cluster_metrics, %{})
     |> assign(:cluster_health, %{})
     |> assign(:distributed_sandboxes, [])
     |> assign(:selected_node, nil)
     |> load_initial_cluster_data()}
  end
  
  def handle_info({:cluster_metrics_update, metrics}, socket) do
    {:noreply, 
     socket
     |> assign(:cluster_metrics, metrics)
     |> assign(:cluster_nodes, Map.keys(metrics))
     |> maybe_refresh_sandboxes()}
  end
  
  def handle_info({:cluster_health_update, health}, socket) do
    {:noreply, assign(socket, :cluster_health, health)}
  end
  
  def handle_info(:refresh_cluster_data, socket) do
    {:noreply, refresh_distributed_sandboxes(socket)}
  end
  
  def handle_event("select_node", %{"node" => node_name}, socket) do
    node_atom = String.to_existing_atom(node_name)
    
    {:noreply, 
     socket
     |> assign(:selected_node, node_atom)
     |> load_node_details(node_atom)}
  end
  
  def handle_event("migrate_sandbox", %{"sandbox_id" => sandbox_id, "target_node" => target_node}, socket) do
    case OTPSupervisor.Distributed.SandboxManager.migrate_sandbox(sandbox_id, String.to_atom(target_node)) do
      {:ok, _} ->
        {:noreply, put_flash(socket, :info, "Sandbox #{sandbox_id} migration initiated")}
      
      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Migration failed: #{inspect(reason)}")}
    end
  end
  
  def render(assigns) do
    ~H"""
    <div class="fixed inset-0 bg-gray-900 text-green-400 flex flex-col">
      <!-- Status Bar -->
      <.live_component
        module={TerminalStatusBar}
        id="distributed-status-bar"
        title="Distributed Cluster Dashboard"
        metrics={cluster_status_metrics(assigns)}
        navigation_links={TerminalNavigationLinks.page_navigation_links("distributed", %{})}
      />
      
      <!-- Main Layout -->
      <.live_component
        module={TerminalPanelLayout}
        id="distributed-panel-layout"
        layout_type={:grid}
        panels={distributed_dashboard_panels(assigns)}
        gap="gap-4"
        padding="p-4"
      />
    </div>
    """
  end
  
  defp distributed_dashboard_panels(assigns) do
    [
      # Cluster Overview Panel
      %{
        title: "Cluster Overview",
        slot: render_cluster_overview(assigns),
        span: %{cols: 2, rows: 1}
      },
      
      # Node Details Panel
      %{
        title: "Node Details",
        slot: render_node_details(assigns),
        span: %{cols: 2, rows: 1}
      },
      
      # Distributed Sandboxes Panel
      %{
        title: "Distributed Sandboxes",
        slot: render_distributed_sandboxes(assigns),
        span: %{cols: 2, rows: 2}
      },
      
      # Cluster Health Panel
      %{
        title: "Cluster Health",
        slot: render_cluster_health(assigns),
        span: %{cols: 2, rows: 1}
      }
    ]
  end
  
  defp render_cluster_overview(assigns) do
    ~H"""
    <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
      <%= for {node, metrics} <- @cluster_metrics do %>
        <button
          phx-click="select_node"
          phx-value-node={to_string(node)}
          class={[
            "p-4 rounded border-2 transition-all duration-200 text-left",
            node_card_classes(node, @cluster_health, @selected_node)
          ]}
        >
          <div class="flex items-center justify-between mb-2">
            <h3 class="font-mono text-sm font-bold"><%= node %></h3>
            <div class={["w-3 h-3 rounded-full", node_status_color(Map.get(@cluster_health, node, :unknown))]}></div>
          </div>
          
          <div class="space-y-1 text-xs font-mono">
            <div>Sandboxes: <%= metrics.sandbox_info.total_sandboxes %></div>
            <div>Processes: <%= metrics.system_info.process_count %></div>
            <div>Memory: <%= format_bytes(metrics.system_info.memory_usage.total) %></div>
          </div>
        </button>
      <% end %>
    </div>
    """
  end
  
  defp render_distributed_sandboxes(assigns) do
    ~H"""
    <div class="space-y-4">
      <div class="overflow-x-auto">
        <table class="w-full text-sm font-mono">
          <thead>
            <tr class="border-b border-green-500/30">
              <th class="text-left p-2">Sandbox ID</th>
              <th class="text-left p-2">Node</th>
              <th class="text-left p-2">Status</th>
              <th class="text-left p-2">Memory</th>
              <th class="text-left p-2">Actions</th>
            </tr>
          </thead>
          <tbody>
            <%= for sandbox <- @distributed_sandboxes do %>
              <tr class="border-b border-green-500/10 hover:bg-green-500/5">
                <td class="p-2"><%= sandbox.id %></td>
                <td class="p-2"><%= sandbox.node %></td>
                <td class="p-2">
                  <span class={["px-2 py-1 rounded text-xs", sandbox_status_classes(sandbox.status)]}>
                    <%= sandbox.status %>
                  </span>
                </td>
                <td class="p-2"><%= format_bytes(sandbox.resource_usage.memory) %></td>
                <td class="p-2">
                  <button
                    phx-click="migrate_sandbox"
                    phx-value-sandbox-id={sandbox.id}
                    phx-value-target-node="auto"
                    class="px-2 py-1 bg-blue-500/20 text-blue-400 rounded text-xs hover:bg-blue-500/30"
                  >
                    Migrate
                  </button>
                </td>
              </tr>
            <% end %>
          </tbody>
        </table>
      </div>
    </div>
    """
  end
  
  defp cluster_status_metrics(assigns) do
    total_nodes = length(assigns.cluster_nodes)
    healthy_nodes = Enum.count(assigns.cluster_health, fn {_, health} -> health == :healthy end)
    total_sandboxes = length(assigns.distributed_sandboxes)
    
    [
      %{label: "Nodes", value: "#{healthy_nodes}/#{total_nodes}"},
      %{label: "Sandboxes", value: "#{total_sandboxes}"},
      %{label: "Selected", value: to_string(assigns.selected_node || "None")},
      %{label: "Status", value: overall_cluster_status(assigns.cluster_health)}
    ]
  end
end
```

## Implementation Challenges and Solutions

### 1. Network Partitions and Split-Brain Scenarios
**Challenge**: When network partitions occur, different nodes may have conflicting views of active sandboxes.

**Solution**: Implement a quorum-based consensus mechanism:
```elixir
defmodule OTPSupervisor.Distributed.QuorumManager do
  def has_quorum?(cluster_nodes) do
    total_nodes = length(cluster_nodes)
    reachable_nodes = count_reachable_nodes(cluster_nodes)
    
    reachable_nodes > (total_nodes / 2)
  end
  
  def execute_with_quorum(operation, cluster_nodes) do
    case has_quorum?(cluster_nodes) do
      true -> operation.()
      false -> {:error, :no_quorum}
    end
  end
end
```

### 2. Sandbox State Persistence
**Challenge**: Sandboxes may have state that needs to survive node failures.

**Solution**: Implement periodic state checkpointing:
```elixir
defmodule OTPSupervisor.Distributed.StateCheckpointer do
  def checkpoint_sandbox_state(sandbox_id, state) do
    # Store state in distributed storage (could be ETS, Mnesia, or external store)
    checkpoint_data = %{
      sandbox_id: sandbox_id,
      state: state,
      timestamp: DateTime.utc_now(),
      node: Node.self()
    }
    
    # Replicate to multiple nodes
    cluster_nodes = [Node.self() | Node.list()]
    replication_targets = Enum.take_random(cluster_nodes, 2)
    
    Enum.each(replication_targets, fn node ->
      :rpc.cast(node, __MODULE__, :store_checkpoint, [checkpoint_data])
    end)
  end
end
```

### 3. Load Balancing and Resource Management
**Challenge**: Distributing sandboxes optimally across nodes based on resource availability.

**Solution**: Implement intelligent node selection:
```elixir
defmodule OTPSupervisor.Distributed.LoadBalancer do
  def select_optimal_node(resource_requirements) do
    cluster_nodes = [Node.self() | Node.list()]
    
    node_capacities = Task.async_stream(cluster_nodes, fn node ->
      case get_node_capacity(node) do
        {:ok, capacity} -> {node, calculate_suitability_score(capacity, resource_requirements)}
        {:error, _} -> {node, 0}
      end
    end, timeout: 1_000)
    
    node_capacities
    |> Enum.max_by(fn {:ok, {_node, score}} -> score end)
    |> elem(1)
    |> elem(0)
  end
  
  defp calculate_suitability_score(capacity, requirements) do
    memory_score = calculate_memory_score(capacity.available_memory, requirements.memory)
    cpu_score = calculate_cpu_score(capacity.cpu_usage, requirements.cpu)
    network_score = calculate_network_score(capacity.network_load, requirements.network)
    
    # Weighted scoring
    (memory_score * 0.5) + (cpu_score * 0.3) + (network_score * 0.2)
  end
end
```

### 4. Monitoring and Alerting
**Challenge**: Comprehensive monitoring of distributed sandbox health and performance.

**Solution**: Implement distributed monitoring with centralized alerting:
```elixir
defmodule OTPSupervisor.Distributed.MonitoringSystem do
  def monitor_distributed_sandboxes do
    cluster_nodes = [Node.self() | Node.list()]
    
    monitoring_tasks = Task.async_stream(cluster_nodes, fn node ->
      case :rpc.call(node, __MODULE__, :get_local_sandbox_health, []) do
        {:badrpc, _} -> {node, :unreachable}
        health_data -> {node, health_data}
      end
    end, timeout: 5_000)
    
    health_summary = 
      monitoring_tasks
      |> Enum.reduce(%{}, fn {:ok, {node, health}}, acc ->
        Map.put(acc, node, health)
      end)
    
    # Analyze health data and trigger alerts
    analyze_and_alert(health_summary)
  end
  
  defp analyze_and_alert(health_summary) do
    alerts = []
    
    # Check for node failures
    alerts = check_node_failures(health_summary, alerts)
    
    # Check for resource exhaustion
    alerts = check_resource_exhaustion(health_summary, alerts)
    
    # Check for sandbox failures
    alerts = check_sandbox_failures(health_summary, alerts)
    
    # Send alerts
    Enum.each(alerts, &send_alert/1)
  end
end
```

## Testing Strategy

### Unit Testing
- Test individual distributed components in isolation
- Mock RPC calls for unit tests
- Test failure scenarios and edge cases

### Integration Testing
- Test multi-node scenarios with actual cluster setup
- Test network partition scenarios
- Test node failure and recovery

### Performance Testing
- Load testing with multiple sandboxes across nodes
- Network latency impact on operations
- Resource utilization under different load patterns

### Chaos Engineering
- Implement controlled failures for testing resilience
- Test network partitions and node failures
- Validate recovery mechanisms

## Migration Strategy

### Phase 1: Dual Operation
- Run both local and distributed sandbox managers
- Gradual migration of sandboxes to distributed system
- Fallback mechanisms for compatibility

### Phase 2: Feature Parity
- Ensure all existing features work in distributed mode
- Performance optimization and tuning
- Complete test coverage

### Phase 3: Full Migration
- Disable local sandbox manager
- Complete transition to distributed architecture
- Documentation and training updates

## Conclusion

This distributed sandbox architecture provides a robust foundation for multi-node OTP educational environments. The design maintains backward compatibility while adding powerful distributed capabilities. The phased implementation approach ensures minimal disruption to existing functionality while building a scalable, fault-tolerant system.

Key benefits:
- **Scalability**: Distribute sandboxes across multiple nodes
- **Fault Tolerance**: Automatic failover and recovery
- **Load Balancing**: Intelligent resource allocation
- **Monitoring**: Comprehensive cluster-wide visibility
- **Educational Value**: Real-world distributed systems experience

The architecture leverages Horde's proven distributed supervision capabilities while maintaining the educational focus and innovative features of the original system.