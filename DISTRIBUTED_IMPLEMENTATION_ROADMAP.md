# Distributed Implementation Roadmap

## Project Overview

This roadmap outlines the step-by-step implementation of distributed sandboxed environments for the OTP Supervisor Educational Platform. The implementation will transform the current single-node system into a robust multi-node distributed architecture using Horde for distributed supervision.

## Implementation Phases

### Phase 1: Foundation and Clustering (Weeks 1-2)
**Goal**: Establish basic distributed cluster capabilities

#### Week 1: Cluster Setup and Configuration

**Day 1-2: WSL Environment Setup**
- [ ] Set up two separate WSL instances for development
- [ ] Configure port-based networking (WSL instances share IP, use different ports)
- [ ] Install and configure Elixir/OTP on both nodes
- [ ] Set up shared development environment with port differentiation

**Day 3-4: LibCluster Integration**
```elixir
# Add to mix.exs
defp deps do
  [
    {:libcluster, "~> 3.3"},
    {:horde, "~> 0.8.0"},
    # ... existing deps
  ]
end
```

**Configuration Files (WSL Port-Based Setup):**
```elixir
# config/config.exs
config :libcluster,
  debug: true,
  topologies: [
    sandbox_cluster: [
      strategy: Cluster.Strategy.Epmd,
      config: [
        hosts: [
          :"superlearner@localhost",
          :"superlearner2@localhost"
        ],
        polling_interval: 5_000
      ]
    ]
  ]

# config/dev.exs (Node 1)
config :superlearner, :node_name, :"superlearner@localhost"

config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [port: 4000],
  debug_errors: true,
  code_reloader: true,
  check_origin: false

# config/dev2.exs (Node 2) - create this file
import_config "dev.exs"

config :superlearner, :node_name, :"superlearner2@localhost"

config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [port: 4001],
  debug_errors: true,
  code_reloader: true,
  check_origin: false
```

**Day 5: Basic Cluster Formation**
- [ ] Test cluster formation between WSL instances
- [ ] Verify both nodes can access each other (Node.list())
- [ ] Test basic process communication across nodes
- [ ] Validate both web interfaces accessible (localhost:4000 and localhost:4001)

**WSL-Specific Setup Commands:**
```bash
# Terminal 1 (Node 1)
iex --name superlearner@localhost --cookie secret_cluster_cookie -S mix phx.server

# Terminal 2 (Node 2) 
MIX_ENV=dev iex --name superlearner2@localhost --cookie secret_cluster_cookie --erl "-config dev2" -S mix phx.server

# Test connectivity from either node
Node.list()  # Should show the other node
Node.ping(:"superlearner@localhost")   # From node 2
Node.ping(:"superlearner2@localhost")  # From node 1
```

#### Week 2: Horde Integration

**Day 6-7: Horde Setup**
```elixir
# lib/otp_supervisor/distributed/cluster_supervisor.ex
defmodule OTPSupervisor.Distributed.ClusterSupervisor do
  use Supervisor
  
  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  @impl true
  def init(_opts) do
    children = [
      {Cluster.Supervisor, [Application.get_env(:libcluster, :topologies)]},
      {Horde.Registry, [name: OTPSupervisor.Distributed.Registry, keys: :unique]},
      {Horde.DynamicSupervisor, [
        name: OTPSupervisor.Distributed.Supervisor,
        strategy: :one_for_one,
        distribution_strategy: Horde.UniformDistribution
      ]},
      OTPSupervisor.Distributed.ClusterManager
    ]
    
    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

**Day 8-9: Cluster Manager Implementation**
```elixir
# lib/otp_supervisor/distributed/cluster_manager.ex
defmodule OTPSupervisor.Distributed.ClusterManager do
  use GenServer
  require Logger
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def get_cluster_status do
    GenServer.call(__MODULE__, :get_cluster_status)
  end
  
  @impl true
  def init(_opts) do
    :net_kernel.monitor_nodes(true)
    Logger.info("Cluster Manager started on #{Node.self()}")
    {:ok, %{nodes: [Node.self()], status: :initializing}}
  end
  
  @impl true
  def handle_call(:get_cluster_status, _from, state) do
    cluster_info = %{
      current_node: Node.self(),
      connected_nodes: Node.list(),
      cluster_size: length([Node.self() | Node.list()]),
      horde_members: get_horde_members(),
      status: state.status
    }
    
    {:reply, cluster_info, state}
  end
  
  @impl true
  def handle_info({:nodeup, node}, state) do
    Logger.info("Node joined cluster: #{node}")
    
    # Add node to Horde cluster
    members = [Node.self() | Node.list()]
    set_horde_members(members)
    
    new_nodes = [node | state.nodes] |> Enum.uniq()
    {:noreply, %{state | nodes: new_nodes, status: :connected}}
  end
  
  @impl true
  def handle_info({:nodedown, node}, state) do
    Logger.warning("Node left cluster: #{node}")
    
    # Update Horde cluster
    members = [Node.self() | Node.list()]
    set_horde_members(members)
    
    new_nodes = List.delete(state.nodes, node)
    {:noreply, %{state | nodes: new_nodes}}
  end
  
  defp get_horde_members do
    case Horde.Cluster.members(OTPSupervisor.Distributed.Supervisor) do
      {:ok, members} -> members
      _ -> []
    end
  end
  
  defp set_horde_members(members) do
    Horde.Cluster.set_members(OTPSupervisor.Distributed.Supervisor, members)
    Horde.Cluster.set_members(OTPSupervisor.Distributed.Registry, members)
  end
end
```

**Day 10: Integration and Testing**
- [ ] Integrate cluster components into main supervision tree
- [ ] Test cluster formation and node failure scenarios
- [ ] Implement basic cluster health monitoring

**Deliverables:**
- Basic cluster formation working
- Horde registry and supervisor operational
- Node join/leave handling
- Cluster status monitoring

### Phase 2: Distributed Sandbox Management (Weeks 3-4)
**Goal**: Implement distributed sandbox lifecycle management

#### Week 3: Sandbox Distribution Logic

**Day 11-12: Node Selection Algorithm**
```elixir
# lib/otp_supervisor/distributed/node_selector.ex
defmodule OTPSupervisor.Distributed.NodeSelector do
  @moduledoc """
  Intelligent node selection for optimal sandbox placement
  """
  
  def select_optimal_node(requirements \\ %{}) do
    case get_available_nodes() do
      [] -> {:error, :no_available_nodes}
      nodes -> {:ok, select_best_node(nodes, requirements)}
    end
  end
  
  defp get_available_nodes do
    [Node.self() | Node.list()]
    |> Enum.filter(&node_available?/1)
  end
  
  defp node_available?(node) do
    case :rpc.call(node, :erlang, :system_info, [:process_count], 1000) do
      {:badrpc, _} -> false
      _count -> true
    end
  end
  
  defp select_best_node(nodes, requirements) do
    nodes
    |> Enum.map(&{&1, calculate_node_score(&1, requirements)})
    |> Enum.max_by(fn {_node, score} -> score end)
    |> elem(0)
  end
  
  defp calculate_node_score(node, requirements) do
    case get_node_metrics(node) do
      {:ok, metrics} ->
        cpu_score = 100 - metrics.cpu_usage
        memory_score = 100 - metrics.memory_usage_percent
        sandbox_score = 100 - min(metrics.sandbox_count, 100)
        
        # Weighted scoring
        (cpu_score * 0.4) + (memory_score * 0.4) + (sandbox_score * 0.2)
      
      {:error, _} -> 0
    end
  end
  
  defp get_node_metrics(node) do
    case :rpc.call(node, __MODULE__, :collect_local_metrics, [], 2000) do
      {:badrpc, reason} -> {:error, reason}
      metrics -> {:ok, metrics}
    end
  end
  
  def collect_local_metrics do
    memory_info = :erlang.memory()
    process_count = length(Process.list())
    
    %{
      cpu_usage: get_cpu_usage(),
      memory_usage_percent: calculate_memory_percentage(memory_info),
      sandbox_count: count_local_sandboxes(),
      process_count: process_count,
      node: Node.self(),
      timestamp: DateTime.utc_now()
    }
  end
  
  defp get_cpu_usage do
    # Simplified CPU usage calculation
    case :cpu_sup.util() do
      {:error, _} -> 0
      usage -> usage
    end
  end
  
  defp calculate_memory_percentage(memory_info) do
    total = Keyword.get(memory_info, :total, 1)
    used = Keyword.get(memory_info, :processes, 0)
    
    (used / total) * 100
  end
  
  defp count_local_sandboxes do
    case :ets.info(:sandboxes) do
      :undefined -> 0
      info -> Keyword.get(info, :size, 0)
    end
  end
end
```

**Day 13-14: Distributed Sandbox Manager**
```elixir
# lib/otp_supervisor/distributed/sandbox_manager.ex
defmodule OTPSupervisor.Distributed.SandboxManager do
  @moduledoc """
  Distributed sandbox lifecycle management
  """
  
  use GenServer
  require Logger
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def create_sandbox(sandbox_id, module_or_app, opts \\ []) do
    GenServer.call(__MODULE__, {:create_sandbox, sandbox_id, module_or_app, opts})
  end
  
  def destroy_sandbox(sandbox_id) do
    GenServer.call(__MODULE__, {:destroy_sandbox, sandbox_id})
  end
  
  def list_sandboxes do
    GenServer.call(__MODULE__, :list_sandboxes)
  end
  
  def get_sandbox_info(sandbox_id) do
    GenServer.call(__MODULE__, {:get_sandbox_info, sandbox_id})
  end
  
  @impl true
  def init(_opts) do
    Logger.info("Distributed Sandbox Manager started on #{Node.self()}")
    {:ok, %{local_sandboxes: %{}}}
  end
  
  @impl true
  def handle_call({:create_sandbox, sandbox_id, module_or_app, opts}, _from, state) do
    case OTPSupervisor.Distributed.NodeSelector.select_optimal_node() do
      {:ok, target_node} ->
        sandbox_spec = %{
          id: sandbox_id,
          module_or_app: module_or_app,
          opts: opts,
          target_node: target_node,
          created_at: DateTime.utc_now()
        }
        
        case start_distributed_sandbox(sandbox_spec) do
          {:ok, sandbox_info} ->
            register_sandbox(sandbox_id, sandbox_info)
            {:reply, {:ok, sandbox_info}, state}
          
          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
  
  @impl true
  def handle_call({:destroy_sandbox, sandbox_id}, _from, state) do
    case lookup_sandbox(sandbox_id) do
      {:ok, sandbox_info} ->
        case stop_distributed_sandbox(sandbox_info) do
          :ok ->
            unregister_sandbox(sandbox_id)
            {:reply, :ok, state}
          
          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
  
  @impl true
  def handle_call(:list_sandboxes, _from, state) do
    case get_all_distributed_sandboxes() do
      {:ok, sandboxes} ->
        {:reply, sandboxes, state}
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
  
  @impl true
  def handle_call({:get_sandbox_info, sandbox_id}, _from, state) do
    case lookup_sandbox(sandbox_id) do
      {:ok, sandbox_info} ->
        {:reply, {:ok, sandbox_info}, state}
      
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
  
  defp start_distributed_sandbox(sandbox_spec) do
    child_spec = {
      OTPSupervisor.Distributed.SandboxInstance,
      [sandbox_spec]
    }
    
    case Horde.DynamicSupervisor.start_child(
      OTPSupervisor.Distributed.Supervisor,
      child_spec
    ) do
      {:ok, pid} ->
        sandbox_info = %{
          id: sandbox_spec.id,
          pid: pid,
          node: sandbox_spec.target_node,
          module_or_app: sandbox_spec.module_or_app,
          opts: sandbox_spec.opts,
          created_at: sandbox_spec.created_at,
          status: :running
        }
        
        {:ok, sandbox_info}
      
      {:error, reason} ->
        {:error, reason}
    end
  end
  
  defp stop_distributed_sandbox(sandbox_info) do
    case Horde.DynamicSupervisor.terminate_child(
      OTPSupervisor.Distributed.Supervisor,
      sandbox_info.pid
    ) do
      :ok -> :ok
      {:error, reason} -> {:error, reason}
    end
  end
  
  defp register_sandbox(sandbox_id, sandbox_info) do
    case Horde.Registry.register(
      OTPSupervisor.Distributed.Registry,
      sandbox_id,
      sandbox_info
    ) do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end
  
  defp unregister_sandbox(sandbox_id) do
    Horde.Registry.unregister(OTPSupervisor.Distributed.Registry, sandbox_id)
  end
  
  defp lookup_sandbox(sandbox_id) do
    case Horde.Registry.lookup(OTPSupervisor.Distributed.Registry, sandbox_id) do
      [{_pid, sandbox_info}] -> {:ok, sandbox_info}
      [] -> {:error, :not_found}
    end
  end
  
  defp get_all_distributed_sandboxes do
    case Horde.Registry.select(
      OTPSupervisor.Distributed.Registry,
      [{{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$3"}}]}]
    ) do
      sandboxes when is_list(sandboxes) ->
        formatted_sandboxes = Enum.map(sandboxes, fn {id, info} ->
          Map.put(info, :id, id)
        end)
        
        {:ok, formatted_sandboxes}
      
      {:error, reason} ->
        {:error, reason}
    end
  end
end
```

**Day 15: Sandbox Instance Implementation**
```elixir
# lib/otp_supervisor/distributed/sandbox_instance.ex
defmodule OTPSupervisor.Distributed.SandboxInstance do
  @moduledoc """
  Individual sandbox instance running on a specific node
  """
  
  use GenServer
  require Logger
  
  def start_link([sandbox_spec]) do
    GenServer.start_link(__MODULE__, sandbox_spec)
  end
  
  @impl true
  def init(sandbox_spec) do
    Logger.info("Starting sandbox #{sandbox_spec.id} on #{Node.self()}")
    
    case start_local_sandbox(sandbox_spec) do
      {:ok, local_sandbox_info} ->
        state = %{
          sandbox_spec: sandbox_spec,
          local_sandbox_info: local_sandbox_info,
          health_check_timer: schedule_health_check(),
          status: :running
        }
        
        {:ok, state}
      
      {:error, reason} ->
        Logger.error("Failed to start sandbox #{sandbox_spec.id}: #{inspect(reason)}")
        {:stop, reason}
    end
  end
  
  @impl true
  def handle_info(:health_check, state) do
    case check_sandbox_health(state.local_sandbox_info) do
      :healthy ->
        {:noreply, %{state | health_check_timer: schedule_health_check()}}
      
      :unhealthy ->
        Logger.warning("Sandbox #{state.sandbox_spec.id} is unhealthy, attempting restart")
        
        case restart_local_sandbox(state) do
          {:ok, new_sandbox_info} ->
            {:noreply, %{
              state |
              local_sandbox_info: new_sandbox_info,
              health_check_timer: schedule_health_check()
            }}
          
          {:error, reason} ->
            Logger.error("Failed to restart sandbox: #{inspect(reason)}")
            {:stop, reason, state}
        end
    end
  end
  
  @impl true
  def terminate(reason, state) do
    Logger.info("Terminating sandbox #{state.sandbox_spec.id}: #{inspect(reason)}")
    
    # Clean up local sandbox
    cleanup_local_sandbox(state.local_sandbox_info)
    
    # Cancel health check timer
    if state.health_check_timer do
      :timer.cancel(state.health_check_timer)
    end
    
    :ok
  end
  
  defp start_local_sandbox(sandbox_spec) do
    # Use the existing local sandbox manager
    case OTPSupervisor.Core.SandboxManager.create_sandbox(
      sandbox_spec.id,
      sandbox_spec.module_or_app,
      sandbox_spec.opts
    ) do
      {:ok, local_info} ->
        {:ok, local_info}
      
      {:error, reason} ->
        {:error, reason}
    end
  end
  
  defp restart_local_sandbox(state) do
    cleanup_local_sandbox(state.local_sandbox_info)
    start_local_sandbox(state.sandbox_spec)
  end
  
  defp cleanup_local_sandbox(sandbox_info) do
    case OTPSupervisor.Core.SandboxManager.destroy_sandbox(sandbox_info.id) do
      :ok -> :ok
      {:error, reason} -> Logger.warning("Cleanup failed: #{inspect(reason)}")
    end
  end
  
  defp check_sandbox_health(sandbox_info) do
    case Process.alive?(sandbox_info.app_pid) do
      true -> :healthy
      false -> :unhealthy
    end
  end
  
  defp schedule_health_check do
    case :timer.send_after(30_000, self(), :health_check) do
      {:ok, timer_ref} -> timer_ref
      {:error, _} -> nil
    end
  end
end
```

#### Week 4: Testing and Optimization

**Day 16-17: Integration Testing**
- [ ] Test sandbox creation across multiple nodes
- [ ] Test node failure scenarios
- [ ] Test load balancing behavior

**Day 18-19: Performance Optimization**
- [ ] Optimize node selection algorithm
- [ ] Implement caching for node metrics
- [ ] Add batch operations for efficiency

**Day 20: Documentation and Cleanup**
- [ ] Document distributed sandbox API
- [ ] Clean up test code
- [ ] Prepare for next phase

**Deliverables:**
- Distributed sandbox creation working
- Node selection algorithm implemented
- Health monitoring for sandboxes
- Load balancing across nodes

### Phase 3: Arsenal Distribution (Weeks 5-6)
**Goal**: Extend Arsenal operations for distributed environments

#### Week 5: Distributed Arsenal Operations

**Day 21-22: Core Distributed Operations**
```elixir
# lib/otp_supervisor/distributed/arsenal/operations/list_distributed_sandboxes.ex
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
        },
        %{
          name: :limit,
          type: :integer,
          required: false,
          description: "Limit number of results",
          location: :query
        }
      ],
      responses: %{
        200 => %{
          description: "Distributed sandbox list",
          schema: %{
            type: :object,
            properties: %{
              cluster_info: %{type: :object},
              sandboxes: %{type: :array},
              total_sandboxes: %{type: :integer}
            }
          }
        }
      }
    }
  end
  
  def validate_params(params) do
    validated = %{}
    
    # Validate node parameter
    validated = case Map.get(params, "node") do
      nil -> validated
      node_string -> 
        case String.to_existing_atom(node_string) do
          node_atom -> Map.put(validated, "node", node_atom)
        rescue
          ArgumentError -> {:error, {:invalid_parameter, :node, "invalid node name"}}
        end
    end
    
    # Validate status parameter
    validated = case Map.get(params, "status") do
      nil -> validated
      status when status in ["running", "stopped", "error"] -> 
        Map.put(validated, "status", String.to_atom(status))
      _ -> 
        {:error, {:invalid_parameter, :status, "must be running, stopped, or error"}}
    end
    
    # Validate limit parameter
    validated = case Map.get(params, "limit") do
      nil -> validated
      limit_string ->
        case Integer.parse(limit_string) do
          {limit, ""} when limit > 0 -> Map.put(validated, "limit", limit)
          _ -> {:error, {:invalid_parameter, :limit, "must be positive integer"}}
        end
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
    limit = Map.get(params, "limit", 1000)
    
    target_nodes = case node_filter do
      nil -> cluster_nodes
      specific_node -> [specific_node]
    end
    
    # Collect sandboxes from all target nodes
    {successful_nodes, failed_nodes, all_sandboxes} = 
      collect_distributed_sandboxes(target_nodes)
    
    # Apply filters
    filtered_sandboxes = apply_filters(all_sandboxes, status_filter, limit)
    
    result = %{
      cluster_info: %{
        total_nodes: length(cluster_nodes),
        queried_nodes: length(target_nodes),
        successful_nodes: length(successful_nodes),
        failed_nodes: length(failed_nodes),
        query_timestamp: DateTime.utc_now()
      },
      sandboxes: filtered_sandboxes,
      total_sandboxes: length(filtered_sandboxes),
      node_failures: failed_nodes
    }
    
    {:ok, result}
  end
  
  defp collect_distributed_sandboxes(target_nodes) do
    node_results = Task.async_stream(target_nodes, fn node ->
      case :rpc.call(node, OTPSupervisor.Distributed.SandboxManager, :list_sandboxes, [], 5000) do
        {:badrpc, reason} -> {node, {:error, reason}}
        {:ok, sandboxes} -> {node, {:ok, sandboxes}}
        sandboxes when is_list(sandboxes) -> {node, {:ok, sandboxes}}
        error -> {node, {:error, error}}
      end
    end, timeout: 6000)
    
    Enum.reduce(node_results, {[], [], []}, fn 
      {:ok, {node, {:ok, sandboxes}}}, {success, failed, all_sandboxes} ->
        node_sandboxes = Enum.map(sandboxes, &Map.put(&1, :node, node))
        {[node | success], failed, all_sandboxes ++ node_sandboxes}
      
      {:ok, {node, {:error, reason}}}, {success, failed, all_sandboxes} ->
        {success, [{node, reason} | failed], all_sandboxes}
    end)
  end
  
  defp apply_filters(sandboxes, status_filter, limit) do
    sandboxes
    |> maybe_filter_by_status(status_filter)
    |> Enum.take(limit)
  end
  
  defp maybe_filter_by_status(sandboxes, nil), do: sandboxes
  defp maybe_filter_by_status(sandboxes, status) do
    Enum.filter(sandboxes, &(&1.status == status))
  end
end
```

**Day 23-24: Cross-Node Operations**
```elixir
# lib/otp_supervisor/distributed/arsenal/operations/cross_node_sandbox_management.ex
defmodule OTPSupervisor.Distributed.Arsenal.Operations.CrossNodeSandboxManagement do
  use OTPSupervisor.Core.Arsenal.Operation
  
  def rest_config do
    %{
      method: :post,
      path: "/api/v1/distributed/sandboxes",
      summary: "Create sandbox on optimal node",
      parameters: [
        %{
          name: :sandbox_id,
          type: :string,
          required: true,
          description: "Unique sandbox identifier",
          location: :body
        },
        %{
          name: :module_or_app,
          type: :string,
          required: true,
          description: "Module or application to run",
          location: :body
        },
        %{
          name: :preferred_node,
          type: :string,
          required: false,
          description: "Preferred node for placement",
          location: :body
        },
        %{
          name: :resource_requirements,
          type: :object,
          required: false,
          description: "Resource requirements for sandbox",
          location: :body
        }
      ],
      responses: %{
        201 => %{
          description: "Sandbox created successfully",
          schema: %{
            type: :object,
            properties: %{
              sandbox_id: %{type: :string},
              node: %{type: :string},
              status: %{type: :string},
              created_at: %{type: :string}
            }
          }
        }
      }
    }
  end
  
  def validate_params(params) do
    required_fields = ["sandbox_id", "module_or_app"]
    
    case validate_required_fields(params, required_fields) do
      :ok ->
        validated = %{
          "sandbox_id" => params["sandbox_id"],
          "module_or_app" => params["module_or_app"]
        }
        
        # Validate optional fields
        validated = add_optional_field(validated, params, "preferred_node", &validate_node/1)
        validated = add_optional_field(validated, params, "resource_requirements", &validate_resources/1)
        
        {:ok, validated}
      
      {:error, missing_field} ->
        {:error, {:missing_parameter, missing_field}}
    end
  end
  
  def execute(params) do
    sandbox_id = params["sandbox_id"]
    module_or_app = params["module_or_app"]
    preferred_node = Map.get(params, "preferred_node")
    resource_requirements = Map.get(params, "resource_requirements", %{})
    
    # Convert module string to atom
    module_atom = case String.starts_with?(module_or_app, "Elixir.") do
      true -> String.to_existing_atom(module_or_app)
      false -> String.to_existing_atom("Elixir." <> module_or_app)
    end
    
    opts = [
      preferred_node: preferred_node,
      resource_requirements: resource_requirements
    ]
    
    case OTPSupervisor.Distributed.SandboxManager.create_sandbox(
      sandbox_id,
      module_atom,
      opts
    ) do
      {:ok, sandbox_info} ->
        {:ok, %{
          sandbox_id: sandbox_info.id,
          node: sandbox_info.node,
          pid: inspect(sandbox_info.pid),
          status: sandbox_info.status,
          created_at: sandbox_info.created_at,
          resource_allocation: get_resource_allocation(sandbox_info)
        }}
      
      {:error, reason} ->
        {:error, reason}
    end
  end
  
  defp validate_required_fields(params, required_fields) do
    case Enum.find(required_fields, &(not Map.has_key?(params, &1))) do
      nil -> :ok
      missing_field -> {:error, missing_field}
    end
  end
  
  defp add_optional_field(validated, params, field_name, validator) do
    case Map.get(params, field_name) do
      nil -> validated
      value ->
        case validator.(value) do
          {:ok, validated_value} -> Map.put(validated, field_name, validated_value)
          {:error, _} -> validated  # Skip invalid optional fields
        end
    end
  end
  
  defp validate_node(node_string) do
    try do
      node_atom = String.to_existing_atom(node_string)
      case node_atom in [Node.self() | Node.list()] do
        true -> {:ok, node_atom}
        false -> {:error, :node_not_available}
      end
    rescue
      ArgumentError -> {:error, :invalid_node_name}
    end
  end
  
  defp validate_resources(resource_map) when is_map(resource_map) do
    {:ok, resource_map}
  end
  
  defp validate_resources(_), do: {:error, :invalid_resource_format}
  
  defp get_resource_allocation(sandbox_info) do
    case :rpc.call(sandbox_info.node, Process, :info, [sandbox_info.pid, :memory]) do
      {:badrpc, _} -> %{memory: 0}
      {:memory, memory} -> %{memory: memory}
      _ -> %{memory: 0}
    end
  end
end
```

**Day 25: Arsenal Registry Updates**
```elixir
# lib/otp_supervisor/distributed/arsenal/registry.ex
defmodule OTPSupervisor.Distributed.Arsenal.Registry do
  @moduledoc """
  Extended Arsenal registry for distributed operations
  """
  
  use GenServer
  
  @distributed_operations [
    OTPSupervisor.Distributed.Arsenal.Operations.ListDistributedSandboxes,
    OTPSupervisor.Distributed.Arsenal.Operations.CrossNodeSandboxManagement,
    OTPSupervisor.Distributed.Arsenal.Operations.ClusterStatus,
    OTPSupervisor.Distributed.Arsenal.Operations.NodeHealthCheck,
    OTPSupervisor.Distributed.Arsenal.Operations.MigrateSandbox
  ]
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def list_distributed_operations do
    GenServer.call(__MODULE__, :list_distributed_operations)
  end
  
  @impl true
  def init(_opts) do
    # Create ETS table for distributed operations
    :ets.new(:distributed_arsenal_operations, [
      :named_table,
      :public,
      :set,
      {:read_concurrency, true}
    ])
    
    # Register distributed operations
    register_distributed_operations()
    
    {:ok, %{}}
  end
  
  @impl true
  def handle_call(:list_distributed_operations, _from, state) do
    operations = 
      :distributed_arsenal_operations
      |> :ets.tab2list()
      |> Enum.map(fn {module, config} -> Map.put(config, :module, module) end)
    
    {:reply, operations, state}
  end
  
  defp register_distributed_operations do
    Enum.each(@distributed_operations, fn module ->
      case register_distributed_operation(module) do
        {:ok, config} ->
          :ets.insert(:distributed_arsenal_operations, {module, config})
          require Logger
          Logger.debug("Registered distributed Arsenal operation: #{module}")
        
        {:error, reason} ->
          require Logger
          Logger.warning("Failed to register distributed Arsenal operation #{module}: #{inspect(reason)}")
      end
    end)
  end
  
  defp register_distributed_operation(module) do
    try do
      config = module.rest_config()
      
      case validate_distributed_config(config) do
        :ok -> {:ok, Map.put(config, :module, module)}
        error -> error
      end
    rescue
      UndefinedFunctionError -> {:error, :invalid_operation_module}
      error -> {:error, {:module_error, error}}
    end
  end
  
  defp validate_distributed_config(config) do
    required_keys = [:method, :path, :summary]
    
    case Enum.all?(required_keys, &Map.has_key?(config, &1)) do
      true -> :ok
      false -> {:error, {:invalid_config, :missing_required_keys}}
    end
  end
end
```

#### Week 6: API Integration and Testing

**Day 26-27: Web Interface Integration**
- [ ] Update Arsenal LiveView for distributed operations
- [ ] Add cluster status display
- [ ] Implement distributed sandbox management UI

**Day 28-29: Testing and Validation**
- [ ] Test distributed Arsenal operations
- [ ] Validate API responses
- [ ] Test error handling across nodes

**Day 30: Performance Optimization**
- [ ] Optimize RPC call timeouts
- [ ] Implement operation caching
- [ ] Add request rate limiting

**Deliverables:**
- Distributed Arsenal operations working
- Cross-node sandbox management
- Web UI updated for distributed features
- Comprehensive API testing

### Phase 4: Monitoring and Observability (Weeks 7-8)
**Goal**: Implement comprehensive cluster monitoring and visualization

#### Week 7: Telemetry and Monitoring

**Day 31-32: Cluster Telemetry System**
```elixir
# lib/otp_supervisor/distributed/telemetry.ex
defmodule OTPSupervisor.Distributed.Telemetry do
  use GenServer
  require Logger
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def get_cluster_metrics do
    GenServer.call(__MODULE__, :get_cluster_metrics)
  end
  
  @impl true
  def init(_opts) do
    # Subscribe to cluster events
    Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "cluster_events")
    
    # Schedule periodic metric collection
    :timer.send_interval(5_000, :collect_metrics)
    :timer.send_interval(30_000, :health_check)
    
    state = %{
      cluster_metrics: %{},
      metric_history: [],
      alerts: []
    }
    
    {:ok, state}
  end
  
  @impl true
  def handle_call(:get_cluster_metrics, _from, state) do
    {:reply, state.cluster_metrics, state}
  end
  
  @impl true
  def handle_info(:collect_metrics, state) do
    cluster_nodes = [Node.self() | Node.list()]
    
    metrics = collect_cluster_metrics(cluster_nodes)
    
    # Update history (keep last 100 entries)
    new_history = [metrics | state.metric_history] |> Enum.take(100)
    
    # Broadcast metrics
    Phoenix.PubSub.broadcast(
      OtpSupervisor.PubSub,
      "cluster_metrics",
      {:metrics_update, metrics}
    )
    
    {:noreply, %{
      state |
      cluster_metrics: metrics,
      metric_history: new_history
    }}
  end
  
  @impl true
  def handle_info(:health_check, state) do
    alerts = perform_health_checks(state.cluster_metrics)
    
    # Broadcast alerts if any
    if length(alerts) > 0 do
      Phoenix.PubSub.broadcast(
        OtpSupervisor.PubSub,
        "cluster_alerts",
        {:alerts_update, alerts}
      )
    end
    
    {:noreply, %{state | alerts: alerts}}
  end
  
  defp collect_cluster_metrics(cluster_nodes) do
    metrics_tasks = Task.async_stream(cluster_nodes, fn node ->
      case :rpc.call(node, __MODULE__, :collect_node_metrics, [], 3000) do
        {:badrpc, reason} -> 
          {node, %{status: :unreachable, error: reason}}
        metrics -> 
          {node, Map.put(metrics, :status, :healthy)}
      end
    end, timeout: 4000)
    
    metrics_tasks
    |> Enum.reduce(%{}, fn {:ok, {node, metrics}}, acc ->
      Map.put(acc, node, Map.put(metrics, :timestamp, DateTime.utc_now()))
    end)
  end
  
  def collect_node_metrics do
    memory_info = :erlang.memory()
    process_count = length(Process.list())
    
    %{
      node: Node.self(),
      system: %{
        process_count: process_count,
        memory_total: Keyword.get(memory_info, :total, 0),
        memory_used: Keyword.get(memory_info, :processes, 0),
        cpu_usage: get_cpu_usage(),
        uptime: :erlang.system_info(:uptime)
      },
      sandboxes: %{
        total: count_node_sandboxes(),
        running: count_running_sandboxes(),
        memory_usage: get_sandbox_memory_usage()
      },
      cluster: %{
        connected_nodes: length(Node.list()),
        horde_members: get_horde_member_count()
      }
    }
  end
  
  defp perform_health_checks(cluster_metrics) do
    alerts = []
    
    # Check for node failures
    alerts = check_node_health(cluster_metrics, alerts)
    
    # Check for resource exhaustion
    alerts = check_resource_usage(cluster_metrics, alerts)
    
    # Check for sandbox failures
    alerts = check_sandbox_health(cluster_metrics, alerts)
    
    alerts
  end
  
  defp check_node_health(cluster_metrics, alerts) do
    unreachable_nodes = 
      cluster_metrics
      |> Enum.filter(fn {_node, metrics} -> metrics.status == :unreachable end)
      |> Enum.map(fn {node, _} -> node end)
    
    case unreachable_nodes do
      [] -> alerts
      nodes -> 
        alert = %{
          type: :node_unreachable,
          severity: :critical,
          message: "Nodes unreachable: #{Enum.join(nodes, ", ")}",
          nodes: nodes,
          timestamp: DateTime.utc_now()
        }
        
        [alert | alerts]
    end
  end
  
  defp check_resource_usage(cluster_metrics, alerts) do
    high_memory_nodes = 
      cluster_metrics
      |> Enum.filter(fn {_node, metrics} ->
        case metrics.status do
          :healthy -> 
            memory_usage = metrics.system.memory_used / metrics.system.memory_total
            memory_usage > 0.85
          _ -> false
        end
      end)
      |> Enum.map(fn {node, _} -> node end)
    
    case high_memory_nodes do
      [] -> alerts
      nodes ->
        alert = %{
          type: :high_memory_usage,
          severity: :warning,
          message: "High memory usage on nodes: #{Enum.join(nodes, ", ")}",
          nodes: nodes,
          timestamp: DateTime.utc_now()
        }
        
        [alert | alerts]
    end
  end
  
  defp check_sandbox_health(cluster_metrics, alerts) do
    # Check for nodes with high sandbox counts
    alerts
  end
  
  defp get_cpu_usage do
    case :cpu_sup.util() do
      {:error, _} -> 0
      usage -> usage
    end
  end
  
  defp count_node_sandboxes do
    case :ets.info(:sandboxes) do
      :undefined -> 0
      info -> Keyword.get(info, :size, 0)
    end
  end
  
  defp count_running_sandboxes do
    # Implementation depends on sandbox state tracking
    0
  end
  
  defp get_sandbox_memory_usage do
    # Implementation depends on sandbox monitoring
    0
  end
  
  defp get_horde_member_count do
    case Horde.Cluster.members(OTPSupervisor.Distributed.Supervisor) do
      {:ok, members} -> length(members)
      _ -> 0
    end
  end
end
```

**Day 33-34: Distributed Dashboard**
```elixir
# lib/otp_supervisor_web/live/distributed_dashboard_live.ex
defmodule OtpSupervisorWeb.Live.DistributedDashboardLive do
  use Phoenix.LiveView
  
  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OtpSupervisorWeb.Components.Layout.TerminalPanelLayout
  
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "cluster_metrics")
      Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "cluster_alerts")
      :timer.send_interval(10_000, :refresh_data)
    end
    
    {:ok,
     socket
     |> assign(:page_title, "Distributed Cluster Dashboard")
     |> assign(:current_page, "distributed")
     |> assign(:cluster_metrics, %{})
     |> assign(:cluster_alerts, [])
     |> assign(:selected_node, nil)
     |> assign(:sandbox_distribution, %{})
     |> load_initial_data()}
  end
  
  def handle_info({:metrics_update, metrics}, socket) do
    {:noreply, 
     socket
     |> assign(:cluster_metrics, metrics)
     |> assign(:sandbox_distribution, calculate_sandbox_distribution(metrics))
     |> maybe_update_selected_node(metrics)}
  end
  
  def handle_info({:alerts_update, alerts}, socket) do
    {:noreply, assign(socket, :cluster_alerts, alerts)}
  end
  
  def handle_info(:refresh_data, socket) do
    {:noreply, refresh_cluster_data(socket)}
  end
  
  def handle_event("select_node", %{"node" => node_name}, socket) do
    node_atom = String.to_existing_atom(node_name)
    {:noreply, assign(socket, :selected_node, node_atom)}
  end
  
  def handle_event("migrate_sandbox", %{"sandbox_id" => sandbox_id}, socket) do
    case OTPSupervisor.Distributed.SandboxManager.migrate_sandbox(sandbox_id) do
      {:ok, _} ->
        {:noreply, put_flash(socket, :info, "Sandbox migration initiated")}
      
      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Migration failed: #{inspect(reason)}")}
    end
  end
  
  def handle_event("rebalance_cluster", _params, socket) do
    case OTPSupervisor.Distributed.LoadBalancer.rebalance_cluster() do
      :ok ->
        {:noreply, put_flash(socket, :info, "Cluster rebalancing initiated")}
      
      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Rebalancing failed: #{inspect(reason)}")}
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
        panels={distributed_panels(assigns)}
        gap="gap-4"
        padding="p-4"
      />
    </div>
    """
  end
  
  defp distributed_panels(assigns) do
    [
      # Cluster Overview
      %{
        title: "Cluster Overview",
        slot: render_cluster_overview(assigns),
        span: %{cols: 2, rows: 1}
      },
      
      # Node Details
      %{
        title: "Node Details",
        slot: render_node_details(assigns),
        span: %{cols: 2, rows: 1}
      },
      
      # Sandbox Distribution
      %{
        title: "Sandbox Distribution",
        slot: render_sandbox_distribution(assigns),
        span: %{cols: 2, rows: 2}
      },
      
      # Alerts
      %{
        title: "Cluster Alerts",
        slot: render_cluster_alerts(assigns),
        span: %{cols: 2, rows: 1}
      }
    ]
  end
  
  defp render_cluster_overview(assigns) do
    ~H"""
    <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
      <%= for {node, metrics} <- @cluster_metrics do %>
        <div class={[
          "p-4 rounded border-2 transition-all duration-200",
          node_card_classes(node, metrics, @selected_node)
        ]}>
          <div class="flex items-center justify-between mb-2">
            <h3 class="font-mono text-sm font-bold"><%= node %></h3>
            <div class={["w-3 h-3 rounded-full", status_color(metrics.status)]}></div>
          </div>
          
          <div class="space-y-1 text-xs font-mono">
            <div>Sandboxes: <%= metrics.sandboxes.total %></div>
            <div>Memory: <%= format_percentage(metrics.system.memory_used, metrics.system.memory_total) %>%</div>
            <div>CPU: <%= metrics.system.cpu_usage %>%</div>
            <div>Processes: <%= metrics.system.process_count %></div>
          </div>
          
          <button
            phx-click="select_node"
            phx-value-node={to_string(node)}
            class="mt-2 w-full px-2 py-1 bg-green-500/20 text-green-400 rounded text-xs hover:bg-green-500/30"
          >
            Select
          </button>
        </div>
      <% end %>
    </div>
    """
  end
  
  defp render_sandbox_distribution(assigns) do
    ~H"""
    <div class="space-y-4">
      <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div>
          <h4 class="text-sm font-mono font-bold mb-2">Distribution by Node</h4>
          <div class="space-y-2">
            <%= for {node, count} <- @sandbox_distribution do %>
              <div class="flex justify-between items-center">
                <span class="text-xs font-mono"><%= node %></span>
                <span class="text-xs font-mono"><%= count %></span>
              </div>
            <% end %>
          </div>
        </div>
        
        <div>
          <h4 class="text-sm font-mono font-bold mb-2">Actions</h4>
          <div class="space-y-2">
            <button
              phx-click="rebalance_cluster"
              class="w-full px-3 py-2 bg-blue-500/20 text-blue-400 rounded text-xs hover:bg-blue-500/30"
            >
              Rebalance Cluster
            </button>
            
            <button
              phx-click="migrate_sandbox"
              phx-value-sandbox-id="auto"
              class="w-full px-3 py-2 bg-purple-500/20 text-purple-400 rounded text-xs hover:bg-purple-500/30"
            >
              Auto Migrate
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end
  
  defp render_cluster_alerts(assigns) do
    ~H"""
    <div class="space-y-2">
      <%= if @cluster_alerts == [] do %>
        <div class="text-center text-green-400/70 font-mono text-sm">
          No alerts - cluster is healthy
        </div>
      <% else %>
        <%= for alert <- @cluster_alerts do %>
          <div class={[
            "p-3 rounded border-l-4",
            alert_classes(alert.severity)
          ]}>
            <div class="flex items-center justify-between">
              <span class="font-mono text-xs font-bold"><%= alert.type %></span>
              <span class="font-mono text-xs"><%= format_timestamp(alert.timestamp) %></span>
            </div>
            <div class="font-mono text-xs mt-1"><%= alert.message %></div>
          </div>
        <% end %>
      <% end %>
    </div>
    """
  end
  
  # Helper functions
  defp load_initial_data(socket) do
    case OTPSupervisor.Distributed.Telemetry.get_cluster_metrics() do
      metrics ->
        socket
        |> assign(:cluster_metrics, metrics)
        |> assign(:sandbox_distribution, calculate_sandbox_distribution(metrics))
      
      _ ->
        socket
    end
  end
  
  defp calculate_sandbox_distribution(metrics) do
    Enum.map(metrics, fn {node, node_metrics} ->
      {node, node_metrics.sandboxes.total}
    end)
  end
  
  defp cluster_status_metrics(assigns) do
    total_nodes = map_size(assigns.cluster_metrics)
    healthy_nodes = Enum.count(assigns.cluster_metrics, fn {_, metrics} -> metrics.status == :healthy end)
    total_sandboxes = Enum.sum(Enum.map(assigns.sandbox_distribution, fn {_, count} -> count end))
    alert_count = length(assigns.cluster_alerts)
    
    [
      %{label: "Nodes", value: "#{healthy_nodes}/#{total_nodes}"},
      %{label: "Sandboxes", value: "#{total_sandboxes}"},
      %{label: "Alerts", value: "#{alert_count}"},
      %{label: "Status", value: cluster_health_status(assigns.cluster_metrics)}
    ]
  end
  
  defp cluster_health_status(metrics) do
    case Enum.all?(metrics, fn {_, node_metrics} -> node_metrics.status == :healthy end) do
      true -> "Healthy"
      false -> "Degraded"
    end
  end
  
  defp format_percentage(used, total) when total > 0 do
    ((used / total) * 100) |> Float.round(1)
  end
  
  defp format_percentage(_, _), do: 0
  
  defp format_timestamp(timestamp) do
    timestamp
    |> DateTime.to_time()
    |> Time.to_string()
    |> String.slice(0, 8)
  end
  
  defp node_card_classes(node, metrics, selected_node) do
    base_classes = ["border-green-500/30", "hover:border-green-500/50"]
    
    status_classes = case metrics.status do
      :healthy -> ["bg-green-500/5"]
      :unreachable -> ["bg-red-500/5", "border-red-500/50"]
      _ -> ["bg-yellow-500/5", "border-yellow-500/50"]
    end
    
    selected_classes = if node == selected_node do
      ["border-green-500", "bg-green-500/10"]
    else
      []
    end
    
    base_classes ++ status_classes ++ selected_classes
  end
  
  defp status_color(:healthy), do: "bg-green-500"
  defp status_color(:unreachable), do: "bg-red-500"
  defp status_color(_), do: "bg-yellow-500"
  
  defp alert_classes(:critical), do: ["border-red-500", "bg-red-500/10", "text-red-400"]
  defp alert_classes(:warning), do: ["border-yellow-500", "bg-yellow-500/10", "text-yellow-400"]
  defp alert_classes(:info), do: ["border-blue-500", "bg-blue-500/10", "text-blue-400"]
end
```

#### Week 8: Performance Optimization and Documentation

**Day 35-36: Performance Monitoring**
- [ ] Implement performance metrics collection
- [ ] Add latency monitoring for distributed operations
- [ ] Optimize RPC call patterns

**Day 37-38: Load Testing**
- [ ] Create load testing scenarios
- [ ] Test cluster under stress
- [ ] Identify and fix bottlenecks

**Day 39-40: Documentation and Deployment**
- [ ] Complete implementation documentation
- [ ] Create deployment guides
- [ ] Prepare production configurations

**Deliverables:**
- Comprehensive cluster monitoring
- Real-time distributed dashboard
- Performance optimization
- Complete documentation

## Success Criteria

### Phase 1 Success Criteria
- [ ] Two nodes can form a cluster successfully
- [ ] Horde registry and supervisor are operational
- [ ] Basic node failure detection works
- [ ] Cluster status can be monitored

### Phase 2 Success Criteria
- [ ] Sandboxes can be created on any node in the cluster
- [ ] Node selection algorithm distributes load appropriately
- [ ] Sandbox health monitoring works across nodes
- [ ] Node failures trigger sandbox migration

### Phase 3 Success Criteria
- [ ] Arsenal operations work across multiple nodes
- [ ] Distributed sandbox management API is functional
- [ ] Web interface supports distributed operations
- [ ] Cross-node process management works

### Phase 4 Success Criteria
- [ ] Comprehensive cluster monitoring is operational
- [ ] Real-time dashboard shows cluster health
- [ ] Performance metrics are collected and displayed
- [ ] Alerting system works for cluster issues

## Risk Mitigation

### Technical Risks
1. **Network Partitions**: Implement quorum-based decision making
2. **State Consistency**: Use distributed state replication
3. **Performance Degradation**: Implement caching and optimization
4. **Security Issues**: Add authentication and isolation

### Operational Risks
1. **Complexity**: Phased implementation with thorough testing
2. **Debugging Difficulty**: Comprehensive logging and tracing
3. **Deployment Challenges**: Detailed deployment documentation
4. **Maintenance Overhead**: Automated monitoring and alerts

## Testing Strategy

### Unit Testing
- Test individual distributed components
- Mock RPC calls and network interactions
- Validate error handling paths

### Integration Testing
- Test multi-node scenarios
- Validate cluster formation and failure handling
- Test distributed operations end-to-end

### Performance Testing
- Load testing with multiple nodes
- Latency testing for distributed operations
- Resource utilization monitoring

### Chaos Engineering
- Simulate network partitions
- Test node failures and recovery
- Validate system resilience

## Conclusion

This implementation roadmap provides a structured approach to building a distributed sandboxed environment system. The phased approach ensures incremental progress while maintaining system stability and educational value. Each phase builds upon the previous one, creating a robust, scalable, and maintainable distributed architecture.

The success of this implementation depends on careful attention to distributed systems challenges, comprehensive testing, and proactive monitoring. The end result will be a powerful educational platform that demonstrates real-world distributed systems concepts while providing practical OTP supervision capabilities.