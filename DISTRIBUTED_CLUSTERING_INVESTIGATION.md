# Distributed Clustering Investigation for World-Class Elixir Debugger

## Executive Summary

This investigation analyzes the integration of LibCluster and Horde for building a distributed Elixir debugging platform with 90%+ success probability. The goal is to create the world's leading Elixir debugger with native clustering capabilities.

## Current State Analysis

### Existing Infrastructure
- **WSL Cluster Setup**: Two-node development environment with port-based differentiation
- **Core Debugging Platform**: Comprehensive OTP introspection with Control, SandboxManager, Arsenal APIs
- **LiveView Interface**: Real-time web dashboards for system monitoring
- **Sandbox Isolation**: True fault isolation with IsolatedCompiler and ModuleVersionManager

### Current Limitations
- Single-node operation only
- No cross-node process debugging
- No distributed sandbox management
- No cluster-wide monitoring

## LibCluster vs Horde: Roles and Responsibilities

### LibCluster: Cluster Formation and Membership
**Purpose**: Automatic node discovery and cluster formation
**Responsibilities**:
- Node discovery and connection
- Cluster membership management
- Network partition handling
- Topology strategies (Epmd, Gossip, Kubernetes, etc.)

**Key Features for Our Use Case**:
- `Cluster.Strategy.Epmd` for development (WSL environment)
- Automatic reconnection on network issues
- Configurable polling intervals
- Debug logging for troubleshooting

### Horde: Distributed Process Management
**Purpose**: Distributed supervision and process registry
**Responsibilities**:
- Distributed process supervision across nodes
- Global process registry
- Automatic process migration on node failures
- Load balancing across cluster nodes

**Key Components**:
- `Horde.Registry`: Global process registry
- `Horde.DynamicSupervisor`: Distributed supervision
- `Horde.UniformDistribution`: Load balancing strategy

## Integration Architecture

### Phase 1: Foundation (Minimal Viable Cluster)
**Goal**: Basic cluster formation with distributed process registry
**Success Metrics**: 
- Two nodes automatically discover each other
- Processes can be registered globally
- Basic cross-node communication works

### Phase 2: Distributed Debugging Core
**Goal**: Extend existing Control module for cross-node operations
**Success Metrics**:
- List processes across all cluster nodes
- Kill processes on remote nodes
- Get process info from any node in cluster

### Phase 3: Distributed Sandbox Management
**Goal**: Sandboxes can be created on any node and managed globally
**Success Metrics**:
- Create sandbox on optimal node based on resources
- List all sandboxes across cluster
- Migrate sandboxes between nodes

### Phase 4: Cluster Monitoring Dashboard
**Goal**: Real-time cluster-wide monitoring and visualization
**Success Metrics**:
- Live cluster topology view
- Cross-node resource monitoring
- Distributed sandbox status

## Minimum Viable Implementation

### 1. LibCluster Configuration
```elixir
# config/config.exs
config :libcluster,
  topologies: [
    debug_cluster: [
      strategy: Cluster.Strategy.Epmd,
      config: [
        hosts: [
          :superlearner1@localhost,
          :superlearner2@localhost
        ],
        polling_interval: 2_000,
        connect: :all_visible,
        hidden: false
      ]
    ]
  ]
```

### 2. Horde Integration
```elixir
# lib/otp_supervisor/distributed/cluster_supervisor.ex
defmodule OTPSupervisor.Distributed.ClusterSupervisor do
  use Supervisor
  
  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(_opts) do
    children = [
      # LibCluster for node discovery
      {Cluster.Supervisor, [Application.get_env(:libcluster, :topologies)]},
      
      # Horde components for distributed processes
      {Horde.Registry, [
        name: OTPSupervisor.Distributed.Registry,
        keys: :unique,
        members: :auto
      ]},
      {Horde.DynamicSupervisor, [
        name: OTPSupervisor.Distributed.Supervisor,
        strategy: :one_for_one,
        distribution_strategy: Horde.UniformDistribution,
        members: :auto
      ]},
      
      # Cluster event handler
      OTPSupervisor.Distributed.ClusterEventHandler
    ]
    
    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

### 3. Distributed Control Extension
```elixir
# lib/otp_supervisor/distributed/control.ex
defmodule OTPSupervisor.Distributed.Control do
  @moduledoc """
  Distributed extensions to the core Control module.
  Provides cross-node process management and debugging capabilities.
  """
  
  def list_cluster_processes(opts \\ []) do
    cluster_nodes = [Node.self() | Node.list()]
    timeout = Keyword.get(opts, :timeout, 5_000)
    
    # Parallel process listing across all nodes
    cluster_nodes
    |> Task.async_stream(fn node ->
      case :rpc.call(node, OTPSupervisor.Core.Control, :list_all_processes, [opts], timeout) do
        {:badrpc, reason} -> {node, {:error, reason}}
        processes -> {node, {:ok, processes}}
      end
    end, timeout: timeout + 1_000)
    |> Enum.reduce(%{}, fn
      {:ok, {node, {:ok, processes}}}, acc ->
        Map.put(acc, node, processes)
      {:ok, {node, {:error, reason}}}, acc ->
        Map.put(acc, node, {:error, reason})
    end)
  end
  
  def kill_remote_process(node, pid_or_string) do
    case :rpc.call(node, OTPSupervisor.Core.Control, :kill_process, [pid_or_string]) do
      {:badrpc, reason} -> {:error, {:rpc_failed, reason}}
      result -> result
    end
  end
  
  def get_cluster_status do
    cluster_nodes = [Node.self() | Node.list()]
    
    %{
      local_node: Node.self(),
      cluster_nodes: cluster_nodes,
      cluster_size: length(cluster_nodes),
      horde_members: %{
        registry: Horde.Cluster.members(OTPSupervisor.Distributed.Registry),
        supervisor: Horde.Cluster.members(OTPSupervisor.Distributed.Supervisor)
      }
    }
  end
end
```

### 4. Cluster Event Handler
```elixir
# lib/otp_supervisor/distributed/cluster_event_handler.ex
defmodule OTPSupervisor.Distributed.ClusterEventHandler do
  use GenServer
  require Logger
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def init(_opts) do
    # Subscribe to cluster events
    :net_kernel.monitor_nodes(true, [nodedown_reason: true])
    
    {:ok, %{
      cluster_nodes: [Node.self()],
      node_status: %{Node.self() => :up}
    }}
  end
  
  def handle_info({:nodeup, node, _info}, state) do
    Logger.info("Node joined cluster: #{node}")
    
    # Broadcast cluster change
    Phoenix.PubSub.broadcast(
      OtpSupervisor.PubSub,
      "cluster_events",
      {:node_joined, node}
    )
    
    new_nodes = [node | state.cluster_nodes] |> Enum.uniq()
    new_status = Map.put(state.node_status, node, :up)
    
    {:noreply, %{state | cluster_nodes: new_nodes, node_status: new_status}}
  end
  
  def handle_info({:nodedown, node, reason}, state) do
    Logger.warning("Node left cluster: #{node}, reason: #{inspect(reason)}")
    
    # Broadcast cluster change
    Phoenix.PubSub.broadcast(
      OtpSupervisor.PubSub,
      "cluster_events",
      {:node_left, node, reason}
    )
    
    new_nodes = List.delete(state.cluster_nodes, node)
    new_status = Map.put(state.node_status, node, :down)
    
    {:noreply, %{state | cluster_nodes: new_nodes, node_status: new_status}}
  end
end
```

## Success Metrics and Testing Strategy

### Phase 1 Success Criteria
1. **Cluster Formation**: Both nodes automatically connect within 10 seconds
2. **Horde Registry**: Can register/lookup processes globally
3. **Basic RPC**: Cross-node function calls work reliably
4. **Event Handling**: Node up/down events are properly handled

### Phase 2 Success Criteria
1. **Cross-Node Process Listing**: Can list processes from all nodes
2. **Remote Process Control**: Can kill processes on remote nodes
3. **Distributed Process Info**: Can get detailed info from any node
4. **Error Handling**: Graceful handling of node failures

### Phase 3 Success Criteria
1. **Distributed Sandboxes**: Can create sandboxes on any node
2. **Load Balancing**: Sandboxes distributed based on node resources
3. **Sandbox Migration**: Can move sandboxes between nodes
4. **Global Sandbox Registry**: Can list/manage all sandboxes

### Phase 4 Success Criteria
1. **Real-time Dashboard**: Live cluster topology visualization
2. **Resource Monitoring**: Cross-node resource usage tracking
3. **Health Monitoring**: Automatic detection of node issues
4. **Performance Metrics**: Cluster-wide performance tracking

## Risk Mitigation

### Network Partitions
- Implement quorum-based decisions for critical operations
- Use Horde's built-in partition tolerance
- Graceful degradation to single-node mode

### Node Failures
- Automatic process migration via Horde
- Sandbox state persistence for recovery
- Health checks and automatic failover

### Performance Impact
- Configurable RPC timeouts
- Async operations where possible
- Resource usage monitoring

## Implementation Timeline

### Week 1: Foundation Setup
- LibCluster configuration and testing
- Basic Horde integration
- Cluster event handling

### Week 2: Distributed Control
- Cross-node process management
- Remote debugging capabilities
- Error handling and timeouts

### Week 3: Distributed Sandboxes
- Global sandbox registry
- Load balancing implementation
- Sandbox migration basics

### Week 4: Monitoring Dashboard
- Real-time cluster visualization
- Resource monitoring
- Performance metrics

## Dependencies and Requirements

### New Dependencies
```elixir
# mix.exs
{:libcluster, "~> 3.3"},
{:horde, "~> 0.8.0"}
```

### Configuration Changes
- LibCluster topology configuration
- Horde member configuration
- Node-specific environment variables

### Infrastructure Requirements
- Consistent Erlang cookies across nodes
- Network connectivity between nodes
- Shared code deployment

## Conclusion

This investigation provides a clear path to implementing distributed clustering with 90%+ success probability by:

1. **Leveraging Existing Architecture**: Building on proven Control and SandboxManager modules
2. **Minimal Viable Approach**: Starting with basic clustering and incrementally adding features
3. **Clear Success Metrics**: Measurable goals for each phase
4. **Risk Mitigation**: Addressing common distributed system challenges
5. **Realistic Timeline**: Achievable milestones with concrete deliverables

The combination of LibCluster for cluster formation and Horde for distributed process management provides a solid foundation for building the world's leading Elixir debugger with native clustering capabilities.