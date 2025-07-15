# PROMPT 01: Distributed Clustering Foundation for World-Class Elixir Debugger

## Context and Vision

You are building the **world's leading debugger for Elixir** with native clustering capabilities. This is not an educational platform - this is a production-grade, distributed debugging and introspection system that will revolutionize how developers debug Elixir applications in distributed environments.

### Current System Status

The platform currently has:
- ✅ **Core Process Control** (`lib/otp_supervisor/core/control.ex`) - Complete supervisor and process management
- ✅ **Sandbox Management** (`lib/otp_supervisor/core/sandbox_manager.ex`) - Isolated sandbox creation and management  
- ✅ **Analytics Server** (`lib/otp_supervisor/core/analytics_server.ex`) - Supervisor restart tracking and analytics
- ✅ **Message Tracing** (`lib/otp_supervisor/core/message_tracer.ex`) - Process message flow analysis
- ✅ **Arsenal API System** - Metaprogrammed REST API framework with dynamic routing
- ✅ **LiveView Interfaces** - Real-time web interfaces with terminal-style components
- ✅ **Basic Clustering Setup** - WSL cluster setup scripts and initial configuration

### What You're Building

A **distributed debugging platform** that enables:
- **Cross-node process inspection** and control
- **Distributed sandbox management** with intelligent node placement
- **Cluster-wide message tracing** and analysis
- **Real-time distributed system visualization**
- **Hot code reloading across cluster nodes**
- **Distributed breakpoints and debugging sessions**
- **Cluster health monitoring and auto-scaling**

## Required Reading

### Design Documents
Read these documents completely before starting:

1. **`DISTRIBUTED_SANDBOX_ARCHITECTURE.md`** - Complete distributed architecture plan
2. **`WSL_CLUSTER_SETUP.md`** - Current clustering setup and configuration
3. **`docs/technical_design/01_ARCHITECTURE_DESIGN.md`** - Overall system architecture
4. **`docs/technical_design/07_PERFORMANCE_SCALABILITY_DESIGN.md`** - Performance and scalability requirements

### Current Implementation Files
Study these existing implementations to understand patterns:

1. **`lib/otp_supervisor/core/control.ex`** - Core API patterns and process management
2. **`lib/otp_supervisor/core/sandbox_manager.ex`** - Sandbox lifecycle management
3. **`lib/otp_supervisor/core/analytics_server.ex`** - Analytics and monitoring patterns
4. **`lib/otp_supervisor/core/arsenal.ex`** - Arsenal API framework patterns
5. **`lib/otp_supervisor_web/controllers/api/v1/`** - REST API controller patterns
6. **`lib/otp_supervisor_web/live/supervisor_live.ex`** - LiveView patterns

### Clustering Infrastructure
Review the existing clustering work:

1. **`scripts/cluster_status.sh`** - Cluster monitoring script
2. **`scripts/start_node1.sh`** and **`scripts/start_node2.sh`** - Node startup scripts
3. **`test_cluster.exs`** - Basic cluster connectivity testing
4. **`config/dev.exs`** - Current node configuration

### Test Files
Review these test patterns:

1. **`test/otp_supervisor/core/control_test.exs`** - Testing patterns for core modules
2. **`test/otp_supervisor/core/sandbox_manager_test.exs`** - Sandbox testing patterns
3. **`test/otp_supervisor/core/analytics_server_test.exs`** - Analytics testing patterns

## Implementation Requirements

### Phase 1: Distributed Foundation (Weeks 1-2)

#### 1.1 Cluster Manager

Create `lib/otp_supervisor/distributed/cluster_manager.ex`:

```elixir
defmodule OTPSupervisor.Distributed.ClusterManager do
  @moduledoc """
  Core cluster management for distributed debugging platform.
  
  Manages cluster formation, node health monitoring, and provides
  the foundation for distributed debugging operations across nodes.
  """
  
  use GenServer
  require Logger
  
  defstruct [
    :cluster_nodes,        # Map: node -> NodeInfo
    :node_health,         # Map: node -> HealthStatus
    :cluster_topology,    # Current cluster topology
    :partition_detector,  # PID of partition detection process
    :health_monitor,      # PID of health monitoring process
    :metrics_collector    # PID of cluster metrics collection
  ]
  
  # Public API functions you must implement:
  # - start_link/1
  # - get_cluster_status/0
  # - get_node_health/1
  # - get_cluster_topology/0
  # - register_node_join/1
  # - register_node_leave/1
  # - detect_network_partitions/0
  # - get_cluster_metrics/0
  # - select_optimal_node/1 (for sandbox placement)
end
```

#### 1.2 Distributed Sandbox Manager

Create `lib/otp_supervisor/distributed/sandbox_manager.ex`:

```elixir
defmodule OTPSupervisor.Distributed.SandboxManager do
  @moduledoc """
  Distributed sandbox management with intelligent node placement.
  
  Extends the existing sandbox manager to work across cluster nodes,
  providing intelligent placement, migration, and fault tolerance.
  """
  
  use GenServer
  require Logger
  
  # Must implement distributed sandbox lifecycle:
  # - create_distributed_sandbox/3
  # - migrate_sandbox/3
  # - destroy_distributed_sandbox/1
  # - list_distributed_sandboxes/0
  # - get_sandbox_node/1
  # - balance_sandbox_load/0
  
  # Must integrate with existing SandboxManager
  # Must provide intelligent node selection based on resources
  # Must handle node failures gracefully with sandbox migration
end
```

#### 1.3 LibCluster Integration

Update `mix.exs` to add clustering dependencies:

```elixir
defp deps do
  [
    # ... existing deps
    {:libcluster, "~> 3.3"},
    {:horde, "~> 0.8.0"},
    {:phoenix_pubsub, "~> 2.1"},  # For distributed events
    # ... rest of deps
  ]
end
```

Create `lib/otp_supervisor/distributed/cluster_supervisor.ex`:

```elixir
defmodule OTPSupervisor.Distributed.ClusterSupervisor do
  @moduledoc """
  Supervision tree for distributed clustering components.
  
  Manages LibCluster, Horde components, and distributed debugging services.
  """
  
  use Supervisor
  
  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  @impl true
  def init(_opts) do
    children = [
      # LibCluster for automatic cluster formation
      {Cluster.Supervisor, [Application.get_env(:libcluster, :topologies)]},
      
      # Horde for distributed process management
      {Horde.Registry, [name: OTPSupervisor.Distributed.Registry, keys: :unique]},
      {Horde.DynamicSupervisor, [
        name: OTPSupervisor.Distributed.Supervisor,
        strategy: :one_for_one,
        distribution_strategy: Horde.UniformDistribution
      ]},
      
      # Distributed debugging components
      OTPSupervisor.Distributed.ClusterManager,
      OTPSupervisor.Distributed.SandboxManager,
      OTPSupervisor.Distributed.MessageTracer,
      OTPSupervisor.Distributed.ProcessInspector,
      OTPSupervisor.Distributed.HealthMonitor
    ]
    
    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

#### 1.4 Configuration Updates

Create `config/cluster.exs`:

```elixir
import Config

# LibCluster configuration for automatic cluster formation
config :libcluster,
  debug: true,
  topologies: [
    debug_cluster: [
      strategy: Cluster.Strategy.Epmd,
      config: [
        hosts: [
          :"superlearner@U2401",
          :"superlearner2@U2402"
        ],
        polling_interval: 5_000,
        connect: :all_visible,
        hidden: false
      ]
    ]
  ]

# Horde configuration for distributed process management
config :horde,
  registry: [
    name: OTPSupervisor.Distributed.Registry,
    keys: :unique
  ],
  supervisor: [
    name: OTPSupervisor.Distributed.Supervisor,
    strategy: :one_for_one,
    distribution_strategy: Horde.UniformDistribution
  ]

# Distributed debugging configuration
config :otp_supervisor, :distributed,
  cluster_name: :debug_cluster,
  node_discovery: :epmd,
  health_check_interval: 5_000,
  partition_detection_interval: 10_000,
  sandbox_migration_timeout: 30_000
```

Update `config/dev.exs` and create `config/dev2.exs`:

```elixir
# config/dev.exs (Node 1)
import_config "cluster.exs"

config :otp_supervisor, :node_config,
  name: :"superlearner@U2401",
  role: :primary,
  port: 4000,
  cluster_port: 25001

# config/dev2.exs (Node 2)
import_config "dev.exs"

config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [port: 4010]

config :otp_supervisor, :node_config,
  name: :"superlearner2@U2402", 
  role: :secondary,
  port: 4010,
  cluster_port: 25002
```

### Phase 2: Distributed Process Management (Weeks 3-4)

#### 2.1 Distributed Process Inspector

Create `lib/otp_supervisor/distributed/process_inspector.ex`:

```elixir
defmodule OTPSupervisor.Distributed.ProcessInspector do
  @moduledoc """
  Cross-node process inspection and debugging capabilities.
  
  Provides the ability to inspect, trace, and debug processes
  across all nodes in the cluster with unified interfaces.
  """
  
  use GenServer
  require Logger
  
  # Must implement cross-node process operations:
  # - list_all_cluster_processes/0
  # - get_remote_process_info/2 (node, pid)
  # - trace_cross_node_messages/2
  # - set_distributed_breakpoints/3
  # - get_cluster_process_graph/0
  # - kill_remote_process/3 (node, pid, reason)
  # - inspect_remote_process_state/2
  
  # Must integrate with existing Control module
  # Must provide unified view across all cluster nodes
end
```

#### 2.2 Distributed Message Tracer

Create `lib/otp_supervisor/distributed/message_tracer.ex`:

```elixir
defmodule OTPSupervisor.Distributed.MessageTracer do
  @moduledoc """
  Distributed message tracing across cluster nodes.
  
  Extends message tracing capabilities to work across the entire
  cluster, providing unified message flow analysis and debugging.
  """
  
  use GenServer
  require Logger
  
  # Must implement distributed tracing:
  # - start_cluster_trace/2 (process_pattern, options)
  # - trace_cross_node_messages/2
  # - get_cluster_message_history/1
  # - analyze_distributed_patterns/1
  # - stop_cluster_trace/1
  # - get_message_flow_graph/0
  
  # Must integrate with existing MessageTracer
  # Must handle cross-node message correlation
end
```

#### 2.3 Health Monitor

Create `lib/otp_supervisor/distributed/health_monitor.ex`:

```elixir
defmodule OTPSupervisor.Distributed.HealthMonitor do
  @moduledoc """
  Cluster-wide health monitoring and alerting system.
  
  Monitors the health of all nodes, detects failures, and provides
  early warning systems for distributed debugging operations.
  """
  
  use GenServer
  require Logger
  
  # Must implement health monitoring:
  # - monitor_cluster_health/0
  # - detect_node_failures/0
  # - detect_network_partitions/0
  # - get_cluster_health_report/0
  # - alert_on_health_issues/1
  # - predict_node_failures/0 (basic ML)
  
  # Must integrate with Phoenix PubSub for real-time alerts
  # Must provide health metrics for dashboard
end
```

### Phase 3: Arsenal Distribution (Weeks 5-6)

#### 3.1 Distributed Arsenal Operations

Create distributed Arsenal operations in `lib/otp_supervisor/distributed/arsenal/operations/`:

**`list_cluster_processes.ex`**:
```elixir
defmodule OTPSupervisor.Distributed.Arsenal.Operations.ListClusterProcesses do
  @moduledoc """
  Arsenal operation to list processes across all cluster nodes.
  """
  
  use OTPSupervisor.Core.Arsenal.Operation
  
  def rest_config do
    %{
      method: :get,
      path: "/api/v1/cluster/processes",
      summary: "List all processes across cluster nodes",
      parameters: [
        %{name: :node, type: :string, required: false, location: :query},
        %{name: :type, type: :string, required: false, location: :query},
        %{name: :page, type: :integer, required: false, location: :query},
        %{name: :per_page, type: :integer, required: false, location: :query}
      ]
    }
  end
  
  # Must implement validate_params/1 and execute/1
  # Must provide unified process listing across cluster
end
```

**`cross_node_sandbox_management.ex`**:
```elixir
defmodule OTPSupervisor.Distributed.Arsenal.Operations.CrossNodeSandboxManagement do
  @moduledoc """
  Arsenal operations for distributed sandbox management.
  """
  
  use OTPSupervisor.Core.Arsenal.Operation
  
  # Must implement operations for:
  # - Creating sandboxes on specific nodes
  # - Migrating sandboxes between nodes
  # - Load balancing sandbox distribution
  # - Getting cluster-wide sandbox status
end
```

**`distributed_debugging.ex`**:
```elixir
defmodule OTPSupervisor.Distributed.Arsenal.Operations.DistributedDebugging do
  @moduledoc """
  Arsenal operations for distributed debugging capabilities.
  """
  
  use OTPSupervisor.Core.Arsenal.Operation
  
  # Must implement operations for:
  # - Setting breakpoints across nodes
  # - Cross-node message tracing
  # - Distributed process inspection
  # - Cluster-wide performance profiling
end
```

#### 3.2 Distributed API Controllers

Create `lib/otp_supervisor_web/controllers/api/v1/cluster_controller.ex`:

```elixir
defmodule OtpSupervisorWeb.Api.V1.ClusterController do
  @moduledoc """
  REST API controller for cluster-wide operations.
  
  Provides HTTP endpoints for distributed debugging and cluster management.
  """
  
  use OtpSupervisorWeb, :controller
  
  alias OTPSupervisor.Distributed.ClusterManager
  alias OTPSupervisor.Distributed.ProcessInspector
  alias OTPSupervisor.Distributed.SandboxManager
  
  # Must implement endpoints:
  # GET /api/v1/cluster/status
  # GET /api/v1/cluster/nodes
  # GET /api/v1/cluster/processes
  # GET /api/v1/cluster/sandboxes
  # POST /api/v1/cluster/sandboxes
  # PUT /api/v1/cluster/sandboxes/:id/migrate
  # DELETE /api/v1/cluster/sandboxes/:id
  # GET /api/v1/cluster/health
  # POST /api/v1/cluster/trace/start
  # DELETE /api/v1/cluster/trace/:id
end
```

### Phase 4: Real-time Distributed Dashboard (Weeks 7-8)

#### 4.1 Distributed Dashboard LiveView

Create `lib/otp_supervisor_web/live/distributed_dashboard_live.ex`:

```elixir
defmodule OtpSupervisorWeb.Live.DistributedDashboardLive do
  @moduledoc """
  Real-time distributed cluster dashboard.
  
  Provides comprehensive visualization of cluster state, process distribution,
  sandbox management, and debugging operations across all nodes.
  """
  
  use Phoenix.LiveView
  
  # Must implement real-time dashboard with:
  # - Cluster topology visualization
  # - Node health monitoring
  # - Distributed sandbox management
  # - Cross-node process inspection
  # - Real-time message tracing
  # - Performance metrics across cluster
  
  # Must integrate with existing LiveView components
  # Must use Phoenix PubSub for real-time updates
end
```

#### 4.2 Cluster Telemetry System

Create `lib/otp_supervisor/distributed/cluster_telemetry.ex`:

```elixir
defmodule OTPSupervisor.Distributed.ClusterTelemetry do
  @moduledoc """
  Cluster-wide telemetry collection and distribution.
  
  Collects metrics from all nodes and provides unified telemetry
  for monitoring, alerting, and performance analysis.
  """
  
  use GenServer
  require Logger
  
  # Must implement telemetry collection:
  # - collect_cluster_metrics/0
  # - distribute_telemetry_events/1
  # - aggregate_node_metrics/0
  # - detect_performance_anomalies/0
  # - generate_cluster_reports/0
  
  # Must integrate with existing analytics server
  # Must provide real-time metrics for dashboard
end
```

## Integration Requirements

### 1. Application Supervisor Integration

Update `lib/otp_supervisor/application.ex`:

```elixir
def start(_type, _args) do
  children = [
    # ... existing children
    
    # Add distributed clustering components
    OTPSupervisor.Distributed.ClusterSupervisor,
    
    # ... rest of children
  ]
  
  opts = [strategy: :one_for_one, name: OtpSupervisor.Supervisor]
  Supervisor.start_link(children, opts)
end
```

### 2. Router Integration

Update `lib/otp_supervisor_web/router.ex`:

```elixir
scope "/api/v1", OtpSupervisorWeb.Api.V1 do
  pipe_through :api
  
  # ... existing routes
  
  # Distributed cluster routes
  get "/cluster/status", ClusterController, :status
  get "/cluster/nodes", ClusterController, :nodes
  get "/cluster/processes", ClusterController, :processes
  get "/cluster/sandboxes", ClusterController, :sandboxes
  post "/cluster/sandboxes", ClusterController, :create_sandbox
  put "/cluster/sandboxes/:id/migrate", ClusterController, :migrate_sandbox
  delete "/cluster/sandboxes/:id", ClusterController, :destroy_sandbox
  get "/cluster/health", ClusterController, :health
  post "/cluster/trace/start", ClusterController, :start_trace
  delete "/cluster/trace/:id", ClusterController, :stop_trace
end

# Add distributed dashboard route
live "/distributed", DistributedDashboardLive, :index
```

### 3. Existing Module Integration

Your distributed modules MUST integrate seamlessly with existing modules:

```elixir
# Example: Extend existing Control module
defmodule OTPSupervisor.Core.Control do
  # ... existing functions
  
  # Add distributed capabilities
  def list_cluster_processes(opts \\ []) do
    case Application.get_env(:otp_supervisor, :distributed_mode, false) do
      true -> 
        OTPSupervisor.Distributed.ProcessInspector.list_all_cluster_processes(opts)
      false -> 
        list_all_processes(opts)
    end
  end
  
  def get_cluster_supervision_tree(supervisor_name) do
    case Application.get_env(:otp_supervisor, :distributed_mode, false) do
      true ->
        OTPSupervisor.Distributed.ProcessInspector.get_cluster_supervision_tree(supervisor_name)
      false ->
        get_supervision_tree(supervisor_name)
    end
  end
end
```

## Data Structures

### Cluster Node Information

```elixir
defmodule OTPSupervisor.Distributed.NodeInfo do
  @enforce_keys [:name, :status, :joined_at]
  defstruct [
    :name,              # Node name (atom)
    :status,            # :healthy, :degraded, :unreachable
    :joined_at,         # When node joined cluster
    :last_seen,         # Last health check timestamp
    :capabilities,      # List of node capabilities
    :resource_usage,    # Current resource utilization
    :sandbox_count,     # Number of sandboxes on node
    :process_count,     # Number of processes on node
    :memory_usage,      # Memory utilization
    :cpu_usage,         # CPU utilization
    :network_latency,   # Network latency to this node
    :version_info       # Node version information
  ]
end
```

### Distributed Sandbox Information

```elixir
defmodule OTPSupervisor.Distributed.SandboxInfo do
  @enforce_keys [:id, :node, :status]
  defstruct [
    :id,                # Sandbox ID
    :node,              # Node where sandbox is running
    :status,            # :running, :stopped, :migrating, :error
    :created_at,        # Creation timestamp
    :resource_usage,    # Current resource usage
    :migration_history, # History of node migrations
    :health_status,     # Health check results
    :dependencies,      # Inter-sandbox dependencies
    :backup_nodes       # Potential backup nodes for migration
  ]
end
```

### Cluster Health Status

```elixir
defmodule OTPSupervisor.Distributed.ClusterHealth do
  @enforce_keys [:overall_status, :node_health]
  defstruct [
    :overall_status,    # :healthy, :degraded, :critical
    :node_health,       # Map of node -> health status
    :partition_status,  # Network partition information
    :performance_metrics, # Cluster-wide performance data
    :alerts,            # Active alerts and warnings
    :recommendations,   # Automated recommendations
    :last_updated       # Last health check timestamp
  ]
end
```

## Testing Requirements

### 1. Distributed Integration Tests

Create comprehensive integration tests:

- `test/otp_supervisor/distributed/cluster_manager_test.exs`
- `test/otp_supervisor/distributed/sandbox_manager_test.exs`
- `test/otp_supervisor/distributed/process_inspector_test.exs`
- `test/otp_supervisor/distributed/message_tracer_test.exs`
- `test/otp_supervisor/distributed/health_monitor_test.exs`

### 2. Multi-Node Testing

Create multi-node test scenarios:

```elixir
# test/support/cluster_test_helper.ex
defmodule ClusterTestHelper do
  def setup_test_cluster do
    # Start multiple nodes for testing
    # Configure test cluster topology
    # Provide helper functions for multi-node testing
  end
  
  def simulate_node_failure(node) do
    # Simulate node failures for testing
  end
  
  def simulate_network_partition(nodes) do
    # Simulate network partitions for testing
  end
end
```

### 3. Performance Testing

Create performance benchmarks:

- Cluster formation time
- Cross-node operation latency
- Sandbox migration performance
- Message tracing overhead
- Dashboard update frequency

## Success Criteria

### Functional Requirements

1. **Cluster Formation**: Automatic cluster formation with 2+ nodes
2. **Distributed Sandboxes**: Create and manage sandboxes across cluster nodes
3. **Cross-Node Debugging**: Inspect and debug processes on any cluster node
4. **Message Tracing**: Trace messages across node boundaries
5. **Health Monitoring**: Real-time cluster health monitoring and alerting
6. **Load Balancing**: Intelligent sandbox placement based on node resources
7. **Fault Tolerance**: Graceful handling of node failures and network partitions
8. **Migration**: Seamless sandbox migration between nodes

### Technical Requirements

1. **Performance**: Cross-node operations complete in <500ms
2. **Scalability**: Support 10+ node clusters
3. **Reliability**: 99.9% uptime with automatic failover
4. **Integration**: Seamless integration with existing codebase
5. **API Compatibility**: All existing APIs work in distributed mode
6. **Real-time Updates**: Dashboard updates in <100ms
7. **Resource Efficiency**: <5% overhead for clustering

### Quality Requirements

1. **Code Quality**: Follow existing patterns and conventions
2. **OTP Compliance**: Use proper OTP supervision and fault tolerance
3. **Testing**: >95% test coverage for distributed components
4. **Documentation**: Complete @doc strings and architecture docs
5. **Monitoring**: Comprehensive telemetry and observability
6. **Security**: Secure inter-node communication
7. **Maintainability**: Clear separation of concerns and modularity

## Implementation Phases

### Phase 1: Foundation (Weeks 1-2)
- Cluster Manager with LibCluster integration
- Basic distributed sandbox management
- Health monitoring system
- Configuration and deployment setup

### Phase 2: Process Management (Weeks 3-4)
- Distributed process inspection
- Cross-node message tracing
- Distributed debugging capabilities
- Performance monitoring

### Phase 3: API Distribution (Weeks 5-6)
- Distributed Arsenal operations
- Cluster API controllers
- Load balancing and migration
- Advanced debugging features

### Phase 4: Visualization (Weeks 7-8)
- Real-time distributed dashboard
- Cluster telemetry system
- Performance analytics
- Alerting and notifications

## Deliverables

1. **Core Distributed Modules**: All distributed components implemented
2. **API Extensions**: Cluster-aware API endpoints
3. **Dashboard**: Real-time distributed cluster dashboard
4. **Test Suite**: Comprehensive multi-node testing
5. **Documentation**: Complete distributed architecture docs
6. **Scripts**: Enhanced cluster management scripts
7. **Configuration**: Production-ready cluster configuration

This foundation will establish the platform as the world's leading Elixir debugger with native clustering capabilities, providing unprecedented visibility and control over distributed Elixir systems.