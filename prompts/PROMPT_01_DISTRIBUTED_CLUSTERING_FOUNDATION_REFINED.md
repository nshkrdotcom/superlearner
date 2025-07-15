# PROMPT 01: Distributed Clustering Foundation - World-Class Elixir Debugger

## Context and Vision

You are building the **world's leading Elixir debugger** with native distributed clustering capabilities. This is not an educational platform - it's a production-grade debugging tool that will revolutionize how developers debug distributed Elixir systems.

The platform already has a solid foundation with comprehensive OTP introspection, sandbox management, and real-time web interfaces. Now we're adding distributed clustering to enable cross-node debugging, distributed sandbox management, and cluster-wide monitoring.

## Required Reading

Before starting, you MUST read and understand these files:

### Core Architecture Files
- `lib/otp_supervisor/core/control.ex` - Core debugging API with process management
- `lib/otp_supervisor/core/sandbox_manager.ex` - Isolated sandbox management
- `lib/otp_supervisor/application.ex` - Current application structure
- `DISTRIBUTED_CLUSTERING_INVESTIGATION.md` - Comprehensive analysis and architecture

### Configuration Files
- `config/dev.exs` - Node 1 configuration
- `config/dev2.exs` - Node 2 configuration
- `mix.exs` - Current dependencies

### Existing Clustering Work
- `WSL_CLUSTER_SETUP.md` - Current cluster setup documentation
- `scripts/start_node1.sh` - Node 1 startup script
- `scripts/start_node2.sh` - Node 2 startup script
- `test_cluster.exs` - Basic cluster connectivity test

## Success Criteria (90% Probability Target)

### Phase 1: Foundation (This Prompt)
1. **Automatic Cluster Formation**: Two nodes connect within 10 seconds of startup
2. **Distributed Registry**: Global process registration works across nodes
3. **Cross-Node Communication**: Basic RPC calls succeed with proper error handling
4. **Event System**: Node join/leave events are captured and broadcast
5. **Health Monitoring**: Basic cluster health status is available

### Measurable Success Metrics
- Cluster formation time: < 10 seconds
- RPC success rate: > 95%
- Node failure detection: < 5 seconds
- Memory overhead: < 50MB per node
- Zero crashes during normal operation

## Implementation Requirements

### 1. Dependencies Addition
Add to `mix.exs`:
```elixir
{:libcluster, "~> 3.3"},
{:horde, "~> 0.8.0"}
```

### 2. Core Files to Create/Modify

#### A. Create `lib/otp_supervisor/distributed/cluster_supervisor.ex`
- Supervises LibCluster and Horde components
- Handles cluster topology configuration
- Manages distributed registry and supervisor

#### B. Create `lib/otp_supervisor/distributed/cluster_event_handler.ex`
- Monitors node up/down events
- Broadcasts cluster changes via Phoenix.PubSub
- Maintains cluster state

#### C. Create `lib/otp_supervisor/distributed/control.ex`
- Extends core Control module for distributed operations
- Implements cross-node process listing
- Provides remote process management

#### D. Modify `lib/otp_supervisor/application.ex`
- Add ClusterSupervisor to supervision tree
- Ensure proper startup order

#### E. Update `config/config.exs`
- Add LibCluster topology configuration
- Configure Horde settings

### 3. Configuration Requirements

#### LibCluster Configuration
```elixir
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

#### Node-Specific Environment Variables
- Use existing NODE_ROLE environment variable
- Ensure consistent Erlang cookies
- Proper port configuration

### 4. Testing Requirements

#### A. Create `test/distributed/cluster_formation_test.exs`
- Test automatic cluster formation
- Verify node discovery
- Test reconnection after network issues

#### B. Create `test/distributed/distributed_control_test.exs`
- Test cross-node process listing
- Test remote process management
- Test error handling for unreachable nodes

#### C. Update `test_cluster.exs`
- Add Horde registry tests
- Add distributed supervisor tests
- Add cluster event tests

## Detailed Implementation Guide

### Step 1: Dependencies and Configuration
1. Add LibCluster and Horde to `mix.exs`
2. Update `config/config.exs` with LibCluster topology
3. Ensure node-specific configurations are correct

### Step 2: Core Distributed Components
1. Implement `ClusterSupervisor` with proper supervision tree
2. Implement `ClusterEventHandler` for node monitoring
3. Create basic `Distributed.Control` module

### Step 3: Application Integration
1. Add `ClusterSupervisor` to main application supervision tree
2. Ensure proper startup order (PubSub before ClusterSupervisor)
3. Test basic cluster formation

### Step 4: Cross-Node Operations
1. Implement `list_cluster_processes/1`
2. Implement `kill_remote_process/2`
3. Implement `get_cluster_status/0`

### Step 5: Testing and Validation
1. Create comprehensive test suite
2. Test with both nodes running
3. Test node failure scenarios
4. Validate performance metrics

## Error Handling Requirements

### Network Issues
- Graceful handling of RPC timeouts
- Automatic reconnection attempts
- Fallback to single-node operation

### Node Failures
- Proper cleanup of dead node references
- Event broadcasting for node failures
- Health status updates

### Process Failures
- Horde automatic process migration
- Supervisor restart strategies
- Error logging and monitoring

## Performance Requirements

### Memory Usage
- Minimal overhead for clustering components
- Efficient process registry
- Bounded event history

### Network Traffic
- Configurable polling intervals
- Efficient RPC calls
- Minimal heartbeat overhead

### Latency
- Sub-second cross-node operations
- Fast cluster formation
- Responsive health monitoring

## Integration Points

### Existing Systems
- **Control Module**: Extend for distributed operations
- **SandboxManager**: Prepare for distributed sandboxes (future)
- **Arsenal API**: Prepare for distributed endpoints (future)
- **LiveView**: Prepare for cluster monitoring (future)

### Phoenix.PubSub Integration
- Use existing PubSub for cluster events
- Broadcast node status changes
- Enable real-time cluster monitoring

## Validation Checklist

Before considering this prompt complete, verify:

- [ ] Both nodes start and connect automatically
- [ ] `OTPSupervisor.Distributed.Control.get_cluster_status()` returns correct info
- [ ] `OTPSupervisor.Distributed.Control.list_cluster_processes()` works
- [ ] Node failure is detected and handled gracefully
- [ ] All tests pass
- [ ] No memory leaks or performance issues
- [ ] Error handling works for all edge cases

## Next Steps (Future Prompts)

1. **Distributed Sandbox Management**: Extend SandboxManager for cross-node operations
2. **Arsenal API Distribution**: Add distributed endpoints to Arsenal framework
3. **Cluster Monitoring Dashboard**: Real-time cluster visualization
4. **Advanced Features**: Load balancing, automatic failover, performance optimization

## Code Quality Requirements

- Comprehensive documentation for all modules
- Proper error handling with meaningful messages
- Consistent logging at appropriate levels
- Type specs for all public functions
- Unit tests with >90% coverage
- Integration tests for distributed scenarios

## Success Validation Commands

After implementation, these commands should work:

```elixir
# Start both nodes, then in either node:

# Check cluster status
OTPSupervisor.Distributed.Control.get_cluster_status()

# List processes across cluster
OTPSupervisor.Distributed.Control.list_cluster_processes()

# Test Horde registry
{:ok, _} = Horde.Registry.register(OTPSupervisor.Distributed.Registry, :test_key, "test_value")
Horde.Registry.lookup(OTPSupervisor.Distributed.Registry, :test_key)

# Test cluster events (should see events when nodes join/leave)
Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "cluster_events")
```

This prompt is designed for 90%+ success probability by:
- Building on existing, proven architecture
- Providing specific, measurable success criteria
- Including comprehensive error handling
- Offering detailed implementation guidance
- Requiring thorough testing and validation
- Focusing on minimal viable functionality first