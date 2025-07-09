# Sandbox Architecture Documentation

## Overview

The Sandbox system provides isolated OTP application environments for safe experimentation, debugging, and live development. Each sandbox runs as a separate OTP application with its own supervision tree, allowing developers to create, modify, and destroy processes without affecting the main system.

## Core Components

### 1. Sandbox Manager (`lib/otp_supervisor/core/sandbox_manager.ex`)

The central coordinator for all sandbox operations, providing:

#### Dynamic Application Management
- **Application Lifecycle**: Create, start, stop, restart, and destroy sandbox applications
- **Process Isolation**: Each sandbox runs in its own OTP application boundary
- **Unique Identification**: Auto-generated sandbox IDs with restart counters
- **Code Path Management**: Dynamic loading of sandbox-specific modules

#### Key Functions
```elixir
create_sandbox(name, opts \\ [])     # Create new sandbox application
destroy_sandbox(sandbox_id)         # Clean shutdown and removal
restart_sandbox(sandbox_id)         # Full restart with state preservation
list_sandboxes()                     # All active sandboxes with metadata
get_sandbox_info(sandbox_id)        # Detailed sandbox information
```

#### ETS-based Process Registry
- **Fast Lookups**: O(1) process discovery by PID or name
- **Metadata Storage**: Process types, relationships, and custom data
- **Automatic Cleanup**: Registry cleanup when processes terminate
- **Cross-Reference**: Bidirectional PID ↔ sandbox_id mapping

### 2. Control System (`lib/otp_supervisor/core/control.ex`)

Advanced process management and introspection capabilities:

#### Process Lifecycle Control
```elixir
start_process(sandbox_id, module, args)    # Start new process in sandbox
kill_process(pid, reason)                  # Safe process termination
restart_process(pid)                       # Process restart with state preservation
```

#### Supervision Tree Inspection
- **Hierarchy Traversal**: Complete supervision tree walking
- **Process Classification**: Automatic identification of supervisors, GenServers, workers
- **Relationship Mapping**: Links, monitors, and dependency graphs
- **State Extraction**: GenServer state retrieval via `:sys.get_state/2`

#### Process Analytics
- **Performance Metrics**: Process restart history, failure rates, message queue lengths
- **Health Monitoring**: Process responsiveness and resource usage
- **Dependency Analysis**: Process relationship graphs and circular dependency detection

### 3. Arsenal Operation Framework (`lib/otp_supervisor/core/arsenal.ex`)

Metaprogramming framework for extensible operations:

#### Protocol-Driven Architecture
```elixir
defprotocol Arsenal.Operation do
  @doc "Execute the operation with given parameters"
  def execute(operation, params)
  
  @doc "Validate operation parameters"
  def validate(operation, params)
  
  @doc "Generate HTTP route for operation"
  def route(operation)
end
```

#### Current Operations
- **`TraceProcess`**: Advanced message tracing with session management
- **`KillProcess`**: Safe process termination with critical process protection
- **`SendMessage`**: Multi-mode message sending (send, cast, call)
- **`GetProcessInfo`**: Comprehensive process information retrieval
- **`ListSupervisors`**: Paginated supervisor discovery

#### Dynamic API Generation
- **Runtime Routing**: Automatic HTTP endpoint generation from operations
- **Parameter Validation**: Multi-level validation pipeline
- **Response Formatting**: Consistent JSON API responses
- **Error Handling**: Standardized error responses with operation context

### 4. Message Tracing System (`lib/otp_supervisor/core/message_tracer.ex`)

Real-time debugging and monitoring infrastructure:

#### Message Flow Analysis
- **Bidirectional Tracing**: Capture incoming and outgoing messages
- **Pattern Recognition**: Message frequency and timing analysis
- **Selective Filtering**: Trace specific message types or patterns
- **Session Management**: Multiple concurrent tracing sessions

#### Registry-based Tracking
- **Tracer Process Management**: Automatic tracer lifecycle management
- **Resource Cleanup**: Automatic cleanup on target process termination
- **Performance Optimization**: Minimal overhead tracing implementation

## Sandbox Lifecycle

### Creation Flow
1. **Generate Unique ID**: Timestamp-based with restart counter
2. **Create Application Module**: Dynamic module generation with supervision tree
3. **Register Application**: Add to OTP application controller
4. **Start Application**: Launch with configured parameters
5. **Initialize Registry**: Set up ETS tables for process tracking
6. **Return Metadata**: Sandbox ID, PID, and configuration

### Process Management
1. **Process Registration**: Automatic registration in sandbox registry
2. **Metadata Tracking**: Process type, start time, relationships
3. **Monitoring Setup**: Process monitoring for cleanup and analytics
4. **State Management**: Optional state persistence and recovery

### Cleanup Flow
1. **Stop Application**: Graceful application shutdown
2. **Process Termination**: Supervised process cleanup
3. **Registry Cleanup**: Remove ETS entries
4. **Unregister Application**: Remove from OTP application controller
5. **Resource Cleanup**: Memory and handle cleanup

## Safety Mechanisms

### Process Protection
- **Critical Process Detection**: Prevent termination of essential system processes
- **Sandbox Isolation**: Prevent cross-sandbox interference
- **Resource Limits**: Configurable limits on process count and memory usage
- **Graceful Degradation**: Fallback mechanisms for failure scenarios

### Error Handling
- **Supervision Strategy**: Robust supervision trees with appropriate restart strategies
- **Error Propagation**: Controlled error propagation with circuit breakers
- **Recovery Mechanisms**: Automatic recovery from transient failures
- **Audit Logging**: Comprehensive logging of all sandbox operations

## Development Patterns

### Hot Code Reloading
```elixir
# Load new module version in sandbox
SandboxManager.load_module(sandbox_id, module_source)

# Update running processes with new code
Control.update_process_code(pid, new_module)
```

### State Manipulation
```elixir
# Extract current GenServer state
state = Control.get_process_state(pid)

# Modify and restore state
new_state = update_state_function.(state)
Control.set_process_state(pid, new_state)
```

### Dynamic Process Creation
```elixir
# Create worker from specification
worker_spec = %{
  id: :dynamic_worker,
  start: {MyWorker, :start_link, [args]},
  restart: :temporary
}

Control.start_dynamic_child(supervisor_pid, worker_spec)
```

## Integration Points

### REST API Endpoints
```
POST   /api/v1/sandboxes                    # Create sandbox
DELETE /api/v1/sandboxes/:id               # Destroy sandbox
GET    /api/v1/sandboxes/:id/processes     # List sandbox processes
POST   /api/v1/sandboxes/:id/operations    # Execute operations
```

### WebSocket Connections
```
/api/v1/sandboxes/:id/live                 # Live updates and REPL
/api/v1/processes/:pid/trace               # Real-time message tracing
```

### Programmatic Interface
```elixir
# Direct Elixir API usage
{:ok, sandbox_id} = SandboxManager.create_sandbox("test_app")
{:ok, pid} = Control.start_process(sandbox_id, Counter, [0])
:ok = Arsenal.execute(TraceProcess, %{pid: pid, duration: 5000})
```

## Performance Characteristics

### Scaling Limits
- **Concurrent Sandboxes**: 100+ sandboxes on modern hardware
- **Processes per Sandbox**: Limited by BEAM process limits (~134M processes)
- **Message Throughput**: Minimal overhead for traced processes
- **Memory Usage**: ~1MB base overhead per sandbox

### Optimization Strategies
- **ETS Registry**: O(1) lookups for process discovery
- **Lazy Loading**: On-demand module loading and compilation
- **Process Pooling**: Reuse of common worker processes
- **Selective Tracing**: Targeted tracing to minimize overhead

## Security Considerations

### Isolation Boundaries
- **Application Isolation**: Each sandbox runs in separate OTP application
- **Process Isolation**: Standard BEAM process isolation
- **Code Isolation**: Separate code paths and module namespaces
- **Resource Isolation**: Configurable resource limits per sandbox

### Access Control
- **Operation Permissions**: Role-based operation access control
- **Sandbox Ownership**: User-based sandbox access control
- **API Authentication**: Token-based API access
- **Audit Logging**: Complete operation audit trail

## Future Enhancements

### Planned Features
- **Persistent Sandboxes**: Disk persistence across system restarts
- **Sandbox Templates**: Pre-configured sandbox templates
- **Collaborative Debugging**: Multi-user sandbox access
- **Time Travel Debugging**: State checkpointing and rollback
- **Visual Process Trees**: GUI-based supervision tree editor

### Integration Roadmap
- **IDE Integration**: VS Code and Emacs extensions
- **CI/CD Integration**: Automated testing in sandbox environments
- **Production Debugging**: Safe production environment debugging
- **Performance Profiling**: Advanced performance analysis tools