# SANDBOX System V2 Documentation

## Overview

The SANDBOX system provides a robust, application-based process isolation mechanism for safely experimenting with OTP supervisors and processes. This system uses true OTP application lifecycle management to achieve superior isolation, resource management, and hot code reloading capabilities.

## Architecture

### Core Components

1. **SandboxManager** (`lib/otp_supervisor/core/sandbox_manager.ex`)
   - GenServer that manages sandbox lifecycle
   - Maintains ETS table for fast sandbox lookup
   - Handles application loading, compilation, and monitoring

2. **Control API** (`lib/otp_supervisor/core/control.ex`)
   - High-level interface for sandbox operations
   - Integrates with analytics and monitoring systems
   - Provides consistent API for both direct modules and applications

3. **Sandbox Applications** (`sandbox/examples/`)
   - Complete OTP applications with proper structure
   - Independent compilation and loading
   - Isolated process trees and resource management

### Application-Based Isolation

The system uses **application-based isolation** instead of simple supervisor spawning:

```elixir
# Superior approach - True application isolation
{:ok, sandbox_info} = Control.create_sandbox(OtpSandbox.TestDemoSupervisor, strategy: :one_for_one)

# sandbox_info contains:
# - id: unique sandbox identifier
# - app_name: :otp_sandbox (or custom app)
# - supervisor_module: OtpSandbox.TestDemoSupervisor
# - app_pid: application master PID
# - supervisor_pid: supervisor within the application
# - opts: configuration options
# - restart_count: number of restarts
```

## Key Benefits

### 1. True Process Isolation
- Each sandbox runs in its own OTP application context
- Complete separation of process trees
- Independent crash domains

### 2. Hot Code Reloading
- Applications can be recompiled without affecting the host system
- Code changes are isolated to the sandbox environment
- Safe experimentation with code modifications

### 3. Resource Management
- Applications have their own supervision trees
- Independent configuration and environment variables
- Proper cleanup on sandbox destruction

### 4. Monitoring and Analytics
- Each sandbox is monitored at the application level
- Integration with analytics system for failure tracking
- Comprehensive restart history and failure rate calculation

## API Reference

### Core Operations

#### Creating a Sandbox
```elixir
# Create with supervisor module
{:ok, info} = Control.create_sandbox(OtpSandbox.TestDemoSupervisor, strategy: :one_for_one)

# Create with application name
{:ok, info} = Control.create_sandbox(:otp_sandbox, supervisor_module: OtpSandbox.TestDemoSupervisor)
```

#### Managing Sandboxes
```elixir
# List all sandboxes
sandboxes = Control.list_sandboxes()

# Get sandbox information
{:ok, info} = Control.get_sandbox_info(sandbox_id)

# Restart sandbox (preserves configuration)
{:ok, restarted_info} = Control.restart_sandbox(sandbox_id)

# Destroy sandbox
:ok = Control.destroy_sandbox(sandbox_id)
```

#### Analytics Integration
```elixir
# Get restart history
{:ok, history} = Control.get_restart_history(supervisor_pid)

# Get failure rate
{:ok, rate} = Control.get_failure_rate(supervisor_pid, time_window_ms)

# Get system-wide analytics
stats = Control.get_supervisor_analytics()
```

## Implementation Details

### Sandbox Lifecycle

1. **Creation Phase**
   - Parse module or application parameter
   - Resolve sandbox application path
   - Compile application if needed
   - Load and start OTP application
   - Start supervisor within application context
   - Monitor application master process

2. **Runtime Phase**
   - Application runs independently
   - Supervisor manages its children
   - Analytics system tracks events
   - ETS table provides fast lookups

3. **Cleanup Phase**
   - Stop application gracefully
   - Unload application code
   - Remove from monitoring
   - Clean up ETS entries

### Path Resolution and Compilation

The system automatically handles:
- Sandbox application discovery in `sandbox/examples/`
- Automatic compilation using `mix compile`
- Code path management for lib and ebin directories
- Application file (.app) validation

```elixir
# Example structure
sandbox/examples/otp_sandbox/
├── lib/
│   └── otp_sandbox/
│       ├── test_demo_supervisor.ex
│       └── workers/
├── mix.exs
└── _build/dev/lib/otp_sandbox/ebin/
    └── otp_sandbox.app
```

### Error Handling

The system provides comprehensive error handling:
- Application loading failures
- Compilation errors
- Supervisor startup failures
- Process monitoring and cleanup

## Testing Integration

### Test Structure
```elixir
defmodule SandboxIntegrationTest do
  use ExUnit.Case, async: false
  @moduletag :ui  # Tagged as UI test

  test "can create and destroy sandbox" do
    {:ok, sandbox_info} = Control.create_sandbox(OtpSandbox.TestDemoSupervisor, strategy: :one_for_one)
    
    # Verify sandbox structure
    assert is_binary(sandbox_info.id)
    assert sandbox_info.app_name == :otp_sandbox
    assert Process.alive?(sandbox_info.app_pid)
    
    # Cleanup
    :ok = Control.destroy_sandbox(sandbox_info.id)
  end
end
```

### Test Categories
- **Integration Tests**: Full sandbox lifecycle testing
- **Isolation Tests**: Multi-sandbox independence verification
- **Resilience Tests**: Crash recovery and cleanup validation
- **Analytics Tests**: Monitoring and telemetry integration

## Advantages Over Simple Supervisor Spawning

1. **True Isolation**: Applications provide complete process isolation
2. **Code Safety**: Hot code reloading without affecting host system
3. **Resource Management**: Independent supervision trees and configurations
4. **Monitoring**: Application-level monitoring and analytics
5. **Cleanup**: Proper resource cleanup on destruction
6. **Scalability**: Better resource utilization and process management

## Future Enhancements

### Planned Features
- Configuration hot-swapping
- Sandbox templates and presets
- Resource usage monitoring
- Performance benchmarking integration
- Multi-node sandbox distribution

### Extensibility Points
- Custom application templates
- Plugin system for sandbox behaviors
- Integration with external monitoring systems
- Advanced analytics and reporting

## Best Practices

### Development
1. Always use the Control API for sandbox operations
2. Handle sandbox creation errors gracefully
3. Monitor sandbox health using analytics functions
4. Clean up sandboxes properly in tests and production

### Testing
1. Use `async: false` for sandbox integration tests
2. Tag sandbox tests with `@moduletag :ui` for exclusion
3. Use `ExUnit.CaptureLog` for expected warning suppression
4. Verify process isolation between sandboxes

### Production
1. Monitor sandbox resource usage
2. Set appropriate timeouts for sandbox operations
3. Use restart policies for critical sandboxes
4. Implement proper logging and alerting

## Conclusion

The SANDBOX system provides a powerful, application-based approach to process isolation and experimentation. By leveraging OTP application lifecycle management, it achieves superior isolation, resource management, and monitoring capabilities compared to simple supervisor spawning approaches.

The system is designed for both educational purposes (understanding OTP principles) and production use (safe experimentation and testing), making it a valuable tool for Elixir/OTP development and learning.