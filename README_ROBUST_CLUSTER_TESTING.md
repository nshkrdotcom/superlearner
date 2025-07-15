# Robust Mix Cluster Testing

A comprehensive solution for reliable distributed testing in WSL and other development environments.

## Overview

This system provides automated cluster management for distributed Elixir tests, eliminating the manual setup and teardown of test nodes while ensuring reliable networking and port management across different environments.

## Key Features

### 🌐 WSL Compatibility
- **Smart Hostname Resolution**: Automatically detects and uses the best hostname strategy (system hostname → localhost → 127.0.0.1)
- **Dynamic Port Management**: Finds available ports automatically and handles conflicts gracefully
- **WSL-Specific Error Guidance**: Provides actionable solutions for common WSL networking issues

### 🔧 Intelligent Error Handling
- **Comprehensive Diagnostics**: Detailed error analysis with specific remediation steps
- **Prerequisite Checking**: Validates environment before attempting cluster startup
- **Actionable Solutions**: Every error includes specific commands to resolve issues

### 🚀 Automated Lifecycle Management
- **Dynamic Port Allocation**: No more hardcoded ports or conflicts
- **Reliable Cleanup**: Thorough cleanup of processes, ports, and test artifacts
- **Graceful Failure Handling**: Robust error recovery and resource cleanup

## Quick Start

### Environment Check
```bash
# Verify your environment is ready for distributed testing
mix cluster.test preflight
```

### Manual Cluster Management
```bash
# Start a test cluster
mix cluster.test start

# Check cluster status
mix cluster.test status

# Run tests with cluster
mix test --include distributed

# Clean up everything
mix cluster.test clean
```

### Automated Test Integration
```bash
# Run distributed tests with automatic cluster management
mix cluster.test run

# Or use the integrated test workflow (coming soon)
mix test --distributed
```

## Architecture

### Core Components

```
┌─────────────────────────────────────────────────────────────────┐
│                    Mix.Tasks.Cluster.Test                       │
│                    (Enhanced error handling)                    │
├─────────────────────────────────────────────────────────────────┤
│                 TestCluster.Manager                             │
│                 (WSL-compatible cluster management)             │
├─────────────────────────────────────────────────────────────────┤
│  HostnameResolver │  PortManager     │  Diagnostics            │
│  - WSL fallback   │  - Dynamic ports │  - Error analysis       │
│  - Consistency    │  - Conflict res. │  - Actionable guidance   │
│  - Validation     │  - Cleanup       │  - Environment checks    │
└─────────────────────────────────────────────────────────────────┘
```

### Hostname Resolution Strategy
1. **System Hostname**: Try the actual system hostname first
2. **Localhost Fallback**: Fall back to localhost if system hostname fails
3. **IP Fallback**: Use 127.0.0.1 as last resort
4. **Validation**: Ensure chosen hostname actually resolves

### Port Management Strategy
1. **Dynamic Allocation**: Find available ports starting from base ranges
2. **Conflict Detection**: Check if ports are actually free before use
3. **Cleanup Integration**: Track and clean up ports used by test processes
4. **Range Management**: Use separate ranges for HTTP and distribution ports

## Usage Examples

### Basic Distributed Test
```elixir
defmodule MyDistributedTest do
  use ExUnit.Case
  
  @tag :distributed
  test "distributed functionality" do
    # Test will automatically have cluster available
    nodes = [Node.self() | Node.list()]
    assert length(nodes) > 1
  end
end
```

### Using ClusterTestHelper
```elixir
defmodule MyClusterTest do
  use ExUnit.Case
  
  setup do
    # Enhanced helper with WSL compatibility
    {:ok, nodes} = ClusterTestHelper.start_test_cluster(2)
    
    on_exit(fn ->
      ClusterTestHelper.stop_test_cluster(nodes)
    end)
    
    {:ok, nodes: nodes}
  end
  
  test "cluster operations", %{nodes: nodes} do
    assert length(nodes) == 2
    # Your distributed test logic here
  end
end
```

## Configuration

### Environment Variables
```bash
# Override default port ranges
export TEST_HTTP_PORT_BASE=5000
export TEST_DIST_PORT_BASE=10000

# Force specific hostname
export TEST_CLUSTER_HOSTNAME=localhost

# Enable debug logging
export TEST_CLUSTER_DEBUG=true
```

### Mix Configuration
```elixir
# config/test.exs
config :otp_supervisor, :test_cluster,
  node_count: 3,
  http_port_base: 4100,
  dist_port_base: 9100,
  startup_timeout: 30_000,
  cleanup_timeout: 10_000
```

## Troubleshooting

### Common Issues

#### "Ports are in use"
```bash
# Clean up test processes
mix cluster.test clean

# Check what's using the ports
netstat -tulpn | grep :4100

# Kill specific processes
pkill -f test_node
```

#### "Cannot resolve hostname"
```bash
# Test hostname resolution
ping localhost
ping 127.0.0.1

# Restart DNS resolver (WSL)
sudo systemctl restart systemd-resolved
```

#### "Cannot connect to node"
```bash
# Check EPMD
epmd -names

# Start EPMD if needed
epmd -daemon

# Verify hostname resolution
ping $(hostname)
```

### WSL-Specific Issues

#### Hostname Resolution
WSL sometimes has issues with hostname resolution. The system automatically handles this by trying multiple strategies, but you can force a specific hostname:

```bash
export TEST_CLUSTER_HOSTNAME=localhost
mix cluster.test start
```

#### Port Conflicts
WSL can have port conflicts with Windows processes. Use the cleanup command:

```bash
mix cluster.test clean
```

#### Network Interface Issues
If you see network interface errors:

```bash
# Check network interfaces
ip addr show

# Restart networking (if needed)
sudo systemctl restart systemd-networkd
```

## Performance

### Benchmarks
- **Hostname Resolution**: < 1ms per call
- **Port Allocation**: < 100ms for 5 nodes  
- **Cluster Startup**: < 30s for typical clusters
- **Cleanup**: < 1s comprehensive cleanup

### Optimization Tips
1. **Reuse Clusters**: Use `mix cluster.test run` for multiple test runs
2. **Parallel Tests**: The system handles concurrent test execution
3. **Resource Limits**: Adjust node count based on system resources

## Development

### Running Tests
```bash
# Run all cluster testing tests
mix test test/otp_supervisor/test_cluster/

# Run integration tests
mix test --include integration

# Run validation tests
mix test --include validation
```

### Adding New Features
1. **Hostname Strategies**: Add new resolution methods in `HostnameResolver`
2. **Port Strategies**: Extend port allocation in `PortManager`
3. **Diagnostics**: Add new error patterns in `Diagnostics`

## API Reference

### HostnameResolver
```elixir
# Get the best hostname for cluster operations
{:ok, hostname} = HostnameResolver.get_cluster_hostname()

# Test if a hostname resolves
{:ok, hostname} = HostnameResolver.test_hostname_resolution("localhost")
```

### PortManager
```elixir
# Find available port pairs for nodes
{:ok, port_pairs} = PortManager.find_available_ports(3)

# Check if a port is free
true = PortManager.port_free?(4100)

# Clean up ports
:ok = PortManager.cleanup_ports(port_pairs)
```

### Diagnostics
```elixir
# Check prerequisites
:ok = Diagnostics.check_prerequisites()

# Diagnose startup failures
diagnosis = Diagnostics.diagnose_startup_failure(error)
IO.puts(diagnosis.problem)
Enum.each(diagnosis.solutions, &IO.puts("• #{&1}"))

# Get comprehensive environment report
report = Diagnostics.environment_report()
```

### ClusterTestHelper
```elixir
# Start test nodes
{:ok, node} = ClusterTestHelper.start_test_node("my_test")
{:ok, nodes} = ClusterTestHelper.start_test_cluster(3)

# Wait for cluster formation
:ok = ClusterTestHelper.wait_for_cluster(3, 10_000)

# Clean up
ClusterTestHelper.stop_test_cluster(nodes)
ClusterTestHelper.cleanup_all_test_resources()
```

## Contributing

### Testing Your Changes
1. Run the validation suite: `mix test --include validation`
2. Test WSL compatibility: `mix cluster.test preflight`
3. Verify cleanup: `mix cluster.test clean`

### Adding Tests
- Unit tests: `test/otp_supervisor/test_cluster/`
- Integration tests: Use `@tag :integration`
- Validation tests: Use `@tag :validation`

## License

This robust cluster testing system is part of the OTP Supervisor project and follows the same license terms.