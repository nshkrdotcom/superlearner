# Distributed Test Infrastructure Tooling

## Overview

The OTP Supervisor Educational Platform includes comprehensive distributed test infrastructure tooling that enables reliable automated testing of distributed functionality. This system addresses critical distributed Erlang startup issues and provides professional-grade cluster management capabilities.

## 🎯 Problem Solved

**Before:** Manual server management, unreliable distributed Erlang startup, risk of testing against stale code
**After:** Automated, reliable, isolated test cluster management with comprehensive error handling

### Critical Issues Addressed

- ✅ **Distributed Erlang Startup Failures** - `:nodistribution` and `:not_alive` errors
- ✅ **Network Configuration Issues** - 127.0.0.1 vs localhost binding problems  
- ✅ **Cookie Authentication Failures** - Inconsistent cookie management
- ✅ **EPMD Dependency Issues** - Erlang Port Mapper Daemon management
- ✅ **Stale Code Testing** - Prevents testing against old running servers
- ✅ **Port Conflicts** - Dedicated port management with conflict detection
- ✅ **Environment Isolation** - Clean test environments with proper cleanup

## 🚀 Quick Start

### Basic Usage

```bash
# Check cluster status
mix cluster.test status

# Start a test cluster
mix cluster.test start

# Run comprehensive health check
mix cluster.test health

# Run full test cycle (start → test → cleanup)
mix cluster.test run

# Stop cluster
mix cluster.test stop

# Clean up all artifacts
mix cluster.test clean
```

### Development Workflow

```bash
# Start development with fresh cluster
mix cluster.test start
mix test --only real_nodes

# Make code changes, restart with new code
mix cluster.test restart
mix test --only real_nodes

# Debug specific issues
mix cluster.test health
mix cluster.test logs

# Clean shutdown
mix cluster.test stop
```

## 📋 CLI Commands Reference

### Core Commands

| Command | Description | Example |
|---------|-------------|---------|
| `start` | Start distributed test cluster | `mix cluster.test start` |
| `stop` | Stop test cluster | `mix cluster.test stop` |
| `restart` | Restart cluster with fresh code | `mix cluster.test restart` |
| `status` | Show cluster status | `mix cluster.test status` |
| `health` | Comprehensive health check | `mix cluster.test health` |
| `run` | Full automated test cycle | `mix cluster.test run` |
| `clean` | Clean up all test artifacts | `mix cluster.test clean` |
| `logs` | Show cluster logs | `mix cluster.test logs [node]` |

### Advanced Usage

```bash
# Get help
mix cluster.test --help

# View logs from specific node
mix cluster.test logs node1

# Run with verbose output
mix cluster.test run --verbose

# Health check with detailed diagnostics
mix cluster.test health
```

## 🏗️ Architecture

### Core Components

```
┌─────────────────────────────────────────────────────────────┐
│                    CLI Tool (mix cluster.test)              │
├─────────────────────────────────────────────────────────────┤
│                  TestCluster.Manager                        │
│                    (GenServer)                              │
├─────────────────────────────────────────────────────────────┤
│  NodeProvisioner  │  HealthChecker  │    CodeSync          │
│  - Node startup   │  - Health tests │  - Code sync        │
│  - Error handling │  - Diagnostics  │  - Version check    │
│  - Retry logic    │  - Performance  │  - Hot reload       │
├─────────────────────────────────────────────────────────────┤
│              Test Nodes (Ports 4100-4102)                  │
│  test_node1@127.0.0.1  │  test_node2@127.0.0.1  │  ...    │
└─────────────────────────────────────────────────────────────┘
```

### Node Configuration

```elixir
@test_nodes %{
  node1: %{
    name: :"test_node1@127.0.0.1",
    http_port: 4100,
    dist_port: 9100,
    cookie: :test_cluster_cookie
  },
  node2: %{
    name: :"test_node2@127.0.0.1", 
    http_port: 4101,
    dist_port: 9101,
    cookie: :test_cluster_cookie
  }
}
```

## 🔧 Technical Features

### Robust Node Startup

The `NodeProvisioner` module handles all critical distributed Erlang startup issues:

```elixir
# Pre-flight environment validation
- EPMD daemon running and responsive
- Network configuration (127.0.0.1 vs localhost)
- Port availability (4100-4102, 9100-9199)
- Current node distributed mode
- Cookie authentication setup

# Startup with retry logic
- 3 retry attempts with exponential backoff
- Comprehensive error diagnostics
- Automatic cleanup on failures
- Detailed troubleshooting suggestions
```

### Health Monitoring

Comprehensive health checks validate:

- ✅ **Node Connectivity** - RPC calls and ping tests
- ✅ **Cluster Formation** - LibCluster integration
- ✅ **Code Synchronization** - Module version consistency
- ✅ **Arsenal Operations** - Distributed operation functionality
- ✅ **Performance Metrics** - Latency and resource usage
- ✅ **Resource Usage** - Memory and CPU monitoring

### Code Synchronization

Prevents testing against stale code:

- ✅ **Automatic Compilation** - Ensures fresh code builds
- ✅ **Version Validation** - Checks module consistency across nodes
- ✅ **Hot Code Reloading** - Updates running nodes with new code
- ✅ **Application Startup** - Ensures required apps are running

## 🧪 Integration with Test Suite

### Test Tags

Use these tags to control test execution:

```elixir
@tag :real_nodes          # Requires real multi-node cluster
@tag :simulation          # Uses SingleNodeSimulator
@tag :distributed         # General distributed functionality
```

### Test Helpers

The tooling integrates with existing test helpers:

```elixir
# In your test files
use OTPSupervisor.DistributedTestCase

test "my distributed feature" do
  # Test cluster is automatically managed
  assert_cluster_size(2)
  assert_arsenal_operation_succeeds(ClusterHealth, %{})
end
```

### Automated Test Execution

```bash
# Run all distributed tests with automatic cluster management
mix cluster.test run

# Equivalent to:
# mix cluster.test start
# mix test --only real_nodes  
# mix cluster.test stop
```

## 🔍 Troubleshooting

### Common Issues and Solutions

#### 1. Node Startup Failures

**Error:** `:nodistribution` or `:not_alive`

**Solution:**
```bash
# Check EPMD status
epmd -names

# Restart EPMD if needed
pkill epmd
epmd -daemon

# Try cluster startup
mix cluster.test start
```

#### 2. Port Conflicts

**Error:** `ports_unavailable: [4100, 4101]`

**Solution:**
```bash
# Check what's using the ports
netstat -an | grep 4100

# Kill processes using test ports
mix cluster.test clean

# Or manually kill processes
pkill -f test_node
```

#### 3. Network Configuration Issues

**Error:** Network validation failed

**Solution:**
```bash
# Test network connectivity
ping 127.0.0.1
nslookup localhost

# Check hostname resolution
hostname
nslookup $(hostname)
```

#### 4. Code Synchronization Issues

**Error:** Version mismatches detected

**Solution:**
```bash
# Force recompilation and restart
mix clean
mix compile
mix cluster.test restart
```

### Health Check Diagnostics

Run comprehensive diagnostics:

```bash
mix cluster.test health
```

Example output:
```
🏥 Overall Health: ✅ HEALTHY

  ✅ connectivity: Node connectivity and communication
  ✅ cluster_formation: LibCluster formation  
  ✅ code_sync: Code version synchronization
  ✅ arsenal_operations: Arsenal operations functionality
  ✅ performance: Performance and latency
  ✅ resource_usage: Resource usage
```

### Debug Logging

Enable debug logging for detailed diagnostics:

```bash
# Set log level
export LOG_LEVEL=debug

# Run with verbose output
mix cluster.test start
```

## 🔧 Configuration

### Environment Variables

```bash
# Test cluster configuration
export TEST_CLUSTER_COOKIE=test_cluster_cookie
export TEST_NODE_COUNT=2
export TEST_TIMEOUT=30000

# Network configuration  
export TEST_HOST=127.0.0.1
export TEST_PORT_BASE=4100
export TEST_DIST_PORT_BASE=9100
```

### Application Configuration

```elixir
# config/test.exs
config :otp_supervisor, :test_cluster,
  node_count: 2,
  timeout: 30_000,
  ports: %{
    http: 4100..4199,
    distribution: 9100..9199
  }
```

## 📊 Performance Characteristics

### Startup Times

- **Cluster startup:** < 10 seconds
- **Health check:** < 5 seconds  
- **Code synchronization:** < 3 seconds
- **Node provisioning:** < 5 seconds per node

### Resource Usage

- **Memory per node:** ~50-100MB
- **CPU usage:** < 5% during normal operation
- **Network latency:** < 1ms between local nodes
- **Disk usage:** Minimal (logs and temp files)

## 🚀 CI/CD Integration

### GitHub Actions Example

```yaml
name: Distributed Tests
on: [push, pull_request]

jobs:
  distributed-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Setup Elixir
        uses: erlef/setup-beam@v1
        with:
          elixir-version: '1.18.3'
          otp-version: '27'
      - name: Install dependencies
        run: mix deps.get
      - name: Run distributed tests
        run: mix cluster.test run --timeout 300
```

### Docker Integration

```dockerfile
# Dockerfile.test
FROM elixir:1.18.3-otp-27

# Install EPMD and networking tools
RUN apt-get update && apt-get install -y \
    net-tools \
    iputils-ping \
    && rm -rf /var/lib/apt/lists/*

# Start EPMD
RUN epmd -daemon

# Run tests
CMD ["mix", "cluster.test", "run"]
```

## 🔮 Advanced Features

### Custom Node Configurations

```elixir
# Start cluster with custom configuration
mix cluster.test start --nodes 3 --timeout 60000
```

### Performance Testing

```bash
# Run performance benchmarks
mix cluster.test health --benchmark

# Test with load
mix cluster.test start --load-test
```

### Network Partition Simulation

```bash
# Simulate network partitions
mix cluster.test partition node1 node2

# Heal partitions
mix cluster.test heal
```

## 📚 API Reference

### TestCluster.Manager

```elixir
# Start cluster programmatically
{:ok, nodes} = OTPSupervisor.TestCluster.Manager.start_cluster()

# Get cluster status
{:ok, status} = OTPSupervisor.TestCluster.Manager.get_status()

# Run health check
{:ok, results} = OTPSupervisor.TestCluster.Manager.health_check()

# Stop cluster
:ok = OTPSupervisor.TestCluster.Manager.stop_cluster()
```

### NodeProvisioner

```elixir
# Provision individual node
node_config = %{
  name: :"test_node@127.0.0.1",
  http_port: 4100,
  dist_port: 9100,
  cookie: :test_cluster_cookie
}

{:ok, node} = OTPSupervisor.TestCluster.NodeProvisioner.provision_node(node_config)
```

### HealthChecker

```elixir
# Run comprehensive health check
{:ok, results} = OTPSupervisor.TestCluster.HealthChecker.comprehensive_health_check(nodes)

# Run basic health check
{:ok, results} = OTPSupervisor.TestCluster.HealthChecker.basic_health_check(nodes)
```

## 🤝 Contributing

### Development Setup

```bash
# Clone and setup
git clone <repo>
cd otp-supervisor-educational-platform

# Install dependencies
mix deps.get

# Run tests
mix test

# Test distributed functionality
mix cluster.test run
```

### Adding New Features

1. **Extend CLI commands** in `lib/mix/tasks/cluster/test.ex`
2. **Add Manager functionality** in `lib/otp_supervisor/test_cluster/manager.ex`
3. **Enhance health checks** in `lib/otp_supervisor/test_cluster/health_checker.ex`
4. **Update documentation** in this README

### Testing Changes

```bash
# Test CLI functionality
mix cluster.test --help

# Test cluster operations
mix cluster.test start
mix cluster.test health
mix cluster.test stop

# Run full test suite
mix test
mix cluster.test run
```

## 📄 License

This distributed test infrastructure tooling is part of the OTP Supervisor Educational Platform and follows the same license terms.

## 🙏 Acknowledgments

This tooling addresses critical distributed Erlang issues identified through extensive analysis and testing. Special thanks to the Elixir community for distributed systems best practices and the OTP team for robust distributed Erlang foundations.

---

**Built with ❤️ for reliable distributed testing**