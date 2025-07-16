# N-Node Cluster Support

The superlearner cluster system now supports arbitrary numbers of nodes instead of being limited to 2 nodes. This document describes how to use the new functionality.

## Quick Start

### Using Mix Tasks

```bash
# Start a cluster with default size (2 nodes)
mix cluster.test start

# Start a cluster with 4 nodes
mix cluster.test start --size 4

# Run tests with a 3-node cluster
mix cluster.test run --size 3

# Restart with 5 nodes
mix cluster.test restart --size 5
```

### Using Shell Scripts

```bash
# Start individual nodes
./scripts/start_node.sh 1 4  # Start node 1 of a 4-node cluster
./scripts/start_node.sh 2 4  # Start node 2 of a 4-node cluster

# Start entire cluster
./scripts/start_cluster.sh 4  # Start 4-node cluster

# Stop cluster
./scripts/stop_cluster.sh

# Stop individual node
./scripts/stop_node.sh 2
```

### Using Environment Variables

```bash
# Set cluster size globally
export CLUSTER_SIZE=4

# Start nodes with environment config
NODE_INDEX=1 mix phx.server
NODE_INDEX=2 mix phx.server  # In another terminal

# Override cluster size for CI
export CI_CLUSTER_SIZE=3
```

## Configuration

### Application Configuration

```elixir
# config/test.exs or config/dev.exs
config :otp_supervisor, :distributed_testing,
  default_cluster_size: System.get_env("CLUSTER_SIZE", "2") |> String.to_integer(),
  max_cluster_size: 10,
  min_cluster_size: 1,
  http_port_base: 4200,
  dist_port_base: 9200
```

### Runtime Configuration

The system automatically configures nodes based on `NODE_INDEX`:

- Node 1: `superlearner@hostname`, port 4000
- Node 2: `superlearner2@hostname`, port 4010
- Node 3: `superlearner3@hostname`, port 4020
- Node N: `superlearnerN@hostname`, port 4000 + (N-1)*10

## Cluster Discovery

Use the new `ClusterDiscovery` module to connect nodes:

```elixir
# Discover all nodes in a 4-node cluster
nodes = OTPSupervisor.ClusterDiscovery.discover_nodes(4)
# => [:"superlearner@localhost", :"superlearner2@localhost", :"superlearner3@localhost"]

# Connect to all nodes
{:ok, connected} = OTPSupervisor.ClusterDiscovery.connect_cluster(4)

# Wait for cluster formation
{:ok, nodes} = OTPSupervisor.ClusterDiscovery.wait_for_cluster(4, 30_000)

# Get cluster info
OTPSupervisor.ClusterDiscovery.cluster_info()
# => %{
#   self: :"superlearner@localhost",
#   index: 1,
#   connected_nodes: [...],
#   cluster_size: 4,
#   cluster_formed: true
# }
```

## Testing with Different Cluster Sizes

```elixir
# In your tests
@tag distributed: true, cluster_size: 3
test "works with 3 nodes" do
  assert length(Node.list()) == 2  # 3 total, minus self
end

@tag distributed: true, cluster_size: 5
test "scales to 5 nodes" do
  nodes = Node.list()
  assert length(nodes) == 4
  
  # Use cluster discovery
  assert OTPSupervisor.ClusterDiscovery.cluster_formed?(5)
end
```

## Port Management

Ports are allocated dynamically:

- HTTP ports: 4000, 4010, 4020, ... (base + (index-1)*10)
- Distribution ports: 9200, 9210, 9220, ... (when using test cluster)

Override port configuration:

```bash
export BASE_HTTP_PORT=5000
export PORT_SPACING=20
```

## Backward Compatibility

The system maintains backward compatibility:

- Default cluster size is still 2
- Old scripts (`start_node1.sh`, `start_node2.sh`) still work
- `NODE_ROLE` environment variable still recognized
- Primary/secondary terminology still supported for 2-node clusters

## Troubleshooting

### Check cluster status
```bash
mix cluster.test status
```

### View logs from specific node
```bash
mix cluster.test logs node3
```

### Clean up stuck processes
```bash
mix cluster.test clean
```

### Manual cleanup
```bash
# Stop all superlearner processes
pkill -f superlearner

# Clean specific ports
fuser -k 4020/tcp
```

## Performance Considerations

- Each node uses ~150MB RAM (Elixir/Phoenix baseline)
- Port availability: ensure you have enough free ports
- Network limits: may need to increase OS connection limits for large clusters

### For large clusters (10+ nodes):

```bash
# Increase file descriptor limits
ulimit -n 4096

# Increase max user processes
ulimit -u 4096
```

## Examples

### Development: 3-node cluster
```bash
# Terminal 1
NODE_INDEX=1 CLUSTER_SIZE=3 mix phx.server

# Terminal 2
NODE_INDEX=2 CLUSTER_SIZE=3 mix phx.server

# Terminal 3
NODE_INDEX=3 CLUSTER_SIZE=3 mix phx.server
```

### Testing: Dynamic cluster
```bash
# Run tests with 4 nodes
CLUSTER_SIZE=4 mix test --only distributed

# Or use mix task
mix cluster.test run --size 4
```

### CI: Resource-conscious
```bash
# Use smaller clusters in CI
export CI_CLUSTER_SIZE=2
mix test
```

## Migration Guide

If you have existing code that assumes 2 nodes:

1. **Update hardcoded node names**:
   ```elixir
   # Old
   nodes = [:"superlearner@localhost", :"superlearner2@localhost"]
   
   # New
   nodes = OTPSupervisor.ClusterDiscovery.discover_nodes()
   ```

2. **Update port assumptions**:
   ```elixir
   # Old
   ports = [4000, 4010]
   
   # New
   base_port = Application.get_env(:otp_supervisor, :node_port)
   # Or calculate from node_index
   ```

3. **Update test assertions**:
   ```elixir
   # Old
   assert length(Node.list()) == 1
   
   # New
   cluster_size = System.get_env("CLUSTER_SIZE", "2") |> String.to_integer()
   assert length(Node.list()) == cluster_size - 1
   ```

## Future Enhancements

- Automatic node discovery via multicast/broadcast
- Dynamic cluster resizing (add/remove nodes on the fly)
- Load balancing across N nodes
- Cluster health monitoring dashboard for N nodes
- Kubernetes/container orchestration support