# Cluster Usage Examples

## Quick Examples

### 1. Start a 3-node cluster
```bash
mix cluster.test start --size 3
```

### 2. Using environment variables
```bash
# Terminal 1
NODE_INDEX=1 CLUSTER_SIZE=3 iex --name superlearner@localhost -S mix phx.server

# Terminal 2  
NODE_INDEX=2 CLUSTER_SIZE=3 iex --name superlearner2@localhost -S mix phx.server

# Terminal 3
NODE_INDEX=3 CLUSTER_SIZE=3 iex --name superlearner3@localhost -S mix phx.server
```

### 3. Using the generic scripts
```bash
# Start complete cluster
./scripts/start_cluster.sh 5

# Or start individual nodes
./scripts/start_node.sh 1 5
./scripts/start_node.sh 2 5
# ... etc
```

## In Your Application Code

### Using ClusterDiscovery
```elixir
# In your application supervisor or startup code
defmodule MyApp.ClusterConnector do
  use GenServer
  alias OTPSupervisor.ClusterDiscovery
  
  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end
  
  def init(_) do
    # Connect to cluster on startup
    connect_to_cluster()
    {:ok, %{}}
  end
  
  defp connect_to_cluster do
    cluster_size = System.get_env("CLUSTER_SIZE", "2") |> String.to_integer()
    
    case ClusterDiscovery.connect_cluster(cluster_size) do
      {:ok, nodes} ->
        IO.puts("Connected to nodes: #{inspect(nodes)}")
      {:partial, connected, failed} ->
        IO.puts("Partially connected. Success: #{inspect(connected)}, Failed: #{inspect(failed)}")
      _ ->
        IO.puts("Failed to connect to cluster")
    end
  end
end
```

### In Tests
```elixir
defmodule MyApp.DistributedTest do
  use ExUnit.Case
  
  @tag distributed: true, cluster_size: 4
  test "distributed operation across 4 nodes" do
    # The cluster is automatically started with 4 nodes
    nodes = Node.list()
    assert length(nodes) == 3  # 4 total minus self
    
    # Use cluster discovery
    assert OTPSupervisor.ClusterDiscovery.cluster_formed?(4)
    
    # Get cluster info
    info = OTPSupervisor.ClusterDiscovery.cluster_info()
    assert info.cluster_size == 4
    assert info.index in 1..4
  end
end
```

### Dynamic Node Discovery Pattern
```elixir
defmodule MyApp.NodeRegistry do
  @doc """
  Get all nodes in the cluster dynamically
  """
  def all_nodes do
    [node() | Node.list()] |> Enum.sort()
  end
  
  @doc """
  Execute function on all nodes
  """
  def on_all_nodes(module, function, args) do
    all_nodes()
    |> Enum.map(fn node ->
      Task.async(fn ->
        :rpc.call(node, module, function, args)
      end)
    end)
    |> Task.await_many()
  end
  
  @doc """
  Pick a random node for load balancing
  """
  def random_node do
    Enum.random(all_nodes())
  end
end
```

## Testing Different Cluster Sizes

```bash
# Test with 2 nodes (default)
mix test --only distributed

# Test with 5 nodes
CLUSTER_SIZE=5 mix test --only distributed

# Or use the mix task
mix cluster.test run --size 5
```

## Monitoring Cluster Health

```elixir
# In IEx on any node
OTPSupervisor.ClusterDiscovery.cluster_info()

# Check if all nodes are connected
OTPSupervisor.ClusterDiscovery.cluster_formed?(5)

# Get list of other nodes
OTPSupervisor.ClusterDiscovery.discover_nodes(5)
```

## Common Patterns

### 1. Leader Election (using global)
```elixir
defmodule MyApp.Leader do
  use GenServer
  
  def start_link(_) do
    case :global.register_name(__MODULE__, self()) do
      :yes -> 
        GenServer.start_link(__MODULE__, :leader, name: __MODULE__)
      :no ->
        GenServer.start_link(__MODULE__, :follower, name: __MODULE__)
    end
  end
end
```

### 2. Distributed PubSub
```elixir
# Broadcast to all nodes
Phoenix.PubSub.broadcast(MyApp.PubSub, "topic", %{event: "data"})

# Will be received on all nodes that subscribed
Phoenix.PubSub.subscribe(MyApp.PubSub, "topic")
```

### 3. Distributed Tasks
```elixir
# Run task on specific node based on index
def run_on_node(index, fun) do
  hostname = OTPSupervisor.ClusterDiscovery.get_hostname()
  node_name = OTPSupervisor.ClusterDiscovery.node_name(index, hostname)
  
  Task.Supervisor.async({MyApp.TaskSupervisor, node_name}, fun)
end
```

## Troubleshooting

### Check what's running
```bash
mix cluster.test status
```

### Clean up everything
```bash
mix cluster.test clean
```

### Manual node connection in IEx
```elixir
# Connect to specific node
Node.connect(:"superlearner3@localhost")

# List connected nodes
Node.list()

# Ping a node
Node.ping(:"superlearner2@localhost")
```