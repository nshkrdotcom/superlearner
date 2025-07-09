# WSL Cluster Setup Guide

## Overview

This guide provides step-by-step instructions for setting up a distributed Elixir cluster using two WSL instances sharing the same IP address but using different ports for differentiation.

## Prerequisites

- Windows with WSL2 enabled
- Two WSL Ubuntu instances
- Elixir 1.15+ and OTP 25+
- Git access to the superlearner repository

## WSL Instance Setup

### Instance 1: Primary Node (superlearner@localhost)

**Terminal 1 - Ubuntu WSL Instance 1:**
```bash
# Navigate to your project
cd /path/to/superlearner

# Install dependencies if not already done
mix deps.get

# Create the default dev configuration (if not exists)
# config/dev.exs should have:
# config :otp_supervisor, OtpSupervisorWeb.Endpoint, http: [port: 4000]

# Start Node 1
iex --name superlearner@localhost --cookie secret_cluster_cookie -S mix phx.server
```

**Access Node 1:** http://localhost:4000

### Instance 2: Secondary Node (superlearner2@localhost)

**Terminal 2 - Ubuntu WSL Instance 2:**
```bash
# Navigate to your project (same codebase)
cd /path/to/superlearner

# Create Node 2 configuration
# config/dev2.exs
```

**Create `config/dev2.exs`:**
```elixir
import_config "dev.exs"

# Override node-specific settings
config :superlearner, 
  node_name: :"superlearner2@localhost"

config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [port: 4010],
  debug_errors: true,
  code_reloader: true,
  check_origin: false
```

**Start Node 2:**
```bash
# Start Node 2 with different config
MIX_ENV=dev iex --name superlearner2@localhost --cookie secret_cluster_cookie --erl "-config dev2" -S mix phx.server
```

**Access Node 2:** http://localhost:4010

## Cluster Formation Verification

### Test Basic Connectivity

In **Node 1** IEx console:
```elixir
# Check current node
Node.self()
# => :"superlearner@localhost"

# List connected nodes
Node.list()
# => [:"superlearner2@localhost"]

# Ping Node 2
Node.ping(:"superlearner2@localhost")
# => :pong
```

In **Node 2** IEx console:
```elixir
# Check current node
Node.self()
# => :"superlearner2@localhost"

# List connected nodes
Node.list()
# => [:"superlearner@localhost"]

# Ping Node 1
Node.ping(:"superlearner@localhost")
# => :pong
```

### Test Process Communication

**Cross-node process spawning:**
```elixir
# From Node 1, spawn a process on Node 2
pid = Node.spawn(:"superlearner2@localhost", fn -> 
  IO.puts("Hello from Node 2!")
  receive do
    :stop -> IO.puts("Stopping process on Node 2")
  end
end)

# Send message to the remote process
send(pid, :stop)
```

## LibCluster Configuration

### Add Dependencies

**Update `mix.exs`:**
```elixir
defp deps do
  [
    {:libcluster, "~> 3.3"},
    {:horde, "~> 0.8.0"},
    # ... existing deps
  ]
end
```

### Configure LibCluster

**Update `config/config.exs`:**
```elixir
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
```

## Horde Setup

### Basic Horde Configuration

**Create `lib/otp_supervisor/distributed/cluster_supervisor.ex`:**
```elixir
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

### Update Application Supervisor

**Update `lib/otp_supervisor/application.ex`:**
```elixir
def start(_type, _args) do
  children = [
    # ... existing children
    OTPSupervisor.Distributed.ClusterSupervisor,  # Add this line
    # ... rest of children
  ]
  
  opts = [strategy: :one_for_one, name: OTPSupervisor.Supervisor]
  Supervisor.start_link(children, opts)
end
```

## Testing the Distributed Setup

### 1. Restart Both Nodes

After configuration changes, restart both nodes:

**Node 1:**
```bash
iex --name superlearner@localhost --cookie secret_cluster_cookie -S mix phx.server
```

**Node 2:**
```bash
MIX_ENV=dev iex --name superlearner2@localhost --cookie secret_cluster_cookie --erl "-config dev2" -S mix phx.server
```

### 2. Verify Horde Components

**Test Horde Registry:**
```elixir
# From Node 1
{:ok, _} = Horde.Registry.register(OTPSupervisor.Distributed.Registry, :test_key, %{node: Node.self()})

# From Node 2
Horde.Registry.lookup(OTPSupervisor.Distributed.Registry, :test_key)
# Should return the registration from Node 1
```

**Test Horde DynamicSupervisor:**
```elixir
# From Node 1
child_spec = %{
  id: :test_worker,
  start: {Agent, :start_link, [fn -> %{node: Node.self()} end]}
}

{:ok, pid} = Horde.DynamicSupervisor.start_child(OTPSupervisor.Distributed.Supervisor, child_spec)

# From Node 2
# The process might be running on either node due to Horde's distribution
Process.alive?(pid)
```

### 3. Test Cluster Manager

**Check cluster status:**
```elixir
# From either node
OTPSupervisor.Distributed.ClusterManager.get_cluster_status()
```

## Troubleshooting

### Common Issues

**1. Nodes not connecting:**
- Verify same cookie on both nodes
- Check that both nodes are using `@localhost` suffix
- Ensure EPMD is running: `epmd -names`

**2. Port conflicts:**
- Ensure Phoenix uses different ports (4000 vs 4001)
- Check no other applications are using these ports

**3. LibCluster not forming cluster:**
- Check `:libcluster` application is started
- Verify host names in configuration match actual node names
- Enable debug logging: `config :libcluster, debug: true`

### Debug Commands

**Check EPMD:**
```bash
epmd -names
# Should show both nodes
```

**Check node connectivity:**
```elixir
# From either node
:net_adm.ping(:"superlearner@localhost")
:net_adm.ping(:"superlearner2@localhost")
```

**Check Horde cluster members:**
```elixir
Horde.Cluster.members(OTPSupervisor.Distributed.Supervisor)
Horde.Cluster.members(OTPSupervisor.Distributed.Registry)
```

## Startup Scripts

### Create Startup Scripts

**`scripts/start_node1.sh`:**
```bash
#!/bin/bash
cd "$(dirname "$0")/.."
echo "Starting Node 1 on port 4000..."
iex --name superlearner@localhost --cookie secret_cluster_cookie -S mix phx.server
```

**`scripts/start_node2.sh`:**
```bash
#!/bin/bash
cd "$(dirname "$0")/.."
echo "Starting Node 2 on port 4010..."
MIX_ENV=dev iex --name superlearner2@localhost --cookie secret_cluster_cookie --erl "-config dev2" -S mix phx.server
```

**Make scripts executable:**
```bash
chmod +x scripts/start_node1.sh
chmod +x scripts/start_node2.sh
```

## Next Steps

Once you have both nodes running and connected:

1. **Verify cluster formation** - Both nodes should see each other
2. **Test Horde components** - Registry and DynamicSupervisor should work across nodes
3. **Access both web interfaces** - Node 1 at :4000, Node 2 at :4001
4. **Proceed with Phase 2** - Begin distributed sandbox implementation

## Port Summary

| Component | Node 1 | Node 2 |
|-----------|---------|--------|
| Phoenix Web | 4000 | 4010 |
| Node Name | superlearner@localhost | superlearner2@localhost |
| Web Access | http://localhost:4000 | http://localhost:4010 |
| EPMD | Shared (4369) | Shared (4369) |
| Erlang Cookie | secret_cluster_cookie | secret_cluster_cookie |

This setup allows you to develop and test distributed features locally using a single machine with two WSL instances, perfectly simulating a multi-node cluster environment.