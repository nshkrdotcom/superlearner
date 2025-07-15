# Distributed Cluster Mix Solution

## Problem Analysis

The current `mix cluster.test start` fails with `:nodistribution` error because:

1. The Mix process is already running in distributed mode (from `test_helper.exs`)
2. The TestCluster.Manager tries to start additional distributed nodes using `:peer` module
3. Erlang doesn't allow multiple distributed configurations in the same process
4. This creates a conflict that prevents cluster startup

## Root Cause

The issue is in the approach - we're trying to start **peer nodes** when we should be starting **connected processes** that can communicate with the existing distributed node.

## Technical Solution

### Option 1: Multi-Port Phoenix Servers (Recommended)

Instead of starting peer nodes, start multiple Phoenix servers on different ports that connect to the same distributed node:

```elixir
# In TestCluster.Manager
defp start_cluster_nodes do
  nodes = [
    %{name: :node1, port: 4001, host: "127.0.0.1"},
    %{name: :node2, port: 4002, host: "127.0.0.1"},
    %{name: :node3, port: 4003, host: "127.0.0.1"}
  ]
  
  Enum.map(nodes, &start_phoenix_server/1)
end

defp start_phoenix_server(%{name: name, port: port, host: host}) do
  # Start Phoenix server on specific port
  cmd = "PORT=#{port} MIX_ENV=test elixir --name #{name}@#{host} -S mix phx.server"
  
  port_ref = Port.open({:spawn, cmd}, [:binary, :exit_status])
  
  # Wait for server to be ready
  :timer.sleep(2000)
  
  # Verify server is running
  case :httpc.request(:get, {'http://#{host}:#{port}/health', []}, [], []) do
    {:ok, {{_, 200, _}, _, _}} -> 
      {:ok, %{name: name, port: port, host: host, ref: port_ref}}
    _ -> 
      {:error, :server_not_ready}
  end
end
```

### Option 2: Supervised Task Approach

Use Elixir's Task.Supervisor to spawn supervised processes that act as cluster nodes:

```elixir
defp start_cluster_nodes do
  {:ok, supervisor} = Task.Supervisor.start_link()
  
  nodes = [:node1, :node2, :node3]
  
  tasks = Enum.map(nodes, fn node_name ->
    Task.Supervisor.async(supervisor, fn ->
      start_cluster_node(node_name)
    end)
  end)
  
  results = Task.await_many(tasks, 10_000)
  {:ok, results}
end

defp start_cluster_node(node_name) do
  # Register the process as a "node"
  Process.register(self(), node_name)
  
  # Start the node's services
  {:ok, _} = OTPSupervisor.Core.Arsenal.start_link()
  {:ok, _} = OTPSupervisor.Core.SandboxManager.start_link()
  
  # Keep the process alive
  receive do
    :stop -> :ok
  end
end
```

### Option 3: Dynamic Node Names (Advanced)

Use dynamic node names to avoid conflicts:

```elixir
defp start_primary_node do
  # Generate unique node name
  timestamp = :os.system_time(:millisecond)
  node_name = :"test_cluster_#{timestamp}@127.0.0.1"
  
  # Check if we can start distributed mode
  case Node.alive?() do
    true ->
      # Already distributed, use current node
      {:ok, Node.self()}
    false ->
      # Start distributed mode
      case Node.start(node_name, :shortnames) do
        {:ok, _} -> {:ok, node_name}
        {:error, reason} -> {:error, reason}
      end
  end
end
```

## Implementation Plan

### Step 1: Fix the Manager

```elixir
# lib/otp_supervisor/test_cluster/manager.ex
defmodule OTPSupervisor.TestCluster.Manager do
  use GenServer
  
  def start_cluster do
    GenServer.call(__MODULE__, :start_cluster, 30_000)
  end
  
  def handle_call(:start_cluster, _from, state) do
    case start_cluster_servers() do
      {:ok, servers} ->
        new_state = %{state | servers: servers, status: :running}
        {:reply, {:ok, servers}, new_state}
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
  
  defp start_cluster_servers do
    # Start 3 Phoenix servers on different ports
    servers = [
      start_server(:node1, 4001),
      start_server(:node2, 4002), 
      start_server(:node3, 4003)
    ]
    
    case Enum.all?(servers, &match?({:ok, _}, &1)) do
      true -> {:ok, Enum.map(servers, fn {:ok, server} -> server end)}
      false -> {:error, :server_startup_failed}
    end
  end
  
  defp start_server(name, port) do
    # Use System.cmd to start server in background
    env = [{"PORT", to_string(port)}, {"MIX_ENV", "test"}]
    
    case System.cmd("elixir", ["-S", "mix", "phx.server"], 
                    env: env, 
                    into: IO.stream(:stdio, :line)) do
      {_, 0} -> 
        # Wait for server to be ready
        wait_for_server("127.0.0.1", port)
        {:ok, %{name: name, port: port, status: :running}}
      {_, exit_code} -> 
        {:error, {:exit_code, exit_code}}
    end
  end
  
  defp wait_for_server(host, port, retries \\ 10) do
    case :gen_tcp.connect(String.to_charlist(host), port, [], 1000) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        :ok
      {:error, _} when retries > 0 ->
        :timer.sleep(1000)
        wait_for_server(host, port, retries - 1)
      {:error, reason} ->
        {:error, reason}
    end
  end
end
```

### Step 2: Add Health Endpoints

```elixir
# lib/otp_supervisor_web/controllers/health_controller.ex
defmodule OTPSupervisorWeb.HealthController do
  use OTPSupervisorWeb, :controller
  
  def check(conn, _params) do
    status = %{
      node: Node.self(),
      status: :healthy,
      timestamp: DateTime.utc_now(),
      services: check_services()
    }
    
    json(conn, status)
  end
  
  defp check_services do
    %{
      arsenal: Process.whereis(OTPSupervisor.Core.Arsenal) != nil,
      sandbox_manager: Process.whereis(OTPSupervisor.Core.SandboxManager) != nil
    }
  end
end
```

### Step 3: Update Router

```elixir
# lib/otp_supervisor_web/router.ex
scope "/api", OTPSupervisorWeb do
  pipe_through :api
  
  get "/health", HealthController, :check
  # ... other routes
end
```

### Step 4: Fix CLI Commands

```elixir
# lib/mix/tasks/cluster/test.ex
defp start_cluster do
  Mix.shell().info("🚀 Starting distributed test cluster...")
  
  # Ensure application is started
  Application.ensure_all_started(:otp_supervisor)
  
  # Start the cluster manager if not already started
  case GenServer.whereis(OTPSupervisor.TestCluster.Manager) do
    nil -> 
      {:ok, _} = OTPSupervisor.TestCluster.Manager.start_link()
    _ -> 
      :ok
  end
  
  case OTPSupervisor.TestCluster.Manager.start_cluster() do
    {:ok, servers} ->
      Mix.shell().info("✅ Cluster started successfully!")
      display_cluster_info(servers)
      :ok
    {:error, reason} ->
      Mix.shell().error("❌ Failed to start cluster: #{inspect(reason)}")
      display_troubleshooting_suggestions()
      {:error, reason}
  end
end

defp display_cluster_info(servers) do
  Mix.shell().info("\n📊 Cluster Information:")
  Mix.shell().info("=" <> String.duplicate("=", 40))
  
  Enum.each(servers, fn server ->
    Mix.shell().info("• #{server.name}: http://127.0.0.1:#{server.port}")
  end)
  
  Mix.shell().info("\n🔗 Test the cluster:")
  Mix.shell().info("curl http://127.0.0.1:4001/api/health")
  Mix.shell().info("curl http://127.0.0.1:4002/api/health") 
  Mix.shell().info("curl http://127.0.0.1:4003/api/health")
end
```

## Testing the Solution

### 1. Start the cluster:
```bash
mix cluster.test start
```

### 2. Verify servers are running:
```bash
curl http://127.0.0.1:4001/api/health
curl http://127.0.0.1:4002/api/health
curl http://127.0.0.1:4003/api/health
```

### 3. Check cluster status:
```bash
mix cluster.test status
```

### 4. Run health checks:
```bash
mix cluster.test health
```

## Why This Approach Works

1. **No Distributed Conflicts**: We use the existing distributed node instead of creating new ones
2. **Real Servers**: Actual Phoenix servers running on different ports that you can connect to
3. **Proper Isolation**: Each server runs in its own OS process
4. **Easy Debugging**: Clear separation, easy to see what's running
5. **Mix Integration**: Leverages the existing Mix ecosystem properly

## Expected Output

When working correctly, you should see:

```bash
$ mix cluster.test start
🚀 Starting distributed test cluster...
✅ Cluster started successfully!

📊 Cluster Information:
========================================
• node1: http://127.0.0.1:4001
• node2: http://127.0.0.1:4002  
• node3: http://127.0.0.1:4003

🔗 Test the cluster:
curl http://127.0.0.1:4001/api/health
curl http://127.0.0.1:4002/api/health
curl http://127.0.0.1:4003/api/health

$ epmd -names
epmd: up and running on port 4369 with data:
name test_cluster_1234567890 at port 12345
```

This approach gives you real, running servers that you can connect to and test against, while working within the Mix ecosystem you've already built.