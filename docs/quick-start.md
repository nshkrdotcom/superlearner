# OTP Supervisor Educational Tool - Quick Start Guide

## Prerequisites

- Elixir 1.15 or higher
- Phoenix 1.7 or higher
- Node.js 14+ (for assets)
- PostgreSQL (optional, for session storage)

## Installation

### Step 1: Create the Phoenix Project

```bash
# Create new Phoenix project with LiveView
mix phx.new otp_supervisor_tool --live --no-ecto

# Navigate to project directory
cd otp_supervisor_tool

# Install dependencies
mix deps.get

# Install Node.js dependencies
cd assets && npm install && cd ..
```

### Step 2: Add Required Dependencies

Edit `mix.exs` and add to the `deps` function:

```elixir
defp deps do
  [
    # ... existing deps ...
    {:phoenix_live_dashboard, "~> 0.8"},
    {:observer_cli, "~> 1.7"},
    {:vega_lite, "~> 0.1.8"},  # For advanced visualizations
    {:jason, "~> 1.4"}
  ]
end
```

Run `mix deps.get` to install new dependencies.

### Step 3: Create the Sandboxed Application

Create the sandbox application structure:

```bash
# Create sandbox app directories
mkdir -p lib/sandbox_app/{supervisors,workers}

# Create the main application file
touch lib/sandbox_app/application.ex
```

### Step 4: Basic Setup Code

Create the sandbox application:

```elixir
# lib/sandbox_app/application.ex
defmodule SandboxApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # We'll add supervisors here dynamically
    ]

    opts = [strategy: :one_for_one, name: SandboxApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

Create a simple worker:

```elixir
# lib/sandbox_app/workers/counter.ex
defmodule SandboxApp.Workers.Counter do
  use GenServer

  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, 0, name: name)
  end

  @impl true
  def init(initial_value) do
    {:ok, initial_value}
  end

  @impl true
  def handle_call(:get, _from, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_cast({:add, value}, state) do
    {:noreply, state + value}
  end
end
```

### Step 5: Create the Supervisor Control Module

```elixir
# lib/otp_supervisor_tool/supervisor_control.ex
defmodule OTPSupervisorTool.SupervisorControl do
  @moduledoc """
  Core API for controlling supervisors
  """

  def start_simple_supervisor do
    children = [
      {SandboxApp.Workers.Counter, name: :counter1},
      {SandboxApp.Workers.Counter, name: :counter2}
    ]

    Supervisor.start_link(children, 
      strategy: :one_for_one, 
      name: :demo_supervisor
    )
  end

  def get_tree(supervisor_name) do
    supervisor_name
    |> Supervisor.which_children()
    |> Enum.map(fn {id, pid, type, _modules} ->
      %{
        id: id,
        pid: inspect(pid),
        type: type,
        alive: Process.alive?(pid)
      }
    end)
  end

  def kill_child(supervisor_name, child_id) do
    case Supervisor.terminate_child(supervisor_name, child_id) do
      :ok -> Supervisor.restart_child(supervisor_name, child_id)
      error -> error
    end
  end
end
```

### Step 6: Create a LiveView Component

```elixir
# lib/otp_supervisor_tool_web/live/supervisor_demo_live.ex
defmodule OTPSupervisorToolWeb.SupervisorDemoLive do
  use OTPSupervisorToolWeb, :live_view
  
  alias OTPSupervisorTool.SupervisorControl

  @impl true
  def mount(_params, _session, socket) do
    # Start a demo supervisor
    {:ok, _pid} = SupervisorControl.start_simple_supervisor()
    
    if connected?(socket) do
      :timer.send_interval(1000, self(), :update)
    end

    {:ok, assign(socket, :children, get_children())}
  end

  @impl true
  def handle_info(:update, socket) do
    {:noreply, assign(socket, :children, get_children())}
  end

  @impl true
  def handle_event("kill_process", %{"child_id" => child_id}, socket) do
    child_atom = String.to_existing_atom(child_id)
    SupervisorControl.kill_child(:demo_supervisor, child_atom)
    {:noreply, assign(socket, :children, get_children())}
  end

  defp get_children do
    SupervisorControl.get_tree(:demo_supervisor)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="max-w-4xl mx-auto p-6">
      <h1 class="text-3xl font-bold mb-6">OTP Supervisor Demo</h1>
      
      <div class="bg-white shadow rounded-lg p-6">
        <h2 class="text-xl font-semibold mb-4">Supervision Tree</h2>
        
        <div class="space-y-4">
          <%= for child <- @children do %>
            <div class="border rounded p-4 flex justify-between items-center">
              <div>
                <p class="font-medium"><%= child.id %></p>
                <p class="text-sm text-gray-600">PID: <%= child.pid %></p>
                <p class="text-sm text-gray-600">Type: <%= child.type %></p>
              </div>
              
              <div class="flex items-center space-x-4">
                <span class={"px-3 py-1 rounded text-sm " <> if child.alive, do: "bg-green-100 text-green-800", else: "bg-red-100 text-red-800"}>
                  <%= if child.alive, do: "Alive", else: "Dead" %>
                </span>
                
                <button
                  phx-click="kill_process"
                  phx-value-child_id={child.id}
                  class="px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600"
                >
                  Kill Process
                </button>
              </div>
            </div>
          <% end %>
        </div>
      </div>
      
      <div class="mt-6 bg-blue-50 rounded-lg p-4">
        <h3 class="font-semibold mb-2">What's happening?</h3>
        <p class="text-sm">
          This demo shows a supervisor with two worker processes using the :one_for_one strategy.
          When you kill a process, the supervisor automatically restarts it. Watch the PIDs change!
        </p>
      </div>
    </div>
    """
  end
end
```

### Step 7: Update Router

Add the LiveView route to your router:

```elixir
# lib/otp_supervisor_tool_web/router.ex
scope "/", OTPSupervisorToolWeb do
  pipe_through :browser

  get "/", PageController, :index
  live "/demo", SupervisorDemoLive
  
  # Add LiveDashboard
  import Phoenix.LiveDashboard.Router
  live_dashboard "/dashboard", metrics: OTPSupervisorToolWeb.Telemetry
end
```

### Step 8: Run the Application

```bash
# Start the Phoenix server
mix phx.server
```

Now visit:
- http://localhost:4000/demo - See the supervisor demo
- http://localhost:4000/dashboard - Access Phoenix LiveDashboard

## Next Steps

### 1. Add More Supervisor Strategies

Create examples for `:all_for_one` and `:rest_for_one`:

```elixir
def start_all_for_one_supervisor do
  children = [
    {SandboxApp.Workers.Counter, name: :counter_a},
    {SandboxApp.Workers.Counter, name: :counter_b},
    {SandboxApp.Workers.Counter, name: :counter_c}
  ]

  Supervisor.start_link(children, 
    strategy: :all_for_one, 
    name: :all_for_one_demo
  )
end
```

### 2. Add Visualization

Install D3.js and create interactive tree visualizations:

```bash
cd assets
npm install d3
```

### 3. Create Educational Scenarios

Build modules that demonstrate specific concepts:

```elixir
defmodule OTPSupervisorTool.Scenarios.CascadeFailure do
  def run do
    # Set up linked processes
    # Demonstrate cascade failures
    # Show how supervisors prevent cascades
  end
end
```

### 4. Add API Endpoints

Create REST API for programmatic control:

```elixir
defmodule OTPSupervisorToolWeb.SupervisorController do
  use OTPSupervisorToolWeb, :controller

  def index(conn, _params) do
    supervisors = # ... list supervisors
    json(conn, %{supervisors: supervisors})
  end
end
```

## Troubleshooting

### Common Issues

1. **Supervisor already started**: If you see this error, the supervisor is already running. Either restart your application or use a different name.

2. **Process not found**: Make sure you're using the correct atom names when referencing processes.

3. **WebSocket connection failed**: Ensure LiveView is properly configured and JavaScript is enabled.

## Resources

- [Elixir Supervisor Documentation](https://hexdocs.pm/elixir/Supervisor.html)
- [Phoenix LiveView Documentation](https://hexdocs.pm/phoenix_live_view/Phoenix.LiveView.html)
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)

This quick start guide gets you up and running with the basic framework. From here, you can expand with more complex supervision trees, advanced visualizations, and comprehensive educational content.