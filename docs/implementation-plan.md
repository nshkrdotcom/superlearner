# OTP Supervisor Educational Tool - Implementation Plan

## Project Structure

```
otp_supervisor_tool/
├── mix.exs                    # Main Phoenix project
├── lib/
│   ├── otp_supervisor_web/    # Phoenix web layer
│   │   ├── router.ex
│   │   ├── channels/
│   │   ├── controllers/
│   │   ├── live/
│   │   │   ├── supervisor_live.ex
│   │   │   ├── process_tree_live.ex
│   │   │   └── code_editor_live.ex
│   │   └── templates/
│   ├── otp_supervisor/        # Core business logic
│   │   ├── supervisor_control.ex
│   │   ├── process_visualizer.ex
│   │   ├── educational.ex
│   │   ├── code_manager.ex
│   │   └── instrumentation.ex
│   └── sandbox_app/           # Sandboxed Elixir app
│       ├── application.ex
│       ├── example_supervisors/
│       │   ├── basic_supervisor.ex
│       │   ├── complex_tree.ex
│       │   └── dynamic_example.ex
│       └── example_workers/
│           ├── simple_worker.ex
│           ├── stateful_worker.ex
│           └── failing_worker.ex
├── assets/
│   ├── js/
│   │   ├── app.js
│   │   ├── process_tree_viz.js
│   │   └── code_editor.js
│   └── css/
├── test/
└── docs/
```

## Implementation Steps

### Step 1: Project Setup

```bash
# Create new Phoenix project
mix phx.new otp_supervisor_tool --live

# Add dependencies to mix.exs
defp deps do
  [
    {:phoenix, "~> 1.7.0"},
    {:phoenix_live_view, "~> 0.20.0"},
    {:phoenix_live_dashboard, "~> 0.8.0"},
    {:observer_cli, "~> 1.7"},
    {:jason, "~> 1.4"},
    {:telemetry_metrics, "~> 0.6"},
    {:telemetry_poller, "~> 1.0"},
    # For visualization
    {:vega_lite, "~> 0.1.8"},
    # For code compilation
    {:nimble_parsec, "~> 1.2"}
  ]
end
```

### Step 2: Core Supervisor Control Module

```elixir
# lib/otp_supervisor/supervisor_control.ex
defmodule OTPSupervisor.SupervisorControl do
  @moduledoc """
  Core API for controlling supervision trees programmatically
  """
  
  alias OTPSupervisor.Instrumentation
  
  @doc """
  Start a new supervisor with given strategy and children
  """
  def start_supervisor(strategy, children_specs, opts \\ []) do
    children = Enum.map(children_specs, &build_child_spec/1)
    
    Supervisor.start_link(children, [
      strategy: strategy,
      name: opts[:name]
    ])
  end
  
  @doc """
  Get complete supervision tree structure
  """
  def get_supervision_tree(supervisor) do
    children = Supervisor.which_children(supervisor)
    
    Enum.map(children, fn {id, pid, type, modules} ->
      %{
        id: id,
        pid: pid,
        type: type,
        modules: modules,
        state: get_process_state(pid),
        children: if type == :supervisor, do: get_supervision_tree(pid), else: []
      }
    end)
  end
  
  @doc """
  Inject a fault into a process
  """
  def inject_fault(pid, fault_type) do
    case fault_type do
      :crash -> Process.exit(pid, :kill)
      :normal_exit -> Process.exit(pid, :normal)
      :shutdown -> Process.exit(pid, :shutdown)
      {:custom, reason} -> Process.exit(pid, reason)
    end
  end
  
  @doc """
  Hot reload code for a module
  """
  def reload_module(module) do
    with {:ok, binary} <- compile_module(module),
         :ok <- :code.purge(module),
         {:module, ^module} <- :code.load_binary(module, '#{module}.beam', binary) do
      {:ok, module}
    end
  end
  
  defp build_child_spec(spec) when is_map(spec), do: spec
  defp build_child_spec(module) when is_atom(module), do: module
  defp build_child_spec({module, args}), do: {module, args}
  
  defp get_process_state(pid) when is_pid(pid) do
    case Process.info(pid, [:status, :memory, :message_queue_len, :current_function]) do
      nil -> :dead
      info -> Map.new(info)
    end
  end
end
```

### Step 3: LiveView Components

```elixir
# lib/otp_supervisor_web/live/supervisor_live.ex
defmodule OTPSupervisorWeb.SupervisorLive do
  use OTPSupervisorWeb, :live_view
  
  alias OTPSupervisor.SupervisorControl
  alias OTPSupervisor.ProcessVisualizer
  
  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(1000, self(), :refresh)
    end
    
    {:ok,
     socket
     |> assign(:supervisors, list_supervisors())
     |> assign(:selected_supervisor, nil)
     |> assign(:tree_data, nil)}
  end
  
  @impl true
  def handle_event("select_supervisor", %{"id" => supervisor_id}, socket) do
    supervisor = String.to_existing_atom(supervisor_id)
    tree_data = SupervisorControl.get_supervision_tree(supervisor)
    
    {:noreply,
     socket
     |> assign(:selected_supervisor, supervisor)
     |> assign(:tree_data, tree_data)
     |> push_event("update_tree", %{data: ProcessVisualizer.to_d3_format(tree_data)})}
  end
  
  @impl true
  def handle_event("kill_process", %{"pid" => pid_string}, socket) do
    pid = :erlang.list_to_pid(String.to_charlist(pid_string))
    SupervisorControl.inject_fault(pid, :crash)
    
    {:noreply, socket}
  end
  
  @impl true
  def handle_event("add_child", params, socket) do
    # Implementation for adding children dynamically
  end
  
  @impl true
  def handle_info(:refresh, socket) do
    if socket.assigns.selected_supervisor do
      tree_data = SupervisorControl.get_supervision_tree(socket.assigns.selected_supervisor)
      
      {:noreply,
       socket
       |> assign(:tree_data, tree_data)
       |> push_event("update_tree", %{data: ProcessVisualizer.to_d3_format(tree_data)})}
    else
      {:noreply, socket}
    end
  end
  
  defp list_supervisors do
    # List all registered supervisors
    Process.registered()
    |> Enum.filter(&is_supervisor?/1)
    |> Enum.map(&process_info/1)
  end
end
```

### Step 4: WebSocket Channels

```elixir
# lib/otp_supervisor_web/channels/supervisor_channel.ex
defmodule OTPSupervisorWeb.SupervisorChannel do
  use Phoenix.Channel
  
  alias OTPSupervisor.SupervisorControl
  
  def join("supervisor:lobby", _params, socket) do
    {:ok, socket}
  end
  
  def handle_in("get_tree", %{"supervisor" => supervisor_name}, socket) do
    supervisor = String.to_existing_atom(supervisor_name)
    tree = SupervisorControl.get_supervision_tree(supervisor)
    
    {:reply, {:ok, %{tree: tree}}, socket}
  end
  
  def handle_in("inject_fault", %{"pid" => pid, "type" => fault_type}, socket) do
    pid = :erlang.list_to_pid(String.to_charlist(pid))
    fault = String.to_existing_atom(fault_type)
    
    SupervisorControl.inject_fault(pid, fault)
    
    broadcast(socket, "process_killed", %{pid: pid})
    {:noreply, socket}
  end
end
```

### Step 5: Sandboxed Example Application

```elixir
# lib/sandbox_app/example_supervisors/basic_supervisor.ex
defmodule SandboxApp.BasicSupervisor do
  use Supervisor
  
  def start_link(opts) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  @impl true
  def init(opts) do
    strategy = Keyword.get(opts, :strategy, :one_for_one)
    
    children = [
      {SandboxApp.SimpleWorker, name: :worker_1},
      {SandboxApp.SimpleWorker, name: :worker_2},
      {SandboxApp.StatefulWorker, name: :stateful_worker}
    ]
    
    Supervisor.init(children, strategy: strategy)
  end
end

# lib/sandbox_app/example_workers/simple_worker.ex
defmodule SandboxApp.SimpleWorker do
  use GenServer
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end
  
  @impl true
  def init(:ok) do
    {:ok, %{count: 0}}
  end
  
  @impl true
  def handle_call(:get_count, _from, state) do
    {:reply, state.count, state}
  end
  
  @impl true
  def handle_cast(:increment, state) do
    {:noreply, %{state | count: state.count + 1}}
  end
end
```

### Step 6: JavaScript Visualization

```javascript
// assets/js/process_tree_viz.js
import * as d3 from "d3";

export const ProcessTreeViz = {
  mounted() {
    this.svg = d3.select(this.el).append("svg")
      .attr("width", "100%")
      .attr("height", 600);
    
    this.handleEvent("update_tree", ({data}) => {
      this.updateTree(data);
    });
  },
  
  updateTree(data) {
    const width = this.el.offsetWidth;
    const height = 600;
    
    const tree = d3.tree()
      .size([height - 100, width - 200]);
    
    const root = d3.hierarchy(data);
    tree(root);
    
    // Clear previous
    this.svg.selectAll("*").remove();
    
    const g = this.svg.append("g")
      .attr("transform", "translate(100,50)");
    
    // Draw links
    g.selectAll(".link")
      .data(root.links())
      .enter().append("path")
      .attr("class", "link")
      .attr("d", d3.linkHorizontal()
        .x(d => d.y)
        .x(d => d.x));
    
    // Draw nodes
    const node = g.selectAll(".node")
      .data(root.descendants())
      .enter().append("g")
      .attr("class", "node")
      .attr("transform", d => `translate(${d.y},${d.x})`);
    
    node.append("circle")
      .attr("r", 10)
      .attr("fill", d => d.data.type === "supervisor" ? "#4a90e2" : "#7ed321")
      .on("click", (event, d) => {
        this.pushEvent("process_clicked", {pid: d.data.pid});
      });
    
    node.append("text")
      .attr("dy", 3)
      .attr("x", d => d.children ? -12 : 12)
      .style("text-anchor", d => d.children ? "end" : "start")
      .text(d => d.data.id);
  }
};
```

### Step 7: Educational Modules

```elixir
# lib/otp_supervisor/educational.ex
defmodule OTPSupervisor.Educational do
  @moduledoc """
  Educational scenarios and demonstrations
  """
  
  alias OTPSupervisor.SupervisorControl
  alias SandboxApp.BasicSupervisor
  
  @doc """
  Demonstrate one_for_one restart strategy
  """
  def demonstrate_one_for_one do
    # Start supervisor with one_for_one strategy
    {:ok, sup} = BasicSupervisor.start_link(strategy: :one_for_one)
    
    # Get initial state
    initial_tree = SupervisorControl.get_supervision_tree(sup)
    
    # Kill one worker
    [{_, worker_pid, _, _} | _] = Supervisor.which_children(sup)
    Process.exit(worker_pid, :kill)
    
    # Wait for restart
    Process.sleep(100)
    
    # Get state after restart
    after_restart_tree = SupervisorControl.get_supervision_tree(sup)
    
    %{
      strategy: :one_for_one,
      initial_state: initial_tree,
      after_crash: after_restart_tree,
      explanation: """
      With one_for_one strategy, only the crashed process is restarted.
      Other processes continue running unaffected.
      """
    }
  end
  
  @doc """
  Demonstrate cascade failures
  """
  def demonstrate_cascade_failure do
    # Implementation showing how failures propagate
  end
end
```

### Step 8: API Routes

```elixir
# lib/otp_supervisor_web/controllers/api/supervisor_controller.ex
defmodule OTPSupervisorWeb.API.SupervisorController do
  use OTPSupervisorWeb, :controller
  
  alias OTPSupervisor.SupervisorControl
  
  def index(conn, _params) do
    supervisors = Process.registered()
    |> Enum.filter(&is_supervisor?/1)
    |> Enum.map(&format_supervisor_info/1)
    
    json(conn, %{supervisors: supervisors})
  end
  
  def show(conn, %{"id" => supervisor_name}) do
    supervisor = String.to_existing_atom(supervisor_name)
    tree = SupervisorControl.get_supervision_tree(supervisor)
    
    json(conn, %{tree: tree})
  end
  
  def create_child(conn, %{"id" => supervisor_name, "child_spec" => child_spec}) do
    supervisor = String.to_existing_atom(supervisor_name)
    
    case DynamicSupervisor.start_child(supervisor, child_spec) do
      {:ok, pid} ->
        json(conn, %{status: "ok", pid: inspect(pid)})
      {:error, reason} ->
        conn
        |> put_status(400)
        |> json(%{error: inspect(reason)})
    end
  end
end
```

## Next Steps

1. **Testing Infrastructure**: Set up comprehensive tests for all modules
2. **Documentation**: Create user guides and API documentation
3. **UI Polish**: Improve visualization and user interface
4. **Performance**: Optimize for handling large supervision trees
5. **Educational Content**: Develop interactive tutorials and exercises
6. **Deployment**: Docker setup and deployment configuration

This implementation plan provides a solid foundation for building the OTP Supervisor Educational Tool with all the key features discussed in the architecture document.