# Phase 1: Core Foundation - Detailed Implementation Guide

## Overview

Phase 1 creates the minimal viable supervisor control system with a basic web interface. This phase uses only Phoenix core dependencies and delivers immediate educational value.

## Prerequisites

- Elixir 1.15+
- Phoenix 1.7+
- Node.js 14+ (for Phoenix assets)

## Step 1: Project Setup

### 1.1 Create Phoenix Project

```bash
# Create project without Ecto (no database needed)
mix phx.new otp_supervisor --live --no-ecto

# Enter project directory
cd otp_supervisor

# Get dependencies
mix deps.get

# Install Node dependencies
cd assets && npm install && cd ..

# Verify setup
mix phx.server
# Visit http://localhost:4000 to confirm Phoenix is running
```

### 1.2 Project Structure

Create the following directory structure:

```bash
mkdir -p lib/otp_supervisor/core
mkdir -p lib/otp_supervisor/sandbox
mkdir -p lib/otp_supervisor/sandbox/workers
mkdir -p lib/otp_supervisor/sandbox/supervisors
```

## Step 2: Core Supervisor Control Module

### 2.1 Create the Control Module

```elixir
# lib/otp_supervisor/core/control.ex
defmodule OTPSupervisor.Core.Control do
  @moduledoc """
  Core API for controlling and inspecting supervisors.
  """

  @doc """
  Lists all registered supervisors in the system.
  """
  def list_supervisors do
    Process.registered()
    |> Enum.filter(&is_supervisor?/1)
    |> Enum.map(&format_supervisor_info/1)
  end

  @doc """
  Gets the supervision tree for a given supervisor.
  """
  def get_supervision_tree(supervisor_name) when is_atom(supervisor_name) do
    case Process.whereis(supervisor_name) do
      nil -> {:error, :not_found}
      pid -> get_supervision_tree(pid)
    end
  end

  def get_supervision_tree(supervisor_pid) when is_pid(supervisor_pid) do
    case Supervisor.which_children(supervisor_pid) do
      children when is_list(children) ->
        {:ok, format_children(children)}
      _ ->
        {:error, :not_supervisor}
    end
  end

  @doc """
  Kills a process by PID.
  """
  def kill_process(pid) when is_pid(pid) do
    Process.exit(pid, :kill)
    :ok
  end

  def kill_process(pid_string) when is_binary(pid_string) do
    pid_string
    |> String.to_charlist()
    |> :erlang.list_to_pid()
    |> kill_process()
  end

  @doc """
  Gets detailed information about a process.
  """
  def get_process_info(pid) when is_pid(pid) do
    case Process.info(pid) do
      nil -> {:error, :process_dead}
      info -> {:ok, format_process_info(info)}
    end
  end

  # Private functions

  defp is_supervisor?(name) do
    case Process.whereis(name) do
      nil -> false
      pid -> is_supervisor_pid?(pid)
    end
  end

  defp is_supervisor_pid?(pid) do
    case Process.info(pid, :dictionary) do
      nil -> false
      {:dictionary, dict} ->
        # Check if it has supervisor behavior
        Keyword.get(dict, :"$initial_call") 
        |> case do
          {Supervisor, _, _} -> true
          _ -> 
            # Also check for DynamicSupervisor
            Keyword.get(dict, :"$ancestors", [])
            |> Enum.any?(fn
              pid when is_pid(pid) -> is_supervisor_pid?(pid)
              _ -> false
            end)
        end
    end
  end

  defp format_supervisor_info(name) do
    pid = Process.whereis(name)
    %{
      name: name,
      pid: inspect(pid),
      alive: Process.alive?(pid),
      child_count: count_children(pid)
    }
  end

  defp count_children(pid) do
    case Supervisor.count_children(pid) do
      %{active: active} -> active
      _ -> 0
    end
  rescue
    _ -> 0
  end

  defp format_children(children) do
    Enum.map(children, fn {id, pid, type, modules} ->
      %{
        id: id,
        pid: inspect(pid),
        type: type,
        modules: modules,
        alive: is_pid(pid) && Process.alive?(pid),
        info: if(is_pid(pid), do: get_basic_process_info(pid), else: %{})
      }
    end)
  end

  defp get_basic_process_info(pid) do
    case Process.info(pid, [:memory, :message_queue_len, :status]) do
      nil -> %{}
      info -> Map.new(info)
    end
  end

  defp format_process_info(info) do
    info
    |> Keyword.take([
      :memory, :message_queue_len, :status, :heap_size,
      :stack_size, :reductions, :current_function
    ])
    |> Map.new()
  end
end
```

## Step 3: Sandbox Application

### 3.1 Create Simple Workers

```elixir
# lib/otp_supervisor/sandbox/workers/counter.ex
defmodule OTPSupervisor.Sandbox.Workers.Counter do
  @moduledoc """
  A simple counter GenServer for demonstration.
  """
  use GenServer

  # Client API

  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    initial = Keyword.get(opts, :initial_value, 0)
    GenServer.start_link(__MODULE__, initial, name: name)
  end

  def increment(server) do
    GenServer.cast(server, :increment)
  end

  def get_value(server) do
    GenServer.call(server, :get_value)
  end

  def crash(server) do
    GenServer.cast(server, :crash)
  end

  # Server Callbacks

  @impl true
  def init(initial_value) do
    {:ok, %{value: initial_value, crashes: 0}}
  end

  @impl true
  def handle_call(:get_value, _from, state) do
    {:reply, state.value, state}
  end

  @impl true
  def handle_cast(:increment, state) do
    {:noreply, %{state | value: state.value + 1}}
  end

  @impl true
  def handle_cast(:crash, _state) do
    # Simulate a crash
    raise "Intentional crash for demonstration"
  end
end
```

```elixir
# lib/otp_supervisor/sandbox/workers/printer.ex
defmodule OTPSupervisor.Sandbox.Workers.Printer do
  @moduledoc """
  A simple printer GenServer that logs messages.
  """
  use GenServer
  require Logger

  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  def print(server, message) do
    GenServer.cast(server, {:print, message})
  end

  @impl true
  def init(opts) do
    id = Keyword.get(opts, :id, "printer")
    {:ok, %{id: id, print_count: 0}}
  end

  @impl true
  def handle_cast({:print, message}, state) do
    Logger.info("[#{state.id}] #{message} (count: #{state.print_count + 1})")
    {:noreply, %{state | print_count: state.print_count + 1}}
  end
end
```

### 3.2 Create Example Supervisors

```elixir
# lib/otp_supervisor/sandbox/supervisors/demo_supervisor.ex
defmodule OTPSupervisor.Sandbox.Supervisors.DemoSupervisor do
  @moduledoc """
  A demonstration supervisor with different restart strategies.
  """
  use Supervisor

  def start_link(opts) do
    strategy = Keyword.get(opts, :strategy, :one_for_one)
    name = Keyword.get(opts, :name, __MODULE__)
    Supervisor.start_link(__MODULE__, strategy, name: name)
  end

  @impl true
  def init(strategy) do
    children = [
      {OTPSupervisor.Sandbox.Workers.Counter, name: :counter_1, initial_value: 0},
      {OTPSupervisor.Sandbox.Workers.Counter, name: :counter_2, initial_value: 100},
      {OTPSupervisor.Sandbox.Workers.Printer, name: :printer_1, id: "printer-1"}
    ]

    Supervisor.init(children, strategy: strategy)
  end
end
```

### 3.3 Start Supervisors on Application Start

```elixir
# lib/otp_supervisor/application.ex
defmodule OTPSupervisor.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      OTPSupervisorWeb.Telemetry,
      {Phoenix.PubSub, name: OTPSupervisor.PubSub},
      OTPSupervisorWeb.Endpoint,
      # Start our demo supervisors
      {OTPSupervisor.Sandbox.Supervisors.DemoSupervisor, 
       name: :demo_one_for_one, strategy: :one_for_one}
    ]

    opts = [strategy: :one_for_one, name: OTPSupervisor.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    OTPSupervisorWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
```

## Step 4: LiveView UI

### 4.1 Create the Main LiveView

```elixir
# lib/otp_supervisor_web/live/supervisor_live.ex
defmodule OTPSupervisorWeb.SupervisorLive do
  use OTPSupervisorWeb, :live_view
  
  alias OTPSupervisor.Core.Control

  @refresh_interval 1000

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(@refresh_interval, self(), :refresh)
    end

    {:ok,
     socket
     |> assign(:supervisors, Control.list_supervisors())
     |> assign(:selected_supervisor, nil)
     |> assign(:children, [])
     |> assign(:selected_process, nil)
     |> assign(:process_info, nil)}
  end

  @impl true
  def handle_params(params, _url, socket) do
    supervisor_name = params["supervisor"]
    
    socket = if supervisor_name do
      select_supervisor(socket, supervisor_name)
    else
      socket
    end
    
    {:noreply, socket}
  end

  @impl true
  def handle_event("select_supervisor", %{"name" => name}, socket) do
    {:noreply, 
     socket
     |> select_supervisor(name)
     |> push_patch(to: ~p"/supervisors?supervisor=#{name}")}
  end

  @impl true
  def handle_event("kill_process", %{"pid" => pid_string}, socket) do
    Control.kill_process(pid_string)
    
    # Refresh immediately to show the change
    send(self(), :refresh)
    
    {:noreply, put_flash(socket, :info, "Process killed: #{pid_string}")}
  end

  @impl true
  def handle_event("select_process", %{"pid" => pid_string}, socket) do
    pid = String.to_charlist(pid_string) |> :erlang.list_to_pid()
    
    process_info = case Control.get_process_info(pid) do
      {:ok, info} -> info
      {:error, _} -> nil
    end
    
    {:noreply,
     socket
     |> assign(:selected_process, pid_string)
     |> assign(:process_info, process_info)}
  end

  @impl true
  def handle_info(:refresh, socket) do
    socket = socket
    |> assign(:supervisors, Control.list_supervisors())
    
    socket = if socket.assigns.selected_supervisor do
      refresh_children(socket)
    else
      socket
    end
    
    {:noreply, socket}
  end

  # Private functions

  defp select_supervisor(socket, name) when is_binary(name) do
    select_supervisor(socket, String.to_existing_atom(name))
  rescue
    _ -> socket
  end

  defp select_supervisor(socket, name) when is_atom(name) do
    case Control.get_supervision_tree(name) do
      {:ok, children} ->
        socket
        |> assign(:selected_supervisor, name)
        |> assign(:children, children)
      {:error, _} ->
        socket
    end
  end

  defp refresh_children(socket) do
    case Control.get_supervision_tree(socket.assigns.selected_supervisor) do
      {:ok, children} -> assign(socket, :children, children)
      {:error, _} -> socket
    end
  end
end
```

### 4.2 Create the LiveView Template

```elixir
# lib/otp_supervisor_web/live/supervisor_live.html.heex
<div class="container mx-auto px-4 py-8">
  <h1 class="text-3xl font-bold mb-8">OTP Supervisor Control Panel</h1>
  
  <div class="grid grid-cols-1 lg:grid-cols-3 gap-6">
    <!-- Supervisors List -->
    <div class="bg-white rounded-lg shadow p-6">
      <h2 class="text-xl font-semibold mb-4">Supervisors</h2>
      
      <div class="space-y-2">
        <%= for supervisor <- @supervisors do %>
          <button
            phx-click="select_supervisor"
            phx-value-name={supervisor.name}
            class={"w-full text-left p-3 rounded transition-colors " <>
              if(@selected_supervisor == supervisor.name,
                do: "bg-blue-100 border-blue-500 border",
                else: "bg-gray-50 hover:bg-gray-100"
              )}
          >
            <div class="font-medium"><%= supervisor.name %></div>
            <div class="text-sm text-gray-600">
              PID: <%= supervisor.pid %>
            </div>
            <div class="text-sm text-gray-600">
              Children: <%= supervisor.child_count %>
            </div>
          </button>
        <% end %>
      </div>
    </div>
    
    <!-- Children/Processes -->
    <div class="bg-white rounded-lg shadow p-6">
      <h2 class="text-xl font-semibold mb-4">
        <%= if @selected_supervisor do %>
          Children of <%= @selected_supervisor %>
        <% else %>
          Select a Supervisor
        <% end %>
      </h2>
      
      <%= if @selected_supervisor do %>
        <div class="space-y-3">
          <%= for child <- @children do %>
            <div class="border rounded p-3">
              <div class="flex justify-between items-start">
                <div class="flex-1">
                  <div class="font-medium"><%= child.id %></div>
                  <div class="text-sm text-gray-600">
                    PID: 
                    <button
                      phx-click="select_process"
                      phx-value-pid={child.pid}
                      class="text-blue-600 hover:underline"
                    >
                      <%= child.pid %>
                    </button>
                  </div>
                  <div class="text-sm text-gray-600">
                    Type: <%= child.type %>
                  </div>
                  <%= if child.info[:memory] do %>
                    <div class="text-sm text-gray-600">
                      Memory: <%= format_bytes(child.info.memory) %>
                    </div>
                  <% end %>
                </div>
                
                <div class="ml-4">
                  <span class={"inline-block px-2 py-1 text-xs rounded " <>
                    if(child.alive,
                      do: "bg-green-100 text-green-800",
                      else: "bg-red-100 text-red-800"
                    )}
                  >
                    <%= if child.alive, do: "Alive", else: "Dead" %>
                  </span>
                  
                  <%= if child.alive do %>
                    <button
                      phx-click="kill_process"
                      phx-value-pid={child.pid}
                      class="mt-2 block px-3 py-1 bg-red-500 text-white text-sm rounded hover:bg-red-600"
                    >
                      Kill
                    </button>
                  <% end %>
                </div>
              </div>
            </div>
          <% end %>
        </div>
      <% else %>
        <p class="text-gray-500">
          Select a supervisor from the list to see its children.
        </p>
      <% end %>
    </div>
    
    <!-- Process Details -->
    <div class="bg-white rounded-lg shadow p-6">
      <h2 class="text-xl font-semibold mb-4">Process Details</h2>
      
      <%= if @process_info do %>
        <div class="space-y-2">
          <div>
            <span class="font-medium">PID:</span> 
            <%= @selected_process %>
          </div>
          
          <%= for {key, value} <- @process_info do %>
            <div>
              <span class="font-medium"><%= format_key(key) %>:</span>
              <span class="text-gray-700"><%= format_value(value) %></span>
            </div>
          <% end %>
        </div>
      <% else %>
        <p class="text-gray-500">
          Click on a PID to see process details.
        </p>
      <% end %>
    </div>
  </div>
  
  <!-- Educational Info -->
  <div class="mt-8 bg-blue-50 rounded-lg p-6">
    <h3 class="text-lg font-semibold mb-2">How Supervisors Work</h3>
    <p class="text-gray-700 mb-4">
      Supervisors automatically restart their child processes when they crash. 
      The current demo uses the <code class="bg-white px-1 py-0.5 rounded">:one_for_one</code> 
      strategy, which means only the crashed process is restarted.
    </p>
    <p class="text-gray-700">
      Try killing a process and watch its PID change when the supervisor restarts it!
      Notice how the other processes keep their same PIDs.
    </p>
  </div>
</div>
```

### 4.3 Add Helper Functions to LiveView

Add these helper functions to the LiveView module:

```elixir
# Add to lib/otp_supervisor_web/live/supervisor_live.ex

defp format_bytes(bytes) when is_integer(bytes) do
  cond do
    bytes < 1024 -> "#{bytes} B"
    bytes < 1024 * 1024 -> "#{Float.round(bytes / 1024, 1)} KB"
    true -> "#{Float.round(bytes / (1024 * 1024), 1)} MB"
  end
end

defp format_key(key) do
  key
  |> Atom.to_string()
  |> String.replace("_", " ")
  |> String.split()
  |> Enum.map(&String.capitalize/1)
  |> Enum.join(" ")
end

defp format_value(value) when is_function(value) do
  inspect(value)
end

defp format_value(value) when is_integer(value) and value > 1000 do
  Number.Delimit.number_to_delimited(value)
end

defp format_value(value) do
  to_string(value)
end
```

## Step 5: Router Configuration

```elixir
# lib/otp_supervisor_web/router.ex
defmodule OTPSupervisorWeb.Router do
  use OTPSupervisorWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {OTPSupervisorWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", OTPSupervisorWeb do
    pipe_through :browser

    # Redirect root to supervisors
    get "/", PageController, :home
    
    # Main LiveView route
    live "/supervisors", SupervisorLive
  end
end
```

## Step 6: Simple Home Page

```elixir
# lib/otp_supervisor_web/controllers/page_controller.ex
defmodule OTPSupervisorWeb.PageController do
  use OTPSupervisorWeb, :controller

  def home(conn, _params) do
    redirect(conn, to: ~p"/supervisors")
  end
end
```

## Step 7: Testing

### 7.1 Test the Control Module

```elixir
# test/otp_supervisor/core/control_test.exs
defmodule OTPSupervisor.Core.ControlTest do
  use ExUnit.Case
  
  alias OTPSupervisor.Core.Control
  alias OTPSupervisor.Sandbox.Workers.Counter

  setup do
    # Start a test supervisor
    children = [
      {Counter, name: :test_counter}
    ]
    
    {:ok, sup} = Supervisor.start_link(
      children, 
      strategy: :one_for_one, 
      name: :test_supervisor
    )
    
    on_exit(fn ->
      if Process.alive?(sup), do: Supervisor.stop(sup)
    end)
    
    {:ok, supervisor: sup}
  end

  test "list_supervisors includes test supervisor" do
    supervisors = Control.list_supervisors()
    
    assert Enum.any?(supervisors, fn s -> 
      s.name == :test_supervisor 
    end)
  end

  test "get_supervision_tree returns children" do
    assert {:ok, children} = Control.get_supervision_tree(:test_supervisor)
    assert length(children) == 1
    assert hd(children).id == :test_counter
  end

  test "kill_process terminates the process" do
    [{_, pid, _, _}] = Supervisor.which_children(:test_supervisor)
    assert Process.alive?(pid)
    
    Control.kill_process(pid)
    Process.sleep(10)
    
    refute Process.alive?(pid)
  end
end
```

## Step 8: Running and Testing

### 8.1 Start the Application

```bash
# Run the server
mix phx.server

# Or run with IEx for debugging
iex -S mix phx.server
```

### 8.2 Manual Testing Steps

1. Visit http://localhost:4000 - should redirect to /supervisors
2. Click on `:demo_one_for_one` supervisor
3. See the three child processes
4. Click "Kill" on any process
5. Watch the PID change as it restarts
6. Click on a PID to see process details

### 8.3 Experiment in IEx

```elixir
# Connect to running system
iex -S mix phx.server

# Interact with workers
:counter_1 |> OTPSupervisor.Sandbox.Workers.Counter.increment()
:counter_1 |> OTPSupervisor.Sandbox.Workers.Counter.get_value()

# Cause intentional crash
:counter_1 |> OTPSupervisor.Sandbox.Workers.Counter.crash()

# Check if it restarted with fresh state
:counter_1 |> OTPSupervisor.Sandbox.Workers.Counter.get_value()
```

## Phase 1 Deliverables Checklist

- [x] Phoenix application with LiveView
- [x] Core supervisor control module
- [x] Basic sandbox application with workers
- [x] LiveView UI showing supervisors and processes
- [x] Kill button functionality
- [x] Auto-refresh every second
- [x] Process detail view
- [x] Educational information panel
- [x] Basic tests

## Next Steps (Phase 2 Preview)

With Phase 1 complete, you have a working supervisor control system. Phase 2 will add:

1. Phoenix LiveDashboard integration
2. Multiple supervisor strategies (all_for_one, rest_for_one)
3. REST API endpoints
4. Enhanced process inspection
5. Metrics collection

## Troubleshooting

### Common Issues

1. **"No process" error** - The supervisor or process doesn't exist. Check names.
2. **Supervisor not showing** - Make sure it's registered with a name.
3. **Page not updating** - Check WebSocket connection in browser console.

### Debug Commands

```elixir
# List all registered processes
Process.registered()

# Check if supervisor is running
Process.whereis(:demo_one_for_one)

# Manually inspect supervisor
Supervisor.which_children(:demo_one_for_one)
```

This completes Phase 1 implementation with a fully functional supervisor control panel!