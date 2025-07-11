defmodule OtpSupervisorWeb.Live.SupervisorLive do
  use Phoenix.LiveView

  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalMetricWidget
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OtpSupervisorWeb.Components.Layout.TerminalPanelLayout
  alias OtpSupervisorWeb.Components.Widgets.SupervisorTreeWidget
  alias OtpSupervisorWeb.Components.Widgets.ProcessListWidget
  alias OtpSupervisorWeb.Components.Widgets.SandboxManagementWidget

  @moduledoc """
  OTP supervisor monitoring and control interface.

  Refactored to use LiveComponents for better reusability and maintainability.
  """

  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(2000, self(), :update_supervisors)
      Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "supervisor_updates")
    end

    {:ok,
     socket
     |> assign(:page_title, "Supervisor Monitor")
     |> assign(:current_page, "supervisor")
     |> assign(:selected_supervisor, nil)
     |> assign(:show_children_panel, false)
     |> assign(:supervisor_count, 0)
     |> assign(:sandboxes, [])
     |> assign(:selected_sandbox, nil)
     |> load_supervisor_data()
     |> load_sandbox_data()}
  end

  def handle_info(:update_supervisors, socket) do
    {:noreply, update_supervisor_data(socket)}
  end

  def handle_info(:refresh_sandbox_data, socket) do
    {:noreply, update_sandbox_data(socket)}
  end

  def handle_info({:supervisor_update, data}, socket) do
    {:noreply, handle_supervisor_update(socket, data)}
  end

  def handle_info({:supervisor_selected, supervisor_id}, socket) do
    supervisor = find_supervisor(supervisor_id, socket.assigns.supervisors)
    children = get_supervisor_children(supervisor_id)

    {:noreply,
     socket
     |> assign(:selected_supervisor, supervisor)
     |> assign(:supervisor_children, children)
     |> assign(:show_children_panel, true)}
  end

  def handle_info({:fetch_supervisor_children, supervisor_id}, socket) do
    children = get_supervisor_children(supervisor_id)

    updated_children_map =
      Map.put(socket.assigns[:children_by_supervisor] || %{}, supervisor_id, children)

    # Update the supervisor tree widget with the children
    Phoenix.LiveView.send_update(
      OtpSupervisorWeb.Components.Widgets.SupervisorTreeWidget,
      id: "supervisor-tree",
      children_by_supervisor: updated_children_map
    )

    {:noreply, assign(socket, :children_by_supervisor, updated_children_map)}
  end

  def render(assigns) do
    ~H"""
    <div class="fixed inset-0 bg-gray-900 text-green-400 flex flex-col">
      <!-- Status Bar -->
      <.live_component
        module={TerminalStatusBar}
        id="supervisor-status-bar"
        title="Supervisor Monitor"
        metrics={status_bar_metrics(assigns)}
        navigation_links={TerminalNavigationLinks.page_navigation_links("supervisor", %{})}
      />
      
    <!-- Main Layout -->
      <.live_component
        module={TerminalPanelLayout}
        id="supervisor-panel-layout"
        layout_type={if(@show_children_panel, do: :three_panel, else: :three_panel)}
        panels={supervisor_panels(assigns)}
        gap="gap-4"
        padding="p-4"
      />
    </div>
    """
  end

  # Event handlers

  def handle_event("select_supervisor", %{"supervisor_id" => supervisor_id}, socket) do
    supervisor = find_supervisor(supervisor_id, socket.assigns.supervisors)
    children = get_supervisor_children(supervisor_id)

    {:noreply,
     socket
     |> assign(:selected_supervisor, supervisor)
     |> assign(:supervisor_children, children)
     |> assign(:show_children_panel, true)}
  end

  def handle_event("restart_supervisor", %{"supervisor_id" => supervisor_id}, socket) do
    # Here you would call your supervisor management API
    {:noreply, put_flash(socket, :info, "Supervisor #{supervisor_id} restarted")}
  end

  def handle_event("stop_supervisor", %{"supervisor_id" => supervisor_id}, socket) do
    # Here you would call your supervisor management API
    {:noreply, put_flash(socket, :info, "Supervisor #{supervisor_id} stopped")}
  end

  def handle_event("start_child", %{"child_id" => child_id}, socket) do
    # Here you would call your child process management API
    {:noreply, put_flash(socket, :info, "Child process #{child_id} started")}
  end

  def handle_event("stop_child", %{"child_id" => child_id}, socket) do
    # Here you would call your child process management API
    {:noreply, put_flash(socket, :info, "Child process #{child_id} stopped")}
  end

  def handle_event("restart_child", %{"child_id" => child_id}, socket) do
    # Here you would call your child process management API
    {:noreply, put_flash(socket, :info, "Child process #{child_id} restarted")}
  end

  def handle_event("close_children_panel", _params, socket) do
    {:noreply,
     socket
     |> assign(:selected_supervisor, nil)
     |> assign(:show_children_panel, false)}
  end

  def handle_event("refresh_supervisors", _params, socket) do
    {:noreply, update_supervisor_data(socket)}
  end

  # Private functions

  defp load_supervisor_data(socket) do
    supervisors = get_supervisors()

    socket
    |> assign(:supervisors, supervisors)
    |> assign(:supervisor_count, length(supervisors))
    |> assign(:supervisor_children, [])
    |> assign(:supervisor_health, get_supervisor_health())
    |> assign(:children_by_supervisor, %{})
  end

  defp load_sandbox_data(socket) do
    sandboxes = get_sandboxes()

    socket
    |> assign(:sandboxes, sandboxes)
  end

  defp update_supervisor_data(socket) do
    supervisors = get_supervisors()

    socket
    |> assign(:supervisors, supervisors)
    |> assign(:supervisor_count, length(supervisors))
    |> assign(:supervisor_health, get_supervisor_health())
    |> assign(:children_by_supervisor, socket.assigns[:children_by_supervisor] || %{})
    |> maybe_update_children()
  end

  defp update_sandbox_data(socket) do
    sandboxes = get_sandboxes()

    socket
    |> assign(:sandboxes, sandboxes)
  end

  defp maybe_update_children(socket) do
    if socket.assigns.selected_supervisor do
      children = get_supervisor_children(socket.assigns.selected_supervisor.id)
      assign(socket, :supervisor_children, children)
    else
      socket
    end
  end

  defp handle_supervisor_update(socket, %{type: :supervisor_list, data: supervisors}) do
    socket
    |> assign(:supervisors, supervisors)
    |> assign(:supervisor_count, length(supervisors))
  end

  defp handle_supervisor_update(socket, %{type: :supervisor_health, data: health}) do
    assign(socket, :supervisor_health, health)
  end

  defp handle_supervisor_update(socket, _data), do: socket

  defp status_bar_metrics(assigns) do
    [
      %{label: "Supervisors", value: "#{assigns.supervisor_count}"},
      %{label: "Health", value: supervisor_health_summary(assigns.supervisor_health)},
      %{
        label: "Selected",
        value: if(assigns.selected_supervisor, do: assigns.selected_supervisor.name, else: "None")
      },
      %{label: "Children", value: "#{length(assigns.supervisor_children || [])}"}
    ]
  end

  defp supervisor_panels(assigns) do
    if assigns.show_children_panel do
      [
        # Left panel: Supervisor Tree Widget
        %{
          title: "Supervisor Tree",
          component: SupervisorTreeWidget,
          assigns: %{
            id: "supervisor-tree",
            supervisors: supervisor_tree_data(assigns),
            selected_supervisor: assigns.selected_supervisor,
            show_children: true,
            compact_mode: false,
            children_by_supervisor: assigns[:children_by_supervisor] || %{}
          },
          span: %{cols: 1, rows: 1}
        },

        # Right panel: Children ProcessListWidget
        %{
          title: "Children of #{assigns.selected_supervisor.name}",
          component: ProcessListWidget,
          assigns: %{
            id: "children-process-list",
            processes: supervisor_children_data(assigns),
            selected_process: nil,
            real_time: true,
            show_actions: true,
            compact_mode: true,
            filters: %{
              show_system_processes: true,
              parent_filter: assigns.selected_supervisor.id
            }
          },
          actions: [
            %{type: :button, label: "Close", event: "close_children_panel"}
          ],
          span: %{cols: 1, rows: 1}
        },

        # Bottom panel: Sandbox Management Widget
        %{
          title: "Sandbox Manager",
          component: SandboxManagementWidget,
          assigns: %{
            id: "sandbox-management",
            sandboxes: assigns.sandboxes,
            selected_sandbox: assigns.selected_sandbox,
            show_create_form: false
          },
          span: %{cols: 2, rows: 1}
        }
      ]
    else
      [
        # Left panel: Supervisor Tree Widget
        %{
          title: "Supervisor Tree",
          component: SupervisorTreeWidget,
          assigns: %{
            id: "supervisor-tree",
            supervisors: supervisor_tree_data(assigns),
            selected_supervisor: assigns.selected_supervisor,
            show_children: true,
            compact_mode: false,
            children_by_supervisor: assigns[:children_by_supervisor] || %{}
          },
          span: %{cols: 1, rows: 1}
        },

        # Right panel: Supervisor Health Overview
        %{
          title: "Supervisor Health",
          component: TerminalMetricWidget,
          assigns: %{
            id: "supervisor-health",
            title: "Supervisor Health",
            metrics: health_metrics(assigns.supervisor_health),
            size: :medium
          },
          span: %{cols: 1, rows: 1}
        },

        # Bottom panel: Sandbox Management Widget
        %{
          title: "Sandbox Manager",
          component: SandboxManagementWidget,
          assigns: %{
            id: "sandbox-management",
            sandboxes: assigns.sandboxes,
            selected_sandbox: assigns.selected_sandbox,
            show_create_form: false
          },
          span: %{cols: 2, rows: 1}
        }
      ]
    end
  end

  defp health_metrics(health) do
    [
      %{label: "Healthy", value: health.healthy, format: :number, status: :success},
      %{label: "Warning", value: health.warning, format: :number, status: :warning},
      %{label: "Critical", value: health.critical, format: :number, status: :error},
      %{label: "Total", value: health.total, format: :number}
    ]
  end

  defp supervisor_health_summary(health) do
    cond do
      health.critical > 0 -> "Critical"
      health.warning > 0 -> "Warning"
      health.healthy > 0 -> "Healthy"
      true -> "Unknown"
    end
  end

  # Real supervisor data functions

  defp get_supervisors do
    # Use the Arsenal API for consistency with REST endpoints
    with {:ok, validated_params} <-
           OTPSupervisor.Core.Arsenal.Operations.ListSupervisors.validate_params(%{}),
         {:ok, {supervisors, _meta}} <-
           OTPSupervisor.Core.Arsenal.Operations.ListSupervisors.execute(validated_params) do
      Enum.map(supervisors, &format_arsenal_supervisor_for_display/1)
    else
      {:error, reason} ->
        # Log the error and fallback to Control module
        IO.puts("Arsenal API failed: #{inspect(reason)}")

        OTPSupervisor.Core.Control.list_supervisors()
        |> Enum.map(&format_supervisor_for_display/1)
    end
  end

  defp format_arsenal_supervisor_for_display(supervisor) do
    # Handle both atom and string names from Arsenal
    supervisor_name =
      case supervisor.name do
        name when is_atom(name) -> name
        name when is_binary(name) -> String.to_existing_atom(name)
        name -> name
      end

    %{
      id: to_string(supervisor.name),
      name: to_string(supervisor.name),
      pid: supervisor.pid,
      strategy: Map.get(supervisor, :strategy, get_supervisor_strategy(supervisor_name)),
      children_count: max(Map.get(supervisor, :child_count, 0), 0),
      status: if(supervisor.alive, do: :running, else: :stopped),
      uptime: get_supervisor_uptime(supervisor_name),
      memory: get_supervisor_memory(supervisor_name),
      restart_count: get_supervisor_restart_count(supervisor_name)
    }
  end

  # Keep the old function for backward compatibility during transition
  defp format_supervisor_for_display(supervisor) do
    %{
      id: to_string(supervisor.name),
      name: to_string(supervisor.name),
      pid: supervisor.pid,
      strategy: get_supervisor_strategy(supervisor.name),
      children_count: max(supervisor.child_count, 0),
      status: if(supervisor.alive, do: :running, else: :stopped),
      uptime: get_supervisor_uptime(supervisor.name),
      memory: get_supervisor_memory(supervisor.name),
      restart_count: get_supervisor_restart_count(supervisor.name)
    }
  end

  defp get_supervisor_children(supervisor_id) do
    supervisor_name = String.to_atom(supervisor_id)

    OTPSupervisor.Core.Control.get_supervisor_children(supervisor_name)
    |> Enum.map(&format_child_for_display(&1, supervisor_id))
  end

  defp format_child_for_display(child, supervisor_id) do
    pid_value = Map.get(child, :pid, :undefined)
    child_id = Map.get(child, :id, "unknown")
    child_type = Map.get(child, :type, :worker)

    %{
      id: "child_#{supervisor_id}_#{child_id}",
      name: to_string(child_id),
      pid: pid_value,
      type: child_type,
      status: if(pid_value != :undefined and pid_value != nil, do: :running, else: :stopped),
      memory: get_process_memory(pid_value),
      restart_count: Map.get(child, :restart_count, 0),
      supervisor_id: supervisor_id
    }
  end

  defp get_supervisor_health do
    supervisors = get_supervisors()

    healthy = Enum.count(supervisors, &(&1.status == :running))
    stopped = Enum.count(supervisors, &(&1.status == :stopped))
    error = Enum.count(supervisors, &(&1.status == :error))

    %{
      healthy: healthy,
      warning: stopped,
      critical: error,
      total: healthy + stopped + error
    }
  end

  defp find_supervisor(supervisor_id, supervisors) do
    Enum.find(supervisors, &(&1.id == supervisor_id))
  end

  # New data transformation functions for specialized widgets

  defp supervisor_tree_data(assigns) do
    Enum.map(assigns.supervisors, fn supervisor ->
      %{
        id: supervisor.id,
        name: supervisor.name,
        pid: supervisor.pid,
        strategy: supervisor.strategy,
        children_count: supervisor.children_count,
        status: supervisor.status,
        uptime: supervisor.uptime,
        memory: supervisor.memory,
        restart_count: supervisor.restart_count,
        # Children are loaded dynamically when expanded
        children: []
      }
    end)
  end

  defp supervisor_children_data(assigns) do
    if assigns.supervisor_children do
      Enum.map(assigns.supervisor_children, fn child ->
        %{
          id: child.id,
          pid: child.pid,
          name: child.name,
          status: child.status,
          memory: child.memory,
          cpu_usage: :rand.uniform(50),
          uptime: :rand.uniform(3600),
          parent: assigns.selected_supervisor.name,
          parent_pid: assigns.selected_supervisor.pid,
          strategy: assigns.selected_supervisor.strategy,
          children: [],
          priority: :normal,
          command: "/usr/bin/#{child.name}",
          user: "system",
          type: child.type,
          restart_count: child.restart_count
        }
      end)
    else
      []
    end
  end

  # Helper functions for real supervisor data

  defp get_supervisor_strategy(supervisor_name) do
    try do
      case Process.whereis(supervisor_name) do
        nil ->
          :unknown

        pid ->
          case :sys.get_state(pid, 1000) do
            state when is_map(state) -> Map.get(state, :strategy, :one_for_one)
            _ -> :one_for_one
          end
      end
    rescue
      _ -> :one_for_one
    end
  end

  defp get_supervisor_uptime(supervisor_name) do
    try do
      case Process.whereis(supervisor_name) do
        nil ->
          0

        pid ->
          case Process.info(pid, :current_function) do
            nil -> 0
            # TODO: Calculate real uptime
            _ -> :rand.uniform(86400)
          end
      end
    rescue
      _ -> 0
    end
  end

  defp get_supervisor_memory(supervisor_name) do
    try do
      case Process.whereis(supervisor_name) do
        nil ->
          0

        pid ->
          case Process.info(pid, :memory) do
            {:memory, memory} -> memory
            _ -> 0
          end
      end
    rescue
      _ -> 0
    end
  end

  defp get_supervisor_restart_count(supervisor_name) do
    try do
      case OTPSupervisor.Core.Control.get_restart_history(supervisor_name) do
        {:ok, history} -> length(history)
        _ -> 0
      end
    rescue
      _ -> 0
    end
  end

  defp get_process_memory(pid) when is_pid(pid) do
    try do
      case Process.info(pid, :memory) do
        {:memory, memory} -> memory
        _ -> 0
      end
    rescue
      _ -> 0
    end
  end

  defp get_process_memory(pid_string) when is_binary(pid_string) do
    case OTPSupervisor.Core.Control.to_pid(pid_string) do
      {:ok, pid} -> get_process_memory(pid)
      _ -> 0
    end
  end

  defp get_process_memory(_), do: 0

  # Format functions for compatibility with existing tests
  def format_bytes(bytes) when is_integer(bytes) do
    cond do
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 2)} GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 2)} MB"
      bytes >= 1024 -> "#{Float.round(bytes / 1024, 2)} KB"
      true -> "#{bytes} B"
    end
  end

  def format_bytes(_), do: "N/A"

  def format_key(key) when is_atom(key) do
    key
    |> Atom.to_string()
    |> String.split("_")
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end

  def format_value(value) when is_integer(value), do: to_string(value)
  def format_value(value) when is_atom(value), do: inspect(value)

  def format_value({m, f, a}) when is_atom(m) and is_atom(f) and is_integer(a) do
    "#{m}.#{f}/#{a}"
  end

  def format_value(value), do: inspect(value)

  # Sandbox data functions

  defp get_sandboxes do
    try do
      # Use Arsenal operation to get sandboxes - validate params first
      params = %{
        "status" => nil,
        "page" => 1,
        "per_page" => 100
      }
      
      case OTPSupervisor.Core.Arsenal.Operations.ListSandboxes.validate_params(params) do
        {:ok, validated_params} ->
          case OTPSupervisor.Core.Arsenal.Operations.ListSandboxes.execute(validated_params) do
            {:ok, {sandboxes, _meta}} -> 
              # Format for display
              Enum.map(sandboxes, &format_sandbox_for_display/1)
            
            {:error, _reason} ->
              # Fallback to direct SandboxManager call
              OTPSupervisor.Core.SandboxManager.list_sandboxes()
              |> Enum.map(&format_sandbox_for_display/1)
          end
        
        {:error, _reason} ->
          # Fallback to direct SandboxManager call
          OTPSupervisor.Core.SandboxManager.list_sandboxes()
          |> Enum.map(&format_sandbox_for_display/1)
      end
    rescue
      _ ->
        # Fallback to direct SandboxManager call
        try do
          OTPSupervisor.Core.SandboxManager.list_sandboxes()
          |> Enum.map(&format_sandbox_for_display/1)
        rescue
          _ -> []
        end
    end
  end

  defp format_sandbox_for_display(sandbox) do
    status = if Process.alive?(sandbox.app_pid), do: "running", else: "stopped"
    
    %{
      id: sandbox.id,
      app_name: sandbox.app_name,
      supervisor_module: format_sandbox_module_name(sandbox.supervisor_module),
      app_pid: inspect(sandbox.app_pid),
      supervisor_pid: inspect(sandbox.supervisor_pid),
      status: status,
      created_at: sandbox.created_at,
      restart_count: sandbox.restart_count,
      opts: sandbox.opts
    }
  end

  defp format_sandbox_module_name(module) when is_atom(module) do
    module
    |> Atom.to_string()
    |> String.split(".")
    |> List.last()
  end

  defp format_sandbox_module_name(module), do: inspect(module)
end
