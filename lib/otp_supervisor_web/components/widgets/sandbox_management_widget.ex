defmodule OtpSupervisorWeb.Components.Widgets.SandboxManagementWidget do
  use Phoenix.LiveComponent

  @moduledoc """
  Sandbox management widget for creating, monitoring, and controlling sandbox environments.

  Provides a comprehensive interface for:
  - Listing all active sandboxes
  - Creating new sandboxes with custom configurations
  - Destroying sandboxes safely
  - Restarting sandboxes
  - Viewing detailed sandbox information
  
  All operations are performed using Arsenal API for consistency.
  """

  attr :sandboxes, :list, default: []
  attr :selected_sandbox, :map, default: nil
  attr :show_create_form, :boolean, default: false
  attr :class, :string, default: ""

  def render(assigns) do
    ~H"""
    <div class={[
      "bg-gray-900 border border-green-500/30 rounded text-green-400 h-full flex flex-col",
      @class
    ]}>
      <!-- Header -->
      <div class="flex items-center justify-between p-3 border-b border-green-500/20">
        <h3 class="text-sm font-mono font-bold text-green-300">
          Sandbox Manager
        </h3>
        <div class="flex items-center space-x-2">
          <button
            phx-target={@myself}
            phx-click="toggle_create_form"
            class="px-2 py-1 text-xs bg-blue-500/20 border border-blue-500/30 rounded text-blue-400 font-mono hover:bg-blue-500/30 transition-colors"
          >
            <%= if @show_create_form, do: "Cancel", else: "New Sandbox" %>
          </button>
          <button
            phx-target={@myself}
            phx-click="refresh_sandboxes"
            class="px-2 py-1 text-xs bg-green-500/20 border border-green-500/30 rounded text-green-400 font-mono hover:bg-green-500/30 transition-colors"
          >
            Refresh
          </button>
        </div>
      </div>

      <!-- Create Form -->
      <%= if @show_create_form do %>
        <div class="p-3 border-b border-green-500/20 bg-gray-800/50">
          <.create_sandbox_form myself={@myself} />
        </div>
      <% end %>

      <!-- Sandbox List -->
      <div class="flex-1 overflow-auto">
        <%= if @sandboxes == [] do %>
          <div class="text-center py-8">
            <div class="text-green-400/50 text-sm font-mono mb-2">No sandboxes found</div>
            <button
              phx-target={@myself}
              phx-click="toggle_create_form"
              class="px-3 py-1 text-xs bg-blue-500/20 border border-blue-500/30 rounded text-blue-400 font-mono hover:bg-blue-500/30 transition-colors"
            >
              Create First Sandbox
            </button>
          </div>
        <% else %>
          <div class="space-y-2 p-3">
            <%= for sandbox <- @sandboxes do %>
              <.sandbox_card 
                sandbox={sandbox} 
                selected={@selected_sandbox && @selected_sandbox.id == sandbox.id}
                myself={@myself}
              />
            <% end %>
          </div>
        <% end %>
      </div>

      <!-- Selected Sandbox Details -->
      <%= if @selected_sandbox do %>
        <div class="border-t border-green-500/20 p-3 bg-gray-800/30">
          <.sandbox_details sandbox={@selected_sandbox} myself={@myself} />
        </div>
      <% end %>
    </div>
    """
  end

  def mount(socket) do
    {:ok,
     socket
     |> assign(:sandboxes, [])
     |> assign(:selected_sandbox, nil)
     |> assign(:show_create_form, false)
     |> assign(:loading, false)}
  end

  def update(assigns, socket) do
    {:ok, assign(socket, assigns)}
  end

  # Event handlers

  def handle_event("toggle_create_form", _params, socket) do
    {:noreply, assign(socket, :show_create_form, not socket.assigns.show_create_form)}
  end

  def handle_event("refresh_sandboxes", _params, socket) do
    send(self(), :refresh_sandbox_data)
    {:noreply, assign(socket, :loading, true)}
  end

  def handle_event("select_sandbox", %{"sandbox_id" => sandbox_id}, socket) do
    sandbox = Enum.find(socket.assigns.sandboxes, &(&1.id == sandbox_id))
    {:noreply, assign(socket, :selected_sandbox, sandbox)}
  end

  def handle_event("create_sandbox", params, socket) do
    case create_sandbox_via_arsenal(params) do
      {:ok, _sandbox_info} ->
        # Refresh both sandbox data and supervisor data
        send(self(), :refresh_sandbox_data)
        send(self(), :update_supervisors)
        {:noreply, 
         socket
         |> assign(:show_create_form, false)
         |> put_flash(:info, "Sandbox created successfully")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to create sandbox: #{inspect(reason)}")}
    end
  end

  def handle_event("destroy_sandbox", %{"sandbox_id" => sandbox_id}, socket) do
    case destroy_sandbox_via_arsenal(sandbox_id) do
      {:ok, _result} ->
        # Refresh both sandbox data and supervisor data
        send(self(), :refresh_sandbox_data)
        send(self(), :update_supervisors)
        selected_sandbox = if socket.assigns.selected_sandbox && socket.assigns.selected_sandbox.id == sandbox_id do
          nil
        else
          socket.assigns.selected_sandbox
        end
        
        {:noreply,
         socket
         |> assign(:selected_sandbox, selected_sandbox)
         |> put_flash(:info, "Sandbox destroyed successfully")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to destroy sandbox: #{inspect(reason)}")}
    end
  end

  def handle_event("restart_sandbox", %{"sandbox_id" => sandbox_id}, socket) do
    case restart_sandbox_via_arsenal(sandbox_id) do
      {:ok, _result} ->
        # Refresh both sandbox data and supervisor data
        send(self(), :refresh_sandbox_data)
        send(self(), :update_supervisors)
        {:noreply, put_flash(socket, :info, "Sandbox restarted successfully")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to restart sandbox: #{inspect(reason)}")}
    end
  end

  # Private components

  defp create_sandbox_form(assigns) do
    ~H"""
    <form phx-target={@myself} phx-submit="create_sandbox" class="space-y-3">
      <div class="text-sm font-mono font-bold text-green-300 mb-2">Create New Sandbox</div>
      
      <div class="grid grid-cols-2 gap-3">
        <div>
          <label class="block text-xs text-green-400/70 mb-1">Sandbox ID</label>
          <input
            type="text"
            name="sandbox_id"
            required
            placeholder="my-sandbox-1"
            pattern="[a-zA-Z0-9_-]+"
            class="w-full px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono placeholder-green-400/50 focus:outline-none focus:ring-1 focus:ring-green-500/50"
          />
        </div>
        
        <div>
          <label class="block text-xs text-green-400/70 mb-1">Supervisor Module</label>
          <select
            name="supervisor_module"
            required
            class="w-full px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:ring-1 focus:ring-green-500/50"
          >
            <option value="OtpSandbox.TestDemoSupervisor">TestDemoSupervisor</option>
            <option value="OtpSandbox.Supervisors.DemoSupervisor">DemoSupervisor</option>
          </select>
        </div>
      </div>

      <div class="grid grid-cols-3 gap-3">
        <div>
          <label class="block text-xs text-green-400/70 mb-1">Strategy</label>
          <select
            name="strategy"
            class="w-full px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:ring-1 focus:ring-green-500/50"
          >
            <option value="one_for_one">one_for_one</option>
            <option value="one_for_all">one_for_all</option>
            <option value="rest_for_one">rest_for_one</option>
          </select>
        </div>
        
        <div>
          <label class="block text-xs text-green-400/70 mb-1">Max Restarts</label>
          <input
            type="number"
            name="max_restarts"
            value="3"
            min="1"
            max="10"
            class="w-full px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:ring-1 focus:ring-green-500/50"
          />
        </div>
        
        <div>
          <label class="block text-xs text-green-400/70 mb-1">Max Seconds</label>
          <input
            type="number"
            name="max_seconds"
            value="5"
            min="1"
            max="60"
            class="w-full px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:ring-1 focus:ring-green-500/50"
          />
        </div>
      </div>

      <div class="flex justify-end space-x-2">
        <button
          type="button"
          phx-target={@myself}
          phx-click="toggle_create_form"
          class="px-3 py-1 text-xs bg-gray-500/20 border border-gray-500/30 rounded text-gray-400 font-mono hover:bg-gray-500/30 transition-colors"
        >
          Cancel
        </button>
        <button
          type="submit"
          class="px-3 py-1 text-xs bg-blue-500/20 border border-blue-500/30 rounded text-blue-400 font-mono hover:bg-blue-500/30 transition-colors"
        >
          Create Sandbox
        </button>
      </div>
    </form>
    """
  end

  defp sandbox_card(assigns) do
    ~H"""
    <div class={[
      "p-3 rounded border transition-colors cursor-pointer",
      if(@selected, 
        do: "border-green-500/50 bg-green-500/10", 
        else: "border-green-500/20 hover:border-green-500/40 hover:bg-green-500/5")
    ]}>
      <div class="flex items-center justify-between">
        <button
          phx-target={@myself}
          phx-click="select_sandbox"
          phx-value-sandbox_id={@sandbox.id}
          class="flex-1 text-left"
        >
          <div class="flex items-center space-x-2">
            <span class={[
              "w-2 h-2 rounded-full",
              sandbox_status_color(@sandbox.status)
            ]}></span>
            <span class="font-mono text-sm text-green-400 font-bold">{@sandbox.id}</span>
            <span class="font-mono text-xs text-green-400/70">({@sandbox.app_name})</span>
          </div>
          <div class="text-xs text-green-400/60 mt-1">
            {format_module_name(@sandbox.supervisor_module)} • Restarts: {@sandbox.restart_count}
          </div>
        </button>
        
        <div class="flex items-center space-x-1">
          <button
            phx-target={@myself}
            phx-click="restart_sandbox"
            phx-value-sandbox_id={@sandbox.id}
            class="p-1 text-xs text-yellow-400 hover:text-yellow-300 transition-colors"
            title="Restart sandbox"
          >
            🔄
          </button>
          <button
            phx-target={@myself}
            phx-click="destroy_sandbox"
            phx-value-sandbox_id={@sandbox.id}
            class="p-1 text-xs text-red-400 hover:text-red-300 transition-colors"
            title="Destroy sandbox"
            data-confirm="Are you sure you want to destroy this sandbox?"
          >
            🗑️
          </button>
        </div>
      </div>
    </div>
    """
  end

  defp sandbox_details(assigns) do
    ~H"""
    <div class="space-y-2">
      <div class="text-sm font-mono font-bold text-green-300">
        {@sandbox.id} Details
      </div>
      
      <div class="grid grid-cols-2 gap-4 text-xs">
        <div>
          <span class="text-green-400/70">App PID:</span>
          <span class="text-green-400 font-mono ml-2">{@sandbox.app_pid}</span>
        </div>
        <div>
          <span class="text-green-400/70">Supervisor PID:</span>
          <span class="text-green-400 font-mono ml-2">{@sandbox.supervisor_pid}</span>
        </div>
        <div>
          <span class="text-green-400/70">Status:</span>
          <span class={[
            "font-mono ml-2",
            sandbox_status_text_color(@sandbox.status)
          ]}>
            {String.capitalize(@sandbox.status)}
          </span>
        </div>
        <div>
          <span class="text-green-400/70">Uptime:</span>
          <span class="text-green-400 font-mono ml-2">{format_uptime(@sandbox.created_at)}</span>
        </div>
      </div>

      <div class="pt-2 border-t border-green-500/20">
        <div class="text-xs text-green-400/70 mb-1">Configuration:</div>
        <div class="text-xs font-mono text-green-400">
          {format_opts(@sandbox.opts)}
        </div>
      </div>
    </div>
    """
  end

  # Helper functions for Arsenal API calls

  defp create_sandbox_via_arsenal(params) do
    # Validate parameters first
    case OTPSupervisor.Core.Arsenal.Operations.CreateSandbox.validate_params(params) do
      {:ok, validated_params} ->
        # Execute the creation operation
        case OTPSupervisor.Core.Arsenal.Operations.CreateSandbox.execute(validated_params) do
          {:ok, sandbox_info} ->
            formatted = OTPSupervisor.Core.Arsenal.Operations.CreateSandbox.format_response(sandbox_info)
            {:ok, formatted["data"]}
          {:error, reason} -> {:error, reason}
        end
      {:error, reason} -> {:error, reason}
    end
  end

  defp destroy_sandbox_via_arsenal(sandbox_id) do
    path = "/api/v1/sandboxes/#{sandbox_id}?force=false"
    case make_arsenal_request(:delete, path, %{}) do
      {:ok, %{"data" => result}} -> {:ok, result}
      {:error, reason} -> {:error, reason}
    end
  end

  defp restart_sandbox_via_arsenal(sandbox_id) do
    case make_arsenal_request(:post, "/api/v1/sandboxes/#{sandbox_id}/restart", %{}) do
      {:ok, %{"data" => result}} -> {:ok, result}
      {:error, reason} -> {:error, reason}
    end
  end

  defp make_arsenal_request(method, path, params) do
    # Simulate Arsenal API call for now - in a real implementation,
    # this would make an HTTP request to the Arsenal endpoints
    case method do
      :post when path == "/api/v1/sandboxes" ->
        # Create sandbox
        result = OTPSupervisor.Core.Arsenal.Operations.CreateSandbox.execute(params)
        case result do
          {:ok, sandbox_info} ->
            formatted = OTPSupervisor.Core.Arsenal.Operations.CreateSandbox.format_response(sandbox_info)
            {:ok, formatted}
          error -> error
        end

      :delete ->
        # Extract sandbox_id from path
        [sandbox_id | _] = String.split(path, "?") |> List.first() |> String.split("/") |> Enum.reverse()
        result = OTPSupervisor.Core.Arsenal.Operations.DestroySandbox.execute(%{"sandbox_id" => sandbox_id, "force" => false})
        case result do
          {:ok, result_data} ->
            formatted = OTPSupervisor.Core.Arsenal.Operations.DestroySandbox.format_response(result_data)
            {:ok, formatted}
          error -> error
        end

      :post ->
        # Extract sandbox_id from restart path
        [_ | rest] = String.split(path, "/") |> Enum.reverse()
        [sandbox_id | _] = rest
        result = OTPSupervisor.Core.Arsenal.Operations.RestartSandbox.execute(%{"sandbox_id" => sandbox_id})
        case result do
          {:ok, result_data} ->
            formatted = OTPSupervisor.Core.Arsenal.Operations.RestartSandbox.format_response(result_data)
            {:ok, formatted}
          error -> error
        end
    end
  rescue
    error -> {:error, error}
  end

  # Helper functions

  defp sandbox_status_color("running"), do: "bg-green-400"
  defp sandbox_status_color("stopped"), do: "bg-red-400"
  defp sandbox_status_color(_), do: "bg-gray-400"

  defp sandbox_status_text_color("running"), do: "text-green-400"
  defp sandbox_status_text_color("stopped"), do: "text-red-400"
  defp sandbox_status_text_color(_), do: "text-gray-400"

  defp format_module_name(module) when is_binary(module) do
    module
    |> String.split(".")
    |> List.last()
  end

  defp format_module_name(module), do: inspect(module)

  defp format_uptime(created_at) do
    current_time = System.system_time(:millisecond)
    uptime_seconds = div(current_time - created_at, 1000)
    
    cond do
      uptime_seconds < 60 -> "#{uptime_seconds}s"
      uptime_seconds < 3600 -> "#{div(uptime_seconds, 60)}m #{rem(uptime_seconds, 60)}s"
      true -> "#{div(uptime_seconds, 3600)}h #{div(rem(uptime_seconds, 3600), 60)}m"
    end
  end

  defp format_opts(opts) when is_list(opts) do
    opts
    |> Enum.map(fn {key, value} -> "#{key}: #{inspect(value)}" end)
    |> Enum.join(", ")
  end

  defp format_opts(opts), do: inspect(opts)
end