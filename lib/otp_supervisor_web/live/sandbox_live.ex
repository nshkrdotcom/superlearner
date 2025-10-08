defmodule OtpSupervisorWeb.Live.SandboxLive do
  use Phoenix.LiveView
  import Phoenix.Component

  alias OTPSupervisor.Core.Arsenal.Operations.ListSandboxes
  alias OtpSupervisorWeb.Components.Widgets.SandboxManagementWidget
  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks

  @impl true
  def render(assigns) do
    ~H"""
    <div class="fixed inset-0 bg-gray-900 text-green-400 flex flex-col">
      <!-- Status Bar -->
      <.live_component
        module={TerminalStatusBar}
        id="sandbox-status-bar"
        title="Sandbox Manager"
        metrics={status_bar_metrics(assigns)}
        navigation_links={TerminalNavigationLinks.page_navigation_links("sandbox", %{})}
      />
      
    <!-- Main Content -->
      <div class="flex-1 overflow-auto">
        <div class="container mx-auto px-4 py-8">
          <!-- Main Content Grid -->
          <div class="grid grid-cols-1 lg:grid-cols-3 gap-6">
            <!-- Sandbox Management Widget - Takes up full width on mobile, 2/3 on desktop -->
            <div class="lg:col-span-2">
              <.live_component
                module={SandboxManagementWidget}
                id="sandbox-manager"
                sandboxes={@sandboxes}
              />
            </div>
            
    <!-- Info Panel -->
            <div class="bg-gray-900 border border-green-500/30 rounded p-4 h-fit">
              <h3 class="text-sm font-bold text-green-300 mb-3">Quick Info</h3>

              <div class="space-y-4 text-xs">
                <div>
                  <h4 class="text-green-400 font-semibold mb-1">What are Sandboxes?</h4>

                  <p class="text-green-400/60">
                    Sandboxes are isolated OTP applications that run supervisor trees
                    independently from the main application.
                  </p>
                </div>

                <div>
                  <h4 class="text-green-400 font-semibold mb-1">Available Supervisors</h4>

                  <ul class="text-green-400/60 space-y-1">
                    <li>• TestDemoSupervisor - Simple demo with counter & printer workers</li>

                    <li>• DemoSupervisor - Production example supervisor</li>
                  </ul>
                </div>

                <div>
                  <h4 class="text-green-400 font-semibold mb-1">Strategies</h4>

                  <ul class="text-green-400/60 space-y-1">
                    <li>
                      • <span class="text-green-400">one_for_one</span> - Only restart failed child
                    </li>

                    <li>• <span class="text-green-400">one_for_all</span> - Restart all children</li>

                    <li>
                      • <span class="text-green-400">rest_for_one</span>
                      - Restart failed child and younger siblings
                    </li>
                  </ul>
                </div>

                <div>
                  <h4 class="text-green-400 font-semibold mb-1">Stats</h4>

                  <div class="text-green-400/60">
                    <p>Total Sandboxes: <span class="text-green-400">{length(@sandboxes)}</span></p>

                    <p>
                      Active: <span class="text-green-400">{count_active_sandboxes(@sandboxes)}</span>
                    </p>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Subscribe to sandbox events
      Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "sandboxes")

      # Poll for updates every 5 seconds
      :timer.send_interval(5_000, self(), :refresh_sandbox_data)
    end

    {:ok,
     socket
     |> assign(:page_title, "Sandbox Manager")
     |> assign(:current_page, "sandbox")
     |> assign(:sandboxes, [])
     |> load_sandboxes()}
  end

  @impl true
  def handle_info(:refresh_sandbox_data, socket) do
    {:noreply, load_sandboxes(socket)}
  end

  @impl true
  def handle_info({:sandbox_created, _sandbox}, socket) do
    {:noreply, load_sandboxes(socket)}
  end

  @impl true
  def handle_info({:sandbox_destroyed, _sandbox_id}, socket) do
    {:noreply, load_sandboxes(socket)}
  end

  @impl true
  def handle_info({:sandbox_restarted, _sandbox_id}, socket) do
    {:noreply, load_sandboxes(socket)}
  end

  defp load_sandboxes(socket) do
    case ListSandboxes.execute(%{}) do
      {:ok, {sandboxes, _meta}} ->
        # Format the sandboxes to match what the widget expects
        formatted_sandboxes =
          sandboxes
          |> Enum.map(&format_sandbox/1)
          |> Enum.sort_by(& &1.created_at, :desc)

        assign(socket, :sandboxes, formatted_sandboxes)

      {:error, _reason} ->
        assign(socket, :sandboxes, [])
    end
  end

  defp format_sandbox(sandbox) do
    %{
      id: sandbox.id,
      app_name: sandbox.app_name,
      app_pid: sandbox.app_pid,
      supervisor_pid: sandbox.supervisor_pid,
      supervisor_module: sandbox.supervisor_module,
      status: if(Process.alive?(sandbox.app_pid), do: "running", else: "stopped"),
      restart_count: Map.get(sandbox, :restart_count, 0),
      created_at: Map.get(sandbox, :created_at, System.system_time(:millisecond)),
      opts: sandbox.opts
    }
  rescue
    _ -> sandbox
  end

  defp count_active_sandboxes(sandboxes) do
    Enum.count(sandboxes, fn sandbox ->
      sandbox.status == "running"
    end)
  end

  defp status_bar_metrics(assigns) do
    [
      %{
        label: "Sandboxes",
        value: length(assigns.sandboxes),
        type: :count
      },
      %{
        label: "Active",
        value: count_active_sandboxes(assigns.sandboxes),
        type: :success
      },
      %{
        label: "Memory",
        value: format_memory_usage(),
        type: :info
      }
    ]
  end

  defp format_memory_usage do
    memory = :erlang.memory(:total)
    memory_mb = div(memory, 1024 * 1024)
    "#{memory_mb}MB"
  end
end
