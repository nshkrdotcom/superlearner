defmodule OtpSupervisorWeb.Live.DocsLive do
  use Phoenix.LiveView

  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  alias OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks
  alias OtpSupervisorWeb.Components.Layout.TerminalPanelLayout

  @moduledoc """
  Documentation center with comprehensive OTP system architecture docs.

  Refactored to use LiveComponents for better reusability and maintainability.
  """

  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "OTP Supervisor Documentation")
     |> assign(:current_page, "docs")
     |> assign(:search_query, "")
     |> assign(:search_results, [])
     |> assign(:selected_doc, nil)
     |> load_documentation}
  end

  def handle_params(params, _url, socket) do
    {:noreply, handle_navigation(socket, params)}
  end

  def render(assigns) do
    ~H"""
    <div class="fixed inset-0 bg-gray-900 text-green-400 flex flex-col">
      <!-- Status Bar -->
      <.live_component
        module={TerminalStatusBar}
        id="docs-status-bar"
        title="OTP Supervisor Documentation"
        metrics={status_bar_metrics(assigns)}
        navigation_links={TerminalNavigationLinks.page_navigation_links("docs", %{})}
      />
      
    <!-- Main Content -->
      <.live_component
        module={TerminalPanelLayout}
        id="docs-panel-layout"
        layout_type={:two_panel}
        panels={[
          %{
            slot: render_documentation_sidebar(assigns),
            class: ["w-80", "min-w-80"]
          },
          %{
            slot: render_documentation_content(assigns)
          }
        ]}
        gap="gap-4"
        padding="p-4"
      />
    </div>
    """
  end

  # Event handlers

  def handle_event("search", %{"query" => query}, socket) do
    results = search_documentation(query, socket.assigns.docs)

    {:noreply,
     socket
     |> assign(:search_query, query)
     |> assign(:search_results, results)}
  end

  def handle_event("select_doc", %{"doc_id" => doc_id}, socket) do
    doc = find_document(doc_id, socket.assigns.docs)

    {:noreply, assign(socket, :selected_doc, doc)}
  end

  def handle_event("clear_search", _params, socket) do
    {:noreply,
     socket
     |> assign(:search_query, "")
     |> assign(:search_results, [])}
  end

  # Private functions

  defp handle_navigation(socket, %{"section" => section}) do
    doc = find_document_by_section(section, socket.assigns.docs)
    assign(socket, :selected_doc, doc)
  end

  defp handle_navigation(socket, _params), do: socket

  defp load_documentation(socket) do
    assign(socket, :docs, get_documentation_structure())
  end

  defp status_bar_metrics(assigns) do
    [
      %{label: "Sections", value: length(assigns.docs || [])},
      %{label: "Search Results", value: length(assigns.search_results)},
      %{
        label: "Current",
        value: if(assigns.selected_doc, do: assigns.selected_doc.title, else: "Welcome")
      }
    ]
  end

  defp render_documentation_sidebar(assigns) do
    ~H"""
    <div class="h-full flex flex-col">
      <!-- Search -->
      <div class="p-4 border-b border-green-500/20">
        <div class="relative">
          <input
            type="text"
            placeholder="Search documentation..."
            class="w-full px-3 py-2 bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:border-green-500"
            value={@search_query}
            phx-keyup="search"
            phx-debounce="300"
          />
          <%= if @search_query != "" do %>
            <button
              phx-click="clear_search"
              class="absolute right-2 top-2 text-green-400/70 hover:text-green-400"
            >
              ✕
            </button>
          <% end %>
        </div>
      </div>
      
    <!-- Search Results or Documentation Tree -->
      <div class="flex-1 overflow-y-auto">
        <%= if @search_results != [] do %>
          <div class="p-4">
            <h4 class="text-sm font-mono font-bold mb-3 text-green-300">
              Search Results ({length(@search_results)})
            </h4>
            <%= for result <- @search_results do %>
              <button
                phx-click="select_doc"
                phx-value-doc-id={result.id}
                class="block w-full text-left p-2 mb-1 rounded hover:bg-green-500/10 transition-colors"
              >
                <div class="text-sm font-mono text-green-400">{result.title}</div>
                <div class="text-xs text-green-400/70 mt-1">{result.excerpt}</div>
              </button>
            <% end %>
          </div>
        <% else %>
          <div class="p-4">
            <h4 class="text-sm font-mono font-bold mb-3 text-green-300">Documentation Sections</h4>
            <%= for doc <- @docs || [] do %>
              <button
                phx-click="select_doc"
                phx-value-doc-id={doc.id}
                class={[
                  "block w-full text-left p-2 mb-1 rounded transition-colors font-mono text-sm",
                  if(@selected_doc && @selected_doc.id == doc.id,
                    do: "bg-green-500/20 text-green-300",
                    else: "text-green-400 hover:bg-green-500/10"
                  )
                ]}
              >
                <div class="flex items-center space-x-2">
                  <span>{doc.icon}</span>
                  <span>{doc.title}</span>
                </div>
              </button>
            <% end %>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  defp render_documentation_content(assigns) do
    ~H"""
    <div class="h-full flex flex-col">
      <div class="flex-1 overflow-y-auto">
        <%= if @selected_doc do %>
          <div class="p-6">
            <div class="prose prose-green max-w-none">
              <div class="flex items-center space-x-3 mb-6">
                <span class="text-2xl">{@selected_doc.icon}</span>
                <h1 class="text-xl font-mono font-bold text-green-300 m-0">{@selected_doc.title}</h1>
              </div>

              <div class="text-green-400 font-mono text-sm leading-relaxed space-y-4">
                {Phoenix.HTML.raw(@selected_doc.content)}
              </div>
            </div>
          </div>
        <% else %>
          <div class="h-full flex items-center justify-center">
            <div class="text-center">
              <div class="text-6xl mb-4">📚</div>
              <h2 class="text-xl font-mono font-bold text-green-300 mb-2">
                Welcome to OTP Supervisor Documentation
              </h2>
              <p class="text-green-400/70 font-mono text-sm max-w-md">
                Select a documentation section from the sidebar to get started, or use the search to find specific topics.
              </p>
            </div>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  # Documentation data

  defp get_documentation_structure do
    [
      %{
        id: "overview",
        title: "System Overview",
        icon: "🏗️",
        content: """
        <h2>OTP Supervisor System Architecture</h2>

        <p>This system provides comprehensive monitoring and management of OTP (Open Telecom Platform) supervisors and their child processes. The architecture is designed for high availability, fault tolerance, and real-time monitoring capabilities.</p>

        <h3>Core Components</h3>
        <ul>
          <li><strong>Supervisor Manager:</strong> Central coordination of all supervisor instances</li>
          <li><strong>Process Monitor:</strong> Real-time tracking of process health and performance</li>
          <li><strong>Arsenal System:</strong> Command execution and operation management</li>
          <li><strong>Dashboard:</strong> Visual monitoring and metrics display</li>
        </ul>

        <h3>Key Features</h3>
        <ul>
          <li>Real-time process monitoring with WebSocket updates</li>
          <li>Hierarchical supervisor tree visualization</li>
          <li>Process lifecycle management and control</li>
          <li>Performance metrics and alerting</li>
          <li>Command execution through Arsenal interface</li>
        </ul>
        """
      },
      %{
        id: "architecture",
        title: "Architecture Details",
        icon: "🏛️",
        content: """
        <h2>System Architecture</h2>

        <h3>Supervision Tree</h3>
        <p>The system follows OTP principles with a well-defined supervision tree:</p>

        <pre class="bg-gray-800 p-4 rounded">
        OtpSupervisor.Application
        ├── OtpSupervisor.Core.SupervisorManager
        ├── OtpSupervisor.Core.ProcessMonitor
        ├── OtpSupervisor.Core.SandboxManager
        └── OtpSupervisorWeb.Endpoint
            ├── OtpSupervisorWeb.Telemetry
            └── Phoenix.PubSub
        </pre>

        <h3>Core Modules</h3>
        <ul>
          <li><strong>SupervisorManager:</strong> Manages supervisor instances and their lifecycle</li>
          <li><strong>ProcessMonitor:</strong> Monitors process health and performance metrics</li>
          <li><strong>SandboxManager:</strong> Provides isolated environments for testing</li>
          <li><strong>Arsenal:</strong> Command execution and operation management</li>
        </ul>

        <h3>Data Flow</h3>
        <p>The system uses a combination of GenServer processes, Phoenix PubSub for real-time updates, and LiveView for interactive web interfaces.</p>
        """
      },
      %{
        id: "monitoring",
        title: "Monitoring & Metrics",
        icon: "📊",
        content: """
        <h2>Monitoring and Metrics</h2>

        <h3>Process Monitoring</h3>
        <p>The system continuously monitors:</p>
        <ul>
          <li>Process health status (running, stopped, error)</li>
          <li>Memory usage and garbage collection metrics</li>
          <li>Message queue lengths and processing rates</li>
          <li>CPU utilization per process</li>
          <li>Restart counts and error rates</li>
        </ul>

        <h3>System Metrics</h3>
        <ul>
          <li><strong>Memory:</strong> Total system memory usage, per-process allocation</li>
          <li><strong>CPU:</strong> System-wide CPU utilization and per-core metrics</li>
          <li><strong>Processes:</strong> Total process count, supervisor tree depth</li>
          <li><strong>Network:</strong> Connection counts, message throughput</li>
        </ul>

        <h3>Alerting</h3>
        <p>The system provides configurable alerts for:</p>
        <ul>
          <li>High memory usage (>80% threshold)</li>
          <li>CPU spikes (>90% utilization)</li>
          <li>Process crashes and restart loops</li>
          <li>Supervisor tree instability</li>
        </ul>
        """
      },
      %{
        id: "arsenal",
        title: "Arsenal System",
        icon: "⚡",
        content: """
        <h2>Arsenal Command System</h2>

        <h3>Overview</h3>
        <p>The Arsenal system provides a comprehensive command execution framework for OTP operations. It supports over 200 predefined operations with real-time status tracking.</p>

        <h3>Operation Types</h3>
        <ul>
          <li><strong>Active:</strong> Currently executing operations</li>
          <li><strong>Planned:</strong> Scheduled for future execution</li>
          <li><strong>Inactive:</strong> Disabled or completed operations</li>
        </ul>

        <h3>Command Categories</h3>
        <ul>
          <li><strong>Process Management:</strong> Start, stop, restart processes</li>
          <li><strong>Supervisor Control:</strong> Manage supervisor trees</li>
          <li><strong>System Operations:</strong> Memory cleanup, performance tuning</li>
          <li><strong>Diagnostics:</strong> Health checks, system analysis</li>
        </ul>

        <h3>Execution Flow</h3>
        <ol>
          <li>Command selection from operation grid</li>
          <li>Parameter validation and configuration</li>
          <li>Execution with real-time progress tracking</li>
          <li>Result collection and status reporting</li>
        </ol>
        """
      },
      %{
        id: "api",
        title: "API Reference",
        icon: "🔧",
        content: """
        <h2>API Reference</h2>

        <h3>REST Endpoints</h3>

        <h4>System Information</h4>
        <ul>
          <li><code>GET /api/v1/system</code> - System status and metrics</li>
          <li><code>GET /api/v1/system/processes</code> - All process information</li>
          <li><code>GET /api/v1/system/supervisors</code> - Supervisor tree data</li>
        </ul>

        <h4>Process Management</h4>
        <ul>
          <li><code>POST /api/v1/process/start</code> - Start a new process</li>
          <li><code>POST /api/v1/process/stop</code> - Stop a process</li>
          <li><code>POST /api/v1/process/restart</code> - Restart a process</li>
          <li><code>GET /api/v1/process/:id</code> - Get process details</li>
        </ul>

        <h4>Supervisor Operations</h4>
        <ul>
          <li><code>GET /api/v1/supervisor/:id</code> - Supervisor details</li>
          <li><code>POST /api/v1/supervisor/:id/restart</code> - Restart supervisor</li>
          <li><code>GET /api/v1/supervisor/:id/children</code> - Get child processes</li>
        </ul>

        <h3>WebSocket Events</h3>
        <ul>
          <li><code>process_update</code> - Process status changes</li>
          <li><code>system_metrics</code> - Real-time system metrics</li>
          <li><code>supervisor_change</code> - Supervisor tree updates</li>
        </ul>
        """
      }
    ]
  end

  defp search_documentation(query, docs) when is_binary(query) and query != "" do
    query = String.downcase(query)

    Enum.filter(docs, fn doc ->
      title_match = String.contains?(String.downcase(doc.title), query)
      content_match = String.contains?(String.downcase(doc.content), query)

      title_match or content_match
    end)
    |> Enum.map(fn doc ->
      excerpt = extract_excerpt(doc.content, query)
      Map.put(doc, :excerpt, excerpt)
    end)
  end

  defp search_documentation(_query, _docs), do: []

  defp extract_excerpt(content, query) do
    # Simple excerpt extraction - find the first occurrence of the query
    content
    # Remove HTML tags
    |> String.replace(~r/<[^>]*>/, "")
    |> String.split(~r/\s+/)
    |> Enum.chunk_every(20, 10)
    |> Enum.find(fn chunk ->
      chunk
      |> Enum.join(" ")
      |> String.downcase()
      |> String.contains?(String.downcase(query))
    end)
    |> case do
      nil -> String.slice(content, 0, 100) <> "..."
      chunk -> Enum.join(chunk, " ") <> "..."
    end
  end

  defp find_document(doc_id, docs) do
    Enum.find(docs, &(&1.id == doc_id))
  end

  defp find_document_by_section(section, docs) do
    Enum.find(docs, &(&1.id == section))
  end
end
