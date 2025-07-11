True Sandbox Architecture Integration

  How Current System Would Hook Into Container/VM-Based Sandboxes

  ---
  Current System as Orchestration Layer

  Architecture Transformation

  Instead of managing processes directly, the current system becomes a sandbox orchestrator that manages external environments:

  defmodule OTPSupervisor.Core.SandboxOrchestrator do
    @type sandbox_type :: :docker | :vm | :beam_node | :process_group

    @spec create_external_sandbox(String.t(), sandbox_type(), map()) ::
      {:ok, external_sandbox()} | {:error, term()}
    def create_external_sandbox(sandbox_id, type, config)

    @spec connect_to_sandbox(String.t()) :: {:ok, connection()} | {:error, term()}
    def connect_to_sandbox(sandbox_id)

    @spec monitor_external_sandbox(String.t()) :: :ok
    def monitor_external_sandbox(sandbox_id)
  end

  Container Integration Strategy

  Docker-Based Sandboxes

  defmodule OTPSupervisor.Core.Adapters.DockerAdapter do
    @spec create_container(String.t(), docker_config()) :: {:ok, container_id()} | {:error, term()}
    def create_container(sandbox_id, config) do
      docker_config = %{
        image: "elixir:1.15-alpine",
        volumes: ["#{workspace_path(sandbox_id)}:/app"],
        ports: [{"#{config.port}", "4000"}],
        environment: [
          "MIX_ENV=dev",
          "PHX_HOST=localhost",
          "DATABASE_URL=#{config.database_url}"
        ],
        command: ["mix", "phx.server"]
      }

      case Docker.create_container(docker_config) do
        {:ok, container} ->
          # Start container and wait for readiness
          Docker.start_container(container.id)
          wait_for_readiness(container, config.health_check)
          {:ok, container.id}
        error -> error
      end
    end

    @spec connect_iex(container_id()) :: {:ok, iex_connection()} | {:error, term()}
    def connect_iex(container_id) do
      # Docker exec into running container for IEX session
      Docker.exec(container_id, ["iex", "--remsh", "app@container"])
    end

    @spec get_logs(container_id(), keyword()) :: {:ok, [log_entry()]} | {:error, term()}
    def get_logs(container_id, opts \\ [])

    @spec get_metrics(container_id()) :: {:ok, container_metrics()} | {:error, term()}
    def get_metrics(container_id)
  end

  VM Node Integration

  defmodule OTPSupervisor.Core.Adapters.BeamNodeAdapter do
    @spec start_node(String.t(), node_config()) :: {:ok, node()} | {:error, term()}
    def start_node(sandbox_id, config) do
      node_name = :"sandbox_#{sandbox_id}@#{config.host}"

      # Start distributed Erlang node
      case :net_kernel.start([node_name, :longnames]) do
        {:ok, _} ->
          # Connect to new node and start application
          Node.spawn_link(node_name, fn ->
            Application.start(:my_phoenix_app)
          end)
          {:ok, node_name}
        error -> error
      end
    end

    @spec execute_remote(node(), module(), atom(), [term()]) :: term()
    def execute_remote(node, module, function, args) do
      :rpc.call(node, module, function, args)
    end

    @spec get_remote_state(node(), pid()) :: {:ok, term()} | {:error, term()}
    def get_remote_state(node, pid) do
      :rpc.call(node, :sys, :get_state, [pid])
    end
  end

  Monitoring and Observability Bridge

  Cross-Boundary Process Monitoring

  defmodule OTPSupervisor.Core.ExternalMonitor do
    @spec monitor_external_processes(String.t(), connection()) :: :ok
    def monitor_external_processes(sandbox_id, connection) do
      # Poll external system for process information
      Task.start_link(fn ->
        Stream.interval(1000)
        |> Stream.each(fn _ ->
          case get_external_process_list(connection) do
            {:ok, processes} ->
              # Transform external process data to internal format
              internal_processes = Enum.map(processes, &transform_process_data/1)

              # Update internal state
              GenServer.cast(SandboxManager, {:update_external_processes, sandbox_id, internal_processes})

              # Broadcast to UI
              Phoenix.PubSub.broadcast(
                OtpSupervisor.PubSub,
                "sandbox_updates",
                {:external_processes_updated, sandbox_id, internal_processes}
              )
            _ -> :ok
          end
        end)
        |> Stream.run()
      end)
    end

    defp transform_process_data(external_process) do
      %{
        id: external_process.pid,
        name: external_process.name || "Unknown",
        status: map_external_status(external_process.status),
        memory: external_process.memory,
        cpu_usage: external_process.cpu_percent,
        # Map external data to internal process format
        type: :external,
        sandbox_id: external_process.sandbox_id
      }
    end
  end

  Log Aggregation and Streaming

  defmodule OTPSupervisor.Core.LogAggregator do
    @spec start_log_streaming(String.t(), connection()) :: {:ok, pid()} | {:error, term()}
    def start_log_streaming(sandbox_id, connection) do
      GenServer.start_link(__MODULE__, {sandbox_id, connection})
    end

    def handle_info({:log_data, data}, {sandbox_id, connection} = state) do
      # Parse log data from external source
      parsed_logs = parse_external_logs(data)

      # Stream to web interface
      Phoenix.PubSub.broadcast(
        OtpSupervisor.PubSub,
        "sandbox_logs:#{sandbox_id}",
        {:new_logs, parsed_logs}
      )

      {:noreply, state}
    end
  end

  Cinema Debugger Integration with External Sandboxes

  Remote Tracing Capability

  defmodule OTPSupervisor.Core.CinemaDebugger.RemoteTracer do
    @spec start_remote_tracing(String.t(), connection()) :: {:ok, tracer_pid()} | {:error, term()}
    def start_remote_tracing(sandbox_id, connection) do
      case connection.type do
        :docker ->
          # Inject tracing agent into container
          inject_tracing_agent(connection.container_id)

        :beam_node ->
          # Start remote tracer on node
          :rpc.call(connection.node, CinemaDebugger.Tracer, :start_link, [sandbox_id])

        :process_group ->
          # Direct local tracing (current implementation)
          CinemaDebugger.Tracer.start_link(sandbox_id)
      end
    end

    defp inject_tracing_agent(container_id) do
      # Copy tracing module into container
      tracing_script = generate_tracing_script()
      Docker.copy_to_container(container_id, tracing_script, "/tmp/tracer.exs")

      # Execute tracing script inside container
      Docker.exec(container_id, ["elixir", "/tmp/tracer.exs"])
    end

    defp generate_tracing_script do
      """
      # Standalone tracing agent that reports back to host
      defmodule RemoteTracer do
        def start_link do
          :dbg.tracer(:port, :dbg.trace_port(:file, '/tmp/trace.log'))
          :dbg.p(:all, :c)
          send_traces_to_host()
        end

        defp send_traces_to_host do
          # Stream trace data back to orchestrator
          # Via HTTP, TCP, or shared volume
        end
      end

      RemoteTracer.start_link()
      """
    end
  end

  Unified Interface Layer

  Sandbox Abstraction

  defmodule OTPSupervisor.Core.UnifiedSandbox do
    @type sandbox :: %{
      id: String.t(),
      type: :internal | :docker | :vm | :beam_node,
      status: :starting | :running | :stopping | :stopped | :error,
      connection: connection() | nil,
      config: map(),
      metrics: sandbox_metrics(),
      processes: [process_info()],
      logs: [log_entry()]
    }

    @spec execute_command(String.t(), String.t()) :: {:ok, result()} | {:error, term()}
    def execute_command(sandbox_id, command) do
      case get_sandbox(sandbox_id) do
        %{type: :internal} = sandbox ->
          # Execute in current VM
          execute_internal_command(sandbox, command)

        %{type: :docker} = sandbox ->
          # Execute in container
          Docker.exec(sandbox.connection.container_id, ["sh", "-c", command])

        %{type: :beam_node} = sandbox ->
          # Execute on remote node
          :rpc.call(sandbox.connection.node, :os, :cmd, [command])
      end
    end

    @spec get_iex_session(String.t()) :: {:ok, iex_session()} | {:error, term()}
    def get_iex_session(sandbox_id) do
      # Return appropriate IEX connection based on sandbox type
      case get_sandbox(sandbox_id) do
        %{type: :internal} -> {:ok, :current_session}
        %{type: :docker} = sandbox -> DockerAdapter.connect_iex(sandbox.connection.container_id)
        %{type: :beam_node} = sandbox -> {:ok, {:remote_shell, sandbox.connection.node}}
      end
    end
  end

  Web Interface Adaptation

  Multi-Type Sandbox UI

  defmodule OtpSupervisorWeb.Components.Widgets.UnifiedSandboxWidget do
    def render_sandbox_card(%{sandbox: %{type: :docker}} = assigns) do
      ~H"""
      <div class="sandbox-card docker-sandbox">
        <div class="sandbox-header">
          <h3><%= @sandbox.id %></h3>
          <span class="sandbox-type">🐳 Docker</span>
        </div>
        <div class="sandbox-actions">
          <button phx-click="open_terminal" phx-value-sandbox-id={@sandbox.id}>
            Terminal
          </button>
          <button phx-click="view_logs" phx-value-sandbox-id={@sandbox.id}>
            Logs
          </button>
          <button phx-click="connect_debugger" phx-value-sandbox-id={@sandbox.id}>
            Debug
          </button>
        </div>
        <div class="sandbox-metrics">
          CPU: <%= @sandbox.metrics.cpu %>% |
          Memory: <%= @sandbox.metrics.memory %>MB
        </div>
      </div>
      """
    end

    def render_sandbox_card(%{sandbox: %{type: :internal}} = assigns) do
      ~H"""
      <div class="sandbox-card internal-sandbox">
        <!-- Current internal sandbox UI -->
      </div>
      """
    end
  end

  Migration Strategy

  Gradual Transition

  1. Phase 1: Add external sandbox adapters alongside current system
  2. Phase 2: Implement unified interface that works with both
  3. Phase 3: Extend Cinema Debugger to work with external sandboxes
  4. Phase 4: Add container/VM management capabilities
  5. Phase 5: Full external sandbox support with development workflows

  Backwards Compatibility

  - Current process-based sandboxes remain supported
  - Existing Arsenal operations work unchanged
  - UI gracefully handles both internal and external sandboxes
  - Configuration allows choosing sandbox type per creation

  The current system becomes the control plane for a multi-modal sandbox environment, providing unified monitoring, debugging, and
  management across different isolation levels.
