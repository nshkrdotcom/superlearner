# Missing Components Analysis

**Document Version:** 1.0
**Date:** 2025-10-07
**Status:** Design Phase

## Overview

This document identifies gaps in the current SuperLearner ecosystem, analyzes missing integrations, and defines requirements for bringing the system to full cohesion.

---

## Executive Summary

### Critical Gaps (Blockers)
1. **ArsenalPlug Router Integration** - API routes not configured
2. **OTP Sandbox Library Extraction** - Functionality embedded, not standalone
3. **Playwriter Usage** - Listed as dependency but not used

### High-Priority Gaps (Important)
4. **Unified Documentation** - No cross-library docs
5. **Integration Tests** - Limited cross-library testing
6. **CLI Tools** - No unified command-line interface

### Medium-Priority Gaps (Enhancement)
7. **Example Projects** - Missing integrated examples
8. **Telemetry Dashboard** - No visualization of telemetry data
9. **Performance Testing** - No load tests or benchmarks

---

## Gap 1: ArsenalPlug Router Integration

### Current State
- ✅ Arsenal operations defined in `lib/otp_supervisor/core/arsenal/operations/`
- ✅ Arsenal Plug listed in `mix.exs`
- ❌ Routes not configured in `router.ex`
- ❌ No API documentation endpoint

### Expected State
```elixir
# lib/otp_supervisor_web/router.ex
defmodule OtpSupervisorWeb.Router do
  use OtpSupervisorWeb, :router

  # Arsenal API pipeline
  pipeline :arsenal_api do
    plug :accepts, ["json"]
    plug :put_secure_browser_headers
    # plug ArsenalPlug.AuthPlug (future)
    # plug ArsenalPlug.RateLimitPlug (future)
  end

  # Arsenal routes
  scope "/api/v1/arsenal", OtpSupervisorWeb do
    pipe_through [:api, :arsenal_api]

    # Documentation endpoints
    get "/docs", ArsenalController, :docs
    get "/operations", ArsenalController, :list_operations
  end

  # Dynamic Arsenal operation routing
  scope "/api/v1", OtpSupervisorWeb do
    pipe_through [:api, :arsenal_api]

    # Process operations
    get "/processes", ArsenalController, :operation_handler
    get "/processes/:pid/info", ArsenalController, :operation_handler
    delete "/processes/:pid", ArsenalController, :operation_handler
    post "/processes/:pid/message", ArsenalController, :operation_handler
    post "/processes/:pid/trace", ArsenalController, :operation_handler

    # Supervisor operations
    get "/supervisors", ArsenalController, :operation_handler
    get "/supervisors/:name", ArsenalController, :operation_handler

    # Sandbox operations
    get "/sandboxes", ArsenalController, :operation_handler
    post "/sandboxes", ArsenalController, :operation_handler
    get "/sandboxes/:id", ArsenalController, :operation_handler
    put "/sandboxes/:id/restart", ArsenalController, :operation_handler
    delete "/sandboxes/:id", ArsenalController, :operation_handler

    # Distributed operations (if cluster available)
    get "/cluster/health", ArsenalController, :operation_handler
    get "/cluster/topology", ArsenalController, :operation_handler
    get "/cluster/nodes/:node/info", ArsenalController, :operation_handler
    get "/cluster/processes", ArsenalController, :operation_handler
  end
end
```

### Required Components

#### 1. ArsenalController
```elixir
# lib/otp_supervisor_web/controllers/arsenal_controller.ex
defmodule OtpSupervisorWeb.ArsenalController do
  use OtpSupervisorWeb, :controller
  require Logger

  def docs(conn, _params) do
    docs = Arsenal.generate_api_docs()
    json(conn, docs)
  end

  def list_operations(conn, _params) do
    operations = Arsenal.list_operations()
    json(conn, %{data: operations})
  end

  def operation_handler(conn, params) do
    with {:ok, operation_name} <- extract_operation_name(conn),
         {:ok, result} <- Arsenal.execute(operation_name, params) do
      json(conn, %{data: result, success: true})
    else
      {:error, :not_found} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Operation not found"})

      {:error, {:validation_failed, field, reason}} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: "Validation failed", field: field, reason: reason})

      {:error, reason} ->
        Logger.error("Operation failed: #{inspect(reason)}")
        conn
        |> put_status(:internal_server_error)
        |> json(%{error: "Operation failed", reason: inspect(reason)})
    end
  end

  defp extract_operation_name(conn) do
    # Map path to operation name
    # GET /api/v1/processes → :list_processes
    # GET /api/v1/processes/:pid/info → :get_process_info
    # etc.
  end
end
```

#### 2. Operation Name Mapping
```elixir
# lib/otp_supervisor/core/arsenal/route_mapper.ex
defmodule OTPSupervisor.Core.Arsenal.RouteMapper do
  @moduledoc """
  Maps HTTP routes to Arsenal operation names.
  """

  def route_to_operation(:get, ["processes"]), do: {:ok, :list_processes}
  def route_to_operation(:get, ["processes", _pid, "info"]), do: {:ok, :get_process_info}
  def route_to_operation(:delete, ["processes", _pid]), do: {:ok, :kill_process}
  def route_to_operation(:post, ["processes", _pid, "message"]), do: {:ok, :send_message}
  def route_to_operation(:post, ["processes", _pid, "trace"]), do: {:ok, :trace_process}

  def route_to_operation(:get, ["supervisors"]), do: {:ok, :list_supervisors}

  def route_to_operation(:get, ["sandboxes"]), do: {:ok, :list_sandboxes}
  def route_to_operation(:post, ["sandboxes"]), do: {:ok, :create_sandbox}
  def route_to_operation(:get, ["sandboxes", _id]), do: {:ok, :get_sandbox_info}
  def route_to_operation(:put, ["sandboxes", _id, "restart"]), do: {:ok, :restart_sandbox}
  def route_to_operation(:delete, ["sandboxes", _id]), do: {:ok, :destroy_sandbox}

  def route_to_operation(:get, ["cluster", "health"]), do: {:ok, :cluster_health}
  def route_to_operation(:get, ["cluster", "topology"]), do: {:ok, :cluster_topology}
  def route_to_operation(:get, ["cluster", "nodes", _node, "info"]), do: {:ok, :node_info}
  def route_to_operation(:get, ["cluster", "processes"]), do: {:ok, :process_list}

  def route_to_operation(_, _), do: {:error, :not_found}
end
```

### Implementation Checklist
- [ ] Create `ArsenalController`
- [ ] Create `RouteMapper` module
- [ ] Add routes to `router.ex`
- [ ] Test all endpoints
- [ ] Update API documentation
- [ ] Add integration tests

---

## Gap 2: OTP Sandbox Library Extraction

### Current State
- ❌ OTP Sandbox is minimal stub (`sandbox/examples/otp_sandbox`)
- ✅ Functionality implemented in `OTPSupervisor.Core.SandboxManager`
- ❌ Not usable as standalone library
- ❌ No public API documentation

### Target State
```
otp_sandbox/
├── lib/
│   ├── otp_sandbox.ex                   # Main module
│   ├── otp_sandbox/
│   │   ├── supervisor.ex                # Sandbox supervisor
│   │   ├── manager.ex                   # Manager GenServer
│   │   ├── registry.ex                  # Sandbox registry
│   │   └── experiment.ex                # Experiment behavior
│   │
├── test/
│   ├── otp_sandbox_test.exs
│   └── integration/
│       └── with_supertester_test.exs
│
├── examples/
│   ├── basic_usage.exs
│   ├── crash_recovery.exs
│   └── isolation_demo.exs
│
├── README.md
├── CHANGELOG.md
└── mix.exs
```

### Required API

#### Core Module
```elixir
defmodule OTPSandbox do
  @moduledoc """
  Isolated OTP experimentation environments.

  OTPSandbox provides safe, isolated environments for experimenting with
  OTP supervisors and processes without affecting the main application.

  ## Features

  - Process isolation with unique naming
  - ETS-based fast metadata lookup
  - Automatic cleanup on sandbox destruction
  - Configuration preservation across restarts
  - Monitor-based graceful shutdown

  ## Usage

      # Create a sandbox
      {:ok, pid} = OTPSandbox.create("my_sandbox", MySupervisor, strategy: :one_for_one)

      # Run experiments
      {:ok, result} = OTPSandbox.run_experiment("my_sandbox", fn ->
        # Your experimental code here
      end)

      # Destroy when done
      :ok = OTPSandbox.destroy("my_sandbox")

  ## Integration with Supertester

      import Supertester.Assertions

      test "sandbox isolation" do
        {:ok, sandbox} = OTPSandbox.create("test", Worker)

        assert_no_process_leaks(fn ->
          OTPSandbox.run_experiment(sandbox, fn -> ... end)
          OTPSandbox.destroy(sandbox)
        end)
      end
  """

  @type sandbox_id :: String.t()
  @type supervisor_module :: module()
  @type sandbox_opts :: keyword()

  # Core API
  @spec create(sandbox_id, supervisor_module, sandbox_opts) ::
    {:ok, pid()} | {:error, term()}
  def create(id, module, opts \\ [])

  @spec list() :: [{sandbox_id, pid(), supervisor_module}]
  def list()

  @spec get_info(sandbox_id) :: {:ok, map()} | {:error, :not_found}
  def get_info(id)

  @spec restart(sandbox_id) :: :ok | {:error, term()}
  def restart(id)

  @spec destroy(sandbox_id) :: :ok | {:error, term()}
  def destroy(id)

  # Experiment execution
  @spec run_experiment(sandbox_id, (() -> result)) ::
    {:ok, result} | {:error, term()} when result: term()
  def run_experiment(id, fun)

  # Configuration
  @spec get_config(sandbox_id) :: {:ok, map()} | {:error, :not_found}
  def get_config(id)

  @spec update_config(sandbox_id, map()) :: :ok | {:error, term()}
  def update_config(id, config)
end
```

#### Experiment Behavior
```elixir
defmodule OTPSandbox.Experiment do
  @moduledoc """
  Behavior for defining reusable sandbox experiments.
  """

  @callback init(opts :: keyword()) ::
    {:ok, state :: term()} | {:error, term()}

  @callback run(state :: term()) ::
    {:ok, result :: term()} | {:error, term()}

  @callback cleanup(state :: term()) :: :ok

  @optional_callbacks [cleanup: 1]

  defmacro __using__(_opts) do
    quote do
      @behaviour OTPSandbox.Experiment

      def init(opts), do: {:ok, opts}
      def cleanup(_state), do: :ok

      defoverridable init: 1, cleanup: 1
    end
  end
end
```

### Migration from SuperLearner

1. **Extract Code**
   ```bash
   # Copy SandboxManager to OTP Sandbox
   cp lib/otp_supervisor/core/sandbox_manager.ex ../otp_sandbox/lib/otp_sandbox/manager.ex
   ```

2. **Update SuperLearner**
   ```elixir
   # lib/otp_supervisor/core/sandbox_manager.ex
   defmodule OTPSupervisor.Core.SandboxManager do
     @moduledoc """
     Wrapper around OTPSandbox for SuperLearner integration.
     """

     defdelegate create_sandbox(id, module, opts), to: OTPSandbox, as: :create
     defdelegate list_sandboxes(), to: OTPSandbox, as: :list
     defdelegate get_sandbox_info(id), to: OTPSandbox, as: :get_info
     defdelegate restart_sandbox(id), to: OTPSandbox, as: :restart
     defdelegate destroy_sandbox(id), to: OTPSandbox, as: :destroy
   end
   ```

3. **Update Dependencies**
   ```elixir
   # mix.exs
   {:otp_sandbox, "~> 0.1.0"}  # Change from path to version dependency
   ```

### Implementation Checklist
- [ ] Create standalone OTP Sandbox library structure
- [ ] Extract and refactor SandboxManager code
- [ ] Define public API
- [ ] Write comprehensive tests
- [ ] Create examples and documentation
- [ ] Publish to Hex.pm
- [ ] Update SuperLearner to use published version

---

## Gap 3: Playwriter Integration

### Current State
- ✅ Listed as dependency in `mix.exs`
- ❌ No actual usage in codebase
- ❌ No UI tests
- ❌ No screenshot generation
- ❌ No E2E tests

### Opportunity Areas

#### 1. UI Testing Suite
```elixir
# test/otp_supervisor_web/live/ui_test.exs
defmodule OtpSupervisorWeb.UITest do
  use OtpSupervisorWeb.ConnCase, async: false
  import Playwriter

  @base_url "http://localhost:4002"

  setup do
    # Start test server
    {:ok, _} = start_supervised(OtpSupervisorWeb.Endpoint)
    :ok
  end

  describe "Supervisor Dashboard UI" do
    test "renders supervisor tree" do
      {:ok, html} = with_browser(%{headless: true}, fn page ->
        Playwright.Page.goto(page, "#{@base_url}/supervisors")
        Playwright.Page.wait_for_selector(page, ".supervisor-tree")
        Playwright.Page.content(page)
      end)

      assert html =~ "Supervisor Dashboard"
      assert html =~ "demo_one_for_one"
    end

    test "kill process button works" do
      {:ok, success} = with_browser(%{headless: true}, fn page ->
        Playwright.Page.goto(page, "#{@base_url}/supervisors")
        Playwright.Page.wait_for_selector(page, ".process-item")

        # Click first kill button
        Playwright.Page.click(page, ".kill-process-btn:first-of-type")

        # Wait for confirmation
        Playwright.Page.wait_for_selector(page, ".process-killed-notification")

        true
      end)

      assert success
    end
  end
end
```

#### 2. Screenshot Generation
```elixir
# Script: scripts/generate_docs_screenshots.exs
defmodule DocsScreenshots do
  import Playwriter

  @base_url "http://localhost:4000"
  @output_dir "priv/static/images/docs"

  def generate_all do
    File.mkdir_p!(@output_dir)

    screenshots = [
      {"supervisors", "supervisor-dashboard.png", "Supervisor Dashboard"},
      {"system", "system-analytics.png", "System Analytics"},
      {"api/v1/arsenal/docs", "api-docs.png", "API Documentation"}
    ]

    {:ok, _} = with_browser(%{headless: true}, fn page ->
      for {path, filename, description} <- screenshots do
        IO.puts("Capturing: #{description}")

        Playwright.Page.goto(page, "#{@base_url}/#{path}")
        Playwright.Page.wait_for_load_state(page, "networkidle")

        output_path = Path.join(@output_dir, filename)
        Playwright.Page.screenshot(page, %{
          path: output_path,
          full_page: true
        })

        IO.puts("  → #{output_path}")
      end

      :ok
    end)

    IO.puts("\n✅ All screenshots generated!")
  end
end

DocsScreenshots.generate_all()
```

#### 3. End-to-End Tests
```elixir
# test/integration/e2e_test.exs
defmodule OtpSupervisor.E2ETest do
  use OtpSupervisorWeb.ConnCase, async: false
  import Playwriter

  @moduletag :e2e

  test "complete sandbox workflow" do
    {:ok, result} = with_browser(%{headless: true}, fn page ->
      base = "http://localhost:4002"

      # 1. Navigate to sandboxes page
      Playwright.Page.goto(page, "#{base}/sandboxes")
      Playwright.Page.wait_for_selector(page, "#create-sandbox-btn")

      # 2. Create sandbox
      Playwright.Page.click(page, "#create-sandbox-btn")
      Playwright.Page.fill(page, "#sandbox-id", "e2e_test")
      Playwright.Page.select_option(page, "#sandbox-module", "TestDemoSupervisor")
      Playwright.Page.click(page, "#submit-sandbox")

      # 3. Wait for sandbox to be created
      Playwright.Page.wait_for_selector(page, "#sandbox-e2e_test")

      # 4. View sandbox details
      Playwright.Page.click(page, "#sandbox-e2e_test")
      Playwright.Page.wait_for_selector(page, ".sandbox-details")

      # 5. Run experiment
      Playwright.Page.click(page, "#run-experiment-btn")
      Playwright.Page.wait_for_selector(page, ".experiment-result")

      result = Playwright.Page.text_content(page, ".experiment-result")

      # 6. Destroy sandbox
      Playwright.Page.click(page, "#destroy-sandbox-btn")
      Playwright.Page.wait_for_selector(page, ".sandbox-destroyed")

      result
    end)

    assert result =~ "success"
  end
end
```

### Implementation Checklist
- [ ] Create UI test suite
- [ ] Add screenshot generation script
- [ ] Create E2E test framework
- [ ] Add visual regression testing
- [ ] Document Playwriter usage patterns
- [ ] Add CI/CD integration for tests

---

## Gap 4: Unified Documentation

### Current State
- ✅ Individual library READMEs exist
- ❌ No cross-library documentation
- ❌ No unified documentation site
- ❌ No integration guides
- ❌ No API documentation portal

### Target State

#### Documentation Site Structure
```
docs/
├── index.md                           # Landing page
├── getting-started.md                 # Quick start guide
├── architecture/
│   ├── overview.md                    # System architecture
│   ├── ecosystem.md                   # Library relationships
│   └── data-flow.md                   # Data flow patterns
│
├── libraries/
│   ├── superlearner.md                # SuperLearner guide
│   ├── arsenal.md                     # Arsenal guide
│   ├── arsenal-plug.md                # Arsenal Plug guide
│   ├── supertester.md                 # Supertester guide
│   ├── otp-sandbox.md                 # OTP Sandbox guide
│   └── playwriter.md                  # Playwriter guide
│
├── tutorials/
│   ├── 01-first-sandbox.md
│   ├── 02-supervision-strategies.md
│   ├── 03-testing-otp-systems.md
│   ├── 04-building-operations.md
│   └── 05-distributed-systems.md
│
├── integration/
│   ├── arsenal-operations.md          # Creating operations
│   ├── sandbox-experiments.md         # Sandbox patterns
│   ├── testing-strategies.md          # Using Supertester
│   └── ui-automation.md               # Playwriter usage
│
└── api/
    ├── rest-api.md                    # REST API reference
    ├── operations.md                  # Arsenal operations
    └── telemetry.md                   # Telemetry events
```

#### Documentation Tools

**Option 1: ExDoc with Groups**
```elixir
# mix.exs
def project do
  [
    docs: [
      main: "readme",
      extras: [
        "README.md",
        "docs/getting-started.md",
        "docs/architecture/overview.md",
        # ... more docs
      ],
      groups_for_extras: [
        "Architecture": ~r/docs\/architecture\/.*/,
        "Libraries": ~r/docs\/libraries\/.*/,
        "Tutorials": ~r/docs\/tutorials\/.*/,
        "Integration": ~r/docs\/integration\/.*/,
        "API": ~r/docs\/api\/.*/
      ]
    ]
  ]
end
```

**Option 2: Standalone Documentation Site**
- MkDocs + Material theme
- Docusaurus
- GitBook

### Implementation Checklist
- [ ] Choose documentation tool
- [ ] Create documentation structure
- [ ] Write getting started guide
- [ ] Write integration guides
- [ ] Write API reference
- [ ] Create tutorials
- [ ] Add code examples
- [ ] Deploy documentation site

---

## Gap 5: Integration Tests

### Current State
- ✅ Unit tests for individual libraries
- ✅ Some integration tests in SuperLearner
- ❌ No systematic cross-library integration tests
- ❌ No test coverage for library boundaries

### Required Test Coverage

#### Arsenal ↔ Arsenal Plug
```elixir
# test/integration/arsenal_plug_test.exs
defmodule Integration.ArsenalPlugTest do
  use OtpSupervisorWeb.ConnCase, async: true

  describe "Arsenal operations via HTTP" do
    test "operation execution through HTTP", %{conn: conn} do
      # Test that Arsenal operations are accessible via HTTP
    end

    test "parameter validation errors return 422", %{conn: conn} do
      # Test validation through the full stack
    end

    test "operation not found returns 404", %{conn: conn} do
      # Test error handling
    end
  end
end
```

#### SuperLearner ↔ Supertester
```elixir
# test/integration/supertester_integration_test.exs
defmodule Integration.SupertesterIntegrationTest do
  use ExUnit.Case, async: true
  import Supertester.OTPHelpers
  import Supertester.Assertions

  test "Supertester helpers work with SuperLearner processes" do
    # Create processes using SuperLearner
    # Test with Supertester helpers
  end
end
```

#### OTP Sandbox ↔ Supertester (Future)
```elixir
# test/integration/sandbox_supertester_test.exs
defmodule Integration.SandboxSupertesterTest do
  use ExUnit.Case, async: true
  import Supertester.Assertions

  test "sandbox isolation verified with Supertester" do
    assert_no_process_leaks(fn ->
      {:ok, _} = OTPSandbox.create("test", Worker)
      :ok = OTPSandbox.destroy("test")
    end)
  end
end
```

### Test Infrastructure Needs

#### Test Helper Module
```elixir
# test/support/ecosystem_test_helper.ex
defmodule EcosystemTestHelper do
  @moduledoc """
  Helpers for testing ecosystem integration.
  """

  def with_sandbox(id, module, opts \\ [], fun) do
    {:ok, pid} = OTPSandbox.create(id, module, opts)

    try do
      fun.(pid)
    after
      OTPSandbox.destroy(id)
    end
  end

  def with_api_server(fun) do
    # Start test server
    # Run tests
    # Stop server
  end

  def assert_cross_library_compatibility do
    # Verify version compatibility
    # Check API contracts
  end
end
```

### Implementation Checklist
- [ ] Create integration test directory
- [ ] Write Arsenal ↔ Arsenal Plug tests
- [ ] Write SuperLearner ↔ Supertester tests
- [ ] Write OTP Sandbox ↔ Supertester tests (after extraction)
- [ ] Add boundary tests for all library pairs
- [ ] Create test helper utilities
- [ ] Add CI/CD integration

---

## Gap 6: CLI Tools

### Current State
- ✅ Individual library CLIs exist (Playwriter)
- ❌ No unified ecosystem CLI
- ❌ No SuperLearner CLI
- ❌ No operations CLI

### Target CLI Structure

```bash
# Unified CLI: 'superlearner'
superlearner --version                    # Show versions of all libraries
superlearner --help                       # Show help

# Sandbox management
superlearner sandbox create <id> <module>
superlearner sandbox list
superlearner sandbox info <id>
superlearner sandbox destroy <id>

# Process management
superlearner process list
superlearner process info <pid>
superlearner process kill <pid>

# Operations
superlearner operation list
superlearner operation run <name> <params>
superlearner operation docs <name>

# Server management
superlearner server start [--port 4000]
superlearner server stop

# Development tools
superlearner docs generate               # Generate screenshots
superlearner test e2e                   # Run E2E tests
superlearner cluster start              # Start test cluster
```

### Implementation

#### Escript Approach
```elixir
# mix.exs
def project do
  [
    escript: [main_module: SuperLearner.CLI]
  ]
end
```

```elixir
# lib/otp_supervisor/cli.ex
defmodule SuperLearner.CLI do
  @moduledoc """
  Unified CLI for SuperLearner ecosystem.
  """

  def main(args) do
    args
    |> parse_args()
    |> handle_command()
  end

  defp parse_args(args) do
    {opts, command, _} = OptionParser.parse(args,
      switches: [
        help: :boolean,
        version: :boolean,
        port: :integer
      ],
      aliases: [
        h: :help,
        v: :version,
        p: :port
      ]
    )

    {opts, command}
  end

  defp handle_command({[help: true], _}), do: print_help()
  defp handle_command({[version: true], _}), do: print_version()

  defp handle_command({_opts, ["sandbox", "create", id, module]}) do
    # Create sandbox
  end

  defp handle_command({_opts, ["sandbox", "list"]}) do
    # List sandboxes
  end

  # ... more commands
end
```

### Implementation Checklist
- [ ] Design CLI interface
- [ ] Create CLI module
- [ ] Implement core commands
- [ ] Add help system
- [ ] Add shell completions
- [ ] Write CLI tests
- [ ] Package as escript

---

## Gap 7: Example Projects

### Current State
- ✅ Examples in individual libraries
- ❌ No integrated ecosystem examples
- ❌ No real-world application examples

### Needed Examples

#### 1. Complete Application Example
```
examples/todo_app/
├── lib/
│   ├── todo_app.ex
│   ├── todo_app/
│   │   ├── supervisor.ex           # OTP supervisor
│   │   ├── worker.ex               # GenServer worker
│   │   └── operations/             # Arsenal operations
│   │       ├── list_todos.ex
│   │       └── create_todo.ex
│   └── todo_app_web/
│       ├── router.ex               # ArsenalPlug integration
│       └── live/                   # LiveView UI
│
├── test/
│   ├── todo_app_test.exs           # Using Supertester
│   └── todo_app_web/
│       └── e2e_test.exs            # Using Playwriter
│
└── README.md
```

#### 2. Distributed System Example
```
examples/distributed_cache/
├── lib/
│   ├── cache.ex                    # Distributed cache
│   ├── cache/
│   │   ├── supervisor.ex
│   │   ├── partition.ex
│   │   └── replication.ex
│   └── operations/                 # Arsenal operations
│
├── test/
│   ├── cache_test.exs              # Single-node tests
│   └── distributed_test.exs        # Multi-node tests
│
└── README.md
```

#### 3. Testing Patterns Example
```
examples/testing_patterns/
├── lib/
│   └── examples/
│       ├── basic_genserver.ex
│       ├── supervisor_tree.ex
│       └── distributed_worker.ex
│
├── test/
│   └── examples/
│       ├── basic_test.exs          # Supertester basics
│       ├── isolation_test.exs      # Sandbox usage
│       ├── chaos_test.exs          # Chaos engineering
│       └── distributed_test.exs    # Distributed testing
│
└── README.md
```

### Implementation Checklist
- [ ] Create examples directory
- [ ] Build todo app example
- [ ] Build distributed cache example
- [ ] Build testing patterns example
- [ ] Write README for each example
- [ ] Add to documentation
- [ ] Create video tutorials

---

## Gap 8: Telemetry Dashboard

### Current State
- ✅ Telemetry events emitted
- ✅ Basic Phoenix LiveDashboard
- ❌ No Arsenal operation metrics
- ❌ No sandbox metrics
- ❌ No integration metrics

### Target Dashboard

#### Custom LiveDashboard Page
```elixir
# lib/otp_supervisor_web/live/ecosystem_dashboard_live.ex
defmodule OtpSupervisorWeb.EcosystemDashboardLive do
  use OtpSupervisorWeb, :live_view

  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(1000, self(), :update)
    end

    {:ok, assign_metrics(socket)}
  end

  def handle_info(:update, socket) do
    {:noreply, assign_metrics(socket)}
  end

  defp assign_metrics(socket) do
    assign(socket,
      arsenal_metrics: get_arsenal_metrics(),
      sandbox_metrics: get_sandbox_metrics(),
      integration_metrics: get_integration_metrics()
    )
  end

  defp get_arsenal_metrics do
    # Operation execution counts
    # Average execution time
    # Error rates
    # Most used operations
  end

  defp get_sandbox_metrics do
    # Active sandboxes
    # Sandbox creation/destruction rate
    # Average sandbox lifetime
  end

  defp get_integration_metrics do
    # Cross-library calls
    # Integration points
    # Performance bottlenecks
  end
end
```

#### Metrics Collection
```elixir
# lib/otp_supervisor/telemetry/metrics_collector.ex
defmodule OTPSupervisor.Telemetry.MetricsCollector do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    # Attach to telemetry events
    :telemetry.attach_many(
      "metrics-collector",
      [
        [:arsenal, :operation, :stop],
        [:otp_supervisor, :sandbox, :created],
        [:otp_supervisor, :sandbox, :destroyed]
      ],
      &handle_event/4,
      nil
    )

    {:ok, %{
      operation_counts: %{},
      operation_durations: %{},
      sandbox_count: 0,
      sandbox_events: []
    }}
  end

  def handle_event([:arsenal, :operation, :stop], measurements, metadata, _config) do
    GenServer.cast(__MODULE__, {:operation_completed, metadata.operation, measurements.duration})
  end

  # ... more handlers
end
```

### Implementation Checklist
- [ ] Design dashboard layout
- [ ] Create metrics collector
- [ ] Build LiveView dashboard
- [ ] Add charts and visualizations
- [ ] Integrate with LiveDashboard
- [ ] Add export capabilities
- [ ] Document metrics

---

## Gap 9: Performance Testing

### Current State
- ✅ Unit tests with good coverage
- ❌ No load testing
- ❌ No benchmarks
- ❌ No performance regression testing

### Required Performance Tests

#### 1. Benchmark Suite
```elixir
# benchmark/ecosystem_bench.exs
defmodule EcosystemBench do
  use Benchee

  def run do
    Benchee.run(%{
      "arsenal operation execution" => fn ->
        Arsenal.execute(:list_processes, %{limit: 100})
      end,
      "sandbox creation" => fn ->
        {:ok, _} = OTPSandbox.create("bench_#{:rand.uniform(10000)}", Worker)
      end,
      "sandbox destruction" => fn id ->
        OTPSandbox.destroy(id)
      end
    },
    time: 10,
    memory_time: 2,
    formatters: [
      Benchee.Formatters.Console,
      {Benchee.Formatters.HTML, file: "benchmark/results.html"}
    ])
  end
end

EcosystemBench.run()
```

#### 2. Load Tests
```elixir
# test/load/api_load_test.exs
defmodule LoadTests.APITest do
  use ExUnit.Case

  @tag :load
  test "API handles 1000 concurrent requests" do
    tasks = for i <- 1..1000 do
      Task.async(fn ->
        HTTPoison.get("http://localhost:4000/api/v1/processes")
      end)
    end

    results = Task.await_many(tasks, 30_000)

    # All requests should succeed
    assert Enum.all?(results, fn {:ok, %{status_code: code}} -> code == 200 end)
  end
end
```

#### 3. Memory Leak Tests
```elixir
# test/memory/leak_test.exs
defmodule MemoryTests.LeakTest do
  use ExUnit.Case

  @tag :memory
  test "no memory leak from sandbox lifecycle" do
    initial_memory = :erlang.memory(:total)

    for _ <- 1..1000 do
      {:ok, pid} = OTPSandbox.create("test_#{:rand.uniform(10000)}", Worker)
      OTPSandbox.destroy(pid)
    end

    :erlang.garbage_collect()
    final_memory = :erlang.memory(:total)

    # Memory should not grow significantly
    assert final_memory < initial_memory * 1.1
  end
end
```

### Implementation Checklist
- [ ] Add Benchee dependency
- [ ] Create benchmark suite
- [ ] Write load tests
- [ ] Add memory leak tests
- [ ] Set up performance CI
- [ ] Create performance dashboard
- [ ] Document performance characteristics

---

## Priority Matrix

| Gap | Impact | Effort | Priority | Estimated Time |
|-----|--------|--------|----------|----------------|
| 1. ArsenalPlug Routes | High | Low | P0 - Critical | 1 day |
| 2. OTP Sandbox Extract | High | High | P1 - High | 1 week |
| 3. Playwriter Integration | Medium | Medium | P2 - Medium | 3 days |
| 4. Unified Docs | High | High | P1 - High | 1 week |
| 5. Integration Tests | High | Medium | P1 - High | 3 days |
| 6. CLI Tools | Medium | High | P2 - Medium | 1 week |
| 7. Example Projects | Medium | High | P2 - Medium | 1 week |
| 8. Telemetry Dashboard | Low | Medium | P3 - Low | 3 days |
| 9. Performance Tests | Low | Medium | P3 - Low | 3 days |

---

## Summary

### Must Have (P0-P1)
1. **ArsenalPlug Router Integration** - Enable API access
2. **OTP Sandbox Extraction** - Make library standalone
3. **Unified Documentation** - Enable ecosystem adoption
4. **Integration Tests** - Ensure quality

### Should Have (P2)
5. **Playwriter Integration** - UI testing capabilities
6. **CLI Tools** - Better developer experience
7. **Example Projects** - Learning resources

### Nice to Have (P3)
8. **Telemetry Dashboard** - Operational visibility
9. **Performance Tests** - Production readiness

### Total Estimated Time
- **Critical Path**: 2 weeks
- **Full Implementation**: 4-5 weeks

---

## Next Steps

See **05_implementation_roadmap.md** for phased implementation plan with detailed tasks and milestones.
