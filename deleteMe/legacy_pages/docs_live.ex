defmodule OtpSupervisorWeb.DocsLive do
  @moduledoc """
  Documentation center with comprehensive OTP system architecture docs
  """
  use OtpSupervisorWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "Documentation")
     |> assign(:current_section, "supervisor-architecture")
     |> assign(:search_query, "")}
  end

  def handle_event("navigate_section", %{"section" => section}, socket) do
    {:noreply, assign(socket, :current_section, section)}
  end

  def handle_event("search", %{"search" => query}, socket) do
    {:noreply, assign(socket, :search_query, query)}
  end

  def get_doc_sections do
    [
      %{
        id: "supervisor-architecture",
        title: "SUPERVISOR ARCHITECTURE",
        description: "OTP supervision tree design and patterns"
      },
      %{
        id: "sandbox-system",
        title: "SANDBOX SYSTEM",
        description: "Dynamic supervisor lifecycle management"
      },
      %{
        id: "arsenal-operations",
        title: "ARSENAL OPERATIONS",
        description: "REST API operation framework"
      },
      %{
        id: "monitoring-analytics",
        title: "MONITORING & ANALYTICS",
        description: "Real-time process monitoring system"
      },
      %{
        id: "process-control",
        title: "PROCESS CONTROL",
        description: "Process introspection and manipulation"
      },
      %{
        id: "testing-patterns",
        title: "TESTING PATTERNS",
        description: "OTP testing strategies and helpers"
      }
    ]
  end

  def get_doc_content("supervisor-architecture") do
    """
    # SUPERVISOR ARCHITECTURE OVERVIEW

    ## BOOT-TIME SUPERVISION TREE

    The main application supervisor (OtpSupervisor.Supervisor) orchestrates 
    the entire system using a :one_for_one restart strategy:

    OtpSupervisor.Supervisor (:one_for_one)
    ├── OtpSupervisorWeb.Telemetry          [Phoenix telemetry]
    ├── DNSCluster                          [Service discovery]
    ├── Phoenix.PubSub                      [Pub/sub messaging]
    ├── Finch                               [HTTP client]
    ├── TracerRegistry                      [Message tracing]
    ├── AnalyticsServer                     [Supervisor monitoring]
    ├── SandboxManager                      [Dynamic supervisors]
    ├── Arsenal.Registry                    [Operation discovery]
    ├── OtpSupervisorWeb.Endpoint          [Phoenix web server]
    └── DemoSupervisor                      [Demo processes]
        ├── :counter_1                      [Counter worker (val: 0)]
        ├── :counter_2                      [Counter worker (val: 100)]
        └── :printer_1                      [Printer worker]

    ## SUPERVISOR TYPES

    ### PERMANENT SUPERVISORS
    - Started at application boot
    - Registered with predictable names
    - Core system functionality
    - Managed by main application supervisor

    ### DYNAMIC SUPERVISORS  
    - Created at runtime via SandboxManager
    - Generated unique names (collision-safe)
    - Isolated testing and temporary workloads
    - Monitored but not linked (fault isolation)

    ## RESTART STRATEGIES

    ### :one_for_one (Default)
    - Only failed child is restarted
    - Other children continue unaffected
    - Best for independent processes

    ### :one_for_all
    - All children restart if one fails
    - Useful for interdependent processes
    - More disruptive but ensures consistency

    ### :rest_for_one
    - Failed child and subsequent children restart
    - Maintains child start order dependencies
    - Compromise between independence and consistency

    ## DETECTION ALGORITHMS

    The system uses multiple supervisor detection strategies:

    ### Control.list_supervisors()
    - Scans Process.registered() for supervisor names
    - Examines process dictionary :$initial_call
    - Filters by supervisor module types
    - Returns 56 total supervisors in current system

    ### Arsenal.ListSupervisors
    - Comprehensive Process.list() scanning
    - Enhanced detection patterns
    - Application attribution
    - Pagination and child details

    ## PROCESS ISOLATION

    ### SandboxManager Isolation
    - Uses Process.unlink() to prevent cascade failures
    - Monitors created supervisors without linking
    - Cleans up on supervisor death via :DOWN messages
    - ETS-based O(1) sandbox lookup

    ### Registration Patterns
    Boot-time: :demo_one_for_one, :analytics_server
    Dynamic:   :"sandbox_\#{id}_\#{unique_id}"
    Workers:   :"counter_1_\#{unique_id}", :"printer_1_\#{unique_id}"
    """
  end

  def get_doc_content("sandbox-system") do
    """
    # SANDBOX SYSTEM ARCHITECTURE

    ## SANDBOXMANAGER OVERVIEW

    The SandboxManager implements a sophisticated supervisor-of-supervisors
    pattern for dynamic process lifecycle management:

    ### CORE FEATURES
    - Dynamic supervisor creation at runtime
    - Process isolation via unlinking
    - ETS-based fast lookup (:sandboxes table)
    - Monitoring without linking for fault tolerance
    - Unique name generation to prevent conflicts

    ### LIFECYCLE OPERATIONS

    #### CREATE_SANDBOX
    1. Generate unique sandbox name: :"sandbox_\#{id}_\#{unique_id}"
    2. Start supervisor with merged options
    3. Unlink supervisor (critical for isolation)
    4. Monitor supervisor process
    5. Store metadata in ETS and GenServer state

    #### DESTROY_SANDBOX
    1. Stop monitoring supervisor
    2. Gracefully shutdown supervisor (5s timeout)
    3. Remove from ETS table and state
    4. Clean up all associated resources

    #### RESTART_SANDBOX
    1. Stop current supervisor
    2. Start new supervisor with same configuration
    3. Update monitoring reference
    4. Increment restart count
    5. Update metadata

    ### DATA STRUCTURES

    #### ETS Table (:sandboxes)
    {sandbox_id, %{
      id: sandbox_id,
      supervisor_module: module,
      supervisor_pid: pid,
      opts: keyword_list,
      created_at: timestamp,
      restart_count: integer
    }}

    #### GenServer State
    %{
      sandboxes: %{
        sandbox_id => {sandbox_info, monitor_ref}
      },
      next_id: integer
    }

    ### FAULT TOLERANCE

    #### Monitor Handling
    - Receives :DOWN messages when supervisors die
    - Automatically cleans up dead sandbox entries
    - Logs death reasons for debugging
    - Maintains system consistency

    #### Critical Design Decision
    Process.unlink(pid) after supervisor creation prevents SandboxManager
    from dying when sandbox supervisors are killed, ensuring system stability.

    ### API EXAMPLES

    #### Creating Test Supervisors
    {:ok, info} = SandboxManager.create_sandbox(
      "test_env_1",
      TestSupervisor,
      [strategy: :one_for_all, workers: 5]
    )

    #### Runtime Management
    SandboxManager.restart_sandbox("test_env_1")
    SandboxManager.destroy_sandbox("test_env_1")
    SandboxManager.list_sandboxes()

    ### INTEGRATION WITH DEMO SUPERVISOR

    The :demo_one_for_one supervisor is PERMANENT (boot-time) while
    sandbox supervisors are DYNAMIC (runtime). Both coexist:

    - Demo supervisor: Fixed infrastructure for demonstrations
    - Sandbox supervisors: Isolated environments for testing
    - No conflicts due to unique naming schemes
    - Independent lifecycle management
    """
  end

  def get_doc_content("arsenal-operations") do
    """
    # ARSENAL OPERATIONS FRAMEWORK

    ## ARCHITECTURE OVERVIEW

    Arsenal provides a dynamic REST API generation framework for OTP
    operations with automatic discovery, validation, and execution.

    ### CORE COMPONENTS

    #### Arsenal.Registry
    - GenServer-based operation discovery
    - ETS table for O(1) operation lookup
    - Automatic registration of operation modules
    - Support for 200+ planned operations

    #### ArsenalPlug
    - HTTP middleware for dynamic routing
    - Path reconstruction from catch-all routes
    - Parameter extraction and validation
    - Error handling and response formatting

    #### Operation Modules
    - Standardized behavior interface
    - REST configuration metadata
    - Parameter validation logic
    - Execution and response formatting

    ### OPERATION LIFECYCLE

    #### 1. DISCOVERY
    Registry scans for modules implementing Arsenal.Operation behavior:

    @known_operations [
      OTPSupervisor.Core.Arsenal.Operations.GetProcessInfo,
      OTPSupervisor.Core.Arsenal.Operations.KillProcess,
      OTPSupervisor.Core.Arsenal.Operations.ListSupervisors,
      OTPSupervisor.Core.Arsenal.Operations.SendMessage,
      OTPSupervisor.Core.Arsenal.Operations.TraceProcess
    ]

    #### 2. REGISTRATION
    Each operation provides rest_config/0 with:
    - HTTP method and path pattern
    - Parameter definitions and validation rules
    - Response schemas and error codes
    - Operation summary and description

    #### 3. ROUTING
    ArsenalPlug dynamically matches requests:
    - Reconstructs paths from Phoenix catch-all routes
    - Applies regex pattern matching
    - Handles URL encoding for PID parameters
    - Falls back to manual controllers on no match

    #### 4. EXECUTION
    - Parameter merging (path, query, body)
    - Validation via operation module
    - Safe execution with error handling
    - Response formatting and HTTP status

    ### HYBRID ARCHITECTURE

    Arsenal coexists with manual Phoenix controllers:

    #### Arsenal-First Approach
    1. ArsenalPlug attempts operation matching
    2. On match: Execute Arsenal operation
    3. On no match: Fall through to manual controller
    4. Catch-all routes return Arsenal 404 errors

    #### Route Configuration
    scope "/api/v1", OtpSupervisorWeb.Api.V1 do
      pipe_through [:api, ArsenalPlug]
      
      # Manual controllers (non-conflicting)
      get "/processes", ProcessController, :index
      get "/system/health", SystemController, :health
    end

    scope "/api/v1", OtpSupervisorWeb do
      pipe_through [:api, ArsenalPlug]
      match :*, "/*path", ArsenalController, :operation_handler
    end

    ### ACTIVE OPERATIONS

    #### GetProcessInfo
    GET /api/v1/processes/:pid/info
    - Complete process introspection
    - Memory, message queue, status
    - Links, monitors, registered name

    #### KillProcess  
    DELETE /api/v1/processes/:pid
    - Graceful or forced termination
    - Configurable exit reasons
    - Critical process protection

    #### ListSupervisors
    GET /api/v1/supervisors
    - System-wide supervisor discovery
    - Application attribution
    - Children information (optional)
    - Pagination support

    #### SendMessage
    POST /api/v1/processes/:pid/message
    - Arbitrary message sending
    - Support for send, cast, call types
    - Response tracking capabilities
    - Timeout configuration

    #### TraceProcess
    POST /api/v1/processes/:pid/trace
    - Enable process tracing
    - Configurable trace types
    - Data collection and retrieval
    - Performance monitoring

    ### ERROR HANDLING

    #### Validation Errors (422)
    - Parameter type mismatches
    - Missing required parameters
    - Invalid PID formats
    - Out-of-range values

    #### Process Errors (404)
    - Process not found/dead
    - Invalid process references
    - Permission denied

    #### Execution Errors (500)
    - Operation timeouts
    - System errors
    - Unexpected failures

    ### PLANNED OPERATIONS (195)

    Categories include:
    - Process Lifecycle (12 ops)
    - Advanced Control (10 ops)  
    - Supervisor Management (15 ops)
    - System Introspection (15 ops)
    - Messaging (12 ops)
    - Tracing/Debugging (15 ops)
    - State Management (8 ops)
    - Error Handling (7 ops)
    - Application Management (6 ops)
    - Resource Management (8 ops)
    - Performance Monitoring (8 ops)
    - Security (5 ops)
    - Testing (5 ops)
    - OTP Patterns (6 ops)
    - Distributed Operations (5 ops)
    - Events (4 ops)
    - Hot Code Reloading (4 ops)
    - System Integration (6 ops)
    """
  end

  def get_doc_content("monitoring-analytics") do
    """
    # MONITORING & ANALYTICS SYSTEM

    ## ANALYTICSSERVER ARCHITECTURE

    Real-time supervisor monitoring with restart detection and analysis.

    ### MONITORING STRATEGY

    #### Process-Based Monitoring
    Due to OTP 27 telemetry limitations, uses periodic process scanning:

    - Scans all supervisors every 5 seconds
    - Compares supervisor children between scans
    - Detects child restarts via PID changes
    - Records restart events with timestamps

    #### Data Collection
    - Supervisor metadata (PID, strategy, intensity)
    - Child process information (PIDs, types, modules)
    - Restart events with reasons and context
    - Failure rate calculations over time windows

    ### DATA STRUCTURES

    #### State Schema
    %{
      restart_history: %{
        supervisor_pid => [
          %{event_type, child_id, timestamp, metadata}
        ]
      },
      supervisor_info: %{
        supervisor_pid => %{name, strategy, intensity, period}
      },
      supervisor_children: %{
        supervisor_pid => %{child_id => child_info}
      },
      total_restarts: integer(),
      start_time: timestamp()
    }

    #### Event Types
    - :child_started - New child process detected
    - :child_restarted - Child PID changed (restart detected)
    - :child_terminated - Child no longer present
    - :supervisor_started - New supervisor detected
    - :supervisor_stopped - Supervisor no longer running

    ### RESTART DETECTION ALGORITHM

    #### Child Comparison Logic
    1. Get current supervisor children via Supervisor.which_children/1
    2. Compare with previous scan results
    3. Detect PID changes for same child IDs
    4. Record restart events with metadata
    5. Update child tracking state

    #### Restart Event Recording
    For each detected restart:
    - Record timestamp (millisecond precision)
    - Capture child ID and new PID
    - Store supervisor context
    - Maintain bounded history (1000 events per supervisor)

    ### ANALYTICS FEATURES

    #### Failure Rate Calculation
    get_failure_rate(supervisor_pid, time_window_ms) ->
      - Count restarts within time window
      - Calculate rate per second
      - Provide restart count and rate

    #### Supervisor Statistics
    - Total restarts across all supervisors
    - Per-supervisor restart counts
    - Uptime calculations
    - Children statistics (active, specs, workers, supervisors)

    #### Historical Analysis
    - Restart history with full event context
    - Time-based filtering
    - Trend analysis over configurable windows
    - Pattern detection for debugging

    ### INTEGRATION POINTS

    #### Control Module Integration
    - get_restart_history/1 - Retrieve supervisor restart events
    - get_supervisor_analytics/0 - Global statistics
    - get_failure_rate/2 - Calculated failure rates

    #### Web Interface Integration
    - Real-time metrics display
    - Historical charts and graphs
    - Alert thresholds and notifications
    - Export capabilities for analysis

    ### PERFORMANCE CONSIDERATIONS

    #### Memory Management
    - Bounded event history prevents memory leaks
    - Periodic cleanup of old events
    - Efficient ETS usage for fast lookups

    #### Monitoring Overhead
    - 5-second scan interval balances accuracy vs performance
    - Lazy supervisor discovery reduces CPU usage
    - Efficient diff algorithms minimize processing

    ### PRODUCTION MONITORING

    #### Health Checks
    - AnalyticsServer process monitoring
    - Scan interval verification
    - Memory usage tracking
    - Error rate monitoring

    #### Alerting Integration
    Ready for integration with external monitoring:
    - Prometheus metrics export
    - StatsD integration
    - Custom webhook notifications
    - Email/SMS alert capabilities
    """
  end

  def get_doc_content("process-control") do
    """
    # PROCESS CONTROL SYSTEM

    ## CONTROL MODULE OVERVIEW

    Comprehensive process introspection and manipulation API for OTP systems.

    ### PROCESS DISCOVERY

    #### list_all_processes(opts)
    Scans all system processes with classification:

    - Process type detection (:supervisor, :genserver, :worker)
    - Registration name discovery
    - Filtering by process type
    - Pagination support for large systems

    #### Process Classification Algorithm
    1. Get initial_call from process dictionary
    2. Match against known OTP behavior modules
    3. Classify as supervisor, genserver, or worker
    4. Handle edge cases (dead processes, unknown types)

    ### SUPERVISOR OPERATIONS

    #### list_supervisors()
    - Filters registered processes for supervisors only
    - Returns supervisor metadata (name, PID, alive, child_count)
    - Uses is_supervisor_pid/1 for accurate detection
    - Handles 56+ supervisors in current system

    #### get_supervision_tree(supervisor)
    - Retrieves complete child information
    - Handles both atom names and PIDs
    - Returns formatted child data with metadata
    - Error handling for invalid supervisors

    ### PROCESS INTROSPECTION

    #### get_process_info(pid)
    Comprehensive process information:
    - Memory usage (bytes)
    - Message queue length
    - Process status (:running, :waiting, etc.)
    - Heap and stack sizes
    - Reduction count
    - Current function

    #### get_process_state(pid)
    Safe GenServer state extraction:
    - Uses :sys.get_state/2 with timeout
    - Handles non-GenServer processes gracefully
    - Timeout protection (100ms)
    - Error categorization

    ### PROCESS MANIPULATION

    #### kill_process(pid_or_string)
    - Supports PID objects and string representations
    - Handles both "#PID<...>" and "<...>" formats
    - Immediate termination with :kill signal
    - Return value normalization

    #### to_pid(identifier)
    Flexible PID conversion:
    - String PID parsing with format normalization
    - Registered name lookup via Process.whereis/1
    - PID object passthrough
    - Comprehensive error handling

    ### PROCESS RELATIONSHIPS

    #### build_process_graph()
    Complete system relationship mapping:
    - Process list with classifications
    - Link relationships between processes
    - Monitor relationships
    - Visual representation data

    #### Link Analysis
    - Bidirectional failure propagation connections
    - Critical for understanding fault tolerance
    - Supervision tree visualization
    - Dependency analysis

    #### Monitor Analysis
    - Unidirectional observation relationships
    - Non-intrusive process watching
    - Event notification patterns
    - Debugging aid for complex systems

    ### ADVANCED FEATURES

    #### Sandbox Integration
    - create_sandbox/2 - Isolated supervisor creation
    - destroy_sandbox/1 - Clean resource disposal
    - restart_sandbox/1 - Fresh supervisor instances
    - list_sandboxes/0 - Active sandbox enumeration

    #### Analytics Integration
    - get_restart_history/1 - Historical restart data
    - get_supervisor_analytics/0 - System-wide statistics
    - get_failure_rate/2 - Calculated failure rates
    - simulate_crash/3 - Controlled failure testing

    ### ERROR HANDLING

    #### Graceful Degradation
    - Dead process detection and handling
    - Timeout protection for unresponsive processes
    - Invalid input validation and normalization
    - Comprehensive error categorization

    #### Error Types
    - :process_dead - Process no longer exists
    - :not_found - Invalid process reference
    - :not_supervisor - Process is not a supervisor
    - :invalid_pid - Malformed PID string

    ### PERFORMANCE OPTIMIZATIONS

    #### Caching Strategies
    - Process.registered/0 caching for supervisor lists
    - Lazy evaluation of expensive operations
    - Bounded result sets for large systems

    #### Batch Operations
    - Efficient bulk process information gathering
    - Parallel processing where safe
    - Memory-conscious large dataset handling

    ### SAFETY CONSIDERATIONS

    #### Production Safety
    - Read-only operations by default
    - Explicit confirmation for destructive operations
    - Timeout protection against hanging
    - Graceful error handling

    #### Critical Process Protection
    - System process identification
    - Prevention of accidental critical process termination
    - Warning systems for dangerous operations
    - Audit logging for security compliance
    """
  end

  def get_doc_content("testing-patterns") do
    """
    # TESTING PATTERNS & STRATEGIES

    ## OTP TESTING ARCHITECTURE

    Comprehensive testing framework for supervisor and process behavior.

    ### TESTING INFRASTRUCTURE

    #### SupervisorTestHelper
    Specialized helper module for OTP testing:

    - Unique supervisor name generation
    - Automatic cleanup after tests
    - Process isolation between test cases
    - Consistent test environment setup

    #### Test Supervisor Creation
    create_test_supervisor(module, opts) ->
      - Generates unique supervisor names
      - Prevents test interference
      - Automatic registration cleanup
      - Consistent initialization patterns

    ### ARSENAL OPERATION TESTING

    #### Integration Test Patterns

    ##### GetProcessInfo Tests
    - Process information accuracy validation
    - Dead process handling
    - Memory and queue statistics verification
    - Response format consistency

    ##### KillProcess Tests
    - Process termination verification
    - Supervisor restart behavior testing
    - Different exit reason handling
    - Critical process protection

    ##### SendMessage Tests
    - Message delivery verification
    - Different message types (send, cast, call)
    - Timeout handling
    - Response tracking

    ##### TraceProcess Tests
    - Trace activation/deactivation
    - Data collection verification
    - Performance impact measurement
    - Multiple trace type handling

    ### SUPERVISOR BEHAVIOR TESTING

    #### Restart Strategy Testing
    Test each restart strategy behavior:

    ##### :one_for_one Testing
    1. Start supervisor with multiple children
    2. Kill one child process
    3. Verify only that child restarts
    4. Confirm other children unaffected

    ##### :one_for_all Testing  
    1. Start supervisor with multiple children
    2. Kill one child process
    3. Verify all children restart
    4. Confirm PID changes for all children

    ##### :rest_for_one Testing
    1. Start supervisor with ordered children
    2. Kill middle child
    3. Verify child and subsequent children restart
    4. Confirm earlier children unaffected

    ### SANDBOX TESTING PATTERNS

    #### Isolation Testing
    - Sandbox creation without main system impact
    - Resource cleanup verification
    - Process leakage prevention
    - Concurrent sandbox testing

    #### Lifecycle Testing
    - Create/destroy cycle verification
    - Restart behavior validation
    - Failure handling robustness
    - Memory leak prevention

    ### PROCESS TESTING STRATEGIES

    #### GenServer Testing
    - State consistency verification
    - Message handling correctness
    - Crash recovery behavior
    - Performance characteristics

    #### Worker Process Testing
    - Functional behavior verification
    - Error condition handling
    - Resource management
    - Integration with supervisors

    ### MONITORING TESTING

    #### Analytics Testing
    - Restart detection accuracy
    - Event recording completeness
    - Statistical calculation correctness
    - Historical data integrity

    #### Real-time Monitoring
    - Live update verification
    - Performance metric accuracy
    - Alert threshold testing
    - Dashboard data consistency

    ### FAULT INJECTION TESTING

    #### Controlled Failure Testing
    simulate_crash/3 patterns:
    - Different crash reasons (:normal, :kill, :shutdown)
    - Timing-based failures
    - Resource exhaustion simulation
    - Network partition simulation

    #### Chaos Engineering
    - Random process termination
    - Resource limit testing
    - Network failure simulation
    - Performance degradation testing

    ### TEST DATA MANAGEMENT

    #### Test Process Creation
    - Lightweight test processes
    - Predictable behavior patterns
    - Easy cleanup and disposal
    - Minimal resource usage

    #### Test State Management
    - Isolated test state
    - Consistent initial conditions
    - Deterministic test outcomes
    - Parallel test execution safety

    ### PERFORMANCE TESTING

    #### Load Testing
    - High child count supervisors
    - Rapid restart scenarios
    - Memory usage under load
    - Response time measurement

    #### Stress Testing
    - System resource exhaustion
    - Maximum concurrent operations
    - Recovery behavior verification
    - Stability under pressure

    ### INTEGRATION TESTING

    #### End-to-End Testing
    - Complete request/response cycles
    - Multi-component interaction
    - Real-world scenario simulation
    - Cross-module integration

    #### API Testing
    - HTTP endpoint verification
    - Parameter validation testing
    - Error response consistency
    - Authentication/authorization

    ### TESTING BEST PRACTICES

    #### Test Organization
    - Logical test grouping by functionality
    - Consistent naming conventions
    - Comprehensive edge case coverage
    - Clear test documentation

    #### Test Reliability
    - Deterministic test outcomes
    - Proper cleanup procedures
    - Race condition prevention
    - Timeout handling

    #### Test Maintenance
    - Regular test review and updates
    - Deprecated functionality removal
    - Performance regression detection
    - Documentation synchronization
    """
  end

  def get_doc_content(_), do: "Documentation section not found."

  def render_markdown(content) do
    content
    |> String.split("\n")
    |> Enum.map(&format_line/1)
    |> Enum.join("")
  end

  defp format_line("# " <> title),
    do:
      "<h1 class='text-green-300 text-2xl font-bold border-b border-green-500/30 pb-2 mb-6'>#{title}</h1>"

  defp format_line("## " <> title),
    do: "<h2 class='text-green-300 text-xl font-bold mt-8 mb-4'>#{title}</h2>"

  defp format_line("### " <> title),
    do: "<h3 class='text-green-300 text-lg font-bold mt-6 mb-3'>#{title}</h3>"

  defp format_line("#### " <> title),
    do: "<h4 class='text-green-300 text-md font-bold mt-4 mb-2'>#{title}</h4>"

  defp format_line(""), do: "<br>"

  defp format_line("    " <> code),
    do:
      "<pre class='bg-black border border-green-500/30 p-3 text-green-400 text-sm ml-4'>#{code}</pre>"

  defp format_line("- " <> item), do: "<div class='ml-4'>• #{item}</div>"
  defp format_line(line), do: "<p class='text-green-400 mb-2'>#{line}</p>"
end
