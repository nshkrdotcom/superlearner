# Phase 1: Enhanced Prompts for OTP Supervisor Production Tool - REVISED

## Overview

This document contains five comprehensive Test-Driven Development (TDD) prompts for enhancing the OTP Supervisor Production Tool. Each prompt follows strict TDD methodology with required reading, test writing, implementation, and compliance verification phases.

## Prompt 1: Advanced Process Monitoring with TDD

**Objective**: Implement comprehensive process monitoring capabilities with real-time metrics collection and historical data tracking.

### Required Reading Phase

Before writing any code, you MUST read and understand:

1. **Test Design Standards** (`docs/code-standards/test-design-reference.md`)
   - Study the TDD methodology requirements
   - Understand test isolation and naming conventions
   - Review assertion patterns and test structure guidelines

2. **Existing Process Monitoring** (`lib/otp_supervisor/core/control.ex`)
   - Analyze current `list_all_processes/0` implementation
   - Understand existing process introspection capabilities
   - Study the process information data structures

3. **Performance Metrics Pattern** (`lib/otp_supervisor_web/live/system_dashboard_live.ex`)
   - Review how system metrics are currently calculated
   - Understand the real-time update mechanisms
   - Study the existing LiveView integration patterns

### Test-First Implementation Phase

Write comprehensive failing tests that cover:

1. **Process Metrics Collection**
   ```elixir
   test "collects comprehensive process metrics including memory, reductions, and message queue stats"
   test "tracks process metrics over time with configurable sampling intervals"
   test "handles edge cases like process death during metrics collection"
   ```

2. **Real-Time Monitoring**
   ```elixir
   test "provides real-time process monitoring with subscribe/unsubscribe functionality"
   test "broadcasts process state changes to subscribed observers"
   test "handles high-frequency updates without overwhelming subscribers"
   ```

3. **Historical Data Management**
   ```elixir
   test "maintains historical process metrics with configurable retention periods"
   test "provides efficient querying of historical data by process, time range, and metric type"
   test "automatically prunes old data to prevent memory leaks"
   ```

### Implementation Requirements

Create `lib/otp_supervisor/core/process_monitor.ex` with:

- `ProcessMonitor` GenServer for coordinating monitoring activities
- Real-time metrics collection using `:erlang.process_info/2`
- Historical data storage with efficient time-series data structures
- Subscription mechanism for real-time updates using Phoenix.PubSub
- Automatic cleanup of stale data and dead process references

### Compliance Review Phase

Verify that your implementation:
- Uses only standard Erlang/OTP process introspection APIs
- Follows proper GenServer patterns with supervision
- Implements robust error handling for process death scenarios
- Maintains test coverage above 95% for all public functions
- Documents all public APIs with `@doc` and `@spec` annotations

## Prompt 2: Enhanced Supervisor Analytics with TDD

**Objective**: Implement advanced supervisor analytics including restart pattern analysis, failure prediction, and performance optimization recommendations.

### Required Reading Phase

You MUST thoroughly read:

1. **Existing Analytics** (`lib/otp_supervisor/core/control.ex`)
   - Study current `list_supervisors/0` implementation
   - Understand supervisor information gathering
   - Analyze supervision tree traversal patterns

2. **LiveView Analytics Display** (`lib/otp_supervisor_web/live/supervisor_live.ex`)
   - Review how supervisor data is currently presented
   - Understand the real-time update mechanisms
   - Study the user interaction patterns

3. **Test Infrastructure** (`test/support/supervisor_test_helper.ex`)
   - Understand how test supervisors are created and managed
   - Review isolation patterns for supervisor testing
   - Study the cleanup mechanisms

### Test-First Implementation Phase

Write failing tests for:

1. **Restart Pattern Analysis**
   ```elixir
   test "analyzes restart patterns to identify problematic children"
   test "calculates restart frequency and intensity metrics"
   test "detects restart storms and provides early warnings"
   ```

2. **Performance Analytics**
   ```elixir
   test "measures supervisor overhead and child startup times"
   test "analyzes memory usage patterns across supervision trees"
   test "provides performance recommendations based on metrics"
   ```

3. **Failure Prediction**
   ```elixir
   test "identifies supervisors at risk of restart storms"
   test "predicts likely failure points based on historical data"
   test "provides actionable recommendations for supervision strategy adjustments"
   ```

### Implementation Requirements

Create `lib/otp_supervisor/core/supervisor_analytics.ex` with:

- Real-time supervisor monitoring without external state tracking
- Statistical analysis of restart patterns and timing
- Performance profiling of supervisor operations
- Predictive algorithms for failure risk assessment
- Recommendation engine for supervision strategy optimization

### Compliance Review Phase

Ensure your implementation:
- Never interferes with normal supervisor operation
- Uses only observer patterns, never controller patterns
- Provides actionable insights backed by data
- Maintains comprehensive test coverage
- Includes detailed documentation and examples

## Prompt 3: Interactive Process Debugging Tools with TDD

**Objective**: Create sophisticated debugging tools for process inspection, message tracing, and state analysis for production OTP monitoring.

### Required Reading Phase

Read and understand:

1. **Message Tracing** (`lib/otp_supervisor/core/message_tracer.ex`)
   - Study existing tracing implementation
   - Understand trace data structures and patterns
   - Review performance considerations

2. **Process State Access** (`lib/otp_supervisor/core/control.ex`)
   - Analyze `get_process_state/1` implementation
   - Understand GenServer state access patterns
   - Study error handling for non-GenServer processes

3. **Web Interface Patterns** (`lib/otp_supervisor_web/live/`)
   - Review LiveView component organization
   - Understand real-time update mechanisms
   - Study user interaction patterns

### Test-First Implementation Phase

Create comprehensive tests for:

1. **Advanced Message Tracing**
   ```elixir
   test "traces messages with filtering by type, sender, and content patterns"
   test "provides message flow visualization between processes"
   test "handles high-volume message tracing without performance degradation"
   ```

2. **State Inspection Tools**
   ```elixir
   test "provides deep inspection of GenServer state with type information"
   test "tracks state changes over time with diff capabilities"
   test "safely handles complex data structures and circular references"
   ```

3. **Interactive Debugging Interface**
   ```elixir
   test "allows real-time interaction with processes through web interface"
   test "provides safe process manipulation tools with undo capabilities"
   test "includes comprehensive debugging context and operation history"
   ```

### Implementation Requirements

Develop `lib/otp_supervisor/core/process_debugger.ex` and corresponding LiveView components:

- Advanced message filtering and pattern matching
- Safe state inspection with serialization handling
- Interactive debugging tools with comprehensive operation logging
- Performance monitoring during debugging operations
- Comprehensive safety checks to prevent system disruption

### Compliance Review Phase

Verify that:
- Debugging operations never compromise system stability
- All tools include comprehensive operation logging and audit trails
- Performance impact is minimized and measurable
- Safety mechanisms prevent accidental process termination
- Test coverage demonstrates safe operation under all conditions

## Prompt 4: API Migration and Cleanup with TDD

**Objective**: Remove simulated/fake functionality and implement proper OTP patterns while maintaining API compatibility where possible.

### Required Reading Phase

Before any implementation, you MUST read:

1. **Current API Implementation**
   - `lib/otp_supervisor_web/controllers/api/v1/supervisor_controller.ex` - Study ALL endpoints
   - `lib/otp_supervisor_web/controllers/api/v1/process_controller.ex` - Review process management
   - `lib/otp_supervisor_web/controllers/api/v1/system_controller.ex` - Analyze system endpoints

2. **Flawed Modules to Remove** (understand WHY they're harmful)
   - `lib/otp_supervisor/core/restart_tracker.ex` - External monitoring antipattern
   - `lib/otp_supervisor/core/supervisor_controller.ex` - Destructive pause/resume simulation
   - Related functions in `lib/otp_supervisor/core/control.ex`

3. **API Routes Configuration**
   - `lib/otp_supervisor_web/router.ex` - Current API routing structure
   - Test files: `test/otp_supervisor_web/controllers/api/v1/*_test.exs`

4. **Documentation Analysis**
   - `docs/impl/harmful-patterns-analysis.md` - Why current patterns are harmful
   - `docs/impl/proper-otp-replacements.md` - What should replace them

### Test-First Implementation Phase

Write failing tests that demonstrate the migration:

1. **API Endpoint Removal Tests**
   ```elixir
   test "pause endpoint returns 410 Gone with deprecation message"
   test "resume endpoint returns 410 Gone with migration information"
   test "removed endpoints include proper error responses with alternatives"
   ```

2. **Analytics Endpoint Migration Tests**
   ```elixir
   test "analytics endpoint returns telemetry-based data in compatible format"
   test "new analytics maintain backward-compatible field names where possible"
   test "deprecated fields include warnings and replacement recommendations"
   ```

3. **New Sandbox Management Tests**
   ```elixir
   test "new sandbox endpoints provide real supervisor lifecycle management"
   test "sandbox creation uses proper OTP supervisor start patterns"
   test "sandbox operations maintain isolation and proper cleanup"
   ```

### Implementation Requirements

Phase 1 - Remove Harmful Patterns:
```elixir
# DELETE these files entirely:
# - lib/otp_supervisor/core/restart_tracker.ex
# - lib/otp_supervisor/core/supervisor_controller.ex

# REMOVE these functions from Control module:
# - start_restart_tracking/1
# - get_restart_history/1 (old version)
# - record_restart_event/5
# - pause_supervisor/1
# - resume_supervisor/1
# - supervisor_paused?/1
```

Phase 2 - Implement Telemetry-Based Analytics:
```elixir
# CREATE: lib/otp_supervisor/core/analytics_server.ex
# - Uses :telemetry events for restart tracking
# - Provides real-time analytics without external state
# - Maintains compatibility with analytics API endpoint

# ADD to Control module:
# - get_restart_history/1 (telemetry-based)
# - get_supervisor_analytics/0
# - get_failure_rate/2
```

Phase 3 - Real Sandbox Management:
```elixir
# CREATE: lib/otp_supervisor/core/sandbox_manager.ex
# - Real supervisor lifecycle management
# - Proper OTP patterns for production supervisor management
# - Safe isolation between sandboxes

# ADD to Control module:
# - create_sandbox/2
# - destroy_sandbox/1
# - restart_sandbox/1
# - list_sandboxes/0
# - get_sandbox_info/1
```

### API Migration Strategy

Update `supervisor_controller.ex`:

1. **Remove Endpoints** (return 410 Gone):
   ```elixir
   def pause(conn, _params) do
     conn
     |> put_status(410)
     |> json(%{
       error: %{
         code: "endpoint_removed",
         message: "Pause functionality removed - was harmful simulation",
         alternative: "Use sandbox management: POST /api/v1/sandboxes"
       }
     })
   end
   ```

2. **Migrate Analytics** (maintain compatibility):
   ```elixir
   def analytics(conn, %{"name" => supervisor_name}) do
     # Use new telemetry-based analytics
     analytics = Control.get_supervisor_analytics(supervisor_name)
     
     # Transform to maintain API compatibility where possible
     formatted_analytics = format_analytics_for_api(analytics)
     
     conn
     |> put_status(200)
     |> json(%{data: formatted_analytics})
   end
   ```

3. **Add New Sandbox Endpoints**:
   ```elixir
   # Add to router.ex:
   post "/sandboxes", SandboxController, :create
   get "/sandboxes", SandboxController, :index
   get "/sandboxes/:id", SandboxController, :show
   put "/sandboxes/:id/restart", SandboxController, :restart
   delete "/sandboxes/:id", SandboxController, :destroy
   ```

### Web Interface Updates

Update LiveView components:

1. **Remove Harmful UI Elements**:
   - Delete pause/resume buttons from supervisor interface
   - Remove restart tracking displays that use external monitoring
   - Update analytics dashboards to use telemetry data

2. **Add Professional Context**:
   - Document why pause/resume was removed for production stability
   - Demonstrate proper OTP lifecycle patterns
   - Add sandbox management interface

### Compliance Review Phase

Verify your migration:

1. **API Compatibility**:
   - Non-breaking endpoints still work identically
   - Breaking changes return proper HTTP status codes (410 Gone)
   - New endpoints follow existing API patterns and conventions
   - All responses include migration guidance where applicable

2. **OTP Correctness**:
   - No external monitoring of supervisor internals
   - All supervisor operations use proper OTP APIs
   - Telemetry integration follows OTP best practices
   - Sandbox management demonstrates real supervisor patterns

3. **Production Value**:
   - Clear documentation of why changes were made
   - Examples of proper OTP patterns replacing harmful ones
   - Migration guides and documentation updates
   - Professional context in error messages and API responses

4. **Test Coverage**:
   - All new functionality has comprehensive tests
   - Migration paths are fully tested
   - Error conditions and edge cases covered
   - Performance impact measured and documented

### Required Documentation Updates

Create/update these files:
- `API.md` - Document endpoint changes and migration paths
- `docs/api-migration-guide.md` - Comprehensive migration guide for API consumers
- `docs/otp-patterns-explained.md` - Documentation about proper OTP patterns
- Update all existing API documentation to reflect changes

## Prompt 5: REST API for External Tool Integration with TDD

**Objective**: Maintain and enhance the existing comprehensive REST API for external tools to interact with OTP processes, ensuring it works with the cleaned-up OTP patterns.

### Required Reading Phase

You MUST read and understand:

1. **Current API Implementation** (verify compatibility with cleanup)
   - `lib/otp_supervisor_web/controllers/api/v1/process_controller.ex`
   - `lib/otp_supervisor_web/controllers/api/v1/system_controller.ex`  
   - Updated `lib/otp_supervisor_web/controllers/api/v1/supervisor_controller.ex`

2. **Router Configuration**
   - `lib/otp_supervisor_web/router.ex` - Existing API routes
   - Ensure all routes work with cleaned-up backend

3. **Core Control Module** (post-cleanup)
   - `lib/otp_supervisor/core/control.ex` - Verify all API dependencies exist
   - New telemetry-based analytics functions
   - New sandbox management functions

### Test-First Implementation Phase

Write comprehensive tests ensuring API works with cleaned-up backend:

1. **Process API Verification Tests**
   ```elixir
   test "process listing works with cleaned-up process monitoring"
   test "process state access works without external tracking"
   test "message tracing integrates properly with real OTP patterns"
   ```

2. **Supervisor API Migration Tests** 
   ```elixir
   test "supervisor listing works with telemetry-based analytics"
   test "analytics endpoint returns telemetry data in compatible format"
   test "removed endpoints return proper deprecation responses"
   ```

3. **New Sandbox API Tests**
   ```elixir
   test "sandbox creation endpoint creates real supervisors"
   test "sandbox management provides proper OTP lifecycle operations"
   test "sandbox isolation prevents interference between instances"
   ```

### Implementation Requirements

Verify and enhance these controllers work with cleaned-up patterns:

1. **ProcessController** - ensure compatibility with real monitoring
2. **SystemController** - verify telemetry integration 
3. **SupervisorController** - implement migration strategy from Prompt 4
4. **New SandboxController** - implement real supervisor management

### Compliance Review Phase

Ensure the API:
- Works correctly with real OTP patterns (no simulated functionality)
- Maintains backward compatibility where possible
- Provides clear migration paths for breaking changes
- Demonstrates proper OTP production patterns
- Has comprehensive test coverage for all endpoints

---

## TDD Methodology Requirements

All prompts must follow this strict TDD process:

1. **Read First**: Complete all required reading before writing any code
2. **Test First**: Write failing tests that fully specify the desired behavior
3. **Implement**: Write minimal code to make tests pass
4. **Refactor**: Improve code quality while keeping tests green
5. **Review**: Verify compliance with OTP best practices and production requirements

## Quality Standards

- Minimum 95% test coverage for all new code
- All public functions must have `@doc` and `@spec` annotations
- Follow existing code style and patterns
- Include comprehensive error handling
- Provide comprehensive documentation explaining OTP patterns
- Never compromise system stability for features

## Production Focus

Remember that this tool is for production OTP monitoring and management. Every implementation should:
- Demonstrate proper OTP patterns
- Avoid antipatterns and harmful practices
- Include comprehensive documentation
- Show production-ready techniques
- Provide robust supervision tree and process management capabilities