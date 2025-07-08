# 02 - Implement Telemetry-Based Analytics

## Task
Implement a proper telemetry-based analytics system to replace the flawed RestartTracker. This uses OTP's built-in telemetry events for reliable supervisor monitoring without external simulation.

## Required Reading
**You must read these files before starting:**

1. `docs/TELEMETRY_ANALYTICS_IMPLEMENTATION.md` - Complete implementation guide
2. `docs/REAL_OTP_DESIGN2.md` - Context on why telemetry is the correct approach
3. `lib/otp_supervisor/application.ex` - Where to add AnalyticsServer
4. `lib/otp_supervisor/core/control.ex` - Where to add new analytics functions
5. `test/otp_supervisor/core/control_test.exs` - Pattern for writing tests

## Prerequisites
- ✅ **Phase 01 must be completed first** - All flawed features removed
- ✅ `mix test` passes after cleanup
- ✅ No references to RestartTracker remain in codebase

## Implementation Steps

### Step 1: Create AnalyticsServer
1. **Create the file**: `lib/otp_supervisor/core/analytics_server.ex`
2. **Implement the complete module** from the documentation:
   - GenServer with telemetry event attachment
   - State management for restart history and supervisor info
   - Event processing for all supervisor lifecycle events
   - Public API for querying analytics data
3. **Key functions to implement**:
   - `start_link/1`
   - `get_restart_history/1`
   - `get_all_supervisor_stats/0`
   - `get_failure_rate/2`
   - `handle_telemetry_event/4`

### Step 2: Add to Application Supervision Tree
1. **Update**: `lib/otp_supervisor/application.ex`
2. **Add AnalyticsServer** to the children list:
   ```elixir
   children = [
     OtpSupervisorWeb.Telemetry,
     # ... other children ...
     OTPSupervisor.Core.AnalyticsServer,  # Add this
     # ... rest of children ...
   ]
   ```

### Step 3: Update Control Module
1. **Update**: `lib/otp_supervisor/core/control.ex`
2. **Add new analytics functions**:
   - `get_restart_history/1` (replacement for old flawed version)
   - `get_supervisor_analytics/0`
   - `get_failure_rate/2`
3. **Ensure these use the AnalyticsServer**, not external monitoring

### Step 4: Create Comprehensive Tests
1. **Create**: `test/otp_supervisor/core/analytics_server_test.exs`
2. **Test the telemetry integration**:
   - Server startup and telemetry attachment
   - Restart event capture via real supervisor restarts
   - Analytics data retrieval
   - Failure rate calculations
3. **Update**: `test/otp_supervisor/core/control_test.exs`
4. **Add test section**: "telemetry-based analytics" 
5. **Test the Control module analytics functions**

### Step 5: Integration Testing
1. **Test with real supervisor operations**:
   - Start/stop children
   - Kill processes and verify restart tracking
   - Test with different supervisor strategies
2. **Verify telemetry events are captured**:
   - Use actual `Process.exit/2` to kill children
   - Verify restart events appear in analytics
   - Test with dynamic supervisor scenarios

## Expected Implementation Files

### New Files to Create
- `lib/otp_supervisor/core/analytics_server.ex` - Main telemetry-based analytics
- `test/otp_supervisor/core/analytics_server_test.exs` - Comprehensive tests

### Files to Modify  
- `lib/otp_supervisor/application.ex` - Add AnalyticsServer to supervision tree
- `lib/otp_supervisor/core/control.ex` - Add analytics API functions
- `test/otp_supervisor/core/control_test.exs` - Add analytics tests

## Key Implementation Requirements

### AnalyticsServer Must
- ✅ Use `:telemetry.attach_many/4` to hook into supervisor events
- ✅ Handle all supervisor lifecycle events (init, start, terminate, restart)
- ✅ Store restart history keyed by supervisor PID
- ✅ Provide failure rate calculations over time windows
- ✅ Handle supervisor crashes gracefully
- ✅ Maintain bounded history (e.g., last 1000 events per supervisor)

### Tests Must
- ✅ Test real supervisor restart scenarios
- ✅ Verify telemetry events are captured correctly
- ✅ Test failure rate calculations
- ✅ Test with multiple supervisors simultaneously
- ✅ Use actual OTP operations, not mocked events

### Control Module Must
- ✅ Provide clean API wrapping AnalyticsServer
- ✅ Handle PID conversion using existing `to_pid/1`
- ✅ Return proper error tuples for invalid supervisors
- ✅ Be consistent with existing Control module patterns

## Success Criteria
- [ ] AnalyticsServer starts successfully in application tree
- [ ] Telemetry events are captured when children restart
- [ ] `Control.get_restart_history/1` returns real restart data
- [ ] `Control.get_failure_rate/2` calculates rates correctly
- [ ] All tests pass including new analytics tests
- [ ] No external process monitoring or race conditions
- [ ] Works with both static and dynamic supervisors
- [ ] Performance is good (no blocking operations in telemetry handlers)

## Testing Approach

### Manual Testing
```elixir
# In IEx after starting the application:
iex> children = Supervisor.which_children(:demo_one_for_one)
iex> {_, pid, _, _} = hd(children)
iex> Process.exit(pid, :kill)
iex> :timer.sleep(100)  # Allow restart to complete
iex> OTPSupervisor.Core.Control.get_restart_history(:demo_one_for_one)
# Should show restart event
```

### Automated Testing
- Create supervisors in tests
- Kill children and verify events are captured
- Test telemetry integration without mocking
- Verify analytics calculations are accurate

## Notes
- **Use real telemetry** - Don't mock or simulate telemetry events
- **Handle async nature** - Telemetry events are asynchronous
- **Bounded storage** - Don't let restart history grow unbounded
- **Error handling** - Handle supervisor crashes and missing PIDs gracefully
- **Performance** - Keep telemetry handlers fast and non-blocking

This implementation provides **real OTP monitoring** using the standard telemetry approach used in production Elixir applications.