# 03 - Implement Sandbox Manager

## Task
Implement a proper SandboxManager for real OTP supervisor lifecycle management. This replaces the flawed SupervisorController with genuine OTP operations for creating, destroying, and managing sandbox supervision trees.

## Required Reading
**You must read these files before starting:**

1. `docs/SANDBOX_MANAGER_IMPLEMENTATION.md` - Complete implementation guide
2. `docs/REAL_OTP_DESIGN2.md` - Context on why real lifecycle management is correct
3. `lib/otp_supervisor/application.ex` - Where to add SandboxManager
4. `lib/otp_supervisor/core/control.ex` - Where to add sandbox management functions
5. `lib/otp_supervisor/sandbox/supervisors/demo_supervisor.ex` - Pattern for supervisor implementation
6. `test/support/test_demo_supervisor.ex` - Pattern for test supervisors

## Prerequisites
- ✅ **Phase 01 completed** - All flawed features removed
- ✅ **Phase 02 completed** - Telemetry analytics implemented and working
- ✅ `mix test` passes completely
- ✅ No references to SupervisorController remain in codebase

## Implementation Steps

### Step 1: Create SandboxManager
1. **Create the file**: `lib/otp_supervisor/core/sandbox_manager.ex`
2. **Implement the complete module** from the documentation:
   - GenServer with ETS table for fast sandbox lookup
   - Sandbox lifecycle management (create, destroy, restart)
   - Process monitoring for graceful cleanup
   - Unique naming for sandbox isolation
3. **Key functions to implement**:
   - `start_link/1`
   - `create_sandbox/3`
   - `destroy_sandbox/1`
   - `restart_sandbox/1`
   - `get_sandbox_info/1`
   - `list_sandboxes/0`

### Step 2: Enhance TestDemoSupervisor
1. **Update**: `test/support/test_demo_supervisor.ex`
2. **Add unique naming support**:
   - Accept `unique_id` in options
   - Generate unique child names using the ID
   - Support dynamic strategy configuration
3. **Ensure compatibility** with SandboxManager requirements

### Step 3: Add to Application Supervision Tree
1. **Update**: `lib/otp_supervisor/application.ex`
2. **Add SandboxManager** to the children list:
   ```elixir
   children = [
     OtpSupervisorWeb.Telemetry,
     # ... other children ...
     OTPSupervisor.Core.AnalyticsServer,
     OTPSupervisor.Core.SandboxManager,  # Add this
     # ... rest of children ...
   ]
   ```

### Step 4: Update Control Module
1. **Update**: `lib/otp_supervisor/core/control.ex`
2. **Add new sandbox management functions**:
   - `create_sandbox/2` (supervisor_module, opts)
   - `destroy_sandbox/1`
   - `restart_sandbox/1`
   - `list_sandboxes/0`
   - `get_sandbox_info/1`
3. **Remove any remaining references** to pause/resume functionality

### Step 5: Create Comprehensive Tests
1. **Create**: `test/otp_supervisor/core/sandbox_manager_test.exs`
2. **Test sandbox lifecycle operations**:
   - Creating sandboxes with different configurations
   - Destroying sandboxes and verifying cleanup
   - Restarting sandboxes with preserved configuration
   - Handling supervisor crashes gracefully
   - Multiple concurrent sandboxes
3. **Update**: `test/otp_supervisor/core/control_test.exs`
4. **Add test section**: "sandbox management"
5. **Test the Control module sandbox functions**

### Step 6: Integration with LiveView (Optional)
1. **Update LiveView components** to use sandbox management instead of pause/resume:
   - Replace pause/resume buttons with create/destroy sandbox controls
   - Show list of active sandboxes
   - Allow restarting sandboxes
   - Display sandbox information and health

## Expected Implementation Files

### New Files to Create
- `lib/otp_supervisor/core/sandbox_manager.ex` - Main sandbox lifecycle manager
- `test/otp_supervisor/core/sandbox_manager_test.exs` - Comprehensive tests

### Files to Modify
- `lib/otp_supervisor/application.ex` - Add SandboxManager to supervision tree
- `lib/otp_supervisor/core/control.ex` - Add sandbox management API
- `test/support/test_demo_supervisor.ex` - Enhance with unique naming
- `test/otp_supervisor/core/control_test.exs` - Add sandbox management tests
- `lib/otp_supervisor_web/live/*` - Update UI to use sandbox management

## Key Implementation Requirements

### SandboxManager Must
- ✅ Use real OTP supervisor start/stop operations
- ✅ Provide unique naming for sandbox isolation
- ✅ Monitor sandbox supervisors for crash detection
- ✅ Store sandbox metadata in ETS for fast lookup
- ✅ Handle graceful shutdown with proper cleanup
- ✅ Support multiple concurrent sandboxes
- ✅ Preserve sandbox configuration for restart operations

### TestDemoSupervisor Must
- ✅ Accept `unique_id` parameter for child naming
- ✅ Generate unique child names to avoid conflicts
- ✅ Support different supervisor strategies
- ✅ Work with SandboxManager's lifecycle operations
- ✅ Start/stop cleanly without affecting other sandboxes

### Tests Must
- ✅ Test real supervisor lifecycle operations
- ✅ Verify sandbox isolation (no name conflicts)
- ✅ Test supervisor crash handling
- ✅ Test concurrent sandbox operations
- ✅ Verify proper cleanup on destruction
- ✅ Test restart with configuration preservation

### Control Module Must
- ✅ Provide clean API wrapping SandboxManager
- ✅ Handle errors gracefully with proper error tuples
- ✅ Generate unique sandbox IDs automatically
- ✅ Be consistent with existing Control module patterns
- ✅ Support querying sandbox status and information

## Success Criteria
- [ ] SandboxManager starts successfully in application tree
- [ ] Can create multiple sandboxes without conflicts
- [ ] Sandbox destruction properly cleans up all processes
- [ ] Sandbox restart preserves original configuration
- [ ] Supervisor crashes are detected and cleaned up
- [ ] All tests pass including new sandbox management tests
- [ ] No simulation or fake operations - all real OTP
- [ ] LiveView integration works (if implemented)

## Testing Approach

### Manual Testing
```elixir
# In IEx after starting the application:
iex> alias OTPSupervisor.Core.Control
iex> alias OTPSupervisor.Sandbox.TestDemoSupervisor

# Create a sandbox
iex> {:ok, info} = Control.create_sandbox(TestDemoSupervisor, [strategy: :one_for_one])
iex> Process.alive?(info.supervisor_pid)
true

# Verify children are running
iex> Supervisor.which_children(info.supervisor_pid)
# Should show 3 children

# Destroy sandbox
iex> :ok = Control.destroy_sandbox(info.id)
iex> Process.alive?(info.supervisor_pid)
false
```

### Automated Testing
- Create/destroy sandboxes in rapid succession
- Test multiple concurrent sandboxes
- Kill sandbox supervisors and verify cleanup
- Test restart with different strategies
- Verify unique naming prevents conflicts

## Architecture Benefits

### Real OTP Patterns
- ✅ **Proper lifecycle management** - Real supervisor start/stop
- ✅ **Process supervision** - Monitor sandbox supervisors
- ✅ **Fault tolerance** - Handle crashes gracefully
- ✅ **Resource management** - Clean process cleanup

### Educational Value
- ✅ **Subsystem management** - Real-world pattern for managing parts of applications
- ✅ **Dynamic supervision** - Shows how to create/destroy supervision trees
- ✅ **Process monitoring** - Demonstrates proper process monitoring patterns
- ✅ **Unique naming** - Shows how to handle process naming in dynamic systems

## Notes
- **Use real supervisor operations** - No simulation or fake state
- **Handle naming conflicts** - Each sandbox must have unique child names
- **Monitor for crashes** - Detect when sandbox supervisors die
- **Clean shutdown** - Always clean up ETS entries and state
- **Test thoroughly** - This is complex due to process lifecycle management
- **Document patterns** - This shows real-world OTP subsystem management

This implementation provides **real OTP supervisor lifecycle management** that demonstrates genuine patterns used in production Elixir applications for managing subsystems and dynamic supervision trees.