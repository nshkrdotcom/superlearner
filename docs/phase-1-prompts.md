# Phase 1: Implementation Prompts

## Overview

This document contains self-contained prompts for implementing Phase 1 of the OTP Supervisor Educational Tool. Each prompt is designed to be executed independently without requiring context from previous prompts.

---

## Prompt 1: Phoenix Project Setup

**Required Reading:**
- `/docs/phase-1-implementation-guide.md` (Step 1: Project Setup)
- `/docs/staged-implementation-plan.md` (Phase 1 section)

**Prompt:**
```
Create a new Phoenix LiveView project called "otp_supervisor" without Ecto (no database). 
Set up the basic directory structure for the project with these additional directories:
- lib/otp_supervisor/core
- lib/otp_supervisor/sandbox
- lib/otp_supervisor/sandbox/workers  
- lib/otp_supervisor/sandbox/supervisors

Ensure the project runs successfully on http://localhost:4000.

The project should use Phoenix 1.7+ with LiveView enabled.
```

---

## Prompt 2: Core Supervisor Control Module

**Required Reading:**
- `/docs/phase-1-implementation-guide.md` (Step 2: Core Supervisor Control Module)
- `/docs/architecture.md` (Core Modules section - Supervisor Control API)

**Prompt:**
```
Create a module at lib/otp_supervisor/core/control.ex that provides an API for controlling 
and inspecting Elixir supervisors. The module should include these functions:

1. list_supervisors/0 - Lists all registered supervisors with their info
2. get_supervision_tree/1 - Gets children of a supervisor (by name or pid)
3. kill_process/1 - Kills a process by PID or PID string
4. get_process_info/1 - Gets detailed info about a process

The module should:
- Identify which registered processes are supervisors
- Format supervisor and process information into maps
- Handle both supervisor names (atoms) and PIDs
- Include helper functions for checking if a process is a supervisor
- Return process memory, message queue length, and status information

Make the functions defensive with proper error handling.
```

---

## Prompt 3: Sandbox Worker Processes

**Required Reading:**
- `/docs/phase-1-implementation-guide.md` (Step 3.1: Create Simple Workers)
- `/docs/architecture.md` (Sandboxed Elixir Application section)

**Prompt:**
```
Create two GenServer worker modules for demonstration purposes:

1. lib/otp_supervisor/sandbox/workers/counter.ex
   - A counter that maintains a numeric value
   - Functions: start_link/1, increment/1, get_value/1, crash/1
   - The crash/1 function should intentionally raise an error
   - Should accept a :name option and :initial_value option

2. lib/otp_supervisor/sandbox/workers/printer.ex
   - A printer that logs messages with Logger
   - Functions: start_link/1, print/2
   - Should track how many messages it has printed
   - Should accept :name and :id options

Both GenServers should properly implement the GenServer behavior with init/1 
and appropriate handle_call/handle_cast callbacks.
```

---

## Prompt 4: Demo Supervisor

**Required Reading:**
- `/docs/phase-1-implementation-guide.md` (Step 3.2: Create Example Supervisors)
- Elixir documentation on Supervisor behavior

**Prompt:**
```
Create a supervisor module at lib/otp_supervisor/sandbox/supervisors/demo_supervisor.ex that:

1. Uses the Supervisor behavior
2. Accepts a :strategy option (defaulting to :one_for_one) 
3. Accepts a :name option for registration
4. Starts three children:
   - Two Counter workers (named :counter_1 and :counter_2)
   - One Printer worker (named :printer_1)
5. Properly implements the init/1 callback

The supervisor should demonstrate basic OTP supervisor patterns and be suitable
for educational demonstrations of supervisor restart strategies.
```

---

## Prompt 5: Application Configuration

**Required Reading:**
- `/docs/phase-1-implementation-guide.md` (Step 3.3: Start Supervisors on Application Start)
- Phoenix application structure documentation

**Prompt:**
```
Modify lib/otp_supervisor/application.ex to start a demo supervisor when the 
application starts. The application should:

1. Start the standard Phoenix components (Telemetry, PubSub, Endpoint)
2. Additionally start a DemoSupervisor with:
   - Name: :demo_one_for_one
   - Strategy: :one_for_one
3. Use proper OTP application structure

Ensure the demo supervisor is part of the main application supervision tree
and starts automatically when the Phoenix application starts.
```

---

## Prompt 6: LiveView UI Component

**Required Reading:**
- `/docs/phase-1-implementation-guide.md` (Step 4.1: Create the Main LiveView)
- Phoenix LiveView documentation basics

**Prompt:**
```
Create a Phoenix LiveView module at lib/otp_supervisor_web/live/supervisor_live.ex that:

1. Shows a list of all supervisors in the system
2. Allows selecting a supervisor to see its children
3. Shows process details when clicking on a PID
4. Has a "Kill" button for each living process
5. Auto-refreshes every second using :timer.send_interval/3
6. Handles these events:
   - "select_supervisor" - selects a supervisor to inspect
   - "kill_process" - kills a process by PID
   - "select_process" - shows process details

The LiveView should use the OTPSupervisor.Core.Control module for all
supervisor operations. Include proper error handling and user feedback.
```

---

## Prompt 7: LiveView Template

**Required Reading:**
- `/docs/phase-1-implementation-guide.md` (Step 4.2: Create the LiveView Template)
- Phoenix HEEx template syntax

**Prompt:**
```
Create a HEEx template at lib/otp_supervisor_web/live/supervisor_live.html.heex with:

1. A three-column layout (using Tailwind CSS classes):
   - Left: List of supervisors
   - Middle: Children of selected supervisor  
   - Right: Process details

2. For each supervisor, show:
   - Name, PID, and child count
   - Highlight the selected supervisor

3. For each child process, show:
   - ID, PID, type, and alive/dead status
   - Memory usage (if alive)
   - Kill button (if alive)

4. Educational information panel explaining how supervisors work

5. Proper Tailwind styling for a clean, professional appearance

Include helper functions for formatting bytes, process keys, and values.
```

---

## Prompt 8: Router and Navigation

**Required Reading:**
- `/docs/phase-1-implementation-guide.md` (Steps 5-6: Router Configuration)
- Phoenix Router documentation

**Prompt:**
```
Configure the Phoenix router and navigation:

1. In lib/otp_supervisor_web/router.ex:
   - Add a live route "/supervisors" pointing to SupervisorLive
   - Keep the standard Phoenix pipelines

2. In lib/otp_supervisor_web/controllers/page_controller.ex:
   - Modify the home action to redirect to "/supervisors"

3. Ensure the supervisor name is preserved in the URL as a query parameter
   when selecting a supervisor (e.g., /supervisors?supervisor=demo_one_for_one)

The navigation should work smoothly with browser back/forward buttons.
```

---

## Prompt 9: Testing Infrastructure

**Required Reading:**
- `/docs/phase-1-implementation-guide.md` (Step 7: Testing)
- ExUnit testing documentation

**Prompt:**
```
Create comprehensive tests for the Control module:

1. Create test/otp_supervisor/core/control_test.exs with tests for:
   - Listing supervisors (including the test supervisor)
   - Getting supervision trees
   - Killing processes and verifying termination
   - Handling errors gracefully

2. The test should:
   - Set up a test supervisor in the setup block
   - Clean up properly in on_exit callbacks
   - Test both success and error cases
   - Verify supervisor restart behavior

Include at least 4 test cases covering the main functionality.
```

---

## Prompt 10: Documentation and Final Integration

**Required Reading:**
- `/docs/phase-1-implementation-guide.md` (Step 8: Running and Testing)
- `/docs/features-overview.md` (Core Features section)

**Prompt:**
```
Complete the Phase 1 implementation:

1. Add moduledoc documentation to all modules explaining their purpose
2. Add @doc documentation to all public functions
3. Create helpful error messages for common issues
4. Add the format_bytes/1, format_key/1, and format_value/1 helper functions
   to the LiveView module for better data display

5. Verify the complete system works:
   - Supervisors are listed
   - Processes can be killed and restart
   - Process details are shown
   - Auto-refresh works properly

6. Add IEx helper examples in comments showing how to interact with the
   sandbox workers for educational purposes.

The final system should be a working supervisor control panel suitable for
learning OTP supervisor concepts.
```

---

## Execution Notes

1. **Order**: Execute prompts in sequence (1-10)
2. **Testing**: After each prompt, verify the component works
3. **Dependencies**: Each prompt assumes only Phoenix 1.7+ with LiveView
4. **Context**: Each prompt references specific documentation sections for context
5. **Validation**: The final system should match the Phase 1 deliverables checklist

## Success Criteria

After completing all prompts:
- [ ] Phoenix app starts without errors
- [ ] Supervisors are visible at http://localhost:4000/supervisors  
- [ ] Can select supervisors and see children
- [ ] Can kill processes and see them restart
- [ ] Process details are displayed when clicking PIDs
- [ ] Auto-refresh updates the UI every second
- [ ] Tests pass with `mix test`
- [ ] Educational panel explains supervisor behavior

This completes the Phase 1 implementation in manageable, self-contained chunks.