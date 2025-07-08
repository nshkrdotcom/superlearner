# Test Isolation Fix Prompts

This document contains a series of prompts to systematically fix all test isolation violations found in the compliance report. Each prompt is designed to be executed in a fresh context and includes all necessary references.

---

## Prompt 1: Fix Control Test - Global Process Names (High Priority)

**Required Reading:**
- `/home/home/p/g/n/superlearner/test_isolation_compliance_report.md` (Focus on control_test.exs violations)
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Core principles)
- `/home/home/p/g/n/superlearner/otp_supervisor/test/otp_supervisor/core/control_test.exs` (Current implementation)
- `/home/home/p/g/n/superlearner/otp_supervisor/test/support/supervisor_test_helper.ex` (Helper functions)

**Context:**
The compliance report identified 21 VIOLATION tests in control_test.exs that use hardcoded global process names like `:test_control_supervisor`, `:test_counter_restart`, `:detection_test_sup`, etc. These violations prevent safe parallel test execution.

**Task:**
Fix all hardcoded global process names in control_test.exs by:

1. **Replace hardcoded names with unique identifiers** for these specific violations:
   - Line 70: `:test_control_supervisor` and `:test_worker_1`
   - Line 189: `:detection_test_sup`
   - Line 197: `:not_a_supervisor`
   - Line 237: `:test_counter_restart`, `:test_printer_restart`, `:test_restart_supervisor`
   - Line 337: `:crash_test_sup`
   - Line 357: `:slow_sup`
   - Line 384: `:bad_child_sup`
   - Line 415: `:test_dynamic_sup`
   - Line 432: `:test_task_sup`
   - Line 461: `:unusual_process`
   - Line 699: `:integration_test_supervisor`

2. **Use unique naming pattern**: `:"#{base_name}_#{:erlang.unique_integer([:positive])}"`

3. **Maintain test functionality**: Ensure all tests continue to pass and test the same behavior

4. **Follow isolation guide**: Use proper setup/teardown patterns as documented

**Success Criteria:**
- All hardcoded global names replaced with unique identifiers
- All tests in control_test.exs pass
- No global name conflicts possible between tests
- Test behavior and coverage unchanged

---

## Prompt 2: Fix Control Test - Process.sleep Synchronization (High Priority)

**Required Reading:**
- `/home/home/p/g/n/superlearner/test_isolation_compliance_report.md` (Focus on WARNING status tests)
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Synchronization patterns)
- `/home/home/p/g/n/superlearner/otp_supervisor/test/otp_supervisor/core/control_test.exs` (Current implementation)
- `/home/home/p/g/n/superlearner/otp_supervisor/test/support/supervisor_test_helper.ex` (Helper functions)

**Context:**
The compliance report identified 7 WARNING tests in control_test.exs that use `Process.sleep()` for synchronization instead of proper process monitoring. This creates race conditions and flaky tests.

**Task:**
Replace all `Process.sleep()` usage with proper synchronization for these specific tests:

1. **Replace sleep with monitoring** in these tests:
   - Line 100: `test "terminates process by pid"`
   - Line 117: `test "terminates process by pid string"`
   - Line 165: `test "returns error for dead process"`
   - Line 585: `test "handles already dead processes"`
   - Line 594: `test "handles protected processes gracefully"`
   - Line 626: `test "handles PID strings from different formats"`
   - Line 643: `test "handles concurrent kill operations"`

2. **Use proper patterns** (NO sleep calls allowed):
   - Process death: `Process.monitor/1` and `assert_receive {:DOWN, ...}`
   - Supervisor restart: `SupervisorTestHelper.wait_for_restart/1` (pass supervisor PID, not name)
   - For crash tests: Monitor process death FIRST, then wait for restart
   - If helpers fail: Debug the helper usage, don't add sleep as fallback

3. **Maintain test reliability**: Ensure tests are deterministic and don't depend on timing

**Success Criteria:**
- ZERO `Process.sleep()` calls anywhere in control_test.exs
- All tests pass consistently without race conditions  
- Tests execute faster due to proper synchronization
- Test behavior and coverage unchanged
- If any test times out or fails: debug the synchronization logic, don't add sleep

---

## Prompt 3: Convert Control Test to Use SupervisorTestHelper (High Priority)

**Required Reading:**
- `/home/home/p/g/n/superlearner/test_isolation_compliance_report.md` (Focus on control_test.exs recommendations)
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Helper usage patterns)
- `/home/home/p/g/n/superlearner/otp_supervisor/test/otp_supervisor/core/control_test.exs` (Current implementation)
- `/home/home/p/g/n/superlearner/otp_supervisor/test/support/supervisor_test_helper.ex` (Available helpers)
- `/home/home/p/g/n/superlearner/otp_supervisor/test/otp_supervisor_web/live/supervisor_live_test.exs` (Reference implementation)

**Context:**
The compliance report shows control_test.exs has poor isolation (35% compliant) because it doesn't use the available SupervisorTestHelper functions. The LiveView tests (100% compliant) demonstrate the correct patterns.

**Task:**
Refactor control_test.exs to use SupervisorTestHelper for proper isolation:

1. **Import the helper**: Add `import SupervisorTestHelper` at the top

2. **Categorize and convert tests**:
   - **Destructive tests** (kill processes, modify state): Use `setup_isolated_supervisor/1`
   - **Read-only tests** (only inspect): Use `get_demo_supervisor/0` where appropriate
   - **Error tests**: Use `setup_crash_test_supervisor/1` for crash scenarios

3. **Update specific test groups**:
   - "supervisor restart behavior" describe block: Use isolated supervisors
   - "integration with real supervision scenarios": Use isolated supervisors
   - "malformed supervision tree handling": Mix of isolated and crash test supervisors

4. **Follow LiveView test patterns**: Use the same setup patterns as supervisor_live_test.exs

**Success Criteria:**
- All appropriate tests use SupervisorTestHelper functions
- Test isolation dramatically improved
- Tests follow the same patterns as compliant LiveView tests
- All tests pass with proper isolation

---

## Prompt 4: Fix Counter Test - Global Process Names (Medium Priority)

**Required Reading:**
- `/home/home/p/g/n/superlearner/test_isolation_compliance_report.md` (Focus on counter_test.exs violations)
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Core principles)
- `/home/home/p/g/n/superlearner/otp_supervisor/test/otp_supervisor/sandbox/workers/counter_test.exs` (Current implementation)

**Context:**
The compliance report identified 10 VIOLATION tests in counter_test.exs that use hardcoded global process names. The file has 66% compliance and needs these specific fixes.

**Task:**
Fix all hardcoded global process names in counter_test.exs:

1. **Replace hardcoded names** in these specific tests:
   - Line 30: `:test_counter` → unique name
   - Line 47: `:named_counter` → unique name
   - Line 170: `:independent_counter_1`, `:independent_counter_2`, `:independent_counter_3` → unique names
   - Line 192: `:crashable_counter` → unique name
   - Line 219: `:supervised_counter` → unique name
   - Line 374: `:counter_a`, `:counter_b`, `:counter_c` → unique names
   - Line 411: `:named_test_counter` → unique name
   - Line 432: `:cleanup_test` → unique name
   - Line 446: `:demo_counter_1`, `:demo_counter_2` → unique names
   - Line 508: `:educational_counter` → unique name

2. **Use unique naming pattern**: `:"#{base_name}_#{:erlang.unique_integer([:positive])}"`

3. **Maintain educational value**: Keep test names descriptive for learning purposes

4. **Update documentation**: Ensure test comments reflect the isolation patterns

**Success Criteria:**
- All hardcoded global names replaced
- Counter test compliance improves to 90%+
- All tests pass with proper isolation
- Educational value preserved

---

## Prompt 5: Fix Counter Test - Process.sleep Synchronization (Medium Priority)

**Required Reading:**
- `/home/home/p/g/n/superlearner/test_isolation_compliance_report.md` (Focus on counter_test.exs warnings)
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Synchronization patterns)
- `/home/home/p/g/n/superlearner/otp_supervisor/test/otp_supervisor/sandbox/workers/counter_test.exs` (Current implementation)

**Context:**
The compliance report identified tests in counter_test.exs that use `Process.sleep()` for synchronization, creating potential race conditions.

**Task:**
Replace `Process.sleep()` with proper synchronization in counter_test.exs:

1. **Fix these specific tests**:
   - Line 139: `test "state consistency under concurrent load"` - remove minimal sleep
   - Line 219: `test "state resets after supervisor restart"` - use process monitoring
   - Line 374: `test "works correctly under one_for_one supervision"` - use proper waiting
   - Line 446: `test "demonstrates restart strategy differences"` - use monitoring
   - Line 508: `test "shows how crashes trigger supervisor restarts"` - use monitoring

2. **Use proper patterns** (ZERO sleep calls allowed):
   - Process monitoring: `Process.monitor/1` and `assert_receive {:DOWN, ...}`
   - Supervisor restart: `SupervisorTestHelper.wait_for_restart/1`
   - Synchronous GenServer calls: Use `GenServer.call/2` to ensure processing complete
   - Never use `Process.sleep/1` for any synchronization

3. **Maintain educational value**: Update comments to explain proper synchronization

**Success Criteria:**
- ZERO `Process.sleep()` calls anywhere in counter_test.exs
- Tests are deterministic and reliable
- Educational comments explain proper OTP patterns
- All tests pass consistently
- Any test failure must be debugged and fixed without adding sleep

---

## Prompt 6: Fix Printer Test - Global Process Names (Medium Priority)

**Required Reading:**
- `/home/home/p/g/n/superlearner/test_isolation_compliance_report.md` (Focus on printer_test.exs violations)
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Core principles)
- `/home/home/p/g/n/superlearner/otp_supervisor/test/otp_supervisor/sandbox/workers/printer_test.exs` (Current implementation)

**Context:**
The compliance report identified 8 VIOLATION tests in printer_test.exs that use hardcoded global process names. The file has 70% compliance.

**Task:**
Fix all hardcoded global process names in printer_test.exs:

1. **Replace hardcoded names** in these specific tests:
   - Line 34: `:test_printer` → unique name
   - Line 53: `:named_printer` → unique name
   - Line 338: Helper function `create_test_printers` uses `:"test_printer_#{i}"` → make unique
   - Line 417: `:supervised_printer` → unique name
   - Line 486: `:printer_alpha`, `:printer_beta`, `:printer_gamma` → unique names
   - Line 519: `:named_test_printer` → unique name
   - Line 543: `:cleanup_test_printer` → unique name
   - Line 565: `:demo_printer_1`, `:demo_printer_2` → unique names

2. **Fix helper functions**: Update `create_test_printers` to generate truly unique names

3. **Use unique naming pattern**: `:"#{base_name}_#{:erlang.unique_integer([:positive])}"`

4. **Preserve logging integration**: Ensure unique IDs don't break logging tests

**Success Criteria:**
- All hardcoded global names replaced
- Helper functions generate unique names
- Printer test compliance improves to 90%+
- All tests pass with proper isolation

---

## Prompt 7: Fix Printer Test - Process.sleep Synchronization (Medium Priority)

**Required Reading:**
- `/home/home/p/g/n/superlearner/test_isolation_compliance_report.md` (Focus on printer_test.exs warnings)
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Synchronization patterns)
- `/home/home/p/g/n/superlearner/otp_supervisor/test/otp_supervisor/sandbox/workers/printer_test.exs` (Current implementation)

**Context:**
The compliance report identified 4 WARNING tests in printer_test.exs that use `Process.sleep()` for synchronization instead of proper GenServer patterns.

**Task:**
Replace `Process.sleep()` with proper synchronization in printer_test.exs:

1. **Fix these specific tests**:
   - Line 417: `test "state reset after crashes and restarts"` - use process monitoring
   - Line 486: `test "multiple printer instances under supervision"` - remove sleep
   - Line 519: `test "supports named vs unnamed process registration"` - remove sleep
   - Line 543: `test "proper cleanup on process termination"` - remove sleep
   - Line 565: `test "demonstrates restart behavior differences"` - use monitoring
   - Line 614: `test "demonstrates message passing patterns"` - remove sleep
   - Line 637: `test "shows state management across operations"` - remove sleep
   - Line 679: `test "demonstrates concurrent access safety"` - remove sleep

2. **Use GenServer patterns** (ZERO sleep calls allowed):
   - Synchronous `get_print_count/1` calls ensure message processing complete
   - Process monitoring: `Process.monitor/1` and `assert_receive {:DOWN, ...}`
   - Supervisor restart: `SupervisorTestHelper.wait_for_restart/1`
   - `capture_log/1` for testing logging behavior without timing dependencies

3. **Maintain educational value**: Show proper async/sync GenServer patterns

**Success Criteria:**
- ZERO `Process.sleep()` calls anywhere in printer_test.exs
- Tests demonstrate proper GenServer synchronization patterns
- Educational comments explain async vs sync patterns  
- All tests pass reliably and deterministically
- Any test failure must be debugged and fixed without adding sleep

---

## Prompt 8: Validate All Fixes and Run Full Test Suite (Final)

**Required Reading:**
- `/home/home/p/g/n/superlearner/test_isolation_compliance_report.md` (Original violations)
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Final validation)
- All modified test files

**Context:**
After implementing all previous fixes, validate that the test suite achieves full compliance with the test isolation guide and runs reliably in parallel.

**Task:**
Perform comprehensive validation of all test isolation fixes:

1. **Run full test suite**: Execute `mix test` and verify all tests pass

2. **Test parallel execution**: Run tests multiple times to ensure no race conditions

3. **Validate compliance improvements**:
   - control_test.exs: Should improve from 35% to 90%+ compliance
   - counter_test.exs: Should improve from 66% to 95%+ compliance  
   - printer_test.exs: Should improve from 70% to 95%+ compliance
   - supervisor_live_test.exs: Should remain at 100% compliance

4. **Check for remaining violations**:
   - No hardcoded global process names
   - No `Process.sleep()` for synchronization
   - All destructive tests use isolation helpers
   - All read-only tests use appropriate helpers

5. **Performance validation**: Tests should run faster due to proper synchronization

6. **Update compliance report**: Create new compliance analysis showing improvements

**Success Criteria:**
- All tests pass consistently
- Overall compliance >95%
- No remaining VIOLATION status tests
- Tests run reliably in parallel
- Performance improved due to better synchronization
- Updated compliance report shows dramatic improvement

---

## Execution Order

Execute these prompts in the following order for systematic resolution:

1. **Prompt 1** (Control - Global Names) - Highest impact violations
2. **Prompt 2** (Control - Sleep) - Critical race conditions
3. **Prompt 3** (Control - Helpers) - Architectural improvement
4. **Prompt 4** (Counter - Global Names) - Medium priority violations
5. **Prompt 5** (Counter - Sleep) - Medium priority warnings
6. **Prompt 6** (Printer - Global Names) - Medium priority violations
7. **Prompt 7** (Printer - Sleep) - Medium priority warnings
8. **Prompt 8** (Validation) - Final verification

Each prompt is self-contained and can be executed in a fresh context with the specified required reading materials.