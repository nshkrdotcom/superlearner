# Test Implementation Prompts for OTP Supervisor Educational Tool

## Overview

This document contains self-contained prompts for implementing comprehensive test coverage for the OTP Supervisor Educational Tool. Each prompt is designed to be executed independently with full context and specific success metrics.

**Target**: Achieve 90% test coverage with robust error handling and educational value.

---

## Prompt 1: Control Module Error Handling Tests

**Required Reading:**
- `/home/home/p/g/n/superlearner/otp_supervisor/lib/otp_supervisor/core/control.ex`
- `/home/home/p/g/n/superlearner/otp_supervisor/test/otp_supervisor/core/control_test.exs`
- `/home/home/p/g/n/superlearner/docs/comprehensive-test-plan.md` (Phase 1.1)

**Context:**
The Control module is the core API for supervisor operations but lacks comprehensive error handling tests. Current coverage is 74.47% with critical gaps in error scenarios.

**Prompt:**
```
Extend the existing Control module tests to achieve comprehensive error handling coverage.

Add these test cases to test/otp_supervisor/core/control_test.exs:

1. **Malformed Supervision Tree Handling**:
   - Test get_supervision_tree/1 with crashed supervisors
   - Test with supervisors that have malformed child specs
   - Test with supervisors in transition states

2. **Supervisor Detection Edge Cases**:
   - Test is_supervisor_pid?/1 with DynamicSupervisor
   - Test with processes missing dictionary entries
   - Test with processes having malformed :$initial_call
   - Test with Task.Supervisor processes

3. **Process Information Error Scenarios**:
   - Test get_process_info/1 with processes that die during inspection
   - Test with processes having extreme memory usage
   - Test with processes having corrupt state

4. **Kill Process Error Handling**:
   - Test kill_process/1 with already dead processes
   - Test with protected/system processes
   - Test with malformed PID strings in various formats
   - Test with PIDs from different nodes

Each test should:
- Use proper setup/teardown with on_exit callbacks
- Include realistic error scenarios students might encounter
- Verify proper error return values and messages
- Test both success and failure paths

The tests should be educational, showing how OTP handles edge cases.
```

**Success Metrics:**
- [ ] All existing tests continue to pass
- [ ] 12 new test cases added
- [ ] Control module coverage increases to 85%+
- [ ] All error paths return appropriate error tuples
- [ ] Tests run successfully with `mix test test/otp_supervisor/core/control_test.exs`

---

## Prompt 2: Counter Worker Comprehensive Tests

**Required Reading:**
- `/home/home/p/g/n/superlearner/otp_supervisor/lib/otp_supervisor/sandbox/workers/counter.ex`
- `/home/home/p/g/n/superlearner/docs/comprehensive-test-plan.md` (Phase 1.2)

**Context:**
The Counter worker is a demonstration GenServer but lacks dedicated tests. It needs comprehensive testing for educational scenarios including crashes, concurrency, and state management.

**Prompt:**
```
Create a complete test file for the Counter worker module.

Create test/otp_supervisor/sandbox/workers/counter_test.exs with:

1. **Basic Functionality Tests**:
   - Test start_link/1 with various options
   - Test increment/1 and get_value/1 operations
   - Test crash/1 function behavior
   - Test initial_value parameter handling

2. **Concurrent Operations Tests**:
   - Test multiple processes incrementing simultaneously
   - Test rapid increment/get_value cycles
   - Test state consistency under concurrent load
   - Test process restart during concurrent operations

3. **Error Recovery Tests**:
   - Test behavior after intentional crashes
   - Test state reset after supervisor restart
   - Test handling of invalid messages
   - Test timeout scenarios

4. **State Validation Tests**:
   - Test with negative initial values
   - Test with extremely large values
   - Test with non-integer initial values
   - Test memory usage under high counts

5. **Integration Tests**:
   - Test interaction with supervisor restart policies
   - Test named vs unnamed process registration
   - Test cleanup on process termination

Include educational documentation in tests showing:
- How GenServer state is managed
- How crashes trigger supervisor restarts
- How concurrent access is handled
- How to verify process behavior

Use proper ExUnit patterns with describe blocks and descriptive test names.
```

**Success Metrics:**
- [ ] New test file created with 10+ test cases
- [ ] All Counter worker functionality tested
- [ ] Concurrent operation handling verified
- [ ] Error recovery scenarios covered
- [ ] Tests pass with `mix test test/otp_supervisor/sandbox/workers/counter_test.exs`
- [ ] Educational value demonstrated through test scenarios

---

## Prompt 3: Printer Worker Comprehensive Tests

**Required Reading:**
- `/home/home/p/g/n/superlearner/otp_supervisor/lib/otp_supervisor/sandbox/workers/printer.ex`
- `/home/home/p/g/n/superlearner/docs/comprehensive-test-plan.md` (Phase 1.2)

**Context:**
The Printer worker demonstrates logging and message handling but lacks dedicated tests. It needs comprehensive testing for message handling, logging integration, and state management.

**Prompt:**
```
Create a complete test file for the Printer worker module.

Create test/otp_supervisor/sandbox/workers/printer_test.exs with:

1. **Basic Functionality Tests**:
   - Test start_link/1 with name and id options
   - Test print/2 message handling
   - Test get_print_count/1 state tracking
   - Test process registration and cleanup

2. **Message Handling Tests**:
   - Test printing of various message types (strings, atoms, numbers)
   - Test very long messages (>1000 characters)
   - Test messages with special characters and unicode
   - Test binary message handling
   - Test nil and empty message handling

3. **Logging Integration Tests**:
   - Test Logger integration and log message format
   - Test log levels and filtering
   - Test concurrent logging operations
   - Test logging backend failures (if applicable)

4. **High-Volume Tests**:
   - Test rapid message printing (stress test)
   - Test print count accuracy under load
   - Test memory usage with high message volumes
   - Test performance with concurrent printers

5. **State Management Tests**:
   - Test print_count persistence across messages
   - Test state consistency during concurrent operations
   - Test state reset after crashes and restarts
   - Test handling of large print counts

6. **Integration Tests**:
   - Test interaction with supervisor restart policies
   - Test multiple printer instances
   - Test cleanup on process termination

Include helper functions for:
- Capturing log output for verification
- Setting up multiple printer instances
- Generating test messages of various types

Use proper ExUnit patterns with setup blocks and descriptive test names.
```

**Success Metrics:**
- [ ] New test file created with 8+ test cases
- [ ] All Printer worker functionality tested
- [ ] Message handling edge cases covered
- [ ] Logging integration verified
- [ ] High-volume scenarios tested
- [ ] Tests pass with `mix test test/otp_supervisor/sandbox/workers/printer_test.exs`

---

## Prompt 4: SupervisorLive Enhanced Tests

**Required Reading:**
- `/home/home/p/g/n/superlearner/otp_supervisor/lib/otp_supervisor_web/live/supervisor_live.ex`
- `/home/home/p/g/n/superlearner/otp_supervisor/test/otp_supervisor_web/live/supervisor_live_test.exs`
- `/home/home/p/g/n/superlearner/docs/comprehensive-test-plan.md` (Phase 1.3)
- `/home/home/p/g/n/superlearner/docs/code-standards/otp-testing-standards.md`

**Context:**
The SupervisorLive module has basic tests but lacks comprehensive coverage of real-time functionality, error handling, and user interactions. Current coverage is 42.86%.

**Prompt:**
```
Extend the existing SupervisorLive tests to achieve comprehensive coverage.

Add these test cases to test/otp_supervisor_web/live/supervisor_live_test.exs:

1. **Real-Time Functionality Tests**:
   - Test auto-refresh timer functionality
   - Test supervisor list updates during refresh
   - Test process state changes reflected in real-time
   - Test handling of processes that die during display

2. **Process Selection and Killing Tests**:
   - Test process selection via PID clicking
   - Test process killing via kill button
   - Test error handling when killing fails
   - Test UI updates after process kills
   - Test PID format handling ("#PID<0.123.0>" vs "<0.123.0>")

3. **Error Handling Tests**:
   - Test invalid supervisor selection
   - Test malformed PID selection
   - Test supervisor that crashes during inspection
   - Test network disconnection scenarios
   - Test error message display and flash handling

4. **URL Parameter Handling Tests**:
   - Test loading with ?supervisor=demo_one_for_one
   - Test invalid supervisor parameter handling
   - Test navigation state preservation
   - Test browser back/forward button behavior

5. **Process Information Display Tests**:
   - Test process details formatting
   - Test handling of processes with missing info
   - Test process info refresh for selected processes
   - Test info display when process dies

6. **WebSocket Integration Tests**:
   - Test connection establishment
   - Test message handling during page updates
   - Test reconnection after disconnection
   - Test concurrent user interactions

Include helper functions for:
- Simulating WebSocket connections
- Creating test supervisors and processes
- Triggering real-time updates
- Verifying UI state changes

Use Phoenix.LiveViewTest utilities for proper LiveView testing.
```

**Success Metrics:**
- [ ] 10+ new test cases added to existing file
- [ ] Real-time functionality thoroughly tested
- [ ] Error handling scenarios covered
- [ ] URL parameter handling verified
- [ ] SupervisorLive coverage increases to 75%+
- [ ] Tests pass with `mix test test/otp_supervisor_web/live/supervisor_live_test.exs`

---

## Prompt 5: Demo Supervisor Strategy Tests

**Required Reading:**
- `/home/home/p/g/n/superlearner/otp_supervisor/lib/otp_supervisor/sandbox/supervisors/demo_supervisor.ex`
- `/home/home/p/g/n/superlearner/docs/comprehensive-test-plan.md` (Phase 2.1)
- `/home/home/p/g/n/superlearner/docs/code-standards/otp-testing-standards.md`
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Critical for supervisor tests)
- Elixir Supervisor documentation for restart strategies

**Context:**
The Demo Supervisor demonstrates supervisor patterns but lacks comprehensive testing of different restart strategies and supervisor management scenarios.

**Prompt:**
```
Create a complete test file for the Demo Supervisor module.

Create test/otp_supervisor/sandbox/supervisors/demo_supervisor_test.exs with:

1. **Basic Supervisor Tests**:
   - Test start_link/1 with various strategies
   - Test supervisor registration and naming
   - Test child specification creation
   - Test supervisor info and state

2. **Restart Strategy Tests**:
   - Test :one_for_one strategy (kill one child, others continue)
   - Test :one_for_all strategy (kill one child, all restart)
   - Test :rest_for_one strategy (kill one child, subsequent restart)
   - Test restart intensity and period limits

3. **Child Management Tests**:
   - Test all expected children are started
   - Test child restart behavior after crashes
   - Test child state reset after restarts
   - Test child PID changes after restarts

4. **Supervisor Crash Recovery Tests**:
   - Test supervisor crash and restart
   - Test supervision tree recreation
   - Test child recovery after supervisor restart
   - Test state consistency after recovery

5. **Dynamic Child Management Tests**:
   - Test adding children at runtime (if applicable)
   - Test removing children at runtime (if applicable)
   - Test child specification updates
   - Test temporary vs permanent child handling

6. **Integration Tests**:
   - Test interaction with application supervision tree
   - Test supervision tree inspection via Control module
   - Test LiveView interaction with supervisor
   - Test educational scenarios (killing processes)

Include helper functions for:
- Creating test supervisors with different strategies
- Verifying child restart behavior
- Simulating supervisor crashes
- Checking supervision tree state

Use proper ExUnit setup/teardown for supervisor lifecycle management.
```

**Success Metrics:**
- [ ] New test file created with 8+ test cases
- [ ] All restart strategies tested comprehensively
- [ ] Child management scenarios covered
- [ ] Supervisor crash recovery verified
- [ ] Tests pass with `mix test test/otp_supervisor/sandbox/supervisors/demo_supervisor_test.exs`
- [ ] Educational value demonstrated through restart scenarios

---

## Prompt 6: Full System Integration Tests

**Required Reading:**
- `/home/home/p/g/n/superlearner/otp_supervisor/lib/otp_supervisor/application.ex`
- `/home/home/p/g/n/superlearner/otp_supervisor/lib/otp_supervisor/core/control.ex`
- `/home/home/p/g/n/superlearner/otp_supervisor/lib/otp_supervisor_web/live/supervisor_live.ex`
- `/home/home/p/g/n/superlearner/docs/comprehensive-test-plan.md` (Phase 2.2)
- `/home/home/p/g/n/superlearner/docs/code-standards/otp-testing-standards.md`
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Critical for integration tests)

**Context:**
The system lacks integration tests that verify cross-module interactions and end-to-end workflows. These tests are crucial for ensuring the educational tool works as a complete system.

**Prompt:**
```
Create comprehensive integration tests for the full system.

Create test/integration/full_system_test.exs with:

1. **Application Startup Tests**:
   - Test application starts with expected supervision tree
   - Test demo supervisor is created and registered
   - Test all worker processes are started
   - Test Phoenix endpoint and LiveView availability

2. **Cross-Module Integration Tests**:
   - Test Control module can inspect demo supervisor
   - Test LiveView displays supervisors from Control module
   - Test killing processes via Control reflects in LiveView
   - Test supervisor restarts are detected by Control module

3. **End-to-End Workflow Tests**:
   - Test complete user workflow: load page → select supervisor → kill process → verify restart
   - Test multiple concurrent users accessing the system
   - Test system behavior under educational scenarios
   - Test error recovery across module boundaries

4. **Data Flow Tests**:
   - Test real-time data flow from processes to LiveView
   - Test process state changes propagate correctly
   - Test supervisor tree changes are reflected in UI
   - Test error states are communicated properly

5. **System Resilience Tests**:
   - Test system recovery after demo supervisor crash
   - Test handling of multiple process failures
   - Test system behavior under stress conditions
   - Test graceful degradation scenarios

6. **Educational Scenario Tests**:
   - Test typical student interactions work correctly
   - Test crash-and-restart demonstrations
   - Test supervision tree exploration
   - Test process inspection workflows

Include helper functions for:
- Starting/stopping full application
- Simulating user interactions
- Verifying system state consistency
- Testing concurrent access patterns

Use proper test isolation and cleanup to avoid test interference.
```

**Success Metrics:**
- [ ] New test file created with 6+ integration test cases
- [ ] Cross-module interactions verified
- [ ] End-to-end workflows tested
- [ ] System resilience demonstrated
- [ ] Educational scenarios validated
- [ ] Tests pass with `mix test test/integration/full_system_test.exs`

---

## Prompt 7: Performance and Load Tests

**Required Reading:**
- `/home/home/p/g/n/superlearner/otp_supervisor/lib/otp_supervisor/core/control.ex`
- `/home/home/p/g/n/superlearner/otp_supervisor/lib/otp_supervisor_web/live/supervisor_live.ex`
- `/home/home/p/g/n/superlearner/docs/comprehensive-test-plan.md` (Phase 3.1)
- `/home/home/p/g/n/superlearner/docs/code-standards/otp-testing-standards.md`
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Critical for load tests)

**Context:**
The system needs performance testing to ensure it remains responsive under educational load scenarios with multiple students and high-frequency operations.

**Prompt:**
```
Create performance and load tests for the system.

Create test/integration/performance_test.exs with:

1. **High-Volume Operation Tests**:
   - Test rapid process creation/destruction (100+ processes)
   - Test frequent supervisor tree inspection
   - Test high-frequency LiveView updates
   - Test concurrent process killing operations

2. **Concurrent Access Tests**:
   - Test multiple LiveView sessions simultaneously
   - Test concurrent Control module operations
   - Test race conditions in process killing
   - Test supervisor inspection under load

3. **Memory Usage Tests**:
   - Test memory usage during high-volume operations
   - Test memory cleanup after process termination
   - Test memory leaks in long-running scenarios
   - Test garbage collection under load

4. **Response Time Tests**:
   - Test supervisor list response times
   - Test process information retrieval times
   - Test LiveView update latency
   - Test system responsiveness under load

Include performance benchmarks and assertions for:
- Maximum acceptable response times
- Memory usage thresholds
- Concurrent operation limits
- System stability metrics

Use proper performance testing patterns with:
- Baseline measurements
- Load generation
- Resource monitoring
- Statistical analysis

Add @tag :performance to tests for optional execution.
```

**Success Metrics:**
- [ ] New test file created with 4+ performance test cases
- [ ] High-volume scenarios tested
- [ ] Concurrent access patterns verified
- [ ] Memory usage monitored
- [ ] Response time benchmarks established
- [ ] Tests pass with `mix test test/integration/performance_test.exs`

---

## Prompt 8: Edge Case and Error Boundary Tests

**Required Reading:**
- All existing source files in `/home/home/p/g/n/superlearner/otp_supervisor/lib/`
- All existing test files in `/home/home/p/g/n/superlearner/otp_supervisor/test/`
- `/home/home/p/g/n/superlearner/docs/comprehensive-test-plan.md` (Phase 3.2)
- `/home/home/p/g/n/superlearner/docs/code-standards/otp-testing-standards.md`
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Critical for edge cases)

**Context:**
The system needs comprehensive edge case testing to handle malformed input, extreme values, and unusual scenarios that students might encounter during exploration.

**Prompt:**
```
Add edge case and error boundary tests across all existing test files.

For each existing test file, add edge case tests:

1. **Control Module Edge Cases** (extend control_test.exs):
   - Test with extremely large supervision trees
   - Test with deeply nested supervisors
   - Test with processes having extreme memory usage
   - Test with malformed process names and PIDs
   - Test with processes from different nodes/distribution

2. **Worker Module Edge Cases** (extend worker test files):
   - Test with extreme counter values (near integer limits)
   - Test with very long printer messages (>100KB)
   - Test with unicode and special characters
   - Test with binary data handling
   - Test with process names containing special characters

3. **SupervisorLive Edge Cases** (extend supervisor_live_test.exs):
   - Test with malformed URL parameters
   - Test with extremely long process lists
   - Test with processes having unusual states
   - Test with WebSocket connection failures
   - Test with invalid user input

4. **System Edge Cases** (extend integration tests):
   - Test with system under extreme load
   - Test with network partitions (if applicable)
   - Test with filesystem issues
   - Test with process limit exhaustion
   - Test with memory pressure scenarios

5. **Input Validation Tests**:
   - Test all user inputs with invalid data
   - Test boundary values for all parameters
   - Test injection attempts and security concerns
   - Test malformed configuration data

Include comprehensive error handling verification:
- Proper error messages for educational value
- Graceful degradation under stress
- Recovery from error states
- User-friendly error reporting

Use property-based testing where appropriate for exploring edge cases.
```

**Success Metrics:**
- [ ] 6+ edge case tests added across existing files
- [ ] All boundary conditions tested
- [ ] Error handling comprehensively verified
- [ ] Input validation complete
- [ ] System stability under edge conditions confirmed
- [ ] All tests pass with `mix test`

---

## Prompt 9: Final Test Coverage and Cleanup

**Required Reading:**
- All test files in `/home/home/p/g/n/superlearner/otp_supervisor/test/`
- `/home/home/p/g/n/superlearner/docs/comprehensive-test-plan.md`
- `/home/home/p/g/n/superlearner/docs/code-standards/otp-testing-standards.md`
- `/home/home/p/g/n/superlearner/docs/test-isolation-guide.md` (Ensure all tests follow isolation patterns)

**Context:**
This final prompt ensures comprehensive test coverage is achieved, performs cleanup, and validates the entire test suite.

**Prompt:**
```
Perform final test coverage analysis and cleanup.

1. **Coverage Analysis**:
   - Run test coverage analysis with mix test --cover
   - Identify any remaining gaps in coverage
   - Add targeted tests for uncovered code paths
   - Achieve 90% overall test coverage

2. **Test Suite Validation**:
   - Ensure all tests pass consistently
   - Check for flaky or non-deterministic tests
   - Verify test isolation and independence
   - Optimize test performance and execution time

3. **Code Quality Improvements**:
   - Refactor any test code duplication
   - Add missing test documentation
   - Ensure consistent test naming and structure
   - Add helper functions for common test patterns

4. **Educational Value Enhancement**:
   - Add educational comments to complex test scenarios
   - Ensure tests demonstrate OTP concepts clearly
   - Add examples of common testing patterns
   - Document test scenarios for learning purposes

5. **Documentation Updates**:
   - Update README with testing instructions
   - Document test coverage achievements
   - Add troubleshooting guide for test failures
   - Create test execution guidelines

6. **Final Validation**:
   - Run full test suite multiple times
   - Test on clean system/environment
   - Verify all success metrics are met
   - Document any remaining limitations

Include comprehensive test execution commands and coverage reporting.
```

**Success Metrics:**
- [ ] 90% test coverage achieved
- [ ] All tests pass consistently
- [ ] Test suite optimized for performance
- [ ] Educational value maximized
- [ ] Documentation updated
- [ ] Full system validation completed

---

## Execution Guidelines

### Prerequisites
- Elixir 1.15+ with Phoenix 1.7+
- Existing OTP Supervisor Educational Tool project
- Mix and ExUnit testing framework

### Execution Order
1. Execute prompts in numerical order (1-9)
2. Verify success metrics before proceeding
3. Maintain all existing functionality
4. Address any test failures before continuing

### Success Validation
After each prompt:
```bash
# Run specific test file
mix test path/to/test_file.exs

# Run all tests  
mix test

# Check coverage
mix test --cover

# Verify application still works
mix phx.server
```

### Quality Assurance
- All tests must pass consistently
- No degradation in application functionality
- Meaningful error messages for educational value
- Proper test isolation and cleanup

This comprehensive test implementation plan will result in a robust, well-tested educational tool suitable for teaching OTP supervisor concepts with 90% test coverage and comprehensive error handling.