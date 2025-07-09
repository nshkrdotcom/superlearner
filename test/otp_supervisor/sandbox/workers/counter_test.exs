defmodule OtpSandbox.Workers.CounterTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog
  import SupervisorTestHelper

  alias OtpSandbox.Workers.Counter

  @moduledoc """
  Comprehensive tests for the Counter worker GenServer.

  This test suite demonstrates:
  - Basic GenServer functionality and lifecycle
  - Concurrent operations and state consistency
  - Error recovery and supervisor restart patterns
  - State validation with edge cases
  - Integration with supervision trees

  These tests serve both verification and educational purposes,
  showing how to properly test GenServer implementations.
  """

  describe "basic functionality" do
    test "start_link/1 starts a counter with default options" do
      assert {:ok, pid} = Counter.start_link()
      assert Process.alive?(pid)
      assert Counter.get_value(pid) == 0

      # Cleanup
      GenServer.stop(pid)
    end

    test "start_link/1 accepts name option" do
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"test_counter_#{unique_id}"
      assert {:ok, pid} = Counter.start_link(name: counter_name)
      assert Process.whereis(counter_name) == pid
      assert Counter.get_value(counter_name) == 0

      # Cleanup
      GenServer.stop(pid)
    end

    test "start_link/1 accepts initial_value option" do
      assert {:ok, pid} = Counter.start_link(initial_value: 42)
      assert Counter.get_value(pid) == 42

      # Cleanup
      GenServer.stop(pid)
    end

    test "start_link/1 with both name and initial_value options" do
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"named_counter_#{unique_id}"
      assert {:ok, pid} = Counter.start_link(name: counter_name, initial_value: 100)
      assert Process.whereis(counter_name) == pid
      assert Counter.get_value(counter_name) == 100

      # Cleanup
      GenServer.stop(pid)
    end

    test "increment/1 increases counter value" do
      {:ok, pid} = Counter.start_link(initial_value: 5)

      assert Counter.get_value(pid) == 5
      assert Counter.increment(pid) == :ok
      assert Counter.get_value(pid) == 6

      # Multiple increments
      Counter.increment(pid)
      Counter.increment(pid)
      assert Counter.get_value(pid) == 8

      # Cleanup
      GenServer.stop(pid)
    end

    test "get_value/1 returns current counter value" do
      {:ok, pid} = Counter.start_link(initial_value: 123)
      assert Counter.get_value(pid) == 123

      # Cleanup
      GenServer.stop(pid)
    end

    test "crash/1 terminates the process" do
      # Start unlinked process to avoid test process crash
      {:ok, pid} = Counter.start_link()
      # Unlink to prevent crash from affecting test
      Process.unlink(pid)

      # Monitor the process to detect when it dies
      ref = Process.monitor(pid)

      capture_log(fn -> Counter.crash(pid) end)

      # Wait for the process to crash
      assert_receive {:DOWN, ^ref, :process, ^pid,
                      {%RuntimeError{message: "Intentional crash for demonstration"}, _}}

      refute Process.alive?(pid)
    end
  end

  describe "concurrent operations" do
    test "multiple processes can increment simultaneously" do
      {:ok, pid} = Counter.start_link(initial_value: 0)

      # Spawn multiple processes that increment concurrently
      tasks =
        for _i <- 1..10 do
          Task.async(fn ->
            for _j <- 1..5 do
              Counter.increment(pid)
            end
          end)
        end

      # Wait for all tasks to complete
      Enum.each(tasks, &Task.await/1)

      # Should have 50 increments total (10 processes * 5 increments each)
      assert Counter.get_value(pid) == 50

      # Cleanup
      GenServer.stop(pid)
    end

    test "rapid increment/get_value cycles maintain consistency" do
      {:ok, pid} = Counter.start_link(initial_value: 0)

      # Rapid operations
      for i <- 1..100 do
        Counter.increment(pid)
        # Occasionally check the value during increments
        if rem(i, 10) == 0 do
          value = Counter.get_value(pid)
          # Value should be at least the number of increments so far
          assert value >= i
        end
      end

      assert Counter.get_value(pid) == 100

      # Cleanup
      GenServer.stop(pid)
    end

    test "state consistency under concurrent load" do
      {:ok, pid} = Counter.start_link(initial_value: 0)

      # Mix of increment and get_value operations
      increment_tasks =
        for _i <- 1..5 do
          Task.async(fn ->
            for _j <- 1..20 do
              Counter.increment(pid)
            end
          end)
        end

      read_tasks =
        for _i <- 1..3 do
          Task.async(fn ->
            for _j <- 1..10 do
              Counter.get_value(pid)
              # get_value/1 is synchronous - no additional synchronization needed
            end
          end)
        end

      # Wait for all tasks
      Enum.each(increment_tasks ++ read_tasks, &Task.await/1)

      # Should have 100 increments total (5 processes * 20 increments each)
      assert Counter.get_value(pid) == 100

      # Cleanup
      GenServer.stop(pid)
    end

    test "multiple counter processes operate independently" do
      unique_id = :erlang.unique_integer([:positive])
      counter_name_1 = :"independent_counter_1_#{unique_id}"
      counter_name_2 = :"independent_counter_2_#{unique_id}"
      counter_name_3 = :"independent_counter_3_#{unique_id}"

      {:ok, pid1} = Counter.start_link(name: counter_name_1, initial_value: 10)
      {:ok, pid2} = Counter.start_link(name: counter_name_2, initial_value: 20)
      {:ok, pid3} = Counter.start_link(name: counter_name_3, initial_value: 30)

      # Increment each counter a different number of times
      for _i <- 1..5, do: Counter.increment(counter_name_1)
      for _i <- 1..10, do: Counter.increment(counter_name_2)
      for _i <- 1..15, do: Counter.increment(counter_name_3)

      # 10 + 5
      assert Counter.get_value(counter_name_1) == 15
      # 20 + 10
      assert Counter.get_value(counter_name_2) == 30
      # 30 + 15
      assert Counter.get_value(counter_name_3) == 45

      # Cleanup
      GenServer.stop(pid1)
      GenServer.stop(pid2)
      GenServer.stop(pid3)
    end
  end

  describe "error recovery" do
    test "process can be restarted after crash" do
      # This test demonstrates manual restart (what a supervisor would do)
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"crashable_counter_#{unique_id}"

      {:ok, pid1} = Counter.start_link(name: counter_name, initial_value: 42)
      # Unlink to prevent crash from affecting test
      Process.unlink(pid1)
      original_pid = pid1

      # Increment a few times
      Counter.increment(counter_name)
      Counter.increment(counter_name)
      assert Counter.get_value(counter_name) == 44

      # Crash the process
      ref = Process.monitor(pid1)
      capture_log(fn -> Counter.crash(counter_name) end)
      assert_receive {:DOWN, ^ref, :process, ^original_pid, _}

      # Restart with same name (simulating supervisor behavior)
      {:ok, pid2} = Counter.start_link(name: counter_name, initial_value: 42)

      # Verify it's a new process with fresh state
      assert pid2 != original_pid
      # Reset to initial value
      assert Counter.get_value(counter_name) == 42

      # Cleanup
      GenServer.stop(pid2)
    end

    test "state resets after supervisor restart" do
      # Create a simple supervisor for testing
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"supervised_counter_#{unique_id}"

      children = [
        {Counter, name: counter_name, initial_value: 100}
      ]

      {:ok, sup_pid} = Supervisor.start_link(children, strategy: :one_for_one)

      # Verify initial state
      assert Counter.get_value(counter_name) == 100

      # Increment and verify change
      Counter.increment(counter_name)
      Counter.increment(counter_name)
      assert Counter.get_value(counter_name) == 102

      # Get the current process PID
      original_counter_pid = Process.whereis(counter_name)

      # Crash the counter
      capture_log(fn -> Counter.crash(counter_name) end)

      # Wait for supervisor to restart the process
      :ok = wait_for_process_restart(counter_name, original_counter_pid)

      # Verify the process was restarted with fresh state
      new_counter_pid = Process.whereis(counter_name)
      assert new_counter_pid != original_counter_pid
      # Back to initial value
      assert Counter.get_value(counter_name) == 100

      # Cleanup
      Supervisor.stop(sup_pid)
    end

    test "handles invalid messages gracefully" do
      {:ok, pid} = Counter.start_link(initial_value: 10)

      # Send invalid messages that should be ignored
      capture_log(fn ->
        send(pid, :invalid_message)
        send(pid, {:unknown, :message})
        send(pid, "string message")
      end)

      # Ensure messages are processed by making a synchronous call
      Counter.get_value(pid)

      # Counter should still function normally
      assert Counter.get_value(pid) == 10
      Counter.increment(pid)
      assert Counter.get_value(pid) == 11

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles call timeout scenarios" do
      {:ok, pid} = Counter.start_link()

      # Normal call should work fine
      assert Counter.get_value(pid) == 0

      # Test with explicit timeout (should still work for quick operations)
      assert GenServer.call(pid, :get_value, 100) == 0

      # Cleanup
      GenServer.stop(pid)
    end
  end

  describe "state validation" do
    test "accepts negative initial values" do
      {:ok, pid} = Counter.start_link(initial_value: -50)
      assert Counter.get_value(pid) == -50

      Counter.increment(pid)
      assert Counter.get_value(pid) == -49

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles zero initial value" do
      {:ok, pid} = Counter.start_link(initial_value: 0)
      assert Counter.get_value(pid) == 0

      Counter.increment(pid)
      assert Counter.get_value(pid) == 1

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles large initial values" do
      large_value = 1_000_000_000
      {:ok, pid} = Counter.start_link(initial_value: large_value)
      assert Counter.get_value(pid) == large_value

      Counter.increment(pid)
      assert Counter.get_value(pid) == large_value + 1

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles very large values near integer limits" do
      # Test with a large value approaching system limits
      # Close to max 64-bit signed integer
      large_value = 9_223_372_036_854_775_800
      {:ok, pid} = Counter.start_link(initial_value: large_value)
      assert Counter.get_value(pid) == large_value

      # Should still be able to increment
      Counter.increment(pid)
      assert Counter.get_value(pid) == large_value + 1

      # Cleanup
      GenServer.stop(pid)
    end

    test "accepts non-integer initial values" do
      # The current implementation actually accepts any value as initial state
      # Let's test what actually happens with non-integer values
      {:ok, pid} = Counter.start_link(initial_value: 3.14)
      assert Counter.get_value(pid) == 3.14

      # Incrementing a float will still work (but may have floating point precision issues)
      Counter.increment(pid)
      result = Counter.get_value(pid)
      # Allow for floating point precision
      assert abs(result - 4.14) < 0.001

      # Cleanup
      GenServer.stop(pid)
    end
  end

  describe "integration with supervisors" do
    test "works correctly under one_for_one supervision" do
      unique_id = :erlang.unique_integer([:positive])
      counter_name_a = :"counter_a_#{unique_id}"
      counter_name_b = :"counter_b_#{unique_id}"
      counter_name_c = :"counter_c_#{unique_id}"

      children = [
        Supervisor.child_spec({Counter, name: counter_name_a, initial_value: 1}, id: :counter_a),
        Supervisor.child_spec({Counter, name: counter_name_b, initial_value: 2}, id: :counter_b),
        Supervisor.child_spec({Counter, name: counter_name_c, initial_value: 3}, id: :counter_c)
      ]

      {:ok, sup_pid} = Supervisor.start_link(children, strategy: :one_for_one)

      # Verify all counters started
      assert Counter.get_value(counter_name_a) == 1
      assert Counter.get_value(counter_name_b) == 2
      assert Counter.get_value(counter_name_c) == 3

      # Get PIDs for comparison
      pid_a = Process.whereis(counter_name_a)
      pid_b = Process.whereis(counter_name_b)
      pid_c = Process.whereis(counter_name_c)

      # Crash one counter
      capture_log(fn -> Counter.crash(counter_name_b) end)
      # Wait for restart
      :ok = wait_for_process_restart(counter_name_b, pid_b)

      # Verify only the crashed counter was restarted
      # Same PID
      assert Process.whereis(counter_name_a) == pid_a
      # Different PID (restarted)
      assert Process.whereis(counter_name_b) != pid_b
      # Same PID
      assert Process.whereis(counter_name_c) == pid_c

      # Verify states
      # Unchanged
      assert Counter.get_value(counter_name_a) == 1
      # Reset to initial
      assert Counter.get_value(counter_name_b) == 2
      # Unchanged
      assert Counter.get_value(counter_name_c) == 3

      # Cleanup
      Supervisor.stop(sup_pid)
    end

    test "supports named vs unnamed process registration" do
      # Named process
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"named_test_counter_#{unique_id}"

      {:ok, named_pid} = Counter.start_link(name: counter_name)
      assert Process.whereis(counter_name) == named_pid

      # Unnamed process
      {:ok, unnamed_pid} = Counter.start_link()
      # Not registered
      assert Process.whereis(:unnamed_counter) == nil

      # Both should work independently
      Counter.increment(counter_name)
      Counter.increment(unnamed_pid)

      assert Counter.get_value(counter_name) == 1
      assert Counter.get_value(unnamed_pid) == 1

      # Cleanup
      GenServer.stop(named_pid)
      GenServer.stop(unnamed_pid)
    end

    test "proper cleanup on process termination" do
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"cleanup_test_#{unique_id}"

      {:ok, pid} = Counter.start_link(name: counter_name)

      # Verify process is registered
      assert Process.whereis(counter_name) == pid

      # Terminate normally
      GenServer.stop(pid)

      # Verify cleanup
      refute Process.alive?(pid)
      assert Process.whereis(counter_name) == nil
    end

    test "demonstrates restart strategy differences" do
      # This test shows how counters behave under different supervisor strategies
      # and serves as an educational example

      unique_id = :erlang.unique_integer([:positive])
      counter_name_1 = :"demo_counter_1_#{unique_id}"
      counter_name_2 = :"demo_counter_2_#{unique_id}"

      children = [
        Supervisor.child_spec({Counter, name: counter_name_1, initial_value: 10},
          id: :demo_counter_1
        ),
        Supervisor.child_spec({Counter, name: counter_name_2, initial_value: 20},
          id: :demo_counter_2
        )
      ]

      {:ok, sup_pid} = Supervisor.start_link(children, strategy: :one_for_one)

      # Increment both counters
      Counter.increment(counter_name_1)
      Counter.increment(counter_name_2)
      Counter.increment(counter_name_2)

      assert Counter.get_value(counter_name_1) == 11
      assert Counter.get_value(counter_name_2) == 22

      # Store PIDs for comparison
      pid1_before = Process.whereis(counter_name_1)
      pid2_before = Process.whereis(counter_name_2)

      # Crash one counter
      capture_log(fn -> Counter.crash(counter_name_1) end)
      :ok = wait_for_process_restart(counter_name_1, pid1_before)

      # With :one_for_one strategy:
      # - Only the crashed process restarts
      # - Other processes continue unchanged
      pid1_after = Process.whereis(counter_name_1)
      pid2_after = Process.whereis(counter_name_2)

      # Restarted
      assert pid1_after != pid1_before
      # Unchanged
      assert pid2_after == pid2_before

      # Reset to initial
      assert Counter.get_value(counter_name_1) == 10
      # Preserved state
      assert Counter.get_value(counter_name_2) == 22

      # Cleanup
      Supervisor.stop(sup_pid)
    end
  end

  describe "educational scenarios" do
    test "demonstrates GenServer state management" do
      # This test serves as documentation for how GenServer state works
      {:ok, pid} = Counter.start_link(initial_value: 0)

      # State is maintained between calls
      Counter.increment(pid)
      Counter.increment(pid)
      Counter.increment(pid)
      assert Counter.get_value(pid) == 3

      # State persists until process dies
      assert Counter.get_value(pid) == 3

      # Cleanup
      GenServer.stop(pid)
    end

    test "shows how crashes trigger supervisor restarts" do
      # Educational example of supervision in action
      unique_id = :erlang.unique_integer([:positive])
      counter_name = :"educational_counter_#{unique_id}"

      children = [{Counter, name: counter_name, initial_value: 42}]
      {:ok, sup_pid} = Supervisor.start_link(children, strategy: :one_for_one)

      # 1. Process starts with initial state
      assert Counter.get_value(counter_name) == 42

      # 2. State can be modified
      Counter.increment(counter_name)
      assert Counter.get_value(counter_name) == 43

      # 3. Crash destroys state but supervisor recreates process
      original_pid = Process.whereis(counter_name)
      capture_log(fn -> Counter.crash(counter_name) end)
      :ok = wait_for_process_restart(counter_name, original_pid)

      # 4. New process with fresh state
      new_pid = Process.whereis(counter_name)
      assert new_pid != original_pid
      # Reset
      assert Counter.get_value(counter_name) == 42

      # 5. New process works normally
      Counter.increment(counter_name)
      assert Counter.get_value(counter_name) == 43

      # Cleanup
      Supervisor.stop(sup_pid)
    end

    test "demonstrates concurrent access patterns" do
      # Educational example of how GenServer handles concurrency
      {:ok, pid} = Counter.start_link(initial_value: 0)

      # GenServer processes messages sequentially, ensuring state consistency
      parent = self()

      # Spawn a process that increments rapidly
      _incrementer =
        spawn(fn ->
          for i <- 1..100 do
            Counter.increment(pid)

            if rem(i, 20) == 0 do
              send(parent, {:increment_progress, i})
            end
          end

          send(parent, :increment_done)
        end)

      # Spawn a process that reads values
      _reader =
        spawn(fn ->
          for i <- 1..50 do
            value = Counter.get_value(pid)
            send(parent, {:read_value, i, value})
          end

          send(parent, :read_done)
        end)

      # Wait for both to complete
      assert_receive :increment_done, 1000
      assert_receive :read_done, 1000

      # Final value should be exactly 100 (all increments completed)
      assert Counter.get_value(pid) == 100

      # Cleanup
      GenServer.stop(pid)
    end
  end
end
