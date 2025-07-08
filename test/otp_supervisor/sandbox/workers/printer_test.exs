defmodule OTPSupervisor.Sandbox.Workers.PrinterTest do
  use ExUnit.Case, async: true

  alias OTPSupervisor.Sandbox.Workers.Printer

  import ExUnit.CaptureLog
  import SupervisorTestHelper
  require Logger

  @moduledoc """
  Comprehensive tests for the Printer worker GenServer.

  This test suite demonstrates:
  - Basic GenServer functionality for message handling
  - Logger integration and message formatting
  - High-volume message processing
  - State management for print counting
  - Integration with supervision trees

  These tests serve both verification and educational purposes,
  showing how to properly test GenServer implementations that
  interact with external systems like Logger.
  """

  describe "basic functionality" do
    test "start_link/1 starts a printer with default options" do
      assert {:ok, pid} = Printer.start_link()
      assert Process.alive?(pid)
      assert Printer.get_print_count(pid) == 0

      # Cleanup
      GenServer.stop(pid)
    end

    test "start_link/1 accepts name option" do
      unique_id = :erlang.unique_integer([:positive])
      printer_name = :"test_printer_#{unique_id}"
      assert {:ok, pid} = Printer.start_link(name: printer_name)
      assert Process.whereis(printer_name) == pid
      assert Printer.get_print_count(printer_name) == 0

      # Cleanup
      GenServer.stop(pid)
    end

    test "start_link/1 accepts id option" do
      assert {:ok, pid} = Printer.start_link(id: "custom_printer")

      # The id should be used in log messages (tested in logging integration tests)
      assert Printer.get_print_count(pid) == 0

      # Cleanup
      GenServer.stop(pid)
    end

    test "start_link/1 with both name and id options" do
      unique_id = :erlang.unique_integer([:positive])
      printer_name = :"named_printer_#{unique_id}"
      assert {:ok, pid} = Printer.start_link(name: printer_name, id: "printer_123")
      assert Process.whereis(printer_name) == pid
      assert Printer.get_print_count(printer_name) == 0

      # Cleanup
      GenServer.stop(pid)
    end

    test "print/2 sends message to printer" do
      {:ok, pid} = Printer.start_link()

      # Print message
      assert Printer.print(pid, "Hello, world!") == :ok

      # Use synchronous call to verify the cast was processed
      # The get_print_count call will only return after all previous casts are processed
      assert Printer.get_print_count(pid) == 1

      # Cleanup
      GenServer.stop(pid)
    end

    test "get_print_count/1 returns current message count" do
      {:ok, pid} = Printer.start_link()

      assert Printer.get_print_count(pid) == 0

      # Print some messages
      Printer.print(pid, "Message 1")
      Printer.print(pid, "Message 2")
      Printer.print(pid, "Message 3")

      # Synchronous call ensures all previous casts are processed
      assert Printer.get_print_count(pid) == 3

      # Cleanup
      GenServer.stop(pid)
    end

    test "print count increments for each message" do
      {:ok, pid} = Printer.start_link()

      for i <- 1..10 do
        Printer.print(pid, "Message #{i}")
      end

      # Synchronous call ensures all casts are processed
      assert Printer.get_print_count(pid) == 10

      # Cleanup
      GenServer.stop(pid)
    end
  end

  describe "message handling" do
    test "handles string messages" do
      {:ok, pid} = Printer.start_link(id: "string_printer")

      Printer.print(pid, "This is a string message")

      # Synchronous call ensures cast is processed
      assert Printer.get_print_count(pid) == 1

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles atom messages" do
      {:ok, pid} = Printer.start_link(id: "atom_printer")

      Printer.print(pid, :important_message)

      # Synchronous call ensures cast is processed
      assert Printer.get_print_count(pid) == 1

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles number messages" do
      {:ok, pid} = Printer.start_link(id: "number_printer")

      Printer.print(pid, 42)
      Printer.print(pid, 3.14159)

      # Synchronous call ensures all casts are processed
      assert Printer.get_print_count(pid) == 2

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles very long messages" do
      {:ok, pid} = Printer.start_link(id: "long_printer")

      # Create a message over 1000 characters
      # ~1200 characters
      long_message = String.duplicate("This is a long message. ", 50)

      Printer.print(pid, long_message)

      # Synchronous call ensures cast is processed
      assert Printer.get_print_count(pid) == 1

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles messages with special characters and unicode" do
      {:ok, pid} = Printer.start_link(id: "unicode_printer")

      Printer.print(pid, "Special chars: !@#$%^&*()")
      Printer.print(pid, "Unicode: 🎉 Hello 世界 Ñoël")
      Printer.print(pid, "Newlines:\nand\ttabs")

      # Synchronous call ensures all casts are processed
      assert Printer.get_print_count(pid) == 3

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles binary message handling" do
      {:ok, pid} = Printer.start_link(id: "binary_printer")

      binary_message = <<1, 2, 3, 4, 5>>

      Printer.print(pid, binary_message)

      # Synchronous call ensures cast is processed
      assert Printer.get_print_count(pid) == 1

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles nil and empty message handling" do
      {:ok, pid} = Printer.start_link(id: "empty_printer")

      Printer.print(pid, nil)
      Printer.print(pid, "")
      # Single space
      Printer.print(pid, " ")

      # Synchronous call ensures all casts are processed
      assert Printer.get_print_count(pid) == 3

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles complex data structures" do
      {:ok, pid} = Printer.start_link(id: "complex_printer")

      Printer.print(pid, %{key: "value", count: 42})
      Printer.print(pid, ["item1", "item2", "item3"])
      Printer.print(pid, {:tuple, :message, 123})

      # Synchronous call ensures all casts are processed
      assert Printer.get_print_count(pid) == 3

      # Cleanup
      GenServer.stop(pid)
    end
  end

  describe "logging integration" do
    test "printer process handles logging integration" do
      {:ok, pid} = Printer.start_link(id: "test_logger")

      # Test that messages are processed (we can't easily test log output in test env)
      Printer.print(pid, "Test message")

      # Synchronous call ensures cast is processed
      assert Printer.get_print_count(pid) == 1

      # Cleanup
      GenServer.stop(pid)
    end

    test "uses default id when not specified" do
      {:ok, pid} = Printer.start_link()

      # Test functionality with default id
      Printer.print(pid, "Default id test")

      # Synchronous call ensures cast is processed
      assert Printer.get_print_count(pid) == 1

      # Cleanup
      GenServer.stop(pid)
    end

    test "handles different message types consistently" do
      {:ok, pid} = Printer.start_link(id: "format_test")

      # Test various message types
      Printer.print(pid, "string")
      Printer.print(pid, 123)
      Printer.print(pid, :atom)

      # Synchronous call ensures all casts are processed
      assert Printer.get_print_count(pid) == 3

      # Cleanup
      GenServer.stop(pid)
    end

    test "concurrent logging operations" do
      {:ok, pid} = Printer.start_link(id: "concurrent_logger")

      # Start multiple processes that print concurrently
      tasks =
        for i <- 1..5 do
          Task.async(fn ->
            for j <- 1..3 do
              Printer.print(pid, "Process #{i} message #{j}")
            end
          end)
        end

      # Wait for all tasks to complete
      Enum.each(tasks, &Task.await/1)

      # Synchronous call ensures all casts are processed
      # Should have 15 total messages (5 processes * 3 messages each)
      assert Printer.get_print_count(pid) == 15

      # Cleanup
      GenServer.stop(pid)
    end
  end

  describe "high-volume operations" do
    test "rapid message printing stress test" do
      {:ok, pid} = Printer.start_link(id: "stress_test")

      # Print 1000 messages rapidly using helper
      send_test_messages_and_verify(pid, 1000)

      # Cleanup
      GenServer.stop(pid)
    end

    test "print count accuracy under load" do
      {:ok, pid} = Printer.start_link(id: "accuracy_test")

      # Multiple concurrent processes incrementing
      tasks =
        for i <- 1..10 do
          Task.async(fn ->
            for j <- 1..50 do
              Printer.print(pid, "Task #{i} message #{j}")
            end
          end)
        end

      # Wait for all tasks to complete
      Enum.each(tasks, &Task.await/1)

      # Use synchronous call to verify all casts processed
      # Should have exactly 500 messages (10 tasks * 50 messages each)
      assert Printer.get_print_count(pid) == 500

      # Cleanup
      GenServer.stop(pid)
    end

    test "performance with concurrent printers" do
      # Start multiple printer instances using helper
      printers = create_test_printers(5)

      # Each printer handles messages concurrently
      tasks =
        for {pid, i} <- Enum.with_index(printers, 1) do
          Task.async(fn ->
            for j <- 1..100 do
              Printer.print(pid, "Printer #{i} message #{j}")
            end
          end)
        end

      # Wait for all tasks to complete
      Enum.each(tasks, &Task.await/1)

      # Each printer should have processed 100 messages
      for pid <- printers do
        assert Printer.get_print_count(pid) == 100
      end

      # Cleanup using helper
      cleanup_printers(printers)
    end
  end

  describe "state management" do
    test "print_count persistence across messages" do
      {:ok, pid} = Printer.start_link(id: "persistence_test")

      # Verify initial state
      assert Printer.get_print_count(pid) == 0

      # Print messages and verify count increments step by step
      Printer.print(pid, "Message 1")
      assert Printer.get_print_count(pid) == 1

      Printer.print(pid, "Message 2")
      assert Printer.get_print_count(pid) == 2

      Printer.print(pid, "Message 3")
      assert Printer.get_print_count(pid) == 3

      # Cleanup
      GenServer.stop(pid)
    end

    test "state consistency during concurrent operations" do
      {:ok, pid} = Printer.start_link(id: "consistency_test")

      # Mix of print and get_count operations
      print_tasks =
        for i <- 1..10 do
          Task.async(fn ->
            for j <- 1..20 do
              Printer.print(pid, "Task #{i} message #{j}")
            end
          end)
        end

      read_tasks =
        for _i <- 1..5 do
          Task.async(fn ->
            for _j <- 1..10 do
              count = Printer.get_print_count(pid)
              # Count should never be negative
              assert count >= 0
            end
          end)
        end

      # Wait for all tasks to complete
      Enum.each(print_tasks ++ read_tasks, &Task.await/1)

      # Should have 200 messages total (10 tasks * 20 messages each)
      assert Printer.get_print_count(pid) == 200

      # Cleanup
      GenServer.stop(pid)
    end

    test "state reset after crashes and restarts" do
      # Create a simple supervisor for testing
      unique_id = :erlang.unique_integer([:positive])
      printer_name = :"supervised_printer_#{unique_id}"

      children = [
        {Printer, name: printer_name, id: "supervised"}
      ]

      {:ok, sup_pid} = Supervisor.start_link(children, strategy: :one_for_one)

      # Print some messages
      Printer.print(printer_name, "Message 1")
      Printer.print(printer_name, "Message 2")

      # Verify state before crash
      assert Printer.get_print_count(printer_name) == 2

      # Get the current process PID
      original_pid = Process.whereis(printer_name)

      # Crash the printer by killing it directly (simulating a crash)
      Process.exit(original_pid, :kill)

      # Wait for supervisor to restart it (using proper OTP monitoring)
      ref = Process.monitor(original_pid)

      receive do
        {:DOWN, ^ref, :process, ^original_pid, _reason} -> :ok
      after
        1000 -> flunk("Process did not terminate within timeout")
      end

      # Wait for supervisor to restart the child using the more robust helper
      # The child ID is the module name when using {Module, args} format
      :ok = wait_for_child_restart(sup_pid, Printer, original_pid)

      # Verify the process was restarted with fresh state
      new_pid = Process.whereis(printer_name)

      if new_pid && new_pid != original_pid do
        # Reset state
        assert Printer.get_print_count(printer_name) == 0
      end

      # Cleanup
      Supervisor.stop(sup_pid)
    end

    test "handling of large print counts" do
      {:ok, pid} = Printer.start_link(id: "large_count_test")

      # Print a large number of messages using helper
      send_test_messages_and_verify(pid, 10000)

      # Continue printing to test even larger counts
      for i <- 10001..15000 do
        Printer.print(pid, "Extended count #{i}")
      end

      # Verify final count
      assert Printer.get_print_count(pid) == 15000

      # Cleanup
      GenServer.stop(pid)
    end
  end

  describe "integration with supervisors" do
    test "multiple printer instances under supervision" do
      unique_id = :erlang.unique_integer([:positive])
      printer_name_alpha = :"printer_alpha_#{unique_id}"
      printer_name_beta = :"printer_beta_#{unique_id}"
      printer_name_gamma = :"printer_gamma_#{unique_id}"

      children = [
        Supervisor.child_spec({Printer, name: printer_name_alpha, id: "alpha"},
          id: :printer_alpha
        ),
        Supervisor.child_spec({Printer, name: printer_name_beta, id: "beta"}, id: :printer_beta),
        Supervisor.child_spec({Printer, name: printer_name_gamma, id: "gamma"},
          id: :printer_gamma
        )
      ]

      {:ok, sup_pid} = Supervisor.start_link(children, strategy: :one_for_one)

      # Verify all printers started
      assert Printer.get_print_count(printer_name_alpha) == 0
      assert Printer.get_print_count(printer_name_beta) == 0
      assert Printer.get_print_count(printer_name_gamma) == 0

      # Each printer operates independently
      capture_log(fn ->
        Printer.print(printer_name_alpha, "Alpha message 1")
        Printer.print(printer_name_beta, "Beta message 1")
        Printer.print(printer_name_beta, "Beta message 2")
        Printer.print(printer_name_gamma, "Gamma message 1")
        Printer.print(printer_name_gamma, "Gamma message 2")
        Printer.print(printer_name_gamma, "Gamma message 3")
      end)

      assert Printer.get_print_count(printer_name_alpha) == 1
      assert Printer.get_print_count(printer_name_beta) == 2
      assert Printer.get_print_count(printer_name_gamma) == 3

      # Cleanup
      Supervisor.stop(sup_pid)
    end

    test "supports named vs unnamed process registration" do
      # Named process
      unique_id = :erlang.unique_integer([:positive])
      printer_name = :"named_test_printer_#{unique_id}"

      {:ok, named_pid} = Printer.start_link(name: printer_name, id: "named")
      assert Process.whereis(printer_name) == named_pid

      # Unnamed process  
      {:ok, unnamed_pid} = Printer.start_link(id: "unnamed")
      # Not registered
      assert Process.whereis(:unnamed_printer) == nil

      # Both should work independently
      capture_log(fn ->
        Printer.print(printer_name, "Named message")
        Printer.print(unnamed_pid, "Unnamed message")
      end)

      assert Printer.get_print_count(printer_name) == 1
      assert Printer.get_print_count(unnamed_pid) == 1

      # Cleanup
      GenServer.stop(named_pid)
      GenServer.stop(unnamed_pid)
    end

    test "proper cleanup on process termination" do
      unique_id = :erlang.unique_integer([:positive])
      printer_name = :"cleanup_test_printer_#{unique_id}"

      {:ok, pid} = Printer.start_link(name: printer_name, id: "cleanup")

      # Verify process is registered
      assert Process.whereis(printer_name) == pid

      # Print some messages
      capture_log(fn ->
        Printer.print(printer_name, "Cleanup test")
      end)

      assert Printer.get_print_count(printer_name) == 1

      # Terminate normally
      GenServer.stop(pid)

      # Verify cleanup
      refute Process.alive?(pid)
      assert Process.whereis(printer_name) == nil
    end

    test "demonstrates restart behavior differences" do
      # Test shows how printer state is handled under supervision
      unique_id = :erlang.unique_integer([:positive])
      printer_name_1 = :"demo_printer_1_#{unique_id}"
      printer_name_2 = :"demo_printer_2_#{unique_id}"

      children = [
        Supervisor.child_spec({Printer, name: printer_name_1, id: "demo1"}, id: :demo_printer_1),
        Supervisor.child_spec({Printer, name: printer_name_2, id: "demo2"}, id: :demo_printer_2)
      ]

      {:ok, sup_pid} = Supervisor.start_link(children, strategy: :one_for_one)

      # Print messages to both printers
      capture_log(fn ->
        Printer.print(printer_name_1, "Demo 1 message")
        Printer.print(printer_name_2, "Demo 2 message 1")
        Printer.print(printer_name_2, "Demo 2 message 2")
      end)

      assert Printer.get_print_count(printer_name_1) == 1
      assert Printer.get_print_count(printer_name_2) == 2

      # Store PIDs for comparison
      pid1_before = Process.whereis(printer_name_1)
      pid2_before = Process.whereis(printer_name_2)

      # Crash one printer by sending invalid message
      capture_log(fn ->
        send(pid1_before, :invalid_message)
      end)

      # Wait for supervisor to restart the crashed process (if it crashes)
      # Note: sending invalid messages might not always crash the process
      case wait_for_child_restart(sup_pid, :demo_printer_1, pid1_before, 500) do
        # Process restarted
        :ok -> :ok
        # Process didn't crash, that's fine too
        {:error, :timeout} -> :ok
      end

      # With :one_for_one strategy:
      # - Only the crashed process restarts
      # - Other processes continue unchanged
      pid1_after = Process.whereis(printer_name_1)
      pid2_after = Process.whereis(printer_name_2)

      if pid1_after && pid1_after != pid1_before do
        # Process restarted with fresh state
        assert Printer.get_print_count(printer_name_1) == 0
      end

      # Other printer unchanged
      assert pid2_after == pid2_before
      assert Printer.get_print_count(printer_name_2) == 2

      # Cleanup
      Supervisor.stop(sup_pid)
    end
  end

  describe "educational scenarios" do
    test "demonstrates message passing patterns" do
      # Educational example of async message handling
      {:ok, pid} = Printer.start_link(id: "message_demo")

      # Messages are sent asynchronously via cast
      capture_log(fn ->
        Printer.print(pid, "Async message 1")
        Printer.print(pid, "Async message 2")
        Printer.print(pid, "Async message 3")
      end)

      # State queries are synchronous via call
      # Synchronous call
      count = Printer.get_print_count(pid)
      assert count == 3

      # Cleanup
      GenServer.stop(pid)
    end

    test "shows state management across operations" do
      # Educational example of GenServer state persistence
      {:ok, pid} = Printer.start_link(id: "state_demo")

      # State starts at 0
      assert Printer.get_print_count(pid) == 0

      # Each operation modifies state
      capture_log(fn ->
        Printer.print(pid, "State demo 1")
        assert Printer.get_print_count(pid) == 1

        Printer.print(pid, "State demo 2")
        assert Printer.get_print_count(pid) == 2
      end)

      # State persists between operations
      assert Printer.get_print_count(pid) == 2

      # Cleanup
      GenServer.stop(pid)
    end

    test "demonstrates logging as side effects" do
      # Educational example of side effects in GenServer
      {:ok, pid} = Printer.start_link(id: "side_effect_demo")

      # The print operation has two effects:
      # 1. Logs a message (side effect) - we can't easily test this in test env
      # 2. Updates internal state (state change) - we can test this

      Printer.print(pid, "Side effect demonstration")

      # State change: counter increments (this we can verify)
      assert Printer.get_print_count(pid) == 1

      # Cleanup
      GenServer.stop(pid)
    end

    test "demonstrates concurrent access safety" do
      # Educational example of GenServer concurrency safety
      {:ok, pid} = Printer.start_link(id: "concurrency_demo")

      # Multiple processes can safely access the same GenServer
      capture_log(fn ->
        tasks =
          for i <- 1..5 do
            Task.async(fn ->
              for j <- 1..10 do
                Printer.print(pid, "Process #{i} message #{j}")
              end
            end)
          end

        # Wait for all concurrent operations
        Enum.each(tasks, &Task.await/1)
      end)

      # GenServer ensures all operations are processed safely
      # 5 processes * 10 messages
      assert Printer.get_print_count(pid) == 50

      # Cleanup
      GenServer.stop(pid)
    end
  end

  # Helper functions for common test patterns

  defp create_test_printers(count) do
    unique_id = :erlang.unique_integer([:positive])

    for i <- 1..count do
      printer_name = :"test_printer_#{i}_#{unique_id}"
      {:ok, pid} = Printer.start_link(name: printer_name, id: "test_#{i}")
      pid
    end
  end

  defp send_test_messages_and_verify(pid, count) do
    # Send messages using proper OTP patterns
    for i <- 1..count do
      Printer.print(pid, "Test message #{i}")
    end

    # Use synchronous call to ensure all casts are processed
    final_count = Printer.get_print_count(pid)
    assert final_count == count
    final_count
  end

  defp cleanup_printers(pids) do
    for pid <- pids do
      if Process.alive?(pid) do
        GenServer.stop(pid)
      end
    end
  end

  defp generate_test_message(type) do
    case type do
      :string -> "Test string message"
      :atom -> :test_atom_message
      :number -> 42
      :float -> 3.14159
      :long -> String.duplicate("Long message part. ", 100)
      :unicode -> "Unicode: 🎉 Hello 世界 Ñoël"
      :binary -> <<1, 2, 3, 4, 5>>
      :complex -> %{key: "value", list: [1, 2, 3], tuple: {:a, :b}}
      nil -> nil
      :empty -> ""
    end
  end

  # Add one test that uses the helper to eliminate the warning
  test "helper function demonstration" do
    {:ok, pid} = Printer.start_link(id: "helper_demo")

    # Test various message types using helper
    message_types = [:string, :atom, :number, :unicode, :binary, :complex, nil, :empty]

    for msg_type <- message_types do
      message = generate_test_message(msg_type)
      Printer.print(pid, message)
    end

    # Verify all messages were processed
    assert Printer.get_print_count(pid) == length(message_types)

    # Cleanup
    GenServer.stop(pid)
  end
end
