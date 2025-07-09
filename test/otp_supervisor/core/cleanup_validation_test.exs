defmodule OTPSupervisor.Core.CleanupValidationTest do
  use ExUnit.Case, async: true
  import SupervisorTestHelper

  @moduledoc """
  Validates that core OTP functionality remains intact after removing flawed features.
  These tests must pass before, during, and after the cleanup process.
  """

  describe "core supervision functionality preserved" do
    setup do
      setup_isolated_supervisor("cleanup_validation")
    end

    test "supervisor introspection still works", %{supervisor: supervisor} do
      {:ok, children} = OTPSupervisor.Core.Control.get_supervision_tree(supervisor)
      assert is_list(children)
      assert length(children) > 0

      # Verify each child has required fields
      for child <- children do
        assert Map.has_key?(child, :id)
        assert Map.has_key?(child, :pid)
        assert Map.has_key?(child, :type)
      end
    end

    test "process killing still works", %{supervisor: supervisor} do
      {:ok, children} = OTPSupervisor.Core.Control.get_supervision_tree(supervisor)
      target_child = hd(children)
      target_pid = extract_pid_from_string(target_child.pid)

      # Monitor the process
      ref = Process.monitor(target_pid)

      # Kill the process
      :ok = OTPSupervisor.Core.Control.kill_process(target_pid)

      # Verify process died
      receive do
        {:DOWN, ^ref, :process, ^target_pid, _reason} -> :ok
      after
        1000 -> flunk("Process did not terminate")
      end

      refute Process.alive?(target_pid)
    end

    test "PID conversion utilities still work", %{supervisor: supervisor} do
      {:ok, children} = OTPSupervisor.Core.Control.get_supervision_tree(supervisor)
      target_child = hd(children)
      pid_string = target_child.pid

      # Test PID conversion
      {:ok, pid} = OTPSupervisor.Core.Control.to_pid(pid_string)
      assert is_pid(pid)
      assert Process.alive?(pid)

      # Test with actual PID (should pass through)
      {:ok, same_pid} = OTPSupervisor.Core.Control.to_pid(pid)
      assert same_pid == pid

      # Test with atom name
      atom_name = target_child.id

      case Process.whereis(atom_name) do
        # Not all children are named
        nil ->
          :ok

        registered_pid ->
          {:ok, resolved_pid} = OTPSupervisor.Core.Control.to_pid(atom_name)
          assert resolved_pid == registered_pid
      end
    end
  end

  describe "sandbox functionality preserved" do
    setup do
      import SandboxTestHelper
      setup_sandbox_test(nil)
    end

    test "can create sandbox with demo supervisor" do
      # Test that we can create a sandbox with the demo supervisor
      {:ok, sandbox_info} =
        OTPSupervisor.Core.Control.create_sandbox(OtpSandbox.TestDemoSupervisor)

      # Verify sandbox has correct structure
      assert is_binary(sandbox_info.id)
      assert sandbox_info.supervisor_module == OtpSandbox.TestDemoSupervisor
      assert is_pid(sandbox_info.supervisor_pid)
      assert Process.alive?(sandbox_info.supervisor_pid)

      # Verify children are running
      children = Supervisor.which_children(sandbox_info.supervisor_pid)
      assert length(children) == 3

      # Cleanup
      :ok = OTPSupervisor.Core.Control.destroy_sandbox(sandbox_info.id)
    end

    test "worker processes functional in sandbox" do
      # Create sandbox
      {:ok, sandbox_info} =
        OTPSupervisor.Core.Control.create_sandbox(OtpSandbox.TestDemoSupervisor)

      # Get children from sandbox supervisor
      children = Supervisor.which_children(sandbox_info.supervisor_pid)

      # Find counter and printer processes
      counter_1 = Enum.find(children, fn {id, _pid, _type, _modules} -> id == :counter_1 end)
      printer_1 = Enum.find(children, fn {id, _pid, _type, _modules} -> id == :printer_1 end)

      assert counter_1 != nil
      assert printer_1 != nil

      # Test counter functionality
      {_id, counter_pid, _type, _modules} = counter_1
      original_value = OtpSandbox.Workers.Counter.get_value(counter_pid)
      OtpSandbox.Workers.Counter.increment(counter_pid)
      new_value = OtpSandbox.Workers.Counter.get_value(counter_pid)
      assert new_value == original_value + 1

      # Test printer functionality
      {_id, printer_pid, _type, _modules} = printer_1
      assert :ok = OtpSandbox.Workers.Printer.print(printer_pid, "test")

      # Cleanup
      :ok = OTPSupervisor.Core.Control.destroy_sandbox(sandbox_info.id)
    end
  end

  describe "phoenix application functionality preserved" do
    test "application starts successfully" do
      # This test runs in the context of the started application
      # If it runs, the application started successfully
      assert Process.whereis(OtpSupervisorWeb.Endpoint) != nil
      assert Process.whereis(OtpSupervisor.PubSub) != nil
    end

    test "registry functionality preserved" do
      # TracerRegistry should still be available
      assert Process.whereis(TracerRegistry) != nil

      # Should be able to register/lookup
      unique_id = :erlang.unique_integer([:positive])
      test_key = {:test_cleanup, unique_id}

      {:ok, _} = Registry.register(TracerRegistry, test_key, "test_value")
      [{pid, "test_value"}] = Registry.lookup(TracerRegistry, test_key)
      assert pid == self()
    end
  end
end
