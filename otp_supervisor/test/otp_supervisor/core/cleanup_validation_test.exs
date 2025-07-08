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
        nil -> :ok  # Not all children are named
        registered_pid ->
          {:ok, resolved_pid} = OTPSupervisor.Core.Control.to_pid(atom_name)
          assert resolved_pid == registered_pid
      end
    end
  end
  
  describe "demo supervisor functionality preserved" do
    setup do
      get_demo_supervisor()
    end
    
    test "demo supervisor accessible", %{supervisor: supervisor} do
      assert supervisor == :demo_one_for_one
      {:ok, children} = OTPSupervisor.Core.Control.get_supervision_tree(supervisor)
      assert length(children) >= 3  # Should have counters and printer
    end
    
    test "worker processes functional", %{supervisor: _supervisor} do
      # Test counter functionality
      if Process.whereis(:counter_1) do
        original_value = OTPSupervisor.Sandbox.Workers.Counter.get_value(:counter_1)
        OTPSupervisor.Sandbox.Workers.Counter.increment(:counter_1)
        new_value = OTPSupervisor.Sandbox.Workers.Counter.get_value(:counter_1)
        assert new_value == original_value + 1
      end
      
      # Test printer functionality
      if Process.whereis(:printer_1) do
        assert :ok = OTPSupervisor.Sandbox.Workers.Printer.print(:printer_1, "test")
      end
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