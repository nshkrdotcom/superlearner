defmodule SandboxIntegrationTest do
  use ExUnit.Case, async: false

  @moduletag :ui

  alias OTPSupervisor.Core.Control

  describe "sandbox integration" do
    test "can create and destroy sandbox with TestDemoSupervisor" do
      # Create sandbox
      {:ok, sandbox_info} =
        Control.create_sandbox(OtpSandbox.TestDemoSupervisor, strategy: :one_for_one)

      # Verify sandbox info structure
      assert is_binary(sandbox_info.id)
      assert sandbox_info.app_name == :otp_sandbox
      assert sandbox_info.supervisor_module == OtpSandbox.TestDemoSupervisor
      assert is_pid(sandbox_info.app_pid)
      assert is_pid(sandbox_info.supervisor_pid)
      assert Process.alive?(sandbox_info.app_pid)
      assert Process.alive?(sandbox_info.supervisor_pid)

      # Verify children are running
      children = Supervisor.which_children(sandbox_info.supervisor_pid)
      assert length(children) == 3

      # Verify child naming
      child_names = Enum.map(children, fn {id, _pid, _type, _modules} -> id end)
      assert :counter_1 in child_names
      assert :counter_2 in child_names
      assert :printer_1 in child_names

      # Cleanup
      :ok = Control.destroy_sandbox(sandbox_info.id)

      # Verify sandbox is cleaned up from manager
      {:error, :not_found} = Control.get_sandbox_info(sandbox_info.id)
    end

    test "can create multiple isolated sandboxes" do
      # Create multiple sandboxes
      {:ok, sandbox1} =
        Control.create_sandbox(OtpSandbox.TestDemoSupervisor, strategy: :one_for_one)

      {:ok, sandbox2} =
        Control.create_sandbox(OtpSandbox.TestDemoSupervisor, strategy: :one_for_all)

      # Verify they're separate
      assert sandbox1.id != sandbox2.id
      assert sandbox1.supervisor_pid != sandbox2.supervisor_pid

      # Verify they're listed
      sandboxes = Control.list_sandboxes()
      sandbox_ids = Enum.map(sandboxes, & &1.id)
      assert sandbox1.id in sandbox_ids
      assert sandbox2.id in sandbox_ids

      # Cleanup
      :ok = Control.destroy_sandbox(sandbox1.id)
      :ok = Control.destroy_sandbox(sandbox2.id)
    end

    test "can restart sandbox preserving configuration" do
      # Create sandbox
      {:ok, original_info} =
        Control.create_sandbox(OtpSandbox.TestDemoSupervisor, strategy: :one_for_one)

      original_supervisor_pid = original_info.supervisor_pid

      # Restart sandbox
      {:ok, restarted_info} = Control.restart_sandbox(original_info.id)

      # Verify restart worked
      assert restarted_info.id == original_info.id
      assert restarted_info.restart_count == 1
      assert restarted_info.supervisor_pid != original_supervisor_pid
      assert Process.alive?(restarted_info.supervisor_pid)

      # Verify children are still there
      children = Supervisor.which_children(restarted_info.supervisor_pid)
      assert length(children) == 3

      # Cleanup
      :ok = Control.destroy_sandbox(restarted_info.id)
    end
  end
end
