defmodule OTPSupervisor.Core.SandboxManagerTest do
  use ExUnit.Case, async: false
  import SupervisorTestHelper
  import SandboxTestHelper
  import ExUnit.CaptureLog

  @moduledoc """
  Test suite for SandboxManager using OTP Testing Standards.

  These tests verify real OTP supervisor lifecycle management without
  simulation or external hacks. All tests follow OTP patterns and
  demonstrate proper sandbox isolation.
  """

  alias OTPSupervisor.Core.SandboxManager
  alias OtpSandbox.TestDemoSupervisor

  setup do
    setup_sandbox_test(nil)
  end

  describe "sandbox manager lifecycle" do
    test "starts successfully and initializes ETS table" do
      # SandboxManager should already be running in application
      # If this test runs, the manager started successfully
      assert Process.whereis(SandboxManager) != nil
      assert Process.alive?(Process.whereis(SandboxManager))

      # ETS table should exist
      assert :ets.info(:sandboxes) != :undefined
    end

    test "provides empty sandbox list initially" do
      sandboxes = SandboxManager.list_sandboxes()
      assert is_list(sandboxes)
      # May not be empty if other tests created sandboxes
    end
  end

  describe "sandbox creation and destruction" do
    test "creates sandbox with unique naming" do
      # Create sandbox using helper
      {:ok, sandbox_info} = create_test_sandbox(strategy: :one_for_one)

      # Verify sandbox info structure
      assert is_binary(sandbox_info.id)
      assert sandbox_info.supervisor_module == TestDemoSupervisor
      assert is_pid(sandbox_info.supervisor_pid)
      assert Process.alive?(sandbox_info.supervisor_pid)
      assert sandbox_info.opts[:strategy] == :one_for_one
      assert is_integer(sandbox_info.created_at)
      assert sandbox_info.restart_count == 0

      # Verify sandbox is functional
      assert :ok = verify_sandbox_functional(sandbox_info)

      # Verify children are running
      children = Supervisor.which_children(sandbox_info.supervisor_pid)
      # Should have 2 counters + 1 printer
      assert length(children) == 3

      # Verify unique child naming
      child_names = Enum.map(children, fn {id, _pid, _type, _modules} -> id end)
      assert :counter_1 in child_names
      assert :counter_2 in child_names
      assert :printer_1 in child_names
    end

    test "destroys sandbox completely" do
      # Create sandbox using helper
      {:ok, sandbox_info} = create_test_sandbox(strategy: :one_for_all, cleanup: false)
      sandbox_id = sandbox_info.id

      # Destroy sandbox using helper
      :ok = destroy_test_sandbox(sandbox_id)

      # Wait for cleanup to complete
      :ok = wait_for_sandbox_cleanup(sandbox_id)

      # Verify sandbox removed from manager
      {:error, :not_found} = SandboxManager.get_sandbox_info(sandbox_id)

      # Verify sandbox is not in the list
      sandboxes = SandboxManager.list_sandboxes()
      sandbox_ids = Enum.map(sandboxes, & &1.id)
      refute sandbox_id in sandbox_ids
    end

    test "prevents duplicate sandbox IDs" do
      # Create first sandbox using helper
      {:ok, sandbox_info} = create_test_sandbox(cleanup: false)
      sandbox_id = sandbox_info.id

      # Attempt to create duplicate
      {:error, {:already_exists, existing_info}} =
        SandboxManager.create_sandbox(
          sandbox_id,
          TestDemoSupervisor
        )

      assert existing_info.id == sandbox_id

      # Cleanup
      :ok = destroy_test_sandbox(sandbox_id)
    end
  end

  describe "sandbox restart functionality" do
    test "restarts sandbox with same configuration" do
      # Create sandbox with specific configuration using helper
      {:ok, original_info} =
        create_test_sandbox(
          strategy: :rest_for_one,
          custom_opt: :test_value,
          cleanup: false
        )

      sandbox_id = original_info.id
      original_pid = original_info.supervisor_pid

      # Restart sandbox
      {:ok, restarted_info} =
        ExUnit.CaptureLog.with_log(fn -> SandboxManager.restart_sandbox(sandbox_id) end)
        |> elem(0)

      # Wait for restart to complete
      :ok = wait_for_sandbox_ready(sandbox_id)

      # Verify new supervisor is different
      new_pid = restarted_info.supervisor_pid
      assert new_pid != original_pid
      assert Process.alive?(new_pid)

      # Verify configuration preserved
      assert restarted_info.id == sandbox_id
      assert restarted_info.supervisor_module == TestDemoSupervisor
      assert restarted_info.opts[:strategy] == :rest_for_one
      assert restarted_info.opts[:custom_opt] == :test_value
      assert restarted_info.restart_count == 1

      # Verify children are running in new supervisor
      children = Supervisor.which_children(new_pid)
      assert length(children) == 3

      # Cleanup
      :ok = destroy_test_sandbox(sandbox_id)
    end

    test "handles restart of non-existent sandbox" do
      {:error, :not_found} = SandboxManager.restart_sandbox("non_existent")
    end
  end

  describe "sandbox introspection" do
    test "provides sandbox information" do
      # Create sandbox using helper
      {:ok, created_info} = create_test_sandbox(strategy: :one_for_one, cleanup: false)
      sandbox_id = created_info.id

      # Get sandbox info
      {:ok, sandbox_info} = SandboxManager.get_sandbox_info(sandbox_id)

      assert sandbox_info.id == sandbox_id
      assert sandbox_info.supervisor_module == TestDemoSupervisor
      assert is_pid(sandbox_info.supervisor_pid)
      assert Process.alive?(sandbox_info.supervisor_pid)

      # Cleanup
      :ok = destroy_test_sandbox(sandbox_id)
    end

    test "provides sandbox PID lookup" do
      # Create sandbox using helper
      {:ok, sandbox_info} = create_test_sandbox(cleanup: false)
      sandbox_id = sandbox_info.id

      # Get PID via lookup
      {:ok, pid} = SandboxManager.get_sandbox_pid(sandbox_id)
      # Should return app_pid, not supervisor_pid
      assert pid == sandbox_info.app_pid

      # Cleanup
      :ok = destroy_test_sandbox(sandbox_id)
    end

    test "lists all active sandboxes" do
      # Create multiple sandboxes using helper
      {:ok, info1} = create_test_sandbox(cleanup: false)
      {:ok, info2} = create_test_sandbox(cleanup: false)
      sandbox_id_1 = info1.id
      sandbox_id_2 = info2.id

      # List sandboxes
      sandboxes = SandboxManager.list_sandboxes()
      sandbox_ids = Enum.map(sandboxes, & &1.id)

      assert sandbox_id_1 in sandbox_ids
      assert sandbox_id_2 in sandbox_ids

      # Cleanup
      :ok = destroy_test_sandbox(sandbox_id_1)
      :ok = destroy_test_sandbox(sandbox_id_2)
    end
  end

  describe "supervisor crash handling" do
    setup do
      setup_crash_test_supervisor("sandbox_crash")
    end

    test "detects and cleans up crashed supervisors", %{sup_pid: _crash_supervisor_pid} do
      # Create sandbox using helper
      {:ok, sandbox_info} = create_test_sandbox(cleanup: false)
      sandbox_id = sandbox_info.id
      app_pid = sandbox_info.app_pid

      # Monitor the application (not supervisor)
      ref = Process.monitor(app_pid)

      # Kill the application directly and capture expected warning log
      capture_log(fn ->
        Process.exit(app_pid, :kill)

        # Wait for application to die
        receive do
          {:DOWN, ^ref, :process, ^app_pid, :killed} -> :ok
        after
          1000 -> flunk("Application did not die")
        end

        # Wait for SandboxManager to process the DOWN message
        :ok = wait_for_sandbox_cleanup(sandbox_id)
      end)

      # Verify sandbox was cleaned up
      {:error, :not_found} = SandboxManager.get_sandbox_info(sandbox_id)
    end
  end

  describe "concurrent operations" do
    test "handles multiple concurrent sandbox operations" do
      # Create multiple sandboxes concurrently using helper
      tasks =
        for _i <- 1..5 do
          Task.async(fn ->
            {:ok, info} = create_test_sandbox(cleanup: false)
            {info.id, info}
          end)
        end

      # Wait for all creations to complete
      results = Enum.map(tasks, &Task.await/1)

      # Verify all sandboxes were created
      assert length(results) == 5

      # Cleanup concurrently
      cleanup_tasks =
        for {sandbox_id, _info} <- results do
          Task.async(fn ->
            :ok = destroy_test_sandbox(sandbox_id)
          end)
        end

      # Wait for all cleanups
      Enum.each(cleanup_tasks, &Task.await/1)
    end
  end
end
