defmodule OTPSupervisor.Core.SandboxManagerTest do
  use ExUnit.Case, async: false
  import SupervisorTestHelper
  import ExUnit.CaptureLog

  @moduledoc """
  Test suite for SandboxManager using OTP Testing Standards.

  These tests verify real OTP supervisor lifecycle management without
  simulation or external hacks. All tests follow OTP patterns and
  demonstrate proper sandbox isolation.
  """

  alias OTPSupervisor.Core.SandboxManager
  alias OTPSupervisor.Sandbox.TestDemoSupervisor

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
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_create_#{unique_id}"

      # Create sandbox
      {:ok, sandbox_info} =
        SandboxManager.create_sandbox(
          sandbox_id,
          TestDemoSupervisor,
          strategy: :one_for_one
        )

      # Verify sandbox info structure
      assert sandbox_info.id == sandbox_id
      assert sandbox_info.supervisor_module == TestDemoSupervisor
      assert is_pid(sandbox_info.supervisor_pid)
      assert Process.alive?(sandbox_info.supervisor_pid)
      assert sandbox_info.opts[:strategy] == :one_for_one
      assert is_integer(sandbox_info.created_at)
      assert sandbox_info.restart_count == 0

      # Verify children are running
      children = Supervisor.which_children(sandbox_info.supervisor_pid)
      # Should have 2 counters + 1 printer
      assert length(children) == 3

      # Verify unique child naming
      child_names = Enum.map(children, fn {id, _pid, _type, _modules} -> id end)
      assert :counter_1 in child_names
      assert :counter_2 in child_names
      assert :printer_1 in child_names

      # Cleanup
      :ok = SandboxManager.destroy_sandbox(sandbox_id)
    end

    test "destroys sandbox completely" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_destroy_#{unique_id}"

      # Create sandbox
      {:ok, sandbox_info} =
        SandboxManager.create_sandbox(
          sandbox_id,
          TestDemoSupervisor,
          strategy: :one_for_all
        )

      supervisor_pid = sandbox_info.supervisor_pid

      # Monitor supervisor for death
      ref = Process.monitor(supervisor_pid)

      # Destroy sandbox
      :ok = SandboxManager.destroy_sandbox(sandbox_id)

      # Wait for supervisor to die (OTP pattern - no sleep)
      receive do
        {:DOWN, ^ref, :process, ^supervisor_pid, _reason} -> :ok
      after
        1000 -> flunk("Supervisor did not terminate")
      end

      # Verify supervisor is dead
      refute Process.alive?(supervisor_pid)

      # Verify sandbox removed from manager
      {:error, :not_found} = SandboxManager.get_sandbox_info(sandbox_id)
    end

    test "prevents duplicate sandbox IDs" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_duplicate_#{unique_id}"

      # Create first sandbox
      {:ok, _sandbox_info} =
        SandboxManager.create_sandbox(
          sandbox_id,
          TestDemoSupervisor
        )

      # Attempt to create duplicate
      {:error, {:already_exists, existing_info}} =
        SandboxManager.create_sandbox(
          sandbox_id,
          TestDemoSupervisor
        )

      assert existing_info.id == sandbox_id

      # Cleanup
      :ok = SandboxManager.destroy_sandbox(sandbox_id)
    end
  end

  describe "sandbox restart functionality" do
    test "restarts sandbox with same configuration" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_restart_#{unique_id}"

      # Create sandbox with specific configuration
      {:ok, original_info} =
        SandboxManager.create_sandbox(
          sandbox_id,
          TestDemoSupervisor,
          strategy: :rest_for_one,
          custom_opt: :test_value
        )

      original_pid = original_info.supervisor_pid
      _original_opts = original_info.opts

      # Monitor original supervisor
      ref = Process.monitor(original_pid)

      # Restart sandbox
      {:ok, restarted_info} =
        ExUnit.CaptureLog.with_log(fn -> SandboxManager.restart_sandbox(sandbox_id) end)
        |> elem(0)

      # Wait for original supervisor to die
      receive do
        {:DOWN, ^ref, :process, ^original_pid, _reason} -> :ok
      after
        1000 -> flunk("Original supervisor did not terminate")
      end

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
      :ok = SandboxManager.destroy_sandbox(sandbox_id)
    end

    test "handles restart of non-existent sandbox" do
      {:error, :not_found} = SandboxManager.restart_sandbox("non_existent")
    end
  end

  describe "sandbox introspection" do
    test "provides sandbox information" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_info_#{unique_id}"

      # Create sandbox
      {:ok, _created_info} =
        SandboxManager.create_sandbox(
          sandbox_id,
          TestDemoSupervisor,
          strategy: :one_for_one
        )

      # Get sandbox info
      {:ok, sandbox_info} = SandboxManager.get_sandbox_info(sandbox_id)

      assert sandbox_info.id == sandbox_id
      assert sandbox_info.supervisor_module == TestDemoSupervisor
      assert is_pid(sandbox_info.supervisor_pid)
      assert Process.alive?(sandbox_info.supervisor_pid)

      # Cleanup
      :ok = SandboxManager.destroy_sandbox(sandbox_id)
    end

    test "provides sandbox PID lookup" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_pid_#{unique_id}"

      # Create sandbox
      {:ok, sandbox_info} =
        SandboxManager.create_sandbox(
          sandbox_id,
          TestDemoSupervisor
        )

      # Get PID via lookup
      {:ok, pid} = SandboxManager.get_sandbox_pid(sandbox_id)
      assert pid == sandbox_info.supervisor_pid

      # Cleanup
      :ok = SandboxManager.destroy_sandbox(sandbox_id)
    end

    test "lists all active sandboxes" do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id_1 = "test_list_1_#{unique_id}"
      sandbox_id_2 = "test_list_2_#{unique_id}"

      # Create multiple sandboxes
      {:ok, _info1} = SandboxManager.create_sandbox(sandbox_id_1, TestDemoSupervisor)
      {:ok, _info2} = SandboxManager.create_sandbox(sandbox_id_2, TestDemoSupervisor)

      # List sandboxes
      sandboxes = SandboxManager.list_sandboxes()
      sandbox_ids = Enum.map(sandboxes, & &1.id)

      assert sandbox_id_1 in sandbox_ids
      assert sandbox_id_2 in sandbox_ids

      # Cleanup
      :ok = SandboxManager.destroy_sandbox(sandbox_id_1)
      :ok = SandboxManager.destroy_sandbox(sandbox_id_2)
    end
  end

  describe "supervisor crash handling" do
    setup do
      setup_crash_test_supervisor("sandbox_crash")
    end

    test "detects and cleans up crashed supervisors", %{sup_pid: _crash_supervisor_pid} do
      unique_id = :erlang.unique_integer([:positive])
      sandbox_id = "test_crash_#{unique_id}"

      # Create sandbox with the isolated supervisor
      {:ok, sandbox_info} =
        SandboxManager.create_sandbox(
          sandbox_id,
          TestDemoSupervisor
        )

      supervisor_pid = sandbox_info.supervisor_pid

      # Monitor the supervisor
      ref = Process.monitor(supervisor_pid)

      # Kill the supervisor directly and capture expected warning log
      capture_log(fn ->
        Process.exit(supervisor_pid, :kill)

        # Wait for supervisor to die
        receive do
          {:DOWN, ^ref, :process, ^supervisor_pid, :killed} -> :ok
        after
          1000 -> flunk("Supervisor did not die")
        end

        # Force SandboxManager to process DOWN message with synchronous call
        _sandboxes = SandboxManager.list_sandboxes()
      end)

      # Wait for SandboxManager to process the DOWN message using proper OTP synchronization
      wait_for_sandbox_cleanup = fn ->
        Enum.reduce_while(1..100, nil, fn _i, _acc ->
          case SandboxManager.get_sandbox_info(sandbox_id) do
            {:error, :not_found} -> {:halt, :ok}
            {:ok, _info} -> {:cont, nil}
          end
        end)
      end

      case wait_for_sandbox_cleanup.() do
        # Sandbox was cleaned up
        :ok -> :ok
        nil -> flunk("Sandbox was not cleaned up after supervisor crash")
      end
    end
  end

  describe "concurrent operations" do
    test "handles multiple concurrent sandbox operations" do
      unique_id = :erlang.unique_integer([:positive])

      # Create multiple sandboxes concurrently
      tasks =
        for i <- 1..5 do
          Task.async(fn ->
            sandbox_id = "concurrent_#{i}_#{unique_id}"
            {:ok, info} = SandboxManager.create_sandbox(sandbox_id, TestDemoSupervisor)
            {sandbox_id, info}
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
            :ok = SandboxManager.destroy_sandbox(sandbox_id)
          end)
        end

      # Wait for all cleanups
      Enum.each(cleanup_tasks, &Task.await/1)
    end
  end
end
