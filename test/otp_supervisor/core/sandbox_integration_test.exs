defmodule OTPSupervisor.Core.SandboxIntegrationTest do
  use ExUnit.Case, async: true
  # import SupervisorTestHelper
  import ExUnit.CaptureLog

  @moduledoc """
  Integration tests for sandbox management with analytics and control systems.
  Tests the full integration between SandboxManager, AnalyticsServer, and Control.
  """

  alias OTPSupervisor.Core.Control
  alias OtpSandbox.TestDemoSupervisor

  describe "end-to-end sandbox lifecycle with analytics" do
    test "sandbox operations integrate with analytics system" do
      # Create sandbox via Control module
      {:ok, sandbox_info} = Control.create_sandbox(TestDemoSupervisor, strategy: :one_for_one)
      sandbox_pid = sandbox_info.supervisor_pid

      # Verify analytics server can handle supervisor queries
      {:ok, initial_history} = Control.get_restart_history(sandbox_pid)
      assert is_list(initial_history)

      # Verify analytics server provides global stats
      stats = Control.get_supervisor_analytics()
      assert is_map(stats)
      assert Map.has_key?(stats, :total_supervisors)
      assert Map.has_key?(stats, :total_restarts)

      # Verify failure rate calculation works
      {:ok, rate} = Control.get_failure_rate(sandbox_pid, 5000)
      assert is_map(rate)
      assert Map.has_key?(rate, :restarts)
      assert Map.has_key?(rate, :rate)

      # Cleanup - suppress expected supervisor death warning
      capture_log(fn -> :ok = Control.destroy_sandbox(sandbox_info.id) end)
    end

    test "multiple sandboxes work independently" do
      # Create multiple sandboxes with different strategies
      {:ok, sandbox1} = Control.create_sandbox(TestDemoSupervisor, strategy: :one_for_one)
      {:ok, sandbox2} = Control.create_sandbox(TestDemoSupervisor, strategy: :one_for_all)

      # Verify they're listed
      sandboxes = Control.list_sandboxes()
      sandbox_ids = Enum.map(sandboxes, & &1.id)
      assert sandbox1.id in sandbox_ids
      assert sandbox2.id in sandbox_ids

      # Verify they have different PIDs
      assert sandbox1.supervisor_pid != sandbox2.supervisor_pid

      # Verify both are functional
      children1 = Supervisor.which_children(sandbox1.supervisor_pid)
      children2 = Supervisor.which_children(sandbox2.supervisor_pid)
      assert length(children1) == 3
      assert length(children2) == 3

      # Cleanup - suppress expected supervisor death warnings
      capture_log(fn ->
        :ok = Control.destroy_sandbox(sandbox1.id)
        :ok = Control.destroy_sandbox(sandbox2.id)
      end)
    end
  end

  describe "sandbox resilience and error handling" do
    test "survives sandbox supervisor crashes" do
      # Create sandbox
      {:ok, sandbox_info} = Control.create_sandbox(TestDemoSupervisor)
      supervisor_pid = sandbox_info.supervisor_pid
      sandbox_id = sandbox_info.id

      # Kill the sandbox supervisor directly and capture expected warning log
      ref = Process.monitor(supervisor_pid)

      capture_log(fn ->
        Process.exit(supervisor_pid, :kill)

        # Wait for death
        receive do
          {:DOWN, ^ref, :process, ^supervisor_pid, :killed} -> :ok
        after
          1000 -> flunk("Supervisor did not die")
        end

        # Sync with Control to ensure SandboxManager processes DOWN message
        _sandboxes = Control.list_sandboxes()
      end)

      # Wait for SandboxManager to process the DOWN message using proper OTP synchronization
      wait_for_sandbox_cleanup = fn ->
        Enum.reduce_while(1..100, nil, fn _i, _acc ->
          case Control.get_sandbox_info(sandbox_id) do
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

      # Verify SandboxManager is still functional
      {:ok, new_sandbox} = Control.create_sandbox(TestDemoSupervisor)
      assert Process.alive?(new_sandbox.supervisor_pid)

      # Cleanup
      capture_log(fn -> :ok = Control.destroy_sandbox(new_sandbox.id) end)
    end
  end
end
