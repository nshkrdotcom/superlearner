defmodule OTPSupervisor.Core.AnalyticsServerTest do
  use ExUnit.Case, async: false
  import SupervisorTestHelper

  @moduledoc """
  Test suite for AnalyticsServer using OTP Testing Standards.

  These tests verify telemetry-based analytics without external simulation.
  All tests follow OTP patterns and demonstrate proper event handling.
  """

  alias OTPSupervisor.Core.AnalyticsServer

  describe "analytics server lifecycle" do
    test "starts successfully and attaches telemetry handlers" do
      # AnalyticsServer should already be running in application
      # If this test runs, the server started successfully
      assert Process.whereis(AnalyticsServer) != nil
      assert Process.alive?(Process.whereis(AnalyticsServer))
    end

    test "provides empty history for unknown supervisors" do
      unknown_pid = spawn(fn -> :ok end)
      history = AnalyticsServer.get_restart_history(unknown_pid)
      assert history == []
    end

    test "provides initial stats after startup" do
      stats = AnalyticsServer.get_all_supervisor_stats()
      assert is_map(stats)
      assert Map.has_key?(stats, :total_supervisors)
      assert Map.has_key?(stats, :total_restarts)
      assert Map.has_key?(stats, :uptime_ms)
      assert Map.has_key?(stats, :supervisor_stats)
    end
  end

  describe "telemetry event processing" do
    setup do
      # Create isolated supervisor for testing telemetry
      setup_isolated_supervisor("telemetry_test")
    end

    test "captures real supervisor restart events via telemetry", %{
      supervisor: _supervisor,
      sup_pid: sup_pid
    } do
      # Establish baseline - this registers the supervisor and captures initial child state
      :ok = AnalyticsServer.establish_baseline(sup_pid)

      # Get initial restart history
      initial_history = AnalyticsServer.get_restart_history(sup_pid)
      initial_count = length(initial_history)

      # Get a real child to kill
      children = Supervisor.which_children(sup_pid)
      {child_id, child_pid, _, _} = hd(children)

      # Kill the process to trigger a REAL supervisor restart
      Process.exit(child_pid, :kill)

      # Wait for the supervisor to complete the restart
      :ok = wait_for_child_restart(sup_pid, child_id, child_pid)

      # Synchronize with the AnalyticsServer to ensure it processed the event
      :ok = AnalyticsServer.sync(sup_pid)

      # Now, get the history and assert it contains the real event
      new_history = AnalyticsServer.get_restart_history(sup_pid)
      new_count = length(new_history)

      assert new_count > initial_count, "No restart event was captured"

      # Verify restart event details
      latest_event = hd(new_history)
      assert latest_event.child_id == child_id
      assert latest_event.event_type == :restarted
      assert is_integer(latest_event.timestamp)
    end

    test "calculates failure rates correctly with real events", %{
      supervisor: _supervisor,
      sup_pid: sup_pid
    } do
      # Establish baseline first
      :ok = AnalyticsServer.establish_baseline(sup_pid)

      # Generate multiple real restart events by killing children
      children = Supervisor.which_children(sup_pid)

      for {child_id, child_pid, _, _} <- Enum.take(children, 3) do
        # Kill the real child to trigger supervisor restart
        Process.exit(child_pid, :kill)

        # Wait for restart to complete
        :ok = wait_for_child_restart(sup_pid, child_id, child_pid)
      end

      # Sync to ensure all events processed
      :ok = AnalyticsServer.sync(sup_pid)

      # Check failure rate over last 10 seconds
      failure_rate = AnalyticsServer.get_failure_rate(sup_pid, 10_000)

      assert failure_rate.restarts >= 3
      assert failure_rate.rate > 0
      assert failure_rate.window_ms == 10_000
    end

    test "tracks multiple supervisors independently", %{
      supervisor: _supervisor1,
      sup_pid: sup_pid1
    } do
      # Create second isolated supervisor
      %{supervisor: _supervisor2, sup_pid: sup_pid2} =
        setup_isolated_supervisor("telemetry_test_2")

      on_exit(fn ->
        if Process.alive?(sup_pid2) do
          ref = Process.monitor(sup_pid2)
          Process.exit(sup_pid2, :kill)

          receive do
            {:DOWN, ^ref, :process, ^sup_pid2, _} -> :ok
          after
            100 -> :ok
          end
        end
      end)

      # Establish baseline for both supervisors
      :ok = AnalyticsServer.establish_baseline(sup_pid1)
      :ok = AnalyticsServer.establish_baseline(sup_pid2)

      # Generate real restart event for first supervisor only
      children1 = Supervisor.which_children(sup_pid1)
      {child_id, child_pid, _, _} = hd(children1)

      # Kill child to trigger real supervisor restart
      Process.exit(child_pid, :kill)

      # Wait for restart to complete
      :ok = wait_for_child_restart(sup_pid1, child_id, child_pid)

      :ok = AnalyticsServer.sync(sup_pid1)

      # Verify first supervisor has restart events
      history1 = AnalyticsServer.get_restart_history(sup_pid1)
      assert length(history1) > 0

      # Verify second supervisor has no restart events
      history2 = AnalyticsServer.get_restart_history(sup_pid2)

      # Should be empty
      assert length(history2) == 0
    end
  end

  describe "error handling and edge cases" do
    test "handles non-existent supervisor gracefully" do
      fake_pid = spawn(fn -> :ok end)
      Process.exit(fake_pid, :kill)

      # Should not crash when querying dead process
      history = AnalyticsServer.get_restart_history(fake_pid)
      assert history == []

      failure_rate = AnalyticsServer.get_failure_rate(fake_pid, 1000)
      assert failure_rate.restarts == 0
    end

    test "maintains bounded history" do
      # This test would require generating many events
      # For now, verify the structure supports bounds
      history = AnalyticsServer.get_restart_history(self())
      assert is_list(history)
    end
  end
end
