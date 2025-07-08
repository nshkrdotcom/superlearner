defmodule OTPSupervisor.Core.AnalyticsIntegrationTest do
  # Telemetry is global
  use ExUnit.Case, async: false
  import SupervisorTestHelper

  @moduledoc """
  Integration tests for analytics with real OTP supervisors.
  Tests the full flow from supervisor events to analytics data.
  """

  alias OTPSupervisor.Core.{AnalyticsServer, Control}

  describe "end-to-end analytics flow" do
    setup do
      setup_isolated_supervisor("integration_test")
    end

    test "complete restart tracking flow", %{supervisor: supervisor, sup_pid: sup_pid} do
      # 1. Establish baseline for supervisor monitoring
      :ok = AnalyticsServer.establish_baseline(sup_pid)

      # 2. Verify initial state
      {:ok, initial_history} = Control.get_restart_history(supervisor)
      initial_count = length(initial_history)

      # 3. Trigger a REAL supervisor restart by killing a child
      children = Supervisor.which_children(sup_pid)
      {child_id, child_pid, _, _} = hd(children)

      # Kill the child to trigger supervisor restart
      Process.exit(child_pid, :kill)

      # Wait for restart to complete
      :ok = wait_for_child_restart(sup_pid, child_id, child_pid)

      # 4. Verify analytics captured the real event
      :ok = AnalyticsServer.sync(sup_pid)
      {:ok, final_history} = Control.get_restart_history(supervisor)

      assert length(final_history) > initial_count

      # 5. Verify global stats updated
      stats = Control.get_supervisor_analytics()
      assert stats.total_restarts > 0
    end

    test "analytics work with demo supervisor", %{} do
      # Test with the real demo supervisor
      {:ok, demo_history} = Control.get_restart_history(:demo_one_for_one)
      assert is_list(demo_history)

      # Should be able to get failure rate
      {:ok, failure_rate} = Control.get_failure_rate(:demo_one_for_one, 10_000)
      assert is_map(failure_rate)
    end
  end

  describe "performance characteristics" do
    setup do
      setup_isolated_supervisor("perf_test")
    end

    test "supervisor scanning is efficient", %{sup_pid: sup_pid} do
      # Register supervisor
      :ok = AnalyticsServer.register_supervisor(sup_pid)

      # Measure scan time
      start_time = System.monotonic_time(:microsecond)

      # Force scan 
      :ok = AnalyticsServer.sync(sup_pid)

      end_time = System.monotonic_time(:microsecond)
      duration_us = end_time - start_time

      # Should complete quickly
      assert duration_us < 10_000, "Supervisor scanning too slow: #{duration_us}μs"
    end
  end
end
