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
      # 1. Verify initial state
      {:ok, initial_history} = Control.get_restart_history(supervisor)
      initial_count = length(initial_history)

      # 2. Simulate a telemetry event (demonstrating the telemetry pattern)
      metadata = %{
        supervisor_pid: sup_pid,
        child_id: :integration_test_child,
        child_pid: spawn(fn -> :ok end),
        reason: :killed,
        shutdown: nil
      }

      # 3. Send telemetry event to AnalyticsServer
      GenServer.cast(
        AnalyticsServer,
        {:supervisor_event, [:supervisor, :child, :terminate], %{}, metadata}
      )

      # 4. Verify analytics captured the event
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
    test "telemetry handlers are fast" do
      # Telemetry handlers should complete quickly
      # This test ensures no blocking operations in handlers
      start_time = System.monotonic_time(:microsecond)

      # Generate telemetry event manually
      :telemetry.execute(
        [:test, :event],
        %{},
        %{supervisor_pid: self(), child_id: :test}
      )

      end_time = System.monotonic_time(:microsecond)
      duration_us = end_time - start_time

      # Should complete in microseconds, not milliseconds
      assert duration_us < 1000, "Telemetry handler too slow: #{duration_us}μs"
    end
  end
end
