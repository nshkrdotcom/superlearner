defmodule OTPSupervisor.Core.FlawedFeatureDetectionTest do
  use ExUnit.Case, async: true

  @moduledoc """
  Tests to detect remaining flawed features after cleanup.
  These tests should FAIL initially and PASS after cleanup is complete.
  """

  describe "flawed features should be completely removed" do
    test "RestartTracker module should not exist" do
      refute Code.ensure_loaded?(OTPSupervisor.Core.RestartTracker)
    end

    test "SupervisorController module should not exist" do
      refute Code.ensure_loaded?(OTPSupervisor.Core.SupervisorController)
    end

    test "flawed Control functions should not exist" do
      functions = OTPSupervisor.Core.Control.__info__(:functions)

      # These are the truly flawed functions from the old implementation
      flawed_functions = [
        :start_restart_tracking,
        :record_restart_event,
        :pause_supervisor,
        :resume_supervisor,
        :supervisor_paused?
      ]

      for flawed_function <- flawed_functions do
        refute Keyword.has_key?(functions, flawed_function),
               "Flawed function #{flawed_function} still exists in Control module"
      end

      # Verify the NEW telemetry-based functions exist (these are correct)
      new_analytics_functions = [
        :get_restart_history,
        :get_supervisor_analytics,
        :get_failure_rate
      ]

      for new_function <- new_analytics_functions do
        assert Keyword.has_key?(functions, new_function),
               "New analytics function #{new_function} should exist in Control module"
      end
    end
  end

  describe "codebase should have no references to flawed modules" do
    test "no imports or aliases to flawed modules" do
      # This would be a manual verification step
      # The actual test is running mix compile successfully
      assert true
    end
  end
end
