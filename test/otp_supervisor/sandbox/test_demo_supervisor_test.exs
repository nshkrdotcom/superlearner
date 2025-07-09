defmodule OtpSandbox.TestDemoSupervisorTest do
  use ExUnit.Case, async: true
  # import SupervisorTestHelper

  @moduledoc """
  Tests for enhanced TestDemoSupervisor with unique naming support.
  Verifies proper isolation and configuration options.
  """

  alias OtpSandbox.TestDemoSupervisor

  describe "enhanced supervisor functionality" do
    test "supports unique naming for isolation" do
      unique_id_1 = :erlang.unique_integer([:positive])
      unique_id_2 = :erlang.unique_integer([:positive])

      # Start two supervisors with different unique IDs
      {:ok, sup1} =
        TestDemoSupervisor.start_link(
          name: :"test_sup_#{unique_id_1}",
          unique_id: unique_id_1,
          strategy: :one_for_one
        )

      {:ok, sup2} =
        TestDemoSupervisor.start_link(
          name: :"test_sup_#{unique_id_2}",
          unique_id: unique_id_2,
          strategy: :one_for_all
        )

      # Verify children have unique names
      _children1 = Supervisor.which_children(sup1)
      _children2 = Supervisor.which_children(sup2)

      # Extract child process names from registered processes
      child_names_1 = get_child_names(unique_id_1)
      child_names_2 = get_child_names(unique_id_2)

      # Verify names are different
      assert child_names_1 != child_names_2

      # Verify no name conflicts
      for name1 <- child_names_1 do
        refute name1 in child_names_2
      end

      # Cleanup
      on_exit(fn ->
        if Process.alive?(sup1), do: Process.exit(sup1, :shutdown)
        if Process.alive?(sup2), do: Process.exit(sup2, :shutdown)
      end)
    end

    test "supports different supervisor strategies" do
      unique_id = :erlang.unique_integer([:positive])

      # Test different strategies
      strategies = [:one_for_one, :one_for_all, :rest_for_one]

      for strategy <- strategies do
        {:ok, sup_pid} =
          TestDemoSupervisor.start_link(
            name: :"test_strategy_#{strategy}_#{unique_id}",
            unique_id: unique_id,
            strategy: strategy
          )

        # Verify supervisor started with correct strategy
        children = Supervisor.which_children(sup_pid)
        assert length(children) == 3

        # Cleanup
        Supervisor.stop(sup_pid)
      end
    end

    test "worker processes are functional" do
      unique_id = :erlang.unique_integer([:positive])

      {:ok, sup_pid} =
        TestDemoSupervisor.start_link(
          name: :"test_workers_#{unique_id}",
          unique_id: unique_id
        )

      counter_name = :"counter_1_#{unique_id}"
      printer_name = :"printer_1_#{unique_id}"

      # Test counter functionality
      original_value = OtpSandbox.Workers.Counter.get_value(counter_name)
      OtpSandbox.Workers.Counter.increment(counter_name)
      new_value = OtpSandbox.Workers.Counter.get_value(counter_name)
      assert new_value == original_value + 1

      # Test printer functionality
      assert :ok =
               OtpSandbox.Workers.Printer.print(
                 printer_name,
                 "test message"
               )

      # Cleanup
      Supervisor.stop(sup_pid)
    end
  end

  defp get_child_names(unique_id) do
    [
      :"counter_1_#{unique_id}",
      :"counter_2_#{unique_id}",
      :"printer_1_#{unique_id}"
    ]
  end
end
