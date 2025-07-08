defmodule OTPSupervisor.Sandbox.TestDemoSupervisor do
  @moduledoc """
  Production-grade demo supervisor with unique naming for sandbox isolation.

  This supervisor provides proper OTP patterns for dynamic supervisor
  creation with unique child naming to prevent conflicts between multiple
  instances running concurrently.
  """

  use Supervisor

  def start_link(opts) do
    strategy = Keyword.get(opts, :strategy, :one_for_one)
    unique_id = Keyword.get(opts, :unique_id, :erlang.unique_integer([:positive]))
    name = Keyword.get(opts, :name, :"test_demo_supervisor_#{unique_id}")

    Supervisor.start_link(__MODULE__, {strategy, unique_id}, name: name)
  end

  @impl true
  def init({strategy, unique_id}) do
    children = [
      %{
        id: :counter_1,
        start:
          {OTPSupervisor.Sandbox.Workers.Counter, :start_link,
           [[name: :"counter_1_#{unique_id}", initial_value: 0]]}
      },
      %{
        id: :counter_2,
        start:
          {OTPSupervisor.Sandbox.Workers.Counter, :start_link,
           [[name: :"counter_2_#{unique_id}", initial_value: 100]]}
      },
      %{
        id: :printer_1,
        start:
          {OTPSupervisor.Sandbox.Workers.Printer, :start_link,
           [[name: :"printer_1_#{unique_id}", id: "printer-#{unique_id}"]]}
      }
    ]

    Supervisor.init(children, strategy: strategy)
  end
end
