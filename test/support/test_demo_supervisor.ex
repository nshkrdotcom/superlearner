defmodule TestDemoSupervisor do
  @moduledoc """
  A test-only version of DemoSupervisor that creates children with unique names
  to avoid conflicts between tests.

  Unlike the main DemoSupervisor which uses global names like :counter_1, :counter_2,
  this version generates unique names for each instance to ensure proper test isolation.
  """
  use Supervisor

  alias OtpSandbox.Workers.Counter
  alias OtpSandbox.Workers.Printer

  def start_link(opts \\ []) do
    strategy = Keyword.get(opts, :strategy, :one_for_one)
    name = Keyword.get(opts, :name, __MODULE__)
    unique_id = Keyword.get(opts, :unique_id, :erlang.unique_integer([:positive]))

    Supervisor.start_link(__MODULE__, {strategy, unique_id}, name: name)
  end

  @impl true
  def init({strategy, unique_id}) do
    # Create unique child names to avoid conflicts
    counter_1_name = :"counter_1_#{unique_id}"
    counter_2_name = :"counter_2_#{unique_id}"
    printer_1_name = :"printer_1_#{unique_id}"

    children = [
      # First counter with initial value 0
      %{
        id: :counter_1,
        start: {Counter, :start_link, [[name: counter_1_name, initial_value: 0]]}
      },
      # Second counter with initial value 100
      %{
        id: :counter_2,
        start: {Counter, :start_link, [[name: counter_2_name, initial_value: 100]]}
      },
      # Printer worker
      %{
        id: :printer_1,
        start: {Printer, :start_link, [[name: printer_1_name, id: "printer-#{unique_id}"]]}
      }
    ]

    # Configure supervisor with the specified strategy
    Supervisor.init(children, strategy: strategy)
  end
end
