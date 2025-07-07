defmodule OTPSupervisor.Sandbox.Supervisors.DemoSupervisor do
  @moduledoc """
  A demonstration supervisor with different restart strategies.
  
  This supervisor manages three worker processes (two counters and one printer)
  and can be configured with different restart strategies to demonstrate
  OTP supervisor behavior.
  
  ## Restart Strategies
  
    * `:one_for_one` - Only restart the failed child process
    * `:one_for_all` - Restart all child processes if one fails
    * `:rest_for_one` - Restart the failed process and any processes started after it
  
  ## Examples
  
      # Start with default :one_for_one strategy
      {:ok, sup} = DemoSupervisor.start_link(name: :my_supervisor)
      
      # Start with :one_for_all strategy
      {:ok, sup} = DemoSupervisor.start_link(
        name: :my_supervisor,
        strategy: :one_for_all
      )
      
  ## IEx Experiments
  
      # Observe the pre-started supervisor
      iex> Supervisor.which_children(:demo_one_for_one)
      [
        {:printer_1, #PID<0.328.0>, :worker, [OTPSupervisor.Sandbox.Workers.Printer]},
        {:counter_2, #PID<0.327.0>, :worker, [OTPSupervisor.Sandbox.Workers.Counter]},
        {:counter_1, #PID<0.326.0>, :worker, [OTPSupervisor.Sandbox.Workers.Counter]}
      ]
      
      # Kill a child and watch it restart with a new PID
      iex> Process.exit(pid(:counter_1), :kill)
      iex> Process.sleep(100)
      iex> Supervisor.which_children(:demo_one_for_one)
      # Notice counter_1 has a new PID!
  """
  use Supervisor

  @doc """
  Starts the demo supervisor.
  
  ## Options
  
    * `:strategy` - The restart strategy to use (defaults to :one_for_one)
    * `:name` - The name to register the supervisor under (defaults to module name)
  
  ## Examples
  
      iex> {:ok, pid} = DemoSupervisor.start_link(name: :demo_sup)
      iex> Supervisor.count_children(:demo_sup)
      %{active: 3, specs: 3, supervisors: 0, workers: 3}
  """
  def start_link(opts \\ []) do
    strategy = Keyword.get(opts, :strategy, :one_for_one)
    name = Keyword.get(opts, :name, __MODULE__)
    Supervisor.start_link(__MODULE__, strategy, name: name)
  end

  @impl true
  def init(strategy) do
    children = [
      # First counter with initial value 0
      %{
        id: :counter_1,
        start: {OTPSupervisor.Sandbox.Workers.Counter, :start_link, 
                [[name: :counter_1, initial_value: 0]]}
      },
      # Second counter with initial value 100
      %{
        id: :counter_2,
        start: {OTPSupervisor.Sandbox.Workers.Counter, :start_link,
                [[name: :counter_2, initial_value: 100]]}
      },
      # Printer worker
      %{
        id: :printer_1,
        start: {OTPSupervisor.Sandbox.Workers.Printer, :start_link,
                [[name: :printer_1, id: "printer-1"]]}
      }
    ]

    # Configure supervisor with the specified strategy
    Supervisor.init(children, strategy: strategy)
  end
end