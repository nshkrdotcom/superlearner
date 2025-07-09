defmodule OtpSandbox.Workers.Counter do
  @moduledoc """
  A simple counter GenServer for demonstration.

  This worker maintains a numeric counter value that can be incremented
  and retrieved. It also includes a crash function for demonstrating
  supervisor restart behavior.

  ## IEx Examples

      # Interacting with the pre-started counter
      iex> alias OtpSandbox.Workers.Counter
      iex> Counter.get_value(:counter_1)
      0
      iex> Counter.increment(:counter_1)
      :ok
      iex> Counter.get_value(:counter_1)
      1
      
      # Demonstrating crash and restart
      iex> pid_before = Process.whereis(:counter_1)
      iex> Counter.crash(:counter_1)
      :ok
      iex> Process.sleep(100)
      iex> pid_after = Process.whereis(:counter_1)
      iex> pid_before != pid_after
      true
      iex> Counter.get_value(:counter_1)  # State resets after crash
      0
  """
  use GenServer

  # Client API

  @doc """
  Starts a counter process.

  ## Options

    * `:name` - The name to register the process under (defaults to module name)
    * `:initial_value` - The initial counter value (defaults to 0)

  ## Examples

      iex> {:ok, pid} = Counter.start_link(name: :my_counter, initial_value: 10)
      iex> Counter.get_value(:my_counter)
      10
  """
  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    initial = Keyword.get(opts, :initial_value, 0)
    GenServer.start_link(__MODULE__, initial, name: name)
  end

  @doc """
  Increments the counter value by 1.

  ## Examples

      iex> Counter.increment(:my_counter)
      :ok
  """
  def increment(server) do
    GenServer.cast(server, :increment)
  end

  @doc """
  Returns the current counter value.

  ## Examples

      iex> Counter.get_value(:my_counter)
      42
  """
  def get_value(server) do
    GenServer.call(server, :get_value)
  end

  @doc """
  Intentionally crashes the process for demonstration purposes.

  This is useful for demonstrating supervisor restart strategies.

  ## Examples

      iex> Counter.crash(:my_counter)
      :ok
  """
  def crash(server) do
    GenServer.cast(server, :crash)
  end

  # Server Callbacks

  @impl true
  def init(initial_value) do
    {:ok, %{value: initial_value, crashes: 0}}
  end

  @impl true
  def handle_call(:get_value, _from, state) do
    {:reply, state.value, state}
  end

  @impl true
  def handle_cast(:increment, state) do
    {:noreply, %{state | value: state.value + 1}}
  end

  @impl true
  def handle_cast(:crash, _state) do
    # Simulate a crash
    raise "Intentional crash for demonstration"
  end

  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
