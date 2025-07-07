defmodule OTPSupervisor.Sandbox.Workers.Printer do
  @moduledoc """
  A simple printer GenServer that logs messages.
  
  This worker logs messages using the Logger module and tracks
  how many messages it has printed. Useful for demonstrating
  GenServer message handling and process state.
  
  ## IEx Examples
  
      # Interacting with the pre-started printer
      iex> alias OTPSupervisor.Sandbox.Workers.Printer
      iex> Printer.print(:printer_1, "Hello from IEx!")
      :ok
      # [info] [Printer printer-1] Hello from IEx!
      
      iex> Printer.get_print_count(:printer_1)
      1
      
      # Demonstrating state persistence across messages
      iex> Printer.print(:printer_1, "Another message")
      :ok
      iex> Printer.print(:printer_1, "And one more")
      :ok
      iex> Printer.get_print_count(:printer_1)
      3
  """
  use GenServer
  require Logger

  # Client API

  @doc """
  Starts a printer process.
  
  ## Options
  
    * `:name` - The name to register the process under (defaults to module name)
    * `:id` - An identifier for this printer instance (used in log messages)
  
  ## Examples
  
      iex> {:ok, pid} = Printer.start_link(name: :my_printer, id: "printer_1")
      iex> Printer.print(:my_printer, "Hello, world!")
      :ok
  """
  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Prints a message to the log and increments the message counter.
  
  ## Examples
  
      iex> Printer.print(:my_printer, "Important message")
      :ok
  """
  def print(server, message) do
    GenServer.cast(server, {:print, message})
  end

  @doc """
  Returns the number of messages printed by this printer.
  
  ## Examples
  
      iex> Printer.get_print_count(:my_printer)
      42
  """
  def get_print_count(server) do
    GenServer.call(server, :get_print_count)
  end

  # Server Callbacks

  @impl true
  def init(opts) do
    id = Keyword.get(opts, :id, "default")
    state = %{
      id: id,
      print_count: 0
    }
    {:ok, state}
  end

  @impl true
  def handle_call(:get_print_count, _from, state) do
    {:reply, state.print_count, state}
  end

  @impl true
  def handle_cast({:print, message}, state) do
    Logger.info("[Printer #{state.id}] #{message}")
    new_state = %{state | print_count: state.print_count + 1}
    {:noreply, new_state}
  end
end