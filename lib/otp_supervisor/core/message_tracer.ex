defmodule OTPSupervisor.Core.MessageTracer do
  @moduledoc """
  Provides message tracing capabilities for debugging message flow in OTP systems.

  This module enables comprehensive message tracing for GenServer processes, allowing
  developers to understand message patterns, debug communication issues, and analyze
  system behavior. It provides both real-time tracing and historical analysis capabilities.

  ## Educational Value

  The MessageTracer demonstrates several important OTP concepts:
  - Process monitoring and tracing using Erlang's trace facility
  - GenServer behavior for managing tracer state
  - Registry usage for process tracking
  - Safe process introspection patterns
  - Message pattern analysis for debugging

  ## Safety Considerations

  - Uses Erlang's built-in tracing which has minimal performance impact
  - Automatically limits message history to prevent memory issues
  - Provides graceful cleanup when processes terminate
  - Safe to use in development and testing environments

  ## Usage Examples

      # Start tracing a GenServer
      {:ok, tracer_pid} = MessageTracer.trace_messages(my_genserver_pid)
      
      # Generate some activity
      GenServer.cast(my_genserver_pid, :some_message)
      GenServer.call(my_genserver_pid, :get_state)
      
      # Retrieve message history
      messages = MessageTracer.get_message_history(my_genserver_pid)
      
      # Analyze patterns
      patterns = MessageTracer.analyze_message_patterns(messages)
      
      # Stop tracing
      MessageTracer.stop_tracing(my_genserver_pid)
  """

  use GenServer
  require Logger

  @doc """
  Starts tracing messages for a specific process.

  Creates a tracer process that monitors message flow to and from the target process.
  The tracer automatically registers itself and begins collecting message information.

  ## Parameters

    * `pid` - The PID of the process to trace
    * `opts` - Options for configuring the tracer
      * `:max_messages` - Maximum number of messages to store (default: 100)
      * `:duration` - Automatic stop duration in seconds (default: :infinity)

  ## Returns

    * `{:ok, tracer_pid}` - Successfully started tracer
    * `{:error, reason}` - Failed to start tracer

  ## Educational Notes

  This function demonstrates:
  - GenServer process creation patterns
  - Erlang tracing API usage
  - Process registration with Registry
  - Error handling for invalid processes
  """
  def trace_messages(pid, opts \\ []) when is_pid(pid) do
    max_messages = Keyword.get(opts, :max_messages, 100)
    duration = Keyword.get(opts, :duration, :infinity)

    # Check if process is alive before starting tracer
    if Process.alive?(pid) do
      case GenServer.start(__MODULE__, {pid, max_messages, duration}) do
        {:ok, tracer_pid} ->
          try do
            # Start tracing the target process using proper Erlang trace API
            # Format: erlang:trace(PidPortSpec, How, FlagList)
            1 = :erlang.trace(pid, true, [:send, :receive, {:tracer, tracer_pid}])
            {:ok, tracer_pid}
          rescue
            error ->
              GenServer.stop(tracer_pid)
              {:error, {:trace_failed, error}}
          end

        {:error, {:already_traced, _}} ->
          {:error, :already_traced}

        error ->
          error
      end
    else
      {:error, :process_not_alive}
    end
  end

  @doc """
  Retrieves the message history for a traced process.

  Returns all collected messages for the specified process, or an empty list
  if the process is not being traced.

  ## Parameters

    * `pid` - The PID of the traced process

  ## Returns

  List of message maps, each containing:
    * `:timestamp` - When the message was captured (milliseconds)
    * `:direction` - `:incoming` or `:outgoing`
    * `:content` - The actual message content
    * `:to` - Destination PID (for outgoing messages)

  ## Educational Notes

  This function shows:
  - Registry lookup patterns
  - GenServer call/response handling
  - Safe process communication
  """
  def get_message_history(pid) do
    case Registry.lookup(TracerRegistry, pid) do
      [{tracer_pid, _}] ->
        try do
          GenServer.call(tracer_pid, :get_messages, 1000)
        catch
          :exit, _ -> []
        end

      [] ->
        []
    end
  end

  @doc """
  Stops tracing for a specific process.

  Gracefully stops the tracer process and cleans up all tracing state.

  ## Parameters

    * `pid` - The PID of the process to stop tracing

  ## Returns

    * `:ok` - Tracing stopped successfully

  ## Educational Notes

  Demonstrates:
  - Graceful process termination
  - Registry cleanup patterns
  - Resource management
  """
  def stop_tracing(pid) do
    case Registry.lookup(TracerRegistry, pid) do
      [{tracer_pid, _}] ->
        if Process.alive?(tracer_pid) do
          try do
            # Use synchronous GenServer.stop to ensure complete cleanup
            GenServer.stop(tracer_pid, :normal, 1000)
          catch
            :exit, _ -> :ok
          end
        end

        :ok

      [] ->
        :ok
    end
  end

  @doc """
  Analyzes message patterns to identify trends and anomalies.

  Processes a list of messages to extract useful debugging information
  such as message frequency patterns, timing analysis, and common message types.

  ## Parameters

    * `messages` - List of message maps from get_message_history/1

  ## Returns

  Map containing analysis results:
    * `:total_messages` - Total number of messages
    * `:cast_frequency` - Number of GenServer casts
    * `:call_frequency` - Number of GenServer calls
    * `:avg_message_interval` - Average time between messages
    * `:message_types` - Breakdown by message type

  ## Educational Notes

  This function demonstrates:
  - Functional data processing patterns
  - Message pattern recognition
  - Statistical analysis of communication patterns
  - Debugging techniques for OTP systems
  """
  def analyze_message_patterns(messages) when is_list(messages) do
    if length(messages) == 0 do
      %{
        total_messages: 0,
        cast_frequency: 0,
        call_frequency: 0,
        avg_message_interval: 0.0,
        message_types: %{}
      }
    else
      cast_count = count_message_type(messages, :cast)
      call_count = count_message_type(messages, :call)
      avg_interval = calculate_average_interval(messages)
      message_types = categorize_message_types(messages)

      %{
        total_messages: length(messages),
        cast_frequency: cast_count,
        call_frequency: call_count,
        avg_message_interval: avg_interval,
        message_types: message_types
      }
    end
  end

  # GenServer Callbacks

  @impl true
  def init({traced_pid, max_messages, duration}) do
    # Register this tracer for the traced process
    case Registry.register(TracerRegistry, traced_pid, nil) do
      {:ok, _} ->
        # Monitor the traced process to clean up if it dies
        Process.monitor(traced_pid)

        # Set up automatic cleanup if duration is specified
        if duration != :infinity do
          Process.send_after(self(), :auto_stop, duration * 1000)
        end

        state = %{
          traced_pid: traced_pid,
          messages: [],
          max_messages: max_messages,
          start_time: System.system_time(:millisecond)
        }

        {:ok, state}

      {:error, {:already_registered, _pid}} ->
        # The safe option is to fail here.
        {:stop, :already_traced}
    end
  end

  @impl true
  def handle_call(:get_messages, _from, state) do
    {:reply, Enum.reverse(state.messages), state}
  end

  @impl true
  def handle_info({:trace, pid, :send, message, to}, state) when pid == state.traced_pid do
    new_message = %{
      timestamp: System.system_time(:millisecond),
      direction: :outgoing,
      content: message,
      to: inspect(to)
    }

    new_state = add_message(state, new_message)
    {:noreply, new_state}
  end

  def handle_info({:trace, pid, :receive, message}, state) when pid == state.traced_pid do
    new_message = %{
      timestamp: System.system_time(:millisecond),
      direction: :incoming,
      content: message
    }

    new_state = add_message(state, new_message)
    {:noreply, new_state}
  end

  # Handle trace messages with timestamp
  def handle_info({:trace_ts, pid, :send, message, to, timestamp}, state)
      when pid == state.traced_pid do
    new_message = %{
      timestamp: timestamp_to_milliseconds(timestamp),
      direction: :outgoing,
      content: message,
      to: inspect(to)
    }

    new_state = add_message(state, new_message)
    {:noreply, new_state}
  end

  def handle_info({:trace_ts, pid, :receive, message, timestamp}, state)
      when pid == state.traced_pid do
    new_message = %{
      timestamp: timestamp_to_milliseconds(timestamp),
      direction: :incoming,
      content: message
    }

    new_state = add_message(state, new_message)
    {:noreply, new_state}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) when pid == state.traced_pid do
    # Traced process died, stop tracing
    Logger.debug("Traced process #{inspect(pid)} died, stopping tracer")
    {:stop, :normal, state}
  end

  def handle_info(:auto_stop, state) do
    Logger.debug("Auto-stopping tracer for #{inspect(state.traced_pid)}")
    {:stop, :normal, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, state) do
    # Clean up tracing
    try do
      :erlang.trace(state.traced_pid, false, [:send, :receive])
    rescue
      _ -> :ok
    end

    # Explicit registry cleanup to ensure it happens
    try do
      Registry.unregister(TracerRegistry, state.traced_pid)
    rescue
      _ -> :ok
    end

    :ok
  end

  # Private Helper Functions

  defp timestamp_to_milliseconds({mega, sec, micro}) do
    (mega * 1_000_000 + sec) * 1_000 + div(micro, 1_000)
  end

  defp add_message(state, message) do
    new_messages =
      [message | state.messages]
      |> Enum.take(state.max_messages)

    %{state | messages: new_messages}
  end

  defp count_message_type(messages, type) do
    Enum.count(messages, fn msg ->
      case msg.content do
        {^type, _} -> true
        _ -> false
      end
    end)
  end

  defp calculate_average_interval(messages) when length(messages) < 2 do
    0.0
  end

  defp calculate_average_interval(messages) do
    sorted_messages = Enum.sort_by(messages, & &1.timestamp)

    intervals =
      sorted_messages
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.map(fn [first, second] -> second.timestamp - first.timestamp end)

    if length(intervals) > 0 do
      Enum.sum(intervals) / length(intervals)
    else
      0.0
    end
  end

  defp categorize_message_types(messages) do
    messages
    |> Enum.map(fn msg -> extract_message_type(msg.content) end)
    |> Enum.frequencies()
  end

  defp extract_message_type(content) do
    case content do
      {type, _} when type in [:call, :cast] -> type
      {:DOWN, _, _, _, _} -> :monitor_down
      {:EXIT, _, _} -> :exit_signal
      _ -> :other
    end
  end
end
