defmodule OTPSupervisor.Core.Arsenal.Operations.TraceProcess do
  @moduledoc """
  Operation to enable/disable process tracing with various options.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  alias OTPSupervisor.Core.Arsenal.Operations.Storage.TraceSessionStorage

  def rest_config do
    %{
      method: :post,
      path: "/api/v1/processes/:pid/trace",
      summary: "Enable process tracing with specified flags",
      parameters: [
        %{
          name: :pid,
          type: :string,
          required: true,
          description: "Process ID to trace",
          location: :path
        },
        %{
          name: :trace_flags,
          type: :array,
          required: false,
          description: "Trace flags: 'send', 'receive', 'call', 'procs', 'garbage_collection'",
          location: :body
        },
        %{
          name: :duration_ms,
          type: :integer,
          required: false,
          description: "Duration to trace in milliseconds (default: 60000)",
          location: :body
        },
        %{
          name: :max_events,
          type: :integer,
          required: false,
          description: "Maximum number of trace events to collect",
          location: :body
        },
        %{
          name: :filter_patterns,
          type: :array,
          required: false,
          description: "Patterns to filter trace events",
          location: :body
        }
      ],
      responses: %{
        200 => %{
          description: "Tracing started successfully",
          schema: %{
            type: :object,
            properties: %{
              data: %{
                type: :object,
                properties: %{
                  tracing: %{type: :boolean},
                  trace_id: %{type: :string},
                  flags: %{type: :array},
                  duration_ms: %{type: :integer},
                  max_events: %{type: :integer}
                }
              }
            }
          }
        },
        404 => %{description: "Process not found"},
        400 => %{description: "Invalid trace parameters"}
      }
    }
  end

  def validate_params(%{"pid" => pid_string} = params) do
    with {:ok, pid} <- parse_pid(pid_string),
         {:ok, flags} <-
           validate_trace_flags(Map.get(params, "trace_flags", ["send", "receive"])),
         {:ok, duration} <- validate_duration(Map.get(params, "duration_ms", 60_000)),
         {:ok, max_events} <- validate_max_events(Map.get(params, "max_events", 1000)) do
      validated_params = %{
        "pid" => pid,
        "trace_flags" => flags,
        "duration_ms" => duration,
        "max_events" => max_events,
        "filter_patterns" => Map.get(params, "filter_patterns", [])
      }

      {:ok, validated_params}
    end
  end

  def validate_params(_params) do
    {:error, {:missing_parameter, :pid}}
  end

  def execute(%{
        "pid" => pid,
        "trace_flags" => flags,
        "duration_ms" => duration,
        "max_events" => max_events,
        "filter_patterns" => filter_patterns
      }) do
    if Process.alive?(pid) do
      case start_tracing(pid, flags, duration, max_events, filter_patterns) do
        {:ok, trace_info} -> {:ok, trace_info}
        {:error, reason} -> {:error, reason}
      end
    else
      {:error, :process_not_found}
    end
  end

  def format_response(trace_info) do
    %{data: trace_info}
  end

  defp parse_pid(pid_string) when is_binary(pid_string) do
    case OTPSupervisor.Core.Control.to_pid(pid_string) do
      {:ok, pid} -> {:ok, pid}
      {:error, _} -> {:error, {:invalid_parameter, :pid, "invalid PID format"}}
    end
  end

  defp validate_trace_flags(flags) when is_list(flags) do
    valid_flags = [
      "send",
      "receive",
      "call",
      "procs",
      "garbage_collection",
      "running",
      "set_on_spawn"
    ]

    case Enum.all?(flags, &(&1 in valid_flags)) do
      true ->
        atom_flags = Enum.map(flags, &String.to_atom/1)
        {:ok, atom_flags}

      false ->
        {:error, {:invalid_parameter, :trace_flags, "invalid trace flag"}}
    end
  end

  defp validate_trace_flags(_), do: {:error, {:invalid_parameter, :trace_flags, "must be array"}}

  defp validate_duration(duration)
       when is_integer(duration) and duration > 0 and duration <= 600_000 do
    {:ok, duration}
  end

  defp validate_duration(_) do
    {:error, {:invalid_parameter, :duration_ms, "must be positive integer <= 600000"}}
  end

  defp validate_max_events(max_events)
       when is_integer(max_events) and max_events > 0 and max_events <= 10_000 do
    {:ok, max_events}
  end

  defp validate_max_events(_) do
    {:error, {:invalid_parameter, :max_events, "must be positive integer <= 10000"}}
  end

  defp start_tracing(pid, flags, duration, max_events, filter_patterns) do
    try do
      # Generate unique trace ID
      trace_id = generate_trace_id()

      # Start trace collector process
      {:ok, collector_pid} = start_trace_collector(trace_id, max_events, filter_patterns)

      # Enable tracing with the collector as tracer
      case :erlang.trace(pid, true, [:timestamp | flags] ++ [{:tracer, collector_pid}]) do
        1 ->
          # Schedule automatic trace stopping
          Process.send_after(self(), {:stop_trace, pid, trace_id}, duration)

          # Store trace information
          store_trace_info(trace_id, %{
            pid: pid,
            flags: flags,
            collector: collector_pid,
            start_time: System.monotonic_time(:millisecond),
            duration: duration,
            max_events: max_events
          })

          result = %{
            "tracing" => true,
            "trace_id" => trace_id,
            "flags" => flags,
            "duration_ms" => duration,
            "max_events" => max_events,
            "collector_pid" => inspect(collector_pid)
          }

          {:ok, result}

        0 ->
          # Clean up collector if trace setup failed
          GenServer.stop(collector_pid)
          {:error, :trace_setup_failed}
      end
    rescue
      error -> {:error, {:tracing_failed, error}}
    end
  end

  defp generate_trace_id do
    :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
  end

  defp start_trace_collector(trace_id, max_events, filter_patterns) do
    GenServer.start_link(__MODULE__.TraceCollector, %{
      trace_id: trace_id,
      max_events: max_events,
      filter_patterns: filter_patterns,
      events: []
    })
  end

  defp store_trace_info(trace_id, info) do
    # Store using the trace session storage service
    case TraceSessionStorage.store_session(trace_id, info) do
      :ok ->
        :ok

      {:error, reason} ->
        # Log error but don't fail the trace operation
        require Logger
        Logger.warning("Failed to store trace session #{trace_id}: #{inspect(reason)}")
        :ok
    end
  end

  # Handle automatic trace stopping
  def handle_info({:stop_trace, pid, trace_id}, state) do
    stop_trace(pid, trace_id)
    {:noreply, state}
  end

  defp stop_trace(pid, trace_id) do
    # Disable tracing
    :erlang.trace(pid, false, [:all])

    # Stop collector
    case TraceSessionStorage.get_session(trace_id) do
      {:ok, %{collector: collector_pid}} ->
        GenServer.stop(collector_pid)
        TraceSessionStorage.delete_session(trace_id)

      {:error, :not_found} ->
        :ok
    end
  end
end

defmodule OTPSupervisor.Core.Arsenal.Operations.TraceProcess.TraceCollector do
  @moduledoc """
  GenServer that collects trace events for a tracing session.
  """

  use GenServer

  def init(state) do
    {:ok, state}
  end

  # Handle trace events
  def handle_info({:trace_ts, pid, event_type, data, timestamp}, state) do
    if should_collect_event?(event_type, state.filter_patterns) do
      event = %{
        pid: inspect(pid),
        type: event_type,
        data: format_trace_data(data),
        timestamp: timestamp
      }

      new_events = [event | state.events]

      # Limit events to max_events
      limited_events = Enum.take(new_events, state.max_events)

      {:noreply, %{state | events: limited_events}}
    else
      {:noreply, state}
    end
  end

  def handle_info(_, state), do: {:noreply, state}

  def handle_call(:get_events, _from, state) do
    {:reply, Enum.reverse(state.events), state}
  end

  defp should_collect_event?(_event_type, []), do: true

  defp should_collect_event?(event_type, patterns) do
    # Simple pattern matching - could be more sophisticated
    Enum.any?(patterns, fn pattern ->
      String.contains?(Atom.to_string(event_type), pattern)
    end)
  end

  defp format_trace_data(data) when is_pid(data), do: inspect(data)
  defp format_trace_data(data) when is_reference(data), do: inspect(data)
  defp format_trace_data(data), do: data
end
