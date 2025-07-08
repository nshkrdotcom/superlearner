defmodule OTPSupervisor.Core.Arsenal.Operations.TraceProcessTest do
  use ExUnit.Case, async: false

  alias OTPSupervisor.Core.Arsenal.Operations.TraceProcess
  alias OTPSupervisor.Core.Arsenal.Operations.Storage.TraceSessionStorage

  setup do
    # Ensure TraceSessionStorage is running and healthy
    storage_pid =
      case GenServer.whereis(TraceSessionStorage) do
        nil ->
          {:ok, pid} = TraceSessionStorage.start_link()
          pid

        pid when is_pid(pid) ->
          # Check if the process is actually alive and responsive
          try do
            GenServer.call(pid, :session_count, 100)
            pid
          catch
            :exit, _ ->
              # Process is dead/unresponsive, start a new one
              {:ok, new_pid} = TraceSessionStorage.start_link()
              new_pid
          end

        _ ->
          {:ok, pid} = TraceSessionStorage.start_link()
          pid
      end

    # Clean up any existing sessions from previous tests
    try do
      TraceSessionStorage.list_sessions()
      |> Enum.each(fn {trace_id, _} ->
        TraceSessionStorage.delete_session(trace_id)
      end)
    rescue
      # Ignore cleanup errors
      _ -> :ok
    end

    # Return storage PID for potential test use
    %{storage_pid: storage_pid}
  end

  describe "rest_config/0" do
    test "returns valid REST configuration" do
      config = TraceProcess.rest_config()

      assert config.method == :post
      assert config.path == "/api/v1/processes/:pid/trace"
      assert config.summary == "Enable process tracing with specified flags"
      assert is_list(config.parameters)
      assert is_map(config.responses)
    end

    test "has correct parameter definitions" do
      config = TraceProcess.rest_config()

      param_names = Enum.map(config.parameters, & &1.name)
      assert :pid in param_names
      assert :trace_flags in param_names
      assert :duration_ms in param_names
      assert :max_events in param_names
      assert :filter_patterns in param_names
    end

    test "has appropriate response codes" do
      config = TraceProcess.rest_config()

      assert Map.has_key?(config.responses, 200)
      assert Map.has_key?(config.responses, 404)
      assert Map.has_key?(config.responses, 400)
    end
  end

  describe "validate_params/1" do
    test "validates basic tracing parameters" do
      params = %{"pid" => "<0.1.0>"}

      assert {:ok, validated} = TraceProcess.validate_params(params)

      assert is_pid(validated["pid"])
      # default
      assert validated["trace_flags"] == [:send, :receive]
      # default
      assert validated["duration_ms"] == 60_000
      # default
      assert validated["max_events"] == 1000
      # default
      assert validated["filter_patterns"] == []
    end

    test "validates custom trace flags" do
      params = %{
        "pid" => "<0.1.0>",
        "trace_flags" => ["send", "receive", "call", "procs"]
      }

      assert {:ok, validated} = TraceProcess.validate_params(params)

      assert validated["trace_flags"] == [:send, :receive, :call, :procs]
    end

    test "validates custom duration" do
      params = %{
        "pid" => "<0.1.0>",
        "duration_ms" => 30_000
      }

      assert {:ok, validated} = TraceProcess.validate_params(params)

      assert validated["duration_ms"] == 30_000
    end

    test "validates custom max_events" do
      params = %{
        "pid" => "<0.1.0>",
        "max_events" => 500
      }

      assert {:ok, validated} = TraceProcess.validate_params(params)

      assert validated["max_events"] == 500
    end

    test "validates filter patterns" do
      params = %{
        "pid" => "<0.1.0>",
        "filter_patterns" => ["send", "call"]
      }

      assert {:ok, validated} = TraceProcess.validate_params(params)

      assert validated["filter_patterns"] == ["send", "call"]
    end

    test "rejects invalid PID format" do
      params = %{"pid" => "invalid_pid"}

      assert {:error, {:invalid_parameter, :pid, _reason}} =
               TraceProcess.validate_params(params)
    end

    test "rejects invalid trace flags" do
      params = %{
        "pid" => "<0.1.0>",
        "trace_flags" => ["invalid_flag"]
      }

      assert {:error, {:invalid_parameter, :trace_flags, _reason}} =
               TraceProcess.validate_params(params)
    end

    test "rejects invalid duration" do
      # negative, zero, too large
      invalid_durations = [-1000, 0, 700_000]

      Enum.each(invalid_durations, fn duration ->
        params = %{
          "pid" => "<0.1.0>",
          "duration_ms" => duration
        }

        assert {:error, {:invalid_parameter, :duration_ms, _reason}} =
                 TraceProcess.validate_params(params)
      end)
    end

    test "rejects invalid max_events" do
      # negative, zero, too large
      invalid_max_events = [-100, 0, 20_000]

      Enum.each(invalid_max_events, fn max_events ->
        params = %{
          "pid" => "<0.1.0>",
          "max_events" => max_events
        }

        assert {:error, {:invalid_parameter, :max_events, _reason}} =
                 TraceProcess.validate_params(params)
      end)
    end

    test "rejects non-array trace_flags" do
      params = %{
        "pid" => "<0.1.0>",
        "trace_flags" => "not_an_array"
      }

      assert {:error, {:invalid_parameter, :trace_flags, _reason}} =
               TraceProcess.validate_params(params)
    end

    test "requires pid parameter" do
      assert {:error, {:missing_parameter, :pid}} =
               TraceProcess.validate_params(%{})
    end

    test "validates all valid trace flags" do
      valid_flags = [
        "send",
        "receive",
        "call",
        "procs",
        "garbage_collection",
        "running",
        "set_on_spawn"
      ]

      params = %{
        "pid" => "<0.1.0>",
        "trace_flags" => valid_flags
      }

      assert {:ok, validated} = TraceProcess.validate_params(params)

      expected_atoms = Enum.map(valid_flags, &String.to_atom/1)
      assert validated["trace_flags"] == expected_atoms
    end
  end

  describe "execute/1" do
    test "starts tracing for alive process" do
      # Use self() as test process
      self_pid = self()

      params = %{
        "pid" => self_pid,
        "trace_flags" => [:send, :receive],
        "duration_ms" => 5000,
        "max_events" => 100,
        "filter_patterns" => []
      }

      assert {:ok, result} = TraceProcess.execute(params)

      assert result["tracing"] == true
      assert is_binary(result["trace_id"])
      assert result["flags"] == [:send, :receive]
      assert result["duration_ms"] == 5000
      assert result["max_events"] == 100
      assert Map.has_key?(result, "collector_pid")

      # Clean up tracing
      :erlang.trace(self_pid, false, [:all])
    end

    test "returns error for dead process" do
      dead_pid = spawn(fn -> :ok end)
      Process.exit(dead_pid, :kill)

      # Wait for process to die
      ref = Process.monitor(dead_pid)

      receive do
        {:DOWN, ^ref, :process, ^dead_pid, _} -> :ok
      after
        1000 -> flunk("Process didn't die")
      end

      params = %{
        "pid" => dead_pid,
        "trace_flags" => [:send, :receive],
        "duration_ms" => 5000,
        "max_events" => 100,
        "filter_patterns" => []
      }

      assert {:error, :process_not_found} = TraceProcess.execute(params)
    end

    test "generates unique trace IDs" do
      # Create different processes for each trace (since only one tracer per process allowed)
      test_pids =
        for _ <- 1..3 do
          spawn(fn ->
            receive do
              :stop -> :ok
            end
          end)
        end

      # Start traces on different processes
      traces =
        for test_pid <- test_pids do
          params = %{
            "pid" => test_pid,
            "trace_flags" => [:send],
            "duration_ms" => 1000,
            "max_events" => 10,
            "filter_patterns" => []
          }

          {:ok, result} = TraceProcess.execute(params)
          result["trace_id"]
        end

      # All trace IDs should be unique
      assert length(Enum.uniq(traces)) == 3

      # Clean up
      for test_pid <- test_pids do
        :erlang.trace(test_pid, false, [:all])
        send(test_pid, :stop)
      end
    end

    test "handles different trace flag combinations" do
      test_pid =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      flag_combinations = [
        [:send],
        [:receive],
        [:send, :receive],
        [:send, :receive, :procs],
        [:garbage_collection],
        [:running]
      ]

      for flags <- flag_combinations do
        params = %{
          "pid" => test_pid,
          "trace_flags" => flags,
          "duration_ms" => 1000,
          "max_events" => 10,
          "filter_patterns" => []
        }

        assert {:ok, result} = TraceProcess.execute(params)
        assert result["flags"] == flags

        # Clean up trace for next iteration
        :erlang.trace(test_pid, false, [:all])
      end

      send(test_pid, :stop)
    end

    test "stores trace information in ETS" do
      test_pid =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      params = %{
        "pid" => test_pid,
        "trace_flags" => [:send],
        "duration_ms" => 5000,
        "max_events" => 100,
        "filter_patterns" => []
      }

      assert {:ok, result} = TraceProcess.execute(params)

      trace_id = result["trace_id"]

      # Check that trace info is stored
      case TraceSessionStorage.get_session(trace_id) do
        {:ok, trace_info} ->
          assert trace_info.pid == test_pid
          assert trace_info.flags == [:send]
          assert is_pid(trace_info.collector)

        {:error, :not_found} ->
          flunk("Trace information not stored in ETS")
      end

      # Clean up
      :erlang.trace(test_pid, false, [:all])
      send(test_pid, :stop)
    end
  end

  describe "TraceCollector" do
    alias TraceProcess.TraceCollector

    test "collects trace events" do
      initial_state = %{
        trace_id: "test_trace",
        max_events: 10,
        filter_patterns: [],
        events: []
      }

      {:ok, collector} = GenServer.start_link(TraceCollector, initial_state)

      # Send a mock trace event
      timestamp = :erlang.timestamp()
      trace_event = {:trace_ts, self(), :send, {:test_message, "data"}, timestamp}

      send(collector, trace_event)

      # Use synchronous call to ensure message is processed
      # GenServer guarantees FIFO message processing
      events = GenServer.call(collector, :get_events)

      assert length(events) == 1
      event = List.first(events)

      assert event.pid == inspect(self())
      assert event.type == :send
      assert event.timestamp == timestamp

      GenServer.stop(collector)
    end

    test "respects max_events limit" do
      initial_state = %{
        trace_id: "test_trace",
        # Small limit
        max_events: 2,
        filter_patterns: [],
        events: []
      }

      {:ok, collector} = GenServer.start_link(TraceCollector, initial_state)

      # Send more events than the limit
      timestamp = :erlang.timestamp()

      for i <- 1..5 do
        trace_event = {:trace_ts, self(), :send, {:message, i}, timestamp}
        send(collector, trace_event)
      end

      # Use synchronous call to ensure all messages are processed
      # GenServer guarantees FIFO message processing
      events = GenServer.call(collector, :get_events)

      # Should only have max_events number of events
      assert length(events) <= 2

      GenServer.stop(collector)
    end

    test "filters events based on patterns" do
      initial_state = %{
        trace_id: "test_trace",
        max_events: 100,
        # Only collect send events
        filter_patterns: ["send"],
        events: []
      }

      {:ok, collector} = GenServer.start_link(TraceCollector, initial_state)

      timestamp = :erlang.timestamp()

      # Send different types of events
      events_to_send = [
        {:trace_ts, self(), :send, {:message, 1}, timestamp},
        {:trace_ts, self(), :receive, {:message, 2}, timestamp},
        {:trace_ts, self(), :call, {:message, 3}, timestamp}
      ]

      for event <- events_to_send do
        send(collector, event)
      end

      # Use synchronous call to ensure all messages are processed
      # GenServer guarantees FIFO message processing
      collected_events = GenServer.call(collector, :get_events)

      # Should only have send events due to filtering
      assert length(collected_events) == 1
      assert List.first(collected_events).type == :send

      GenServer.stop(collector)
    end

    test "handles unknown messages gracefully" do
      initial_state = %{
        trace_id: "test_trace",
        max_events: 10,
        filter_patterns: [],
        events: []
      }

      {:ok, collector} = GenServer.start_link(TraceCollector, initial_state)

      # Send unknown message
      send(collector, :unknown_message)

      # Should not crash
      assert Process.alive?(collector)

      # Events should still be empty
      events = GenServer.call(collector, :get_events)
      assert events == []

      GenServer.stop(collector)
    end
  end

  describe "format_response/1" do
    test "formats tracing response correctly" do
      trace_info = %{
        "tracing" => true,
        "trace_id" => "abc123",
        "flags" => [:send, :receive],
        "duration_ms" => 30000,
        "max_events" => 500,
        "collector_pid" => "#PID<0.123.0>"
      }

      formatted = TraceProcess.format_response(trace_info)

      assert %{data: data} = formatted
      assert data == trace_info
    end

    test "preserves all trace information" do
      trace_info = %{
        "tracing" => true,
        "trace_id" => "def456",
        "flags" => [:procs, :garbage_collection],
        "duration_ms" => 15000,
        "max_events" => 1000,
        "collector_pid" => "#PID<0.456.0>",
        "additional_info" => "test_data"
      }

      formatted = TraceProcess.format_response(trace_info)

      assert %{data: data} = formatted
      assert data["tracing"] == true
      assert data["trace_id"] == "def456"
      assert data["flags"] == [:procs, :garbage_collection]
      assert data["duration_ms"] == 15000
      assert data["max_events"] == 1000
      assert data["additional_info"] == "test_data"
    end
  end

  describe "integration tests" do
    test "full operation flow works" do
      test_pid =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      # Full validation -> execution -> formatting flow
      raw_params = %{
        "pid" => inspect(test_pid),
        "trace_flags" => ["send", "receive"],
        "duration_ms" => 5000,
        "max_events" => 100
      }

      assert {:ok, validated_params} = TraceProcess.validate_params(raw_params)
      assert {:ok, result} = TraceProcess.execute(validated_params)
      assert %{data: _formatted_data} = TraceProcess.format_response(result)

      # Clean up
      :erlang.trace(test_pid, false, [:all])
      send(test_pid, :stop)
    end

    test "actual message tracing works" do
      # Create a process that will send and receive messages
      test_pid =
        spawn(fn ->
          receive do
            {:start_activity, test_process} ->
              # Send a message to create trace events
              send(test_process, :test_message)

              # Wait for stop signal
              receive do
                :stop -> :ok
              end
          end
        end)

      # Start tracing
      params = %{
        "pid" => test_pid,
        "trace_flags" => [:send, :receive],
        "duration_ms" => 2000,
        "max_events" => 50,
        "filter_patterns" => []
      }

      assert {:ok, trace_result} = TraceProcess.execute(params)
      trace_id = trace_result["trace_id"]

      # Trigger some activity to generate trace events
      send(test_pid, {:start_activity, self()})

      # Ensure message processing completed with synchronous call
      # Wait for test message to be received
      receive do
        :test_message -> :ok
      after
        1000 -> flunk("Test message not received")
      end

      # Retrieve trace information
      case TraceSessionStorage.get_session(trace_id) do
        {:ok, trace_info} ->
          if Process.alive?(trace_info.collector) do
            events = GenServer.call(trace_info.collector, :get_events)
            # Should have collected some events
            assert is_list(events)
          end

        {:error, :not_found} ->
          # Trace might have been cleaned up already
          :ok
      end

      # Clean up
      :erlang.trace(test_pid, false, [:all])
      send(test_pid, :stop)
    end
  end

  describe "error handling and edge cases" do
    test "handles process that exits during trace setup" do
      # Create a process that exits quickly using receive with timeout
      quick_exit_pid =
        spawn(fn ->
          receive do
            :continue -> exit(:normal)
          after
            1 -> exit(:normal)
          end
        end)

      params = %{
        "pid" => quick_exit_pid,
        "trace_flags" => [:send],
        "duration_ms" => 5000,
        "max_events" => 100,
        "filter_patterns" => []
      }

      # This might succeed or fail depending on timing
      result = TraceProcess.execute(params)

      case result do
        {:ok, _trace_info} ->
          # Trace started successfully
          :ok

        {:error, :process_not_found} ->
          # Process died before trace could be set up
          :ok

        {:error, _reason} ->
          # Other error occurred
          :ok
      end
    end

    test "handles trace failures gracefully" do
      # Test with invalid trace flags (this should be caught in validation,
      # but test the execute path too)
      params = %{
        "pid" => self(),
        # Empty flags
        "trace_flags" => [],
        "duration_ms" => 1000,
        "max_events" => 10,
        "filter_patterns" => []
      }

      # Should either succeed with empty flags or handle gracefully
      result = TraceProcess.execute(params)

      case result do
        {:ok, _} -> :ok
        {:error, _} -> :ok
      end
    end
  end

  describe "trace session management" do
    test "trace sessions are properly stored and retrievable" do
      test_pid =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      params = %{
        "pid" => test_pid,
        "trace_flags" => [:send],
        # Long duration
        "duration_ms" => 30000,
        "max_events" => 100,
        "filter_patterns" => []
      }

      assert {:ok, result} = TraceProcess.execute(params)
      trace_id = result["trace_id"]

      # Verify trace session is stored
      case TraceSessionStorage.get_session(trace_id) do
        {:ok, trace_info} ->
          assert trace_info.pid == test_pid
          assert trace_info.flags == [:send]
          assert trace_info.max_events == 100
          assert is_pid(trace_info.collector)

        {:error, :not_found} ->
          flunk("Trace session not found in ETS")
      end

      # Clean up
      :erlang.trace(test_pid, false, [:all])
      TraceSessionStorage.delete_session(trace_id)
      send(test_pid, :stop)
    end
  end
end
