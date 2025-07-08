defmodule OTPSupervisor.Core.Arsenal.Operations.SendMessageTest do
  use ExUnit.Case, async: true

  alias OTPSupervisor.Core.Arsenal.Operations.SendMessage

  describe "rest_config/0" do
    test "returns valid REST configuration" do
      config = SendMessage.rest_config()

      assert config.method == :post
      assert config.path == "/api/v1/processes/:pid/message"
      assert config.summary == "Send a message to a process"
      assert is_list(config.parameters)
      assert is_map(config.responses)
    end

    test "has correct parameter definitions" do
      config = SendMessage.rest_config()

      param_names = Enum.map(config.parameters, & &1.name)
      assert :pid in param_names
      assert :message in param_names
      assert :message_type in param_names
      assert :timeout_ms in param_names
      assert :track_response in param_names
    end
  end

  describe "validate_params/1" do
    test "validates basic send message" do
      params = %{
        "pid" => "<0.1.0>",
        "message" => %{"content" => "test"}
      }

      assert {:ok, validated} = SendMessage.validate_params(params)

      assert is_pid(validated["pid"])
      assert validated["message"] == %{"content" => "test"}
      assert validated["message_type"] == :send
      assert validated["timeout_ms"] == 5000
      assert validated["track_response"] == false
    end

    test "validates custom message type" do
      params = %{
        "pid" => "<0.1.0>",
        "message" => %{"action" => "get_state"},
        "message_type" => "call"
      }

      assert {:ok, validated} = SendMessage.validate_params(params)

      assert validated["message_type"] == :call
    end

    test "validates custom timeout" do
      params = %{
        "pid" => "<0.1.0>",
        "message" => %{"content" => "test"},
        "timeout_ms" => 10000
      }

      assert {:ok, validated} = SendMessage.validate_params(params)

      assert validated["timeout_ms"] == 10000
    end

    test "validates track_response option" do
      params = %{
        "pid" => "<0.1.0>",
        "message" => %{"content" => "test"},
        "track_response" => true
      }

      assert {:ok, validated} = SendMessage.validate_params(params)

      assert validated["track_response"] == true
    end

    test "rejects invalid PID format" do
      params = %{
        "pid" => "invalid_pid",
        "message" => %{"content" => "test"}
      }

      assert {:error, {:invalid_parameter, :pid, _reason}} =
               SendMessage.validate_params(params)
    end

    test "rejects invalid message type" do
      params = %{
        "pid" => "<0.1.0>",
        "message" => %{"content" => "test"},
        "message_type" => "invalid_type"
      }

      assert {:error, {:invalid_parameter, :message_type, _reason}} =
               SendMessage.validate_params(params)
    end

    test "rejects invalid timeout" do
      params = %{
        "pid" => "<0.1.0>",
        "message" => %{"content" => "test"},
        "timeout_ms" => -1000
      }

      assert {:error, {:invalid_parameter, :timeout_ms, _reason}} =
               SendMessage.validate_params(params)
    end

    test "requires pid parameter" do
      params = %{"message" => %{"content" => "test"}}

      assert {:error, {:missing_parameter, :pid}} =
               SendMessage.validate_params(params)
    end

    test "handles message with type formatting" do
      params = %{
        "pid" => "<0.1.0>",
        "message" => %{"type" => "get_state", "content" => "extra_data"}
      }

      assert {:ok, validated} = SendMessage.validate_params(params)

      # Should convert to tuple format
      assert validated["message"] == {:get_state, "extra_data"}
    end
  end

  describe "execute/1 - send message type" do
    test "sends basic message successfully" do
      # Create a test process that can receive messages
      test_pid =
        spawn(fn ->
          receive do
            message -> send(self(), {:received, message})
          end
        end)

      params = %{
        "pid" => test_pid,
        "message" => %{"content" => "test_message"},
        "message_type" => :send,
        "timeout_ms" => 5000,
        "track_response" => false
      }

      assert {:ok, result} = SendMessage.execute(params)

      assert result["message_sent"] == true
      assert result["message_type"] == "send"
      assert result["target_pid"] == inspect(test_pid)
    end

    test "sends message with tracking" do
      test_pid =
        spawn(fn ->
          receive do
            {:tracked_message, _ref, _message} -> :ok
          end
        end)

      params = %{
        "pid" => test_pid,
        "message" => %{"content" => "tracked_test"},
        "message_type" => :send,
        "timeout_ms" => 5000,
        "track_response" => true
      }

      assert {:ok, result} = SendMessage.execute(params)

      assert result["message_sent"] == true
      assert Map.has_key?(result, "tracking_ref")
      assert Map.has_key?(result, "note")
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
        "message" => %{"content" => "test"},
        "message_type" => :send,
        "timeout_ms" => 5000,
        "track_response" => false
      }

      assert {:error, :process_not_found} = SendMessage.execute(params)
    end
  end

  describe "execute/1 - cast message type" do
    test "sends GenServer cast successfully" do
      # Start a simple GenServer for testing
      {:ok, genserver_pid} = Agent.start_link(fn -> %{} end)

      params = %{
        "pid" => genserver_pid,
        "message" => {:cast, fn state -> Map.put(state, :test, true) end},
        "message_type" => :cast,
        "timeout_ms" => 5000,
        "track_response" => false
      }

      assert {:ok, result} = SendMessage.execute(params)

      assert result["message_sent"] == true
      assert result["message_type"] == "cast"

      # Clean up
      Agent.stop(genserver_pid)
    end

    test "handles cast to non-GenServer process" do
      regular_pid =
        spawn(fn ->
          receive do
            {:"$gen_cast", _message} -> :ok
          end
        end)

      params = %{
        "pid" => regular_pid,
        "message" => %{"action" => "test"},
        "message_type" => :cast,
        "timeout_ms" => 5000,
        "track_response" => false
      }

      assert {:ok, result} = SendMessage.execute(params)

      assert result["message_sent"] == true
      assert result["message_type"] == "cast"
    end
  end

  describe "execute/1 - call message type" do
    test "sends GenServer call successfully" do
      # Start a GenServer that responds to calls
      {:ok, genserver_pid} = Agent.start_link(fn -> %{counter: 0} end)

      params = %{
        "pid" => genserver_pid,
        "message" => {:get, fn state -> state.counter end},
        "message_type" => :call,
        "timeout_ms" => 5000,
        "track_response" => false
      }

      assert {:ok, result} = SendMessage.execute(params)

      assert result["message_sent"] == true
      assert result["message_type"] == "call"
      assert Map.has_key?(result, "response")

      # Clean up
      Agent.stop(genserver_pid)
    end

    test "handles call timeout" do
      # Create a GenServer that doesn't respond
      genserver_pid =
        spawn(fn ->
          receive do
            {:"$gen_call", _from, _message} ->
              # Don't reply - cause timeout
              receive do
                :never -> :ok
              end
          end
        end)

      params = %{
        "pid" => genserver_pid,
        "message" => %{"action" => "timeout_test"},
        "message_type" => :call,
        # Short timeout
        "timeout_ms" => 100,
        "track_response" => false
      }

      assert {:error, :call_timeout} = SendMessage.execute(params)
    end

    test "handles call to non-GenServer process" do
      regular_pid = spawn(fn -> :ok end)

      params = %{
        "pid" => regular_pid,
        "message" => %{"action" => "test"},
        "message_type" => :call,
        "timeout_ms" => 1000,
        "track_response" => false
      }

      # This should fail since regular processes don't handle GenServer calls
      assert {:error, {:call_failed, _reason}} = SendMessage.execute(params)
    end
  end

  describe "format_response/1" do
    test "formats send response correctly" do
      result = %{
        "message_sent" => true,
        "message_type" => "send",
        "target_pid" => "<0.123.0>"
      }

      formatted = SendMessage.format_response(result)

      assert %{data: data} = formatted
      assert data == result
    end

    test "formats call response with result" do
      result = %{
        "message_sent" => true,
        "message_type" => "call",
        "target_pid" => "<0.123.0>",
        "response" => %{"status" => "ok", "data" => "test_result"}
      }

      formatted = SendMessage.format_response(result)

      assert %{data: data} = formatted
      assert data["response"]["status"] == "ok"
      assert data["response"]["data"] == "test_result"
    end

    test "formats tracking response" do
      result = %{
        "message_sent" => true,
        "message_type" => "send",
        "target_pid" => "<0.123.0>",
        "tracking_ref" => "#Reference<0.1.2.3>",
        "note" => "Message sent but response tracking requires target process cooperation"
      }

      formatted = SendMessage.format_response(result)

      assert %{data: data} = formatted
      assert Map.has_key?(data, "tracking_ref")
      assert Map.has_key?(data, "note")
    end

    test "handles complex response values" do
      # Test with various Erlang/Elixir types that need formatting
      result = %{
        "message_sent" => true,
        "message_type" => "call",
        "target_pid" => "<0.123.0>",
        "response" => %{
          "pid" => self(),
          "ref" => make_ref(),
          "function" => fn -> :ok end,
          "port" => hd(Port.list()),
          "simple" => "value"
        }
      }

      formatted = SendMessage.format_response(result)

      assert %{data: data} = formatted
      response = data["response"]

      # PIDs should be stringified
      assert is_binary(response["pid"])
      assert String.starts_with?(response["pid"], "#PID")

      # References should be stringified
      assert is_binary(response["ref"])
      assert String.starts_with?(response["ref"], "#Reference")

      # Functions should be simplified
      assert response["function"] == "#Function<>"

      # Ports should be stringified
      assert is_binary(response["port"])

      # Simple values should pass through
      assert response["simple"] == "value"
    end
  end

  describe "message content parsing" do
    test "parses simple message content" do
      params = %{
        "pid" => "<0.1.0>",
        "message" => "simple_string"
      }

      assert {:ok, validated} = SendMessage.validate_params(params)
      assert validated["message"] == "simple_string"
    end

    test "parses message with type conversion" do
      params = %{
        "pid" => "<0.1.0>",
        "message" => %{
          "type" => "get_state",
          "content" => %{"key" => "value"}
        }
      }

      assert {:ok, validated} = SendMessage.validate_params(params)
      assert validated["message"] == {:get_state, %{"key" => "value"}}
    end

    test "parses message with type but no content" do
      params = %{
        "pid" => "<0.1.0>",
        "message" => %{"type" => "ping"}
      }

      assert {:ok, validated} = SendMessage.validate_params(params)
      assert validated["message"] == {:ping, %{}}
    end

    test "parses complex nested message" do
      complex_message = %{
        "action" => "update",
        "data" => %{
          "user_id" => 123,
          "changes" => ["name", "email"],
          "metadata" => %{"timestamp" => "2024-01-01"}
        }
      }

      params = %{
        "pid" => "<0.1.0>",
        "message" => complex_message
      }

      assert {:ok, validated} = SendMessage.validate_params(params)
      assert validated["message"] == complex_message
    end
  end

  describe "integration tests" do
    test "full operation flow for send" do
      test_pid =
        spawn(fn ->
          receive do
            message -> send(self(), {:got_message, message})
          end
        end)

      # Full validation -> execution -> formatting flow
      raw_params = %{
        "pid" => inspect(test_pid),
        "message" => %{"content" => "integration_test"},
        "message_type" => "send"
      }

      assert {:ok, validated_params} = SendMessage.validate_params(raw_params)
      assert {:ok, result} = SendMessage.execute(validated_params)
      assert %{data: _formatted_data} = SendMessage.format_response(result)
    end

    test "full operation flow for GenServer call" do
      {:ok, agent_pid} = Agent.start_link(fn -> 42 end)

      raw_params = %{
        "pid" => inspect(agent_pid),
        "message" => {:get, fn state -> state end},
        "message_type" => "call",
        "timeout_ms" => 5000
      }

      assert {:ok, validated_params} = SendMessage.validate_params(raw_params)
      assert {:ok, result} = SendMessage.execute(validated_params)
      assert %{data: formatted_data} = SendMessage.format_response(result)

      assert formatted_data["message_sent"] == true
      assert formatted_data["response"] == 42

      Agent.stop(agent_pid)
    end
  end

  describe "error handling and edge cases" do
    test "handles process that exits during message sending" do
      # This is a race condition test - using proper OTP patterns
      test_pid =
        spawn(fn ->
          # Use receive with timeout instead of sleep to create controlled delay
          receive do
            :continue -> exit(:normal)
          after
            1 -> exit(:normal)
          end
        end)

      params = %{
        "pid" => test_pid,
        "message" => %{"content" => "test"},
        "message_type" => :send,
        "timeout_ms" => 5000,
        "track_response" => false
      }

      # Either succeeds (message sent before exit) or fails (process died)
      result = SendMessage.execute(params)
      assert match?({:ok, _}, result) or match?({:error, :process_not_found}, result)
    end

    test "handles very large messages" do
      large_message = %{
        "data" => String.duplicate("a", 10_000),
        "metadata" => %{
          "size" => 10_000,
          "type" => "large_test"
        }
      }

      test_pid =
        spawn(fn ->
          receive do
            _large_msg -> :ok
          end
        end)

      params = %{
        "pid" => test_pid,
        "message" => large_message,
        "message_type" => :send,
        "timeout_ms" => 5000,
        "track_response" => false
      }

      assert {:ok, result} = SendMessage.execute(params)
      assert result["message_sent"] == true
    end
  end
end
