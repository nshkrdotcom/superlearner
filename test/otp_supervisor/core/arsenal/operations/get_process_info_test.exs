defmodule OTPSupervisor.Core.Arsenal.Operations.GetProcessInfoTest do
  use ExUnit.Case, async: true

  alias OTPSupervisor.Core.Arsenal.Operations.GetProcessInfo
  alias SupervisorTestHelper

  describe "rest_config/0" do
    test "returns valid REST configuration" do
      config = GetProcessInfo.rest_config()

      assert config.method == :get
      assert config.path == "/api/v1/processes/:pid/info"
      assert config.summary == "Get comprehensive process information"
      assert is_list(config.parameters)
      assert is_map(config.responses)
    end

    test "has required parameter configuration" do
      config = GetProcessInfo.rest_config()

      pid_param = Enum.find(config.parameters, &(&1.name == :pid))
      assert pid_param.type == :string
      assert pid_param.required == true
      assert pid_param.location == :path
    end
  end

  describe "validate_params/1" do
    test "validates valid PID format" do
      valid_params = %{"pid" => "<0.1.0>"}

      assert {:ok, validated} = GetProcessInfo.validate_params(valid_params)
      assert is_pid(validated["pid"])
    end

    test "validates PID with #PID prefix" do
      valid_params = %{"pid" => "#PID<0.1.0>"}

      assert {:ok, validated} = GetProcessInfo.validate_params(valid_params)
      assert is_pid(validated["pid"])
    end

    test "validates keys parameter when provided" do
      params = %{"pid" => "<0.1.0>", "keys" => ["memory", "message_queue_len"]}

      assert {:ok, validated} = GetProcessInfo.validate_params(params)
      assert validated["keys"] == [:memory, :message_queue_len]
    end

    test "rejects invalid PID format" do
      invalid_params = %{"pid" => "invalid_pid"}

      assert {:error, {:invalid_parameter, :pid, _reason}} =
               GetProcessInfo.validate_params(invalid_params)
    end

    test "rejects malformed PID" do
      invalid_params = %{"pid" => "<0.1>"}

      assert {:error, {:invalid_parameter, :pid, _reason}} =
               GetProcessInfo.validate_params(invalid_params)
    end

    test "rejects invalid keys" do
      params = %{"pid" => "<0.1.0>", "keys" => ["invalid_key"]}

      assert {:error, {:invalid_parameter, :keys, _reason}} =
               GetProcessInfo.validate_params(params)
    end

    test "requires pid parameter" do
      assert {:error, {:missing_parameter, :pid}} =
               GetProcessInfo.validate_params(%{})
    end
  end

  describe "execute/1" do
    test "returns process info for alive process" do
      # Use self() as test process
      self_pid = self()
      params = %{"pid" => self_pid}

      assert {:ok, info} = GetProcessInfo.execute(params)
      assert is_map(info)
      assert Map.has_key?(info, :memory)
      assert Map.has_key?(info, :message_queue_len)
    end

    test "returns specific keys when requested" do
      self_pid = self()
      params = %{"pid" => self_pid, "keys" => [:memory, :message_queue_len]}

      assert {:ok, info} = GetProcessInfo.execute(params)
      assert is_map(info)
      assert Map.has_key?(info, :memory)
      assert Map.has_key?(info, :message_queue_len)
      # Should only have the requested keys
      assert map_size(info) == 2
    end

    test "returns error for dead process" do
      # Create a process with unique naming and kill it
      _unique_id = :erlang.unique_integer([:positive])
      dead_pid = spawn(fn -> :ok end)
      Process.exit(dead_pid, :kill)

      # Wait for process to die
      ref = Process.monitor(dead_pid)

      receive do
        {:DOWN, ^ref, :process, ^dead_pid, _} -> :ok
      after
        1000 -> flunk("Process didn't die")
      end

      params = %{"pid" => dead_pid}

      assert {:error, :process_not_found} = GetProcessInfo.execute(params)
    end

    test "handles system processes correctly" do
      # Test with a known system process (application_controller)
      app_controller = Process.whereis(:application_controller)

      if app_controller do
        params = %{"pid" => app_controller}

        assert {:ok, info} = GetProcessInfo.execute(params)
        assert is_map(info)
        assert info[:registered_name] == :application_controller
      end
    end
  end

  describe "format_response/1" do
    test "formats process info correctly" do
      info = %{
        memory: 1024,
        message_queue_len: 0,
        links: [self()],
        registered_name: :test_process
      }

      formatted = GetProcessInfo.format_response(info)

      assert %{data: data} = formatted
      assert data[:memory] == 1024
      assert data[:message_queue_len] == 0
      # PID should be stringified
      assert is_binary(data[:links] |> List.first())
      assert data[:registered_name] == :test_process
    end

    test "handles empty info" do
      formatted = GetProcessInfo.format_response(%{})

      assert %{data: data} = formatted
      assert data == %{}
    end

    test "formats complex data structures" do
      # Create temporary process with unique naming
      _unique_id = :erlang.unique_integer([:positive])

      temp_pid =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      # Ensure cleanup
      on_exit(fn ->
        if Process.alive?(temp_pid) do
          send(temp_pid, :stop)
        end
      end)

      info = %{
        links: [self(), temp_pid],
        monitors: [make_ref()],
        dictionary: [{"$initial_call", {GenServer, :init, 1}}]
      }

      formatted = GetProcessInfo.format_response(info)

      assert %{data: data} = formatted
      assert is_list(data[:links])
      assert Enum.all?(data[:links], &is_binary/1)
      assert is_list(data[:monitors])
      assert is_list(data[:dictionary])
    end
  end

  describe "integration tests" do
    test "full operation flow works" do
      # Full validation -> execution -> formatting flow
      raw_params = %{"pid" => inspect(self())}

      assert {:ok, validated_params} = GetProcessInfo.validate_params(raw_params)
      assert {:ok, result} = GetProcessInfo.execute(validated_params)
      assert %{data: _formatted_data} = GetProcessInfo.format_response(result)
    end

    test "handles edge cases gracefully" do
      # Test with process that exits during execution
      _unique_id = :erlang.unique_integer([:positive])

      test_pid =
        spawn(fn ->
          receive do
            :exit -> exit(:normal)
          after
            100 -> :ok
          end
        end)

      # Ensure cleanup
      on_exit(fn ->
        if Process.alive?(test_pid) do
          Process.exit(test_pid, :kill)
        end
      end)

      params = %{"pid" => test_pid}

      # Process might be alive or dead depending on timing
      case GetProcessInfo.execute(params) do
        # Process was still alive
        {:ok, _info} -> :ok
        # Process died
        {:error, :process_not_found} -> :ok
      end
    end
  end

  describe "error handling" do
    test "provides meaningful error messages" do
      cases = [
        {%{"pid" => "not_a_pid"}, "invalid PID format"},
        {%{}, "missing parameter"},
        {%{"pid" => "<0.1.0>", "keys" => "not_a_list"}, "must be an array"}
      ]

      Enum.each(cases, fn {params, _expected_error_type} ->
        assert {:error, error} = GetProcessInfo.validate_params(params)
        assert is_tuple(error)
        # Error should contain meaningful information
        error_string = inspect(error)
        assert String.length(error_string) > 10
      end)
    end
  end
end
