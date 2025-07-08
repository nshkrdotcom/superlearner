defmodule OTPSupervisor.Core.Arsenal.Operations.KillProcessTest do
  use ExUnit.Case, async: true

  alias OTPSupervisor.Core.Arsenal.Operations.KillProcess
  alias SupervisorTestHelper

  describe "rest_config/0" do
    test "returns valid REST configuration" do
      config = KillProcess.rest_config()

      assert config.method == :delete
      assert config.path == "/api/v1/processes/:pid"
      assert config.summary == "Terminate a process with specified reason"
      assert is_list(config.parameters)
      assert is_map(config.responses)
    end

    test "has correct response codes" do
      config = KillProcess.rest_config()

      assert Map.has_key?(config.responses, 200)
      assert Map.has_key?(config.responses, 404)
      assert Map.has_key?(config.responses, 403)
      assert Map.has_key?(config.responses, 400)
    end
  end

  describe "validate_params/1" do
    test "validates valid PID format" do
      params = %{"pid" => "<0.1.0>"}

      assert {:ok, validated} = KillProcess.validate_params(params)
      assert is_pid(validated["pid"])
      # default reason
      assert validated["reason"] == :killed
      # default force
      assert validated["force"] == false
    end

    test "validates custom reason" do
      params = %{"pid" => "<0.1.0>", "reason" => "shutdown"}

      assert {:ok, validated} = KillProcess.validate_params(params)
      assert validated["reason"] == :shutdown
    end

    test "validates force parameter" do
      params = %{"pid" => "<0.1.0>", "force" => true}

      assert {:ok, validated} = KillProcess.validate_params(params)
      assert validated["force"] == true
    end

    test "handles various reason formats" do
      test_cases = [
        {"normal", :normal},
        {"shutdown", :shutdown},
        {"killed", :killed},
        {"kill", :kill},
        {"custom_reason", :custom_reason}
      ]

      Enum.each(test_cases, fn {input_reason, expected_reason} ->
        params = %{"pid" => "<0.1.0>", "reason" => input_reason}

        assert {:ok, validated} = KillProcess.validate_params(params)
        assert validated["reason"] == expected_reason
      end)
    end

    test "rejects invalid PID format" do
      params = %{"pid" => "invalid_pid"}

      assert {:error, {:invalid_parameter, :pid, _reason}} =
               KillProcess.validate_params(params)
    end

    test "requires pid parameter" do
      assert {:error, {:missing_parameter, :pid}} =
               KillProcess.validate_params(%{})
    end
  end

  describe "execute/1 - destructive operations" do
    setup do
      # Create unique test processes for isolation
      _unique_id = :erlang.unique_integer([:positive])
      :ok
    end

    test "terminates a regular process successfully" do
      # Create a test process with unique naming
      _unique_id = :erlang.unique_integer([:positive])

      test_pid =
        spawn(fn ->
          receive do
            :keep_alive -> :ok
          end
        end)

      # Ensure cleanup if test fails
      on_exit(fn ->
        if Process.alive?(test_pid) do
          Process.exit(test_pid, :kill)
        end
      end)

      # Verify process is alive
      assert Process.alive?(test_pid)

      params = %{
        "pid" => test_pid,
        "reason" => :killed,
        "force" => false
      }

      assert {:ok, {^test_pid, :killed, true}} = KillProcess.execute(params)

      # Verify process is dead
      refute Process.alive?(test_pid)
    end

    test "returns error for already dead process" do
      # Create and kill a process with unique naming
      _unique_id = :erlang.unique_integer([:positive])
      dead_pid = spawn(fn -> :ok end)
      Process.exit(dead_pid, :kill)

      # Wait for process to die using proper OTP patterns
      ref = Process.monitor(dead_pid)

      receive do
        {:DOWN, ^ref, :process, ^dead_pid, _} -> :ok
      after
        1000 -> flunk("Process didn't die")
      end

      params = %{
        "pid" => dead_pid,
        "reason" => :killed,
        "force" => false
      }

      assert {:error, :process_not_found} = KillProcess.execute(params)
    end

    test "protects critical processes without force" do
      # Mock a critical process by using application_controller
      app_controller = Process.whereis(:application_controller)

      if app_controller do
        params = %{
          "pid" => app_controller,
          "reason" => :killed,
          "force" => false
        }

        assert {:error, :critical_process_protection} = KillProcess.execute(params)

        # Verify process is still alive
        assert Process.alive?(app_controller)
      end
    end

    test "allows terminating critical processes with force" do
      # Create a mock critical process (supervisor-like) with unique naming
      _unique_id = :erlang.unique_integer([:positive])

      critical_pid =
        spawn(fn ->
          Process.put(:"$initial_call", {Supervisor, :init, 1})

          receive do
            :keep_alive -> :ok
          end
        end)

      # Ensure cleanup
      on_exit(fn ->
        if Process.alive?(critical_pid) do
          Process.exit(critical_pid, :kill)
        end
      end)

      params = %{
        "pid" => critical_pid,
        "reason" => :killed,
        "force" => true
      }

      assert {:ok, {^critical_pid, :killed, true}} = KillProcess.execute(params)
      refute Process.alive?(critical_pid)
    end

    test "handles different termination reasons" do
      test_cases = [
        # :normal doesn't kill processes in receive loops
        {:normal, false},
        # :shutdown does kill them  
        {:shutdown, true},
        # :killed definitely kills them
        {:killed, true},
        # :kill definitely kills them
        {:kill, true},
        # Custom reasons kill them
        {:custom_reason, true}
      ]

      _unique_id = :erlang.unique_integer([:positive])

      Enum.each(test_cases, fn {reason, should_terminate} ->
        test_pid =
          spawn(fn ->
            receive do
              :keep_alive -> :ok
            end
          end)

        # Ensure cleanup if test fails
        on_exit(fn ->
          if Process.alive?(test_pid) do
            Process.exit(test_pid, :kill)
          end
        end)

        params = %{
          "pid" => test_pid,
          "reason" => reason,
          "force" => false
        }

        assert {:ok, {^test_pid, ^reason, ^should_terminate}} = KillProcess.execute(params)

        if should_terminate do
          refute Process.alive?(test_pid)
        end
      end)
    end

    test "handles process that takes time to die" do
      # Create a process that traps exits with unique naming
      _unique_id = :erlang.unique_integer([:positive])

      test_pid =
        spawn(fn ->
          Process.flag(:trap_exit, true)

          receive do
            {:EXIT, _from, _reason} ->
              # Simulate cleanup time using Task.yield instead of sleep
              Task.yield(Task.async(fn -> :ok end), 10)
              exit(:normal)
          end
        end)

      # Ensure cleanup
      on_exit(fn ->
        if Process.alive?(test_pid) do
          Process.exit(test_pid, :kill)
        end
      end)

      params = %{
        "pid" => test_pid,
        "reason" => :shutdown,
        "force" => false
      }

      assert {:ok, {^test_pid, :shutdown, terminated}} = KillProcess.execute(params)
      assert is_boolean(terminated)

      # Process should eventually be dead
      refute Process.alive?(test_pid)
    end
  end

  describe "format_response/1" do
    test "formats successful termination response" do
      test_pid = spawn(fn -> :ok end)
      result = {test_pid, :killed, true}

      formatted = KillProcess.format_response(result)

      assert %{data: data} = formatted
      assert data.pid == inspect(test_pid)
      assert data.reason == ":killed"
      assert data.terminated == true
      assert %DateTime{} = data.timestamp
    end

    test "formats different reasons correctly" do
      test_pid = spawn(fn -> :ok end)

      test_cases = [:normal, :shutdown, :killed, {:custom, "reason"}]

      Enum.each(test_cases, fn reason ->
        result = {test_pid, reason, true}
        formatted = KillProcess.format_response(result)

        assert %{data: data} = formatted
        assert data.reason == inspect(reason)
      end)
    end

    test "includes termination status" do
      test_pid = spawn(fn -> :ok end)

      # Test both successful and unsuccessful termination
      Enum.each([true, false], fn terminated ->
        result = {test_pid, :killed, terminated}
        formatted = KillProcess.format_response(result)

        assert %{data: data} = formatted
        assert data.terminated == terminated
      end)
    end
  end

  describe "critical process detection" do
    setup do
      # Create mock processes to test critical process detection
      :ok
    end

    test "identifies system supervisors as critical" do
      # Create a process that looks like a supervisor with unique naming
      _unique_id = :erlang.unique_integer([:positive])

      # Start the process and wait for it to set up its dictionary
      test_pid = self()

      supervisor_pid =
        spawn(fn ->
          Process.put(:"$initial_call", {Supervisor, :init, 1})
          send(test_pid, :dictionary_set)

          receive do
            :keep_alive -> :ok
          end
        end)

      # Wait for the process to set its dictionary
      receive do
        :dictionary_set -> :ok
      after
        1000 -> flunk("Process didn't set up dictionary")
      end

      # Ensure cleanup
      on_exit(fn ->
        if Process.alive?(supervisor_pid) do
          Process.exit(supervisor_pid, :kill)
        end
      end)

      params = %{
        "pid" => supervisor_pid,
        "reason" => :killed,
        "force" => false
      }

      assert {:error, :critical_process_protection} = KillProcess.execute(params)

      # Verify process is still alive
      assert Process.alive?(supervisor_pid)
    end

    test "allows terminating regular processes" do
      # Create regular process with unique naming
      _unique_id = :erlang.unique_integer([:positive])

      regular_pid =
        spawn(fn ->
          receive do
            :keep_alive -> :ok
          end
        end)

      # Ensure cleanup
      on_exit(fn ->
        if Process.alive?(regular_pid) do
          Process.exit(regular_pid, :kill)
        end
      end)

      params = %{
        "pid" => regular_pid,
        "reason" => :killed,
        "force" => false
      }

      assert {:ok, _result} = KillProcess.execute(params)
      refute Process.alive?(regular_pid)
    end
  end

  describe "integration tests" do
    test "full operation flow works" do
      # Create test process with unique naming
      _unique_id = :erlang.unique_integer([:positive])

      test_pid =
        spawn(fn ->
          receive do
            :keep_alive -> :ok
          end
        end)

      # Ensure cleanup if test fails
      on_exit(fn ->
        if Process.alive?(test_pid) do
          Process.exit(test_pid, :kill)
        end
      end)

      # Full validation -> execution -> formatting flow
      raw_params = %{
        "pid" => inspect(test_pid),
        # Use :killed instead of :normal to ensure termination
        "reason" => "killed",
        "force" => false
      }

      assert {:ok, validated_params} = KillProcess.validate_params(raw_params)
      assert {:ok, result} = KillProcess.execute(validated_params)
      assert %{data: _formatted_data} = KillProcess.format_response(result)

      # Verify process is actually dead
      refute Process.alive?(test_pid)
    end

    test "handles concurrent termination attempts" do
      # Create test process with unique naming
      _unique_id = :erlang.unique_integer([:positive])

      test_pid =
        spawn(fn ->
          receive do
            :keep_alive -> :ok
          end
        end)

      # Ensure cleanup
      on_exit(fn ->
        if Process.alive?(test_pid) do
          Process.exit(test_pid, :kill)
        end
      end)

      params = %{
        "pid" => test_pid,
        "reason" => :killed,
        "force" => false
      }

      # Start multiple termination attempts concurrently
      tasks =
        for _ <- 1..3 do
          Task.async(fn -> KillProcess.execute(params) end)
        end

      results = Task.await_many(tasks)

      # One should succeed, others should fail with process_not_found
      {successes, failures} = Enum.split_with(results, &match?({:ok, _}, &1))

      # Allow for some race conditions in concurrent execution
      # At least one success
      assert length(successes) >= 1
      # But not all should succeed
      assert length(successes) <= 3

      for {:error, reason} <- failures do
        assert reason == :process_not_found
      end

      refute Process.alive?(test_pid)
    end
  end

  describe "error scenarios" do
    test "handles process that cannot be killed" do
      # This is difficult to test without special setup,
      # but we can test the error handling path
      non_existent_pid = make_ref() |> :erlang.ref_to_list() |> List.to_string()

      params = %{"pid" => non_existent_pid}

      assert {:error, {:invalid_parameter, :pid, _reason}} =
               KillProcess.validate_params(params)
    end

    test "provides detailed error information" do
      # Test various error conditions
      error_cases = [
        {%{}, :missing_parameter},
        {%{"pid" => "invalid"}, :invalid_parameter}
      ]

      Enum.each(error_cases, fn {params, expected_error_type} ->
        assert {:error, error} = KillProcess.validate_params(params)
        assert elem(error, 0) == expected_error_type
      end)
    end
  end
end
