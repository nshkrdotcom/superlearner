defmodule OTPSupervisor.Core.MessageTracerTest do
  use ExUnit.Case, async: true
  
  alias OTPSupervisor.Core.MessageTracer
  alias OTPSupervisor.Core.Control
  
  import SupervisorTestHelper

  setup_all do
    # Ensure TracerRegistry is started
    case Registry.start_link(keys: :unique, name: TracerRegistry) do
      {:ok, _} -> :ok
      {:error, {:already_started, _}} -> :ok
    end
    :ok
  end
  
  describe "message tracing" do
    setup do
      SupervisorTestHelper.setup_isolated_supervisor("message_tracing")
    end

    test "trace_messages/2 starts tracing successfully", %{supervisor: supervisor} do
      # Get a child process from the isolated supervisor
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)
      
      result = MessageTracer.trace_messages(counter_pid, max_messages: 10)
      assert {:ok, tracer_pid} = result
      assert Process.alive?(tracer_pid)
    end

    test "captures GenServer calls and casts", %{supervisor: supervisor} do
      # Get a child process from the isolated supervisor
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)
      
      {:ok, _tracer} = MessageTracer.trace_messages(counter_pid, max_messages: 5)
      
      # Generate some messages
      GenServer.cast(counter_pid, :increment)
      GenServer.cast(counter_pid, :increment)
      
      # Use synchronous call to ensure all previous casts are processed
      final_value = GenServer.call(counter_pid, :get_value)
      assert final_value == 2
      
      messages = MessageTracer.get_message_history(counter_pid)
      assert length(messages) >= 2
      
      # Verify message structure
      Enum.each(messages, fn msg ->
        assert Map.has_key?(msg, :timestamp)
        assert Map.has_key?(msg, :direction)
        assert Map.has_key?(msg, :content)
      end)
    end

    test "stops tracing properly", %{supervisor: supervisor} do
      # Get a child process from the isolated supervisor
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)
      
      {:ok, tracer_pid} = MessageTracer.trace_messages(counter_pid, max_messages: 5)
      
      result = MessageTracer.stop_tracing(counter_pid)
      assert :ok = result
      
      # Verify tracer process is cleaned up
      refute Process.alive?(tracer_pid)
    end

    test "handles max_messages limit", %{supervisor: supervisor} do
      # Get a child process from the isolated supervisor
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)
      
      {:ok, _tracer} = MessageTracer.trace_messages(counter_pid, max_messages: 2)
      
      # Generate more messages than limit
      GenServer.cast(counter_pid, :increment)
      GenServer.cast(counter_pid, :increment)
      GenServer.cast(counter_pid, :increment)
      GenServer.cast(counter_pid, :increment)
      
      # Synchronize to ensure all messages processed
      GenServer.call(counter_pid, :get_value)
      
      messages = MessageTracer.get_message_history(counter_pid)
      assert length(messages) <= 2
    end

    test "handles tracing non-existent process", %{supervisor: _supervisor} do
      fake_pid = spawn(fn -> :ok end)
      Process.exit(fake_pid, :kill)
      
      result = MessageTracer.trace_messages(fake_pid, max_messages: 10)
      assert {:error, _reason} = result
    end

    test "handles multiple tracers for different processes", %{supervisor: supervisor} do
      # Get two different child processes from the isolated supervisor
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter1_child = Enum.find(children, &(&1.id == :counter_1))
      counter2_child = Enum.find(children, &(&1.id == :printer_1))
      
      counter_pid_1 = extract_pid_from_string(counter1_child.pid)
      counter_pid_2 = extract_pid_from_string(counter2_child.pid)
      
      {:ok, tracer1} = MessageTracer.trace_messages(counter_pid_1, max_messages: 5)
      {:ok, tracer2} = MessageTracer.trace_messages(counter_pid_2, max_messages: 5)
      
      assert tracer1 != tracer2
      assert Process.alive?(tracer1)
      assert Process.alive?(tracer2)
      
      # Cleanup
      MessageTracer.stop_tracing(counter_pid_2)
    end
  end

  describe "message analysis" do
    test "analyze_message_patterns/1 identifies common patterns" do
      # Create mock message history
      messages = [
        %{content: {:cast, :increment}, timestamp: 1000, direction: :incoming},
        %{content: {:call, :get_value}, timestamp: 1001, direction: :incoming},
        %{content: {:cast, :increment}, timestamp: 1002, direction: :incoming},
        %{content: {:cast, :increment}, timestamp: 1003, direction: :incoming}
      ]
      
      patterns = MessageTracer.analyze_message_patterns(messages)
      
      assert Map.has_key?(patterns, :cast_frequency)
      assert Map.has_key?(patterns, :call_frequency)
      assert patterns.cast_frequency > patterns.call_frequency
    end

    test "analyze_message_patterns/1 handles empty message list" do
      patterns = MessageTracer.analyze_message_patterns([])
      
      assert %{cast_frequency: 0, call_frequency: 0, total_messages: 0} = patterns
    end

    test "analyze_message_patterns/1 calculates message timing stats" do
      messages = [
        %{content: {:cast, :increment}, timestamp: 1000, direction: :incoming},
        %{content: {:call, :get_value}, timestamp: 1100, direction: :incoming},
        %{content: {:cast, :increment}, timestamp: 1200, direction: :incoming}
      ]
      
      patterns = MessageTracer.analyze_message_patterns(messages)
      
      assert Map.has_key?(patterns, :avg_message_interval)
      assert patterns.avg_message_interval == 100.0
    end
  end

  describe "tracer registry" do
    setup do
      SupervisorTestHelper.setup_isolated_supervisor("tracer_registry")
    end

    test "registry tracks tracers correctly", %{supervisor: supervisor} do
      # Get a child process from the isolated supervisor
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)
      
      {:ok, tracer_pid} = MessageTracer.trace_messages(counter_pid, max_messages: 5)
      
      # Verify tracer is registered
      case Registry.lookup(TracerRegistry, counter_pid) do
        [{registered_tracer, _}] ->
          assert registered_tracer == tracer_pid
        [] ->
          flunk("Tracer not registered")
      end
      
      # Stop tracing - this should be synchronous
      MessageTracer.stop_tracing(counter_pid)
      
      # Verify tracer is unregistered
      assert Registry.lookup(TracerRegistry, counter_pid) == []
    end
  end

  describe "error handling" do
    setup do
      SupervisorTestHelper.setup_isolated_supervisor("error_handling")
    end

    test "handles tracer process crash gracefully", %{supervisor: supervisor} do
      # Get a child process from the isolated supervisor
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)
      
      {:ok, tracer_pid} = MessageTracer.trace_messages(counter_pid, max_messages: 5)
      
      # Kill the tracer process
      Process.exit(tracer_pid, :kill)
      
      # Verify cleanup happens
      refute Process.alive?(tracer_pid)
      
      # Should be able to start a new tracer
      {:ok, new_tracer} = MessageTracer.trace_messages(counter_pid, max_messages: 5)
      assert Process.alive?(new_tracer)
      
      # Cleanup
      MessageTracer.stop_tracing(counter_pid)
    end

    test "handles getting messages from non-traced process", %{supervisor: supervisor} do
      # Get a child process from the isolated supervisor but don't trace it
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)
      
      messages = MessageTracer.get_message_history(counter_pid)
      assert messages == []
    end
  end
end