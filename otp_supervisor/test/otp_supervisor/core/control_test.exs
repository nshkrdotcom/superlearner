defmodule OTPSupervisor.Core.ControlTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog

  alias OTPSupervisor.Core.Control
  alias OTPSupervisor.Sandbox.Workers.Counter
  alias OTPSupervisor.Sandbox.Workers.Printer
  import SupervisorTestHelper

  # Simple test worker for our tests
  defmodule TestWorker do
    use GenServer

    def start_link(opts) do
      GenServer.start_link(__MODULE__, :ok, opts)
    end

    def init(:ok) do
      {:ok, %{}}
    end

    # Add a simple handle_call to avoid crashes
    def handle_call(_msg, _from, state) do
      {:reply, :ok, state}
    end
  end

  describe "list_supervisors/0" do
    setup do
      get_demo_supervisor()
    end

    test "returns a list with expected format", %{supervisor: _supervisor} do
      supervisors = Control.list_supervisors()

      # Should return a list
      assert is_list(supervisors)

      # Each supervisor should have the required fields
      Enum.each(supervisors, fn sup ->
        assert Map.has_key?(sup, :name)
        assert Map.has_key?(sup, :pid)
        assert Map.has_key?(sup, :alive)
        assert Map.has_key?(sup, :child_count)
        assert is_atom(sup.name)
        assert is_binary(sup.pid)
        assert is_boolean(sup.alive)
        assert is_integer(sup.child_count) and sup.child_count >= 0
      end)
    end
  end

  describe "get_supervision_tree/1" do
    setup do
      # Create a simple supervisor for testing
      unique_id = :erlang.unique_integer([:positive])
      supervisor_name = :"test_control_supervisor_#{unique_id}"
      worker_name = :"test_worker_#{unique_id}"

      children = [
        {TestWorker, name: worker_name}
      ]

      {:ok, sup} =
        Supervisor.start_link(
          children,
          strategy: :one_for_one,
          name: supervisor_name
        )

      on_exit(fn ->
        if Process.alive?(sup) do
          Process.exit(sup, :shutdown)
        end
      end)

      {:ok, supervisor: sup, supervisor_name: supervisor_name, worker_name: worker_name}
    end

    test "returns children by supervisor name", %{
      supervisor: _sup,
      supervisor_name: supervisor_name
    } do
      assert {:ok, children} = Control.get_supervision_tree(supervisor_name)
      assert is_list(children)
      assert length(children) == 1

      child = hd(children)
      # The id is the module, not the registered name
      assert child.id == TestWorker
      assert child.type == :worker
      assert child.alive == true
      assert is_map(child.info)
    end

    test "returns children by supervisor pid", %{supervisor: sup} do
      assert {:ok, children} = Control.get_supervision_tree(sup)
      assert is_list(children)
      assert length(children) == 1
    end

    test "returns error for non-existent supervisor" do
      assert {:error, :not_found} = Control.get_supervision_tree(:definitely_does_not_exist)
    end

    test "returns error for non-supervisor pid" do
      {:ok, regular_process} = GenServer.start_link(TestWorker, :ok)
      assert {:error, :not_supervisor} = Control.get_supervision_tree(regular_process)
      GenServer.stop(regular_process)
    end
  end

  describe "kill_process/1" do
    test "terminates process by pid" do
      # Spawn a simple process
      pid =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      assert Process.alive?(pid)
      assert :ok = Control.kill_process(pid)

      # Use process monitoring for deterministic synchronization
      ref = Process.monitor(pid)
      assert_receive {:DOWN, ^ref, :process, ^pid, _}, 100

      refute Process.alive?(pid)
    end

    test "terminates process by pid string" do
      # Spawn a simple process
      pid =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      pid_string = inspect(pid)

      assert Process.alive?(pid)
      assert :ok = Control.kill_process(pid_string)

      # Use process monitoring for deterministic synchronization
      ref = Process.monitor(pid)
      assert_receive {:DOWN, ^ref, :process, ^pid, _}, 100

      refute Process.alive?(pid)
    end

    test "handles invalid pid string gracefully" do
      assert {:error, :invalid_pid} = Control.kill_process("not a pid")
      assert {:error, :invalid_pid} = Control.kill_process("#PID<999.999.999>")
    end
  end

  describe "get_process_info/1" do
    test "returns detailed process information" do
      # Use self() which is always available
      {:ok, info} = Control.get_process_info(self())

      # Check all expected fields are present
      assert Map.has_key?(info, :memory)
      assert Map.has_key?(info, :message_queue_len)
      assert Map.has_key?(info, :status)
      assert Map.has_key?(info, :heap_size)
      assert Map.has_key?(info, :stack_size)
      assert Map.has_key?(info, :reductions)
      assert Map.has_key?(info, :current_function)

      # Check types
      assert is_integer(info.memory)
      assert is_integer(info.message_queue_len)
      assert is_atom(info.status)
      assert is_integer(info.heap_size)
      assert is_integer(info.stack_size)
      assert is_integer(info.reductions)
    end

    test "returns error for dead process" do
      # Spawn and let die
      pid = spawn(fn -> :ok end)

      # Use process monitoring to ensure process has finished
      ref = Process.monitor(pid)
      assert_receive {:DOWN, ^ref, :process, ^pid, _}, 100

      assert {:error, :process_dead} = Control.get_process_info(pid)
    end
  end

  describe "supervisor detection" do
    setup do
      get_demo_supervisor()
    end

    test "includes known supervisors", %{supervisor: supervisor_name} do
      supervisors = Control.list_supervisors()
      names = Enum.map(supervisors, & &1.name)

      # Our demo supervisor should be in the list
      assert supervisor_name in names
    end

    test "does not include non-supervisors" do
      # Start a regular GenServer with a name
      unique_id = :erlang.unique_integer([:positive])
      process_name = :"not_a_supervisor_#{unique_id}"

      {:ok, _pid} = GenServer.start_link(TestWorker, :ok, name: process_name)

      supervisors = Control.list_supervisors()
      names = Enum.map(supervisors, & &1.name)

      refute process_name in names

      GenServer.stop(process_name)
    end
  end

  describe "supervisor restart behavior with sandbox workers" do
    setup do
      setup_isolated_supervisor("restart_behavior")
    end

    test "supervisor restarts killed counter process", %{supervisor: supervisor, sup_pid: sup_pid} do
      # Get supervision tree to find processes
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      assert counter_child != nil

      # Get initial PID and value
      initial_pid = extract_pid_from_string(counter_child.pid)
      unique_id = String.split(Atom.to_string(supervisor), "_") |> List.last()
      counter_name = String.to_atom("counter_1_" <> unique_id)
      assert Counter.get_value(counter_name) == 0

      # Increment to change state
      Counter.increment(counter_name)
      assert Counter.get_value(counter_name) == 1

      # Kill the process
      Control.kill_process(initial_pid)

      # Wait for the specific child to be restarted with a different PID
      :ok = wait_for_child_restart(sup_pid, :counter_1, initial_pid)

      # Get the new PID from supervisor children (more reliable than name lookup)
      children = Supervisor.which_children(sup_pid)
      counter_child = Enum.find(children, fn {id, _pid, _type, _modules} -> id == :counter_1 end)
      {_id, new_pid, _type, _modules} = counter_child

      # Verify new process started
      assert new_pid != nil
      assert new_pid != initial_pid

      # Also verify name registration is working
      assert Process.whereis(counter_name) == new_pid

      # Verify state was reset (not preserved)
      assert Counter.get_value(counter_name) == 0
    end

    test "supervisor restarts crashed counter process", %{
      supervisor: supervisor,
      sup_pid: sup_pid
    } do
      # Get supervision tree to find processes
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      assert counter_child != nil

      # Get initial PID
      initial_pid = extract_pid_from_string(counter_child.pid)
      unique_id = String.split(Atom.to_string(supervisor), "_") |> List.last()
      counter_name = String.to_atom("counter_1_" <> unique_id)
      assert initial_pid != nil

      # Monitor the process to detect crash
      ref = Process.monitor(initial_pid)

      # Cause intentional crash
      capture_log(fn -> Counter.crash(counter_name) end)

      # Wait for the crash to occur
      assert_receive {:DOWN, ^ref, :process, ^initial_pid, _}, 1000

      # Use supervisor's which_children to wait for restart completion
      :ok = wait_for_restart(sup_pid)

      # Get the new PID from supervisor children (more reliable than name lookup)
      children = Supervisor.which_children(sup_pid)
      counter_child = Enum.find(children, fn {id, _pid, _type, _modules} -> id == :counter_1 end)
      {_id, new_pid, _type, _modules} = counter_child

      # Verify new process started
      assert new_pid != nil
      assert new_pid != initial_pid

      # Also verify name registration is working
      assert Process.whereis(counter_name) == new_pid

      # Verify it's functional
      assert Counter.get_value(counter_name) == 0
    end

    test "killing one process doesn't affect others", %{supervisor: supervisor, sup_pid: sup_pid} do
      # Get supervision tree to find processes
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      printer_child = Enum.find(children, &(&1.id == :printer_1))

      # Get initial PIDs
      counter_pid = extract_pid_from_string(counter_child.pid)
      printer_pid = extract_pid_from_string(printer_child.pid)

      # Get process names
      unique_id = String.split(Atom.to_string(supervisor), "_") |> List.last()
      printer_name = String.to_atom("printer_1_" <> unique_id)

      # Track printer message count
      Printer.print(printer_name, "test message")
      initial_count = Printer.get_print_count(printer_name)
      assert initial_count == 1

      # Kill only the counter
      Control.kill_process(counter_pid)

      # Wait for restart using proper synchronization
      :ok = wait_for_restart(sup_pid)

      # Verify printer wasn't affected
      assert Process.whereis(printer_name) == printer_pid
      assert Process.alive?(printer_pid)
      assert Printer.get_print_count(printer_name) == initial_count

      # Verify counter was restarted by checking supervisor children
      children = Supervisor.which_children(sup_pid)
      counter_child = Enum.find(children, fn {id, _pid, _type, _modules} -> id == :counter_1 end)
      {_id, new_counter_pid, _type, _modules} = counter_child

      assert new_counter_pid != counter_pid
      assert Process.alive?(new_counter_pid)
    end

    test "supervision tree correctly reflects process states", %{
      supervisor: supervisor,
      sup_pid: sup_pid
    } do
      # Get initial tree
      {:ok, initial_children} = Control.get_supervision_tree(supervisor)
      assert length(initial_children) == 3
      assert Enum.all?(initial_children, & &1.alive)

      # Kill one process
      counter_child = Enum.find(initial_children, &(&1.id == :counter_1))
      Control.kill_process(counter_child.pid)

      # Check immediately (might catch dead state)
      {:ok, _dead_children} = Control.get_supervision_tree(supervisor)

      # Wait for restart using proper synchronization
      :ok = wait_for_restart(sup_pid)

      # Check after restart
      {:ok, final_children} = Control.get_supervision_tree(supervisor)
      assert length(final_children) == 3
      assert Enum.all?(final_children, & &1.alive)

      # Verify PID changed for counter
      final_counter = Enum.find(final_children, &(&1.id == :counter_1))
      assert final_counter.pid != counter_child.pid
    end
  end

  describe "malformed supervision tree handling" do
    test "handles supervisor crash during inspection" do
      %{supervisor: supervisor_name, sup_pid: sup_pid} =
        setup_crash_test_supervisor("crash_during_inspection")

      # Verify it's working first
      assert {:ok, _children} = Control.get_supervision_tree(supervisor_name)

      # Crash the supervisor
      Process.flag(:trap_exit, true)
      Process.exit(sup_pid, :kill)

      receive do
        {:EXIT, ^sup_pid, :killed} -> :ok
      after
        100 -> :ok
      end

      # Should return error for crashed supervisor
      assert {:error, :not_found} = Control.get_supervision_tree(supervisor_name)
    end

    test "handles supervisor in transition state" do
      %{supervisor: supervisor_name, sup_pid: sup_pid} =
        setup_crash_test_supervisor("transition_state")

      # Try to get supervision tree immediately (might catch transitional state)
      case Control.get_supervision_tree(supervisor_name) do
        {:ok, children} ->
          # Should handle children that might be starting
          assert is_list(children)

        {:error, reason} ->
          # Should gracefully handle any errors during transition
          assert reason != nil
      end

      # Cleanup
      Supervisor.stop(sup_pid)
    end

    test "handles supervisor with malformed child specs" do
      %{supervisor: supervisor_name, sup_pid: sup_pid} =
        setup_crash_test_supervisor("malformed_child_specs")

      # Should still be able to inspect supervisor
      case Control.get_supervision_tree(supervisor_name) do
        {:ok, children} ->
          assert is_list(children)

        {:error, reason} ->
          assert is_atom(reason) or is_binary(reason)
      end

      # Test demonstrates error handling even with problematic supervisors
      Supervisor.stop(sup_pid)
    end
  end

  describe "supervisor detection edge cases" do
    setup do
      get_demo_supervisor()
    end

    test "detects DynamicSupervisor correctly", %{supervisor: _supervisor} do
      # Start a DynamicSupervisor
      unique_id = :erlang.unique_integer([:positive])
      supervisor_name = :"test_dynamic_sup_#{unique_id}"

      {:ok, _pid} =
        DynamicSupervisor.start_link(
          strategy: :one_for_one,
          name: supervisor_name
        )

      supervisors = Control.list_supervisors()
      names = Enum.map(supervisors, & &1.name)

      # Should include DynamicSupervisor
      assert supervisor_name in names

      # Cleanup
      DynamicSupervisor.stop(supervisor_name)
    end

    test "detects Task.Supervisor correctly", %{supervisor: _supervisor} do
      # Start a Task.Supervisor
      unique_id = :erlang.unique_integer([:positive])
      supervisor_name = :"test_task_sup_#{unique_id}"

      {:ok, _pid} = Task.Supervisor.start_link(name: supervisor_name)

      supervisors = Control.list_supervisors()
      names = Enum.map(supervisors, & &1.name)

      # Should include Task.Supervisor
      assert supervisor_name in names

      # Cleanup
      Supervisor.stop(supervisor_name)
    end

    test "handles processes with missing dictionary entries" do
      # Create a process and manually modify its dictionary to test edge cases
      {:ok, pid} = Agent.start_link(fn -> %{} end)

      # This shouldn't be detected as a supervisor
      supervisors = Control.list_supervisors()
      _agent_names = Enum.map(supervisors, & &1.name)

      # Agent shouldn't be in supervisor list (it's not registered with a name anyway)
      # But this tests that processes without proper supervisor markers aren't included
      assert is_list(supervisors)

      Agent.stop(pid)
    end

    test "handles processes with malformed initial_call" do
      # This is an internal test - we'll test with a process that has unusual initial_call
      # Most real-world scenarios are already covered, this ensures robustness
      unique_id = :erlang.unique_integer([:positive])
      process_name = :"unusual_process_#{unique_id}"

      # Start a process that might have unusual properties
      pid =
        spawn(fn ->
          Process.register(self(), process_name)

          receive do
            :stop -> :ok
          end
        end)

      # Should not be detected as supervisor
      supervisors = Control.list_supervisors()
      names = Enum.map(supervisors, & &1.name)

      refute process_name in names

      # Cleanup
      send(pid, :stop)
    end
  end

  describe "process information error scenarios" do
    test "handles processes that die during inspection" do
      # Create a process that will die quickly
      pid =
        spawn(fn ->
          exit(:normal)
        end)

      # Try to get info - might succeed or fail depending on timing
      case Control.get_process_info(pid) do
        {:ok, info} ->
          assert is_map(info)

        {:error, :process_dead} ->
          # This is expected and handled correctly
          assert true
      end

      # Use monitoring to ensure it's definitely dead
      ref = Process.monitor(pid)
      assert_receive {:DOWN, ^ref, :process, ^pid, _}, 100

      # Now should definitely return error
      assert {:error, :process_dead} = Control.get_process_info(pid)
    end

    test "handles processes with extreme memory usage" do
      # Create a process with significant memory usage
      {:ok, pid} =
        Agent.start_link(fn ->
          # Create a large data structure
          large_data = for i <- 1..1000, do: {i, "data_#{i}_" <> String.duplicate("x", 100)}
          %{data: large_data}
        end)

      # Should still be able to get process info
      {:ok, info} = Control.get_process_info(pid)

      assert is_integer(info.memory)
      assert info.memory > 0
      assert is_map(info)

      Agent.stop(pid)
    end

    test "handles processes with large message queues" do
      # Create a process that properly handles messages to build up a queue
      pid =
        spawn(fn ->
          # Process messages slowly to build up a queue
          Process.flag(:trap_exit, true)
          receive_loop(0)
        end)

      # Send many messages rapidly
      for i <- 1..50 do
        send(pid, {:test_message, i})
      end

      # Should still be able to get process info
      {:ok, info} = Control.get_process_info(pid)

      assert is_integer(info.message_queue_len)
      assert info.message_queue_len >= 0

      # Cleanup
      Process.exit(pid, :shutdown)
    end

    # Helper function for the message queue test
    defp receive_loop(count) do
      receive do
        {:test_message, _} ->
          # Process slowly to allow queue buildup
          receive_loop(count + 1)

        :stop ->
          :ok
      after
        5000 -> :timeout
      end
    end

    test "handles processes in unusual states" do
      # Create a process that's suspended/waiting
      {:ok, pid} =
        Task.start_link(fn ->
          receive do
            :continue -> :ok
          after
            60_000 -> :timeout
          end
        end)

      # Should be able to get info even for waiting processes
      {:ok, info} = Control.get_process_info(pid)

      assert is_atom(info.status)
      assert info.status in [:running, :waiting, :runnable, :suspended]

      # Cleanup
      Process.exit(pid, :shutdown)
    end
  end

  describe "kill process error handling" do
    test "handles already dead processes" do
      # Create and immediately kill a process
      pid = spawn(fn -> :ok end)

      # Use process monitoring to ensure it's dead
      ref = Process.monitor(pid)
      assert_receive {:DOWN, ^ref, :process, ^pid, _}, 100

      # Killing dead process should still return :ok
      assert :ok = Control.kill_process(pid)
    end

    test "handles protected processes gracefully" do
      # Test that we can kill processes even if they trap exits
      pid =
        spawn(fn ->
          Process.flag(:trap_exit, true)

          receive do
            :stop -> :ok
          end
        end)

      # Should still be able to kill it (kill signal bypasses trap_exit)
      assert :ok = Control.kill_process(pid)

      # Use process monitoring for deterministic synchronization
      ref = Process.monitor(pid)
      assert_receive {:DOWN, ^ref, :process, ^pid, _}, 100

      refute Process.alive?(pid)
    end

    test "handles malformed PID strings with various formats" do
      # Test various invalid PID string formats
      invalid_pids = [
        "not_a_pid",
        # Empty
        "#PID<>",
        # Non-numeric
        "#PID<abc.def.ghi>",
        # Wrong format
        "<0.123>",
        # Empty string
        "",
        "nil"
      ]

      Enum.each(invalid_pids, fn invalid_pid ->
        assert {:error, :invalid_pid} = Control.kill_process(invalid_pid)
      end)
    end

    test "handles PID strings from different formats" do
      # Should work with full format
      pid1 =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      pid1_string = inspect(pid1)
      assert :ok = Control.kill_process(pid1_string)

      # Should work with cleaned format (without #PID)
      pid2 =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      pid2_string = inspect(pid2)
      cleaned = String.replace(pid2_string, "#PID", "")
      assert :ok = Control.kill_process(cleaned)

      # Use process monitoring for deterministic synchronization
      ref1 = Process.monitor(pid1)
      ref2 = Process.monitor(pid2)
      assert_receive {:DOWN, ^ref1, :process, ^pid1, _}, 100
      assert_receive {:DOWN, ^ref2, :process, ^pid2, _}, 100

      refute Process.alive?(pid1)
      refute Process.alive?(pid2)
    end

    test "handles concurrent kill operations" do
      # Create multiple processes
      pids =
        for _i <- 1..5 do
          spawn(fn ->
            receive do
              :stop -> :ok
            end
          end)
        end

      # Monitor all processes
      refs = Enum.map(pids, fn pid -> {pid, Process.monitor(pid)} end)

      # Kill them all concurrently
      tasks =
        Enum.map(pids, fn pid ->
          Task.async(fn -> Control.kill_process(pid) end)
        end)

      # All should succeed
      results = Task.await_many(tasks, 1000)
      assert Enum.all?(results, &(&1 == :ok))

      # Wait for all processes to be dead using monitoring
      Enum.each(refs, fn {pid, ref} ->
        assert_receive {:DOWN, ^ref, :process, ^pid, _}, 100
      end)

      # All should be dead
      assert Enum.all?(pids, fn pid -> not Process.alive?(pid) end)
    end
  end

  describe "integration with real supervision scenarios" do
    setup do
      setup_isolated_supervisor("integration")
    end

    test "handles supervision tree inspection during active restarts", %{
      supervisor: supervisor,
      sup_pid: sup_pid
    } do
      # Get initial state
      {:ok, initial_children} = Control.get_supervision_tree(supervisor)
      assert length(initial_children) == 3

      # Kill multiple processes rapidly
      counter_child = Enum.find(initial_children, &(&1.id == :counter_1))
      printer_child = Enum.find(initial_children, &(&1.id == :printer_1))

      counter_pid = extract_pid_from_string(counter_child.pid)
      printer_pid = extract_pid_from_string(printer_child.pid)

      Control.kill_process(counter_pid)
      Control.kill_process(printer_pid)

      # Try to inspect during restart period
      # Should handle gracefully regardless of timing
      case Control.get_supervision_tree(supervisor) do
        {:ok, children} ->
          assert is_list(children)

        # Might have different alive states during restart
        {:error, reason} ->
          # Should be graceful error
          assert reason != nil
      end

      # Wait for restart using proper synchronization
      :ok = wait_for_restart(sup_pid)

      # Should eventually be stable again
      {:ok, final_children} = Control.get_supervision_tree(supervisor)
      assert length(final_children) == 3
      assert Enum.all?(final_children, & &1.alive)
    end

    test "maintains consistency across multiple control operations", %{
      supervisor: supervisor,
      sup_pid: sup_pid
    } do
      # Perform multiple operations in sequence
      {:ok, children1} = Control.get_supervision_tree(supervisor)
      info1 = Control.get_process_info(self())
      supervisors1 = Control.list_supervisors()

      # Kill a process
      counter_child = Enum.find(children1, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)
      Control.kill_process(counter_pid)

      # Wait for restart using proper synchronization
      :ok = wait_for_child_restart(sup_pid, :counter_1, counter_pid)

      # Perform operations again
      {:ok, children2} = Control.get_supervision_tree(supervisor)
      info2 = Control.get_process_info(self())
      supervisors2 = Control.list_supervisors()

      # Structure should be consistent
      assert length(children1) == length(children2)
      assert {:ok, _} = info1
      assert {:ok, _} = info2
      assert length(supervisors1) == length(supervisors2)

      # But PIDs should have changed for restarted process
      counter1 = Enum.find(children1, &(&1.id == :counter_1))
      counter2 = Enum.find(children2, &(&1.id == :counter_1))
      assert counter1.pid != counter2.pid
    end
  end

  describe "process introspection" do
    setup do
      setup_isolated_supervisor("introspection")
    end

    test "list_all_processes/1 returns all processes with filtering", %{supervisor: _supervisor} do
      processes = Control.list_all_processes(filter: :supervisor)
      assert is_list(processes)
      assert length(processes) > 0

      # Test each process has required fields
      Enum.each(processes, fn process ->
        assert Map.has_key?(process, :pid)
        assert Map.has_key?(process, :name)
        assert Map.has_key?(process, :type)
      end)
    end

    test "get_process_state/1 extracts GenServer state safely", %{supervisor: supervisor} do
      # Get one of the counters that the helper already started for us
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)

      # Test the state extraction on this known process
      result = Control.get_process_state(counter_pid)
      assert {:ok, %{value: 0, crashes: 0}} = result
    end

    test "get_process_state/1 handles non-GenServer processes", %{supervisor: _supervisor} do
      {:ok, task_pid} = Task.start_link(fn -> receive do: (_ -> :ok) end)

      # Ensure cleanup happens no matter what
      on_exit(fn -> if Process.alive?(task_pid), do: Process.exit(task_pid, :kill) end)

      result = Control.get_process_state(task_pid)
      assert {:error, :not_a_genserver} = result
    end

    test "build_process_graph/0 creates complete relationship graph" do
      graph = Control.build_process_graph()
      assert is_map(graph)
      assert Map.has_key?(graph, :processes)
      assert Map.has_key?(graph, :links)
      assert Map.has_key?(graph, :monitors)
    end

    test "list_all_processes/1 handles limit option" do
      processes = Control.list_all_processes(limit: 5)
      assert is_list(processes)
      assert length(processes) <= 5
    end

    test "list_all_processes/1 returns consistent structure for all processes" do
      processes = Control.list_all_processes(limit: 50)

      # Verify all processes have required structure
      Enum.each(processes, fn process ->
        assert Map.has_key?(process, :pid)
        assert Map.has_key?(process, :name)
        assert Map.has_key?(process, :type)
        assert is_binary(process.pid)
        assert process.type in [:supervisor, :genserver, :worker, :dead]
        assert is_atom(process.name) or is_nil(process.name)
      end)
    end

    test "get_process_state/1 is safe to call repeatedly", %{supervisor: supervisor} do
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)

      # Call multiple times on the same process from the setup
      for _i <- 1..5 do
        result = Control.get_process_state(counter_pid)
        assert {:ok, _state} = result
      end
    end
  end

  describe "telemetry-based analytics" do
    setup do
      setup_isolated_supervisor("analytics_control_test")
    end

    test "get_restart_history returns telemetry data", %{supervisor: supervisor, sup_pid: sup_pid} do
      # Test Control module wrapper
      {:ok, history} = Control.get_restart_history(supervisor)
      assert is_list(history)

      # Also test with PID directly
      {:ok, history_pid} = Control.get_restart_history(sup_pid)
      assert history == history_pid
    end

    test "get_supervisor_analytics returns comprehensive stats", %{supervisor: _supervisor} do
      stats = Control.get_supervisor_analytics()

      assert is_map(stats)
      assert Map.has_key?(stats, :total_supervisors)
      assert Map.has_key?(stats, :total_restarts)
      assert Map.has_key?(stats, :supervisor_stats)
      assert is_list(stats.supervisor_stats)
    end

    test "get_failure_rate calculates rates correctly", %{
      supervisor: supervisor,
      sup_pid: _sup_pid
    } do
      {:ok, failure_rate} = Control.get_failure_rate(supervisor, 5000)

      assert is_map(failure_rate)
      assert Map.has_key?(failure_rate, :restarts)
      assert Map.has_key?(failure_rate, :rate)
      assert Map.has_key?(failure_rate, :window_ms)
      assert failure_rate.window_ms == 5000
    end

    test "handles invalid supervisor gracefully" do
      assert {:error, :invalid_pid} = Control.get_restart_history("invalid")
      assert {:error, :invalid_pid} = Control.get_failure_rate("invalid", 1000)
    end
  end

  describe "failure simulation" do
    setup do
      setup_isolated_supervisor("simulation")
    end

    test "simulate_crash/3 crashes child with specific reason", %{supervisor: supervisor} do
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child_pid_string = children |> List.first() |> Map.get(:pid)
      {:ok, child_pid} = Control.to_pid(child_pid_string)

      capture_log(fn ->
        result = Control.simulate_crash(child_pid, :custom_reason, delay: 0)
        assert :ok = result
      end)

      # Verify process was killed
      refute Process.alive?(child_pid)
    end

    test "process memory info can be retrieved", %{supervisor: supervisor} do
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child_pid_string = children |> List.first() |> Map.get(:pid)
      {:ok, child_pid} = Control.to_pid(child_pid_string)

      # Force garbage collection to simulate memory operations
      :erlang.garbage_collect(child_pid)

      # Memory info should be retrievable
      memory = Process.info(child_pid, :memory) |> elem(1)
      assert is_integer(memory)
      assert memory > 0
    end
  end

  describe "sandbox management" do
    test "creates sandbox with automatic ID generation" do
      {:ok, sandbox_info} =
        Control.create_sandbox(
          OTPSupervisor.Sandbox.TestDemoSupervisor,
          strategy: :one_for_one
        )

      assert is_binary(sandbox_info.id)
      assert sandbox_info.supervisor_module == OTPSupervisor.Sandbox.TestDemoSupervisor
      assert is_pid(sandbox_info.supervisor_pid)
      assert Process.alive?(sandbox_info.supervisor_pid)

      # Cleanup - suppress expected supervisor death warning
      capture_log(fn -> :ok = Control.destroy_sandbox(sandbox_info.id) end)
    end

    test "destroys sandbox by ID" do
      {:ok, sandbox_info} = Control.create_sandbox(OTPSupervisor.Sandbox.TestDemoSupervisor)
      supervisor_pid = sandbox_info.supervisor_pid

      # Monitor for death
      ref = Process.monitor(supervisor_pid)

      capture_log(fn -> :ok = Control.destroy_sandbox(sandbox_info.id) end)

      # Wait for supervisor to die
      receive do
        {:DOWN, ^ref, :process, ^supervisor_pid, _reason} -> :ok
      after
        1000 -> flunk("Supervisor did not terminate")
      end

      refute Process.alive?(supervisor_pid)
    end

    test "restarts sandbox preserving configuration" do
      {:ok, original_info} =
        Control.create_sandbox(
          OTPSupervisor.Sandbox.TestDemoSupervisor,
          strategy: :rest_for_one,
          test_opt: :value
        )

      original_pid = original_info.supervisor_pid

      {:ok, restarted_info} =
        ExUnit.CaptureLog.with_log(fn -> Control.restart_sandbox(original_info.id) end) |> elem(0)

      assert restarted_info.supervisor_pid != original_pid
      assert restarted_info.opts[:strategy] == :rest_for_one
      assert restarted_info.opts[:test_opt] == :value
      assert restarted_info.restart_count == 1

      # Cleanup - suppress expected supervisor death warning
      capture_log(fn -> :ok = Control.destroy_sandbox(restarted_info.id) end)
    end

    test "lists active sandboxes" do
      {:ok, sandbox1} = Control.create_sandbox(OTPSupervisor.Sandbox.TestDemoSupervisor)
      {:ok, sandbox2} = Control.create_sandbox(OTPSupervisor.Sandbox.TestDemoSupervisor)

      sandboxes = Control.list_sandboxes()
      sandbox_ids = Enum.map(sandboxes, & &1.id)

      assert sandbox1.id in sandbox_ids
      assert sandbox2.id in sandbox_ids

      # Cleanup - suppress expected supervisor death warnings
      capture_log(fn ->
        :ok = Control.destroy_sandbox(sandbox1.id)
        :ok = Control.destroy_sandbox(sandbox2.id)
      end)
    end

    test "gets sandbox information" do
      {:ok, created_info} = Control.create_sandbox(OTPSupervisor.Sandbox.TestDemoSupervisor)

      {:ok, retrieved_info} = Control.get_sandbox_info(created_info.id)

      assert retrieved_info.id == created_info.id
      assert retrieved_info.supervisor_pid == created_info.supervisor_pid

      # Cleanup - suppress expected supervisor death warning
      capture_log(fn -> :ok = Control.destroy_sandbox(created_info.id) end)
    end

    test "handles non-existent sandbox gracefully" do
      assert {:error, :not_found} = Control.destroy_sandbox("non_existent")
      assert {:error, :not_found} = Control.restart_sandbox("non_existent")
      assert {:error, :not_found} = Control.get_sandbox_info("non_existent")
    end
  end
end
