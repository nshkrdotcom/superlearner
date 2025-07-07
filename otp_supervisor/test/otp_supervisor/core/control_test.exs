defmodule OTPSupervisor.Core.ControlTest do
  use ExUnit.Case
  
  alias OTPSupervisor.Core.Control
  alias OTPSupervisor.Sandbox.Workers.Counter
  alias OTPSupervisor.Sandbox.Workers.Printer

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
    test "returns a list with expected format" do
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
      children = [
        {TestWorker, name: :test_worker_1}
      ]
      
      {:ok, sup} = Supervisor.start_link(
        children, 
        strategy: :one_for_one,
        name: :test_control_supervisor
      )
      
      on_exit(fn ->
        if Process.alive?(sup) do
          Process.exit(sup, :shutdown)
          Process.sleep(10)
        end
      end)
      
      {:ok, supervisor: sup}
    end

    test "returns children by supervisor name", %{supervisor: _sup} do
      assert {:ok, children} = Control.get_supervision_tree(:test_control_supervisor)
      assert is_list(children)
      assert length(children) == 1
      
      child = hd(children)
      assert child.id == TestWorker  # The id is the module, not the registered name
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
      pid = spawn(fn -> 
        receive do
          :stop -> :ok
        end
      end)
      
      assert Process.alive?(pid)
      assert :ok = Control.kill_process(pid)
      
      # Give it a moment
      Process.sleep(10)
      
      refute Process.alive?(pid)
    end

    test "terminates process by pid string" do
      # Spawn a simple process
      pid = spawn(fn -> 
        receive do
          :stop -> :ok
        end
      end)
      
      pid_string = inspect(pid)
      
      assert Process.alive?(pid)
      assert :ok = Control.kill_process(pid_string)
      
      # Give it a moment
      Process.sleep(10)
      
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
      Process.sleep(10)  # Ensure process has finished
      
      assert {:error, :process_dead} = Control.get_process_info(pid)
    end
  end

  describe "supervisor detection" do
    setup do
      # Create a test supervisor
      {:ok, sup} = Supervisor.start_link([], strategy: :one_for_one, name: :detection_test_sup)
      
      on_exit(fn ->
        if Process.alive?(sup) do
          Process.exit(sup, :shutdown)
          Process.sleep(10)
        end
      end)
      
      :ok
    end

    test "includes known supervisors" do
      supervisors = Control.list_supervisors()
      names = Enum.map(supervisors, & &1.name)
      
      # Our test supervisor should be in the list
      assert :detection_test_sup in names
    end

    test "does not include non-supervisors" do
      # Start a regular GenServer with a name
      {:ok, _pid} = GenServer.start_link(TestWorker, :ok, name: :not_a_supervisor)
      
      supervisors = Control.list_supervisors()
      names = Enum.map(supervisors, & &1.name)
      
      refute :not_a_supervisor in names
      
      GenServer.stop(:not_a_supervisor)
    end
  end

  describe "supervisor restart behavior with sandbox workers" do
    setup do
      # Create a supervisor with sandbox workers
      children = [
        {Counter, name: :test_counter_restart, initial_value: 42},
        {Printer, name: :test_printer_restart, id: "test-printer"}
      ]
      
      {:ok, sup} = Supervisor.start_link(
        children, 
        strategy: :one_for_one,
        name: :test_restart_supervisor
      )
      
      on_exit(fn ->
        try do
          if Process.whereis(:test_restart_supervisor) do
            Supervisor.stop(sup, :shutdown, 5000)
          end
        catch
          :exit, _ -> :ok
        end
      end)
      
      {:ok, supervisor: sup}
    end

    test "supervisor restarts killed counter process", %{supervisor: _sup} do
      # Get initial PID and value
      initial_pid = Process.whereis(:test_counter_restart)
      assert initial_pid != nil
      assert Counter.get_value(:test_counter_restart) == 42
      
      # Increment to change state
      Counter.increment(:test_counter_restart)
      assert Counter.get_value(:test_counter_restart) == 43
      
      # Kill the process
      Control.kill_process(initial_pid)
      
      # Wait for restart
      Process.sleep(50)
      
      # Verify new process started
      new_pid = Process.whereis(:test_counter_restart)
      assert new_pid != nil
      assert new_pid != initial_pid
      
      # Verify state was reset (not preserved)
      assert Counter.get_value(:test_counter_restart) == 42
    end

    test "supervisor restarts crashed counter process", %{supervisor: _sup} do
      # Get initial PID
      initial_pid = Process.whereis(:test_counter_restart)
      assert initial_pid != nil
      
      # Cause intentional crash
      Counter.crash(:test_counter_restart)
      
      # Wait for restart
      Process.sleep(50)
      
      # Verify new process started
      new_pid = Process.whereis(:test_counter_restart)
      assert new_pid != nil
      assert new_pid != initial_pid
      
      # Verify it's functional
      assert Counter.get_value(:test_counter_restart) == 42
    end

    test "killing one process doesn't affect others", %{supervisor: _sup} do
      # Get initial PIDs
      counter_pid = Process.whereis(:test_counter_restart)
      printer_pid = Process.whereis(:test_printer_restart)
      
      # Track printer message count
      Printer.print(:test_printer_restart, "test message")
      initial_count = Printer.get_print_count(:test_printer_restart)
      assert initial_count == 1
      
      # Kill only the counter
      Control.kill_process(counter_pid)
      
      # Wait for restart
      Process.sleep(50)
      
      # Verify printer wasn't affected
      assert Process.whereis(:test_printer_restart) == printer_pid
      assert Process.alive?(printer_pid)
      assert Printer.get_print_count(:test_printer_restart) == initial_count
      
      # Verify counter was restarted
      new_counter_pid = Process.whereis(:test_counter_restart)
      assert new_counter_pid != counter_pid
      assert Process.alive?(new_counter_pid)
    end

    test "supervision tree correctly reflects process states", %{supervisor: _sup} do
      # Get initial tree
      {:ok, initial_children} = Control.get_supervision_tree(:test_restart_supervisor)
      assert length(initial_children) == 2
      assert Enum.all?(initial_children, & &1.alive)
      
      # Kill one process
      counter_child = Enum.find(initial_children, & &1.id == Counter)
      Control.kill_process(counter_child.pid)
      
      # Check immediately (might catch dead state)
      {:ok, _dead_children} = Control.get_supervision_tree(:test_restart_supervisor)
      
      # Wait for restart
      Process.sleep(50)
      
      # Check after restart
      {:ok, final_children} = Control.get_supervision_tree(:test_restart_supervisor)
      assert length(final_children) == 2
      assert Enum.all?(final_children, & &1.alive)
      
      # Verify PID changed for counter
      final_counter = Enum.find(final_children, & &1.id == Counter)
      assert final_counter.pid != counter_child.pid
    end
  end
end