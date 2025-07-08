defmodule SupervisorTestHelper do
  @moduledoc """
  Test helper for creating isolated supervisor instances in tests.

  This module provides utilities for proper test isolation when testing
  supervisor behavior, preventing conflicts between tests that would
  otherwise compete for globally named processes.

  ## Usage

  For tests that need to modify/kill processes (destructive tests):

      describe "destructive tests" do
        setup do
          SupervisorTestHelper.setup_isolated_supervisor("killing_tests")
        end
        
        test "can kill processes safely", %{supervisor: supervisor} do
          # Safe to kill processes - they're isolated to this test
        end
      end

  For tests that only read supervisor state (non-destructive):

      describe "read-only tests" do
        setup do
          SupervisorTestHelper.get_demo_supervisor()
        end
        
        test "can read supervisor tree", %{supervisor: supervisor} do
          # Safe to read - no mutations
        end
      end
  """

  alias OTPSupervisor.Core.Control

  @doc """
  Creates an isolated supervisor instance for destructive tests.

  This function creates a unique supervisor that can be safely modified
  or killed without affecting other tests. The supervisor is automatically
  cleaned up when the test completes.

  ## Parameters

    * `test_name` - Optional identifier to make supervisor names more readable

  ## Returns

  A map containing:
    * `:supervisor` - The unique supervisor name (atom)
    * `:sup_pid` - The supervisor PID

  ## Examples

      setup do
        SupervisorTestHelper.setup_isolated_supervisor("process_killing")
      end
  """
  def setup_isolated_supervisor(test_name \\ "") do
    # Create unique supervisor name to avoid conflicts
    unique_id = :erlang.unique_integer([:positive])
    base_name = if test_name != "", do: "#{test_name}_", else: ""
    supervisor_name = :"test_sup_#{base_name}#{unique_id}"

    # Start isolated supervisor with unique child names
    {:ok, sup_pid} =
      TestDemoSupervisor.start_link(
        name: supervisor_name,
        unique_id: unique_id
      )

    # Register supervisor with AnalyticsServer for monitoring
    alias OTPSupervisor.Core.AnalyticsServer
    AnalyticsServer.register_supervisor(sup_pid, supervisor_name)

    # Ensure cleanup happens when test completes
    ExUnit.Callbacks.on_exit(fn ->
      if Process.alive?(sup_pid) do
        ref = Process.monitor(sup_pid)
        Process.exit(sup_pid, :kill)
        # Wait for process to actually exit
        receive do
          {:DOWN, ^ref, :process, ^sup_pid, _} -> :ok
        after
          # Timeout after 100ms
          100 -> :ok
        end
      end
    end)

    %{supervisor: supervisor_name, sup_pid: sup_pid}
  end

  @doc """
  Provides read-only access to the demo supervisor.

  This function returns the shared `:demo_one_for_one` supervisor that
  should be running as part of the application. Use this for tests that
  only need to read supervisor state without modifying it.

  ## Returns

  A map containing:
    * `:supervisor` - The demo supervisor name (`:demo_one_for_one`)

  ## Raises

  Raises an error if the demo supervisor is not available, ensuring
  tests fail explicitly rather than silently.

  ## Examples

      setup do
        SupervisorTestHelper.get_demo_supervisor()
      end
  """
  def get_demo_supervisor do
    supervisor = :demo_one_for_one

    case Control.get_supervision_tree(supervisor) do
      {:ok, _children} ->
        %{supervisor: supervisor}

      {:error, reason} ->
        raise """
        Demo supervisor :demo_one_for_one is not available for testing.
        Error: #{inspect(reason)}

        This usually means the application is not properly started in test mode.
        Check that the demo supervisor is started in test configuration.
        """
    end
  end

  @doc """
  Creates a temporary supervisor for error testing.

  This creates a supervisor specifically intended to be crashed or
  put into error states for testing error handling paths.

  ## Parameters

    * `test_name` - Optional identifier for the error test

  ## Returns

  A map containing:
    * `:supervisor` - The supervisor name
    * `:sup_pid` - The supervisor PID

  ## Examples

      test "handles supervisor crashes" do
        %{supervisor: sup, sup_pid: pid} = SupervisorTestHelper.setup_crash_test_supervisor()
        
        # Safe to crash - it's isolated
        Process.exit(pid, :kill)
      end
  """
  def setup_crash_test_supervisor(test_name \\ "crash") do
    unique_id = :erlang.unique_integer([:positive])
    supervisor_name = :"crash_test_#{test_name}_#{unique_id}"

    {:ok, sup_pid} =
      TestDemoSupervisor.start_link(
        name: supervisor_name,
        unique_id: unique_id
      )

    # Register supervisor with AnalyticsServer for monitoring
    alias OTPSupervisor.Core.AnalyticsServer
    AnalyticsServer.register_supervisor(sup_pid, supervisor_name)

    # Always add cleanup for robustness - if test fails before crashing supervisor
    ExUnit.Callbacks.on_exit(fn ->
      if Process.alive?(sup_pid), do: Process.exit(sup_pid, :kill)
    end)

    %{supervisor: supervisor_name, sup_pid: sup_pid}
  end

  @doc """
  Waits for a supervisor restart to complete.

  When a child process is killed, the supervisor will restart it.
  This function provides a synchronization point to wait for the
  restart to complete using proper synchronization.

  ## Parameters

    * `supervisor` - The supervisor name or PID
    * `timeout` - Maximum time to wait in milliseconds (default: 1000)

  ## Returns

    * `:ok` if restart completes
    * `{:error, :timeout}` if restart doesn't complete in time

  ## Examples

      # Kill a child
      Process.exit(child_pid, :kill)
      
      # Wait for supervisor to restart it
      :ok = SupervisorTestHelper.wait_for_restart(supervisor)
  """
  def wait_for_restart(supervisor, timeout \\ 1000) do
    # Use a synchronous GenServer call to ensure all supervisor messages
    # are processed, which includes restart logic
    try do
      GenServer.call(supervisor, :which_children, timeout)
      :ok
    catch
      :exit, {:timeout, _} -> {:error, :timeout}
    end
  end

  @doc """
  Waits for a specific child process to be restarted by its supervisor.

  This is more robust than polling Process.whereis/1 as it queries the
  supervisor's state directly.

  ## Parameters

    * `supervisor_pid` - The supervisor PID
    * `child_id` - The child ID as used in the supervisor child spec
    * `original_pid` - The PID before the crash
    * `timeout` - Maximum time to wait in milliseconds (default: 1000)

  ## Returns

    * `:ok` if restart completes
    * `{:error, :timeout}` if restart doesn't complete in time

  ## Examples

      original_pid = Process.whereis(:my_process)
      Process.exit(original_pid, :kill)
      :ok = SupervisorTestHelper.wait_for_child_restart(sup_pid, :my_process, original_pid)
  """
  def wait_for_child_restart(supervisor_pid, child_id, original_pid, timeout \\ 1000) do
    task =
      Task.async(fn ->
        _wait_for_child_restart(supervisor_pid, child_id, original_pid)
      end)

    case Task.yield(task, timeout) do
      {:ok, :ok} ->
        :ok

      nil ->
        Task.shutdown(task)
        {:error, :timeout}
    end
  end

  defp _wait_for_child_restart(supervisor_pid, child_id, original_pid) do
    # Query the supervisor for its children
    children = Supervisor.which_children(supervisor_pid)
    restarted_child = Enum.find(children, fn {id, _, _, _} -> id == child_id end)

    case restarted_child do
      # Child found and its PID is different from the original
      {^child_id, new_pid, _, _} when new_pid != original_pid and is_pid(new_pid) ->
        # Restart successful
        :ok

      # Child still has the same PID or hasn't restarted yet
      _ ->
        # Use Task.yield with small interval for polling without sleep
        Task.yield(Task.async(fn -> :ok end), 20)
        _wait_for_child_restart(supervisor_pid, child_id, original_pid)
    end
  end

  @doc """
  Waits for a specific named process to be restarted (new PID).

  This function monitors the original process for death, then waits
  for the name to resolve to a new PID.

  ## Parameters

    * `process_name` - The registered name of the process
    * `original_pid` - The PID before the crash
    * `timeout` - Maximum time to wait in milliseconds (default: 1000)

  ## Returns

    * `:ok` if process restarted
    * `{:error, :timeout}` if restart doesn't complete in time

  ## Examples

      original_pid = Process.whereis(:my_process)
      # Crash the process...
      :ok = SupervisorTestHelper.wait_for_process_restart(:my_process, original_pid)
  """
  def wait_for_process_restart(process_name, original_pid, timeout \\ 1000) do
    # Monitor the original process to know when it dies
    if Process.alive?(original_pid) do
      ref = Process.monitor(original_pid)

      receive do
        {:DOWN, ^ref, :process, ^original_pid, _reason} ->
          # Process died, now wait for restart using proper synchronization
          wait_for_name_change(process_name, original_pid, timeout)
      after
        timeout -> {:error, :timeout}
      end
    else
      # Process already dead, check if restart happened
      wait_for_name_change(process_name, original_pid, timeout)
    end
  end

  defp wait_for_name_change(process_name, original_pid, timeout) do
    # Robust OTP approach: Use Task with proper timeout
    task =
      Task.async(fn ->
        monitor_name_change(process_name, original_pid)
      end)

    case Task.yield(task, timeout) do
      {:ok, result} ->
        result

      nil ->
        Task.shutdown(task)
        {:error, :timeout}
    end
  end

  defp monitor_name_change(process_name, original_pid) do
    case Process.whereis(process_name) do
      ^original_pid ->
        # Still the same PID, wait and check again
        # Use Task.yield with small interval for polling without sleep
        Task.yield(Task.async(fn -> :ok end), 10)
        monitor_name_change(process_name, original_pid)

      nil ->
        # No process registered, wait and check again  
        # Use Task.yield with small interval for polling without sleep
        Task.yield(Task.async(fn -> :ok end), 10)
        monitor_name_change(process_name, original_pid)

      _new_pid ->
        # Different PID - restart successful!
        :ok
    end
  end

  @doc """
  Extracts the first worker PID from a list of children.

  Helper function for tests that need to get a worker process
  to test supervisor restart behavior.

  ## Parameters

    * `children` - List of child specs from Control.get_supervision_tree/1

  ## Returns

  The PID of the first worker process found.

  ## Examples

      {:ok, children} = Control.get_supervision_tree(supervisor)
      worker_pid = SupervisorTestHelper.extract_first_worker_pid(children)
  """
  def extract_first_worker_pid(children) when is_list(children) do
    child = hd(children)
    extract_pid_from_string(child.pid)
  end

  @doc """
  Converts a PID string to an actual PID.

  The Control module returns PIDs as strings like "#PID<0.123.0>".
  This function converts them back to actual PID values using the centralized parser.

  ## Parameters

    * `pid_string` - PID in string format (e.g., "#PID<0.123.0>")

  ## Returns

  The actual PID value.

  ## Examples

      pid = SupervisorTestHelper.extract_pid_from_string("#PID<0.123.0>")
  """
  def extract_pid_from_string(pid_string) when is_binary(pid_string) do
    case Control.to_pid(pid_string) do
      {:ok, pid} -> pid
      {:error, :invalid_pid} -> raise ArgumentError, "Invalid PID format: #{pid_string}"
    end
  end
end
