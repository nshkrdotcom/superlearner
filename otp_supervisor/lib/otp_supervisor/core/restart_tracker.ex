defmodule OTPSupervisor.Core.RestartTracker do
  @moduledoc """
  Tracks restart events for supervisors to provide analytics and insights.

  This module maintains a history of supervisor restart events, enabling
  calculation of restart intensity and prediction of restart storms.
  """

  use GenServer

  @doc """
  Starts tracking restart events for a supervisor.

  Returns `:ok` on success.
  """
  def start_tracking(supervisor_pid) when is_pid(supervisor_pid) do
    case GenServer.start_link(__MODULE__, supervisor_pid, name: tracker_name(supervisor_pid)) do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> :ok
      error -> error
    end
  end

  @doc """
  Gets the restart history for a supervisor.

  Returns a list of restart events.
  """
  def get_history(supervisor_pid) when is_pid(supervisor_pid) do
    try do
      GenServer.call(tracker_name(supervisor_pid), :get_history)
    catch
      :exit, _ -> []
    end
  end

  @doc """
  Records a restart event.
  """
  def record_restart(supervisor_pid, child_id, reason, old_pid, new_pid)
      when is_pid(supervisor_pid) do
    try do
      GenServer.cast(
        tracker_name(supervisor_pid),
        {:record_restart, child_id, reason, old_pid, new_pid}
      )
    catch
      :exit, _ -> :ok
    end
  end

  @doc """
  A synchronous call that ensures all prior messages have been processed.
  This is a robust way to synchronize state in tests without using sleep.
  """
  def sync(supervisor_pid) when is_pid(supervisor_pid) do
    try do
      GenServer.call(tracker_name(supervisor_pid), :sync, 5000)
    catch
      :exit, _ -> :ok
    end
  end

  # GenServer Callbacks

  @impl true
  def init(supervisor_pid) do
    # Monitor the supervisor
    Process.monitor(supervisor_pid)

    # Monitor all current children
    children = get_supervisor_children(supervisor_pid)
    monitored_children = monitor_children(children)

    {:ok,
     %{
       supervisor_pid: supervisor_pid,
       history: [],
       monitored_children: monitored_children
     }}
  end

  @impl true
  def handle_call(:get_history, _from, state) do
    {:reply, Enum.reverse(state.history), state}
  end

  @impl true
  def handle_call(:sync, _from, state) do
    # This call does nothing but reply. Its purpose is to act as a
    # synchronization point, ensuring all previous async messages
    # (like :DOWN) have been processed before the call returns.
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:record_restart, child_id, reason, old_pid, new_pid}, state) do
    event = %{
      timestamp: System.system_time(:millisecond),
      child_id: child_id,
      reason: reason,
      old_pid: inspect(old_pid),
      new_pid: inspect(new_pid)
    }

    new_history =
      [event | state.history]
      # Keep only last 100 events
      |> Enum.take(100)

    {:noreply, %{state | history: new_history}}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, pid, reason}, state) do
    cond do
      pid == state.supervisor_pid ->
        # Supervisor died, stop tracking
        {:stop, :normal, state}

      Map.has_key?(state.monitored_children, ref) ->
        # Child process died - record and monitor new one
        child_id = state.monitored_children[ref]

        # Check for restart
        current_children = get_supervisor_children(state.supervisor_pid)

        case find_restarted_child(child_id, current_children) do
          {:ok, new_pid} ->
            # Record restart event
            event = %{
              timestamp: System.system_time(:millisecond),
              child_id: child_id,
              reason: reason,
              old_pid: inspect(pid),
              new_pid: inspect(new_pid)
            }

            # Monitor new child
            new_ref = Process.monitor(new_pid)

            new_monitored =
              state.monitored_children
              |> Map.delete(ref)
              |> Map.put(new_ref, child_id)

            new_history = [event | state.history] |> Enum.take(100)

            {:noreply, %{state | history: new_history, monitored_children: new_monitored}}

          :not_found ->
            # Child not restarted (maybe paused supervisor)
            new_monitored = Map.delete(state.monitored_children, ref)
            {:noreply, %{state | monitored_children: new_monitored}}
        end

      true ->
        # Unknown process died
        {:noreply, state}
    end
  end

  # Private Functions

  defp tracker_name(supervisor_pid) do
    :"restart_tracker_#{inspect(supervisor_pid)}"
  end

  defp get_supervisor_children(supervisor_pid) do
    try do
      Supervisor.which_children(supervisor_pid)
    rescue
      _ -> []
    end
  end

  defp monitor_children(children) do
    children
    |> Enum.filter(fn {_id, pid, _type, _modules} -> is_pid(pid) end)
    |> Enum.map(fn {id, pid, _type, _modules} ->
      ref = Process.monitor(pid)
      {ref, id}
    end)
    |> Map.new()
  end

  defp find_restarted_child(child_id, current_children) do
    case Enum.find(current_children, fn {id, _pid, _type, _modules} -> id == child_id end) do
      {^child_id, pid, _type, _modules} when is_pid(pid) -> {:ok, pid}
      _ -> :not_found
    end
  end
end
