defmodule OTPSupervisor.Core.SupervisorController do
  @moduledoc """
  Provides runtime control over supervisor behavior.

  This module allows pausing and resuming supervisor restart behavior,
  which is useful for debugging and maintenance scenarios.
  """

  use GenServer

  @doc """
  Pauses supervision for a supervisor.

  When paused, the supervisor will not restart failed children.
  """
  def pause(supervisor_pid) when is_pid(supervisor_pid) do
    case get_or_start_controller(supervisor_pid) do
      {:ok, controller_pid} ->
        GenServer.call(controller_pid, :pause)

      error ->
        error
    end
  end

  @doc """
  Resumes supervision for a supervisor.

  Re-enables automatic restart of failed children.
  """
  def resume(supervisor_pid) when is_pid(supervisor_pid) do
    case get_or_start_controller(supervisor_pid) do
      {:ok, controller_pid} ->
        GenServer.call(controller_pid, :resume)

      error ->
        error
    end
  end

  @doc """
  Checks if a supervisor is paused.
  """
  def paused?(supervisor_pid) when is_pid(supervisor_pid) do
    case get_controller(supervisor_pid) do
      {:ok, controller_pid} ->
        GenServer.call(controller_pid, :paused?)

      _ ->
        false
    end
  end

  # GenServer Callbacks

  @impl true
  def init(supervisor_pid) do
    # Monitor the supervisor
    Process.monitor(supervisor_pid)

    # Note: In a real implementation, this would hook into supervisor internals
    # to actually prevent restarts. For this educational implementation,
    # we'll simulate the behavior by tracking state.

    {:ok,
     %{
       supervisor_pid: supervisor_pid,
       paused: false,
       original_children: []
     }}
  end

  @impl true
  def handle_call(:pause, _from, state) do
    try do
      # Get current children and their specs
      children = Supervisor.which_children(state.supervisor_pid)

      # Get child specs before deleting
      child_specs =
        Enum.map(children, fn {id, _pid, _type, _modules} ->
          spec =
            case :supervisor.get_childspec(state.supervisor_pid, id) do
              {ok, child_spec} when ok in [:ok, :error] -> child_spec
              child_spec -> child_spec
            end

          {id, spec}
        end)

      # Terminate and delete each child
      Enum.each(children, fn {id, pid, _type, _modules} ->
        if is_pid(pid) do
          Supervisor.terminate_child(state.supervisor_pid, id)
          Supervisor.delete_child(state.supervisor_pid, id)
        end
      end)

      {:reply, :ok, %{state | paused: true, original_children: child_specs}}
    rescue
      _e -> {:reply, {:error, :pause_failed}, state}
    end
  end

  @impl true
  def handle_call(:resume, _from, state) do
    if state.paused do
      try do
        # Add back and start all children using stored specs
        Enum.each(state.original_children, fn {_id, child_spec} ->
          Supervisor.start_child(state.supervisor_pid, child_spec)
        end)

        {:reply, :ok, %{state | paused: false, original_children: []}}
      rescue
        _ -> {:reply, {:error, :resume_failed}, state}
      end
    else
      {:reply, :ok, state}
    end
  end

  @impl true
  def handle_call(:paused?, _from, state) do
    {:reply, state.paused, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, _pid, _reason}, state) do
    # Supervisor died, stop controller
    {:stop, :normal, state}
  end

  # Private Functions

  defp get_or_start_controller(supervisor_pid) do
    case get_controller(supervisor_pid) do
      {:ok, controller_pid} ->
        {:ok, controller_pid}

      _ ->
        start_controller(supervisor_pid)
    end
  end

  defp get_controller(supervisor_pid) do
    name = controller_name(supervisor_pid)

    case Process.whereis(name) do
      nil -> {:error, :not_found}
      pid -> {:ok, pid}
    end
  end

  defp start_controller(supervisor_pid) do
    name = controller_name(supervisor_pid)

    case GenServer.start_link(__MODULE__, supervisor_pid, name: name) do
      {:ok, pid} -> {:ok, pid}
      {:error, {:already_started, pid}} -> {:ok, pid}
      error -> error
    end
  end

  defp controller_name(supervisor_pid) do
    :"supervisor_controller_#{inspect(supervisor_pid)}"
  end
end
