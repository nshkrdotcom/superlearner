defmodule OTPSupervisor.Core.SandboxManager do
  @moduledoc """
  Manages the lifecycle of sandbox supervision trees.

  This manager provides a clean API for starting, stopping, and reconfiguring
  entire sandbox supervision trees using proper OTP lifecycle operations.
  This is a real-world pattern for managing subsystems in OTP applications.
  """

  use GenServer
  require Logger

  # Public API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def create_sandbox(sandbox_id, supervisor_module, opts \\ []) do
    GenServer.call(__MODULE__, {:create_sandbox, sandbox_id, supervisor_module, opts})
  end

  def destroy_sandbox(sandbox_id) do
    GenServer.call(__MODULE__, {:destroy_sandbox, sandbox_id})
  end

  def restart_sandbox(sandbox_id) do
    GenServer.call(__MODULE__, {:restart_sandbox, sandbox_id})
  end

  def get_sandbox_info(sandbox_id) do
    GenServer.call(__MODULE__, {:get_sandbox_info, sandbox_id})
  end

  def list_sandboxes do
    GenServer.call(__MODULE__, :list_sandboxes)
  end

  def get_sandbox_pid(sandbox_id) do
    GenServer.call(__MODULE__, {:get_sandbox_pid, sandbox_id})
  end

  def sync do
    GenServer.call(__MODULE__, :sync)
  end

  # GenServer Callbacks

  @impl true
  def init(_opts) do
    # Use ETS table for fast sandbox lookup - check if it already exists
    case :ets.info(:sandboxes) do
      :undefined ->
        :ets.new(:sandboxes, [:named_table, :set, :protected])
        Logger.info("Created new ETS table :sandboxes")

      _ ->
        # Table exists, possibly from previous SandboxManager instance
        Logger.info("ETS table :sandboxes already exists, clearing it")
        :ets.delete_all_objects(:sandboxes)
    end

    state = %{
      sandboxes: %{},
      next_id: 1
    }

    Logger.info("SandboxManager started")
    {:ok, state}
  end

  @impl true
  def handle_call({:create_sandbox, sandbox_id, supervisor_module, opts}, _from, state) do
    case Map.get(state.sandboxes, sandbox_id) do
      nil ->
        # Create new sandbox
        case start_sandbox_supervisor(sandbox_id, supervisor_module, opts) do
          {:ok, pid, full_opts} ->
            sandbox_info = %{
              id: sandbox_id,
              supervisor_module: supervisor_module,
              supervisor_pid: pid,
              opts: full_opts,
              created_at: System.system_time(:millisecond),
              restart_count: 0
            }

            # Monitor the supervisor
            ref = Process.monitor(pid)

            # Store in ETS for fast lookup
            :ets.insert(:sandboxes, {sandbox_id, sandbox_info})

            new_sandboxes = Map.put(state.sandboxes, sandbox_id, {sandbox_info, ref})

            Logger.info("Created sandbox #{sandbox_id} with PID #{inspect(pid)}")
            {:reply, {:ok, sandbox_info}, %{state | sandboxes: new_sandboxes}}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      {existing_info, _ref} ->
        {:reply, {:error, {:already_exists, existing_info}}, state}
    end
  end

  @impl true
  def handle_call({:destroy_sandbox, sandbox_id}, _from, state) do
    case Map.get(state.sandboxes, sandbox_id) do
      {sandbox_info, ref} ->
        # Stop monitoring
        Process.demonitor(ref, [:flush])

        # Gracefully stop the supervisor
        case stop_sandbox_supervisor(sandbox_info.supervisor_pid) do
          :ok ->
            # Remove from state and ETS
            :ets.delete(:sandboxes, sandbox_id)
            new_sandboxes = Map.delete(state.sandboxes, sandbox_id)

            Logger.info("Destroyed sandbox #{sandbox_id}")
            {:reply, :ok, %{state | sandboxes: new_sandboxes}}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end

      nil ->
        {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call({:restart_sandbox, sandbox_id}, _from, state) do
    case Map.get(state.sandboxes, sandbox_id) do
      {sandbox_info, ref} ->
        # Stop current supervisor
        Process.demonitor(ref, [:flush])
        stop_sandbox_supervisor(sandbox_info.supervisor_pid)

        # Start new supervisor with same configuration
        case start_sandbox_supervisor(
               sandbox_id,
               sandbox_info.supervisor_module,
               sandbox_info.opts
             ) do
          {:ok, new_pid, _opts} ->
            # Update sandbox info
            updated_info = %{
              sandbox_info
              | supervisor_pid: new_pid,
                restart_count: sandbox_info.restart_count + 1
            }

            # Monitor new supervisor
            new_ref = Process.monitor(new_pid)

            # Update state and ETS
            :ets.insert(:sandboxes, {sandbox_id, updated_info})
            new_sandboxes = Map.put(state.sandboxes, sandbox_id, {updated_info, new_ref})

            Logger.info("Restarted sandbox #{sandbox_id} with new PID #{inspect(new_pid)}")
            {:reply, {:ok, updated_info}, %{state | sandboxes: new_sandboxes}}

          {:error, reason} ->
            # Remove failed sandbox
            :ets.delete(:sandboxes, sandbox_id)
            new_sandboxes = Map.delete(state.sandboxes, sandbox_id)
            {:reply, {:error, reason}, %{state | sandboxes: new_sandboxes}}
        end

      nil ->
        {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call({:get_sandbox_info, sandbox_id}, _from, state) do
    case :ets.lookup(:sandboxes, sandbox_id) do
      [{^sandbox_id, sandbox_info}] ->
        {:reply, {:ok, sandbox_info}, state}

      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call({:get_sandbox_pid, sandbox_id}, _from, state) do
    case :ets.lookup(:sandboxes, sandbox_id) do
      [{^sandbox_id, sandbox_info}] ->
        {:reply, {:ok, sandbox_info.supervisor_pid}, state}

      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call(:list_sandboxes, _from, state) do
    sandboxes = :ets.tab2list(:sandboxes)
    sandbox_list = Enum.map(sandboxes, fn {_id, info} -> info end)
    {:reply, sandbox_list, state}
  end

  @impl true
  def handle_call(:sync, _from, state) do
    {:reply, :ok, state}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, pid, reason}, state) do
    Logger.info(
      "SandboxManager received DOWN message for #{inspect(pid)} with reason #{inspect(reason)}"
    )

    try do
      # Find which sandbox died
      case find_sandbox_by_ref(state.sandboxes, ref) do
        {sandbox_id, _sandbox_info} ->
          Logger.warning("Sandbox #{sandbox_id} supervisor died: #{inspect(reason)}")

          # Remove from state and ETS
          try do
            :ets.delete(:sandboxes, sandbox_id)
          rescue
            error ->
              Logger.error("Error deleting from ETS: #{inspect(error)}")
          end

          new_sandboxes = Map.delete(state.sandboxes, sandbox_id)

          Logger.info("Successfully cleaned up sandbox #{sandbox_id}")
          {:noreply, %{state | sandboxes: new_sandboxes}}

        nil ->
          Logger.warning("Received DOWN message for unknown process #{inspect(pid)}")
          {:noreply, state}
      end
    rescue
      error ->
        Logger.error("Error in handle_info: #{inspect(error)}")
        # Don't crash the SandboxManager, just log the error
        {:noreply, state}
    end
  end

  # Private Functions

  defp start_sandbox_supervisor(sandbox_id, supervisor_module, opts) do
    # Create unique name for this sandbox
    unique_id = :erlang.unique_integer([:positive])
    sandbox_name = :"sandbox_#{sandbox_id}_#{unique_id}"

    # Merge options with sandbox-specific configuration
    full_opts =
      Keyword.merge(opts,
        name: sandbox_name,
        unique_id: unique_id,
        sandbox_id: sandbox_id
      )

    case supervisor_module.start_link(full_opts) do
      {:ok, pid} ->
        # CRITICAL FIX: Unlink the supervisor - we only want to monitor it, not be linked to it
        # This prevents SandboxManager from dying when the sandbox supervisor is killed
        Process.unlink(pid)
        {:ok, pid, full_opts}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp stop_sandbox_supervisor(supervisor_pid) do
    try do
      # Give the supervisor 5 seconds to shut down gracefully
      Supervisor.stop(supervisor_pid, :normal, 5000)
      :ok
    catch
      :exit, reason -> {:error, reason}
    end
  end

  defp find_sandbox_by_ref(sandboxes, target_ref) do
    try do
      Enum.find_value(sandboxes, fn {sandbox_id, {sandbox_info, ref}} ->
        if ref == target_ref do
          {sandbox_id, sandbox_info}
        else
          nil
        end
      end)
    rescue
      error ->
        Logger.error(
          "Error in find_sandbox_by_ref: #{inspect(error)}, sandboxes: #{inspect(sandboxes)}, target_ref: #{inspect(target_ref)}"
        )

        nil
    end
  end
end
