defmodule OTPSupervisor.Core.SandboxManager do
  @moduledoc """
  Manages the lifecycle of sandbox OTP applications.

  This manager provides a clean API for starting, stopping, and reconfiguring
  entire sandbox applications using proper OTP application lifecycle operations.
  This uses dynamic application loading for true isolation.
  """

  use GenServer
  require Logger

  # Public API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def create_sandbox(sandbox_id, app_name, opts \\ []) do
    GenServer.call(__MODULE__, {:create_sandbox, sandbox_id, app_name, opts})
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
      next_id: 1,
      sandbox_code_paths: %{}
    }

    Logger.info("SandboxManager started")
    {:ok, state}
  end

  @impl true
  def handle_call({:create_sandbox, sandbox_id, app_name, opts}, _from, state) do
    case Map.get(state.sandboxes, sandbox_id) do
      nil ->
        # Create new sandbox application
        case start_sandbox_application(sandbox_id, app_name, opts) do
          {:ok, app_pid, full_opts} ->
            sandbox_info = %{
              id: sandbox_id,
              app_name: app_name,
              app_pid: app_pid,
              opts: full_opts,
              created_at: System.system_time(:millisecond),
              restart_count: 0
            }

            # Monitor the application
            ref = Process.monitor(app_pid)

            # Store in ETS for fast lookup
            :ets.insert(:sandboxes, {sandbox_id, sandbox_info})

            new_sandboxes = Map.put(state.sandboxes, sandbox_id, {sandbox_info, ref})

            Logger.info("Created sandbox #{sandbox_id} with app #{app_name} PID #{inspect(app_pid)}")
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

        # Gracefully stop the application
        case stop_sandbox_application(sandbox_info.app_name, sandbox_info.app_pid) do
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
        # Stop current application
        Process.demonitor(ref, [:flush])
        stop_sandbox_application(sandbox_info.app_name, sandbox_info.app_pid)

        # Start new application with same configuration
        case start_sandbox_application(
               sandbox_id,
               sandbox_info.app_name,
               sandbox_info.opts
             ) do
          {:ok, new_app_pid, _opts} ->
            # Update sandbox info
            updated_info = %{
              sandbox_info
              | app_pid: new_app_pid,
                restart_count: sandbox_info.restart_count + 1
            }

            # Monitor new application
            new_ref = Process.monitor(new_app_pid)

            # Update state and ETS
            :ets.insert(:sandboxes, {sandbox_id, updated_info})
            new_sandboxes = Map.put(state.sandboxes, sandbox_id, {updated_info, new_ref})

            Logger.info("Restarted sandbox #{sandbox_id} with new app PID #{inspect(new_app_pid)}")
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
        {:reply, {:ok, sandbox_info.app_pid}, state}

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

  defp start_sandbox_application(_sandbox_id, app_name, opts) do
    # Ensure the sandbox application code path is loaded
    sandbox_path = Path.join([File.cwd!(), "sandbox", "examples", to_string(app_name)])
    lib_path = Path.join(sandbox_path, "lib")
    
    if File.exists?(lib_path) do
      Code.prepend_path(lib_path)
    end

    # Load the application if not already loaded
    case Application.load(app_name) do
      :ok -> :ok
      {:error, {:already_loaded, ^app_name}} -> :ok
      {:error, reason} -> {:error, {:load_failed, reason}}
    end
    
    # Start the application
    case Application.start(app_name) do
      :ok ->
        # Get the application master pid for monitoring
        case :application_controller.get_master(app_name) do
          pid when is_pid(pid) -> {:ok, pid, opts}
          _ -> {:error, :no_master_pid}
        end
        
      {:error, {:already_started, ^app_name}} ->
        {:error, :already_started}
        
      {:error, reason} ->
        {:error, {:start_failed, reason}}
    end
  end

  defp stop_sandbox_application(app_name, _app_pid) do
    try do
      # Stop the application
      case Application.stop(app_name) do
        :ok -> :ok
        {:error, {:not_started, ^app_name}} -> :ok
        {:error, reason} -> {:error, {:stop_failed, reason}}
      end
      
      # Unload the application
      case Application.unload(app_name) do
        :ok -> :ok
        {:error, {:not_loaded, ^app_name}} -> :ok
        {:error, reason} -> {:error, {:unload_failed, reason}}
      end
      
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
