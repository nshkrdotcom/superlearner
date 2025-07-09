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

  def create_sandbox(sandbox_id, module_or_app, opts \\ []) do
    GenServer.call(__MODULE__, {:create_sandbox, sandbox_id, module_or_app, opts})
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
  def handle_call({:create_sandbox, sandbox_id, module_or_app, opts}, _from, state) do
    case Map.get(state.sandboxes, sandbox_id) do
      nil ->
        # Determine if we're dealing with a supervisor module or application
        {app_name, supervisor_module} = parse_module_or_app(module_or_app, opts)

        # Create new sandbox application
        case start_sandbox_application(sandbox_id, app_name, supervisor_module, opts) do
          {:ok, app_pid, supervisor_pid, full_opts} ->
            sandbox_info = %{
              id: sandbox_id,
              app_name: app_name,
              supervisor_module: supervisor_module,
              app_pid: app_pid,
              supervisor_pid: supervisor_pid,
              opts: full_opts,
              created_at: System.system_time(:millisecond),
              restart_count: 0
            }

            # Monitor the application
            ref = Process.monitor(app_pid)

            # Store in ETS for fast lookup
            :ets.insert(:sandboxes, {sandbox_id, sandbox_info})

            new_sandboxes = Map.put(state.sandboxes, sandbox_id, {sandbox_info, ref})

            Logger.info(
              "Created sandbox #{sandbox_id} with app #{app_name} PID #{inspect(app_pid)}"
            )

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

        # Remove from state and ETS BEFORE stopping application
        # This ensures the sandboxes_using_app count is correct
        :ets.delete(:sandboxes, sandbox_id)
        new_sandboxes = Map.delete(state.sandboxes, sandbox_id)

        # Now stop the application gracefully
        :ok = stop_sandbox_application(sandbox_info.app_name, sandbox_info.app_pid)

        # Terminate the supervisor if it's still alive
        # Use a separate process to avoid killing the GenServer
        supervisor_pid = sandbox_info.supervisor_pid

        if Process.alive?(supervisor_pid) do
          terminate_supervisor(supervisor_pid)
        end

        Logger.info("Destroyed sandbox #{sandbox_id}")
        {:reply, :ok, %{state | sandboxes: new_sandboxes}}

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
               sandbox_info.supervisor_module,
               sandbox_info.opts
             ) do
          {:ok, new_app_pid, new_supervisor_pid, _opts} ->
            # Update sandbox info
            updated_info = %{
              sandbox_info
              | app_pid: new_app_pid,
                supervisor_pid: new_supervisor_pid,
                restart_count: sandbox_info.restart_count + 1
            }

            # Monitor new application
            new_ref = Process.monitor(new_app_pid)

            # Update state and ETS
            :ets.insert(:sandboxes, {sandbox_id, updated_info})
            new_sandboxes = Map.put(state.sandboxes, sandbox_id, {updated_info, new_ref})

            Logger.info(
              "Restarted sandbox #{sandbox_id} with new app PID #{inspect(new_app_pid)}"
            )

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

  defp terminate_supervisor(supervisor_pid) do
    # Use Task.start to run in a separate process with proper monitoring
    Task.start(fn ->
      if Process.alive?(supervisor_pid) do
        # Monitor the supervisor to know when it dies
        supervisor_ref = Process.monitor(supervisor_pid)
        Process.exit(supervisor_pid, :shutdown)

        # Wait for supervisor to terminate gracefully
        receive do
          {:DOWN, ^supervisor_ref, :process, ^supervisor_pid, _reason} ->
            :ok
        after
          2000 ->
            # Just demonitor without force kill to avoid crashing SandboxManager
            Process.demonitor(supervisor_ref, [:flush])
            :ok
        end
      end
    end)
  end

  defp parse_module_or_app(module_or_app, opts) do
    case module_or_app do
      app when is_atom(app) ->
        # Check if it's a known supervisor module
        case to_string(app) do
          "Elixir." <> _ ->
            # It's a module, use default otp_sandbox app
            {:otp_sandbox, app}

          _ ->
            # It's an application name
            supervisor_module =
              Keyword.get(opts, :supervisor_module, OtpSandbox.TestDemoSupervisor)

            {app, supervisor_module}
        end
    end
  end

  defp start_sandbox_application(sandbox_id, app_name, supervisor_module, opts) do
    # Ensure the sandbox application code path is loaded
    sandbox_path = Path.join([File.cwd!(), "sandbox", "examples", to_string(app_name)])
    lib_path = Path.join(sandbox_path, "lib")
    ebin_path = Path.join([sandbox_path, "_build", "dev", "lib", to_string(app_name), "ebin"])

    # Ensure application is compiled
    if File.exists?(sandbox_path) do
      ensure_sandbox_compiled(sandbox_path, app_name)
    end

    # Add both lib and ebin paths
    if File.exists?(lib_path) do
      Code.prepend_path(lib_path)
    end

    if File.exists?(ebin_path) do
      Code.prepend_path(ebin_path)
    end

    # Load the application if not already loaded
    case Application.load(app_name) do
      :ok ->
        :ok

      {:error, {:already_loaded, ^app_name}} ->
        :ok

      {:error, reason} ->
        Logger.error("Failed to load application #{app_name}: #{inspect(reason)}")
        Logger.error("Sandbox path: #{sandbox_path}")
        Logger.error("Lib path exists: #{File.exists?(lib_path)}")
        Logger.error("Ebin path exists: #{File.exists?(ebin_path)}")
        {:error, {:load_failed, reason}}
    end

    # Start the application
    case Application.start(app_name) do
      :ok ->
        # Get the application master pid for monitoring
        case :application_controller.get_master(app_name) do
          app_pid when is_pid(app_pid) ->
            # Start the supervisor within the application
            case start_supervisor_in_application(sandbox_id, supervisor_module, opts) do
              {:ok, supervisor_pid} ->
                {:ok, app_pid, supervisor_pid, opts}

              {:error, reason} ->
                # Clean up the application if supervisor failed
                Application.stop(app_name)
                {:error, reason}
            end

          _ ->
            {:error, :no_master_pid}
        end

      {:error, {:already_started, ^app_name}} ->
        # Application already started, try to start supervisor
        case :application_controller.get_master(app_name) do
          app_pid when is_pid(app_pid) ->
            case start_supervisor_in_application(sandbox_id, supervisor_module, opts) do
              {:ok, supervisor_pid} ->
                {:ok, app_pid, supervisor_pid, opts}

              {:error, reason} ->
                Logger.warning(
                  "Failed to start supervisor in existing application #{app_name}: #{inspect(reason)}"
                )

                {:error, reason}
            end

          _ ->
            Logger.warning("No master PID found for already started application #{app_name}")
            {:error, :no_master_pid}
        end

      {:error, reason} ->
        Logger.error("Failed to start application #{app_name}: #{inspect(reason)}")
        {:error, {:start_failed, reason}}
    end
  end

  defp start_supervisor_in_application(sandbox_id, supervisor_module, opts) do
    # Generate unique supervisor name to avoid conflicts
    unique_id = :erlang.unique_integer([:positive])
    supervisor_name = :"#{supervisor_module}_#{sandbox_id}_#{unique_id}"

    # Prepare options for supervisor with unique naming
    supervisor_opts =
      Keyword.merge(opts,
        name: supervisor_name,
        unique_id: unique_id
      )

    # Start the supervisor
    case supervisor_module.start_link(supervisor_opts) do
      {:ok, pid} ->
        {:ok, pid}

      {:error, reason} ->
        Logger.error("Failed to start supervisor #{supervisor_module}: #{inspect(reason)}")
        {:error, {:supervisor_start_failed, reason}}
    end
  end

  defp stop_sandbox_application(app_name, _app_pid) do
    try do
      # Check if other sandboxes are using this application
      sandboxes_using_app =
        :ets.tab2list(:sandboxes)
        |> Enum.count(fn {_id, sandbox_info} -> sandbox_info.app_name == app_name end)

      # Only stop and unload if this is the last sandbox using the application
      if sandboxes_using_app <= 1 do
        # Stop the application
        case Application.stop(app_name) do
          :ok ->
            :ok

          {:error, {:not_started, ^app_name}} ->
            :ok

          {:error, reason} ->
            Logger.warning("Failed to stop application #{app_name}: #{inspect(reason)}")
            # Don't fail sandbox destruction for stop failures
            :ok
        end

        # Unload the application
        case Application.unload(app_name) do
          :ok ->
            :ok

          {:error, {:not_loaded, ^app_name}} ->
            :ok

          {:error, reason} ->
            Logger.warning("Failed to unload application #{app_name}: #{inspect(reason)}")
            # Don't fail sandbox destruction for unload failures
            :ok
        end
      else
        Logger.debug(
          "Not stopping application #{app_name} - still used by #{sandboxes_using_app - 1} other sandboxes"
        )
      end

      :ok
    catch
      :exit, reason ->
        Logger.warning("Exception while stopping application #{app_name}: #{inspect(reason)}")
        # Don't fail sandbox destruction for exceptions
        :ok
    end
  end

  defp ensure_sandbox_compiled(sandbox_path, app_name) do
    # Check if the application is already compiled
    ebin_path = Path.join([sandbox_path, "_build", "dev", "lib", to_string(app_name), "ebin"])
    app_file = Path.join(ebin_path, "#{app_name}.app")

    if not File.exists?(app_file) do
      Logger.info("Compiling sandbox application #{app_name}")

      # Change to sandbox directory and compile
      old_cwd = File.cwd!()

      try do
        File.cd!(sandbox_path)
        {output, exit_code} = System.cmd("mix", ["compile"], stderr_to_stdout: true)

        if exit_code != 0 do
          Logger.error("Failed to compile sandbox application #{app_name}: #{output}")
          {:error, {:compile_failed, output}}
        else
          Logger.info("Successfully compiled sandbox application #{app_name}")
          :ok
        end
      catch
        :exit, reason ->
          Logger.error("Failed to compile sandbox application #{app_name}: #{inspect(reason)}")
          {:error, {:compile_failed, reason}}
      after
        File.cd!(old_cwd)
      end
    else
      :ok
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
