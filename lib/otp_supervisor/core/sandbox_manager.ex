defmodule OTPSupervisor.Core.SandboxManager do
  @moduledoc """
  Manages the lifecycle of sandbox OTP applications with true hot-reload isolation.

  This manager provides a clean API for starting, stopping, and reconfiguring
  entire sandbox applications using isolated compilation and dynamic module loading.
  Features complete fault isolation and hot-reload capabilities.
  """

  use GenServer
  require Logger

  alias OTPSupervisor.Core.IsolatedCompiler
  alias OTPSupervisor.Core.ModuleVersionManager

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
      sandbox_code_paths: %{},
      compilation_artifacts: %{}
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
        :ok = stop_sandbox_application(sandbox_info.app_name, sandbox_id)

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
        stop_sandbox_application(sandbox_info.app_name, sandbox_id)

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
      module_string when is_binary(module_string) ->
        # Convert string to atom and treat as module
        case String.starts_with?(module_string, "Elixir.") do
          true ->
            module = String.to_atom(module_string)
            {:otp_sandbox, module}
          false ->
            module = String.to_atom("Elixir." <> module_string)
            {:otp_sandbox, module}
        end
        
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
    # Use isolated compilation for true fault isolation
    sandbox_path = Path.join([File.cwd!(), "sandbox", "examples", to_string(app_name)])
    
    # Compile sandbox application in isolation
    with {:ok, compile_info} <- compile_sandbox_isolated(sandbox_id, sandbox_path, app_name, opts),
         :ok <- setup_code_paths(sandbox_id, compile_info),
         :ok <- load_sandbox_modules(sandbox_id, compile_info),
         :ok <- load_sandbox_application(app_name, sandbox_id, compile_info.app_file),
         {:ok, app_pid, supervisor_pid} <- start_application_and_supervisor(sandbox_id, app_name, supervisor_module, opts) do
      
      # Success - return all the info
      {:ok, app_pid, supervisor_pid, Keyword.put(opts, :compile_info, compile_info)}
    else
      {:error, reason} ->
        Logger.error("Failed to start sandbox application #{app_name}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp setup_code_paths(sandbox_id, compile_info) do
    # Set up code paths for the compiled modules
    ebin_path = Path.dirname(List.first(compile_info.beam_files, ""))
    if File.exists?(ebin_path) do
      Code.prepend_path(ebin_path)
      Logger.debug("Added code path for sandbox #{sandbox_id}: #{ebin_path}")
      :ok
    else
      {:error, {:ebin_path_not_found, ebin_path}}
    end
  end

  defp start_application_and_supervisor(sandbox_id, app_name, supervisor_module, opts) do
    # Start the base application (shared, but that's ok)
    case Application.start(app_name) do
      :ok ->
        # Start a unique supervisor with sandbox isolation
        case start_supervisor_in_application(sandbox_id, supervisor_module, opts) do
          {:ok, supervisor_pid} ->
            # Return the supervisor PID as the "app_pid" for this sandbox
            # This ensures each sandbox has its own unique identifier for destruction
            {:ok, supervisor_pid, supervisor_pid}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, {:already_started, ^app_name}} ->
        # Application already started, just start the supervisor
        case start_supervisor_in_application(sandbox_id, supervisor_module, opts) do
          {:ok, supervisor_pid} ->
            # Return the supervisor PID as the "app_pid" for this sandbox
            {:ok, supervisor_pid, supervisor_pid}

          {:error, reason} ->
            {:error, reason}
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

  defp stop_sandbox_application(_app_name, _sandbox_id) do
    # With the new approach, we don't stop the shared application
    # The supervisor termination handles the cleanup
    # We only need to ensure the supervisor (which is the app_pid) is terminated
    :ok
  end

  # New isolated compilation functions

  defp compile_sandbox_isolated(sandbox_id, sandbox_path, app_name, opts) do
    compile_opts = [
      timeout: Keyword.get(opts, :compile_timeout, 30_000),
      validate_beams: Keyword.get(opts, :validate_beams, true),
      env: %{
        "MIX_ENV" => to_string(Mix.env()),
        "MIX_TARGET" => "host"
      }
    ]

    Logger.info("Compiling sandbox #{sandbox_id} in isolation", 
      sandbox_path: sandbox_path, app_name: app_name)

    case IsolatedCompiler.compile_sandbox(sandbox_path, compile_opts) do
      {:ok, compile_info} ->
        Logger.info("Successfully compiled sandbox #{sandbox_id}",
          compilation_time: compile_info.compilation_time,
          beam_files: length(compile_info.beam_files))
        {:ok, compile_info}

      {:error, reason} ->
        report = IsolatedCompiler.compilation_report({:error, reason})
        Logger.error("Failed to compile sandbox #{sandbox_id}",
          reason: inspect(reason),
          details: report.details)
        {:error, reason}
    end
  end

  defp load_sandbox_modules(sandbox_id, compile_info) do
    compile_info.beam_files
    |> Enum.reduce_while(:ok, fn beam_file, :ok ->
      case load_beam_file(sandbox_id, beam_file) do
        :ok -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp load_beam_file(sandbox_id, beam_file) do
    try do
      # Read BEAM file
      beam_data = File.read!(beam_file)
      
      # Extract module name from BEAM
      module = case :beam_lib.info(String.to_charlist(beam_file)) do
        info when is_list(info) ->
          # beam_lib.info sometimes returns a direct list
          Keyword.get(info, :module)
        {:ok, info} ->
          info[:module]
        {:error, reason} ->
          throw({:beam_info_failed, beam_file, reason})
      end
      
      # Actually load the module into the VM
      case :code.load_binary(module, String.to_charlist(beam_file), beam_data) do
        {:module, ^module} ->
          # Register with version manager after successful load
          case ModuleVersionManager.register_module_version(sandbox_id, module, beam_data) do
            {:ok, version} ->
              Logger.debug("Loaded module #{module} version #{version} for sandbox #{sandbox_id}")
              :ok
              
            {:error, reason} ->
              {:error, {:version_registration_failed, module, reason}}
          end
          
        {:error, reason} ->
          {:error, {:code_load_failed, module, reason}}
      end
    rescue
      error ->
        {:error, {:beam_load_failed, beam_file, error}}
    catch
      thrown_error ->
        {:error, thrown_error}
    end
  end

  defp load_sandbox_application(app_name, _sandbox_id, _app_file) do
    # Load the base application (we'll handle uniqueness at the runtime level)
    case Application.load(app_name) do
      :ok ->
        :ok

      {:error, {:already_loaded, ^app_name}} ->
        :ok

      {:error, reason} ->
        Logger.error("Failed to load application #{app_name}: #{inspect(reason)}")
        {:error, {:load_failed, reason}}
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
