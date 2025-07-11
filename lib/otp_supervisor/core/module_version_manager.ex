defmodule OTPSupervisor.Core.ModuleVersionManager do
  @moduledoc """
  Tracks module versions and handles hot-swapping for sandbox applications.

  This module provides version management for dynamically loaded modules,
  enabling safe hot-swapping with rollback capabilities and dependency
  tracking to ensure proper reload ordering.
  """

  use GenServer
  require Logger

  @table_name :module_versions
  @max_versions_per_module 10

  @type module_version :: %{
          sandbox_id: String.t(),
          module: atom(),
          version: non_neg_integer(),
          beam_data: binary(),
          loaded_at: DateTime.t(),
          dependencies: [atom()],
          checksum: String.t()
        }

  @type hot_swap_result ::
          {:ok, :hot_swapped}
          | {:ok, :no_change}
          | {:error, :module_not_found}
          | {:error, :same_version}
          | {:error, {:swap_failed, reason :: any()}}
          | {:error, {:state_migration_failed, reason :: any()}}

  # Client API

  @doc """
  Starts the ModuleVersionManager.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Registers a new module version for a sandbox.
  """
  @spec register_module_version(String.t(), atom(), binary()) ::
          {:ok, non_neg_integer()} | {:error, any()}
  def register_module_version(sandbox_id, module, beam_data) do
    GenServer.call(__MODULE__, {:register_module_version, sandbox_id, module, beam_data})
  end

  @doc """
  Hot-swaps a module with state preservation for GenServers.
  """
  @spec hot_swap_module(String.t(), atom(), binary()) :: hot_swap_result()
  def hot_swap_module(sandbox_id, module, new_beam_data) do
    GenServer.call(__MODULE__, {:hot_swap_module, sandbox_id, module, new_beam_data}, 30_000)
  end

  @doc """
  Rolls back a module to a previous version.
  """
  @spec rollback_module(String.t(), atom(), non_neg_integer()) ::
          {:ok, :rolled_back} | {:error, any()}
  def rollback_module(sandbox_id, module, target_version) do
    GenServer.call(__MODULE__, {:rollback_module, sandbox_id, module, target_version})
  end

  @doc """
  Gets the current version number for a module.
  """
  @spec get_current_version(String.t(), atom()) ::
          {:ok, non_neg_integer()} | {:error, :not_found}
  def get_current_version(sandbox_id, module) do
    case :ets.lookup(@table_name, {sandbox_id, module}) do
      [] ->
        {:error, :not_found}

      versions ->
        max_version =
          versions |> Enum.map(fn {_key, version_data} -> version_data.version end) |> Enum.max()

        {:ok, max_version}
    end
  end

  @doc """
  Gets module dependency graph for reload ordering.
  """
  @spec get_module_dependencies([atom()]) :: %{atom() => [atom()]}
  def get_module_dependencies(modules) when is_list(modules) do
    GenServer.call(__MODULE__, {:get_module_dependencies, modules})
  end

  @doc """
  Lists all versions for a specific module in a sandbox.
  """
  @spec list_module_versions(String.t(), atom()) :: [module_version()]
  def list_module_versions(sandbox_id, module) do
    :ets.lookup(@table_name, {sandbox_id, module})
    |> Enum.sort_by(fn {_key, version_data} -> version_data.version end, :desc)
  end

  @doc """
  Cleans up all module versions for a sandbox.
  """
  @spec cleanup_sandbox_modules(String.t()) :: :ok
  def cleanup_sandbox_modules(sandbox_id) do
    GenServer.call(__MODULE__, {:cleanup_sandbox_modules, sandbox_id})
  end

  # GenServer callbacks

  @impl true
  def init(_opts) do
    # Create ETS table for storing module versions
    table =
      :ets.new(@table_name, [
        # Allow multiple versions per module
        :bag,
        :named_table,
        :public,
        read_concurrency: true
      ])

    Logger.info("ModuleVersionManager started with ETS table: #{table}")

    {:ok, %{table: table}}
  end

  @impl true
  def handle_call({:register_module_version, sandbox_id, module, beam_data}, _from, state) do
    result = do_register_module_version(sandbox_id, module, beam_data)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:hot_swap_module, sandbox_id, module, new_beam_data}, _from, state) do
    result = do_hot_swap_module(sandbox_id, module, new_beam_data)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:rollback_module, sandbox_id, module, target_version}, _from, state) do
    result = do_rollback_module(sandbox_id, module, target_version)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:get_module_dependencies, modules}, _from, state) do
    result = do_get_module_dependencies(modules)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:cleanup_sandbox_modules, sandbox_id}, _from, state) do
    result = do_cleanup_sandbox_modules(sandbox_id)
    {:reply, result, state}
  end

  # Private implementation functions

  defp do_register_module_version(sandbox_id, module, beam_data) do
    checksum = calculate_checksum(beam_data)

    # Check if this exact version already exists
    existing_versions = :ets.lookup(@table_name, {sandbox_id, module})

    case Enum.find(existing_versions, fn {_key, version_data} ->
           version_data.checksum == checksum
         end) do
      nil ->
        # New version - calculate next version number
        next_version =
          case existing_versions do
            [] ->
              1

            versions ->
              max_version =
                versions
                |> Enum.map(fn {_key, version_data} -> version_data.version end)
                |> Enum.max()

              max_version + 1
          end

        # Extract dependencies from BEAM
        dependencies = extract_dependencies_from_beam(beam_data)

        module_version = %{
          sandbox_id: sandbox_id,
          module: module,
          version: next_version,
          beam_data: beam_data,
          loaded_at: DateTime.utc_now(),
          dependencies: dependencies,
          checksum: checksum
        }

        # Insert new version
        :ets.insert(@table_name, {{sandbox_id, module}, module_version})

        # Clean up old versions if we exceed the limit
        cleanup_old_versions(sandbox_id, module)

        Logger.debug("Registered module version",
          sandbox_id: sandbox_id,
          module: module,
          version: next_version,
          dependencies: length(dependencies)
        )

        {:ok, next_version}

      {_key, existing} ->
        # Same checksum - return existing version
        {:ok, existing.version}
    end
  end

  defp do_hot_swap_module(sandbox_id, module, new_beam_data) do
    checksum = calculate_checksum(new_beam_data)

    # Check current version
    case get_current_module_version(sandbox_id, module) do
      nil ->
        {:error, :module_not_found}

      {_key, current_version} ->
        if current_version.checksum == checksum do
          {:ok, :no_change}
        else
          perform_hot_swap(sandbox_id, module, current_version, new_beam_data)
        end
    end
  end

  defp perform_hot_swap(sandbox_id, module, current_version, new_beam_data) do
    try do
      # Find all processes using this module
      processes = find_module_processes(module)

      # Capture state for GenServers
      captured_states = capture_process_states(processes, module)

      # Load new module version
      case :code.load_binary(module, ~c"hot_swap", new_beam_data) do
        {:module, ^module} ->
          # Register the new version
          case do_register_module_version(sandbox_id, module, new_beam_data) do
            {:ok, new_version} ->
              # Migrate process states
              case migrate_process_states(
                     processes,
                     captured_states,
                     current_version.version,
                     new_version
                   ) do
                :ok ->
                  Logger.info("Hot-swapped module successfully",
                    sandbox_id: sandbox_id,
                    module: module,
                    from_version: current_version.version,
                    to_version: new_version,
                    affected_processes: length(processes)
                  )

                  {:ok, :hot_swapped}

                {:error, reason} ->
                  # Rollback on state migration failure
                  rollback_module_load(module, current_version.beam_data)
                  {:error, {:state_migration_failed, reason}}
              end
          end

        {:error, reason} ->
          {:error, {:swap_failed, reason}}
      end
    rescue
      error ->
        {:error, {:swap_failed, error}}
    end
  end

  defp do_rollback_module(sandbox_id, module, target_version) do
    case find_module_version(sandbox_id, module, target_version) do
      nil ->
        {:error, :version_not_found}

      target_module_version ->
        case :code.load_binary(module, ~c"rollback", target_module_version.beam_data) do
          {:module, ^module} ->
            Logger.info("Rolled back module to version #{target_version}",
              sandbox_id: sandbox_id,
              module: module
            )

            {:ok, :rolled_back}

          {:error, reason} ->
            {:error, {:rollback_failed, reason}}
        end
    end
  end

  defp do_get_module_dependencies(modules) do
    modules
    |> Enum.reduce(%{}, fn module, acc ->
      # Get dependencies from currently loaded module
      dependencies =
        case :code.which(module) do
          :non_existing -> []
          beam_file -> extract_dependencies_from_beam_file(beam_file)
        end

      Map.put(acc, module, dependencies)
    end)
  end

  defp do_cleanup_sandbox_modules(sandbox_id) do
    # Find all modules for this sandbox
    pattern = {{sandbox_id, :"$1"}, :"$2"}
    matches = :ets.match(@table_name, pattern)

    # Delete all entries
    Enum.each(matches, fn [module, _] ->
      :ets.delete(@table_name, {sandbox_id, module})
    end)

    Logger.debug("Cleaned up modules for sandbox",
      sandbox_id: sandbox_id,
      modules_cleaned: length(matches)
    )

    :ok
  end

  # Helper functions

  defp get_current_module_version(sandbox_id, module) do
    case :ets.lookup(@table_name, {sandbox_id, module}) do
      [] ->
        nil

      versions ->
        versions
        |> Enum.max_by(fn {_key, version_data} -> version_data.version end)
    end
  end

  defp find_module_version(sandbox_id, module, version) do
    :ets.lookup(@table_name, {sandbox_id, module})
    |> Enum.find(fn {_key, version_data} -> version_data.version == version end)
  end

  defp cleanup_old_versions(sandbox_id, module) do
    versions = :ets.lookup(@table_name, {sandbox_id, module})

    if length(versions) > @max_versions_per_module do
      # Keep only the most recent versions
      versions_to_keep =
        versions
        |> Enum.sort_by(fn {_key, version_data} -> version_data.version end, :desc)
        |> Enum.take(@max_versions_per_module)

      versions_to_delete = versions -- versions_to_keep

      Enum.each(versions_to_delete, fn {key, version_data} ->
        :ets.delete_object(@table_name, {key, version_data})
      end)

      Logger.debug("Cleaned up old module versions",
        sandbox_id: sandbox_id,
        module: module,
        deleted_versions: length(versions_to_delete)
      )
    end
  end

  defp calculate_checksum(beam_data) do
    :crypto.hash(:sha256, beam_data)
    |> Base.encode16(case: :lower)
  end

  defp extract_dependencies_from_beam(beam_data) do
    try do
      case :beam_lib.chunks(beam_data, [:imports]) do
        {:ok, {_module, [{:imports, imports}]}} ->
          imports
          |> Enum.map(fn {module, _func, _arity} -> module end)
          |> Enum.uniq()
          |> Enum.reject(&is_erlang_stdlib_module/1)

        _ ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp extract_dependencies_from_beam_file(beam_file) do
    try do
      case :beam_lib.chunks(beam_file, [:imports]) do
        {:ok, {_module, [{:imports, imports}]}} ->
          imports
          |> Enum.map(fn {module, _func, _arity} -> module end)
          |> Enum.uniq()
          |> Enum.reject(&is_erlang_stdlib_module/1)

        _ ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp is_erlang_stdlib_module(module) do
    module_str = to_string(module)

    # Basic check for Erlang/OTP modules
    String.starts_with?(module_str, ":") or
      String.starts_with?(module_str, "Elixir.Enum") or
      String.starts_with?(module_str, "Elixir.String") or
      String.starts_with?(module_str, "Elixir.Process") or
      String.starts_with?(module_str, "Elixir.GenServer")
  end

  defp find_module_processes(module) do
    # Find all processes that are running code from this module
    Process.list()
    |> Enum.filter(fn pid ->
      try do
        case Process.info(pid, :current_function) do
          {:current_function, {^module, _func, _arity}} -> true
          _ -> false
        end
      rescue
        _ -> false
      end
    end)
  end

  defp capture_process_states(processes, _module) do
    processes
    |> Enum.reduce(%{}, fn pid, acc ->
      try do
        # Check if it's a GenServer
        case :sys.get_state(pid, 1000) do
          state when is_map(state) or is_tuple(state) ->
            Map.put(acc, pid, state)

          _ ->
            acc
        end
      rescue
        _ -> acc
      catch
        _ -> acc
      end
    end)
  end

  defp migrate_process_states(processes, captured_states, old_version, new_version) do
    processes
    |> Enum.reduce_while(:ok, fn pid, :ok ->
      case Map.get(captured_states, pid) do
        nil ->
          # No state to migrate
          {:cont, :ok}

        old_state ->
          try do
            # Attempt to call code_change if available
            case GenServer.call(
                   pid,
                   {:system_replace_state,
                    fn _state ->
                      migrate_state(old_state, old_version, new_version)
                    end},
                   5000
                 ) do
              :ok -> {:cont, :ok}
              error -> {:halt, {:state_migration_failed, error}}
            end
          rescue
            error -> {:halt, {:state_migration_failed, error}}
          catch
            error -> {:halt, {:state_migration_failed, error}}
          end
      end
    end)
  end

  defp migrate_state(state, _old_version, _new_version) do
    # Default migration - return state as-is
    # This can be overridden by specific modules
    state
  end

  defp rollback_module_load(module, old_beam_data) do
    try do
      :code.load_binary(module, ~c"rollback", old_beam_data)
    rescue
      # Best effort rollback
      _ -> :ok
    end
  end
end
