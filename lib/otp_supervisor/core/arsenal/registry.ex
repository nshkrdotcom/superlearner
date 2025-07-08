defmodule OTPSupervisor.Core.Arsenal.Registry do
  @moduledoc """
  Registry for discovering and managing arsenal operations.

  This module automatically discovers all modules implementing the Operation behavior
  and provides metadata for REST API generation.
  """

  use GenServer

  @table_name :arsenal_operations

  # Known Arsenal operations that should be automatically registered
  @known_operations [
    OTPSupervisor.Core.Arsenal.Operations.GetProcessInfo,
    OTPSupervisor.Core.Arsenal.Operations.KillProcess,
    OTPSupervisor.Core.Arsenal.Operations.ListSupervisors,
    OTPSupervisor.Core.Arsenal.Operations.SendMessage,
    OTPSupervisor.Core.Arsenal.Operations.TraceProcess
  ]

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Get all registered operations with their metadata.
  """
  def list_operations do
    GenServer.call(__MODULE__, :list_operations)
  end

  @doc """
  Get operation by module name.
  """
  def get_operation(module) when is_atom(module) do
    GenServer.call(__MODULE__, {:get_operation, module})
  end

  @doc """
  Register a new operation module.
  """
  def register_operation(module) when is_atom(module) do
    GenServer.call(__MODULE__, {:register_operation, module})
  end

  def init(_opts) do
    :ets.new(@table_name, [:named_table, :public, :set, {:read_concurrency, true}])

    # Register known Arsenal operations explicitly
    register_known_operations()

    {:ok, %{}}
  end

  def handle_call(:list_operations, _from, state) do
    # Get operations from ETS
    operations =
      @table_name
      |> :ets.tab2list()
      |> Enum.map(fn {module, config} -> Map.put(config, :module, module) end)

    # If no operations found, try to register known operations again
    operations =
      if length(operations) == 0 do
        register_known_operations()

        @table_name
        |> :ets.tab2list()
        |> Enum.map(fn {module, config} -> Map.put(config, :module, module) end)
      else
        operations
      end

    {:reply, operations, state}
  end

  def handle_call({:get_operation, module}, _from, state) do
    case :ets.lookup(@table_name, module) do
      [{^module, config}] ->
        {:reply, {:ok, config}, state}

      [] ->
        # Try to register the module if it's a known operation
        if module in @known_operations do
          case register_operation_module(module) do
            {:ok, config} ->
              :ets.insert(@table_name, {module, config})
              {:reply, {:ok, config}, state}

            error ->
              {:reply, error, state}
          end
        else
          {:reply, {:error, :not_found}, state}
        end
    end
  end

  def handle_call({:register_operation, module}, _from, state) do
    case register_operation_module(module) do
      {:ok, config} ->
        :ets.insert(@table_name, {module, config})
        {:reply, {:ok, config}, state}

      error ->
        {:reply, error, state}
    end
  end

  defp register_known_operations do
    # Register all known Arsenal operations
    Enum.each(@known_operations, fn module ->
      case register_operation_module(module) do
        {:ok, config} ->
          :ets.insert(@table_name, {module, config})
          require Logger
          Logger.debug("Registered Arsenal operation: #{module}")

        {:error, reason} ->
          require Logger
          Logger.warning("Failed to register Arsenal operation #{module}: #{inspect(reason)}")
      end
    end)
  end

  defp register_operation_module(module) do
    try do
      # Try to call rest_config directly instead of using function_exported?
      config = module.rest_config()

      # Validate config structure
      case validate_rest_config(config) do
        :ok -> {:ok, Map.put(config, :module, module)}
        error -> error
      end
    rescue
      UndefinedFunctionError -> {:error, :invalid_operation_module}
      error -> {:error, {:module_error, error}}
    end
  end

  defp validate_rest_config(config) do
    required_keys = [:method, :path, :summary]

    case Enum.all?(required_keys, &Map.has_key?(config, &1)) do
      true -> :ok
      false -> {:error, {:invalid_config, :missing_required_keys}}
    end
  end
end
