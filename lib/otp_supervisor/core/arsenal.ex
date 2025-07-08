defmodule OTPSupervisor.Core.Arsenal do
  @moduledoc """
  Main Arsenal module that coordinates the operation discovery and execution system.

  This module provides the main interface for the arsenal system and ensures
  all components are properly initialized.
  """

  use Application

  def start(_type, _args) do
    children = [
      # Start the operation registry
      OTPSupervisor.Core.Arsenal.Registry,

      # Start trace session storage
      OTPSupervisor.Core.Arsenal.Operations.Storage.TraceSessionStorage,

      # Create other ETS tables if needed
      {Task, fn -> ensure_other_ets_tables() end}
    ]

    opts = [strategy: :one_for_one, name: OTPSupervisor.Core.Arsenal.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @doc """
  Get all available arsenal operations with their configurations.
  """
  def list_operations do
    OTPSupervisor.Core.Arsenal.Registry.list_operations()
  end

  @doc """
  Execute an arsenal operation by module name with given parameters.
  """
  def execute_operation(operation_module, params) when is_atom(operation_module) do
    with {:ok, validated_params} <- validate_params(operation_module, params),
         {:ok, result} <- operation_module.execute(validated_params) do
      {:ok, result}
    end
  end

  @doc """
  Get operation configuration for a specific module.
  """
  def get_operation_config(operation_module) do
    OTPSupervisor.Core.Arsenal.Registry.get_operation(operation_module)
  end

  @doc """
  Register a new operation module dynamically.
  """
  def register_operation(operation_module) do
    OTPSupervisor.Core.Arsenal.Registry.register_operation(operation_module)
  end

  @doc """
  Generate OpenAPI/Swagger documentation for all operations.
  """
  def generate_api_docs do
    operations = list_operations()

    %{
      openapi: "3.0.0",
      info: %{
        title: "OTP Supervisor Arsenal API",
        version: "1.0.0",
        description: "Comprehensive OTP process and supervisor management API"
      },
      servers: [
        %{url: "http://localhost:4000", description: "Development server"}
      ],
      paths: generate_paths_documentation(operations)
    }
  end

  defp validate_params(operation_module, params) do
    if function_exported?(operation_module, :validate_params, 1) do
      operation_module.validate_params(params)
    else
      {:ok, params}
    end
  end

  defp ensure_other_ets_tables do
    # Create other ETS tables needed by arsenal operations
    # Note: trace_sessions is now managed by TraceSessionStorage GenServer

    case :ets.whereis(:operation_stats) do
      :undefined ->
        :ets.new(:operation_stats, [:named_table, :public, :set, {:read_concurrency, true}])

      _ ->
        :ok
    end
  end

  defp generate_paths_documentation(operations) do
    operations
    |> Enum.reduce(%{}, fn operation, acc ->
      path = operation.path
      method = operation.method

      path_doc = %{
        summary: operation.summary,
        parameters: format_parameters_for_docs(Map.get(operation, :parameters, [])),
        responses: Map.get(operation, :responses, %{})
      }

      # Add to paths, potentially merging with existing path if multiple methods
      current_path = Map.get(acc, path, %{})
      updated_path = Map.put(current_path, method, path_doc)

      Map.put(acc, path, updated_path)
    end)
  end

  defp format_parameters_for_docs(parameters) do
    Enum.map(parameters, fn param ->
      %{
        name: param.name,
        in: param_location_to_openapi(param.location),
        required: Map.get(param, :required, false),
        description: Map.get(param, :description, ""),
        schema: %{type: param.type}
      }
    end)
  end

  defp param_location_to_openapi(:path), do: "path"
  defp param_location_to_openapi(:query), do: "query"
  defp param_location_to_openapi(:body), do: "requestBody"
  defp param_location_to_openapi(_), do: "query"
end
