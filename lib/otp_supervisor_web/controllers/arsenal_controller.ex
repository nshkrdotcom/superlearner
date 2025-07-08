defmodule OtpSupervisorWeb.ArsenalController do
  @moduledoc """
  Controller for Arsenal API documentation and metadata endpoints.

  This controller provides endpoints for discovering available Arsenal operations
  and their API documentation. The actual operation execution is handled by
  the ArsenalPlug.
  """

  use OtpSupervisorWeb, :controller

  alias OTPSupervisor.Core.Arsenal.Registry

  def docs(conn, _params) do
    operations = Registry.list_operations()

    docs = generate_api_docs(operations)

    conn
    |> put_status(:ok)
    |> json(docs)
  end

  def list_operations(conn, _params) do
    operations = Registry.list_operations()

    operation_list =
      Enum.map(operations, fn op ->
        %{
          module: op.module,
          method: op.method,
          path: op.path,
          summary: op.summary,
          parameters: Map.get(op, :parameters, []),
          responses: Map.get(op, :responses, %{})
        }
      end)

    conn
    |> put_status(:ok)
    |> json(%{data: operation_list})
  end

  def not_found(conn, _params) do
    conn
    |> put_status(:not_found)
    |> json(%{error: %{message: "Arsenal endpoint not found", code: "endpoint_not_found"}})
  end

  def operation_handler(conn, params) do
    # This should never be called because ArsenalPlug intercepts the request
    require Logger
    Logger.error("ArsenalController.operation_handler called! This should not happen!")
    Logger.error("Params: #{inspect(params)}")
    Logger.error("Request path: #{conn.request_path}")

    conn
    |> put_status(:internal_server_error)
    |> json(%{error: %{message: "ArsenalPlug did not intercept request", code: "plug_failure"}})
  end

  defp generate_api_docs(operations) do
    %{
      openapi: "3.0.0",
      info: %{
        title: "Arsenal API",
        version: "1.0.0",
        description: "Dynamically generated OTP operations API"
      },
      servers: [
        %{
          url: "/api/v1",
          description: "Arsenal API v1"
        }
      ],
      paths: generate_paths(operations)
    }
  end

  defp generate_paths(operations) do
    operations
    |> Enum.reduce(%{}, fn op, acc ->
      path_key = op.path |> String.replace("/api/v1", "")
      method_key = Atom.to_string(op.method)

      path_spec = %{
        summary: op.summary,
        parameters: generate_parameters(Map.get(op, :parameters, [])),
        responses: generate_responses(Map.get(op, :responses, %{}))
      }

      existing_path = Map.get(acc, path_key, %{})
      updated_path = Map.put(existing_path, method_key, path_spec)

      Map.put(acc, path_key, updated_path)
    end)
  end

  defp generate_parameters(parameters) do
    Enum.map(parameters, fn param ->
      %{
        name: param.name,
        in: param.location,
        required: param.required,
        description: param.description,
        schema: %{
          type: param.type
        }
      }
    end)
  end

  defp generate_responses(responses) do
    responses
    |> Enum.map(fn {code, response} ->
      {Integer.to_string(code), response}
    end)
    |> Enum.into(%{})
  end
end
