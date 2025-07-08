defmodule OtpSupervisorWeb.ArsenalPlug do
  @moduledoc """
  Plug that dynamically routes arsenal operations to the appropriate handler.

  This plug inspects the request path and method to determine which arsenal
  operation should handle the request, providing a more flexible alternative
  to compile-time route generation.
  """

  import Plug.Conn

  alias OTPSupervisor.Core.Arsenal.Registry

  def init(opts), do: opts

  def call(conn, _opts) do
    # Always try to match operations for any request to /api/v1
    if String.starts_with?(conn.request_path, "/api/v1") do
      # Reconstruct the full path from the catch-all route  
      full_path =
        case conn.path_params do
          %{"path" => path_parts} when is_list(path_parts) ->
            "/api/v1/" <> Enum.join(path_parts, "/")

          %{"path" => path_part} when is_binary(path_part) ->
            "/api/v1/" <> path_part

          _ ->
            conn.request_path
        end

      # Create a modified conn with the reconstructed path for matching
      matching_conn = %{conn | request_path: full_path}

      case find_matching_operation(matching_conn) do
        {:ok, operation_module} ->
          execute_arsenal_operation(conn, operation_module)

        {:error, :no_match} ->
          # Check if this should be handled by Arsenal (has catch-all route params)
          case conn.path_params do
            %{"path" => _} ->
              # This came from the catch-all route, so it should be a 404 from Arsenal
              conn
              |> put_status(:not_found)
              |> Phoenix.Controller.json(%{
                error: %{
                  message: "Arsenal operation not found",
                  code: "operation_not_found",
                  path: conn.request_path,
                  method: conn.method
                }
              })
              |> halt()

            _ ->
              # No catch-all params, pass through to manual controllers
              conn
          end
      end
    else
      conn
    end
  end

  defp find_matching_operation(conn) do
    operations = Registry.list_operations()

    matching_operation = Enum.find(operations, &matches_request?(conn, &1))

    case matching_operation do
      nil -> {:error, :no_match}
      operation -> {:ok, operation.module}
    end
  end

  defp matches_request?(conn, operation) do
    method_matches?(conn.method, operation.method) and
      path_matches?(conn.request_path, operation.path)
  end

  defp method_matches?(request_method, operation_method) do
    String.downcase(request_method) == Atom.to_string(operation_method)
  end

  defp path_matches?(request_path, operation_path) do
    # URL decode the request path first to handle encoded characters
    decoded_request_path = URI.decode(request_path)

    pattern =
      operation_path
      |> String.replace(~r/:([^\/]+)/, "[^/]+")
      |> String.replace("/", "\\/")

    regex = Regex.compile!("^#{pattern}$")

    # Try both encoded and decoded paths
    Regex.match?(regex, request_path) or Regex.match?(regex, decoded_request_path)
  end

  defp execute_arsenal_operation(conn, operation_module) do
    with {:ok, config} <- get_operation_config(operation_module),
         {:ok, merged_params} <- merge_request_parameters(conn, config),
         {:ok, validated_params} <-
           validate_operation_parameters(operation_module, merged_params),
         {:ok, result} <- execute_operation(operation_module, validated_params),
         {:ok, formatted_response} <- format_operation_response(operation_module, result) do
      conn
      |> put_status(:ok)
      |> Phoenix.Controller.json(formatted_response)
      |> halt()
    else
      {:error, {:validation_error, errors}} ->
        conn
        |> put_status(:unprocessable_entity)
        |> Phoenix.Controller.json(%{
          error: %{
            message: "Validation failed",
            code: "validation_error",
            details: inspect(errors)
          }
        })
        |> halt()

      {:error, :process_not_found} ->
        conn
        |> put_status(:not_found)
        |> Phoenix.Controller.json(%{
          error: %{
            message: "Process not found",
            code: "process_not_found"
          }
        })
        |> halt()

      {:error, :operation_not_found} ->
        conn
        |> put_status(:not_found)
        |> Phoenix.Controller.json(%{
          error: %{
            message: "Operation not found",
            code: "operation_not_found"
          }
        })
        |> halt()

      {:error, :call_timeout} ->
        conn
        |> put_status(:request_timeout)
        |> Phoenix.Controller.json(%{
          error: %{
            message: "Operation timed out",
            code: "timeout"
          }
        })
        |> halt()

      {:error, :critical_process_protection} ->
        conn
        |> put_status(:forbidden)
        |> Phoenix.Controller.json(%{
          error: %{
            message: "Cannot modify critical system process",
            code: "critical_process_protection"
          }
        })
        |> halt()

      {:error, reason} ->
        require Logger
        Logger.error("Arsenal operation failed: #{inspect(reason)}")

        conn
        |> put_status(:internal_server_error)
        |> Phoenix.Controller.json(%{
          error: %{
            message: "Operation failed",
            code: "operation_error",
            details: inspect(reason)
          }
        })
        |> halt()
    end
  end

  defp get_operation_config(operation_module) do
    case Registry.get_operation(operation_module) do
      {:ok, config} ->
        {:ok, config}

      {:error, :not_found} ->
        try do
          config = operation_module.rest_config()
          {:ok, config}
        rescue
          _ -> {:error, :operation_not_found}
        end
    end
  end

  defp merge_request_parameters(conn, config) do
    query_params = conn.query_params || %{}

    body_params =
      case conn.body_params do
        %Plug.Conn.Unfetched{} -> %{}
        params when is_map(params) -> params
        _ -> %{}
      end

    path_params = extract_path_parameters(conn.request_path, config.path)

    merged_params =
      path_params
      |> Map.merge(query_params)
      |> Map.merge(body_params)

    {:ok, merged_params}
  end

  defp validate_operation_parameters(operation_module, params) do
    if function_exported?(operation_module, :validate_params, 1) do
      case operation_module.validate_params(params) do
        {:ok, validated_params} -> {:ok, validated_params}
        {:error, reason} -> {:error, {:validation_error, reason}}
      end
    else
      {:ok, params}
    end
  end

  defp execute_operation(operation_module, params) do
    try do
      operation_module.execute(params)
    rescue
      error -> {:error, {:execution_error, error}}
    catch
      :exit, reason -> {:error, {:execution_exit, reason}}
    end
  end

  defp format_operation_response(operation_module, result) do
    if function_exported?(operation_module, :format_response, 1) do
      try do
        formatted = operation_module.format_response(result)
        {:ok, formatted}
      rescue
        _error ->
          {:ok, %{data: result}}
      end
    else
      {:ok, %{data: result}}
    end
  end

  defp extract_path_parameters(request_path, pattern_path) do
    # URL decode the request path to handle encoded characters like PIDs
    decoded_request_path = URI.decode(request_path)
    request_segments = String.split(decoded_request_path, "/", trim: true)
    pattern_segments = String.split(pattern_path, "/", trim: true)

    pattern_segments
    |> Enum.zip(request_segments)
    |> Enum.reduce(%{}, fn {pattern_seg, request_seg}, acc ->
      case String.starts_with?(pattern_seg, ":") do
        true ->
          param_name = String.slice(pattern_seg, 1..-1//1)
          Map.put(acc, param_name, request_seg)

        false ->
          acc
      end
    end)
  end
end
