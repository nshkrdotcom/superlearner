defmodule OTPSupervisor.Core.Arsenal.Operations.SendMessage do
  @moduledoc """
  Operation to send arbitrary messages to a process.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :post,
      path: "/api/v1/processes/:pid/message",
      summary: "Send a message to a process",
      parameters: [
        %{
          name: :pid,
          type: :string,
          required: true,
          description: "Target process ID",
          location: :path
        },
        %{
          name: :message,
          type: :object,
          required: true,
          description: "Message content to send",
          location: :body
        },
        %{
          name: :message_type,
          type: :string,
          required: false,
          description: "Type of message: 'send', 'cast', 'call' (default: 'send')",
          location: :body
        },
        %{
          name: :timeout_ms,
          type: :integer,
          required: false,
          description: "Timeout for 'call' type messages (default: 5000)",
          location: :body
        },
        %{
          name: :track_response,
          type: :boolean,
          required: false,
          description: "Whether to track and return response for debugging",
          location: :body
        }
      ],
      responses: %{
        200 => %{
          description: "Message sent successfully",
          schema: %{
            type: :object,
            properties: %{
              data: %{
                type: :object,
                properties: %{
                  message_sent: %{type: :boolean},
                  message_type: %{type: :string},
                  target_pid: %{type: :string},
                  response: %{
                    type: :object,
                    description: "Response if call type or tracking enabled"
                  }
                }
              }
            }
          }
        },
        404 => %{description: "Process not found"},
        400 => %{description: "Invalid message or parameters"},
        408 => %{description: "Call timeout"}
      }
    }
  end

  def validate_params(%{"pid" => pid_string} = params) do
    with {:ok, pid} <- parse_pid(pid_string),
         {:ok, message} <- validate_message(Map.get(params, "message")),
         {:ok, message_type} <- validate_message_type(Map.get(params, "message_type", "send")),
         {:ok, timeout} <- validate_timeout(Map.get(params, "timeout_ms", 5000)) do
      validated_params = %{
        "pid" => pid,
        "message" => message,
        "message_type" => message_type,
        "timeout_ms" => timeout,
        "track_response" => Map.get(params, "track_response", false)
      }

      {:ok, validated_params}
    end
  end

  def validate_params(_params) do
    {:error, {:missing_parameter, :pid}}
  end

  def execute(%{
        "pid" => pid,
        "message" => message,
        "message_type" => message_type,
        "timeout_ms" => timeout,
        "track_response" => track_response
      }) do
    if Process.alive?(pid) do
      case send_message_by_type(pid, message, message_type, timeout, track_response) do
        {:ok, result} -> {:ok, result}
        {:error, reason} -> {:error, reason}
      end
    else
      {:error, :process_not_found}
    end
  end

  def format_response(result) do
    %{data: format_result_values(result)}
  end

  defp parse_pid(pid_string) when is_binary(pid_string) do
    case OTPSupervisor.Core.Control.to_pid(pid_string) do
      {:ok, pid} -> {:ok, pid}
      {:error, _} -> {:error, {:invalid_parameter, :pid, "invalid PID format"}}
    end
  end

  defp validate_message(nil), do: {:error, {:missing_parameter, :message}}
  defp validate_message(message), do: {:ok, parse_message_content(message)}

  defp validate_message_type(type) when type in ["send", "cast", "call"],
    do: {:ok, String.to_atom(type)}

  defp validate_message_type(_type),
    do: {:error, {:invalid_parameter, :message_type, "must be 'send', 'cast', or 'call'"}}

  defp validate_timeout(timeout) when is_integer(timeout) and timeout > 0, do: {:ok, timeout}

  defp validate_timeout(_),
    do: {:error, {:invalid_parameter, :timeout_ms, "must be positive integer"}}

  defp parse_message_content(message) when is_map(message) do
    # Convert string keys to atoms for common patterns
    case Map.get(message, "type") do
      nil ->
        message

      type ->
        # Convert to tuple format if type is specified
        content = Map.get(message, "content", Map.delete(message, "type"))
        {String.to_atom(type), content}
    end
  end

  defp parse_message_content(message), do: message

  defp send_message_by_type(pid, message, :send, _timeout, track_response) do
    try do
      # For regular send, we can't get a response unless we set up tracking
      result =
        if track_response do
          # Set up a temporary monitor to track if message was received
          ref = make_ref()
          tracking_message = {:tracked_message, ref, message}

          send(pid, tracking_message)

          # Note: This is a simple implementation. In practice, you'd need
          # cooperation from the target process to confirm message receipt
          %{
            "message_sent" => true,
            "message_type" => "send",
            "target_pid" => inspect(pid),
            "tracking_ref" => inspect(ref),
            "note" => "Message sent but response tracking requires target process cooperation"
          }
        else
          send(pid, message)

          %{
            "message_sent" => true,
            "message_type" => "send",
            "target_pid" => inspect(pid)
          }
        end

      {:ok, result}
    rescue
      error -> {:error, {:send_failed, error}}
    end
  end

  defp send_message_by_type(pid, message, :cast, _timeout, _track_response) do
    try do
      # GenServer cast
      GenServer.cast(pid, message)

      result = %{
        "message_sent" => true,
        "message_type" => "cast",
        "target_pid" => inspect(pid)
      }

      {:ok, result}
    rescue
      error -> {:error, {:cast_failed, error}}
    end
  end

  defp send_message_by_type(pid, message, :call, timeout, _track_response) do
    try do
      # GenServer call with timeout
      response = GenServer.call(pid, message, timeout)

      result = %{
        "message_sent" => true,
        "message_type" => "call",
        "target_pid" => inspect(pid),
        "response" => format_response_value(response)
      }

      {:ok, result}
    rescue
      error -> {:error, {:call_failed, error}}
    catch
      :exit, {:timeout, _} -> {:error, :call_timeout}
      :exit, reason -> {:error, {:call_failed, reason}}
    end
  end

  defp format_result_values(result) when is_map(result) do
    Map.new(result, fn {key, value} -> {key, format_response_value(value)} end)
  end

  defp format_result_values(result), do: format_response_value(result)

  defp format_response_value(value) when is_pid(value), do: inspect(value)
  defp format_response_value(value) when is_reference(value), do: inspect(value)
  defp format_response_value(value) when is_function(value), do: "#Function<>"
  defp format_response_value(value) when is_port(value), do: inspect(value)

  defp format_response_value(value) when is_map(value) do
    Map.new(value, fn {key, val} -> {key, format_response_value(val)} end)
  end

  defp format_response_value(value), do: value
end
