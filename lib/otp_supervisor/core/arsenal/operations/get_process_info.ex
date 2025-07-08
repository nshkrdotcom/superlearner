defmodule OTPSupervisor.Core.Arsenal.Operations.GetProcessInfo do
  @moduledoc """
  Operation to get comprehensive process information.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :get,
      path: "/api/v1/processes/:pid/info",
      summary: "Get comprehensive process information",
      parameters: [
        %{
          name: :pid,
          type: :string,
          required: true,
          description: "Process ID in string format",
          location: :path
        },
        %{
          name: :keys,
          type: :array,
          required: false,
          description: "Specific info keys to retrieve",
          location: :query
        }
      ],
      responses: %{
        200 => %{
          description: "Process information retrieved successfully",
          schema: %{
            type: :object,
            properties: %{
              data: %{
                type: :object,
                properties: %{
                  pid: %{type: :string},
                  memory: %{type: :integer},
                  message_queue_len: %{type: :integer},
                  links: %{type: :array},
                  monitors: %{type: :array},
                  status: %{type: :string}
                }
              }
            }
          }
        },
        404 => %{description: "Process not found"},
        400 => %{description: "Invalid PID format"}
      }
    }
  end

  def validate_params(%{"pid" => pid_string} = params) do
    case parse_pid(pid_string) do
      {:ok, pid} ->
        validated_params = Map.put(params, "pid", pid)

        # Validate keys if provided
        case Map.get(params, "keys") do
          nil ->
            {:ok, validated_params}

          keys when is_list(keys) ->
            case validate_process_info_keys(keys) do
              {:ok, atom_keys} -> {:ok, Map.put(validated_params, "keys", atom_keys)}
              {:error, reason} -> {:error, {:invalid_parameter, :keys, reason}}
            end

          _ ->
            {:error, {:invalid_parameter, :keys, "must be an array"}}
        end

      {:error, reason} ->
        {:error, {:invalid_parameter, :pid, reason}}
    end
  rescue
    ArgumentError -> {:error, {:invalid_parameter, :keys, "invalid key name"}}
  end

  def validate_params(_params) do
    {:error, {:missing_parameter, :pid}}
  end

  def execute(%{"pid" => pid} = params) do
    if Process.alive?(pid) do
      keys = Map.get(params, "keys", :all)

      case get_process_information(pid, keys) do
        {:ok, info} -> {:ok, info}
        {:error, reason} -> {:error, reason}
      end
    else
      {:error, :process_not_found}
    end
  end

  def format_response(info) when is_map(info) do
    %{
      data: format_process_info(info)
    }
  end

  defp parse_pid(pid_string) when is_binary(pid_string) do
    # Use the centralized PID parsing from Control module
    case OTPSupervisor.Core.Control.to_pid(pid_string) do
      {:ok, pid} -> {:ok, pid}
      {:error, _} -> {:error, "invalid PID format"}
    end
  end

  # List of valid process info keys
  @valid_process_info_keys [
    :status,
    :message_queue_len,
    :links,
    :monitors,
    :monitored_by,
    :trap_exit,
    :priority,
    :reductions,
    :binary,
    :memory,
    :garbage_collection,
    :group_leader,
    :total_heap_size,
    :heap_size,
    :stack_size,
    :current_function,
    :current_location,
    :current_stacktrace,
    :initial_call,
    :dictionary,
    :error_handler,
    :suspending,
    :min_heap_size,
    :min_bin_vheap_size,
    :max_heap_size,
    :registered_name
  ]

  defp validate_process_info_keys(keys) do
    try do
      atom_keys =
        Enum.map(keys, fn
          key when is_binary(key) -> String.to_existing_atom(key)
          key when is_atom(key) -> key
          _ -> throw({:invalid_key_type})
        end)

      # Check if all keys are valid process info keys
      invalid_keys = Enum.reject(atom_keys, &(&1 in @valid_process_info_keys))

      case invalid_keys do
        [] -> {:ok, atom_keys}
        [key | _] -> {:error, "invalid process info key: #{key}"}
      end
    rescue
      ArgumentError -> {:error, "invalid key name"}
    catch
      {:invalid_key_type} -> {:error, "keys must be strings or atoms"}
    end
  end

  defp get_process_information(pid, :all) do
    case Process.info(pid) do
      nil ->
        {:error, :process_not_found}

      info ->
        # Add memory info if not present
        info_map = Enum.into(info, %{})

        info_with_memory =
          case Map.get(info_map, :memory) do
            nil ->
              case Process.info(pid, :memory) do
                nil -> info_map
                memory -> Map.put(info_map, :memory, memory)
              end

            _ ->
              info_map
          end

        {:ok, info_with_memory}
    end
  end

  defp get_process_information(pid, keys) when is_list(keys) do
    try do
      info =
        keys
        |> Enum.map(fn key -> {key, Process.info(pid, key)} end)
        |> Enum.into(%{})

      {:ok, info}
    rescue
      _ -> {:error, :invalid_info_keys}
    end
  end

  defp format_process_info(info) do
    info
    |> Enum.map(fn {key, value} -> {key, format_info_value(value)} end)
    |> Enum.into(%{})
  end

  defp format_info_value(pid) when is_pid(pid), do: inspect(pid)

  defp format_info_value(list) when is_list(list) do
    Enum.map(list, &format_info_value/1)
  end

  defp format_info_value(value), do: value
end
