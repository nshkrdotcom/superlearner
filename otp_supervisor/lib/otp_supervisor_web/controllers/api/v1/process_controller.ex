defmodule OtpSupervisorWeb.Api.V1.ProcessController do
  @moduledoc """
  REST API controller for process operations.

  This controller provides a comprehensive REST API for external tools to interact
  with OTP processes. It enables process introspection, state access, message tracing,
  and process manipulation through HTTP endpoints.

  ## Endpoints

  - `GET /api/v1/processes` - List all processes with filtering and pagination
  - `GET /api/v1/processes/:pid` - Get detailed information about a specific process
  - `GET /api/v1/processes/:pid/state` - Get the internal state of a GenServer process
  - `GET /api/v1/processes/:pid/messages` - Get message history for a traced process
  - `POST /api/v1/processes/:pid/trace` - Start message tracing for a process
  - `DELETE /api/v1/processes/:pid/trace` - Stop message tracing for a process
  - `POST /api/v1/processes/:pid/message` - Send a message to a process

  ## Authentication

  Currently uses basic API pipeline. In production, consider adding:
  - API key authentication
  - Rate limiting
  - IP allowlisting

  ## Error Handling

  All endpoints return consistent error responses with:
  - HTTP status codes (400, 404, 422, 500)
  - Error messages with context
  - Error codes for programmatic handling
  """

  use OtpSupervisorWeb, :controller

  alias OTPSupervisor.Core.Control
  alias OTPSupervisor.Core.MessageTracer

  @doc """
  Lists all processes in the system with optional filtering and pagination.

  ## Parameters

  - `page` - Page number (default: 1)
  - `per_page` - Items per page (default: 50, max: 100)
  - `type` - Filter by process type (supervisor, genserver, worker)

  ## Response

  ```json
  {
    "data": [
      {
        "pid": "#PID<0.123.0>",
        "name": "my_process",
        "type": "genserver"
      }
    ],
    "meta": {
      "total": 150,
      "page": 1,
      "per_page": 50,
      "total_pages": 3
    }
  }
  ```
  """
  def index(conn, params) do
    with {:ok, page} <- validate_page(params["page"]),
         {:ok, per_page} <- validate_per_page(params["per_page"]),
         {:ok, type_filter} <- validate_type_filter(params["type"]) do
      list_processes_with_pagination(conn, page, per_page, type_filter)
    else
      {:error, reason} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid parameters",
            code: "invalid_params",
            details: reason
          }
        })
    end
  end

  defp list_processes_with_pagination(conn, page, per_page, type_filter) do
    all_processes = Control.list_all_processes()

    filtered_processes =
      if type_filter do
        Enum.filter(all_processes, fn process ->
          Atom.to_string(process.type) == type_filter
        end)
      else
        all_processes
      end

    total = length(filtered_processes)

    paginated_processes =
      filtered_processes
      |> Enum.drop((page - 1) * per_page)
      |> Enum.take(per_page)
      |> Enum.map(&format_process_for_json/1)

    conn
    |> put_status(200)
    |> json(%{
      data: paginated_processes,
      meta: %{
        total: total,
        page: page,
        per_page: per_page,
        total_pages: ceil(total / per_page)
      }
    })
  end

  # Validation functions

  defp validate_page(nil), do: {:ok, 1}

  defp validate_page(page_str) when is_binary(page_str) do
    case Integer.parse(page_str) do
      {page, ""} when page > 0 -> {:ok, page}
      _ -> {:error, "page must be a positive integer"}
    end
  end

  defp validate_per_page(nil), do: {:ok, 50}

  defp validate_per_page(per_page_str) when is_binary(per_page_str) do
    case Integer.parse(per_page_str) do
      {per_page, ""} when per_page > 0 and per_page <= 100 -> {:ok, per_page}
      {per_page, ""} when per_page > 100 -> {:error, "per_page cannot exceed 100"}
      _ -> {:error, "per_page must be a positive integer"}
    end
  end

  defp validate_type_filter(nil), do: {:ok, nil}

  defp validate_type_filter(type) when type in ["supervisor", "genserver", "worker"],
    do: {:ok, type}

  defp validate_type_filter(_), do: {:error, "type must be one of: supervisor, genserver, worker"}

  def show(conn, %{"pid" => pid_string}) do
    # Convert URL-safe PID back to normal format by replacing first and last underscore
    decoded_pid_string =
      pid_string
      # First underscore becomes <
      |> String.replace(~r/_/, "<", global: false)
      |> String.reverse()
      # Last underscore becomes >
      |> String.replace(~r/_/, ">", global: false)
      |> String.reverse()

    case Control.to_pid(decoded_pid_string) do
      {:ok, pid} ->
        case get_detailed_process_info(pid, decoded_pid_string) do
          {:ok, process_info} ->
            conn
            |> put_status(200)
            |> json(%{data: process_info})

          {:error, :not_found} ->
            conn
            |> put_status(404)
            |> json(%{
              error: %{
                message: "Process not found",
                code: "process_not_found"
              }
            })
        end

      {:error, _} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid PID format",
            code: "invalid_pid"
          }
        })
    end
  end

  def get_state(conn, %{"pid" => pid_string}) do
    decoded_pid_string = URI.decode(pid_string)

    case Control.to_pid(decoded_pid_string) do
      {:ok, pid} ->
        case Control.get_process_state(pid) do
          {:ok, state} ->
            conn
            |> put_status(200)
            |> json(%{
              data: %{
                state: state,
                type: "genserver"
              }
            })

          {:error, :not_a_genserver} ->
            conn
            |> put_status(422)
            |> json(%{
              error: %{
                message: "Not a GenServer process",
                code: "not_genserver"
              }
            })
        end

      {:error, _} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid PID format",
            code: "invalid_pid"
          }
        })
    end
  end

  def start_trace(conn, %{"pid" => pid_string} = params) do
    decoded_pid_string = URI.decode(pid_string)

    case Control.to_pid(decoded_pid_string) do
      {:ok, pid} ->
        max_messages = params["max_messages"] || 100
        duration = params["duration"] || 60

        case MessageTracer.trace_messages(pid, max_messages: max_messages) do
          {:ok, tracer_pid} ->
            # Schedule automatic stop
            Process.send_after(self(), {:stop_trace, pid}, duration * 1000)

            conn
            |> put_status(200)
            |> json(%{
              data: %{
                status: "tracing_started",
                tracer_pid: inspect(tracer_pid),
                max_messages: max_messages,
                duration: duration
              }
            })

          {:error, reason} ->
            conn
            |> put_status(422)
            |> json(%{
              error: %{
                message: "Failed to start tracing",
                code: "trace_failed",
                reason: inspect(reason)
              }
            })
        end

      {:error, _} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid PID format",
            code: "invalid_pid"
          }
        })
    end
  end

  def stop_trace(conn, %{"pid" => pid_string}) do
    decoded_pid_string = URI.decode(pid_string)

    case Control.to_pid(decoded_pid_string) do
      {:ok, pid} ->
        MessageTracer.stop_tracing(pid)

        conn
        |> put_status(200)
        |> json(%{
          data: %{
            status: "tracing_stopped"
          }
        })

      {:error, _} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid PID format",
            code: "invalid_pid"
          }
        })
    end
  end

  def get_messages(conn, %{"pid" => pid_string}) do
    decoded_pid_string = URI.decode(pid_string)

    case Control.to_pid(decoded_pid_string) do
      {:ok, pid} ->
        messages = MessageTracer.get_message_history(pid)

        conn
        |> put_status(200)
        |> json(%{
          data: %{
            messages: messages,
            total: length(messages)
          }
        })

      {:error, _} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid PID format",
            code: "invalid_pid"
          }
        })
    end
  end

  def send_message(conn, %{"pid" => pid_string, "message" => message_params}) do
    decoded_pid_string = URI.decode(pid_string)

    case Control.to_pid(decoded_pid_string) do
      {:ok, pid} ->
        message_type = message_params["type"]
        content = message_params["content"]

        result =
          case message_type do
            "cast" ->
              GenServer.cast(pid, String.to_atom(content))
              {:ok, "cast"}

            "call" ->
              try do
                GenServer.call(pid, String.to_atom(content))
                {:ok, "call"}
              rescue
                _ -> {:error, "call_failed"}
              end

            "send" ->
              send(pid, String.to_atom(content))
              {:ok, "send"}

            _ ->
              {:error, "invalid_message_type"}
          end

        case result do
          {:ok, type} ->
            conn
            |> put_status(200)
            |> json(%{
              data: %{
                status: "message_sent",
                message_type: type
              }
            })

          {:error, reason} ->
            conn
            |> put_status(422)
            |> json(%{
              error: %{
                message: "Failed to send message",
                code: reason
              }
            })
        end

      {:error, _} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid PID format",
            code: "invalid_pid"
          }
        })
    end
  end

  # Private helper functions

  defp format_process_for_json(process) do
    %{
      "pid" => process.pid,
      "name" => process.name,
      "type" => Atom.to_string(process.type)
    }
  end

  defp get_detailed_process_info(pid, pid_string) when is_pid(pid) do
    case Process.info(pid) do
      nil ->
        {:error, :not_found}

      info ->
        process_info = %{
          "pid" => pid_string,
          "name" => get_process_name(info),
          "type" => determine_process_type_string(pid, info),
          "memory" => Keyword.get(info, :memory, 0),
          "message_queue_len" => Keyword.get(info, :message_queue_len, 0),
          "links" => get_process_links(pid),
          "monitors" => get_process_monitors(pid)
        }

        {:ok, process_info}
    end
  end

  defp get_process_name(info) do
    case Keyword.get(info, :registered_name) do
      [] -> nil
      name when is_atom(name) -> Atom.to_string(name)
      _ -> nil
    end
  end

  defp determine_process_type_string(pid, info) do
    case determine_process_type(pid, info) do
      :supervisor -> "supervisor"
      :genserver -> "genserver"
      :worker -> "worker"
    end
  end

  defp determine_process_type(pid, info) do
    cond do
      is_supervisor_pid?(pid) -> :supervisor
      is_genserver_pid?(info) -> :genserver
      true -> :worker
    end
  end

  defp is_supervisor_pid?(pid) when is_pid(pid) do
    case Process.info(pid, :dictionary) do
      nil ->
        false

      {:dictionary, dict} ->
        initial_call = Keyword.get(dict, :"$initial_call", false)

        case initial_call do
          {mod, _, _}
          when mod in [
                 :supervisor,
                 Supervisor,
                 DynamicSupervisor,
                 PartitionSupervisor,
                 Task.Supervisor
               ] ->
            true

          _ ->
            false
        end
    end
  end

  defp is_genserver_pid?(info) do
    case Keyword.get(info, :initial_call) do
      {:gen_server, _, _} -> true
      {mod, _, _} when mod in [GenServer] -> true
      _ -> false
    end
  end

  defp get_process_links(pid) do
    case Process.info(pid, :links) do
      {:links, links} -> Enum.map(links, &inspect/1)
      _ -> []
    end
  end

  defp get_process_monitors(pid) do
    case Process.info(pid, :monitors) do
      {:monitors, monitors} ->
        Enum.map(monitors, fn {_type, ref_or_pid} -> inspect(ref_or_pid) end)

      _ ->
        []
    end
  end
end
