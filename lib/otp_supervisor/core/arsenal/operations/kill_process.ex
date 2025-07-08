defmodule OTPSupervisor.Core.Arsenal.Operations.KillProcess do
  @moduledoc """
  Operation to terminate a process with a specific reason.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :delete,
      path: "/api/v1/processes/:pid",
      summary: "Terminate a process with specified reason",
      parameters: [
        %{
          name: :pid,
          type: :string,
          required: true,
          description: "Process ID to terminate",
          location: :path
        },
        %{
          name: :reason,
          type: :string,
          required: false,
          description: "Termination reason (default: 'killed')",
          location: :body
        },
        %{
          name: :force,
          type: :boolean,
          required: false,
          description: "Force termination even if process is critical",
          location: :body
        }
      ],
      responses: %{
        200 => %{
          description: "Process terminated successfully",
          schema: %{
            type: :object,
            properties: %{
              data: %{
                type: :object,
                properties: %{
                  pid: %{type: :string},
                  reason: %{type: :string},
                  terminated: %{type: :boolean}
                }
              }
            }
          }
        },
        404 => %{description: "Process not found"},
        403 => %{description: "Cannot terminate critical process"},
        400 => %{description: "Invalid parameters"}
      }
    }
  end

  def validate_params(%{"pid" => pid_string} = params) do
    case parse_pid(pid_string) do
      {:ok, pid} ->
        validated_params = %{
          "pid" => pid,
          "reason" => parse_reason(Map.get(params, "reason", "killed")),
          "force" => Map.get(params, "force", false)
        }

        {:ok, validated_params}

      {:error, reason} ->
        {:error, {:invalid_parameter, :pid, reason}}
    end
  end

  def validate_params(_params) do
    {:error, {:missing_parameter, :pid}}
  end

  def execute(%{"pid" => pid, "reason" => reason, "force" => force}) do
    cond do
      not Process.alive?(pid) ->
        {:error, :process_not_found}

      is_critical_process?(pid) and not force ->
        {:error, :critical_process_protection}

      true ->
        perform_termination(pid, reason)
    end
  end

  def format_response({pid, reason, terminated}) do
    %{
      data: %{
        pid: inspect(pid),
        reason: inspect(reason),
        terminated: terminated,
        timestamp: DateTime.utc_now()
      }
    }
  end

  defp parse_pid(pid_string) when is_binary(pid_string) do
    # Use the centralized PID parsing from Control module
    case OTPSupervisor.Core.Control.to_pid(pid_string) do
      {:ok, pid} -> {:ok, pid}
      {:error, _} -> {:error, "invalid PID format"}
    end
  end

  defp parse_reason(reason) when is_binary(reason) do
    case reason do
      "normal" -> :normal
      "shutdown" -> :shutdown
      "killed" -> :killed
      "kill" -> :kill
      other -> String.to_atom(other)
    end
  end

  defp parse_reason(reason) when is_atom(reason), do: reason

  defp is_critical_process?(pid) do
    # Check if process is part of critical system infrastructure
    case Process.info(pid, :registered_name) do
      {:registered_name, name}
      when is_atom(name) and name in [:kernel_sup, :application_controller, :code_server] ->
        true

      _ ->
        # Check if it's a supervisor in the main application tree
        is_system_supervisor?(pid)
    end
  end

  defp is_system_supervisor?(pid) do
    case Process.info(pid, :dictionary) do
      {:dictionary, dict} ->
        Enum.any?(dict, fn
          {:"$initial_call", {Supervisor, :init, 1}} -> true
          _ -> false
        end)

      _ ->
        false
    end
  end

  defp perform_termination(pid, reason) do
    try do
      # Log the termination attempt
      require Logger
      Logger.info("Terminating process #{inspect(pid)} with reason #{inspect(reason)}")

      # Monitor the process to confirm termination
      ref = Process.monitor(pid)

      # Send the exit signal
      Process.exit(pid, reason)

      # Wait for confirmation of termination
      terminated = wait_for_termination(ref, pid, 100)

      {:ok, {pid, reason, terminated}}
    rescue
      error ->
        {:error, {:termination_failed, error}}
    end
  end

  defp wait_for_termination(ref, pid, timeout) do
    receive do
      {:DOWN, ^ref, :process, ^pid, _reason} ->
        true
    after
      timeout ->
        Process.demonitor(ref, [:flush])
        not Process.alive?(pid)
    end
  end
end
