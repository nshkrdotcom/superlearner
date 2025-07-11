defmodule OTPSupervisor.Core.Arsenal.Operations.RestartSandbox do
  @moduledoc """
  Operation to restart a sandbox environment.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :post,
      path: "/api/v1/sandboxes/:sandbox_id/restart",
      summary: "Restart a sandbox environment",
      parameters: [
        %{
          name: :sandbox_id,
          type: :string,
          required: true,
          description: "Unique identifier of the sandbox to restart",
          location: :path
        }
      ],
      responses: %{
        200 => %{
          description: "Sandbox restarted successfully",
          schema: %{
            type: :object,
            properties: %{
              data: %{
                type: :object,
                properties: %{
                  id: %{type: :string},
                  app_name: %{type: :string},
                  supervisor_module: %{type: :string},
                  app_pid: %{type: :string},
                  supervisor_pid: %{type: :string},
                  status: %{type: :string},
                  restart_count: %{type: :integer},
                  restarted_at: %{type: :string}
                }
              }
            }
          }
        },
        404 => %{description: "Sandbox not found"},
        400 => %{description: "Invalid parameters"}
      }
    }
  end

  def validate_params(%{"sandbox_id" => sandbox_id}) do
    validated_params = %{
      "sandbox_id" => validate_sandbox_id(sandbox_id)
    }

    {:ok, validated_params}
  rescue
    error -> {:error, {:invalid_parameters, error}}
  end

  def validate_params(_params) do
    {:error, {:missing_parameter, "sandbox_id is required"}}
  end

  def execute(%{"sandbox_id" => sandbox_id}) do
    case OTPSupervisor.Core.SandboxManager.restart_sandbox(sandbox_id) do
      {:ok, restarted_sandbox_info} ->
        restarted_at = DateTime.utc_now()

        # Log the restart
        require Logger

        Logger.info(
          "Sandbox #{sandbox_id} restarted via Arsenal API at #{DateTime.to_iso8601(restarted_at)}"
        )

        {:ok, {restarted_sandbox_info, restarted_at}}

      {:error, :not_found} ->
        {:error, :sandbox_not_found}

      {:error, reason} ->
        {:error, {:restart_failed, reason}}
    end
  end

  def format_response({sandbox_info, restarted_at}) do
    %{
      data: %{
        id: sandbox_info.id,
        app_name: sandbox_info.app_name,
        supervisor_module: format_module_name(sandbox_info.supervisor_module),
        app_pid: inspect(sandbox_info.app_pid),
        supervisor_pid: inspect(sandbox_info.supervisor_pid),
        status: "running",
        restart_count: sandbox_info.restart_count,
        restarted_at: DateTime.to_iso8601(restarted_at),
        opts: sandbox_info.opts
      }
    }
  end

  defp validate_sandbox_id(sandbox_id) when is_binary(sandbox_id) and byte_size(sandbox_id) > 0 do
    sandbox_id
  end

  defp validate_sandbox_id(_), do: raise("sandbox_id must be a non-empty string")

  defp format_module_name(module) when is_atom(module), do: Atom.to_string(module)
  defp format_module_name(module), do: inspect(module)
end
