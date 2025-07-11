defmodule OTPSupervisor.Core.Arsenal.Operations.CreateSandbox do
  @moduledoc """
  Operation to create a new sandbox environment.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :post,
      path: "/api/v1/sandboxes",
      summary: "Create a new sandbox environment",
      parameters: [
        %{
          name: :sandbox_id,
          type: :string,
          required: true,
          description: "Unique identifier for the sandbox",
          location: :body
        },
        %{
          name: :supervisor_module,
          type: :string,
          required: true,
          description: "Supervisor module to use (e.g., 'OtpSandbox.TestDemoSupervisor')",
          location: :body
        },
        %{
          name: :strategy,
          type: :string,
          required: false,
          description: "Supervisor strategy (one_for_one, one_for_all, rest_for_one)",
          location: :body
        },
        %{
          name: :max_restarts,
          type: :integer,
          required: false,
          description: "Maximum number of restarts allowed",
          location: :body
        },
        %{
          name: :max_seconds,
          type: :integer,
          required: false,
          description: "Time window for restart counting",
          location: :body
        }
      ],
      responses: %{
        201 => %{
          description: "Sandbox created successfully",
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
                  created_at: %{type: :integer},
                  restart_count: %{type: :integer}
                }
              }
            }
          }
        },
        400 => %{description: "Invalid parameters"},
        409 => %{description: "Sandbox already exists"}
      }
    }
  end

  def validate_params(
        %{"sandbox_id" => sandbox_id, "supervisor_module" => supervisor_module} = params
      ) do
    validated_params = %{
      "sandbox_id" => validate_sandbox_id(sandbox_id),
      "supervisor_module" => validate_supervisor_module(supervisor_module),
      "strategy" => validate_strategy(Map.get(params, "strategy", "one_for_one")),
      "max_restarts" => parse_positive_integer(Map.get(params, "max_restarts", "3"), 3),
      "max_seconds" => parse_positive_integer(Map.get(params, "max_seconds", "5"), 5)
    }

    {:ok, validated_params}
  rescue
    error -> {:error, {:invalid_parameters, error}}
  end

  def validate_params(_params) do
    {:error, {:missing_parameters, "sandbox_id and supervisor_module are required"}}
  end

  def execute(%{
        "sandbox_id" => sandbox_id,
        "supervisor_module" => supervisor_module,
        "strategy" => strategy,
        "max_restarts" => max_restarts,
        "max_seconds" => max_seconds
      }) do
    opts = [
      strategy: strategy,
      max_restarts: max_restarts,
      max_seconds: max_seconds
    ]

    case OTPSupervisor.Core.SandboxManager.create_sandbox(sandbox_id, supervisor_module, opts) do
      {:ok, sandbox_info} ->
        {:ok, sandbox_info}

      {:error, {:already_exists, existing_sandbox}} ->
        {:error, {:sandbox_already_exists, existing_sandbox.id}}

      {:error, reason} ->
        {:error, {:sandbox_creation_failed, reason}}
    end
  end

  def format_response(sandbox_info) do
    %{
      data: %{
        id: sandbox_info.id,
        app_name: sandbox_info.app_name,
        supervisor_module: format_module_name(sandbox_info.supervisor_module),
        app_pid: inspect(sandbox_info.app_pid),
        supervisor_pid: inspect(sandbox_info.supervisor_pid),
        status: "running",
        created_at: sandbox_info.created_at,
        restart_count: sandbox_info.restart_count,
        opts: sandbox_info.opts
      }
    }
  end

  defp validate_sandbox_id(sandbox_id) when is_binary(sandbox_id) and byte_size(sandbox_id) > 0 do
    # Ensure it's a valid identifier (alphanumeric, underscores, hyphens)
    if Regex.match?(~r/^[a-zA-Z0-9_-]+$/, sandbox_id) do
      sandbox_id
    else
      raise "Invalid sandbox_id format. Use alphanumeric characters, underscores, or hyphens only."
    end
  end

  defp validate_sandbox_id(_), do: raise("sandbox_id must be a non-empty string")

  defp validate_supervisor_module(module_string) when is_binary(module_string) do
    try do
      # Convert string to module atom
      module = String.to_existing_atom("Elixir." <> module_string)

      # Ensure the module is loaded first
      Code.ensure_loaded(module)

      # Verify the module exists and has start_link function
      if function_exported?(module, :start_link, 1) do
        module
      else
        raise "Module #{module_string} does not export start_link/1"
      end
    rescue
      ArgumentError ->
        raise "Module #{module_string} does not exist"
    end
  end

  defp validate_supervisor_module(_), do: raise("supervisor_module must be a string")

  defp validate_strategy(strategy)
       when strategy in ["one_for_one", "one_for_all", "rest_for_one"] do
    String.to_atom(strategy)
  end

  defp validate_strategy(_),
    do: raise("Invalid strategy. Use: one_for_one, one_for_all, or rest_for_one")

  defp parse_positive_integer(value, default) when is_binary(value) do
    case Integer.parse(value) do
      {int, ""} when int > 0 -> int
      _ -> default
    end
  end

  defp parse_positive_integer(value, _default) when is_integer(value) and value > 0, do: value
  defp parse_positive_integer(_, default), do: default

  defp format_module_name(module) when is_atom(module), do: Atom.to_string(module)
  defp format_module_name(module), do: inspect(module)
end
