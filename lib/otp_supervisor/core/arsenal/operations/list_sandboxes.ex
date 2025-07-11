defmodule OTPSupervisor.Core.Arsenal.Operations.ListSandboxes do
  @moduledoc """
  Operation to list all sandboxes in the system.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :get,
      path: "/api/v1/sandboxes",
      summary: "List all sandboxes in the system",
      parameters: [
        %{
          name: :status,
          type: :string,
          required: false,
          description: "Filter sandboxes by status (running, stopped)",
          location: :query
        },
        %{
          name: :page,
          type: :integer,
          required: false,
          description: "Page number for pagination (default: 1)",
          location: :query
        },
        %{
          name: :per_page,
          type: :integer,
          required: false,
          description: "Items per page (default: 20, max: 100)",
          location: :query
        }
      ],
      responses: %{
        200 => %{
          description: "Sandboxes retrieved successfully",
          schema: %{
            type: :object,
            properties: %{
              data: %{
                type: :array,
                items: %{
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
              },
              meta: %{
                type: :object,
                properties: %{
                  total: %{type: :integer},
                  page: %{type: :integer},
                  per_page: %{type: :integer},
                  total_pages: %{type: :integer}
                }
              }
            }
          }
        }
      }
    }
  end

  def validate_params(params) do
    validated = %{
      "status" => validate_status(Map.get(params, "status")),
      "page" => parse_positive_integer(Map.get(params, "page", "1"), 1),
      "per_page" => parse_per_page(Map.get(params, "per_page", "20"))
    }

    {:ok, validated}
  rescue
    error -> {:error, {:invalid_parameters, error}}
  end

  def execute(params) do
    try do
      # Get all sandboxes from SandboxManager
      all_sandboxes = OTPSupervisor.Core.SandboxManager.list_sandboxes()

      # Filter by status if specified
      filtered_sandboxes =
        case params["status"] do
          nil -> all_sandboxes
          status -> filter_by_status(all_sandboxes, status)
        end

      # Apply pagination
      {paginated_data, meta} = paginate(filtered_sandboxes, params["page"], params["per_page"])

      {:ok, {paginated_data, meta}}
    rescue
      error -> {:error, {:sandbox_discovery_failed, error}}
    end
  end

  def format_response({sandboxes, meta}) do
    %{
      data: Enum.map(sandboxes, &format_sandbox_info/1),
      meta: meta
    }
  end

  defp validate_status(nil), do: nil
  defp validate_status(status) when status in ["running", "stopped"], do: status
  defp validate_status(_), do: raise("Invalid status filter")

  defp parse_positive_integer(value, default) when is_binary(value) do
    case Integer.parse(value) do
      {int, ""} when int > 0 -> int
      _ -> default
    end
  end

  defp parse_positive_integer(value, _default) when is_integer(value) and value > 0, do: value
  defp parse_positive_integer(_, default), do: default

  defp parse_per_page(value) do
    per_page = parse_positive_integer(value, 20)
    # Cap at 100
    min(per_page, 100)
  end

  defp filter_by_status(sandboxes, status) do
    Enum.filter(sandboxes, fn sandbox ->
      sandbox_status = get_sandbox_status(sandbox)
      sandbox_status == status
    end)
  end

  defp get_sandbox_status(sandbox) do
    if Process.alive?(sandbox.app_pid) do
      "running"
    else
      "stopped"
    end
  end

  defp paginate(items, page, per_page) do
    total = length(items)
    total_pages = ceil(total / per_page)
    offset = (page - 1) * per_page

    paginated_items =
      items
      |> Enum.drop(offset)
      |> Enum.take(per_page)

    meta = %{
      total: total,
      page: page,
      per_page: per_page,
      total_pages: total_pages
    }

    {paginated_items, meta}
  end

  defp format_sandbox_info(sandbox) do
    %{
      id: sandbox.id,
      app_name: sandbox.app_name,
      supervisor_module: format_module_name(sandbox.supervisor_module),
      app_pid: inspect(sandbox.app_pid),
      supervisor_pid: inspect(sandbox.supervisor_pid),
      status: get_sandbox_status(sandbox),
      created_at: sandbox.created_at,
      restart_count: sandbox.restart_count,
      opts: sandbox.opts
    }
  end

  defp format_module_name(module) when is_atom(module), do: Atom.to_string(module)
  defp format_module_name(module), do: inspect(module)
end
