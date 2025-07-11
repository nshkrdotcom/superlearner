defmodule OTPSupervisor.Core.Arsenal.Operations.ListSupervisors do
  @moduledoc """
  Operation to list all supervisors in the system.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :get,
      path: "/api/v1/supervisors",
      summary: "List all supervisors in the system",
      parameters: [
        %{
          name: :include_children,
          type: :boolean,
          required: false,
          description: "Include children information for each supervisor",
          location: :query
        },
        %{
          name: :filter_application,
          type: :string,
          required: false,
          description: "Filter supervisors by application name",
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
          description: "Items per page (default: 50, max: 100)",
          location: :query
        }
      ],
      responses: %{
        200 => %{
          description: "Supervisors retrieved successfully",
          schema: %{
            type: :object,
            properties: %{
              data: %{
                type: :array,
                items: %{
                  type: :object,
                  properties: %{
                    name: %{type: :string},
                    pid: %{type: :string},
                    alive: %{type: :boolean},
                    child_count: %{type: :integer},
                    strategy: %{type: :string},
                    application: %{type: :string}
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
      "include_children" => parse_boolean(Map.get(params, "include_children", false)),
      "filter_application" => Map.get(params, "filter_application"),
      "page" => parse_positive_integer(Map.get(params, "page", "1"), 1),
      "per_page" => parse_per_page(Map.get(params, "per_page", "50"))
    }

    {:ok, validated}
  rescue
    error -> {:error, {:invalid_parameters, error}}
  end

  def execute(params) do
    try do
      supervisors = discover_supervisors()

      # Filter by application if specified
      filtered_supervisors =
        case params["filter_application"] do
          nil -> supervisors
          app_name -> filter_by_application(supervisors, app_name)
        end

      # Add children information if requested
      enriched_supervisors =
        case params["include_children"] do
          true -> add_children_info(filtered_supervisors)
          false -> filtered_supervisors
        end

      # Apply pagination
      {paginated_data, meta} = paginate(enriched_supervisors, params["page"], params["per_page"])

      {:ok, {paginated_data, meta}}
    rescue
      error -> {:error, {:discovery_failed, error}}
    end
  end

  def format_response({supervisors, meta}) do
    %{
      data: Enum.map(supervisors, &format_supervisor_info/1),
      meta: meta
    }
  end

  defp parse_positive_integer(value, default) when is_binary(value) do
    case Integer.parse(value) do
      {int, ""} when int > 0 -> int
      _ -> default
    end
  end

  defp parse_positive_integer(value, _default) when is_integer(value) and value > 0, do: value
  defp parse_positive_integer(_, default), do: default

  defp parse_boolean(true), do: true
  defp parse_boolean(false), do: false
  defp parse_boolean("true"), do: true
  defp parse_boolean("false"), do: false
  defp parse_boolean(_), do: false

  defp parse_per_page(value) do
    per_page = parse_positive_integer(value, 50)
    # Cap at 100
    min(per_page, 100)
  end

  defp discover_supervisors do
    # Use the existing working Control module instead of reimplementing
    OTPSupervisor.Core.Control.list_supervisors()
    |> Enum.map(&convert_control_format_to_arsenal_format/1)
  end

  defp convert_control_format_to_arsenal_format(supervisor) do
    %{
      name: supervisor.name,
      pid: supervisor.pid,
      alive: supervisor.alive,
      child_count: supervisor.child_count,
      application: get_process_application_from_name(supervisor.name)
    }
  end

  defp get_process_application_from_name(name) when is_atom(name) do
    case Process.whereis(name) do
      nil -> :system
      pid -> get_process_application_from_pid(pid)
    end
  end

  defp get_process_application_from_name(_), do: :system

  defp get_process_application_from_pid(pid) do
    case :application.get_application(pid) do
      {:ok, app} -> app
      :undefined -> :system
    end
  end

  defp filter_by_application(supervisors, app_name) do
    app_atom = String.to_existing_atom(app_name)

    Enum.filter(supervisors, fn supervisor ->
      supervisor.application == app_atom
    end)
  rescue
    # Invalid application name
    ArgumentError -> []
  end

  defp add_children_info(supervisors) do
    Enum.map(supervisors, fn supervisor ->
      children_info = get_supervisor_children(supervisor.pid)
      Map.merge(supervisor, children_info)
    end)
  end

  defp get_supervisor_children(pid) do
    try do
      children = Supervisor.which_children(pid)
      strategy = get_supervisor_strategy(pid)

      %{
        child_count: length(children),
        strategy: strategy,
        children: format_children(children)
      }
    rescue
      _ ->
        %{
          child_count: 0,
          strategy: :unknown,
          children: []
        }
    end
  end

  defp get_supervisor_strategy(pid) do
    try do
      case :sys.get_state(pid) do
        %{strategy: strategy} -> strategy
        _ -> :unknown
      end
    rescue
      _ -> :unknown
    end
  end

  defp format_children(children) do
    Enum.map(children, fn {id, child_pid, type, modules} ->
      %{
        id: id,
        pid: if(is_pid(child_pid), do: inspect(child_pid), else: child_pid),
        type: type,
        modules: modules
      }
    end)
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

  defp format_supervisor_info(supervisor) do
    %{
      name: format_name(supervisor.name),
      pid: inspect(supervisor.pid),
      alive: supervisor.alive,
      child_count: Map.get(supervisor, :child_count, 0),
      strategy: Map.get(supervisor, :strategy, :unknown),
      application: supervisor.application,
      children: Map.get(supervisor, :children, [])
    }
  end

  defp format_name(name) when is_atom(name), do: Atom.to_string(name)
  defp format_name(pid) when is_pid(pid), do: inspect(pid)
  defp format_name(name), do: inspect(name)
end
