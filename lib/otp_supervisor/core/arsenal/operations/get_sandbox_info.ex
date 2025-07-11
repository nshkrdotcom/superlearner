defmodule OTPSupervisor.Core.Arsenal.Operations.GetSandboxInfo do
  @moduledoc """
  Operation to get detailed information about a specific sandbox.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :get,
      path: "/api/v1/sandboxes/:sandbox_id",
      summary: "Get detailed information about a sandbox",
      parameters: [
        %{
          name: :sandbox_id,
          type: :string,
          required: true,
          description: "Unique identifier of the sandbox",
          location: :path
        },
        %{
          name: :include_children,
          type: :boolean,
          required: false,
          description: "Include supervisor children information",
          location: :query
        },
        %{
          name: :include_stats,
          type: :boolean,
          required: false,
          description: "Include performance and health statistics",
          location: :query
        }
      ],
      responses: %{
        200 => %{
          description: "Sandbox information retrieved successfully",
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
                  restart_count: %{type: :integer},
                  uptime_seconds: %{type: :integer},
                  children: %{type: :array},
                  memory_usage: %{type: :object},
                  health_stats: %{type: :object}
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

  def validate_params(%{"sandbox_id" => sandbox_id} = params) do
    validated_params = %{
      "sandbox_id" => validate_sandbox_id(sandbox_id),
      "include_children" => parse_boolean(Map.get(params, "include_children", false)),
      "include_stats" => parse_boolean(Map.get(params, "include_stats", false))
    }

    {:ok, validated_params}
  rescue
    error -> {:error, {:invalid_parameters, error}}
  end

  def validate_params(_params) do
    {:error, {:missing_parameter, "sandbox_id is required"}}
  end

  def execute(%{
        "sandbox_id" => sandbox_id,
        "include_children" => include_children,
        "include_stats" => include_stats
      }) do
    case OTPSupervisor.Core.SandboxManager.get_sandbox_info(sandbox_id) do
      {:ok, sandbox_info} ->
        enriched_info = enrich_sandbox_info(sandbox_info, include_children, include_stats)
        {:ok, enriched_info}

      {:error, :not_found} ->
        {:error, :sandbox_not_found}

      {:error, reason} ->
        {:error, {:info_retrieval_failed, reason}}
    end
  end

  def format_response(sandbox_info) do
    %{
      data: sandbox_info
    }
  end

  defp validate_sandbox_id(sandbox_id) when is_binary(sandbox_id) and byte_size(sandbox_id) > 0 do
    sandbox_id
  end

  defp validate_sandbox_id(_), do: raise("sandbox_id must be a non-empty string")

  defp parse_boolean(true), do: true
  defp parse_boolean(false), do: false
  defp parse_boolean("true"), do: true
  defp parse_boolean("false"), do: false
  defp parse_boolean(_), do: false

  defp enrich_sandbox_info(sandbox_info, include_children, include_stats) do
    base_info = %{
      id: sandbox_info.id,
      app_name: sandbox_info.app_name,
      supervisor_module: format_module_name(sandbox_info.supervisor_module),
      app_pid: inspect(sandbox_info.app_pid),
      supervisor_pid: inspect(sandbox_info.supervisor_pid),
      status: get_sandbox_status(sandbox_info),
      created_at: sandbox_info.created_at,
      restart_count: sandbox_info.restart_count,
      uptime_seconds: calculate_uptime(sandbox_info.created_at),
      configuration: format_sandbox_config(sandbox_info.opts)
    }

    base_info
    |> maybe_add_children(sandbox_info, include_children)
    |> maybe_add_stats(sandbox_info, include_stats)
  end

  defp get_sandbox_status(sandbox_info) do
    if Process.alive?(sandbox_info.app_pid) do
      "running"
    else
      "stopped"
    end
  end

  defp calculate_uptime(created_at) do
    current_time = System.system_time(:millisecond)
    div(current_time - created_at, 1000)
  end

  defp maybe_add_children(info, sandbox_info, include_children) when include_children in [true, "true"] do
    children = get_supervisor_children(sandbox_info.supervisor_pid)
    Map.put(info, :children, children)
  end

  defp maybe_add_children(info, _sandbox_info, _), do: info

  defp maybe_add_stats(info, sandbox_info, include_stats) when include_stats in [true, "true"] do
    memory_usage = get_memory_usage(sandbox_info)
    health_stats = get_health_stats(sandbox_info)

    info
    |> Map.put(:memory_usage, memory_usage)
    |> Map.put(:health_stats, health_stats)
  end

  defp maybe_add_stats(info, _sandbox_info, _), do: info

  defp get_supervisor_children(supervisor_pid) do
    try do
      Supervisor.which_children(supervisor_pid)
      |> Enum.map(fn {id, child_pid, type, modules} ->
        %{
          id: id,
          pid: if(is_pid(child_pid), do: inspect(child_pid), else: child_pid),
          type: type,
          modules: modules,
          alive: is_pid(child_pid) and Process.alive?(child_pid)
        }
      end)
    rescue
      _ -> []
    end
  end

  defp get_memory_usage(sandbox_info) do
    try do
      app_memory = get_process_memory(sandbox_info.app_pid)
      supervisor_memory = get_process_memory(sandbox_info.supervisor_pid)

      # Get memory for all children
      children_memory =
        try do
          Supervisor.which_children(sandbox_info.supervisor_pid)
          |> Enum.reduce(0, fn {_id, child_pid, _type, _modules}, acc ->
            if is_pid(child_pid) do
              acc + get_process_memory(child_pid)
            else
              acc
            end
          end)
        rescue
          _ -> 0
        end

      %{
        app_memory_bytes: app_memory,
        supervisor_memory_bytes: supervisor_memory,
        children_memory_bytes: children_memory,
        total_memory_bytes: app_memory + supervisor_memory + children_memory
      }
    rescue
      _ ->
        %{
          app_memory_bytes: 0,
          supervisor_memory_bytes: 0,
          children_memory_bytes: 0,
          total_memory_bytes: 0
        }
    end
  end

  defp get_process_memory(pid) when is_pid(pid) do
    case Process.info(pid, :memory) do
      {:memory, memory} -> memory
      _ -> 0
    end
  rescue
    _ -> 0
  end

  defp get_health_stats(sandbox_info) do
    try do
      # Get process counts
      children = Supervisor.which_children(sandbox_info.supervisor_pid)

      alive_children =
        Enum.count(children, fn {_id, child_pid, _type, _modules} ->
          is_pid(child_pid) and Process.alive?(child_pid)
        end)

      # Get supervisor strategy
      strategy = get_supervisor_strategy(sandbox_info.supervisor_pid)

      %{
        total_children: length(children),
        alive_children: alive_children,
        dead_children: length(children) - alive_children,
        supervisor_strategy: strategy,
        app_alive: Process.alive?(sandbox_info.app_pid),
        supervisor_alive: Process.alive?(sandbox_info.supervisor_pid)
      }
    rescue
      _ ->
        %{
          total_children: 0,
          alive_children: 0,
          dead_children: 0,
          supervisor_strategy: :unknown,
          app_alive: false,
          supervisor_alive: false
        }
    end
  end

  defp get_supervisor_strategy(supervisor_pid) do
    try do
      case :sys.get_state(supervisor_pid) do
        %{strategy: strategy} -> strategy
        _ -> :unknown
      end
    rescue
      _ -> :unknown
    end
  end

  defp format_module_name(module) when is_atom(module), do: Atom.to_string(module)
  defp format_module_name(module), do: inspect(module)

  defp format_sandbox_config(opts) when is_list(opts) do
    opts
    |> Enum.into(%{})
    |> Enum.map(fn
      # Convert compile_info to a JSON-serializable format
      {:compile_info, compile_info} ->
        {"compile_info", format_compile_info(compile_info)}
      
      # Convert atom keys to strings
      {key, value} when is_atom(key) ->
        {Atom.to_string(key), format_config_value(value)}
      
      # Keep string keys as-is
      {key, value} ->
        {key, format_config_value(value)}
    end)
    |> Enum.into(%{})
  end

  defp format_compile_info(compile_info) when is_map(compile_info) do
    %{
      "compilation_time_ms" => compile_info.compilation_time,
      "beam_files_count" => length(compile_info.beam_files),
      "output_summary" => String.slice(compile_info.output, 0, 100),
      "temp_dir" => compile_info.temp_dir
    }
  end

  defp format_config_value(value) when is_atom(value), do: Atom.to_string(value)
  defp format_config_value(value), do: value
end
