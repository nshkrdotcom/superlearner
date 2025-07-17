defmodule OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterSupervisionTrees do
  @moduledoc """
  Arsenal operation to get supervision trees across all cluster nodes.

  This operation provides comprehensive supervision tree information from all nodes
  in the cluster, including supervisor hierarchies, children relationships, and
  metadata for cluster-wide supervision tree analysis and visualization.
  """

  use OTPSupervisor.Core.Arsenal.Operation

  alias OTPSupervisor.Core.Control
  alias OTPSupervisor.Distributed.ClusterStateManager

  def rest_config do
    %{
      method: :get,
      path: "/api/v1/cluster/supervision-trees",
      summary: "Get supervision trees across all cluster nodes",
      description:
        "Retrieves supervision tree hierarchies from all nodes in the cluster with detailed supervisor and children information",
      parameters: [
        %{
          name: :include_children,
          type: :boolean,
          required: false,
          default: true,
          description: "Include detailed children information for each supervisor",
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
          name: :include_process_details,
          type: :boolean,
          required: false,
          default: false,
          description: "Include detailed process information for children",
          location: :query
        },
        %{
          name: :max_depth,
          type: :integer,
          required: false,
          description: "Maximum tree depth to traverse (default: no limit)",
          location: :query
        }
      ],
      responses: %{
        200 => %{
          description: "Supervision trees retrieved successfully",
          schema: %{
            type: :object,
            properties: %{
              supervision_trees: %{
                type: :object,
                description: "Supervision trees grouped by node"
              },
              summary: %{
                type: :object,
                properties: %{
                  nodes_queried: %{type: :array, items: %{type: :string}},
                  total_supervisors: %{type: :integer},
                  total_nodes: %{type: :integer},
                  errors: %{type: :array}
                }
              },
              metadata: %{
                type: :object,
                properties: %{
                  timestamp: %{type: :string, format: "date-time"},
                  cluster_health: %{type: :string},
                  operation_duration_ms: %{type: :integer}
                }
              }
            }
          }
        },
        500 => %{description: "Failed to retrieve supervision trees"}
      }
    }
  end

  def validate_params(params) do
    validated_params =
      params
      |> convert_boolean_param("include_children", true)
      |> convert_boolean_param("include_process_details", false)
      |> validate_max_depth()

    {:ok, validated_params}
  end

  def execute(params) do
    start_time = System.monotonic_time(:millisecond)

    try do
      # Get all visible cluster nodes
      cluster_nodes = get_cluster_nodes()

      # Query supervision trees from all nodes
      {supervision_trees, errors} = query_all_nodes_supervision_trees(cluster_nodes, params)

      # Calculate summary statistics
      summary = build_summary(supervision_trees, cluster_nodes, errors)

      # Build metadata
      operation_duration = System.monotonic_time(:millisecond) - start_time
      metadata = build_metadata(operation_duration)

      {:ok,
       %{
         supervision_trees: supervision_trees,
         summary: summary,
         metadata: metadata
       }}
    rescue
      error ->
        {:error, "Failed to retrieve cluster supervision trees: #{inspect(error)}"}
    end
  end

  # Private helper functions

  defp convert_boolean_param(params, key, default_value) do
    case Map.get(params, key) do
      nil -> Map.put(params, key, default_value)
      "true" -> Map.put(params, key, true)
      "false" -> Map.put(params, key, false)
      value when is_boolean(value) -> params
      _ -> Map.put(params, key, default_value)
    end
  end

  defp validate_max_depth(params) do
    case Map.get(params, "max_depth") do
      nil ->
        params

      value when is_binary(value) ->
        case Integer.parse(value) do
          {depth, ""} when depth > 0 -> Map.put(params, "max_depth", depth)
          _ -> Map.delete(params, "max_depth")
        end

      value when is_integer(value) and value > 0 ->
        params

      _ ->
        Map.delete(params, "max_depth")
    end
  end

  defp get_cluster_nodes do
    try do
      # Get all visible nodes including self
      visible_nodes = Node.list([:visible])
      all_nodes = [Node.self() | visible_nodes]
      Enum.uniq(all_nodes)
    rescue
      _ -> [Node.self()]
    end
  end

  defp query_all_nodes_supervision_trees(nodes, params) do
    # Query supervision trees from all nodes in parallel
    tasks =
      nodes
      |> Enum.map(fn node ->
        Task.async(fn ->
          query_node_supervision_trees(node, params)
        end)
      end)

    # Collect results with timeout
    results =
      tasks
      |> Enum.map(fn task ->
        try do
          # 10 second timeout per node
          Task.await(task, 10_000)
        catch
          :exit, {:timeout, _} -> {:error, :timeout}
        end
      end)

    # Separate successful results from errors
    {supervision_trees, errors} =
      nodes
      |> Enum.zip(results)
      |> Enum.reduce({%{}, []}, fn {node, result}, {trees_acc, errors_acc} ->
        case result do
          {:ok, node_trees} ->
            {Map.put(trees_acc, node, node_trees), errors_acc}

          {:error, reason} ->
            error = %{node: node, reason: reason}
            {trees_acc, [error | errors_acc]}
        end
      end)

    {supervision_trees, Enum.reverse(errors)}
  end

  defp query_node_supervision_trees(node, params) when node == node() do
    # Query local node directly
    get_local_supervision_trees(params)
  end

  defp query_node_supervision_trees(node, params) do
    # Query remote node via RPC
    try do
      case :rpc.call(node, __MODULE__, :get_local_supervision_trees, [params], 8_000) do
        {:badrpc, reason} -> {:error, {:rpc_failed, reason}}
        result -> result
      end
    catch
      :exit, reason -> {:error, {:rpc_exit, reason}}
    end
  end

  def get_local_supervision_trees(params) do
    try do
      # Get all supervisors on this node
      supervisors = Control.list_supervisors()

      # Enrich supervisor data with application and strategy
      enriched_supervisors = Enum.map(supervisors, &enrich_supervisor_info/1)

      # Filter by application if specified
      filtered_supervisors = filter_supervisors_by_application(enriched_supervisors, params)

      # Build supervision trees for each supervisor
      supervision_trees =
        filtered_supervisors
        |> Enum.map(fn supervisor ->
          build_supervision_tree(supervisor, params)
        end)
        |> Enum.filter(&(&1 != nil))

      {:ok,
       %{
         supervisors: supervision_trees,
         node: Node.self(),
         total_supervisors: length(supervision_trees),
         applications: get_unique_applications(supervision_trees)
       }}
    rescue
      error -> {:error, {:local_query_failed, error}}
    end
  end

  defp enrich_supervisor_info(supervisor) do
    # Get application from supervisor name
    application = guess_application_from_name(supervisor.name)

    # Try to get strategy from supervisor state
    strategy = get_supervisor_strategy(supervisor)

    supervisor
    |> Map.put(:application, application)
    |> Map.put(:strategy, strategy)
  end

  defp guess_application_from_name(name) when is_atom(name) do
    name_str = Atom.to_string(name)

    cond do
      String.contains?(name_str, "OTPSupervisor") -> :otp_supervisor
      String.contains?(name_str, "Phoenix") -> :phoenix
      String.contains?(name_str, "Elixir") -> :elixir
      String.contains?(name_str, "Sandbox") -> :otp_sandbox
      true -> :unknown
    end
  end

  defp get_supervisor_strategy(supervisor) do
    case supervisor.pid do
      "nil" ->
        :unknown

      pid_str ->
        # Try to convert string PID back to actual PID
        try do
          case Control.to_pid(pid_str) do
            {:ok, pid} when is_pid(pid) ->
              case :sys.get_state(pid, 100) do
                state when is_map(state) ->
                  Map.get(state, :strategy, :one_for_one)

                _ ->
                  :one_for_one
              end

            _ ->
              :one_for_one
          end
        rescue
          _ -> :one_for_one
        end
    end
  end

  defp filter_supervisors_by_application(supervisors, params) do
    case Map.get(params, "filter_application") do
      nil ->
        supervisors

      "" ->
        supervisors

      app_filter when is_binary(app_filter) ->
        app_atom = String.to_atom(app_filter)

        Enum.filter(supervisors, fn supervisor ->
          supervisor.application == app_atom or
            supervisor.application == app_filter
        end)

      _ ->
        supervisors
    end
  end

  defp build_supervision_tree(supervisor, params) do
    try do
      include_children = Map.get(params, "include_children", true)
      include_process_details = Map.get(params, "include_process_details", false)
      max_depth = Map.get(params, "max_depth")

      base_supervisor = %{
        name: supervisor.name,
        pid: supervisor.pid,
        application: supervisor.application,
        strategy: supervisor.strategy,
        alive: supervisor.alive,
        child_count: supervisor.child_count,
        node: Node.self()
      }

      if include_children do
        children = get_supervisor_children(supervisor.pid, include_process_details, max_depth, 0)
        Map.put(base_supervisor, :children, children)
      else
        base_supervisor
      end
    rescue
      _ -> nil
    end
  end

  defp get_supervisor_children(
         _supervisor_pid,
         _include_process_details,
         max_depth,
         current_depth
       )
       when is_integer(max_depth) and current_depth >= max_depth do
    []
  end

  defp get_supervisor_children(supervisor_pid, include_process_details, max_depth, current_depth) do
    try do
      case Control.get_supervision_tree(supervisor_pid) do
        {:ok, tree} ->
          tree.children
          |> Enum.map(fn child ->
            base_child = %{
              id: child.id,
              pid: child.pid,
              type: child.type,
              restart: child.restart,
              shutdown: child.shutdown,
              alive: child.alive
            }

            enhanced_child =
              if include_process_details do
                add_process_details(base_child)
              else
                base_child
              end

            # Recursively get children if this child is a supervisor
            if child.type == :supervisor and child.alive do
              children =
                get_supervisor_children(
                  child.pid,
                  include_process_details,
                  max_depth,
                  current_depth + 1
                )

              Map.put(enhanced_child, :children, children)
            else
              enhanced_child
            end
          end)

        {:error, _} ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp add_process_details(child) do
    try do
      if Process.alive?(child.pid) do
        info = Process.info(child.pid, [:memory, :message_queue_len, :registered_name])

        child
        |> Map.put(:memory, info[:memory] || 0)
        |> Map.put(:message_queue_len, info[:message_queue_len] || 0)
        |> Map.put(:registered_name, info[:registered_name])
      else
        child
      end
    rescue
      _ -> child
    end
  end

  defp get_unique_applications(supervision_trees) do
    supervision_trees
    |> Enum.map(& &1.application)
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp build_summary(supervision_trees, cluster_nodes, errors) do
    total_supervisors =
      supervision_trees
      |> Enum.reduce(0, fn {_node, node_data}, acc ->
        acc + node_data.total_supervisors
      end)

    %{
      nodes_queried: cluster_nodes,
      total_nodes: length(cluster_nodes),
      successful_nodes: map_size(supervision_trees),
      failed_nodes: length(errors),
      total_supervisors: total_supervisors,
      errors: errors
    }
  end

  defp build_metadata(operation_duration) do
    cluster_health = get_cluster_health_status()

    %{
      timestamp: DateTime.utc_now(),
      cluster_health: cluster_health,
      operation_duration_ms: operation_duration
    }
  end

  defp get_cluster_health_status do
    try do
      ClusterStateManager.get_partition_status()
    rescue
      _ -> :unknown
    catch
      :exit, _ -> :unknown
    end
  end
end
