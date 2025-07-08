defmodule OtpSupervisorWeb.Api.V1.SupervisorController do
  use OtpSupervisorWeb, :controller

  alias OTPSupervisor.Core.Control

  def index(conn, _params) do
    supervisors = Control.list_supervisors()

    formatted_supervisors =
      supervisors
      |> Enum.map(&format_supervisor_for_json/1)

    meta = %{
      total: length(supervisors),
      timestamp: System.system_time(:millisecond)
    }

    conn
    |> put_status(200)
    |> json(%{
      data: formatted_supervisors,
      meta: meta
    })
  end

  def show(conn, %{"name" => supervisor_name}) do
    supervisor_atom = String.to_existing_atom(supervisor_name)

    case Control.get_supervision_tree(supervisor_atom) do
      {:ok, children} ->
        supervisor_info = %{
          "name" => supervisor_name,
          "pid" => get_supervisor_pid_string(supervisor_atom),
          "alive" => supervisor_alive?(supervisor_atom),
          "child_count" => length(children),
          "children" => format_children_for_json(children),
          "strategy" => get_supervisor_strategy(supervisor_atom)
        }

        conn
        |> put_status(200)
        |> json(%{data: supervisor_info})

      {:error, :not_found} ->
        conn
        |> put_status(404)
        |> json(%{
          error: %{
            message: "Supervisor not found",
            code: "supervisor_not_found"
          }
        })

      {:error, reason} ->
        conn
        |> put_status(422)
        |> json(%{
          error: %{
            message: "Failed to get supervisor information",
            code: "supervisor_error",
            reason: inspect(reason)
          }
        })
    end
  rescue
    ArgumentError ->
      conn
      |> put_status(404)
      |> json(%{
        error: %{
          message: "Supervisor not found",
          code: "supervisor_not_found"
        }
      })
  end

  def analytics(conn, %{"name" => supervisor_name}) do
    supervisor_atom = String.to_existing_atom(supervisor_name)

    analytics_data = %{
      "performance_metrics" => get_supervisor_performance_metrics(supervisor_atom)
    }

    conn
    |> put_status(200)
    |> json(%{data: analytics_data})
  rescue
    ArgumentError ->
      conn
      |> put_status(404)
      |> json(%{
        error: %{
          message: "Supervisor not found",
          code: "supervisor_not_found"
        }
      })
  end

  def change_strategy(conn, %{"name" => supervisor_name} = params) do
    _supervisor_atom = String.to_existing_atom(supervisor_name)
    new_strategy = params["strategy"]

    # Note: In a real implementation, changing supervisor strategy would require
    # more complex logic and potentially restarting the supervisor
    case validate_strategy(new_strategy) do
      :ok ->
        conn
        |> put_status(200)
        |> json(%{
          data: %{
            status: "strategy_changed",
            new_strategy: new_strategy,
            supervisor: supervisor_name
          }
        })

      {:error, reason} ->
        conn
        |> put_status(400)
        |> json(%{
          error: %{
            message: "Invalid strategy",
            code: "invalid_strategy",
            reason: reason
          }
        })
    end
  rescue
    ArgumentError ->
      conn
      |> put_status(404)
      |> json(%{
        error: %{
          message: "Supervisor not found",
          code: "supervisor_not_found"
        }
      })
  end

  def simulate_failure(conn, %{"name" => supervisor_name} = params) do
    supervisor_atom = String.to_existing_atom(supervisor_name)
    failure_type = params["failure_type"]
    target = params["target"]
    reason = params["reason"] || "simulated_failure"

    case simulate_supervisor_failure(supervisor_atom, failure_type, target, reason) do
      :ok ->
        conn
        |> put_status(200)
        |> json(%{
          data: %{
            status: "failure_simulated",
            failure_type: failure_type,
            supervisor: supervisor_name
          }
        })

      {:error, reason} ->
        conn
        |> put_status(422)
        |> json(%{
          error: %{
            message: "Failed to simulate failure",
            code: "simulation_failed",
            reason: inspect(reason)
          }
        })
    end
  rescue
    ArgumentError ->
      conn
      |> put_status(404)
      |> json(%{
        error: %{
          message: "Supervisor not found",
          code: "supervisor_not_found"
        }
      })
  end

  # Private helper functions

  defp format_supervisor_for_json(supervisor) do
    %{
      "name" => supervisor.name,
      "pid" => supervisor.pid,
      "alive" => supervisor.alive,
      "child_count" => supervisor.child_count
    }
  end

  defp format_children_for_json(children) do
    Enum.map(children, fn child ->
      %{
        "id" => child.id,
        "pid" => child.pid,
        "type" => child.type,
        "modules" => child.modules,
        "alive" => child.alive,
        "info" => child.info
      }
    end)
  end

  defp get_supervisor_pid_string(supervisor_atom) do
    case Process.whereis(supervisor_atom) do
      nil -> "nil"
      pid -> inspect(pid)
    end
  end

  defp supervisor_alive?(supervisor_atom) do
    case Process.whereis(supervisor_atom) do
      nil -> false
      pid -> Process.alive?(pid)
    end
  end

  defp get_supervisor_strategy(_supervisor_atom) do
    # In a real implementation, this would inspect the supervisor's state
    # For now, return a default strategy
    "one_for_one"
  end

  defp get_supervisor_performance_metrics(supervisor_atom) do
    case Process.whereis(supervisor_atom) do
      nil ->
        %{
          "memory_usage" => 0,
          "message_queue_len" => 0,
          "reductions" => 0
        }

      pid ->
        case Process.info(pid, [:memory, :message_queue_len, :reductions]) do
          info when is_list(info) ->
            info
            |> Map.new()
            |> Enum.map(fn {k, v} -> {Atom.to_string(k), v} end)
            |> Map.new()

          _ ->
            %{
              "memory_usage" => 0,
              "message_queue_len" => 0,
              "reductions" => 0
            }
        end
    end
  end

  defp validate_strategy(strategy)
       when strategy in ["one_for_one", "one_for_all", "rest_for_one", "simple_one_for_one"] do
    :ok
  end

  defp validate_strategy(strategy) do
    {:error, "Unknown strategy: #{strategy}"}
  end

  defp simulate_supervisor_failure(supervisor_atom, failure_type, target, reason) do
    case failure_type do
      "child_crash" ->
        simulate_child_crash(supervisor_atom, target, reason)

      "supervisor_crash" ->
        simulate_supervisor_crash(supervisor_atom, reason)

      _ ->
        {:error, "Unknown failure type: #{failure_type}"}
    end
  end

  defp simulate_child_crash(supervisor_atom, target, reason) do
    case Control.get_supervision_tree(supervisor_atom) do
      {:ok, children} ->
        child_to_crash =
          case target do
            "random" -> Enum.random(children)
            child_id -> Enum.find(children, &(&1.id == child_id))
          end

        if child_to_crash && child_to_crash.alive do
          case Control.to_pid(child_to_crash.pid) do
            {:ok, pid} ->
              Control.simulate_crash(pid, String.to_atom(reason))
              :ok

            error ->
              error
          end
        else
          {:error, "No suitable child found to crash"}
        end

      error ->
        error
    end
  end

  defp simulate_supervisor_crash(supervisor_atom, reason) do
    case Process.whereis(supervisor_atom) do
      nil ->
        {:error, "Supervisor not found"}

      pid ->
        Control.simulate_crash(pid, String.to_atom(reason))
        :ok
    end
  end
end
