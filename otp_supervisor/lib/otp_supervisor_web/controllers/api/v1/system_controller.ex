defmodule OtpSupervisorWeb.Api.V1.SystemController do
  use OtpSupervisorWeb, :controller

  alias OTPSupervisor.Core.Control

  def health(conn, _params) do
    metrics = get_system_metrics()

    status = if metrics.supervision_health > 80.0, do: "healthy", else: "degraded"

    conn
    |> put_status(200)
    |> json(%{
      data: %{
        status: status,
        metrics: %{
          total_processes: metrics.total_processes,
          memory_usage: metrics.memory_usage,
          message_queue_lengths: metrics.message_queue_lengths,
          supervision_health: metrics.supervision_health
        },
        timestamp: System.system_time(:millisecond)
      }
    })
  end

  def graph(conn, _params) do
    graph = Control.build_process_graph()

    formatted_graph = %{
      processes: Enum.map(graph.processes, &format_process_for_graph/1),
      links: graph.links,
      monitors: graph.monitors
    }

    conn
    |> put_status(200)
    |> json(%{data: formatted_graph})
  end

  def bottlenecks(conn, _params) do
    bottlenecks = identify_system_bottlenecks()
    analysis = analyze_bottlenecks(bottlenecks)

    conn
    |> put_status(200)
    |> json(%{
      data: %{
        bottlenecks: bottlenecks,
        analysis: analysis
      }
    })
  end

  def anomalies(conn, _params) do
    anomalies = detect_system_anomalies()
    summary = summarize_anomalies(anomalies)

    conn
    |> put_status(200)
    |> json(%{
      data: %{
        anomalies: anomalies,
        summary: summary
      }
    })
  end

  # Private helper functions

  defp get_system_metrics do
    %{
      total_processes: length(Process.list()),
      memory_usage: :erlang.memory(:total),
      message_queue_lengths: get_total_queue_lengths(),
      supervision_health: calculate_supervision_health()
    }
  end

  defp get_total_queue_lengths do
    Process.list()
    |> Enum.map(fn pid ->
      case Process.info(pid, :message_queue_len) do
        {:message_queue_len, len} -> len
        _ -> 0
      end
    end)
    |> Enum.sum()
  end

  defp calculate_supervision_health do
    supervisors = Control.list_supervisors()

    if length(supervisors) > 0 do
      healthy_count = Enum.count(supervisors, &supervisor_healthy?/1)
      healthy_count / length(supervisors) * 100.0
    else
      100.0
    end
  end

  defp supervisor_healthy?(supervisor) do
    case Control.get_supervision_tree(supervisor.name) do
      {:ok, _children} -> true
      _ -> false
    end
  end

  defp format_process_for_graph(process) do
    %{
      "pid" => process.pid,
      "name" => process.name,
      "type" => Atom.to_string(process.type)
    }
  end

  defp identify_system_bottlenecks do
    processes = Control.list_all_processes()

    # Identify processes with high memory usage
    high_memory_processes =
      processes
      |> Enum.filter(fn process ->
        case Control.to_pid(process.pid) do
          {:ok, pid} ->
            case Process.info(pid, :memory) do
              # 10MB threshold
              {:memory, memory} -> memory > 10_000_000
              _ -> false
            end

          _ ->
            false
        end
      end)
      |> Enum.map(fn process ->
        %{
          type: "high_memory",
          pid: process.pid,
          name: process.name,
          details: get_process_memory_details(process.pid)
        }
      end)

    # Identify processes with large message queues
    large_queue_processes =
      processes
      |> Enum.filter(fn process ->
        case Control.to_pid(process.pid) do
          {:ok, pid} ->
            case Process.info(pid, :message_queue_len) do
              {:message_queue_len, len} -> len > 100
              _ -> false
            end

          _ ->
            false
        end
      end)
      |> Enum.map(fn process ->
        %{
          type: "large_message_queue",
          pid: process.pid,
          name: process.name,
          details: get_process_queue_details(process.pid)
        }
      end)

    high_memory_processes ++ large_queue_processes
  end

  defp analyze_bottlenecks(bottlenecks) do
    %{
      total_bottlenecks: length(bottlenecks),
      high_memory_count: Enum.count(bottlenecks, &(&1.type == "high_memory")),
      large_queue_count: Enum.count(bottlenecks, &(&1.type == "large_message_queue")),
      severity: determine_severity(bottlenecks)
    }
  end

  defp determine_severity(bottlenecks) do
    count = length(bottlenecks)

    cond do
      count > 10 -> "critical"
      count > 5 -> "high"
      count > 0 -> "medium"
      true -> "low"
    end
  end

  defp detect_system_anomalies do
    processes = Control.list_all_processes()

    # Detect unsupervised processes
    unsupervised = detect_unsupervised_processes(processes)

    # Detect memory growth patterns
    memory_growth = detect_memory_growth_patterns()

    # Detect growing message queues
    growing_queues = detect_growing_queues(processes)

    unsupervised ++ memory_growth ++ growing_queues
  end

  defp detect_unsupervised_processes(processes) do
    processes
    |> Enum.filter(fn process ->
      case Control.to_pid(process.pid) do
        {:ok, pid} ->
          case Process.info(pid, :links) do
            {:links, links} -> length(links) == 0
            _ -> false
          end

        _ ->
          false
      end
    end)
    |> Enum.map(fn process ->
      %{
        type: "unsupervised_process",
        pid: process.pid,
        name: process.name,
        description: "Process has no supervision links"
      }
    end)
  end

  defp detect_memory_growth_patterns do
    # Simplified memory growth detection
    # In a real implementation, this would track memory over time
    []
  end

  defp detect_growing_queues(processes) do
    processes
    |> Enum.filter(fn process ->
      case Control.to_pid(process.pid) do
        {:ok, pid} ->
          case Process.info(pid, :message_queue_len) do
            {:message_queue_len, len} -> len > 50
            _ -> false
          end

        _ ->
          false
      end
    end)
    |> Enum.map(fn process ->
      %{
        type: "growing_message_queue",
        pid: process.pid,
        name: process.name,
        description: "Process has a growing message queue"
      }
    end)
  end

  defp summarize_anomalies(anomalies) do
    %{
      total_anomalies: length(anomalies),
      unsupervised_processes: Enum.count(anomalies, &(&1.type == "unsupervised_process")),
      memory_growth_issues: Enum.count(anomalies, &(&1.type == "memory_growth")),
      growing_queues: Enum.count(anomalies, &(&1.type == "growing_message_queue")),
      risk_level: determine_anomaly_risk_level(anomalies)
    }
  end

  defp determine_anomaly_risk_level(anomalies) do
    count = length(anomalies)

    cond do
      count > 20 -> "critical"
      count > 10 -> "high"
      count > 5 -> "medium"
      count > 0 -> "low"
      true -> "none"
    end
  end

  defp get_process_memory_details(pid_string) do
    case Control.to_pid(pid_string) do
      {:ok, pid} ->
        case Process.info(pid, [:memory, :heap_size, :stack_size]) do
          info when is_list(info) -> Map.new(info)
          _ -> %{}
        end

      _ ->
        %{}
    end
  end

  defp get_process_queue_details(pid_string) do
    case Control.to_pid(pid_string) do
      {:ok, pid} ->
        case Process.info(pid, [:message_queue_len, :status]) do
          info when is_list(info) -> Map.new(info)
          _ -> %{}
        end

      _ ->
        %{}
    end
  end
end
