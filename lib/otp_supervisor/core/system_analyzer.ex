defmodule OTPSupervisor.Core.SystemAnalyzer do
  @moduledoc """
  Provides system-wide analysis capabilities for detecting anomalies and patterns.
  """

  alias OTPSupervisor.Core.Control

  @doc """
  Searches processes based on a query string.
  """
  def search_processes(query) when is_binary(query) do
    Control.list_all_processes()
    |> Enum.filter(fn process ->
      process_matches_query?(process, query)
    end)
  end

  @doc """
  Detects system anomalies such as memory growth, growing queues, etc.
  """
  def detect_anomalies do
    processes = Control.list_all_processes()

    # Only detect severe anomalies to avoid false positives
    # Detect processes with very large message queues (>1000 messages)
    large_queues = detect_severe_message_queue_anomalies(processes)

    # Detect processes with extremely high memory usage (>50MB)
    high_memory = detect_severe_memory_anomalies(processes)

    large_queues ++ high_memory
  end

  @doc """
  Generates a comprehensive system report.
  """
  def generate_system_report do
    %{
      timestamp: System.system_time(:millisecond),
      processes: Control.list_all_processes(),
      supervisors: Control.list_supervisors(),
      system_info: get_system_info()
    }
  end

  defp process_matches_query?(process, query) do
    query_lower = String.downcase(query)

    name_match =
      case process[:name] do
        nil ->
          false

        name when is_atom(name) ->
          String.contains?(String.downcase(Atom.to_string(name)), query_lower)

        name when is_binary(name) ->
          String.contains?(String.downcase(name), query_lower)

        _ ->
          false
      end

    type_match =
      case process[:type] do
        nil ->
          false

        type when is_atom(type) ->
          String.contains?(String.downcase(Atom.to_string(type)), query_lower)

        type when is_binary(type) ->
          String.contains?(String.downcase(type), query_lower)

        _ ->
          false
      end

    name_match || type_match
  end

  defp detect_severe_message_queue_anomalies(processes) do
    processes
    |> Enum.filter(fn process ->
      case Control.to_pid(process.pid) do
        {:ok, pid} ->
          case Process.info(pid, :message_queue_len) do
            # Very high threshold
            {:message_queue_len, len} -> len > 1000
            _ -> false
          end

        _ ->
          false
      end
    end)
    |> Enum.map(fn process ->
      %{
        type: "severe_message_queue_buildup",
        pid: process.pid,
        name: process.name,
        description: "Process has extremely large message queue (>1000 messages)"
      }
    end)
  end

  defp detect_severe_memory_anomalies(processes) do
    processes
    |> Enum.filter(fn process ->
      case Control.to_pid(process.pid) do
        {:ok, pid} ->
          case Process.info(pid, :memory) do
            # 50MB threshold
            {:memory, memory} -> memory > 50_000_000
            _ -> false
          end

        _ ->
          false
      end
    end)
    |> Enum.map(fn process ->
      %{
        type: "severe_memory_usage",
        pid: process.pid,
        name: process.name,
        description: "Process using extremely high memory (>50MB)"
      }
    end)
  end

  defp get_system_info do
    %{
      total_memory: :erlang.memory(:total),
      process_count: length(Process.list()),
      port_count: length(Port.list()),
      atom_count: :erlang.system_info(:atom_count)
    }
  end
end
