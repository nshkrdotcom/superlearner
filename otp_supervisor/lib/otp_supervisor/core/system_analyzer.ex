defmodule OtpSupervisor.Core.SystemAnalyzer do
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
    # Placeholder implementation
    []
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

  defp get_system_info do
    %{
      total_memory: :erlang.memory(:total),
      process_count: length(Process.list()),
      port_count: length(Port.list()),
      atom_count: :erlang.system_info(:atom_count)
    }
  end
end
