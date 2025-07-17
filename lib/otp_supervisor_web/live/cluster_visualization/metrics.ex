defmodule OtpSupervisorWeb.Live.ClusterVisualization.Metrics do
  @moduledoc """
  Handles metrics and formatting for cluster visualization.
  """

  def status_bar_metrics(assigns) do
    [
      %{label: "Nodes", value: format_number(assigns.total_nodes)},
      %{label: "Processes", value: format_number(assigns.total_processes)},
      %{label: "Auto-refresh", value: if(assigns.auto_refresh, do: "ON", else: "OFF")},
      %{label: "Status", value: get_status_indicator(assigns)}
    ]
  end

  defp format_number(num) when is_integer(num) do
    num
    |> Integer.to_string()
    |> String.reverse()
    |> String.replace(~r/(\d{3})(?=\d)/, "\\1,")
    |> String.reverse()
  end

  defp format_number(_), do: "0"

  defp get_status_indicator(assigns) do
    cond do
      assigns.loading -> "⟳ Loading"
      assigns.error_message -> "✗ Error"
      true -> "✓ Ready"
    end
  end
end