defmodule OtpSupervisor.Core.SnapAlarmFilter do
  @moduledoc """
  Logger filter that removes disk alarm messages from snap mount points.
  """

  def filter(event, _opts) do
    case event do
      %{msg: {:string, msg}} ->
        msg_str = IO.iodata_to_binary(msg)

        if contains_snap_alarm?(msg_str) do
          :stop
        else
          event
        end

      %{msg: {:report, %{label: {:alarm_handler, _}, report: report}}} ->
        if snap_alarm_report?(report) do
          :stop
        else
          event
        end

      _ ->
        event
    end
  end

  defp contains_snap_alarm?(msg) do
    String.contains?(msg, ":alarm_handler:") and String.contains?(msg, "/snap/")
  end

  defp snap_alarm_report?(report) do
    case report do
      {:set, {{:disk_almost_full, path}, _}} when is_list(path) ->
        String.starts_with?(List.to_string(path), "/snap/")

      {:clear, {:disk_almost_full, path}} when is_list(path) ->
        String.starts_with?(List.to_string(path), "/snap/")

      _ ->
        false
    end
  end
end
