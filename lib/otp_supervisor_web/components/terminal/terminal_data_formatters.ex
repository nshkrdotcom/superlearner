defmodule OtpSupervisorWeb.Components.Terminal.TerminalDataFormatters do
  use Phoenix.Component

  @moduledoc """
  Consistent data formatting functions for terminal-themed components.
  
  Provides standardized formatting for common data types across all pages.
  """

  @doc """
  Formats bytes into human-readable format (B, KB, MB, GB, TB).
  """
  def format_bytes(assigns) do
    ~H"""
    <span class="font-mono"><%= format_bytes_value(@value) %></span>
    """
  end

  @doc """
  Formats uptime from seconds into human-readable format.
  """
  def format_uptime(assigns) do
    ~H"""
    <span class="font-mono"><%= format_uptime_value(@value) %></span>
    """
  end

  @doc """
  Formats process status with appropriate color and icon.
  """
  def format_process_status(assigns) do
    ~H"""
    <span class={[
      "inline-flex items-center space-x-1 font-mono text-sm",
      status_color(@status)
    ]}>
      <span><%= status_icon(@status) %></span>
      <span><%= format_status_text(@status) %></span>
    </span>
    """
  end

  @doc """
  Formats numbers with thousands separators.
  """
  def format_number(assigns) do
    ~H"""
    <span class="font-mono"><%= format_number_value(@value) %></span>
    """
  end

  @doc """
  Formats percentage values.
  """
  def format_percentage(assigns) do
    ~H"""
    <span class={[
      "font-mono",
      percentage_color(@value, @threshold)
    ]}>
      <%= format_percentage_value(@value) %>%
    </span>
    """
  end

  @doc """
  Formats timestamp into HH:MM:SS format.
  """
  def format_timestamp(assigns) do
    ~H"""
    <span class="font-mono text-green-400/70"><%= format_timestamp_value(@value) %></span>
    """
  end

  @doc """
  Formats memory usage with color coding based on thresholds.
  """
  def format_memory_usage(assigns) do
    ~H"""
    <div class="flex items-center space-x-2">
      <span class={[
        "font-mono",
        memory_usage_color(@used, @total)
      ]}>
        <%= format_bytes_value(@used) %>
      </span>
      <span class="text-green-400/50 font-mono">/</span>
      <span class="text-green-400/70 font-mono"><%= format_bytes_value(@total) %></span>
      <span class={[
        "text-xs font-mono",
        memory_usage_color(@used, @total)
      ]}>
        (<%= format_percentage_value((@used / @total) * 100) %>%)
      </span>
    </div>
    """
  end

  @doc """
  Formats CPU usage with color coding.
  """
  def format_cpu_usage(assigns) do
    ~H"""
    <div class="flex items-center space-x-2">
      <span class={[
        "font-mono",
        cpu_usage_color(@value)
      ]}>
        <%= format_percentage_value(@value) %>%
      </span>
      <%= if @cores do %>
        <span class="text-green-400/50 font-mono text-xs">
          (<%= @cores %> cores)
        </span>
      <% end %>
    </div>
    """
  end

  @doc """
  Formats process count with appropriate styling.
  """
  def format_process_count(assigns) do
    ~H"""
    <div class="flex items-center space-x-2">
      <span class={[
        "font-mono",
        process_count_color(@count, @limit)
      ]}>
        <%= format_number_value(@count) %>
      </span>
      <%= if @limit do %>
        <span class="text-green-400/50 font-mono">/</span>
        <span class="text-green-400/70 font-mono"><%= format_number_value(@limit) %></span>
      <% end %>
    </div>
    """
  end

  @doc """
  Formats supervisor health status.
  """
  def format_supervisor_health(assigns) do
    ~H"""
    <div class="flex items-center space-x-2">
      <span class={[
        "inline-flex items-center space-x-1 font-mono text-sm",
        health_color(@health)
      ]}>
        <span><%= health_icon(@health) %></span>
        <span><%= String.capitalize(to_string(@health)) %></span>
      </span>
      <%= if @children_count do %>
        <span class="text-green-400/50 font-mono text-xs">
          (<%= @children_count %> children)
        </span>
      <% end %>
    </div>
    """
  end

  @doc """
  Formats operation status for Arsenal.
  """
  def format_operation_status(assigns) do
    ~H"""
    <span class={[
      "inline-flex items-center space-x-1 px-2 py-1 rounded text-xs font-mono",
      operation_status_style(@status)
    ]}>
      <span><%= operation_status_icon(@status) %></span>
      <span><%= String.capitalize(to_string(@status)) %></span>
    </span>
    """
  end

  @doc """
  Formats trend indicators with arrows.
  """
  def format_trend(assigns) do
    ~H"""
    <span class={[
      "inline-flex items-center space-x-1 font-mono text-sm",
      trend_color(@trend)
    ]}>
      <span><%= trend_symbol(@trend) %></span>
      <%= if @value do %>
        <span><%= @value %></span>
      <% end %>
    </span>
    """
  end

  # Private helper functions

  defp format_bytes_value(bytes) when is_integer(bytes) do
    cond do
      bytes >= 1_099_511_627_776 -> "#{Float.round(bytes / 1_099_511_627_776, 1)}TB"
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 1)}GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 1)}MB"
      bytes >= 1_024 -> "#{Float.round(bytes / 1_024, 1)}KB"
      true -> "#{bytes}B"
    end
  end

  defp format_bytes_value(bytes), do: to_string(bytes)

  defp format_uptime_value(seconds) when is_integer(seconds) do
    days = div(seconds, 86400)
    hours = div(rem(seconds, 86400), 3600)
    minutes = div(rem(seconds, 3600), 60)
    secs = rem(seconds, 60)
    
    cond do
      days > 0 -> "#{days}d #{hours}h #{minutes}m"
      hours > 0 -> "#{hours}h #{minutes}m #{secs}s"
      minutes > 0 -> "#{minutes}m #{secs}s"
      true -> "#{secs}s"
    end
  end

  defp format_uptime_value(uptime), do: to_string(uptime)

  defp format_number_value(num) when is_integer(num) and num >= 1000 do
    num
    |> Integer.to_string()
    |> String.reverse()
    |> String.replace(~r/(\d{3})(?=\d)/, "\\1,")
    |> String.reverse()
  end

  defp format_number_value(num) when is_float(num) do
    :erlang.float_to_binary(num, decimals: 2)
  end

  defp format_number_value(num), do: to_string(num)

  defp format_percentage_value(value) when is_float(value) do
    Float.round(value, 1)
  end

  defp format_percentage_value(value), do: value

  defp format_timestamp_value(timestamp) when is_binary(timestamp) do
    case DateTime.from_iso8601(timestamp) do
      {:ok, dt, _} -> Calendar.strftime(dt, "%H:%M:%S")
      _ -> timestamp
    end
  end

  defp format_timestamp_value(%DateTime{} = dt) do
    Calendar.strftime(dt, "%H:%M:%S")
  end

  defp format_timestamp_value(timestamp), do: to_string(timestamp)

  defp format_status_text(status) when is_atom(status) do
    status |> to_string() |> String.capitalize()
  end

  defp format_status_text(status), do: to_string(status)

  # Color and styling functions

  defp status_color(:running), do: "text-green-400"
  defp status_color(:stopped), do: "text-red-400"
  defp status_color(:error), do: "text-red-400"
  defp status_color(:warning), do: "text-yellow-400"
  defp status_color(:pending), do: "text-yellow-400"
  defp status_color(:inactive), do: "text-gray-400"
  defp status_color(_), do: "text-green-400"

  defp status_icon(:running), do: "✓"
  defp status_icon(:stopped), do: "✗"
  defp status_icon(:error), do: "⚠"
  defp status_icon(:warning), do: "⚠"
  defp status_icon(:pending), do: "⏸"
  defp status_icon(:inactive), do: "○"
  defp status_icon(_), do: "●"

  defp percentage_color(value, threshold) when is_number(value) and is_number(threshold) do
    cond do
      value >= threshold * 0.9 -> "text-red-400"
      value >= threshold * 0.7 -> "text-yellow-400"
      true -> "text-green-400"
    end
  end

  defp percentage_color(value, _) when is_number(value) do
    cond do
      value >= 90 -> "text-red-400"
      value >= 70 -> "text-yellow-400"
      true -> "text-green-400"
    end
  end

  defp percentage_color(_, _), do: "text-green-400"

  defp memory_usage_color(used, total) when is_number(used) and is_number(total) and total > 0 do
    percentage = (used / total) * 100
    cond do
      percentage >= 90 -> "text-red-400"
      percentage >= 70 -> "text-yellow-400"
      true -> "text-green-400"
    end
  end

  defp memory_usage_color(_, _), do: "text-green-400"

  defp cpu_usage_color(value) when is_number(value) do
    cond do
      value >= 90 -> "text-red-400"
      value >= 70 -> "text-yellow-400"
      true -> "text-green-400"
    end
  end

  defp cpu_usage_color(_), do: "text-green-400"

  defp process_count_color(count, limit) when is_number(count) and is_number(limit) and limit > 0 do
    percentage = (count / limit) * 100
    cond do
      percentage >= 90 -> "text-red-400"
      percentage >= 70 -> "text-yellow-400"
      true -> "text-green-400"
    end
  end

  defp process_count_color(count, _) when is_number(count) do
    cond do
      count >= 1000 -> "text-yellow-400"
      count >= 10000 -> "text-red-400"
      true -> "text-green-400"
    end
  end

  defp process_count_color(_, _), do: "text-green-400"

  defp health_color(:healthy), do: "text-green-400"
  defp health_color(:warning), do: "text-yellow-400"
  defp health_color(:critical), do: "text-red-400"
  defp health_color(:unknown), do: "text-gray-400"
  defp health_color(_), do: "text-green-400"

  defp health_icon(:healthy), do: "✓"
  defp health_icon(:warning), do: "⚠"
  defp health_icon(:critical), do: "✗"
  defp health_icon(:unknown), do: "?"
  defp health_icon(_), do: "●"

  defp operation_status_style(:active), do: "bg-green-500/20 text-green-400 border border-green-500/30"
  defp operation_status_style(:planned), do: "bg-blue-500/20 text-blue-400 border border-blue-500/30"
  defp operation_status_style(:inactive), do: "bg-gray-500/20 text-gray-400 border border-gray-500/30"
  defp operation_status_style(:error), do: "bg-red-500/20 text-red-400 border border-red-500/30"
  defp operation_status_style(_), do: "bg-green-500/20 text-green-400 border border-green-500/30"

  defp operation_status_icon(:active), do: "●"
  defp operation_status_icon(:planned), do: "○"
  defp operation_status_icon(:inactive), do: "◯"
  defp operation_status_icon(:error), do: "✗"
  defp operation_status_icon(_), do: "●"

  defp trend_color(:up), do: "text-green-400"
  defp trend_color(:down), do: "text-red-400"
  defp trend_color(:stable), do: "text-gray-400"
  defp trend_color(_), do: "text-gray-400"

  defp trend_symbol(:up), do: "↑"
  defp trend_symbol(:down), do: "↓"
  defp trend_symbol(:stable), do: "→"
  defp trend_symbol(_), do: "―"
end