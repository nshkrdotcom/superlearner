defmodule OtpSupervisorWeb.Components.ClusterTreeRenderer do
  @moduledoc """
  High-density text-based cluster tree renderer using Unicode box-drawing characters.

  Converts hierarchical cluster data into ASCII tree format with color coding
  and compact single-line process information display.
  """

  @doc """
  Renders cluster data as high-density ASCII tree text.

  Returns HTML-safe string with proper styling classes for terminal display.
  """
  def render_cluster_tree(cluster_data) when is_map(cluster_data) do
    node_sections =
      cluster_data
      |> Enum.map(fn {node_name, node_data} ->
        render_node_section(node_name, node_data)
      end)

    # Add horizontal separator lines between nodes for visual clarity
    separator = """
    <div class="border-t border-green-500/20 my-4">
      <div class="text-center text-green-500/30 text-xs py-2 font-mono">
        ─────────────────────────────────────────────────────────────────────────────────
      </div>
    </div>
    """

    node_sections
    |> Enum.join(separator)
    |> Phoenix.HTML.raw()
  end

  def render_cluster_tree(_), do: Phoenix.HTML.raw("")

  # Private functions

  defp render_node_section(node_name, node_data) do
    node_header = render_node_header(node_name, node_data)
    node_stats = render_node_stats(node_data)

    tree_content =
      case Map.get(node_data, :children) do
        nil -> ""
        [] -> "    <span class=\"text-green-400/50\">└─ (no processes)</span>"
        children -> render_tree_children(children, "    ", true)
      end

    """
    <div class="font-mono text-sm">
      #{node_header}
      #{node_stats}
      #{tree_content}
    </div>
    """
  end

  defp render_node_header(node_name, node_data) do
    status = if Map.get(node_data, :alive, true), do: "connected", else: "disconnected"
    status_class = if status == "connected", do: "text-green-400", else: "text-red-400"

    """
    <div class="border-b border-green-500/30 pb-1 mb-2">
      <span class="text-green-300 font-bold">=== Node: #{node_name}</span>
      <span class="#{status_class}"> (#{status})</span>
      <span class="text-green-500"> ===</span>
    </div>
    """
  end

  defp render_node_stats(node_data) do
    process_count = count_processes_in_node(node_data)
    total_memory = calculate_total_memory(node_data)
    total_messages = calculate_total_messages(node_data)

    """
    <div class="text-green-400/70 text-xs mb-2 pl-2">
      Processes: #{format_number(process_count)} | 
      Memory: #{format_memory(total_memory)} | 
      Messages: #{format_number(total_messages)}
    </div>
    """
  end

  defp render_tree_children(children, prefix, _is_last_group) do
    children
    |> Enum.with_index()
    |> Enum.map(fn {child, index} ->
      is_last = index == length(children) - 1
      render_tree_item(child, prefix, is_last)
    end)
    |> Enum.join("")
  end

  defp render_tree_item(item, prefix, is_last) do
    connector = if is_last, do: "└─", else: "├─"
    next_prefix = prefix <> if is_last, do: "  ", else: "│ "

    # Determine item styling based on type and status
    {type_indicator, type_class} = get_type_styling(item)
    status_class = if Map.get(item, :alive, true), do: "", else: " text-red-400"

    # Format the main line
    name = Map.get(item, :name, "unnamed")
    pid = format_pid(Map.get(item, :pid))
    memory = format_memory(Map.get(item, :memory, 0))
    messages = Map.get(item, :message_queue_len, 0)

    main_line = """
    <div class="whitespace-nowrap">
      <span class="text-green-500/50">#{prefix}#{connector}</span>
      <span class="#{type_class}#{status_class}"> #{name}</span>
      <span class="text-green-400/60"> [#{pid}]</span>
      <span class="text-blue-400/70"> (#{type_indicator})</span>
      <span class="text-yellow-400/60"> #{memory}</span>
      <span class="text-purple-400/60"> #{messages}msgs</span>
    </div>
    """

    # Render children if any
    children_content =
      case Map.get(item, :children) do
        nil -> ""
        [] -> ""
        children -> render_tree_children(children, next_prefix, false)
      end

    main_line <> children_content
  end

  defp get_type_styling(item) do
    case Map.get(item, :type, :worker) do
      :supervisor -> {"supervisor", "text-green-300 font-semibold"}
      :virtual_supervisor -> {"v-supervisor", "text-cyan-300 font-semibold"}
      :worker -> {"worker", "text-blue-300"}
      :node -> {"node", "text-green-400 font-bold"}
      _ -> {"unknown", "text-gray-400"}
    end
  end

  defp format_pid(pid) when is_pid(pid) do
    pid |> inspect() |> String.replace("#PID", "#")
  end

  defp format_pid(pid) when is_binary(pid), do: pid
  defp format_pid(_), do: "unknown"

  defp format_memory(bytes) when is_integer(bytes) and bytes > 0 do
    cond do
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 1)}GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 1)}MB"
      bytes >= 1024 -> "#{Float.round(bytes / 1024, 1)}KB"
      true -> "#{bytes}B"
    end
  end

  defp format_memory(_), do: "0B"

  defp format_number(num) when is_integer(num) do
    num
    |> Integer.to_string()
    |> String.reverse()
    |> String.replace(~r/(\d{3})(?=\d)/, "\\1,")
    |> String.reverse()
  end

  defp format_number(_), do: "0"

  defp count_processes_in_node(node_data) do
    case Map.get(node_data, :children) do
      nil -> 0
      children -> Enum.reduce(children, 0, &(&2 + count_processes_recursive(&1)))
    end
  end

  defp count_processes_recursive(item) do
    base_count = 1

    case Map.get(item, :children) do
      nil -> base_count
      children -> base_count + Enum.reduce(children, 0, &(&2 + count_processes_recursive(&1)))
    end
  end

  defp calculate_total_memory(node_data) do
    case Map.get(node_data, :children) do
      nil -> 0
      children -> Enum.reduce(children, 0, &(&2 + calculate_memory_recursive(&1)))
    end
  end

  defp calculate_memory_recursive(item) do
    base_memory = Map.get(item, :memory, 0)

    case Map.get(item, :children) do
      nil -> base_memory
      children -> base_memory + Enum.reduce(children, 0, &(&2 + calculate_memory_recursive(&1)))
    end
  end

  defp calculate_total_messages(node_data) do
    case Map.get(node_data, :children) do
      nil -> 0
      children -> Enum.reduce(children, 0, &(&2 + calculate_messages_recursive(&1)))
    end
  end

  defp calculate_messages_recursive(item) do
    base_messages = Map.get(item, :message_queue_len, 0)

    case Map.get(item, :children) do
      nil ->
        base_messages

      children ->
        base_messages + Enum.reduce(children, 0, &(&2 + calculate_messages_recursive(&1)))
    end
  end
end
