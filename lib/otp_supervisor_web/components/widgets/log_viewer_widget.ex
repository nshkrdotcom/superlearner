defmodule OtpSupervisorWeb.Components.Widgets.LogViewerWidget do
  use Phoenix.LiveComponent

  @moduledoc """
  Log viewer widget for displaying and filtering system logs.

  Provides comprehensive log viewing capabilities:
  - Real-time log streaming
  - Multi-level filtering (error, warning, info, debug)
  - Text search and pattern matching
  - Log level highlighting
  - Timestamp formatting
  - Auto-scroll and pause functionality
  - Export and download capabilities
  """

  attr :logs, :list, required: true
  attr :log_level, :atom, values: [:debug, :info, :warning, :error, :all], default: :all
  attr :search_term, :string, default: ""
  attr :auto_scroll, :boolean, default: true
  attr :show_timestamps, :boolean, default: true
  attr :show_levels, :boolean, default: true
  attr :max_lines, :integer, default: 1000
  attr :tail_mode, :boolean, default: true
  attr :word_wrap, :boolean, default: false
  attr :highlight_pattern, :string, default: ""
  attr :class, :string, default: ""

  def render(assigns) do
    ~H"""
    <div class={[
      "bg-gray-900 border border-green-500/30 rounded text-green-400 h-full flex flex-col",
      @class
    ]}>
      <!-- Header with controls -->
      <div class="flex items-center justify-between p-3 border-b border-green-500/20">
        <div class="flex items-center space-x-4">
          <h3 class="text-sm font-mono font-bold text-green-300">
            Log Viewer
          </h3>
          <span class="text-xs text-green-400/70 font-mono">
            ({length(filtered_logs(@logs, @log_level, @search_term))} entries)
          </span>
        </div>

        <div class="flex items-center space-x-2">
          <!-- Log level filter -->
          <select
            phx-target={@myself}
            phx-change="filter_level"
            class="px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:border-green-500"
          >
            <option value="all">All Levels</option>
            <option value="debug">Debug</option>
            <option value="info">Info</option>
            <option value="warning">Warning</option>
            <option value="error">Error</option>
          </select>
          
    <!-- Search input -->
          <input
            type="text"
            placeholder="Search logs..."
            phx-target={@myself}
            phx-keyup="search"
            phx-debounce="300"
            value={@search_term}
            class="px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:border-green-500 w-32"
          />
          
    <!-- Auto-scroll toggle -->
          <button
            phx-target={@myself}
            phx-click="toggle_auto_scroll"
            class={[
              "px-2 py-1 text-xs border border-green-500/30 rounded font-mono hover:bg-green-500/30 transition-colors",
              if(@auto_scroll,
                do: "bg-green-500/20 text-green-400",
                else: "bg-gray-500/20 text-gray-400"
              )
            ]}
          >
            Auto Scroll
          </button>
          
    <!-- Word wrap toggle -->
          <button
            phx-target={@myself}
            phx-click="toggle_word_wrap"
            class={[
              "px-2 py-1 text-xs border border-green-500/30 rounded font-mono hover:bg-green-500/30 transition-colors",
              if(@word_wrap,
                do: "bg-green-500/20 text-green-400",
                else: "bg-gray-500/20 text-gray-400"
              )
            ]}
          >
            Wrap
          </button>
          
    <!-- Clear logs -->
          <button
            phx-target={@myself}
            phx-click="clear_logs"
            class="px-2 py-1 text-xs bg-red-500/20 border border-red-500/30 rounded text-red-400 font-mono hover:bg-red-500/30 transition-colors"
          >
            Clear
          </button>
          
    <!-- Export logs -->
          <button
            phx-target={@myself}
            phx-click="export_logs"
            class="px-2 py-1 text-xs bg-blue-500/20 border border-blue-500/30 rounded text-blue-400 font-mono hover:bg-blue-500/30 transition-colors"
          >
            Export
          </button>
        </div>
      </div>
      
    <!-- Log content -->
      <div class="flex-1 flex flex-col min-h-0">
        <%= if filtered_logs(@logs, @log_level, @search_term) == [] do %>
          <div class="flex-1 flex items-center justify-center">
            <div class="text-center">
              <div class="text-green-400/50 text-sm font-mono">No logs found</div>
              <div class="text-green-400/30 text-xs font-mono mt-1">
                <%= if @search_term != "" do %>
                  Try adjusting your search term or log level filter
                <% else %>
                  Logs will appear here when generated
                <% end %>
              </div>
            </div>
          </div>
        <% else %>
          <div
            class={[
              "flex-1 overflow-auto bg-gray-800 text-xs font-mono",
              if(@word_wrap, do: "whitespace-pre-wrap", else: "whitespace-pre")
            ]}
            id={"log-viewer-#{@myself}"}
            phx-hook={if @auto_scroll, do: "AutoScroll", else: nil}
          >
            <div class="p-2 space-y-1">
              <%= for {log, index} <- Enum.with_index(filtered_logs(@logs, @log_level, @search_term)) do %>
                <div class={[
                  "flex items-start space-x-2 hover:bg-gray-700/50 px-1 py-0.5 rounded",
                  log_line_classes(log.level)
                ]}>
                  <!-- Line number -->
                  <span class="text-green-400/30 text-xs w-8 text-right flex-shrink-0 select-none">
                    {index + 1}
                  </span>
                  
    <!-- Timestamp -->
                  <%= if @show_timestamps do %>
                    <span class="text-green-400/50 text-xs flex-shrink-0 w-20">
                      {format_timestamp(log.timestamp)}
                    </span>
                  <% end %>
                  
    <!-- Log level -->
                  <%= if @show_levels do %>
                    <span class={[
                      "text-xs font-bold px-1 rounded flex-shrink-0 w-12 text-center",
                      log_level_classes(log.level)
                    ]}>
                      {log_level_text(log.level)}
                    </span>
                  <% end %>
                  
    <!-- Source/module -->
                  <%= if log.source do %>
                    <span class="text-blue-400/70 text-xs flex-shrink-0 max-w-24 truncate">
                      {log.source}
                    </span>
                  <% end %>
                  
    <!-- Log message -->
                  <span class={[
                    "text-xs flex-1 min-w-0",
                    log_message_classes(log.level)
                  ]}>
                    {highlight_search_term(log.message, @search_term)}
                  </span>
                </div>
              <% end %>
            </div>
          </div>
        <% end %>
      </div>
      
    <!-- Status bar -->
      <div class="border-t border-green-500/20 p-2">
        <div class="flex items-center justify-between text-xs">
          <div class="flex items-center space-x-4">
            <!-- Log level counts -->
            <%= for {level, count} <- log_level_counts(@logs) do %>
              <div class="flex items-center space-x-1">
                <div class={[
                  "w-2 h-2 rounded-full",
                  log_level_indicator_color(level)
                ]}>
                </div>
                <span class="text-green-400/70 font-mono">
                  {String.capitalize(to_string(level))}: {count}
                </span>
              </div>
            <% end %>
          </div>

          <div class="flex items-center space-x-4">
            <span class="text-green-400/70 font-mono">
              Showing {length(filtered_logs(@logs, @log_level, @search_term))} of {length(@logs)} logs
            </span>

            <%= if @tail_mode do %>
              <div class="flex items-center space-x-1">
                <div class="w-2 h-2 bg-green-400 rounded-full animate-pulse"></div>
                <span class="text-green-400/70 font-mono">Live</span>
              </div>
            <% end %>
          </div>
        </div>
      </div>
    </div>
    """
  end

  def mount(socket) do
    {:ok, socket}
  end

  def update(assigns, socket) do
    {:ok, assign(socket, assigns)}
  end

  # Event handlers

  def handle_event("filter_level", %{"value" => level}, socket) do
    level_atom = if level == "all", do: :all, else: String.to_atom(level)
    {:noreply, assign(socket, :log_level, level_atom)}
  end

  def handle_event("search", %{"value" => search_term}, socket) do
    {:noreply, assign(socket, :search_term, search_term)}
  end

  def handle_event("toggle_auto_scroll", _params, socket) do
    {:noreply, assign(socket, :auto_scroll, !socket.assigns.auto_scroll)}
  end

  def handle_event("toggle_word_wrap", _params, socket) do
    {:noreply, assign(socket, :word_wrap, !socket.assigns.word_wrap)}
  end

  def handle_event("clear_logs", _params, socket) do
    send(self(), :clear_logs)
    {:noreply, socket}
  end

  def handle_event("export_logs", _params, socket) do
    send(self(), :export_logs)
    {:noreply, socket}
  end

  # Private helper functions

  defp filtered_logs(logs, level, search_term) do
    logs
    |> filter_by_level(level)
    |> filter_by_search(search_term)
    # Keep last 1000 logs for performance
    |> Enum.take(-1000)
  end

  defp filter_by_level(logs, :all), do: logs

  defp filter_by_level(logs, level) do
    Enum.filter(logs, &(&1.level == level))
  end

  defp filter_by_search(logs, ""), do: logs

  defp filter_by_search(logs, search_term) do
    search_term = String.downcase(search_term)

    Enum.filter(logs, fn log ->
      String.contains?(String.downcase(log.message), search_term) ||
        (log.source && String.contains?(String.downcase(log.source), search_term))
    end)
  end

  defp log_level_counts(logs) do
    logs
    |> Enum.group_by(& &1.level)
    |> Enum.map(fn {level, logs} -> {level, length(logs)} end)
    |> Enum.sort_by(fn {level, _} -> level_order(level) end)
  end

  defp level_order(:error), do: 1
  defp level_order(:warning), do: 2
  defp level_order(:info), do: 3
  defp level_order(:debug), do: 4
  defp level_order(_), do: 5

  defp log_line_classes(:error), do: "border-l-2 border-red-500/50"
  defp log_line_classes(:warning), do: "border-l-2 border-yellow-500/50"
  defp log_line_classes(:info), do: "border-l-2 border-blue-500/50"
  defp log_line_classes(:debug), do: "border-l-2 border-green-500/50"
  defp log_line_classes(_), do: "border-l-2 border-gray-500/50"

  defp log_level_classes(:error), do: "bg-red-500/20 text-red-400"
  defp log_level_classes(:warning), do: "bg-yellow-500/20 text-yellow-400"
  defp log_level_classes(:info), do: "bg-blue-500/20 text-blue-400"
  defp log_level_classes(:debug), do: "bg-green-500/20 text-green-400"
  defp log_level_classes(_), do: "bg-gray-500/20 text-gray-400"

  defp log_message_classes(:error), do: "text-red-400"
  defp log_message_classes(:warning), do: "text-yellow-400"
  defp log_message_classes(:info), do: "text-blue-400"
  defp log_message_classes(:debug), do: "text-green-400"
  defp log_message_classes(_), do: "text-gray-400"

  defp log_level_indicator_color(:error), do: "bg-red-400"
  defp log_level_indicator_color(:warning), do: "bg-yellow-400"
  defp log_level_indicator_color(:info), do: "bg-blue-400"
  defp log_level_indicator_color(:debug), do: "bg-green-400"
  defp log_level_indicator_color(_), do: "bg-gray-400"

  defp log_level_text(:error), do: "ERR"
  defp log_level_text(:warning), do: "WARN"
  defp log_level_text(:info), do: "INFO"
  defp log_level_text(:debug), do: "DBG"
  defp log_level_text(_), do: "UNK"

  defp highlight_search_term(message, ""), do: message

  defp highlight_search_term(message, search_term) do
    # This is a simple version - in a real implementation,
    # you'd want to use proper HTML escaping and highlighting
    String.replace(
      message,
      search_term,
      ~s(<span class="bg-yellow-500/30 text-yellow-400">#{search_term}</span>),
      global: true
    )
  end

  defp format_timestamp(%DateTime{} = dt) do
    Calendar.strftime(dt, "%H:%M:%S")
  end

  defp format_timestamp(timestamp) when is_binary(timestamp) do
    case DateTime.from_iso8601(timestamp) do
      {:ok, dt, _} -> format_timestamp(dt)
      _ -> timestamp
    end
  end

  defp format_timestamp(_), do: "N/A"
end
