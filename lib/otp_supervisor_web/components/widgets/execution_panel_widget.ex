defmodule OtpSupervisorWeb.Components.Widgets.ExecutionPanelWidget do
  use Phoenix.LiveComponent

  @moduledoc """
  Command execution monitoring widget for Arsenal operations.

  Provides real-time monitoring of command execution including:
  - Live execution history
  - Real-time output streaming
  - Progress tracking
  - Error handling and reporting
  - Execution statistics
  """

  attr :executions, :list, required: true
  attr :live_output, :list, default: []
  attr :show_output, :boolean, default: true
  attr :max_history, :integer, default: 50
  attr :auto_scroll, :boolean, default: true
  attr :compact_mode, :boolean, default: false
  attr :class, :string, default: ""

  def render(assigns) do
    ~H"""
    <div class={[
      "bg-gray-900 border border-green-500/30 rounded text-green-400 h-full flex flex-col",
      @class
    ]}>
      <!-- Header -->
      <div class="flex items-center justify-between p-3 border-b border-green-500/20">
        <div class="flex items-center space-x-4">
          <h3 class="text-sm font-mono font-bold text-green-300">
            Execution Panel
          </h3>
          <span class="text-xs text-green-400/70 font-mono">
            ({length(@executions)} executions)
          </span>
        </div>

        <div class="flex items-center space-x-2">
          <!-- Status indicators -->
          <div class="flex items-center space-x-3">
            <div class="flex items-center space-x-1">
              <div class="w-2 h-2 bg-blue-400 rounded-full animate-pulse"></div>
              <span class="text-xs text-green-400/70 font-mono">
                {count_by_status(@executions, :running)} running
              </span>
            </div>
            <div class="flex items-center space-x-1">
              <div class="w-2 h-2 bg-green-400 rounded-full"></div>
              <span class="text-xs text-green-400/70 font-mono">
                {count_by_status(@executions, :completed)} completed
              </span>
            </div>
            <div class="flex items-center space-x-1">
              <div class="w-2 h-2 bg-red-400 rounded-full"></div>
              <span class="text-xs text-green-400/70 font-mono">
                {count_by_status(@executions, :failed)} failed
              </span>
            </div>
          </div>
          
    <!-- Controls -->
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

          <button
            phx-target={@myself}
            phx-click="clear_history"
            class="px-2 py-1 text-xs bg-red-500/20 border border-red-500/30 rounded text-red-400 font-mono hover:bg-red-500/30 transition-colors"
          >
            Clear
          </button>
        </div>
      </div>
      
    <!-- Execution history -->
      <div class="flex-1 flex flex-col min-h-0">
        <%= if @executions == [] do %>
          <div class="flex-1 flex items-center justify-center">
            <div class="text-center">
              <div class="text-green-400/50 text-sm font-mono">No executions yet</div>
              <div class="text-green-400/30 text-xs font-mono mt-1">
                Execute an operation to see results here
              </div>
            </div>
          </div>
        <% else %>
          <div class="flex-1 overflow-auto">
            <div class="space-y-2 p-4">
              <%= for {execution, _index} <- Enum.with_index(@executions) do %>
                <div class={[
                  "border rounded p-3 transition-all duration-200",
                  execution_border_color(execution.status),
                  if(execution.status == :running, do: "animate-pulse", else: "")
                ]}>
                  <!-- Execution header -->
                  <div class="flex items-center justify-between mb-2">
                    <div class="flex items-center space-x-2">
                      <span class={[
                        "w-2 h-2 rounded-full",
                        execution_status_color(execution.status)
                      ]}>
                      </span>
                      <span class="font-mono text-sm font-bold text-green-300">
                        {execution.operation_name}
                      </span>
                      <span class="text-xs text-green-400/70 font-mono">
                        ID: {execution.id}
                      </span>
                    </div>

                    <div class="flex items-center space-x-2">
                      <span class={[
                        "px-2 py-1 rounded text-xs font-mono",
                        execution_status_classes(execution.status)
                      ]}>
                        {String.capitalize(to_string(execution.status))}
                      </span>

                      <%= if execution.status == :running do %>
                        <button
                          phx-target={@myself}
                          phx-click="cancel_execution"
                          phx-value-execution-id={execution.id}
                          class="px-2 py-1 text-xs bg-red-500/20 border border-red-500/30 rounded text-red-400 font-mono hover:bg-red-500/30 transition-colors"
                        >
                          Cancel
                        </button>
                      <% end %>
                    </div>
                  </div>
                  
    <!-- Execution details -->
                  <div class="grid grid-cols-3 gap-4 text-xs mb-2">
                    <div>
                      <span class="text-green-400/70">Started:</span>
                      <span class="text-green-400 font-mono ml-1">
                        {format_timestamp(execution.started_at)}
                      </span>
                    </div>
                    <%= if execution.completed_at do %>
                      <div>
                        <span class="text-green-400/70">Completed:</span>
                        <span class="text-green-400 font-mono ml-1">
                          {format_timestamp(execution.completed_at)}
                        </span>
                      </div>
                      <div>
                        <span class="text-green-400/70">Duration:</span>
                        <span class="text-green-400 font-mono ml-1">
                          {format_duration(execution.started_at, execution.completed_at)}
                        </span>
                      </div>
                    <% else %>
                      <div>
                        <span class="text-green-400/70">Duration:</span>
                        <span class="text-green-400 font-mono ml-1">
                          {format_duration(execution.started_at, DateTime.utc_now())}
                        </span>
                      </div>
                    <% end %>
                  </div>
                  
    <!-- Progress bar (if running) -->
                  <%= if execution.status == :running and execution.progress do %>
                    <div class="mb-2">
                      <div class="flex items-center justify-between text-xs mb-1">
                        <span class="text-green-400/70 font-mono">Progress</span>
                        <span class="text-green-400 font-mono">{execution.progress}%</span>
                      </div>
                      <div class="h-1 bg-gray-700 rounded overflow-hidden">
                        <div
                          class="h-full bg-blue-500 rounded transition-all duration-300"
                          style={"width: #{execution.progress}%"}
                        >
                        </div>
                      </div>
                    </div>
                  <% end %>
                  
    <!-- Result or error -->
                  <%= if execution.result do %>
                    <div class="mt-2">
                      <div class="text-xs text-green-400/70 font-mono mb-1">Result:</div>
                      <div class={[
                        "p-2 rounded text-xs font-mono",
                        result_classes(execution.result)
                      ]}>
                        {format_result(execution.result)}
                      </div>
                    </div>
                  <% end %>
                  
    <!-- Output preview -->
                  <%= if execution.output and execution.output != "" do %>
                    <div class="mt-2">
                      <div class="text-xs text-green-400/70 font-mono mb-1">Output:</div>
                      <div class="bg-gray-800 rounded p-2 text-xs font-mono text-green-400 max-h-20 overflow-y-auto">
                        {execution.output}
                      </div>
                    </div>
                  <% end %>
                </div>
              <% end %>
            </div>
          </div>
        <% end %>
      </div>
      
    <!-- Live output stream -->
      <%= if @show_output and @live_output != [] do %>
        <div class="border-t border-green-500/20">
          <div class="flex items-center justify-between p-2 bg-green-500/5">
            <span class="text-xs font-mono font-bold text-green-300">Live Output</span>
            <div class="flex items-center space-x-1">
              <div class="w-2 h-2 bg-green-400 rounded-full animate-pulse"></div>
              <span class="text-xs text-green-400/70 font-mono">Streaming</span>
            </div>
          </div>
          <div
            class="bg-gray-800 h-32 overflow-y-auto font-mono text-xs text-green-400 p-2"
            id={"live-output-#{@myself}"}
            phx-hook={if @auto_scroll, do: "AutoScroll", else: nil}
          >
            <%= for {output, index} <- Enum.with_index(@live_output) do %>
              <div class="flex items-start space-x-2">
                <span class="text-green-400/50 flex-shrink-0">
                  {String.pad_leading(to_string(index + 1), 3, "0")}
                </span>
                <span class="flex-1">
                  {output.message}
                </span>
                <span class="text-green-400/30 flex-shrink-0 text-xs">
                  {format_timestamp(output.timestamp)}
                </span>
              </div>
            <% end %>
          </div>
        </div>
      <% end %>
      
    <!-- Footer with statistics -->
      <div class="border-t border-green-500/20 p-2">
        <div class="flex items-center justify-between text-xs">
          <div class="flex items-center space-x-4">
            <span class="text-green-400/70 font-mono">
              Success Rate:
              <span class="text-green-400">
                {calculate_success_rate(@executions)}%
              </span>
            </span>
            <span class="text-green-400/70 font-mono">
              Avg Duration:
              <span class="text-green-400">
                {calculate_avg_duration(@executions)}
              </span>
            </span>
          </div>
          <div class="text-green-400/70 font-mono">
            Total Executions: <span class="text-green-400">{length(@executions)}</span>
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

  def handle_event("toggle_auto_scroll", _params, socket) do
    {:noreply, assign(socket, :auto_scroll, !socket.assigns.auto_scroll)}
  end

  def handle_event("clear_history", _params, socket) do
    send(self(), :clear_execution_history)
    {:noreply, socket}
  end

  def handle_event("cancel_execution", %{"execution_id" => execution_id}, socket) do
    send(self(), {:cancel_execution, execution_id})
    {:noreply, socket}
  end

  # Private helper functions

  defp count_by_status(executions, status) do
    Enum.count(executions, &(&1.status == status))
  end

  defp execution_border_color(:running), do: "border-blue-500/30"
  defp execution_border_color(:completed), do: "border-green-500/30"
  defp execution_border_color(:failed), do: "border-red-500/30"
  defp execution_border_color(_), do: "border-gray-500/30"

  defp execution_status_color(:running), do: "bg-blue-400"
  defp execution_status_color(:completed), do: "bg-green-400"
  defp execution_status_color(:failed), do: "bg-red-400"
  defp execution_status_color(_), do: "bg-gray-400"

  defp execution_status_classes(:running), do: "bg-blue-500/20 text-blue-400"
  defp execution_status_classes(:completed), do: "bg-green-500/20 text-green-400"
  defp execution_status_classes(:failed), do: "bg-red-500/20 text-red-400"
  defp execution_status_classes(_), do: "bg-gray-500/20 text-gray-400"

  defp result_classes(:success), do: "bg-green-500/20 text-green-400"
  defp result_classes(:error), do: "bg-red-500/20 text-red-400"
  defp result_classes(_), do: "bg-gray-500/20 text-gray-400"

  defp format_result(:success), do: "Operation completed successfully"
  defp format_result(:error), do: "Operation failed"
  defp format_result(result) when is_binary(result), do: result
  defp format_result(result), do: inspect(result)

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

  defp format_duration(%DateTime{} = start_time, %DateTime{} = end_time) do
    diff = DateTime.diff(end_time, start_time, :millisecond)

    cond do
      diff < 1000 -> "#{diff}ms"
      diff < 60_000 -> "#{Float.round(diff / 1000, 1)}s"
      diff < 3_600_000 -> "#{div(diff, 60_000)}m #{rem(div(diff, 1000), 60)}s"
      true -> "#{div(diff, 3_600_000)}h #{rem(div(diff, 60_000), 60)}m"
    end
  end

  defp format_duration(_, _), do: "N/A"

  defp calculate_success_rate(executions) do
    if length(executions) == 0 do
      0
    else
      completed = Enum.filter(executions, &(&1.status == :completed))
      successful = Enum.count(completed, &(&1.result == :success))
      Float.round(successful / length(executions) * 100, 1)
    end
  end

  defp calculate_avg_duration(executions) do
    completed =
      Enum.filter(executions, &(&1.status == :completed and &1.started_at and &1.completed_at))

    if length(completed) == 0 do
      "N/A"
    else
      total_duration =
        Enum.reduce(completed, 0, fn execution, acc ->
          duration = DateTime.diff(execution.completed_at, execution.started_at, :millisecond)
          acc + duration
        end)

      avg_duration = div(total_duration, length(completed))

      cond do
        avg_duration < 1000 -> "#{avg_duration}ms"
        avg_duration < 60_000 -> "#{Float.round(avg_duration / 1000, 1)}s"
        true -> "#{Float.round(avg_duration / 60_000, 1)}m"
      end
    end
  end
end
