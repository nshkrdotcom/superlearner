defmodule OtpSupervisorWeb.Components.Terminal.TerminalTable do
  use Phoenix.LiveComponent

  @moduledoc """
  Terminal-themed data table with sorting, filtering, and actions.
  
  Provides a consistent table interface with terminal styling across all pages.
  """

  attr :title, :string, required: true
  attr :rows, :list, required: true
  attr :columns, :list, required: true
  attr :sortable, :boolean, default: true
  attr :filterable, :boolean, default: true
  attr :selectable, :boolean, default: false
  attr :max_height, :string, default: "max-h-96"
  attr :border_color, :string, default: "border-green-500/30"
  attr :bg_color, :string, default: "bg-gray-900"
  attr :text_color, :string, default: "text-green-400"
  attr :empty_message, :string, default: "No data available"
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""

  slot :actions
  slot :row_actions

  def render(assigns) do
    ~H"""
    <div class={[
      "flex flex-col",
      @class
    ]}>
      <!-- Table container -->
      <div class={[
        "flex flex-col border rounded",
        @border_color,
        @bg_color,
        @text_color
      ]}>
        <!-- Header -->
        <div class="flex items-center justify-between p-3 border-b border-green-500/20">
          <div class="flex items-center space-x-4">
            <h3 class="text-sm font-mono font-bold"><%= @title %></h3>
            <%= if @rows != [] do %>
              <span class="text-xs text-green-400/70 font-mono">
                (<%= length(@rows) %> rows)
              </span>
            <% end %>
          </div>
          
          <div class="flex items-center space-x-2">
            <%= if @filterable and @rows != [] do %>
              <input
                type="text"
                placeholder="Filter..."
                class="px-2 py-1 text-xs bg-gray-800 border border-green-500/30 rounded text-green-400 font-mono focus:outline-none focus:border-green-500"
                phx-target={@myself}
                phx-keyup="filter"
                value={@filter_text || ""}
              />
            <% end %>
            
            <%= render_slot(@actions) %>
          </div>
        </div>

        <!-- Table content -->
        <div class={[
          "overflow-auto",
          @max_height
        ]}>
          <%= if @loading do %>
            <div class="p-8 text-center">
              <div class="text-green-400/70 text-sm font-mono">Loading...</div>
            </div>
          <% else %>
            <%= if @rows == [] do %>
              <div class="p-8 text-center">
                <div class="text-green-400/50 text-sm font-mono"><%= @empty_message %></div>
              </div>
            <% else %>
              <table class="w-full">
                <thead>
                  <tr class="border-b border-green-500/20">
                    <%= if @selectable do %>
                      <th class="px-3 py-2 text-left">
                        <input
                          type="checkbox"
                          class="text-green-400 bg-gray-800 border-green-500/30 rounded focus:ring-green-500"
                          phx-target={@myself}
                          phx-click="toggle_all"
                        />
                      </th>
                    <% end %>
                    
                    <%= for column <- @columns do %>
                      <th class="px-3 py-2 text-left">
                        <%= if @sortable and column[:sortable] != false do %>
                          <button
                            class="flex items-center space-x-1 text-green-300 hover:text-green-400 font-mono text-xs font-bold"
                            phx-target={@myself}
                            phx-click="sort"
                            phx-value-column={column.key}
                          >
                            <span><%= column.label %></span>
                            <%= if @sort_column == column.key do %>
                              <span class="text-xs">
                                <%= if @sort_direction == :asc, do: "↑", else: "↓" %>
                              </span>
                            <% end %>
                          </button>
                        <% else %>
                          <span class="text-green-300 font-mono text-xs font-bold">
                            <%= column.label %>
                          </span>
                        <% end %>
                      </th>
                    <% end %>
                    
                    <%= if @row_actions != [] do %>
                      <th class="px-3 py-2 text-left">
                        <span class="text-green-300 font-mono text-xs font-bold">Actions</span>
                      </th>
                    <% end %>
                  </tr>
                </thead>
                <tbody>
                  <%= for {row, _index} <- Enum.with_index(filtered_and_sorted_rows(@rows, assigns)) do %>
                    <tr class={[
                      "border-b border-green-500/10 hover:bg-green-500/5 transition-colors",
                      if(@selectable and Map.get(@selected_rows, row_id(row), false), do: "bg-green-500/10", else: "")
                    ]}>
                      <%= if @selectable do %>
                        <td class="px-3 py-2">
                          <input
                            type="checkbox"
                            class="text-green-400 bg-gray-800 border-green-500/30 rounded focus:ring-green-500"
                            checked={Map.get(@selected_rows, row_id(row), false)}
                            phx-target={@myself}
                            phx-click="toggle_row"
                            phx-value-row-id={row_id(row)}
                          />
                        </td>
                      <% end %>
                      
                      <%= for column <- @columns do %>
                        <td class="px-3 py-2 text-sm font-mono">
                          <%= format_cell_value(row, column) %>
                        </td>
                      <% end %>
                      
                      <%= if @row_actions != [] do %>
                        <td class="px-3 py-2">
                          <div class="flex items-center space-x-2">
                            <%= render_slot(@row_actions) %>
                          </div>
                        </td>
                      <% end %>
                    </tr>
                  <% end %>
                </tbody>
              </table>
            <% end %>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  def mount(socket) do
    {:ok, 
     socket
     |> assign(:sort_column, nil)
     |> assign(:sort_direction, :asc)
     |> assign(:filter_text, "")
     |> assign(:selected_rows, %{})}
  end

  def update(assigns, socket) do
    {:ok, assign(socket, assigns)}
  end

  def handle_event("sort", %{"column" => column}, socket) do
    column = String.to_existing_atom(column)
    
    {sort_column, sort_direction} = 
      if socket.assigns.sort_column == column do
        {column, toggle_direction(socket.assigns.sort_direction)}
      else
        {column, :asc}
      end
    
    {:noreply, 
     socket
     |> assign(:sort_column, sort_column)
     |> assign(:sort_direction, sort_direction)}
  end

  def handle_event("filter", %{"value" => filter_text}, socket) do
    {:noreply, assign(socket, :filter_text, filter_text)}
  end

  def handle_event("toggle_row", %{"row-id" => row_id}, socket) do
    selected_rows = socket.assigns.selected_rows
    new_selected_rows = 
      if Map.get(selected_rows, row_id, false) do
        Map.delete(selected_rows, row_id)
      else
        Map.put(selected_rows, row_id, true)
      end
    
    {:noreply, assign(socket, :selected_rows, new_selected_rows)}
  end

  def handle_event("toggle_all", _params, socket) do
    all_selected = Enum.all?(socket.assigns.rows, fn row -> 
      Map.get(socket.assigns.selected_rows, row_id(row), false) 
    end)
    
    new_selected_rows = 
      if all_selected do
        %{}
      else
        Enum.reduce(socket.assigns.rows, %{}, fn row, acc ->
          Map.put(acc, row_id(row), true)
        end)
      end
    
    {:noreply, assign(socket, :selected_rows, new_selected_rows)}
  end

  # Private functions

  defp filtered_and_sorted_rows(rows, assigns) do
    rows
    |> filter_rows(assigns[:filter_text])
    |> sort_rows(assigns[:sort_column], assigns[:sort_direction])
  end

  defp filter_rows(rows, filter_text) when is_binary(filter_text) and filter_text != "" do
    filter_text = String.downcase(filter_text)
    
    Enum.filter(rows, fn row ->
      row
      |> Map.values()
      |> Enum.any?(fn value ->
        value
        |> to_string()
        |> String.downcase()
        |> String.contains?(filter_text)
      end)
    end)
  end

  defp filter_rows(rows, _), do: rows

  defp sort_rows(rows, nil, _), do: rows

  defp sort_rows(rows, column, direction) do
    sorted = Enum.sort_by(rows, fn row -> 
      Map.get(row, column, "") |> sort_value()
    end)
    
    case direction do
      :asc -> sorted
      :desc -> Enum.reverse(sorted)
    end
  end

  defp sort_value(value) when is_binary(value), do: String.downcase(value)
  defp sort_value(value) when is_number(value), do: value
  defp sort_value(value), do: to_string(value)

  defp toggle_direction(:asc), do: :desc
  defp toggle_direction(:desc), do: :asc

  defp row_id(row) do
    Map.get(row, :id) || Map.get(row, "id") || :erlang.phash2(row)
  end

  defp format_cell_value(row, column) do
    value = Map.get(row, column.key, "")
    
    case column[:format] do
      :bytes -> format_bytes(value)
      :percentage -> "#{value}%"
      :number -> format_number(value)
      :uptime -> format_uptime(value)
      :status -> format_status(value)
      :timestamp -> format_timestamp(value)
      _ -> to_string(value)
    end
  end

  defp format_bytes(bytes) when is_integer(bytes) do
    cond do
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 1)}GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 1)}MB"
      bytes >= 1_024 -> "#{Float.round(bytes / 1_024, 1)}KB"
      true -> "#{bytes}B"
    end
  end

  defp format_bytes(bytes), do: to_string(bytes)

  defp format_number(num) when is_integer(num) and num >= 1000 do
    num
    |> Integer.to_string()
    |> String.reverse()
    |> String.replace(~r/(\d{3})(?=\d)/, "\\1,")
    |> String.reverse()
  end

  defp format_number(num), do: to_string(num)

  defp format_uptime(seconds) when is_integer(seconds) do
    days = div(seconds, 86400)
    hours = div(rem(seconds, 86400), 3600)
    minutes = div(rem(seconds, 3600), 60)
    
    cond do
      days > 0 -> "#{days}d #{hours}h"
      hours > 0 -> "#{hours}h #{minutes}m"
      true -> "#{minutes}m"
    end
  end

  defp format_uptime(uptime), do: to_string(uptime)

  defp format_status(status) when is_atom(status) do
    case status do
      :running -> "✓ Running"
      :stopped -> "✗ Stopped"
      :error -> "⚠ Error"
      :pending -> "⏸ Pending"
      _ -> String.capitalize(to_string(status))
    end
  end

  defp format_status(status), do: to_string(status)

  defp format_timestamp(timestamp) when is_binary(timestamp) do
    case DateTime.from_iso8601(timestamp) do
      {:ok, dt, _} -> Calendar.strftime(dt, "%H:%M:%S")
      _ -> timestamp
    end
  end

  defp format_timestamp(timestamp), do: to_string(timestamp)
end