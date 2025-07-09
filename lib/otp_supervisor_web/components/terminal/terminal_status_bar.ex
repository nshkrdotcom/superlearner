defmodule OtpSupervisorWeb.Components.Terminal.TerminalStatusBar do
  use Phoenix.LiveComponent

  @moduledoc """
  Terminal-themed status bar component used across all pages.

  Provides a consistent header with title, metrics, and navigation links.
  """

  attr :title, :string, required: true
  attr :metrics, :list, default: []
  attr :navigation_links, :list, default: []
  attr :size, :atom, values: [:small, :medium, :large], default: :large
  attr :border_color, :string, default: "border-green-500/30"
  attr :bg_color, :string, default: "bg-gray-900"
  attr :text_color, :string, default: "text-green-400"

  slot :left_section
  slot :right_section
  slot :center_section

  def render(assigns) do
    ~H"""
    <div class={[
      "w-full flex items-center justify-between px-4 border-b",
      height_class(@size),
      @border_color,
      @bg_color,
      @text_color
    ]}>
      <!-- Left section: Title and metrics -->
      <div class="flex items-center space-x-4">
        <h1 class="text-lg font-mono font-bold">{@title}</h1>

        <%= if @metrics != [] do %>
          <div class="flex items-center space-x-3">
            <%= for metric <- @metrics do %>
              <div class="flex items-center space-x-1">
                <span class="text-green-300 text-sm font-mono">{metric.label}:</span>
                <span class="text-green-400 text-sm font-mono font-bold">{metric.value}</span>
              </div>
            <% end %>
          </div>
        <% end %>

        {render_slot(@left_section)}
      </div>
      
    <!-- Center section -->
      <div class="flex items-center">
        {render_slot(@center_section)}
      </div>
      
    <!-- Right section: Navigation links -->
      <div class="flex items-center space-x-4">
        {render_slot(@right_section)}

        <%= if @navigation_links != [] do %>
          <div class="flex items-center space-x-2">
            <%= for link <- @navigation_links do %>
              <.link
                navigate={link.path}
                class={[
                  "px-2 py-1 rounded text-sm font-mono transition-colors",
                  if(Map.get(link, :active, false),
                    do: "bg-green-500/20 text-green-300",
                    else: "text-green-400/70 hover:text-green-300 hover:bg-green-500/10"
                  )
                ]}
              >
                {link.label}
              </.link>
            <% end %>
          </div>
        <% end %>
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

  # Helper functions

  defp height_class(:small), do: "h-12"
  defp height_class(:medium), do: "h-14"
  defp height_class(:large), do: "h-16"
end
