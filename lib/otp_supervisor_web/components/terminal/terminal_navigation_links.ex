defmodule OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks do
  use Phoenix.Component

  @moduledoc """
  Consistent terminal-themed navigation links across all pages.

  Provides standardized navigation styling and behavior.
  """

  attr :current_page, :string, required: true
  attr :links, :list, default: []
  attr :style, :atom, values: [:horizontal, :vertical], default: :horizontal
  attr :size, :atom, values: [:small, :medium, :large], default: :medium
  attr :show_icons, :boolean, default: true
  attr :class, :string, default: ""

  slot :custom_link

  def render(assigns) do
    ~H"""
    <nav class={[
      navigation_container_classes(@style),
      @class
    ]}>
      <%= if @links != [] do %>
        <%= for link <- @links do %>
          <.link
            navigate={link.path}
            class={[
              link_classes(@size),
              if(link_active?(link, @current_page),
                do: active_link_classes(),
                else: inactive_link_classes()
              )
            ]}
          >
            <%= if @show_icons and link[:icon] do %>
              <span class="flex-shrink-0">{link.icon}</span>
            <% end %>
            <span>{link.label}</span>
            <%= if link[:badge] do %>
              <span class={[
                badge_classes(),
                badge_color_classes(link.badge)
              ]}>
                {link.badge.value}
              </span>
            <% end %>
          </.link>
        <% end %>
      <% end %>

      <%= for custom_link <- @custom_link do %>
        {render_slot(custom_link)}
      <% end %>
    </nav>
    """
  end

  # Default navigation links for all pages
  def default_navigation_links do
    [
      %{
        label: "Docs",
        path: "/docs",
        icon: "📚",
        key: "docs"
      },
      %{
        label: "Dashboard",
        path: "/system",
        icon: "📊",
        key: "dashboard"
      },
      %{
        label: "Supervisor",
        path: "/supervisors",
        icon: "🎯",
        key: "supervisor"
      },
      %{
        label: "Cluster",
        path: "/cluster",
        icon: "🔗",
        key: "cluster"
      },
      %{
        label: "Processes",
        path: "/cluster-processes",
        icon: "⚙️",
        key: "cluster-processes"
      },
      %{
        label: "Visualization",
        path: "/cluster-visualization",
        icon: "🌐",
        key: "cluster-visualization"
      },
      %{
        label: "Sandbox",
        path: "/sandbox",
        icon: "📦",
        key: "sandbox"
      },
      %{
        label: "Arsenal",
        path: "/arsenal",
        icon: "⚡",
        key: "arsenal"
      }
    ]
  end

  # Page-specific navigation with context
  def page_navigation_links(page, context \\ %{}) do
    base_links = default_navigation_links()

    # Add active status to base links
    base_links_with_active =
      Enum.map(base_links, fn link ->
        Map.put(link, :active, link_active?(link, page))
      end)

    case page do
      "docs" ->
        add_docs_navigation(base_links_with_active, context)

      "dashboard" ->
        add_dashboard_navigation(base_links_with_active, context)

      "supervisor" ->
        add_supervisor_navigation(base_links_with_active, context)

      "cluster" ->
        add_cluster_navigation(base_links_with_active, context)

      "arsenal" ->
        add_arsenal_navigation(base_links_with_active, context)

      "sandbox" ->
        add_sandbox_navigation(base_links_with_active, context)

      _ ->
        base_links_with_active
    end
  end

  # Private functions

  defp navigation_container_classes(:horizontal) do
    "flex items-center space-x-2"
  end

  defp navigation_container_classes(:vertical) do
    "flex flex-col space-y-1"
  end

  defp link_classes(:small) do
    "inline-flex items-center space-x-1 px-2 py-1 rounded text-xs font-mono transition-colors"
  end

  defp link_classes(:medium) do
    "inline-flex items-center space-x-2 px-3 py-2 rounded text-sm font-mono transition-colors"
  end

  defp link_classes(:large) do
    "inline-flex items-center space-x-3 px-4 py-3 rounded text-base font-mono transition-colors"
  end

  defp active_link_classes do
    "bg-green-500/20 text-green-300 border border-green-500/30"
  end

  defp inactive_link_classes do
    "text-green-400/70 hover:text-green-300 hover:bg-green-500/10 border border-transparent hover:border-green-500/20"
  end

  defp badge_classes do
    "inline-flex items-center justify-center min-w-[1.25rem] h-5 px-1 rounded-full text-xs font-mono font-bold"
  end

  defp badge_color_classes(%{type: :error}), do: "bg-red-500/20 text-red-400"
  defp badge_color_classes(%{type: :warning}), do: "bg-yellow-500/20 text-yellow-400"
  defp badge_color_classes(%{type: :success}), do: "bg-green-500/20 text-green-400"
  defp badge_color_classes(%{type: :info}), do: "bg-blue-500/20 text-blue-400"
  defp badge_color_classes(_), do: "bg-green-500/20 text-green-400"

  defp link_active?(link, current_page) do
    case {link.key, current_page} do
      {"dashboard", "dashboard"} ->
        true

      {"supervisor", "supervisor"} ->
        true

      {"cluster", "cluster"} ->
        true

      {"cluster-processes", "cluster-processes"} ->
        true

      {"cluster-visualization", "cluster-visualization"} ->
        true

      {"docs", "docs"} ->
        true

      {"arsenal", "arsenal"} ->
        true

      {"sandbox", "sandbox"} ->
        true

      _ ->
        link.key == current_page || link.path == current_page
    end
  end

  # Page-specific navigation helpers

  defp add_docs_navigation(links, _context) do
    # For now, docs page handles its own internal navigation
    # No additional external routes needed
    links
  end

  defp add_dashboard_navigation(links, _context) do
    # Dashboard page handles its own internal navigation
    # No additional external routes needed
    links
  end

  defp add_supervisor_navigation(links, _context) do
    # Supervisor page handles its own internal navigation
    # No additional external routes needed
    links
  end

  defp add_cluster_navigation(links, _context) do
    # Cluster page handles its own internal navigation
    # No additional external routes needed
    links
  end

  defp add_arsenal_navigation(links, _context) do
    # Arsenal page handles its own internal navigation
    # No additional external routes needed
    links
  end

  defp add_sandbox_navigation(links, _context) do
    # Sandbox page handles its own internal navigation
    # No additional external routes needed
    links
  end
end
