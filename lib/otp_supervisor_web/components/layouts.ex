defmodule OtpSupervisorWeb.Components.Layouts do
  use Phoenix.Component

  import Phoenix.Controller, only: [get_csrf_token: 0]

  @moduledoc """
  New layouts for the LiveComponent-based pages.
  """

  attr :page_title, :string, default: "OTP Supervisor"

  def root(assigns) do
    ~H"""
    <!DOCTYPE html>
    <html lang="en" class="h-full bg-gray-900">
      <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <meta name="csrf-token" content={get_csrf_token()} />
        <.live_title>
          {assigns[:page_title] || "OTP Supervisor"}
        </.live_title>
        <link phx-track-static rel="stylesheet" href="/assets/app.css" />
        <script defer phx-track-static type="text/javascript" src="/assets/app.js">
        </script>
      </head>
      <body class="h-full bg-gray-900 text-green-400 font-mono">
        {@inner_content}
      </body>
    </html>
    """
  end

  def app(assigns) do
    ~H"""
    <main class="h-full">
      <.flash_group flash={@flash} />
      {@inner_content}
    </main>
    """
  end

  defp flash_group(assigns) do
    ~H"""
    <div class="fixed top-4 right-4 z-50 space-y-2">
      <.flash
        :if={Phoenix.Flash.get(@flash, :info)}
        id="flash-info"
        kind={:info}
        autoshow={true}
        close={false}
      >
        {Phoenix.Flash.get(@flash, :info)}
      </.flash>
      <.flash
        :if={Phoenix.Flash.get(@flash, :error)}
        id="flash-error"
        kind={:error}
        autoshow={true}
        close={false}
      >
        {Phoenix.Flash.get(@flash, :error)}
      </.flash>
    </div>
    """
  end

  def flash(assigns) do
    ~H"""
    <div
      id={@id}
      class={[
        "fixed top-4 right-4 z-50 rounded-lg border p-4 shadow-lg",
        "transition-all duration-300",
        flash_classes(@kind)
      ]}
      phx-mounted={@autoshow && show("##{@id}")}
    >
      <div class="flex items-center space-x-2">
        <span class="text-sm font-mono">
          {render_slot(@inner_block)}
        </span>
        <%= if @close do %>
          <button type="button" class="ml-2 text-sm hover:opacity-75" phx-click={hide("##{@id}")}>
            ✕
          </button>
        <% end %>
      </div>
    </div>
    """
  end

  defp flash_classes(:info) do
    "bg-blue-500/20 border-blue-500/50 text-blue-300"
  end

  defp flash_classes(:error) do
    "bg-red-500/20 border-red-500/50 text-red-300"
  end

  defp show(js \\ %Phoenix.LiveView.JS{}, selector) do
    Phoenix.LiveView.JS.show(js, to: selector)
  end

  defp hide(js \\ %Phoenix.LiveView.JS{}, selector) do
    Phoenix.LiveView.JS.hide(js, to: selector)
  end
end
