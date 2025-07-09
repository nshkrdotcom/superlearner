defmodule OtpSandbox.Application do
  @moduledoc """
  The OTP Sandbox application.

  This application provides a minimal supervision tree for the educational
  OTP sandbox components.
  """

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Add child processes here if needed
      # For now, just start an empty supervisor
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: OtpSandbox.Supervisor]
    Supervisor.start_link(children, opts)
  end
end