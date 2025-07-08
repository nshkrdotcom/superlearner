defmodule OtpSupervisor.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      OtpSupervisorWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:otp_supervisor, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: OtpSupervisor.PubSub},
      # Start the Finch HTTP client for sending emails
      {Finch, name: OtpSupervisor.Finch},
      # Start the TracerRegistry for message tracing
      {Registry, keys: :unique, name: TracerRegistry},

      # Add AnalyticsServer to collect supervisor events
      OTPSupervisor.Core.AnalyticsServer,

      # Start a worker by calling: OtpSupervisor.Worker.start_link(arg)
      # {OtpSupervisor.Worker, arg},
      # Start to serve requests, typically the last entry
      OtpSupervisorWeb.Endpoint,
      # Start our demo supervisor
      {OTPSupervisor.Sandbox.Supervisors.DemoSupervisor,
       name: :demo_one_for_one, strategy: :one_for_one}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: OtpSupervisor.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    OtpSupervisorWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
