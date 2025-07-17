defmodule OtpSupervisor.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    # Install custom alarm handler to filter snap mount warnings
    install_alarm_filter()

    children = [
      OtpSupervisorWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:otp_supervisor, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: OtpSupervisor.PubSub},
      # Start the Finch HTTP client for sending emails
      {Finch, name: OtpSupervisor.Finch},
      # Start the TracerRegistry for message tracing
      {Registry, keys: :unique, name: TracerRegistry},

      # Analytics and sandbox management
      OTPSupervisor.Core.AnalyticsServer,
      {OTPSupervisor.Core.ModuleVersionManager, []},
      {OTPSupervisor.Core.SandboxManager, []},

      # Arsenal operation registry
      OTPSupervisor.Core.Arsenal.Registry,

      # Distributed tooling
      OTPSupervisor.Distributed.ToolManager,
      OTPSupervisor.Distributed.SingleNodeSimulator,
      OTPSupervisor.Distributed.ClusterStateManager,

      # Dynamic supervisor for test cluster servers
      {DynamicSupervisor, name: OtpSupervisor.DynamicSupervisor, strategy: :one_for_one},

      # Test cluster management (only in test environment)
      maybe_test_cluster_manager(),
      maybe_auto_cluster_manager(),

      # Start a worker by calling: OtpSupervisor.Worker.start_link(arg)
      # {OtpSupervisor.Worker, arg},
      # Start to serve requests, typically the last entry
      OtpSupervisorWeb.Endpoint
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

  # Private helper functions

  defp maybe_test_cluster_manager do
    if distributed_testing_enabled?() do
      OTPSupervisor.TestCluster.Manager
    else
      # Return a no-op child spec for non-test environments
      %{
        id: :test_cluster_manager_noop,
        start: {Task, :start_link, [fn -> :ok end]},
        restart: :temporary
      }
    end
  end

  defp maybe_auto_cluster_manager do
    if distributed_testing_enabled?() do
      OTPSupervisor.Testing.AutoClusterManager
    else
      # Return a no-op child spec for non-test environments
      %{
        id: :auto_cluster_manager_noop,
        start: {Task, :start_link, [fn -> :ok end]},
        restart: :temporary
      }
    end
  end

  defp distributed_testing_enabled? do
    # Check if distributed testing configuration is present
    config = Application.get_env(:otp_supervisor, :distributed_testing)
    is_list(config) && Keyword.get(config, :auto_cluster, false)
  end

  defp install_alarm_filter do
    # Add logger filter to suppress snap mount disk alarms
    :logger.add_primary_filter(
      :snap_alarm_filter,
      {&OtpSupervisor.Core.SnapAlarmFilter.filter/2, []}
    )
  end
end
