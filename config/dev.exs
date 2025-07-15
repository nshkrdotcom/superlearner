import Config

# Configure your database
config :otp_supervisor, OtpSupervisor.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "otp_supervisor_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

# For development, we disable any cache and enable
# debugging and code reloading.
config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4000],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "a-very-long-secret-key-base-for-development-only-change-in-production",
  watchers: [
    esbuild: {Esbuild, :install_and_run, [:otp_supervisor, ~w(--watch)]},
    tailwind: {Tailwind, :install_and_run, [:otp_supervisor, ~w(--watch)]}
  ]

# Watch static and templates for browser reloading.
config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  live_reload: [
    patterns: [
      ~r"priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"priv/gettext/.*(po)$",
      ~r"lib/otp_supervisor_web/(controllers|live|components)/.*(ex|heex)$"
    ]
  ]

# Enable dev routes for dashboard and mailbox
config :otp_supervisor, dev_routes: true

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"

# Set a higher stacktrace during development. Avoid configuring such
# in production as building large stacktraces may be expensive.
config :phoenix, :stacktrace_depth, 20

# Initialize plugs at runtime for faster development compilation
config :phoenix, :plug_init_mode, :runtime

# Node 1 specific configuration
config :otp_supervisor, :node_name, :superlearner@U2401
config :otp_supervisor, :node_port, 4000
config :otp_supervisor, :node_role, :primary

# LibCluster configuration for development
config :libcluster,
  debug: true,
  topologies: [
    superlearner_cluster: [
      strategy: Cluster.Strategy.Epmd,
      config: [
        hosts: [
          :superlearner@U2401,
          :superlearner2@U2402
        ],
        polling_interval: 5_000
      ]
    ]
  ]

# Distributed tooling development settings
config :otp_supervisor, :distributed_tooling,
  # Start in single-node mode for development
  default_mode: :single_node,
  enabled: true,
  # More frequent checks in dev
  health_check_interval: 2_000,
  performance_monitoring: true
