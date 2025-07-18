# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

config :otp_supervisor,
  generators: [timestamp_type: :utc_datetime]

# Configures the endpoint
config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [html: OtpSupervisorWeb.ErrorHTML, json: OtpSupervisorWeb.ErrorJSON],
    layout: {OtpSupervisorWeb.Layouts, :root}
  ],
  pubsub_server: OtpSupervisor.PubSub,
  live_view: [signing_salt: "UpqPq+A8"]

# Configures the mailer
#
# By default it uses the "Local" adapter which stores the emails
# locally. You can see the emails in your browser, at "/dev/mailbox".
#
# For production it's recommended to configure a different adapter
# at the `config/runtime.exs`.
config :otp_supervisor, OtpSupervisor.Mailer, adapter: Swoosh.Adapters.Local

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.17.11",
  otp_supervisor: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Configure tailwind (the version is required)
config :tailwind,
  version: "3.4.3",
  otp_supervisor: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
      --minify
    ),
    cd: Path.expand("../assets", __DIR__)
  ]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Handle logger errors gracefully
config :logger,
  handle_otp_reports: true,
  handle_sasl_reports: false,
  compile_time_purge_matching: [
    [level_lower_than: :info]
  ]

# Configure OS_Mon to disable disk monitoring and adjust memory threshold
# (to avoid snap mount and memory warnings)
config :os_mon,
  start_disksup: false,
  system_memory_high_watermark: 0.90

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Distributed tooling configuration
config :otp_supervisor, :distributed_tooling,
  # Default mode - will be auto-detected at startup
  default_mode: :auto,
  # Enable/disable distributed features
  enabled: true,
  # Health monitoring settings
  health_check_interval: 5_000,
  # Performance monitoring settings
  performance_monitoring: true

# Process listing configuration
config :otp_supervisor, :process_listing,
  # Default limit for process queries
  default_limit: 1000,
  # Maximum allowed limit for process queries
  max_limit: 10000,
  # Per-page limit for UI display
  per_page: 100

# LibCluster configuration (will be overridden in env-specific configs)
config :libcluster,
  debug: false,
  topologies: []

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
