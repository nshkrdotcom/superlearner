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

# Node 2 configuration - port 4010, no watchers
config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4010],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "a-very-long-secret-key-base-for-development-only-change-in-production",
  watchers: []

# Disable live reload for node 2
config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  live_reload: []

# Enable dev routes for dashboard and mailbox
config :otp_supervisor, dev_routes: true

# Do not include metadata nor timestamps in development logs
config :logger, :console, format: "[$level] $message\n"

# Set a higher stacktrace during development. Avoid configuring such
# in production as building large stacktraces may be expensive.
config :phoenix, :stacktrace_depth, 20

# Initialize plugs at runtime for faster development compilation
config :phoenix, :plug_init_mode, :runtime

# Node 2 specific configuration  
config :otp_supervisor, :node_name, :superlearner2@localhost
config :otp_supervisor, :node_port, 4010
config :otp_supervisor, :node_role, :secondary