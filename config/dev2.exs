import Config

# Import base development configuration
import_config "dev.exs"

# Node 2 specific overrides
config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4010],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "a-very-long-secret-key-base-for-development-only-change-in-production"

# Node 2 specific configuration  
config :otp_supervisor, :node_name, :superlearner2@localhost
config :otp_supervisor, :node_port, 4010
config :otp_supervisor, :node_role, :secondary

# Database configuration - use same database but different pool name to avoid conflicts
config :otp_supervisor, OtpSupervisor.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "otp_supervisor_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10
