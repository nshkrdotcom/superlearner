import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: String.to_integer(System.get_env("TEST_HTTP_PORT") || "4002")],
  secret_key_base: "JWFENV7jngH1KXFq/RtK6g0vTMOOF3rpWtcM4awQATQqC9uazWtCpCjcbuc5+gFP",
  server: String.to_existing_atom(System.get_env("PHX_SERVER") || "false")

# In test we don't send emails
config :otp_supervisor, OtpSupervisor.Mailer, adapter: Swoosh.Adapters.Test

# Disable swoosh api client as it is only required for production adapters
config :swoosh, :api_client, false

# Print only warnings and above during test (suppress error logs from intentional crashes)
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Enable helpful, but potentially expensive runtime checks
config :phoenix_live_view,
  enable_expensive_runtime_checks: true

# LibCluster configuration for distributed testing
config :libcluster,
  topologies: [
    test_cluster: [
      strategy: Cluster.Strategy.Epmd,
      config: [
        hosts: [
          :"test_primary@127.0.0.1",
          :"test_secondary@127.0.0.1",
          :"test_1@127.0.0.1",
          :"test_2@127.0.0.1"
        ]
      ]
    ]
  ]

# Swoosh configuration for distributed testing (avoid name conflicts)
config :otp_supervisor, OtpSupervisor.Mailer,
  adapter: Swoosh.Adapters.Test,
  # Use unique names per node to avoid conflicts
  local_storage: {:otp_supervisor, :test_mailer_storage}

# Distributed Testing Configuration
config :otp_supervisor, :distributed_testing,
  # Automatic cluster management
  auto_cluster: true,
  reuse_clusters: true,

  # Cluster sizing
  default_cluster_size: 2,
  max_cluster_size: 5,
  min_cluster_size: 1,

  # Environment-specific cluster sizes
  # Smaller clusters in CI to conserve resources
  ci_cluster_size: 2,
  # Development cluster size
  dev_cluster_size: 2,

  # Timeouts (in milliseconds)
  # 30 seconds
  cluster_startup_timeout: 30_000,
  # 10 seconds
  cluster_cleanup_timeout: 10_000,
  # 5 seconds
  cluster_health_timeout: 5_000,
  # 1 minute per distributed test
  distributed_test_timeout: 60_000,

  # Port management (different from dev to avoid conflicts)
  # Dev uses 4000, we use 4200+ for tests
  http_port_base: 4200,
  # Dev uses 9100, we use 9200+ for tests
  dist_port_base: 9200,
  # Allow 100 ports in each range
  port_range_size: 100,

  # CI/CD optimizations
  # ci_mode: auto-detected at runtime
  # Double timeouts in CI
  ci_timeout_multiplier: 2.0,
  # Limit cluster size in CI
  ci_max_cluster_size: 3,

  # Development optimizations
  # Normal timeouts in dev
  dev_timeout_multiplier: 1.0,

  # Debugging and logging
  verbose_cluster_logs: false,
  save_cluster_logs: false,
  debug_cluster_startup: false,

  # Error handling and recovery - NO BYPASSES ALLOWED
  # NEVER skip distributed tests
  skip_distributed_on_failure: false,
  # No retries - fail fast
  retry_cluster_startup: false,
  # No retries allowed
  max_startup_retries: 0,

  # Resource management
  # Always cleanup clusters on exit
  cleanup_on_exit: true,
  # Force cleanup even on errors
  force_cleanup_on_error: true,
  # Monitor cluster health during tests
  monitor_cluster_health: true
