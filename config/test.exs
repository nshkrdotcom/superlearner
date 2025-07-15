import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :otp_supervisor, OtpSupervisorWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "JWFENV7jngH1KXFq/RtK6g0vTMOOF3rpWtcM4awQATQqC9uazWtCpCjcbuc5+gFP",
  server: false

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
