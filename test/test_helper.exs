# Configure logger to reduce noise during tests
Logger.configure(level: :warning)

# Note: GenServer crash reports come from OTP's error_logger and aren't easily suppressed
# We use capture_log/1 in specific tests where we expect crashes

# CRITICAL: Start distributed Erlang for distributed tests
# This addresses the :not_alive errors in distributed test infrastructure
unless Node.alive?() do
  # Try to start distributed Erlang with a unique name
  node_name = :"test_primary_#{System.system_time(:millisecond)}@127.0.0.1"

  case Node.start(node_name, :shortnames) do
    {:ok, _} ->
      Node.set_cookie(:test_cluster_cookie)
      IO.puts("Started distributed Erlang for testing: #{Node.self()}")

    {:error, {:already_started, _}} ->
      Node.set_cookie(:test_cluster_cookie)
      IO.puts("Distributed Erlang already started: #{Node.self()}")

    {:error, reason} ->
      IO.puts("Warning: Could not start distributed Erlang: #{inspect(reason)}")
      IO.puts("Distributed tests will be skipped")
      IO.puts("This is usually due to network configuration issues or EPMD not running")
      IO.puts("Try: epmd -daemon")
  end
else
  Node.set_cookie(:test_cluster_cookie)
  IO.puts("Using existing distributed node: #{Node.self()}")
end

# Ensure EPMD is running for distributed tests
case System.cmd("epmd", ["-daemon"], stderr_to_stdout: true) do
  {_, 0} ->
    :ok

  {output, _} ->
    if String.contains?(output, "already running") do
      :ok
    else
      IO.puts("Warning: EPMD may not be available: #{output}")
    end
end

# Exclude UI tests by default - run with --include ui to include them
ExUnit.start(exclude: [:ui], async: true)
