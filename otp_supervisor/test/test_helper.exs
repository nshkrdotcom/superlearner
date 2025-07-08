# Configure logger to reduce noise during tests
Logger.configure(level: :warning)

# Note: GenServer crash reports come from OTP's error_logger and aren't easily suppressed
# We use capture_log/1 in specific tests where we expect crashes

ExUnit.start()
