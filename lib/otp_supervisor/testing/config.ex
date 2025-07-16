defmodule OTPSupervisor.Testing.Config do
  @moduledoc """
  Configuration management for distributed testing.

  Handles configuration validation, defaults, environment detection,
  and port management to ensure reliable distributed testing across
  different environments.
  """

  require Logger

  @default_config %{
    # Automatic cluster management
    auto_cluster: true,
    reuse_clusters: true,

    # Cluster sizing
    default_cluster_size: 2,
    max_cluster_size: 5,
    min_cluster_size: 1,

    # Timeouts (in milliseconds)
    cluster_startup_timeout: 30_000,
    cluster_cleanup_timeout: 10_000,
    cluster_health_timeout: 5_000,
    distributed_test_timeout: 60_000,

    # Port management
    # Different from dev (4000) to avoid conflicts
    http_port_base: 4200,
    # Different from dev (9100) to avoid conflicts
    dist_port_base: 9200,
    port_range_size: 100,

    # Environment detection
    # Auto-detect if nil
    ci_mode: nil,
    ci_cluster_size: 2,
    ci_timeout_multiplier: 2.0,
    ci_max_cluster_size: 3,

    # Development environment
    dev_cluster_size: 2,
    dev_timeout_multiplier: 1.0,

    # Debugging and logging
    verbose_cluster_logs: false,
    save_cluster_logs: false,
    debug_cluster_startup: false,

    # Test organization - NO BYPASSES ALLOWED
    # NEVER skip distributed tests
    skip_distributed_on_failure: false,
    # No retries - fail fast
    retry_cluster_startup: false,
    # No retries allowed
    max_startup_retries: 0,

    # Resource management
    cleanup_on_exit: true,
    force_cleanup_on_error: true,
    monitor_cluster_health: true
  }

  @required_keys [:auto_cluster, :default_cluster_size, :cluster_startup_timeout]

  @doc """
  Load and validate distributed testing configuration.

  Merges application config with runtime options and environment detection.
  """
  def load_config(runtime_opts \\ []) do
    base_config = Application.get_env(:otp_supervisor, :distributed_testing, [])

    @default_config
    |> Map.merge(Enum.into(base_config, %{}))
    |> Map.merge(Enum.into(runtime_opts, %{}))
    |> detect_environment()
    |> adjust_for_environment()
  end

  @doc """
  Get configuration value with fallback to default.
  """
  def get(key, default \\ nil) do
    config = load_config()
    Map.get(config, key, default)
  end

  @doc """
  Validate configuration and return errors if any.
  """
  def validate_config(config) do
    errors = []

    errors = validate_required_keys(config, errors)
    errors = validate_cluster_sizes(config, errors)
    errors = validate_timeouts(config, errors)
    errors = validate_ports(config, errors)

    case errors do
      [] ->
        config

      _ ->
        error_message =
          "Invalid distributed testing configuration:\n" <>
            Enum.join(errors, "\n")

        raise ArgumentError, error_message
    end
  end

  @doc """
  Detect current environment and return environment-specific settings.
  """
  def detect_environment(config \\ %{}) do
    ci_mode = detect_ci_environment(config)
    env = Application.get_env(:mix, :env)

    config
    |> Map.put(:ci_mode, ci_mode)
    |> Map.put(:mix_env, env)
    |> Map.put(:detected_environment, determine_environment_type(ci_mode, env))
  end

  @doc """
  Get port ranges for distributed testing to avoid conflicts.
  """
  def get_port_ranges(config \\ nil) do
    config = config || load_config()

    %{
      http: {
        config.http_port_base,
        config.http_port_base + config.port_range_size - 1
      },
      distribution: {
        config.dist_port_base,
        config.dist_port_base + config.port_range_size - 1
      }
    }
  end

  @doc """
  Check if ports are available in the configured ranges.
  """
  def check_port_availability(config \\ nil) do
    config = config || load_config()
    port_ranges = get_port_ranges(config)

    http_available = check_port_range_available(port_ranges.http)
    dist_available = check_port_range_available(port_ranges.distribution)

    %{
      http_ports_available: http_available,
      dist_ports_available: dist_available,
      all_available: http_available and dist_available
    }
  end

  @doc """
  Get environment-adjusted configuration for cluster operations.
  """
  def get_cluster_config(config \\ nil) do
    config = config || load_config()

    %{
      cluster_size: get_environment_cluster_size(config),
      startup_timeout: get_environment_timeout(config, :cluster_startup_timeout),
      cleanup_timeout: get_environment_timeout(config, :cluster_cleanup_timeout),
      health_timeout: get_environment_timeout(config, :cluster_health_timeout),
      max_retries: get_environment_retries(config),
      port_ranges: get_port_ranges(config)
    }
  end

  # Private implementation

  defp validate_required_keys(config, errors) do
    missing_keys =
      Enum.filter(@required_keys, fn key ->
        not Map.has_key?(config, key) or is_nil(Map.get(config, key))
      end)

    case missing_keys do
      [] -> errors
      keys -> ["Missing required configuration keys: #{inspect(keys)}" | errors]
    end
  end

  defp validate_cluster_sizes(config, errors) do
    errors =
      if config.default_cluster_size < 1 do
        ["default_cluster_size must be at least 1" | errors]
      else
        errors
      end

    errors =
      if config.max_cluster_size < config.default_cluster_size do
        ["max_cluster_size must be >= default_cluster_size" | errors]
      else
        errors
      end

    errors =
      if config.min_cluster_size < 1 do
        ["min_cluster_size must be at least 1" | errors]
      else
        errors
      end

    if config.ci_cluster_size < 1 do
      ["ci_cluster_size must be at least 1" | errors]
    else
      errors
    end
  end

  defp validate_timeouts(config, errors) do
    timeout_keys = [:cluster_startup_timeout, :cluster_cleanup_timeout, :cluster_health_timeout]

    Enum.reduce(timeout_keys, errors, fn key, acc ->
      value = Map.get(config, key)

      if is_integer(value) and value > 0 do
        acc
      else
        ["#{key} must be a positive integer (milliseconds)" | acc]
      end
    end)
  end

  defp validate_ports(config, errors) do
    errors =
      if config.http_port_base < 1024 or config.http_port_base > 65535 do
        ["http_port_base must be between 1024 and 65535" | errors]
      else
        errors
      end

    errors =
      if config.dist_port_base < 1024 or config.dist_port_base > 65535 do
        ["dist_port_base must be between 1024 and 65535" | errors]
      else
        errors
      end

    errors =
      if config.port_range_size < 10 or config.port_range_size > 1000 do
        ["port_range_size must be between 10 and 1000" | errors]
      else
        errors
      end

    # Check for port range overlap
    http_end = config.http_port_base + config.port_range_size - 1
    dist_start = config.dist_port_base
    dist_end = config.dist_port_base + config.port_range_size - 1

    if config.http_port_base <= dist_end and http_end >= dist_start do
      ["HTTP and distribution port ranges overlap" | errors]
    else
      errors
    end
  end

  defp detect_ci_environment(config) do
    case Map.get(config, :ci_mode) do
      nil ->
        # Auto-detect CI environment
        ci_indicators = [
          System.get_env("CI"),
          System.get_env("CONTINUOUS_INTEGRATION"),
          System.get_env("GITHUB_ACTIONS"),
          System.get_env("GITLAB_CI"),
          System.get_env("JENKINS_URL"),
          System.get_env("BUILDKITE"),
          System.get_env("CIRCLECI")
        ]

        Enum.any?(ci_indicators, fn indicator ->
          indicator != nil and indicator != "" and indicator != "false"
        end)

      explicit_value ->
        explicit_value
    end
  end

  defp determine_environment_type(ci_mode, mix_env) do
    cond do
      ci_mode -> :ci
      mix_env == :test -> :test
      mix_env == :dev -> :development
      true -> :production
    end
  end

  defp adjust_for_environment(config) do
    case config.detected_environment do
      :ci ->
        config
        |> Map.put(:default_cluster_size, config.ci_cluster_size)
        |> Map.put(:max_cluster_size, config.ci_max_cluster_size)
        |> adjust_timeouts_for_ci()
        # More logging in CI
        |> Map.put(:verbose_cluster_logs, true)

      :development ->
        config
        |> Map.put(:default_cluster_size, config.dev_cluster_size)
        |> adjust_timeouts_for_dev()

      _ ->
        config
    end
  end

  defp adjust_timeouts_for_ci(config) do
    multiplier = config.ci_timeout_multiplier

    config
    |> Map.put(:cluster_startup_timeout, round(config.cluster_startup_timeout * multiplier))
    |> Map.put(:cluster_cleanup_timeout, round(config.cluster_cleanup_timeout * multiplier))
    |> Map.put(:cluster_health_timeout, round(config.cluster_health_timeout * multiplier))
  end

  defp adjust_timeouts_for_dev(config) do
    multiplier = config.dev_timeout_multiplier

    config
    |> Map.put(:cluster_startup_timeout, round(config.cluster_startup_timeout * multiplier))
    |> Map.put(:cluster_cleanup_timeout, round(config.cluster_cleanup_timeout * multiplier))
    |> Map.put(:cluster_health_timeout, round(config.cluster_health_timeout * multiplier))
  end

  defp get_environment_cluster_size(config) do
    case config.detected_environment do
      :ci -> config.ci_cluster_size
      :development -> config.dev_cluster_size
      _ -> config.default_cluster_size
    end
  end

  defp get_environment_timeout(config, timeout_key) do
    # The timeout has already been adjusted during load_config/adjust_for_environment
    # so we just return the current value
    Map.get(config, timeout_key)
  end

  defp get_environment_retries(_config) do
    # NO RETRIES ALLOWED - fail fast for distributed tests
    0
  end

  defp check_port_range_available({start_port, end_port}) do
    # Check a few ports in the range to see if they're available
    sample_ports = [start_port, start_port + 10, start_port + 20, end_port - 10, end_port]

    Enum.all?(sample_ports, fn port ->
      is_port_available_retry(port, 3)
    end)
  end

  defp is_port_available_retry(port, _retries_left) do
    case :gen_tcp.listen(port, [{:reuseaddr, true}]) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        true

      {:error, :eaddrinuse} ->
        false

      {:error, _other} ->
        false
    end
  end
end
