defmodule OTPSupervisor.Testing.ConfigTest do
  use ExUnit.Case, async: true

  alias OTPSupervisor.Testing.Config

  describe "load_config/1" do
    test "loads default configuration" do
      # Clear CI environment for this test
      original_ci = System.get_env("CI")
      System.delete_env("CI")

      try do
        config = Config.load_config()

        assert config.auto_cluster == true
        assert config.default_cluster_size == 2
        assert config.cluster_startup_timeout == 30_000
        assert config.http_port_base == 4200
        assert config.dist_port_base == 9200
      after
        case original_ci do
          nil -> :ok
          value -> System.put_env("CI", value)
        end
      end
    end

    test "merges runtime options" do
      # Clear CI environment for this test
      original_ci = System.get_env("CI")
      System.delete_env("CI")

      try do
        config = Config.load_config(default_cluster_size: 4, auto_cluster: false)

        assert config.default_cluster_size == 4
        assert config.auto_cluster == false
        # Other defaults should remain
        assert config.cluster_startup_timeout == 30_000
      after
        case original_ci do
          nil -> :ok
          value -> System.put_env("CI", value)
        end
      end
    end

    test "detects CI environment" do
      # Simulate CI environment
      original_ci = System.get_env("CI")
      System.put_env("CI", "true")

      try do
        config = Config.load_config()

        assert config.ci_mode == true
        assert config.detected_environment == :ci
      after
        # Restore original environment
        case original_ci do
          nil -> System.delete_env("CI")
          value -> System.put_env("CI", value)
        end
      end
    end
  end

  describe "validate_config/1" do
    test "validates valid configuration" do
      config = Config.load_config()

      # Should not raise
      validated_config = Config.validate_config(config)
      assert validated_config == config
    end

    test "raises on invalid cluster size" do
      # Load base config and manually set invalid value
      base_config = Config.load_config()
      invalid_config = Map.put(base_config, :default_cluster_size, 0)

      assert_raise ArgumentError, ~r/default_cluster_size must be at least 1/, fn ->
        Config.validate_config(invalid_config)
      end
    end

    test "raises on invalid timeout" do
      # Load base config and manually set invalid value
      base_config = Config.load_config()
      invalid_config = Map.put(base_config, :cluster_startup_timeout, -1000)

      assert_raise ArgumentError, ~r/cluster_startup_timeout must be a positive integer/, fn ->
        Config.validate_config(invalid_config)
      end
    end

    test "raises on port range overlap" do
      # Load base config and manually set overlapping ports
      base_config = Config.load_config()

      invalid_config =
        base_config
        |> Map.put(:http_port_base, 9200)
        |> Map.put(:dist_port_base, 9200)

      assert_raise ArgumentError, ~r/HTTP and distribution port ranges overlap/, fn ->
        Config.validate_config(invalid_config)
      end
    end
  end

  describe "get_port_ranges/1" do
    test "returns correct port ranges" do
      config =
        Config.load_config(
          http_port_base: 5000,
          dist_port_base: 6000,
          port_range_size: 50
        )

      ranges = Config.get_port_ranges(config)

      assert ranges.http == {5000, 5049}
      assert ranges.distribution == {6000, 6049}
    end
  end

  describe "environment detection" do
    test "detects test environment" do
      # Ensure no CI environment variables
      original_ci = System.get_env("CI")
      System.delete_env("CI")

      try do
        config = Config.detect_environment()

        assert config.ci_mode == false
        # In test environment, it should detect :test
        assert config.detected_environment == :test
      after
        # Restore original environment
        case original_ci do
          nil -> :ok
          value -> System.put_env("CI", value)
        end
      end
    end

    test "detects CI environment in actual CI" do
      # Only run this assertion if we're actually in CI
      if System.get_env("CI") do
        config = Config.detect_environment()
        assert config.ci_mode == true
        assert config.detected_environment == :ci
      end
    end

    test "detects CI environment from various indicators" do
      ci_vars = ["CI", "CONTINUOUS_INTEGRATION", "GITHUB_ACTIONS", "GITLAB_CI"]

      Enum.each(ci_vars, fn var ->
        # Clear all CI vars first
        Enum.each(ci_vars, &System.delete_env/1)

        # Set the specific CI var
        System.put_env(var, "true")

        config = Config.detect_environment()

        assert config.ci_mode == true, "Failed to detect CI from #{var}"
        assert config.detected_environment == :ci

        # Clean up
        System.delete_env(var)
      end)
    end
  end

  describe "get_cluster_config/1" do
    test "returns environment-adjusted cluster configuration" do
      # Clear CI environment for this test
      original_ci = System.get_env("CI")
      System.delete_env("CI")

      try do
        config =
          Config.load_config(
            default_cluster_size: 3,
            cluster_startup_timeout: 20_000
          )

        cluster_config = Config.get_cluster_config(config)

        assert cluster_config.cluster_size == 3
        assert cluster_config.startup_timeout == 20_000
        assert is_map(cluster_config.port_ranges)
      after
        case original_ci do
          nil -> :ok
          value -> System.put_env("CI", value)
        end
      end
    end

    test "adjusts configuration for CI environment" do
      # Simulate CI
      original_ci = System.get_env("CI")
      System.put_env("CI", "true")

      try do
        config =
          Config.load_config(
            ci_cluster_size: 2,
            ci_timeout_multiplier: 3.0,
            cluster_startup_timeout: 10_000
          )

        cluster_config = Config.get_cluster_config(config)

        # CI cluster size
        assert cluster_config.cluster_size == 2
        # The timeout should be adjusted by the CI multiplier during load_config
        # 10_000 * 3.0
        assert cluster_config.startup_timeout == 30_000
      after
        # Restore environment
        case original_ci do
          nil -> System.delete_env("CI")
          value -> System.put_env("CI", value)
        end
      end
    end
  end

  describe "configuration with live cluster scenarios" do
    test "configuration is compatible with AutoClusterManager" do
      config =
        Config.load_config(
          default_cluster_size: 2,
          cluster_startup_timeout: 15_000,
          auto_cluster: true
        )

      # Verify configuration can be used by AutoClusterManager
      cluster_config = Config.get_cluster_config(config)

      # These values should be suitable for cluster operations
      assert cluster_config.cluster_size >= 1
      assert cluster_config.startup_timeout > 0
      assert is_map(cluster_config.port_ranges)

      # Port ranges should not overlap
      http_range = cluster_config.port_ranges.http
      dist_range = cluster_config.port_ranges.distribution

      {http_start, http_end} = http_range
      {dist_start, dist_end} = dist_range

      # Ensure no overlap
      refute http_start <= dist_end and dist_start <= http_end
    end

    test "CI configuration produces smaller, faster clusters" do
      # Simulate CI environment
      original_ci = System.get_env("CI")
      System.put_env("CI", "true")

      try do
        ci_config =
          Config.load_config(
            # Large default
            default_cluster_size: 4,
            # Smaller for CI
            ci_cluster_size: 2,
            cluster_startup_timeout: 30_000,
            # Faster timeouts in CI
            ci_timeout_multiplier: 0.5
          )

        cluster_config = Config.get_cluster_config(ci_config)

        # Should use CI-specific values
        assert cluster_config.cluster_size == 2
        # 30_000 * 0.5
        assert cluster_config.startup_timeout == 15_000

        # Configuration should be valid for constrained environments
        # Reasonable for CI
        assert cluster_config.cluster_size <= 3
        # Not too long for CI
        assert cluster_config.startup_timeout <= 30_000
      after
        case original_ci do
          nil -> System.delete_env("CI")
          value -> System.put_env("CI", value)
        end
      end
    end

    test "port configuration avoids conflicts with development servers" do
      config =
        Config.load_config(
          # Different from Phoenix default (4000)
          http_port_base: 4200,
          # Different from common dev ports
          dist_port_base: 9200
        )

      cluster_config = Config.get_cluster_config(config)
      port_ranges = cluster_config.port_ranges

      # Should not conflict with common development ports
      {http_start, http_end} = port_ranges.http
      {dist_start, dist_end} = port_ranges.distribution

      # Should not use Phoenix default port range (4000-4099)
      refute http_start <= 4099 and 4000 <= http_end

      # Should not use common database ports (5432 PostgreSQL, 3306 MySQL)
      refute http_start <= 5432 and 5432 <= http_end
      refute dist_start <= 5432 and 5432 <= dist_end
      refute http_start <= 3306 and 3306 <= http_end
      refute dist_start <= 3306 and 3306 <= dist_end
    end

    test "configuration validation catches real-world issues" do
      # Test configuration that might cause real cluster problems

      # Port ranges too small
      assert_raise ArgumentError, ~r/port_range_size must be between/, fn ->
        Config.load_config(port_range_size: 1) |> Config.validate_config()
      end

      # Timeouts too short for real clusters
      assert_raise ArgumentError, ~r/cluster_startup_timeout must be a positive integer/, fn ->
        Config.load_config(cluster_startup_timeout: -100) |> Config.validate_config()
      end

      # Cluster size too large for most systems
      assert_raise ArgumentError, ~r/max_cluster_size must be >= default_cluster_size/, fn ->
        # Clear CI environment to avoid interference
        original_ci = System.get_env("CI")
        System.delete_env("CI")

        try do
          Config.load_config(default_cluster_size: 50, max_cluster_size: 10)
          |> Config.validate_config()
        after
          case original_ci do
            nil -> :ok
            value -> System.put_env("CI", value)
          end
        end
      end
    end

    test "configuration adapts to system constraints" do
      # Test that configuration can adapt to different system capabilities
      base_config = Config.load_config()

      # Should provide reasonable defaults
      assert base_config.default_cluster_size >= 2
      # Reasonable for most systems
      assert base_config.default_cluster_size <= 5
      # At least 10 seconds
      assert base_config.cluster_startup_timeout >= 10_000
      # Not more than 1 minute
      assert base_config.cluster_startup_timeout <= 60_000

      # Port ranges should be reasonable
      port_ranges = Config.get_port_ranges(base_config)
      {http_start, http_end} = port_ranges.http
      {dist_start, dist_end} = port_ranges.distribution

      # Should have enough ports for multiple clusters
      assert http_end - http_start + 1 >= 10
      assert dist_end - dist_start + 1 >= 10

      # Should use high-numbered ports to avoid conflicts
      assert http_start >= 4000
      assert dist_start >= 9000
    end
  end
end
