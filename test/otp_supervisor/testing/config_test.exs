defmodule OTPSupervisor.Testing.ConfigTest do
  use ExUnit.Case, async: true
  
  alias OTPSupervisor.Testing.Config
  
  describe "load_config/1" do
    test "loads default configuration" do
      config = Config.load_config()
      
      assert config.auto_cluster == true
      assert config.default_cluster_size == 2
      assert config.cluster_startup_timeout == 30_000
      assert config.http_port_base == 4200
      assert config.dist_port_base == 9200
    end
    
    test "merges runtime options" do
      config = Config.load_config(default_cluster_size: 4, auto_cluster: false)
      
      assert config.default_cluster_size == 4
      assert config.auto_cluster == false
      # Other defaults should remain
      assert config.cluster_startup_timeout == 30_000
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
      invalid_config = base_config
                      |> Map.put(:http_port_base, 9200)
                      |> Map.put(:dist_port_base, 9200)
      
      assert_raise ArgumentError, ~r/HTTP and distribution port ranges overlap/, fn ->
        Config.validate_config(invalid_config)
      end
    end
  end
  
  describe "get_port_ranges/1" do
    test "returns correct port ranges" do
      config = Config.load_config(
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
      config = Config.load_config(
        default_cluster_size: 3,
        cluster_startup_timeout: 20_000
      )
      
      cluster_config = Config.get_cluster_config(config)
      
      assert cluster_config.cluster_size == 3
      assert cluster_config.startup_timeout == 20_000
      assert is_map(cluster_config.port_ranges)
    end
    
    test "adjusts configuration for CI environment" do
      # Simulate CI
      original_ci = System.get_env("CI")
      System.put_env("CI", "true")
      
      try do
        config = Config.load_config(
          ci_cluster_size: 2,
          ci_timeout_multiplier: 3.0,
          cluster_startup_timeout: 10_000
        )
        
        cluster_config = Config.get_cluster_config(config)
        
        assert cluster_config.cluster_size == 2  # CI cluster size
        # The timeout should be adjusted by the CI multiplier during load_config
        assert cluster_config.startup_timeout == 30_000  # 10_000 * 3.0
      after
        # Restore environment
        case original_ci do
          nil -> System.delete_env("CI")
          value -> System.put_env("CI", value)
        end
      end
    end
  end
end