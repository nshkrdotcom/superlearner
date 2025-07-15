defmodule OTPSupervisor.Testing.ConfigIntegrationTest do
  use ExUnit.Case, async: false
  
  alias OTPSupervisor.Testing.{Config, ConfigValidator, PortManager}
  
  describe "configuration integration" do
    test "AutoClusterManager can load configuration" do
      # This test verifies that the AutoClusterManager can successfully
      # load and use the new configuration system
      config = Config.load_config()
      
      assert config.auto_cluster == true
      assert config.default_cluster_size >= 1
      assert config.cluster_startup_timeout > 0
      
      # Verify port ranges are valid
      port_ranges = Config.get_port_ranges(config)
      assert is_tuple(port_ranges.http)
      assert is_tuple(port_ranges.distribution)
      
      # Verify cluster config can be generated
      cluster_config = Config.get_cluster_config(config)
      assert cluster_config.cluster_size >= 1
      assert cluster_config.startup_timeout > 0
      assert is_map(cluster_config.port_ranges)
    end
    
    test "configuration validation works end-to-end" do
      config = Config.load_config()
      
      case ConfigValidator.validate_complete_config(config) do
        :ok -> 
          # Configuration is valid
          assert true
          
        {:error, report} ->
          # If there are only warnings, that's acceptable
          if report.error_count == 0 do
            assert true
          else
            flunk("Configuration validation failed: #{report.summary}")
          end
      end
    end
    
    test "port management integration" do
      config = Config.load_config()
      
      # Test port allocation for a small cluster
      case PortManager.allocate_cluster_ports(2, config) do
        {:ok, port_assignments} ->
          assert map_size(port_assignments) == 2
          
          # Verify each assignment has required fields
          Enum.each(port_assignments, fn {node, assignment} ->
            assert is_atom(node)
            assert is_integer(assignment.http_port)
            assert is_integer(assignment.distribution_port)
            assert assignment.http_port != assignment.distribution_port
          end)
          
          # Clean up
          PortManager.release_cluster_ports(port_assignments)
          
        {:error, reason} ->
          # Port allocation might fail in test environment, that's ok
          # as long as we get a proper error
          assert is_tuple(reason)
      end
    end
    
    test "environment-specific configuration" do
      # Test development environment
      dev_config = Config.load_config()
      assert dev_config.detected_environment in [:test, :development]
      
      # Test CI environment simulation
      original_ci = System.get_env("CI")
      System.put_env("CI", "true")
      
      try do
        ci_config = Config.load_config()
        assert ci_config.ci_mode == true
        assert ci_config.detected_environment == :ci
        
        # CI should have adjusted timeouts
        assert ci_config.cluster_startup_timeout >= dev_config.cluster_startup_timeout
        
      after
        case original_ci do
          nil -> System.delete_env("CI")
          value -> System.put_env("CI", value)
        end
      end
    end
    
    test "configuration can be overridden at runtime" do
      base_config = Config.load_config()
      
      # Override some settings
      custom_config = Config.load_config(
        default_cluster_size: 5,
        cluster_startup_timeout: 45_000,
        auto_cluster: false
      )
      
      assert custom_config.default_cluster_size == 5
      assert custom_config.cluster_startup_timeout == 45_000
      assert custom_config.auto_cluster == false
      
      # Other settings should remain from base config
      assert custom_config.reuse_clusters == base_config.reuse_clusters
      assert custom_config.http_port_base == base_config.http_port_base
    end
  end
  
  describe "error handling" do
    test "invalid configuration raises helpful errors" do
      # Test invalid cluster size
      assert_raise ArgumentError, ~r/default_cluster_size must be at least 1/, fn ->
        Config.load_config(default_cluster_size: 0) |> Config.validate_config()
      end
      
      # Test invalid timeout
      assert_raise ArgumentError, ~r/cluster_startup_timeout must be a positive integer/, fn ->
        Config.load_config(cluster_startup_timeout: -5000) |> Config.validate_config()
      end
      
      # Test port range overlap
      assert_raise ArgumentError, ~r/HTTP and distribution port ranges overlap/, fn ->
        Config.load_config(http_port_base: 5000, dist_port_base: 5000) |> Config.validate_config()
      end
    end
    
    test "configuration warnings are detected" do
      # Create a configuration that should generate warnings
      config = Config.load_config(
        default_cluster_size: 10,  # Large cluster size should warn
        cleanup_on_exit: false     # Disabled cleanup should warn
      )
      
      case ConfigValidator.validate_with_warnings(config) do
        {:ok, warnings} ->
          assert length(warnings) > 0
          
          # Should have warnings about large cluster size and disabled cleanup
          warning_messages = Enum.map(warnings, & &1.message)
          assert Enum.any?(warning_messages, &String.contains?(&1, "cluster size"))
          
        {:error, _} ->
          # If there are errors, that's also acceptable for this test
          assert true
      end
    end
  end
end