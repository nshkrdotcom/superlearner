defmodule OTPSupervisor.Testing.ConfigValidator do
  @moduledoc """
  Comprehensive validation for distributed testing configuration.
  
  Provides detailed validation with helpful error messages and
  suggestions for fixing configuration issues.
  """
  
  require Logger
  alias OTPSupervisor.Testing.{Config, PortManager}
  
  @doc """
  Validate the complete distributed testing configuration.
  
  Returns :ok if valid, or {:error, details} with comprehensive
  error information and suggestions.
  """
  def validate_complete_config(config \\ nil) do
    config = config || Config.load_config()
    
    validations = [
      &validate_basic_config/1,
      &validate_environment_config/1,
      &validate_port_config/1,
      &validate_timeout_config/1,
      &validate_cluster_config/1,
      &validate_system_requirements/1
    ]
    
    case run_validations(config, validations) do
      [] -> :ok
      errors -> {:error, build_validation_report(errors)}
    end
  end
  
  @doc """
  Validate configuration and provide warnings for potential issues.
  """
  def validate_with_warnings(config \\ nil) do
    config = config || Config.load_config()
    
    case validate_complete_config(config) do
      :ok ->
        warnings = check_for_warnings(config)
        {:ok, warnings}
        
      {:error, errors} ->
        {:error, errors}
    end
  end
  
  @doc """
  Quick validation check for essential configuration.
  """
  def validate_essential_config(config \\ nil) do
    config = config || Config.load_config()
    
    essential_validations = [
      &validate_basic_config/1,
      &validate_cluster_config/1
    ]
    
    case run_validations(config, essential_validations) do
      [] -> :ok
      errors -> {:error, build_validation_report(errors)}
    end
  end
  
  @doc """
  Validate configuration for a specific environment.
  """
  def validate_for_environment(environment, config \\ nil) do
    config = config || Config.load_config()
    
    environment_validations = case environment do
      :ci -> [&validate_ci_config/1, &validate_resource_constraints/1]
      :development -> [&validate_dev_config/1, &validate_port_conflicts/1]
      :test -> [&validate_test_config/1]
      _ -> []
    end
    
    case run_validations(config, environment_validations) do
      [] -> :ok
      errors -> {:error, build_validation_report(errors)}
    end
  end
  
  # Private validation functions
  
  defp run_validations(config, validations) do
    validations
    |> Enum.flat_map(fn validation_fn ->
      try do
        validation_fn.(config)
      rescue
        error ->
          [%{
            type: :validation_error,
            message: "Validation function failed: #{inspect(error)}",
            severity: :error
          }]
      end
    end)
  end
  
  defp validate_basic_config(config) do
    errors = []
    
    errors = if not is_boolean(config.auto_cluster) do
      [%{
        type: :invalid_type,
        field: :auto_cluster,
        message: "auto_cluster must be a boolean",
        current_value: config.auto_cluster,
        expected_type: :boolean,
        severity: :error
      } | errors]
    else
      errors
    end
    
    errors = if not is_integer(config.default_cluster_size) or config.default_cluster_size < 1 do
      [%{
        type: :invalid_value,
        field: :default_cluster_size,
        message: "default_cluster_size must be a positive integer",
        current_value: config.default_cluster_size,
        valid_range: "1 or greater",
        severity: :error
      } | errors]
    else
      errors
    end
    
    if not is_boolean(config.reuse_clusters) do
      [%{
        type: :invalid_type,
        field: :reuse_clusters,
        message: "reuse_clusters must be a boolean",
        current_value: config.reuse_clusters,
        expected_type: :boolean,
        severity: :error
      } | errors]
    else
      errors
    end
  end
  
  defp validate_environment_config(config) do
    errors = []
    
    errors = if not is_boolean(config.ci_mode) do
      [%{
        type: :invalid_type,
        field: :ci_mode,
        message: "ci_mode must be a boolean",
        current_value: config.ci_mode,
        expected_type: :boolean,
        severity: :error
      } | errors]
    else
      errors
    end
    
    errors = if not is_integer(config.ci_cluster_size) or config.ci_cluster_size < 1 do
      [%{
        type: :invalid_value,
        field: :ci_cluster_size,
        message: "ci_cluster_size must be a positive integer",
        current_value: config.ci_cluster_size,
        valid_range: "1 or greater",
        severity: :error
      } | errors]
    else
      errors
    end
    
    if not is_number(config.ci_timeout_multiplier) or config.ci_timeout_multiplier <= 0 do
      [%{
        type: :invalid_value,
        field: :ci_timeout_multiplier,
        message: "ci_timeout_multiplier must be a positive number",
        current_value: config.ci_timeout_multiplier,
        valid_range: "greater than 0",
        severity: :error
      } | errors]
    else
      errors
    end
  end
  
  defp validate_port_config(config) do
    errors = []
    
    errors = if not is_integer(config.http_port_base) or 
                config.http_port_base < 1024 or 
                config.http_port_base > 65535 do
      [%{
        type: :invalid_value,
        field: :http_port_base,
        message: "http_port_base must be between 1024 and 65535",
        current_value: config.http_port_base,
        valid_range: "1024-65535",
        severity: :error
      } | errors]
    else
      errors
    end
    
    errors = if not is_integer(config.dist_port_base) or 
                config.dist_port_base < 1024 or 
                config.dist_port_base > 65535 do
      [%{
        type: :invalid_value,
        field: :dist_port_base,
        message: "dist_port_base must be between 1024 and 65535",
        current_value: config.dist_port_base,
        valid_range: "1024-65535",
        severity: :error
      } | errors]
    else
      errors
    end
    
    errors = if not is_integer(config.port_range_size) or 
                config.port_range_size < 10 or 
                config.port_range_size > 1000 do
      [%{
        type: :invalid_value,
        field: :port_range_size,
        message: "port_range_size must be between 10 and 1000",
        current_value: config.port_range_size,
        valid_range: "10-1000",
        severity: :error
      } | errors]
    else
      errors
    end
    
    # Check for port range overlap
    http_end = config.http_port_base + config.port_range_size - 1
    dist_start = config.dist_port_base
    dist_end = config.dist_port_base + config.port_range_size - 1
    
    if (config.http_port_base <= dist_end and http_end >= dist_start) do
      [%{
        type: :port_range_overlap,
        message: "HTTP and distribution port ranges overlap",
        http_range: "#{config.http_port_base}-#{http_end}",
        dist_range: "#{dist_start}-#{dist_end}",
        severity: :error,
        suggestion: "Adjust port_base values to avoid overlap"
      } | errors]
    else
      errors
    end
  end
  
  defp validate_timeout_config(config) do
    timeout_fields = [
      :cluster_startup_timeout,
      :cluster_cleanup_timeout,
      :cluster_health_timeout,
      :distributed_test_timeout
    ]
    
    Enum.flat_map(timeout_fields, fn field ->
      value = Map.get(config, field)
      
      cond do
        not is_integer(value) ->
          [%{
            type: :invalid_type,
            field: field,
            message: "#{field} must be an integer (milliseconds)",
            current_value: value,
            expected_type: :integer,
            severity: :error
          }]
          
        value <= 0 ->
          [%{
            type: :invalid_value,
            field: field,
            message: "#{field} must be positive",
            current_value: value,
            valid_range: "greater than 0",
            severity: :error
          }]
          
        value < 1000 ->
          [%{
            type: :potentially_too_small,
            field: field,
            message: "#{field} might be too small (less than 1 second)",
            current_value: value,
            severity: :warning,
            suggestion: "Consider using at least 1000ms (1 second)"
          }]
          
        true ->
          []
      end
    end)
  end
  
  defp validate_cluster_config(config) do
    errors = []
    
    errors = if config.max_cluster_size < config.default_cluster_size do
      [%{
        type: :inconsistent_values,
        message: "max_cluster_size must be >= default_cluster_size",
        max_cluster_size: config.max_cluster_size,
        default_cluster_size: config.default_cluster_size,
        severity: :error
      } | errors]
    else
      errors
    end
    
    errors = if config.ci_cluster_size > config.max_cluster_size do
      [%{
        type: :inconsistent_values,
        message: "ci_cluster_size should not exceed max_cluster_size",
        ci_cluster_size: config.ci_cluster_size,
        max_cluster_size: config.max_cluster_size,
        severity: :warning
      } | errors]
    else
      errors
    end
    
    if config.default_cluster_size > 10 do
      [%{
        type: :potentially_excessive,
        field: :default_cluster_size,
        message: "default_cluster_size is quite large",
        current_value: config.default_cluster_size,
        severity: :warning,
        suggestion: "Large clusters may consume significant resources"
      } | errors]
    else
      errors
    end
  end
  
  defp validate_system_requirements(config) do
    errors = []
    
    # Check port availability
    case PortManager.check_port_availability(config) do
      %{all_available: true} ->
        errors
        
      %{http_ports_available: false} ->
        [%{
          type: :port_unavailable,
          message: "HTTP port range is not available",
          port_range: Config.get_port_ranges(config).http,
          severity: :warning,
          suggestion: "Adjust http_port_base or stop conflicting services"
        } | errors]
        
      %{dist_ports_available: false} ->
        [%{
          type: :port_unavailable,
          message: "Distribution port range is not available",
          port_range: Config.get_port_ranges(config).distribution,
          severity: :warning,
          suggestion: "Adjust dist_port_base or stop conflicting services"
        } | errors]
    end
  end
  
  defp validate_ci_config(config) do
    if config.ci_mode and config.ci_cluster_size > 4 do
      [%{
        type: :ci_resource_concern,
        message: "Large cluster size in CI environment",
        current_value: config.ci_cluster_size,
        severity: :warning,
        suggestion: "Consider reducing ci_cluster_size to conserve CI resources"
      }]
    else
      []
    end
  end
  
  defp validate_dev_config(config) do
    # Check for potential conflicts with development environment
    if config.http_port_base == 4000 do
      [%{
        type: :dev_port_conflict,
        message: "HTTP port base conflicts with typical Phoenix dev server",
        current_value: config.http_port_base,
        severity: :warning,
        suggestion: "Use a different port base like 4200 to avoid conflicts"
      }]
    else
      []
    end
  end
  
  defp validate_test_config(config) do
    # Validate test-specific configuration
    if config.distributed_test_timeout < 10_000 do
      [%{
        type: :timeout_too_short,
        field: :distributed_test_timeout,
        message: "Distributed test timeout might be too short",
        current_value: config.distributed_test_timeout,
        severity: :warning,
        suggestion: "Consider at least 10 seconds for distributed tests"
      }]
    else
      []
    end
  end
  
  defp validate_resource_constraints(config) do
    # Check if configuration is reasonable for resource-constrained environments
    total_estimated_processes = config.max_cluster_size * 10  # Rough estimate
    
    if total_estimated_processes > 100 do
      [%{
        type: :resource_concern,
        message: "Configuration may require significant system resources",
        estimated_processes: total_estimated_processes,
        severity: :warning,
        suggestion: "Monitor system resources during testing"
      }]
    else
      []
    end
  end
  
  defp validate_port_conflicts(config) do
    # Check for known port conflicts in development
    port_ranges = Config.get_port_ranges(config)
    
    # Check if our ranges conflict with common development ports
    common_dev_ports = [4000, 4001, 3000, 8000, 8080]
    
    conflicts = Enum.filter(common_dev_ports, fn port ->
      in_http_range = port >= elem(port_ranges.http, 0) and port <= elem(port_ranges.http, 1)
      in_dist_range = port >= elem(port_ranges.distribution, 0) and port <= elem(port_ranges.distribution, 1)
      in_http_range or in_dist_range
    end)
    
    case conflicts do
      [] -> []
      ports -> 
        [%{
          type: :dev_port_conflicts,
          message: "Port ranges may conflict with common development servers",
          conflicting_ports: ports,
          severity: :warning,
          suggestion: "Adjust port ranges to avoid conflicts with development servers"
        }]
    end
  end
  
  defp check_for_warnings(config) do
    warning_checks = [
      &check_performance_warnings/1,
      &check_environment_warnings/1,
      &check_configuration_warnings/1
    ]
    
    warning_checks
    |> Enum.flat_map(fn check -> check.(config) end)
    |> Enum.filter(fn issue -> issue.severity == :warning end)
  end
  
  defp check_performance_warnings(config) do
    warnings = []
    
    warnings = if config.cluster_startup_timeout > 60_000 do
      [%{
        type: :performance_warning,
        message: "Very long cluster startup timeout",
        current_value: config.cluster_startup_timeout,
        severity: :warning,
        suggestion: "Long timeouts may slow down test execution"
      } | warnings]
    else
      warnings
    end
    
    if config.default_cluster_size > 5 do
      [%{
        type: :performance_warning,
        message: "Large default cluster size",
        current_value: config.default_cluster_size,
        severity: :warning,
        suggestion: "Large clusters may slow down test execution"
      } | warnings]
    else
      warnings
    end
  end
  
  defp check_environment_warnings(config) do
    case config.detected_environment do
      :ci when not config.ci_mode ->
        [%{
          type: :environment_mismatch,
          message: "Running in CI but ci_mode is false",
          severity: :warning,
          suggestion: "Enable ci_mode for CI-optimized settings"
        }]
        
      _ ->
        []
    end
  end
  
  defp check_configuration_warnings(config) do
    warnings = []
    
    warnings = if not config.cleanup_on_exit do
      [%{
        type: :cleanup_warning,
        message: "Automatic cleanup is disabled",
        severity: :warning,
        suggestion: "Enable cleanup_on_exit to prevent resource leaks"
      } | warnings]
    else
      warnings
    end
    
    if not config.reuse_clusters do
      [%{
        type: :performance_warning,
        message: "Cluster reuse is disabled",
        severity: :warning,
        suggestion: "Enable reuse_clusters for better performance"
      } | warnings]
    else
      warnings
    end
  end
  
  defp build_validation_report(errors) do
    %{
      valid: false,
      error_count: length(Enum.filter(errors, fn e -> e.severity == :error end)),
      warning_count: length(Enum.filter(errors, fn e -> e.severity == :warning end)),
      errors: Enum.filter(errors, fn e -> e.severity == :error end),
      warnings: Enum.filter(errors, fn e -> e.severity == :warning end),
      summary: build_error_summary(errors)
    }
  end
  
  defp build_error_summary(errors) do
    error_types = Enum.map(errors, fn e -> e.type end) |> Enum.uniq()
    
    "Found #{length(errors)} issues across #{length(error_types)} categories: #{Enum.join(error_types, ", ")}"
  end
end