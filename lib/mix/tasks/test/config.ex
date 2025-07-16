defmodule Mix.Tasks.Test.Config do
  @moduledoc """
  Display and validate distributed testing configuration.

  This task helps developers understand and troubleshoot their
  distributed testing configuration.

  ## Usage

      mix test.config                    # Show current configuration
      mix test.config --validate         # Validate configuration
      mix test.config --check-ports      # Check port availability
      mix test.config --environment ci   # Show CI-specific config
      
  ## Examples

      # Show complete configuration
      mix test.config
      
      # Validate configuration and show any issues
      mix test.config --validate
      
      # Check if configured ports are available
      mix test.config --check-ports
      
      # Show configuration as it would appear in CI
      mix test.config --environment ci
  """

  use Mix.Task

  @shortdoc "Display and validate distributed testing configuration"

  alias OTPSupervisor.Testing.{Config, ConfigValidator, PortManager}

  @switches [
    validate: :boolean,
    check_ports: :boolean,
    environment: :string,
    verbose: :boolean
  ]

  def run(args) do
    {opts, _} = OptionParser.parse!(args, switches: @switches)

    config = load_config_for_environment(opts[:environment])

    cond do
      opts[:validate] -> validate_configuration(config, opts)
      opts[:check_ports] -> check_port_availability(config, opts)
      true -> display_configuration(config, opts)
    end
  end

  # Private implementation

  defp load_config_for_environment(nil) do
    Config.load_config()
  end

  defp load_config_for_environment(env_string) do
    # Temporarily set environment variables to simulate the environment
    original_ci = System.get_env("CI")

    case String.downcase(env_string) do
      "ci" ->
        System.put_env("CI", "true")
        config = Config.load_config()
        restore_env("CI", original_ci)
        config

      "dev" ->
        System.delete_env("CI")
        config = Config.load_config()
        restore_env("CI", original_ci)
        config

      _ ->
        Mix.shell().error("Unknown environment: #{env_string}")
        Mix.shell().info("Available environments: ci, dev")
        System.halt(1)
    end
  end

  defp restore_env(_key, nil), do: :ok
  defp restore_env(key, value), do: System.put_env(key, value)

  defp display_configuration(config, opts) do
    Mix.shell().info("🔧 Distributed Testing Configuration")
    Mix.shell().info("=" <> String.duplicate("=", 50))

    display_environment_info(config)
    display_cluster_config(config)
    display_port_config(config)
    display_timeout_config(config)

    if opts[:verbose] do
      display_advanced_config(config)
    end

    Mix.shell().info("\n💡 Use --validate to check for configuration issues")
    Mix.shell().info("💡 Use --check-ports to verify port availability")
  end

  defp display_environment_info(config) do
    Mix.shell().info("\n📍 Environment Detection:")
    Mix.shell().info("  Mix Environment: #{config.mix_env}")
    Mix.shell().info("  Detected Environment: #{config.detected_environment}")
    Mix.shell().info("  CI Mode: #{config.ci_mode}")

    if config.ci_mode do
      Mix.shell().info("  CI Optimizations: Enabled")
      Mix.shell().info("    - Timeout Multiplier: #{config.ci_timeout_multiplier}x")
      Mix.shell().info("    - Max Cluster Size: #{config.ci_max_cluster_size}")
    end
  end

  defp display_cluster_config(config) do
    Mix.shell().info("\n🚀 Cluster Configuration:")
    Mix.shell().info("  Auto Cluster: #{config.auto_cluster}")
    Mix.shell().info("  Reuse Clusters: #{config.reuse_clusters}")
    Mix.shell().info("  Default Cluster Size: #{config.default_cluster_size}")
    Mix.shell().info("  Max Cluster Size: #{config.max_cluster_size}")

    if config.ci_mode do
      Mix.shell().info("  CI Cluster Size: #{config.ci_cluster_size}")
    end
  end

  defp display_port_config(config) do
    port_ranges = Config.get_port_ranges(config)

    Mix.shell().info("\n🔌 Port Configuration:")

    Mix.shell().info(
      "  HTTP Port Range: #{elem(port_ranges.http, 0)}-#{elem(port_ranges.http, 1)}"
    )

    Mix.shell().info(
      "  Distribution Port Range: #{elem(port_ranges.distribution, 0)}-#{elem(port_ranges.distribution, 1)}"
    )

    Mix.shell().info("  Port Range Size: #{config.port_range_size}")
  end

  defp display_timeout_config(config) do
    Mix.shell().info("\n⏱️  Timeout Configuration:")
    Mix.shell().info("  Cluster Startup: #{format_timeout(config.cluster_startup_timeout)}")
    Mix.shell().info("  Cluster Cleanup: #{format_timeout(config.cluster_cleanup_timeout)}")
    Mix.shell().info("  Cluster Health Check: #{format_timeout(config.cluster_health_timeout)}")
    Mix.shell().info("  Distributed Test: #{format_timeout(config.distributed_test_timeout)}")
  end

  defp display_advanced_config(config) do
    Mix.shell().info("\n🔧 Advanced Configuration:")
    Mix.shell().info("  Verbose Cluster Logs: #{config.verbose_cluster_logs}")
    Mix.shell().info("  Save Cluster Logs: #{config.save_cluster_logs}")
    Mix.shell().info("  Debug Cluster Startup: #{config.debug_cluster_startup}")
    Mix.shell().info("  Skip Distributed on Failure: #{config.skip_distributed_on_failure}")
    Mix.shell().info("  Retry Cluster Startup: #{config.retry_cluster_startup}")
    Mix.shell().info("  Max Startup Retries: #{config.max_startup_retries}")
    Mix.shell().info("  Cleanup on Exit: #{config.cleanup_on_exit}")
    Mix.shell().info("  Force Cleanup on Error: #{config.force_cleanup_on_error}")
    Mix.shell().info("  Monitor Cluster Health: #{config.monitor_cluster_health}")
  end

  defp validate_configuration(config, opts) do
    Mix.shell().info("🔍 Validating Distributed Testing Configuration...")

    case ConfigValidator.validate_with_warnings(config) do
      {:ok, []} ->
        Mix.shell().info("✅ Configuration is valid!")

      {:ok, warnings} ->
        Mix.shell().info("✅ Configuration is valid with warnings:")
        display_validation_issues(warnings, :warning)

      {:error, report} ->
        Mix.shell().error("❌ Configuration validation failed!")

        Mix.shell().error(
          "Found #{report.error_count} errors and #{report.warning_count} warnings"
        )

        if not Enum.empty?(report.errors) do
          Mix.shell().error("\n🚨 Errors:")
          display_validation_issues(report.errors, :error)
        end

        if not Enum.empty?(report.warnings) do
          Mix.shell().info("\n⚠️  Warnings:")
          display_validation_issues(report.warnings, :warning)
        end

        System.halt(1)
    end

    if opts[:verbose] do
      display_configuration(config, opts)
    end
  end

  defp check_port_availability(config, opts) do
    Mix.shell().info("🔌 Checking Port Availability...")

    port_ranges = Config.get_port_ranges(config)
    availability = PortManager.check_port_availability(config)

    Mix.shell().info("HTTP Port Range: #{elem(port_ranges.http, 0)}-#{elem(port_ranges.http, 1)}")
    display_port_status(availability.http_ports_available, "HTTP ports")

    Mix.shell().info(
      "Distribution Port Range: #{elem(port_ranges.distribution, 0)}-#{elem(port_ranges.distribution, 1)}"
    )

    display_port_status(availability.dist_ports_available, "Distribution ports")

    if availability.all_available do
      Mix.shell().info("\n✅ All configured ports are available!")
    else
      Mix.shell().error("\n❌ Some ports are not available")
      Mix.shell().info("💡 Try adjusting port_base values in your configuration")
      Mix.shell().info("💡 Or stop services using conflicting ports")

      # Try to find alternative ports
      Mix.shell().info("\n🔍 Searching for alternative ports...")

      case PortManager.find_alternative_ports(config.default_cluster_size, config) do
        {:ok, alternatives} ->
          Mix.shell().info("✅ Found alternative port assignments:")

          if opts[:verbose] do
            display_port_assignments(alternatives)
          else
            Mix.shell().info("  Use --verbose to see detailed port assignments")
          end

        {:error, :no_available_ports} ->
          Mix.shell().error("❌ No alternative ports found")
          Mix.shell().info("💡 Try stopping other services or using smaller cluster sizes")
      end
    end
  end

  defp display_port_status(true, description) do
    Mix.shell().info("  ✅ #{description}: Available")
  end

  defp display_port_status(false, description) do
    Mix.shell().error("  ❌ #{description}: Not available")
  end

  defp display_port_assignments(assignments) do
    Enum.each(assignments, fn {node, assignment} ->
      Mix.shell().info("  #{node}:")
      Mix.shell().info("    HTTP: #{assignment.http_port}")
      Mix.shell().info("    Distribution: #{assignment.distribution_port}")
    end)
  end

  defp display_validation_issues(issues, severity) do
    icon =
      case severity do
        :error -> "🚨"
        :warning -> "⚠️"
      end

    Enum.each(issues, fn issue ->
      Mix.shell().info("  #{icon} #{issue.message}")

      if Map.has_key?(issue, :suggestion) do
        Mix.shell().info("     💡 #{issue.suggestion}")
      end

      if Map.has_key?(issue, :current_value) do
        Mix.shell().info("     Current: #{inspect(issue.current_value)}")
      end
    end)
  end

  defp format_timeout(ms) when is_integer(ms) do
    cond do
      ms >= 60_000 -> "#{div(ms, 60_000)}m #{div(rem(ms, 60_000), 1000)}s"
      ms >= 1_000 -> "#{div(ms, 1000)}s"
      true -> "#{ms}ms"
    end
  end

  defp format_timeout(other), do: inspect(other)
end
