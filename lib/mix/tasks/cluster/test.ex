defmodule Mix.Tasks.Cluster.Test do
  @moduledoc """
  Distributed test cluster management tool.

  Provides automated lifecycle management for distributed test clusters,
  eliminating manual server management and ensuring reliable testing.

  ## Commands

      mix cluster.test start           # Start test cluster
      mix cluster.test stop            # Stop test cluster  
      mix cluster.test restart         # Restart test cluster
      mix cluster.test status          # Show cluster status
      mix cluster.test clean           # Clean up all test artifacts
      mix cluster.test run             # Start cluster + run tests + cleanup
      mix cluster.test health          # Comprehensive health check
      mix cluster.test logs [node]     # Show logs from test nodes
      mix cluster.test preflight       # Run pre-flight environment checks

  ## Examples

      # Check if environment is ready for distributed testing
      mix cluster.test preflight
      
      # Start a fresh test cluster
      mix cluster.test start
      
      # Run distributed tests with automatic cluster management
      mix cluster.test run
      
      # Check if cluster is healthy
      mix cluster.test health
      
      # Clean up everything
      mix cluster.test clean
  """

  use Mix.Task

  @shortdoc "Manage distributed test clusters"

  alias OTPSupervisor.TestCluster.Manager
  alias OTPSupervisor.TestCluster.Diagnostics
  alias OTPSupervisor.TestCluster.PortManager

  # Ensure the Manager GenServer is started
  defp ensure_manager_started do
    case GenServer.whereis(Manager) do
      nil ->
        case Manager.start_link() do
          {:ok, _pid} ->
            Mix.shell().info("Started TestCluster.Manager")
            :ok

          {:error, {:already_started, _pid}} ->
            :ok

          {:error, reason} ->
            Mix.shell().error("Failed to start TestCluster.Manager: #{inspect(reason)}")
            {:error, reason}
        end

      _pid ->
        :ok
    end
  end

  def run(args) do
    # Ensure the application is started for our GenServers
    Mix.Task.run("app.start")

    case args do
      [] -> show_help()
      ["start"] -> start_cluster()
      ["stop"] -> stop_cluster()
      ["restart"] -> restart_cluster()
      ["status"] -> show_status()
      ["clean"] -> clean_cluster()
      ["run"] -> run_full_cycle()
      ["health"] -> health_check()
      ["logs"] -> show_logs()
      ["logs", node] -> show_logs(node)
      ["preflight"] -> preflight_check()
      _ -> show_help()
    end
  end

  defp start_cluster do
    Mix.shell().info("🚀 Starting distributed test cluster...")

    # Run prerequisite check first
    case Diagnostics.check_prerequisites() do
      :ok ->
        start_cluster_after_checks()

      {:error, failed_checks} ->
        Mix.shell().error("❌ Prerequisites failed:")
        display_prerequisite_failures(failed_checks)
        System.halt(1)
    end
  end

  defp start_cluster_after_checks do
    with :ok <- ensure_manager_started(),
         {:ok, nodes} <- Manager.start_cluster() do
      Mix.shell().info("✅ Test cluster started successfully!")
      Mix.shell().info("📍 Nodes: #{inspect(nodes)}")
      show_cluster_info(nodes)
    else
      {:error, reason} ->
        diagnosis = Diagnostics.diagnose_startup_failure(reason)
        display_startup_failure(diagnosis)
        System.halt(1)
    end
  end

  defp stop_cluster do
    Mix.shell().info("🛑 Stopping distributed test cluster...")

    case Manager.stop_cluster() do
      :ok ->
        Mix.shell().info("✅ Test cluster stopped successfully!")

      {:error, reason} ->
        Mix.shell().error("❌ Failed to stop cluster: #{inspect(reason)}")
        Mix.shell().info("💡 Try: mix cluster.test clean")
        System.halt(1)
    end
  end

  defp restart_cluster do
    Mix.shell().info("🔄 Restarting distributed test cluster...")

    with :ok <- Manager.stop_cluster(),
         {:ok, nodes} <- Manager.start_cluster() do
      Mix.shell().info("✅ Test cluster restarted successfully!")
      Mix.shell().info("📍 Nodes: #{inspect(nodes)}")
    else
      {:error, reason} ->
        diagnosis = Diagnostics.diagnose_startup_failure(reason)
        display_startup_failure(diagnosis)
        System.halt(1)
    end
  end

  defp show_status do
    with :ok <- ensure_manager_started(),
         {:ok, status} <- Manager.get_status() do
      display_status(status)
    else
      {:error, reason} ->
        Mix.shell().error("❌ Failed to get cluster status: #{inspect(reason)}")
        System.halt(1)
    end
  end

  defp clean_cluster do
    Mix.shell().info("🧹 Cleaning up test cluster artifacts...")

    # Perform comprehensive cleanup using multiple strategies
    cleanup_results = perform_comprehensive_cleanup()

    case cleanup_results do
      :ok ->
        Mix.shell().info("✅ Cleanup completed successfully!")

      {:partial_success, issues} ->
        Mix.shell().info("⚠️  Cleanup completed with some issues:")

        Enum.each(issues, fn issue ->
          Mix.shell().info("  • #{issue}")
        end)

        Mix.shell().info("✅ Core cleanup operations succeeded")

      {:error, reason} ->
        Mix.shell().error("❌ Cleanup failed: #{inspect(reason)}")
        Mix.shell().info("💡 Try manual cleanup:")
        Mix.shell().info("  • pkill -f test_node")
        Mix.shell().info("  • epmd -kill")
        Mix.shell().info("  • netstat -tulpn | grep 41[0-9][0-9]")
        System.halt(1)
    end
  end

  defp run_full_cycle do
    Mix.shell().info("🎯 Running full distributed test cycle...")

    # Run prerequisite check first
    case Diagnostics.check_prerequisites() do
      :ok ->
        run_full_cycle_after_checks()

      {:error, failed_checks} ->
        Mix.shell().error("❌ Prerequisites failed:")
        display_prerequisite_failures(failed_checks)
        System.halt(1)
    end
  end

  defp run_full_cycle_after_checks do
    with :ok <- ensure_manager_started(),
         {:ok, _nodes} <- Manager.start_cluster(),
         :ok <- run_distributed_tests(),
         :ok <- Manager.stop_cluster() do
      Mix.shell().info("✅ Full test cycle completed successfully!")
    else
      {:error, reason} ->
        diagnosis = Diagnostics.diagnose_startup_failure(reason)
        display_startup_failure(diagnosis)
        # Cleanup on failure
        Manager.stop_cluster()
        System.halt(1)
    end
  end

  defp health_check do
    Mix.shell().info("🏥 Running comprehensive health check...")

    with :ok <- ensure_manager_started(),
         {:ok, results} <- Manager.health_check() do
      display_health_results(results)
    else
      {:error, reason} ->
        Mix.shell().error("❌ Health check failed: #{inspect(reason)}")
        System.halt(1)
    end
  end

  defp show_logs(node \\ nil) do
    with :ok <- ensure_manager_started(),
         {:ok, logs} <- Manager.get_logs(node) do
      display_logs(logs, node)
    else
      {:error, reason} ->
        Mix.shell().error("❌ Failed to get logs: #{inspect(reason)}")
        System.halt(1)
    end
  end

  defp preflight_check do
    Mix.shell().info("🔍 Running pre-flight environment checks...")

    # Use Diagnostics module for core checks
    case Diagnostics.check_prerequisites() do
      :ok ->
        Mix.shell().info("✅ Core prerequisites passed!")
        run_extended_preflight_checks()

      {:error, failed_checks} ->
        Mix.shell().error("❌ Core prerequisites failed:")
        display_prerequisite_failures(failed_checks)

        # Still run extended checks for complete picture
        Mix.shell().info("")
        Mix.shell().info("Running extended checks...")
        run_extended_preflight_checks()
    end
  end

  defp run_extended_preflight_checks do
    checks = [
      {"Distributed Erlang", &check_distributed_erlang/0},
      {"Test environment", &check_test_environment/0}
    ]

    results =
      Enum.map(checks, fn {name, check_fn} ->
        Mix.shell().info("  Checking #{name}...")
        result = check_fn.()
        {name, result}
      end)

    display_extended_preflight_results(results)
  end

  # Private helper functions

  defp run_distributed_tests do
    Mix.shell().info("🧪 Running distributed tests...")

    # Ensure distributed Erlang is started before running tests
    unless Node.alive?() do
      Mix.shell().info("Starting distributed Erlang for tests...")

      case Node.start(:"test_primary@127.0.0.1", :shortnames) do
        {:ok, _} ->
          Node.set_cookie(:test_cluster_cookie)
          Mix.shell().info("✅ Distributed Erlang started: #{Node.self()}")

        {:error, {:already_started, _}} ->
          Node.set_cookie(:test_cluster_cookie)
          Mix.shell().info("✅ Using existing distributed node: #{Node.self()}")

        {:error, reason} ->
          Mix.shell().error("❌ Failed to start distributed Erlang: #{inspect(reason)}")
          {:error, :distributed_startup_failed}
      end
    end

    # Run the actual test suite with real nodes
    case System.cmd("mix", ["test", "--only", "real_nodes", "--timeout", "60000"],
           stderr_to_stdout: true
         ) do
      {output, 0} ->
        Mix.shell().info(output)
        :ok

      {output, exit_code} ->
        Mix.shell().error("Tests failed with exit code #{exit_code}")
        Mix.shell().error(output)
        {:error, :test_failure}
    end
  end

  defp show_cluster_info(servers) do
    Mix.shell().info("")
    Mix.shell().info("🌐 Cluster Information:")
    Mix.shell().info("=" <> String.duplicate("=", 40))

    Enum.each(servers, fn {name, server_info} ->
      Mix.shell().info("  • #{name}: #{server_info.url}")
    end)

    Mix.shell().info("")
    Mix.shell().info("🔗 Test the servers:")

    Enum.each(servers, fn {_name, server_info} ->
      Mix.shell().info("  curl #{server_info.url}")
    end)

    Mix.shell().info("")
    Mix.shell().info("📊 Check status: mix cluster.test status")
    Mix.shell().info("🏥 Health check: mix cluster.test health")

    Mix.shell().info("")
    Mix.shell().info("💡 Next steps:")
    Mix.shell().info("  mix test --only real_nodes    # Run distributed tests")
    Mix.shell().info("  mix cluster.test health       # Check cluster health")
    Mix.shell().info("  mix cluster.test stop          # Stop cluster")
  end

  defp display_status(status) do
    Mix.shell().info("📊 Cluster Status: #{status.overall}")
    Mix.shell().info("")

    Enum.each(status.nodes, fn {name, node_status} ->
      icon = if node_status.healthy, do: "✅", else: "❌"
      Mix.shell().info("  #{icon} #{name}: #{node_status.status}")

      if node_status.http_port do
        Mix.shell().info("     HTTP: http://localhost:#{node_status.http_port}")
      end

      if not node_status.healthy and node_status.issues do
        Enum.each(node_status.issues, fn issue ->
          Mix.shell().info("     ⚠️  #{issue}")
        end)
      end
    end)
  end

  defp display_health_results(results) do
    overall = if results.all_passed, do: "✅ HEALTHY", else: "❌ UNHEALTHY"
    Mix.shell().info("🏥 Overall Health: #{overall}")
    Mix.shell().info("")

    Enum.each(results.checks, fn {check_name, result} ->
      icon = if result.passed, do: "✅", else: "❌"
      Mix.shell().info("  #{icon} #{check_name}: #{result.message}")

      if not result.passed and Map.has_key?(result, :details) do
        Mix.shell().info("     Details: #{result.details}")
      end
    end)

    if not results.all_passed do
      Mix.shell().info("")
      Mix.shell().info("💡 Troubleshooting suggestions:")
      Enum.each(results.suggestions, &Mix.shell().info("  • #{&1}"))
    end
  end

  defp display_logs(logs, node) do
    header = if node, do: "📋 Logs for #{node}:", else: "📋 Cluster Logs:"
    Mix.shell().info(header)
    Mix.shell().info(String.duplicate("=", String.length(header)))
    Mix.shell().info("")

    case logs do
      %{} ->
        # Multiple nodes
        Enum.each(logs, fn {node_name, node_logs} ->
          Mix.shell().info("--- #{node_name} ---")
          Mix.shell().info(node_logs)
          Mix.shell().info("")
        end)

      logs when is_binary(logs) ->
        # Single node
        Mix.shell().info(logs)
    end
  end

  defp display_prerequisite_failures(failed_checks) do
    Mix.shell().info("")

    Enum.each(failed_checks, fn {name, result} ->
      case result do
        {:error, message} ->
          Mix.shell().info("  ❌ #{name}: #{message}")

        {:warning, message} ->
          Mix.shell().info("  ⚠️  #{name}: #{message}")
      end
    end)

    Mix.shell().info("")
    Mix.shell().info("💡 Quick fixes:")
    Mix.shell().info("  • Start EPMD: epmd -daemon")
    Mix.shell().info("  • Clean up ports: mix cluster.test clean")
    Mix.shell().info("  • Check network: ping localhost")
    Mix.shell().info("  • Run full check: mix cluster.test preflight")
  end

  defp display_startup_failure(diagnosis) do
    Mix.shell().error("❌ #{diagnosis.problem}")
    Mix.shell().info("")
    Mix.shell().info("💡 Try these solutions:")
    Enum.each(diagnosis.solutions, &Mix.shell().info("  • #{&1}"))
    Mix.shell().info("")
    Mix.shell().info("🔍 For detailed diagnostics: mix cluster.test preflight")
  end

  defp show_help do
    Mix.shell().info(@moduledoc)
  end

  # Pre-flight check functions

  defp check_distributed_erlang do
    if Node.alive?() do
      {:ok, "Distributed Erlang is running: #{Node.self()}"}
    else
      {:warning, "Distributed Erlang is not running - this is normal for non-test environments"}
    end
  end

  defp check_test_environment do
    try do
      # Check if our test modules can be loaded
      Code.ensure_loaded!(OTPSupervisor.TestCluster.Manager)
      Code.ensure_loaded!(OTPSupervisor.TestCluster.NodeProvisioner)
      Code.ensure_loaded!(OTPSupervisor.TestCluster.HealthChecker)
      {:ok, "Test cluster modules are available"}
    rescue
      error -> {:error, "Test environment issue: #{Exception.message(error)}"}
    end
  end

  defp display_extended_preflight_results(results) do
    Mix.shell().info("")
    Mix.shell().info("🔍 Extended Pre-flight Check Results:")
    Mix.shell().info("=" |> String.duplicate(40))

    all_passed =
      Enum.all?(results, fn {name, result} ->
        case result do
          {:ok, message} ->
            Mix.shell().info("  ✅ #{name}: #{message}")
            true

          {:warning, message} ->
            Mix.shell().info("  ⚠️  #{name}: #{message}")
            true

          {:error, message} ->
            Mix.shell().info("  ❌ #{name}: #{message}")
            false
        end
      end)

    Mix.shell().info("")

    if all_passed do
      Mix.shell().info(
        "✅ All extended checks passed! Environment is ready for distributed testing."
      )
    else
      Mix.shell().info("⚠️  Some extended checks failed, but core functionality may still work.")
      Mix.shell().info("")
      Mix.shell().info("💡 Additional solutions:")
      Mix.shell().info("  • Ensure test modules are compiled: mix compile")
      Mix.shell().info("  • Check dependencies: mix deps.get")
      Mix.shell().info("  • Verify test environment setup")
    end
  end

  # Comprehensive cleanup implementation

  defp perform_comprehensive_cleanup do
    Mix.shell().info("🔧 Starting comprehensive cleanup process...")

    cleanup_steps = [
      {"Manager cleanup", &cleanup_via_manager/0},
      {"Test processes", &cleanup_test_processes/0},
      {"Test ports", &cleanup_test_ports/0},
      {"EPMD cleanup", &cleanup_epmd/0},
      {"Test artifacts", &cleanup_test_artifacts/0}
    ]

    {successful_steps, failed_steps} =
      cleanup_steps
      |> Enum.map(fn {step_name, cleanup_fn} ->
        Mix.shell().info("  • #{step_name}...")

        case cleanup_fn.() do
          :ok ->
            Mix.shell().info("    ✅ #{step_name} completed")
            {:success, step_name}

          {:error, reason} ->
            Mix.shell().info("    ❌ #{step_name} failed: #{inspect(reason)}")
            {:failed, step_name, reason}

          {:partial_success, issues} ->
            Mix.shell().info("    ⚠️  #{step_name} partially completed")

            Enum.each(issues, fn issue ->
              Mix.shell().info("      • #{issue}")
            end)

            {:partial, step_name, issues}
        end
      end)
      |> Enum.split_with(fn
        {:success, _} -> true
        {:partial, _, _} -> true
        {:failed, _, _} -> false
      end)

    case {successful_steps, failed_steps} do
      {_, []} ->
        :ok

      {successful, failed} when successful != [] ->
        issues =
          Enum.map(failed, fn {:failed, step, reason} ->
            "#{step}: #{inspect(reason)}"
          end)

        {:partial_success, issues}

      {[], _} ->
        {:error, :all_cleanup_steps_failed}
    end
  end

  defp cleanup_via_manager do
    case ensure_manager_started() do
      :ok ->
        case Manager.clean_all() do
          :ok -> :ok
          {:error, reason} -> {:error, {:manager_cleanup_failed, reason}}
        end

      {:error, _reason} ->
        # Manager not available, continue with other cleanup methods
        Mix.shell().info("    Manager not available, using direct cleanup methods")
        :ok
    end
  end

  defp cleanup_test_processes do
    Mix.shell().info("    Killing test node processes...")

    case PortManager.cleanup_test_processes() do
      :ok ->
        # Also try additional process cleanup patterns
        additional_cleanup_patterns = [
          "elixir.*test_node",
          "beam.*test_cluster",
          "mix.*phx.server.*test"
        ]

        additional_results = Enum.map(additional_cleanup_patterns, &kill_processes_by_pattern/1)

        if Enum.all?(additional_results, &(&1 == :ok)) do
          :ok
        else
          {:partial_success, ["Some additional test processes may still be running"]}
        end

      {:error, reason} ->
        {:error, {:test_process_cleanup_failed, reason}}
    end
  end

  defp cleanup_test_ports do
    Mix.shell().info("    Cleaning up test ports...")

    # Define the typical port ranges used by test clusters
    test_port_ranges = [
      # HTTP ports
      {4100, 4110},
      # Distribution ports
      {9100, 9110}
    ]

    port_cleanup_results =
      test_port_ranges
      |> Enum.flat_map(fn {start_port, end_port} ->
        Enum.to_list(start_port..end_port)
      end)
      |> Enum.map(&PortManager.cleanup_single_port/1)

    failed_ports = Enum.filter(port_cleanup_results, &match?({:error, _}, &1))

    if Enum.empty?(failed_ports) do
      :ok
    else
      {:partial_success, ["Some ports could not be cleaned: #{length(failed_ports)} failures"]}
    end
  end

  defp cleanup_epmd do
    Mix.shell().info("    Cleaning up EPMD...")

    # Get list of registered nodes before cleanup
    case System.cmd("epmd", ["-names"], stderr_to_stdout: true) do
      {output, 0} ->
        test_nodes =
          output
          |> String.split("\n")
          |> Enum.filter(&String.contains?(&1, "test_node"))

        if Enum.empty?(test_nodes) do
          Mix.shell().info("      No test nodes registered in EPMD")
          :ok
        else
          Mix.shell().info("      Found #{length(test_nodes)} test nodes in EPMD")
          # Kill EPMD to force cleanup of registered nodes
          case System.cmd("epmd", ["-kill"], stderr_to_stdout: true) do
            {_, 0} ->
              # Restart EPMD
              :timer.sleep(1000)

              case System.cmd("epmd", ["-daemon"], stderr_to_stdout: true) do
                {_, 0} -> :ok
                {error, _} -> {:error, {:epmd_restart_failed, error}}
              end

            {error, _} ->
              {:error, {:epmd_kill_failed, error}}
          end
        end

      {_error, _} ->
        # EPMD not running or accessible, which is fine
        :ok
    end
  end

  defp cleanup_test_artifacts do
    Mix.shell().info("    Cleaning up test artifacts...")

    # Clean up common test artifacts
    artifact_cleanup_tasks = [
      &cleanup_test_logs/0,
      &cleanup_test_tmp_files/0,
      &cleanup_test_databases/0
    ]

    results = Enum.map(artifact_cleanup_tasks, & &1.())
    failed_cleanups = Enum.filter(results, &match?({:error, _}, &1))

    if Enum.empty?(failed_cleanups) do
      :ok
    else
      {:partial_success, ["Some test artifacts could not be cleaned"]}
    end
  end

  defp kill_processes_by_pattern(pattern) do
    case System.cmd("pkill", ["-f", pattern], stderr_to_stdout: true) do
      {_output, 0} -> :ok
      # No processes found, which is fine
      {_output, 1} -> :ok
      {_error, _} -> {:error, {:pkill_pattern_failed, pattern}}
    end
  end

  defp cleanup_test_logs do
    # Remove test-specific log files
    log_patterns = [
      "_build/test/logs/test_node*.log",
      "tmp/test_cluster_*.log",
      "test_cluster_*.log"
    ]

    Enum.each(log_patterns, fn pattern ->
      case System.cmd("rm", ["-f"] ++ Path.wildcard(pattern), stderr_to_stdout: true) do
        {_, 0} -> :ok
        # Files might not exist, which is fine
        {_, _} -> :ok
      end
    end)

    :ok
  end

  defp cleanup_test_tmp_files do
    # Remove temporary files created during testing
    tmp_patterns = [
      "tmp/test_cluster_*",
      "/tmp/test_node_*",
      "/tmp/cluster_test_*"
    ]

    Enum.each(tmp_patterns, fn pattern ->
      case System.cmd("rm", ["-rf"] ++ Path.wildcard(pattern), stderr_to_stdout: true) do
        {_, 0} -> :ok
        # Files might not exist, which is fine
        {_, _} -> :ok
      end
    end)

    :ok
  end

  defp cleanup_test_databases do
    # Clean up any test-specific database artifacts
    # This is a placeholder for database cleanup if needed
    :ok
  end
end
