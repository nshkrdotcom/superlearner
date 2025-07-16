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

    # Check if there are actually running processes first
    real_status = check_real_cluster_status()

    if real_status.overall_status == :running do
      Mix.shell().info("🔍 Detected running cluster processes - performing force cleanup...")
      force_stop_cluster()

      # Verify cleanup worked
      :timer.sleep(2000)  # Give processes time to die
      final_status = check_real_cluster_status()

      if final_status.overall_status == :stopped do
        Mix.shell().info("✅ Test cluster stopped successfully!")
      else
        Mix.shell().error("❌ Some processes may still be running")
        Mix.shell().info("💡 Try: mix cluster.test clean")
      end
    else
      # Try Manager cleanup for completeness
      case ensure_manager_started() do
        :ok ->
          case Manager.stop_cluster() do
            :ok ->
              Mix.shell().info("✅ Test cluster stopped successfully!")
            {:error, reason} ->
              Mix.shell().info("ℹ️  Manager cleanup failed: #{inspect(reason)}")
              Mix.shell().info("✅ No running processes detected anyway")
          end
        {:error, _} ->
          Mix.shell().info("✅ No cluster processes detected")
      end
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
    Mix.shell().info("📊 Checking actual cluster status...")

    # Check for real running processes instead of relying on Manager state
    real_status = check_real_cluster_status()
    display_real_status(real_status)

    # Also try Manager status if available
    case ensure_manager_started() do
      :ok ->
        case Manager.get_status() do
          {:ok, manager_status} ->
            Mix.shell().info("")
            Mix.shell().info("📋 Manager Status:")
            display_status(manager_status)
          {:error, _} ->
            Mix.shell().info("📋 Manager Status: Not available")
        end
      {:error, _} ->
        Mix.shell().info("📋 Manager Status: Not running")
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

  defp force_stop_cluster do
    Mix.shell().info("🔧 Force stopping test cluster processes...")

    # Kill processes by pattern
    patterns = [
      "elixir.*test_node",
      "beam.*test_node",
      "mix.*test_node"
    ]

    killed_any = false

    Enum.each(patterns, fn pattern ->
      case System.cmd("pkill", ["-f", pattern], stderr_to_stdout: true) do
        {_, 0} ->
          Mix.shell().info("  ✅ Killed processes matching: #{pattern}")
          killed_any = true
        {_, 1} ->
          Mix.shell().info("  ℹ️  No processes found for: #{pattern}")
        {error, _} ->
          Mix.shell().info("  ⚠️  Failed to kill #{pattern}: #{error}")
      end
    end)

    # Kill processes on test ports
    config = Application.get_env(:otp_supervisor, :distributed_testing, [])
    http_base = Keyword.get(config, :http_port_base, 4200)

    Enum.each(http_base..(http_base + 5), fn port ->
      case System.cmd("lsof", ["-ti:#{port}"], stderr_to_stdout: true) do
        {pids_output, 0} ->
          pids =
            pids_output
            |> String.trim()
            |> String.split("\n")
            |> Enum.reject(&(&1 == ""))

          if not Enum.empty?(pids) do
            Enum.each(pids, fn pid ->
              System.cmd("kill", ["-9", pid])
            end)
            Mix.shell().info("  ✅ Killed #{length(pids)} processes on port #{port}")
            killed_any = true
          end

        {_, _} ->
          # No processes on this port, which is fine
          :ok
      end
    end)

    if killed_any do
      Mix.shell().info("✅ Force stop completed - processes terminated")
    else
      Mix.shell().info("ℹ️  No test cluster processes found to stop")
    end
  end

  defp check_real_cluster_status do
    # Check for actual running processes and ports
    config = Application.get_env(:otp_supervisor, :distributed_testing, [])
    http_base = Keyword.get(config, :http_port_base, 4200)

    # Check ports 4200-4205 for running processes
    port_status =
      Enum.map(http_base..(http_base + 5), fn port ->
        case System.cmd("lsof", ["-ti:#{port}"], stderr_to_stdout: true) do
          {pids_output, 0} ->
            pids =
              pids_output
              |> String.trim()
              |> String.split("\n")
              |> Enum.reject(&(&1 == ""))

            if not Enum.empty?(pids) do
              # Try to get process info
              process_info = get_process_info(List.first(pids))
              {port, :running, pids, process_info}
            else
              {port, :not_running, [], nil}
            end

          {_, _} ->
            {port, :not_running, [], nil}
        end
      end)

    # Check for test_node processes
    test_node_processes =
      case System.cmd("pgrep", ["-f", "test_node"], stderr_to_stdout: true) do
        {pids_output, 0} ->
          pids_output
          |> String.trim()
          |> String.split("\n")
          |> Enum.reject(&(&1 == ""))
        {_, _} ->
          []
      end

    # Check EPMD for registered test nodes
    epmd_nodes =
      case System.cmd("epmd", ["-names"], stderr_to_stdout: true) do
        {output, 0} ->
          output
          |> String.split("\n")
          |> Enum.filter(&String.contains?(&1, "test_node"))
          |> Enum.map(&String.trim/1)
        {_, _} ->
          []
      end

    %{
      ports: port_status,
      test_processes: test_node_processes,
      epmd_nodes: epmd_nodes,
      overall_status: determine_overall_status(port_status, test_node_processes, epmd_nodes)
    }
  end

  defp get_process_info(pid) do
    case System.cmd("ps", ["-p", pid, "-o", "pid,ppid,cmd"], stderr_to_stdout: true) do
      {output, 0} ->
        lines = String.split(output, "\n")
        if length(lines) > 1 do
          Enum.at(lines, 1) |> String.trim()
        else
          "Unknown process"
        end
      {_, _} ->
        "Process info unavailable"
    end
  end

  defp determine_overall_status(port_status, test_processes, epmd_nodes) do
    running_ports = Enum.count(port_status, fn {_, status, _, _} -> status == :running end)

    cond do
      running_ports > 0 or not Enum.empty?(test_processes) or not Enum.empty?(epmd_nodes) ->
        :running
      true ->
        :stopped
    end
  end

  defp display_real_status(status) do
    case status.overall_status do
      :running ->
        Mix.shell().info("🟢 REAL STATUS: CLUSTER IS RUNNING")
      :stopped ->
        Mix.shell().info("🔴 REAL STATUS: NO CLUSTER DETECTED")
    end

    Mix.shell().info("")
    Mix.shell().info("📊 Port Status:")

    Enum.each(status.ports, fn {port, port_status, pids, process_info} ->
      case port_status do
        :running ->
          Mix.shell().info("  🟢 Port #{port}: OCCUPIED by #{length(pids)} process(es)")
          if process_info do
            Mix.shell().info("     Process: #{process_info}")
          end
        :not_running ->
          Mix.shell().info("  ⚪ Port #{port}: Available")
      end
    end)

    if not Enum.empty?(status.test_processes) do
      Mix.shell().info("")
      Mix.shell().info("🔍 Test Node Processes:")
      Enum.each(status.test_processes, fn pid ->
        process_info = get_process_info(pid)
        Mix.shell().info("  • PID #{pid}: #{process_info}")
      end)
    end

    if not Enum.empty?(status.epmd_nodes) do
      Mix.shell().info("")
      Mix.shell().info("📡 EPMD Registered Nodes:")
      Enum.each(status.epmd_nodes, fn node ->
        Mix.shell().info("  • #{node}")
      end)
    end

    if status.overall_status == :running do
      Mix.shell().info("")
      Mix.shell().info("💡 To stop the cluster: mix cluster.test stop")
    end
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
    # Get configured port ranges instead of hardcoded ones
    config = Application.get_env(:otp_supervisor, :distributed_testing, [])
    http_base = Keyword.get(config, :http_port_base, 4200)
    dist_base = Keyword.get(config, :dist_port_base, 9200)

    test_port_ranges = [
      # HTTP ports - use configured range
      {http_base, http_base + 10},
      # Distribution ports - use configured range
      {dist_base, dist_base + 10}
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
