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
    
    with :ok <- ensure_manager_started(),
         {:ok, nodes} <- Manager.start_cluster() do
      Mix.shell().info("✅ Test cluster started successfully!")
      Mix.shell().info("📍 Nodes: #{inspect(nodes)}")
      show_cluster_info(nodes)
    else
      {:error, reason} ->
        Mix.shell().error("❌ Failed to start cluster: #{inspect(reason)}")
        suggest_troubleshooting()
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
        Mix.shell().error("❌ Failed to restart cluster: #{inspect(reason)}")
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
    
    with :ok <- ensure_manager_started(),
         :ok <- Manager.clean_all() do
      Mix.shell().info("✅ Cleanup completed successfully!")
    else
      {:error, reason} ->
        Mix.shell().error("❌ Cleanup failed: #{inspect(reason)}")
        System.halt(1)
    end
  end
  
  defp run_full_cycle do
    Mix.shell().info("🎯 Running full distributed test cycle...")
    
    with {:ok, _nodes} <- Manager.start_cluster(),
         :ok <- run_distributed_tests(),
         :ok <- Manager.stop_cluster() do
      Mix.shell().info("✅ Full test cycle completed successfully!")
    else
      {:error, reason} ->
        Mix.shell().error("❌ Test cycle failed: #{inspect(reason)}")
        Manager.stop_cluster()  # Cleanup on failure
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
    
    checks = [
      {"EPMD availability", &check_epmd/0},
      {"Network configuration", &check_network/0},
      {"Port availability", &check_ports/0},
      {"Distributed Erlang", &check_distributed_erlang/0},
      {"Test environment", &check_test_environment/0}
    ]
    
    results = Enum.map(checks, fn {name, check_fn} ->
      Mix.shell().info("  Checking #{name}...")
      result = check_fn.()
      {name, result}
    end)
    
    display_preflight_results(results)
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
    case System.cmd("mix", ["test", "--only", "real_nodes", "--timeout", "60000"], stderr_to_stdout: true) do
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
  
  defp suggest_troubleshooting do
    Mix.shell().info("")
    Mix.shell().info("💡 Troubleshooting suggestions:")
    Mix.shell().info("  • Check if ports 4100-4102 are available")
    Mix.shell().info("  • Ensure EPMD is running: epmd -daemon")
    Mix.shell().info("  • Try cleaning up: mix cluster.test clean")
    Mix.shell().info("  • Check logs: mix cluster.test logs")
  end
  
  defp show_help do
    Mix.shell().info(@moduledoc)
  end
  
  # Pre-flight check functions
  
  defp check_epmd do
    case System.cmd("epmd", ["-names"], stderr_to_stdout: true) do
      {output, 0} ->
        if String.contains?(output, "up and running") do
          {:ok, "EPMD is running"}
        else
          {:warning, "EPMD may not be running properly"}
        end
      {_, _} ->
        {:error, "EPMD is not available - run 'epmd -daemon'"}
    end
  end
  
  defp check_network do
    case :inet.gethostbyname(~c"127.0.0.1") do
      {:ok, _} -> {:ok, "Network configuration is valid"}
      {:error, reason} -> {:error, "Network issue: #{inspect(reason)}"}
    end
  end
  
  defp check_ports do
    test_ports = [4100, 4101, 4102, 9100, 9101, 9102]
    
    unavailable_ports = 
      test_ports
      |> Enum.filter(fn port ->
        case :gen_tcp.listen(port, []) do
          {:ok, socket} -> 
            :gen_tcp.close(socket)
            false
          {:error, _} -> 
            true
        end
      end)
    
    if Enum.empty?(unavailable_ports) do
      {:ok, "All test ports are available"}
    else
      {:error, "Ports in use: #{inspect(unavailable_ports)}"}
    end
  end
  
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
  
  defp display_preflight_results(results) do
    Mix.shell().info("")
    Mix.shell().info("🔍 Pre-flight Check Results:")
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
      Mix.shell().info("✅ All pre-flight checks passed! Environment is ready for distributed testing.")
    else
      Mix.shell().info("❌ Some pre-flight checks failed. Please address the issues above.")
      Mix.shell().info("")
      Mix.shell().info("💡 Common solutions:")
      Mix.shell().info("  • Start EPMD: epmd -daemon")
      Mix.shell().info("  • Kill processes using test ports: lsof -ti:4100,4101,4102 | xargs kill")
      Mix.shell().info("  • Check network connectivity")
      System.halt(1)
    end
  end
end