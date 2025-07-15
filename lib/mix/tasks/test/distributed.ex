defmodule Mix.Tasks.Test.Distributed do
  @moduledoc """
  Enhanced Mix test task with automatic distributed cluster management.
  
  This task extends the standard `mix test` functionality to seamlessly support
  distributed testing with automatic cluster lifecycle management.
  
  ## Usage
  
      mix test                         # Run non-distributed tests only
      mix test --distributed           # Run all tests with cluster management
      mix test --only distributed      # Run only distributed tests
      mix test --force-cluster         # Force cluster for all tests
      mix test --cluster-size 3        # Specify cluster size
      mix test --no-cluster            # Disable automatic cluster management
      
  ## Test Tags
  
  Tests can be tagged to indicate their distributed requirements:
  
      @tag :distributed               # Requires cluster
      @tag :cluster                   # Requires cluster  
      @tag :multi_node                # Requires cluster
      @tag cluster_size: 3            # Requires specific cluster size
      
  ## Configuration
  
  Configure distributed testing in config/test.exs:
  
      config :otp_supervisor, :distributed_testing,
        auto_cluster: true,
        default_cluster_size: 2,
        cluster_startup_timeout: 30_000,
        ci_cluster_size: 2
        
  ## Examples
  
      # Run all tests including distributed ones
      mix test --distributed
      
      # Run only distributed tests with larger cluster
      mix test --only distributed --cluster-size 4
      
      # Run specific test file with cluster
      mix test test/distributed_test.exs --distributed
      
      # Force cluster even for unit tests (useful for integration testing)
      mix test --force-cluster
  """
  
  use Mix.Task
  
  @shortdoc "Run tests with automatic distributed cluster management"
  
  alias OTPSupervisor.Testing.{AutoClusterManager, TestAnalyzer}
  
  @switches [
    distributed: :boolean,
    force_cluster: :boolean,
    no_cluster: :boolean,
    cluster_size: :integer,
    cluster_timeout: :integer,
    verbose_cluster: :boolean
  ]
  
  @aliases [
    d: :distributed,
    f: :force_cluster,
    s: :cluster_size,
    v: :verbose_cluster
  ]
  
  def run(args) do
    # Parse distributed testing specific arguments
    {distributed_opts, remaining_args} = OptionParser.parse!(args, switches: @switches, aliases: @aliases)
    
    # Determine if we need distributed testing
    needs_distributed = determine_distributed_needs(distributed_opts, remaining_args)
    
    if needs_distributed do
      run_with_cluster_management(distributed_opts, remaining_args)
    else
      run_standard_tests(remaining_args)
    end
  end
  
  # Private implementation
  
  defp determine_distributed_needs(opts, args) do
    cond do
      # Explicit flags
      opts[:distributed] -> true
      opts[:force_cluster] -> true
      Keyword.has_key?(opts, :cluster_size) -> true
      opts[:no_cluster] -> false
      
      # Check if running only distributed tests
      has_distributed_filter?(args) -> true
      
      # Check if test files contain distributed tests
      has_distributed_tests?(args) -> true
      
      # Default to no distributed testing
      true -> false
    end
  end
  
  defp has_distributed_filter?(args) do
    # Look for --only distributed, --only cluster, etc.
    case Enum.find_index(args, &(&1 == "--only")) do
      nil -> false
      index ->
        filter_value = Enum.at(args, index + 1)
        filter_value in ["distributed", "cluster", "multi_node"]
    end
  end
  
  defp has_distributed_tests?(args) do
    # Analyze test files to see if they contain distributed tests
    test_files = extract_test_files(args)
    
    if Enum.empty?(test_files) do
      # No specific files, check if any tests in the project are distributed
      TestAnalyzer.has_distributed_tests?()
    else
      # Check specific files
      Enum.any?(test_files, &TestAnalyzer.file_has_distributed_tests?/1)
    end
  end
  
  defp extract_test_files(args) do
    args
    |> Enum.filter(&String.ends_with?(&1, ".exs"))
    |> Enum.filter(&File.exists?/1)
  end
  
  defp run_with_cluster_management(distributed_opts, test_args) do
    Mix.shell().info("🚀 Starting distributed test execution...")
    
    # Build test requirements from options and analysis
    requirements = build_test_requirements(distributed_opts, test_args)
    
    # Start cluster management
    case AutoClusterManager.start_cluster_for_tests(requirements) do
      {:ok, cluster_info} ->
        if cluster_info.cluster_active do
          Mix.shell().info("✅ Cluster ready with #{length(cluster_info.nodes)} nodes")
          if distributed_opts[:verbose_cluster] do
            Mix.shell().info("   Nodes: #{inspect(cluster_info.nodes)}")
          end
        else
          Mix.shell().info("ℹ️  Running without cluster: #{cluster_info.reason}")
        end
        
        # Run the actual tests
        run_tests_with_cleanup(test_args, cluster_info)
        
      {:error, diagnosis} ->
        handle_cluster_startup_failure(diagnosis, distributed_opts, test_args)
    end
  end
  
  defp build_test_requirements(opts, args) do
    %{
      needs_cluster: not (opts[:no_cluster] || false),
      min_cluster_size: opts[:cluster_size] || determine_required_cluster_size(args),
      test_type: determine_test_type(opts, args),
      timeout: opts[:cluster_timeout],
      force_cluster: opts[:force_cluster] || false,
      verbose: opts[:verbose_cluster] || false
    }
  end
  
  defp determine_required_cluster_size(args) do
    # Analyze test files to determine minimum cluster size needed
    test_files = extract_test_files(args)
    
    if Enum.empty?(test_files) do
      # Default cluster size
      2
    else
      # Get maximum cluster size requirement from test files
      test_files
      |> Enum.map(&TestAnalyzer.get_required_cluster_size/1)
      |> Enum.max(fn -> 2 end)
    end
  end
  
  defp determine_test_type(opts, args) do
    cond do
      opts[:force_cluster] -> :force_cluster
      has_distributed_filter?(args) -> :distributed_only
      opts[:distributed] -> :mixed
      true -> :auto_detect
    end
  end
  
  defp run_tests_with_cleanup(test_args, cluster_info) do
    try do
      # Set environment variables for tests to know about cluster
      if cluster_info.cluster_active do
        set_cluster_env_vars(cluster_info)
      end
      
      # Run the standard Mix.Tasks.Test with remaining arguments
      exit_code = run_standard_tests(test_args)
      
      # Return the same exit code
      if exit_code != 0 do
        System.at_exit(fn _ -> exit({:shutdown, exit_code}) end)
      end
      
    after
      # Always cleanup managed clusters
      cleanup_cluster_if_managed()
      cleanup_cluster_env_vars()
    end
  end
  
  defp set_cluster_env_vars(cluster_info) do
    System.put_env("DISTRIBUTED_TEST_CLUSTER_ACTIVE", "true")
    System.put_env("DISTRIBUTED_TEST_CLUSTER_SIZE", Integer.to_string(length(cluster_info.nodes)))
    System.put_env("DISTRIBUTED_TEST_CLUSTER_NODES", Enum.join(cluster_info.nodes, ","))
  end
  
  defp cleanup_cluster_env_vars do
    System.delete_env("DISTRIBUTED_TEST_CLUSTER_ACTIVE")
    System.delete_env("DISTRIBUTED_TEST_CLUSTER_SIZE") 
    System.delete_env("DISTRIBUTED_TEST_CLUSTER_NODES")
  end
  
  defp cleanup_cluster_if_managed do
    try do
      case AutoClusterManager.cleanup_if_managed() do
        :ok -> 
          Mix.shell().info("🧹 Cluster cleanup completed")
        {:warning, reason} -> 
          Mix.shell().info("⚠️  Cluster cleanup had issues: #{inspect(reason)}")
      end
    catch
      :exit, _ -> 
        Mix.shell().info("⚠️  Cluster cleanup skipped (manager not available)")
    end
  end
  
  defp run_standard_tests(args) do
    # Run the standard Mix test task
    try do
      Mix.Tasks.Test.run(args)
      0  # Success
    catch
      :exit, {:shutdown, code} when is_integer(code) -> code
      :exit, _ -> 1
    end
  end
  
  defp handle_cluster_startup_failure(diagnosis, _opts, _test_args) do
    Mix.shell().error("❌ Cluster startup failed: #{diagnosis.problem}")
    
    # Show solutions
    if not Enum.empty?(diagnosis.solutions) do
      Mix.shell().info("\n💡 Possible solutions:")
      Enum.each(diagnosis.solutions, fn solution ->
        Mix.shell().info("   • #{solution}")
      end)
    end
    
    # Show retry suggestions
    if not Enum.empty?(diagnosis.retry_suggestions) do
      Mix.shell().info("\n🔄 Retry suggestions:")
      Enum.each(diagnosis.retry_suggestions, fn suggestion ->
        Mix.shell().info("   • #{suggestion}")
      end)
    end
    
    # NO FALLBACK STRATEGIES - FAIL HARD FOR DISTRIBUTED TESTS
    Mix.shell().error("\n🛑 Distributed tests require a working cluster - cannot proceed without one")
    Mix.shell().error("🛑 Fix the cluster issues above and try again")
    System.halt(1)
  end
  

end