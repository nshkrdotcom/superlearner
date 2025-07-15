defmodule OTPSupervisor.TestCluster.HealthChecker do
  @moduledoc """
  Comprehensive health checking for distributed test clusters.
  
  Validates:
  - Node connectivity and communication
  - Code version synchronization  
  - Arsenal operations functionality
  - LibCluster formation
  - Network partition detection
  - Performance metrics
  
  Provides detailed diagnostics for troubleshooting distributed issues.
  """
  
  require Logger
  
  @doc """
  Perform comprehensive health check on the entire cluster.
  """
  def comprehensive_health_check(nodes) do
    Logger.info("Running comprehensive health check on #{map_size(nodes)} nodes")
    
    checks = [
      {:connectivity, "Node connectivity and communication", &check_node_connectivity/1},
      {:cluster_formation, "LibCluster formation", &check_cluster_formation/1},
      {:code_sync, "Code version synchronization", &check_code_synchronization/1},
      {:arsenal_operations, "Arsenal operations functionality", &check_arsenal_operations/1},
      {:performance, "Performance and latency", &check_performance_metrics/1},
      {:resource_usage, "Resource usage", &check_resource_usage/1}
    ]
    
    results = run_health_checks(nodes, checks)
    
    overall_status = %{
      all_passed: Enum.all?(results.checks, fn {_, result} -> result.passed end),
      checks: results.checks,
      summary: build_health_summary(results.checks),
      suggestions: build_troubleshooting_suggestions(results.checks),
      timestamp: DateTime.utc_now()
    }
    
    if overall_status.all_passed do
      Logger.info("✅ All health checks passed")
    else
      Logger.warning("❌ Some health checks failed")
    end
    
    {:ok, overall_status}
  end
  
  @doc """
  Perform basic health check for cluster startup validation.
  """
  def basic_health_check(nodes) do
    Logger.debug("Running basic health check on #{map_size(nodes)} nodes")
    
    basic_checks = [
      {:connectivity, "Basic node connectivity", &check_node_connectivity/1},
      {:cluster_formation, "Basic cluster formation", &check_basic_cluster_formation/1}
    ]
    
    results = run_health_checks(nodes, basic_checks)
    
    if Enum.all?(results.checks, fn {_, result} -> result.passed end) do
      {:ok, results}
    else
      failed_checks = 
        results.checks
        |> Enum.filter(fn {_, result} -> not result.passed end)
        |> Enum.map(fn {name, result} -> {name, result.message} end)
      
      {:error, {:basic_health_failed, failed_checks}}
    end
  end
  
  # Private implementation
  
  defp run_health_checks(nodes, checks) do
    check_results = 
      checks
      |> Enum.map(fn {name, description, check_fn} ->
        Logger.debug("Running health check: #{description}")
        
        result = 
          try do
            case check_fn.(nodes) do
              :ok -> 
                %{passed: true, message: "✅ #{description}"}
              {:ok, details} -> 
                %{passed: true, message: "✅ #{description}", details: details}
              {:error, reason} -> 
                %{passed: false, message: "❌ #{description}", error: reason}
            end
          rescue
            error ->
              Logger.error("Health check #{name} crashed: #{inspect(error)}")
              %{passed: false, message: "❌ #{description} (crashed)", error: error}
          end
        
        {name, result}
      end)
    
    %{checks: check_results}
  end
  
  # Individual health check implementations
  
  defp check_node_connectivity(nodes) do
    Logger.debug("Checking node connectivity...")
    
    node_list = Map.values(nodes)
    connectivity_results = 
      node_list
      |> Enum.map(fn node ->
        case :rpc.call(node, Node, :self, [], 5000) do
          ^node -> 
            {node, :ok}
          {:badrpc, reason} -> 
            {node, {:error, reason}}
          other -> 
            {node, {:error, {:unexpected_response, other}}}
        end
      end)
    
    failed_nodes = 
      connectivity_results
      |> Enum.filter(fn {_, result} -> result != :ok end)
    
    if Enum.empty?(failed_nodes) do
      :ok
    else
      {:error, {:connectivity_failed, failed_nodes}}
    end
  end
  
  defp check_cluster_formation(nodes) do
    Logger.debug("Checking LibCluster formation...")
    
    node_list = Map.values(nodes)
    
    # Check if all nodes can see each other
    formation_results = 
      node_list
      |> Enum.map(fn node ->
        case :rpc.call(node, Node, :list, [], 5000) do
          {:badrpc, reason} ->
            {node, {:error, reason}}
          connected_nodes ->
            expected_nodes = node_list -- [node]
            missing_nodes = expected_nodes -- connected_nodes
            
            if Enum.empty?(missing_nodes) do
              {node, :ok}
            else
              {node, {:error, {:missing_connections, missing_nodes}}}
            end
        end
      end)
    
    failed_formations = 
      formation_results
      |> Enum.filter(fn {_, result} -> result != :ok end)
    
    if Enum.empty?(failed_formations) do
      # Also check LibCluster specifically if available
      check_libcluster_status(nodes)
    else
      {:error, {:cluster_formation_failed, failed_formations}}
    end
  end
  
  defp check_basic_cluster_formation(nodes) do
    # Simplified version for startup validation
    node_list = Map.values(nodes)
    
    # Just check that nodes can ping each other
    ping_results = 
      node_list
      |> Enum.map(fn node ->
        case :net_adm.ping(node) do
          :pong -> {node, :ok}
          :pang -> {node, {:error, :ping_failed}}
        end
      end)
    
    failed_pings = 
      ping_results
      |> Enum.filter(fn {_, result} -> result != :ok end)
    
    if Enum.empty?(failed_pings) do
      :ok
    else
      {:error, {:ping_failed, failed_pings}}
    end
  end
  
  defp check_libcluster_status(nodes) do
    # Check if LibCluster is running and has formed the cluster
    node_list = Map.values(nodes)
    
    libcluster_results = 
      node_list
      |> Enum.map(fn node ->
        try do
          case :rpc.call(node, OTPSupervisor.Distributed.ClusterStateManager, :get_cluster_topology, [], 5000) do
            {:badrpc, reason} ->
              {node, {:error, {:cluster_state_manager_failed, reason}}}
            topology ->
              expected_count = length(node_list)
              actual_count = length(topology.nodes)
              
              if actual_count >= expected_count do
                {node, :ok}
              else
                {node, {:error, {:incomplete_topology, actual_count, expected_count}}}
              end
          end
        rescue
          _ ->
            {node, {:error, :cluster_state_manager_unavailable}}
        end
      end)
    
    failed_libcluster = 
      libcluster_results
      |> Enum.filter(fn {_, result} -> result != :ok end)
    
    if Enum.empty?(failed_libcluster) do
      :ok
    else
      {:error, {:libcluster_check_failed, failed_libcluster}}
    end
  end
  
  defp check_code_synchronization(nodes) do
    Logger.debug("Checking code synchronization...")
    
    node_list = Map.values(nodes)
    
    # Check that all nodes have the same version of our key modules
    key_modules = [
      OTPSupervisor.Distributed.ClusterStateManager,
      OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth,
      OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList
    ]
    
    sync_results = 
      key_modules
      |> Enum.map(fn module ->
        module_versions = 
          node_list
          |> Enum.map(fn node ->
            case :rpc.call(node, :code, :get_object_code, [module], 5000) do
              {:badrpc, reason} ->
                {node, {:error, reason}}
              {^module, binary, _filename} ->
                # Use a hash of the binary to check if versions match
                hash = :crypto.hash(:md5, binary) |> Base.encode16()
                {node, {:ok, hash}}
              :error ->
                {node, {:error, :module_not_found}}
            end
          end)
        
        # Check if all versions are the same
        version_hashes = 
          module_versions
          |> Enum.filter(fn {_, result} -> match?({:ok, _}, result) end)
          |> Enum.map(fn {_, {:ok, hash}} -> hash end)
        
        unique_versions = Enum.uniq(version_hashes)
        
        if length(unique_versions) <= 1 do
          {module, :ok}
        else
          {module, {:error, {:version_mismatch, module_versions}}}
        end
      end)
    
    failed_sync = 
      sync_results
      |> Enum.filter(fn {_, result} -> result != :ok end)
    
    if Enum.empty?(failed_sync) do
      :ok
    else
      {:error, {:code_sync_failed, failed_sync}}
    end
  end
  
  defp check_arsenal_operations(_nodes) do
    Logger.debug("Checking Arsenal operations...")
    
    # Test key Arsenal operations across the cluster
    operations_to_test = [
      {OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth, %{}},
      {OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopology, %{}},
      {OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList, %{"limit" => 10}}
    ]
    
    operation_results = 
      operations_to_test
      |> Enum.map(fn {operation_module, params} ->
        try do
          case operation_module.validate_params(params) do
            {:ok, validated_params} ->
              case operation_module.execute(validated_params) do
                {:ok, _data} ->
                  {operation_module, :ok}
                {:error, reason} ->
                  {operation_module, {:error, reason}}
              end
            {:error, reason} ->
              {operation_module, {:error, {:validation_failed, reason}}}
          end
        rescue
          error ->
            {operation_module, {:error, {:operation_crashed, error}}}
        end
      end)
    
    failed_operations = 
      operation_results
      |> Enum.filter(fn {_, result} -> result != :ok end)
    
    if Enum.empty?(failed_operations) do
      :ok
    else
      {:error, {:arsenal_operations_failed, failed_operations}}
    end
  end
  
  defp check_performance_metrics(nodes) do
    Logger.debug("Checking performance metrics...")
    
    node_list = Map.values(nodes)
    
    # Measure inter-node latency
    latency_results = 
      node_list
      |> Enum.map(fn node ->
        start_time = System.monotonic_time(:microsecond)
        
        case :rpc.call(node, :erlang, :system_time, [:microsecond], 5000) do
          {:badrpc, reason} ->
            {node, {:error, reason}}
          _remote_time ->
            end_time = System.monotonic_time(:microsecond)
            latency_us = end_time - start_time
            {node, {:ok, latency_us}}
        end
      end)
    
    # Check if any latencies are too high (> 100ms)
    high_latency_nodes = 
      latency_results
      |> Enum.filter(fn 
        {_, {:ok, latency}} -> latency > 100_000  # 100ms in microseconds
        {_, {:error, _}} -> true
      end)
    
    if Enum.empty?(high_latency_nodes) do
      avg_latency = 
        latency_results
        |> Enum.filter(fn {_, result} -> match?({:ok, _}, result) end)
        |> Enum.map(fn {_, {:ok, latency}} -> latency end)
        |> Enum.sum()
        |> div(length(node_list))
      
      {:ok, %{average_latency_us: avg_latency}}
    else
      {:error, {:high_latency, high_latency_nodes}}
    end
  end
  
  defp check_resource_usage(nodes) do
    Logger.debug("Checking resource usage...")
    
    node_list = Map.values(nodes)
    
    resource_results = 
      node_list
      |> Enum.map(fn node ->
        case :rpc.call(node, :erlang, :memory, [], 5000) do
          {:badrpc, reason} ->
            {node, {:error, reason}}
          memory_info ->
            total_memory = Keyword.get(memory_info, :total, 0)
            process_memory = Keyword.get(memory_info, :processes, 0)
            
            # Check if memory usage is reasonable (< 500MB total)
            if total_memory < 500_000_000 do
              {node, {:ok, %{total: total_memory, processes: process_memory}}}
            else
              {node, {:warning, {:high_memory_usage, total_memory}}}
            end
        end
      end)
    
    high_memory_nodes = 
      resource_results
      |> Enum.filter(fn 
        {_, {:warning, _}} -> true
        {_, {:error, _}} -> true
        _ -> false
      end)
    
    if Enum.empty?(high_memory_nodes) do
      total_cluster_memory = 
        resource_results
        |> Enum.filter(fn {_, result} -> match?({:ok, _}, result) end)
        |> Enum.map(fn {_, {:ok, %{total: total}}} -> total end)
        |> Enum.sum()
      
      {:ok, %{total_cluster_memory: total_cluster_memory}}
    else
      {:error, {:resource_issues, high_memory_nodes}}
    end
  end
  
  # Helper functions for building results
  
  defp build_health_summary(checks) do
    total_checks = length(checks)
    passed_checks = Enum.count(checks, fn {_, result} -> result.passed end)
    
    %{
      total: total_checks,
      passed: passed_checks,
      failed: total_checks - passed_checks,
      success_rate: if(total_checks > 0, do: passed_checks / total_checks * 100, else: 0)
    }
  end
  
  defp build_troubleshooting_suggestions(checks) do
    failed_checks = 
      checks
      |> Enum.filter(fn {_, result} -> not result.passed end)
    
    suggestions = 
      failed_checks
      |> Enum.flat_map(fn {check_name, _result} ->
        case check_name do
          :connectivity ->
            [
              "Check network connectivity between nodes",
              "Verify EPMD is running: epmd -daemon",
              "Check firewall settings for Erlang distribution ports"
            ]
            
          :cluster_formation ->
            [
              "Verify LibCluster configuration",
              "Check that all nodes have the same cookie",
              "Ensure nodes can resolve each other's hostnames"
            ]
            
          :code_sync ->
            [
              "Recompile and restart the cluster",
              "Check that all nodes are running the same code version",
              "Verify code paths are consistent across nodes"
            ]
            
          :arsenal_operations ->
            [
              "Check Arsenal operation logs for specific errors",
              "Verify distributed GenServers are running on all nodes",
              "Test individual operations manually"
            ]
            
          :performance ->
            [
              "Check network latency between nodes",
              "Verify system resources are not overloaded",
              "Consider reducing test load or timeout values"
            ]
            
          :resource_usage ->
            [
              "Monitor memory usage on test nodes",
              "Check for memory leaks in test processes",
              "Consider reducing test scope or adding cleanup"
            ]
            
          _ ->
            ["Check logs for specific error details"]
        end
      end)
      |> Enum.uniq()
    
    if Enum.empty?(suggestions) do
      ["All health checks passed - cluster is healthy!"]
    else
      suggestions
    end
  end
end