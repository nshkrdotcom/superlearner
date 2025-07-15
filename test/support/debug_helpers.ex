defmodule DebugHelpers do
  @moduledoc """
  Debugging utilities for distributed tests.
  
  Provides functions to inspect cluster state, trace operations,
  and debug distributed functionality during testing.
  """
  
  alias OTPSupervisor.Distributed.{ToolManager, ClusterStateManager, SingleNodeSimulator}
  
  @doc """
  Dump comprehensive cluster state information.
  """
  def dump_cluster_state do
    IO.puts("\n" <> String.duplicate("=", 60))
    IO.puts("CLUSTER STATE DUMP")
    IO.puts(String.duplicate("=", 60))
    
    dump_basic_info()
    dump_tool_manager_state()
    dump_cluster_manager_state()
    dump_simulation_state()
    dump_process_distribution()
    
    IO.puts(String.duplicate("=", 60) <> "\n")
  end
  
  @doc """
  Trace an RPC call with detailed logging.
  """
  def trace_rpc_call(node, module, function, args) do
    IO.puts("🔍 RPC CALL: #{node}.#{module}.#{function}(#{format_args(args)})")
    
    start_time = System.monotonic_time(:microsecond)
    
    result = try do
      :rpc.call(node, module, function, args)
    catch
      kind, error ->
        IO.puts("❌ RPC ERROR: #{kind} - #{inspect(error)}")
        {:error, {kind, error}}
    end
    
    end_time = System.monotonic_time(:microsecond)
    duration = end_time - start_time
    
    IO.puts("✅ RPC RESULT (#{duration}μs): #{format_result(result)}")
    
    result
  end
  
  @doc """
  Monitor cluster changes for a period of time.
  """
  def monitor_cluster_changes(duration_ms \\ 5000) do
    IO.puts("🔍 Monitoring cluster changes for #{duration_ms}ms...")
    
    initial_state = get_cluster_snapshot()
    
    :timer.sleep(duration_ms)
    
    final_state = get_cluster_snapshot()
    
    compare_cluster_snapshots(initial_state, final_state)
  end
  
  @doc """
  Verify that all distributed components are responding.
  """
  def health_check_distributed_components do
    IO.puts("🏥 DISTRIBUTED HEALTH CHECK")
    IO.puts(String.duplicate("-", 40))
    
    checks = [
      {"ToolManager", &check_tool_manager/0},
      {"ClusterStateManager", &check_cluster_state_manager/0},
      {"SingleNodeSimulator", &check_single_node_simulator/0},
      {"Arsenal Operations", &check_arsenal_operations/0}
    ]
    
    results = Enum.map(checks, fn {name, check_fn} ->
      IO.write("#{name}... ")
      
      result = try do
        check_fn.()
        IO.puts("✅ OK")
        :ok
      catch
        kind, error ->
          IO.puts("❌ FAILED: #{kind} - #{inspect(error)}")
          {:error, {kind, error}}
      end
      
      {name, result}
    end)
    
    failed_checks = Enum.filter(results, fn {_, result} -> result != :ok end)
    
    if failed_checks == [] do
      IO.puts("🎉 All distributed components are healthy!")
    else
      IO.puts("⚠️  #{length(failed_checks)} component(s) failed health check")
    end
    
    results
  end
  
  @doc """
  Pretty print a data structure for debugging.
  """
  def pretty_inspect(data, label \\ nil) do
    if label do
      IO.puts("#{label}:")
    end
    
    IO.puts(inspect(data, pretty: true, width: 80, limit: :infinity))
  end
  
  @doc """
  Log a test step with formatting.
  """
  def log_test_step(step_name, details \\ nil) do
    IO.puts("🧪 TEST STEP: #{step_name}")
    
    if details do
      IO.puts("   Details: #{details}")
    end
  end
  
  @doc """
  Measure and log execution time of a function.
  """
  def time_execution(label, fun) do
    IO.write("⏱️  #{label}... ")
    
    {time, result} = :timer.tc(fun)
    
    IO.puts("completed in #{format_time(time)}")
    
    result
  end
  
  # Private helper functions
  
  defp dump_basic_info do
    IO.puts("Node Information:")
    IO.puts("  Current Node: #{Node.self()}")
    IO.puts("  Connected Nodes: #{inspect(Node.list())}")
    IO.puts("  Node Alive: #{Node.alive?()}")
    IO.puts("  Cookie: #{Node.get_cookie()}")
    IO.puts("")
  end
  
  defp dump_tool_manager_state do
    IO.puts("Tool Manager State:")
    
    try do
      status = ToolManager.get_cluster_status()
      IO.puts("  Mode: #{status.mode}")
      IO.puts("  Nodes: #{inspect(status.nodes)}")
      IO.puts("  Connected: #{inspect(status.connected_nodes)}")
      IO.puts("  Tools: #{inspect(status.tools)}")
    catch
      kind, error ->
        IO.puts("  ERROR: #{kind} - #{inspect(error)}")
    end
    
    IO.puts("")
  end
  
  defp dump_cluster_manager_state do
    IO.puts("Cluster State Manager:")
    
    try do
      topology = ClusterStateManager.get_cluster_topology()
      IO.puts("  Total Nodes: #{topology.total_nodes}")
      IO.puts("  Cluster Name: #{topology.cluster_name}")
      IO.puts("  Formation Time: #{topology.formation_time}")
      
      partition_status = ClusterStateManager.get_partition_status()
      IO.puts("  Partition Status: #{partition_status}")
    catch
      kind, error ->
        IO.puts("  ERROR: #{kind} - #{inspect(error)}")
    end
    
    IO.puts("")
  end
  
  defp dump_simulation_state do
    IO.puts("Single Node Simulator:")
    
    try do
      enabled = SingleNodeSimulator.simulation_enabled?()
      IO.puts("  Simulation Enabled: #{enabled}")
      
      if enabled do
        nodes = SingleNodeSimulator.get_simulated_nodes()
        IO.puts("  Simulated Nodes: #{inspect(nodes)}")
        
        topology = SingleNodeSimulator.get_simulated_topology()
        IO.puts("  Failed Nodes: #{inspect(topology.failed_nodes)}")
        IO.puts("  Partitioned Nodes: #{inspect(topology.partitioned_nodes)}")
      end
    catch
      kind, error ->
        IO.puts("  ERROR: #{kind} - #{inspect(error)}")
    end
    
    IO.puts("")
  end
  
  defp dump_process_distribution do
    IO.puts("Process Distribution:")
    
    try do
      distribution = ClusterStateManager.get_process_distribution()
      
      Enum.each(distribution, fn {node, processes} ->
        IO.puts("  #{node}: #{length(processes)} processes")
      end)
    catch
      kind, error ->
        IO.puts("  ERROR: #{kind} - #{inspect(error)}")
    end
    
    IO.puts("")
  end
  
  defp format_args(args) when is_list(args) do
    args
    |> Enum.map(&inspect/1)
    |> Enum.join(", ")
  end
  
  defp format_args(args), do: inspect(args)
  
  defp format_result(result) do
    case result do
      {:ok, data} -> "OK - #{inspect(data, limit: 3)}"
      {:error, reason} -> "ERROR - #{inspect(reason)}"
      other -> inspect(other, limit: 3)
    end
  end
  
  defp format_time(microseconds) do
    cond do
      microseconds < 1_000 -> "#{microseconds}μs"
      microseconds < 1_000_000 -> "#{Float.round(microseconds / 1_000, 2)}ms"
      true -> "#{Float.round(microseconds / 1_000_000, 2)}s"
    end
  end
  
  defp get_cluster_snapshot do
    %{
      timestamp: DateTime.utc_now(),
      nodes: Node.list(),
      tool_manager: safe_call(&ToolManager.get_cluster_status/0),
      cluster_topology: safe_call(&ClusterStateManager.get_cluster_topology/0),
      simulation_enabled: safe_call(&SingleNodeSimulator.simulation_enabled?/0)
    }
  end
  
  defp compare_cluster_snapshots(initial, final) do
    IO.puts("📊 CLUSTER CHANGES DETECTED:")
    
    # Node changes
    added_nodes = final.nodes -- initial.nodes
    removed_nodes = initial.nodes -- final.nodes
    
    if added_nodes != [] do
      IO.puts("  ➕ Nodes Added: #{inspect(added_nodes)}")
    end
    
    if removed_nodes != [] do
      IO.puts("  ➖ Nodes Removed: #{inspect(removed_nodes)}")
    end
    
    # Mode changes
    initial_mode = get_in(initial.tool_manager, [:mode])
    final_mode = get_in(final.tool_manager, [:mode])
    
    if initial_mode != final_mode do
      IO.puts("  🔄 Mode Changed: #{initial_mode} → #{final_mode}")
    end
    
    # Simulation changes
    if initial.simulation_enabled != final.simulation_enabled do
      IO.puts("  🎭 Simulation: #{initial.simulation_enabled} → #{final.simulation_enabled}")
    end
    
    if added_nodes == [] and removed_nodes == [] and initial_mode == final_mode do
      IO.puts("  ✨ No significant changes detected")
    end
  end
  
  defp safe_call(fun) do
    try do
      fun.()
    catch
      _, _ -> :error
    end
  end
  
  defp check_tool_manager do
    status = ToolManager.get_cluster_status()
    unless is_map(status) and Map.has_key?(status, :mode) do
      raise "Invalid ToolManager response"
    end
    :ok
  end
  
  defp check_cluster_state_manager do
    topology = ClusterStateManager.get_cluster_topology()
    unless is_map(topology) and Map.has_key?(topology, :nodes) do
      raise "Invalid ClusterStateManager response"
    end
    :ok
  end
  
  defp check_single_node_simulator do
    enabled = SingleNodeSimulator.simulation_enabled?()
    unless is_boolean(enabled) do
      raise "Invalid SingleNodeSimulator response"
    end
    :ok
  end
  
  defp check_arsenal_operations do
    # Test a simple Arsenal operation
    {:ok, _result} = OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterHealth.execute(%{})
    :ok
  end
end