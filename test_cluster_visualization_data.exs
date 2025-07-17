#!/usr/bin/env elixir

# Test script to verify the enhanced ClusterSupervisionTrees operation
# and the data merging logic in ClusterVisualizationLive

Mix.install([
  {:jason, "~> 1.4"}
])

# Start the OTP Supervisor application
Application.ensure_all_started(:otp_supervisor)

# Test the enhanced ClusterSupervisionTrees operation
IO.puts("Testing enhanced ClusterSupervisionTrees operation...")

supervision_params = %{
  "include_children" => true,
  "include_process_details" => true,
  "max_depth" => 3
}

case OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterSupervisionTrees.execute(
       supervision_params
     ) do
  {:ok, result} ->
    IO.puts("✓ ClusterSupervisionTrees operation successful")
    IO.puts("  Nodes: #{length(result.summary.nodes_queried)}")
    IO.puts("  Total supervisors: #{result.summary.total_supervisors}")

    # Check if visualization metadata is present
    first_node_data = result.supervision_trees |> Enum.take(1) |> List.first()

    if first_node_data do
      {node, node_data} = first_node_data
      IO.puts("  Node: #{node}")

      if length(node_data.supervisors) > 0 do
        first_supervisor = List.first(node_data.supervisors)
        IO.puts("  First supervisor has level: #{Map.get(first_supervisor, :level, "MISSING")}")
        IO.puts("  First supervisor has type: #{Map.get(first_supervisor, :type, "MISSING")}")

        # Check children if present
        if Map.has_key?(first_supervisor, :children) and length(first_supervisor.children) > 0 do
          first_child = List.first(first_supervisor.children)
          IO.puts("  First child has level: #{Map.get(first_child, :level, "MISSING")}")
          IO.puts("  First child has node: #{Map.get(first_child, :node, "MISSING")}")
        end
      end
    end

  {:error, reason} ->
    IO.puts("✗ ClusterSupervisionTrees operation failed: #{reason}")
end

IO.puts("\nTesting ProcessList operation...")

process_params = %{
  "include_details" => true,
  "limit" => 100
}

case OTPSupervisor.Core.Arsenal.Operations.Distributed.ProcessList.execute(process_params) do
  {:ok, result} ->
    IO.puts("✓ ProcessList operation successful")
    IO.puts("  Total processes: #{result.total_count}")
    IO.puts("  Nodes queried: #{length(result.nodes_queried)}")

    # Show sample process data
    if length(result.processes) > 0 do
      first_process = List.first(result.processes)
      IO.puts("  First process PID: #{first_process.pid}")
      IO.puts("  First process node: #{first_process.node}")
      IO.puts("  First process has memory: #{Map.has_key?(first_process, :memory)}")

      IO.puts(
        "  First process has message_queue_len: #{Map.has_key?(first_process, :message_queue_len)}"
      )
    end

  {:error, reason} ->
    IO.puts("✗ ProcessList operation failed: #{reason}")
end

IO.puts("\nTesting data merging logic...")

# Test the data merging logic by calling the private functions
# We'll simulate this by creating a simple test module

defmodule TestDataMerging do
  def test_collect_supervised_pids do
    # Sample supervision tree data
    supervisors = [
      %{
        pid: "<0.123.0>",
        children: [
          %{pid: "<0.124.0>", children: []},
          %{
            pid: "<0.125.0>",
            children: [
              %{pid: "<0.126.0>", children: []}
            ]
          }
        ]
      }
    ]

    # Collect PIDs
    supervised_pids = collect_supervised_pids(supervisors)

    expected_pids = ["<0.123.0>", "<0.124.0>", "<0.125.0>", "<0.126.0>"]
    actual_pids = MapSet.to_list(supervised_pids) |> Enum.sort()

    IO.puts("Expected PIDs: #{inspect(expected_pids)}")
    IO.puts("Actual PIDs: #{inspect(actual_pids)}")

    if Enum.sort(expected_pids) == actual_pids do
      IO.puts("✓ PID collection works correctly")
    else
      IO.puts("✗ PID collection failed")
    end
  end

  defp collect_supervised_pids(supervisors) do
    supervisors
    |> Enum.reduce(MapSet.new(), fn supervisor, acc ->
      supervisor_pids = collect_pids_from_supervisor(supervisor)
      MapSet.union(acc, supervisor_pids)
    end)
  end

  defp collect_pids_from_supervisor(supervisor) do
    base_pids = MapSet.new([supervisor.pid])

    children_pids =
      case Map.get(supervisor, :children) do
        nil ->
          MapSet.new()

        children ->
          children
          |> Enum.reduce(MapSet.new(), fn child, acc ->
            child_pids = collect_pids_from_process(child)
            MapSet.union(acc, child_pids)
          end)
      end

    MapSet.union(base_pids, children_pids)
  end

  defp collect_pids_from_process(process) do
    base_pids = MapSet.new([process.pid])

    children_pids =
      case Map.get(process, :children) do
        nil ->
          MapSet.new()

        children ->
          children
          |> Enum.reduce(MapSet.new(), fn child, acc ->
            child_pids = collect_pids_from_process(child)
            MapSet.union(acc, child_pids)
          end)
      end

    MapSet.union(base_pids, children_pids)
  end
end

TestDataMerging.test_collect_supervised_pids()

IO.puts("\n✓ All tests completed!")
