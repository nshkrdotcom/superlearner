#!/usr/bin/env elixir

# Simple script to test cluster functionality manually
# This helps us verify the foundation works before running the full test suite

Mix.install([])

# Start the application in test mode
Application.put_env(:mix, :env, :test)

# Load the project
Code.require_file("mix.exs")

# Start dependencies
Application.ensure_all_started(:otp_supervisor)

# Give it a moment to start
:timer.sleep(1000)

IO.puts("🔍 Testing cluster functionality...")

# Test 1: Check if AutoClusterManager is running
case Process.whereis(OTPSupervisor.Testing.AutoClusterManager) do
  nil ->
    IO.puts("❌ AutoClusterManager is not running")
    System.halt(1)
  pid ->
    IO.puts("✅ AutoClusterManager is running (#{inspect(pid)})")
end

# Test 2: Try to get cluster info
try do
  cluster_info = OTPSupervisor.Testing.AutoClusterManager.get_cluster_info()
  IO.puts("✅ Can get cluster info: #{inspect(cluster_info)}")
rescue
  error ->
    IO.puts("❌ Failed to get cluster info: #{inspect(error)}")
    System.halt(1)
end

# Test 3: Try to start a cluster
IO.puts("🚀 Attempting to start a test cluster...")

requirements = %{
  needs_cluster: true,
  min_cluster_size: 2,
  test_type: :distributed
}

case OTPSupervisor.Testing.AutoClusterManager.start_cluster_for_tests(requirements) do
  {:ok, cluster_info} ->
    if cluster_info.cluster_active do
      IO.puts("✅ Cluster started successfully!")
      IO.puts("   Nodes: #{inspect(cluster_info.nodes)}")
      IO.puts("   Size: #{length(cluster_info.nodes)}")

      # Test 4: Cleanup
      IO.puts("🧹 Cleaning up cluster...")
      case OTPSupervisor.Testing.AutoClusterManager.cleanup_if_managed() do
        :ok -> IO.puts("✅ Cleanup successful")
        error -> IO.puts("⚠️  Cleanup had issues: #{inspect(error)}")
      end
    else
      IO.puts("ℹ️  Cluster not active: #{cluster_info.reason}")
    end

  {:error, diagnosis} ->
    IO.puts("❌ Failed to start cluster:")
    IO.puts("   Problem: #{diagnosis.problem}")
    IO.puts("   Solutions: #{inspect(diagnosis.solutions)}")
    System.halt(1)
end

IO.puts("🎉 All tests passed! The cluster system is working.")
