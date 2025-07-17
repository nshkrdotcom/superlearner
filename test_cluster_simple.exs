#!/usr/bin/env elixir

# Simple test for cluster functionality using the actual application
System.argv() |> IO.inspect(label: "Args")

# Add the compiled beam files to the path
Code.append_path("_build/dev/lib/otp_supervisor/ebin")
Code.append_path("_build/dev/lib/erlexec/ebin")

# Start dependencies manually
Application.ensure_all_started(:logger)
Application.ensure_all_started(:erlexec)

IO.puts("Testing erlexec basic functionality...")

case :exec.run(["echo", "Hello from erlexec!"], []) do
  {:ok, exec_pid, os_pid} ->
    IO.puts("✅ Erlexec test passed!")
    IO.puts("Exec PID: #{inspect(exec_pid)}, OS PID: #{os_pid}")

  {:error, reason} ->
    IO.puts("❌ Erlexec test failed: #{inspect(reason)}")
    System.halt(1)
end

IO.puts("Testing cluster manager...")

# Try to start the TestCluster Manager
case GenServer.start_link(OTPSupervisor.TestCluster.Manager, []) do
  {:ok, pid} ->
    IO.puts("✅ TestCluster.Manager started: #{inspect(pid)}")
    
    # Try a simple status call
    case GenServer.call(pid, :get_status) do
      {:ok, status} ->
        IO.puts("✅ Status call succeeded: #{inspect(status)}")
        
      {:error, reason} ->
        IO.puts("⚠️  Status call failed: #{inspect(reason)}")
    end
    
  {:error, reason} ->
    IO.puts("❌ Failed to start TestCluster.Manager: #{inspect(reason)}")
    System.halt(1)
end

IO.puts("🎉 Basic erlexec integration is working!")