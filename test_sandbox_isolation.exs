# Test script to verify sandbox isolation fix
IO.puts("Testing sandbox isolation fix...")

# Create first sandbox
IO.puts("Creating first sandbox...")
{:ok, sandbox1} = OTPSupervisor.Core.SandboxManager.create_sandbox("test1", OtpSandbox.TestDemoSupervisor, [])
IO.puts("Sandbox 1 created with app PID: #{inspect(sandbox1.app_pid)}")

# Create second sandbox
IO.puts("Creating second sandbox...")
{:ok, sandbox2} = OTPSupervisor.Core.SandboxManager.create_sandbox("test2", OtpSandbox.TestDemoSupervisor, [])
IO.puts("Sandbox 2 created with app PID: #{inspect(sandbox2.app_pid)}")

# Check if PIDs are unique
unique_pids = sandbox1.app_pid != sandbox2.app_pid
IO.puts("PIDs are unique: #{unique_pids}")
IO.puts("App names - Sandbox 1: #{sandbox1.app_name}, Sandbox 2: #{sandbox2.app_name}")

if unique_pids do
  IO.puts("✅ SUCCESS: Sandboxes have unique app PIDs - isolation fix works!")
else
  IO.puts("❌ FAILURE: Sandboxes are sharing app PIDs - isolation issue persists")
end

# Test destroying one sandbox to see if the other survives
IO.puts("\nTesting destruction isolation...")
IO.puts("Destroying sandbox 1...")
:ok = OTPSupervisor.Core.SandboxManager.destroy_sandbox("test1")

# Check if sandbox 2 is still alive
Process.sleep(100)  # Give it a moment
sandbox2_alive = Process.alive?(sandbox2.app_pid)
IO.puts("Sandbox 2 still alive after destroying sandbox 1: #{sandbox2_alive}")

if sandbox2_alive do
  IO.puts("✅ SUCCESS: Destroying one sandbox didn't affect the other!")
  # Clean up
  :ok = OTPSupervisor.Core.SandboxManager.destroy_sandbox("test2")
  IO.puts("Cleaned up sandbox 2")
else
  IO.puts("❌ FAILURE: Destroying one sandbox killed the other - isolation issue persists")
end

IO.puts("Test completed.")