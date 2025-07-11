# Test simple sandbox creation using existing modules
IO.puts("Testing simple sandbox creation using existing modules...")

# First verify the modules exist
modules_to_check = [
  OtpSandbox.TestDemoSupervisor,
  OtpSandbox.Supervisors.DemoSupervisor,
  OtpSandbox.Application
]

IO.puts("\n🔍 Checking existing modules:")

Enum.each(modules_to_check, fn module ->
  case Code.ensure_loaded(module) do
    {:module, ^module} ->
      IO.puts("  ✅ #{module} - loaded")

    {:error, reason} ->
      IO.puts("  ❌ #{module} - error: #{reason}")
  end
end)

# Try creating a sandbox using the old method temporarily
IO.puts("\n🧪 Testing sandbox creation...")

case OTPSupervisor.Core.SandboxManager.create_sandbox(
       "test-simple",
       OtpSandbox.TestDemoSupervisor
     ) do
  {:ok, sandbox_info} ->
    IO.puts("✅ Successfully created sandbox: #{sandbox_info.id}")
    IO.puts("  - App name: #{sandbox_info.app_name}")
    IO.puts("  - App PID: #{inspect(sandbox_info.app_pid)}")
    IO.puts("  - Supervisor PID: #{inspect(sandbox_info.supervisor_pid)}")

    # List all sandboxes
    sandboxes = OTPSupervisor.Core.SandboxManager.list_sandboxes()
    IO.puts("\n📋 Total sandboxes: #{length(sandboxes)}")

  {:error, reason} ->
    IO.puts("❌ Failed to create sandbox: #{inspect(reason)}")
end
