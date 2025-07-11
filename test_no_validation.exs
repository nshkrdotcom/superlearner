# Test sandbox creation with validation completely disabled
IO.puts("Testing sandbox creation with validation completely disabled...")

# Test creating a sandbox with validation disabled
case OTPSupervisor.Core.SandboxManager.create_sandbox("test-no-val", "OtpSandbox.TestDemoSupervisor", validate_beams: false) do
  {:ok, sandbox_info} -> 
    IO.puts("✅ Successfully created sandbox: #{sandbox_info.id}")
    IO.puts("  - App name: #{sandbox_info.app_name}")
    IO.puts("  - App PID: #{inspect(sandbox_info.app_pid)}")
    IO.puts("  - Supervisor PID: #{inspect(sandbox_info.supervisor_pid)}")
    
    # Test if the supervisor is actually working
    IO.puts("\n🔍 Testing supervisor functionality...")
    try do
      children = Supervisor.which_children(sandbox_info.supervisor_pid)
      IO.puts("  - Supervisor has #{length(children)} children")
      Enum.each(children, fn {id, pid, type, modules} ->
        IO.puts("    * #{id}: #{inspect(pid)} (#{type})")
      end)
    rescue
      error -> IO.puts("  - Error getting children: #{inspect(error)}")
    end
    
    # List all sandboxes
    sandboxes = OTPSupervisor.Core.SandboxManager.list_sandboxes()
    IO.puts("\n📋 Total sandboxes: #{length(sandboxes)}")
    
  {:error, reason} -> 
    IO.puts("❌ Failed to create sandbox: #{inspect(reason)}")
end