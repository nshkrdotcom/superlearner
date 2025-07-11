# Test the isolated compilation system
sandbox_path = Path.join([File.cwd!(), "sandbox", "examples", "otp_sandbox"])
IO.puts("Testing isolated compilation on: #{sandbox_path}")

case OTPSupervisor.Core.IsolatedCompiler.compile_sandbox(sandbox_path, validate_beams: false) do
  {:ok, compile_info} ->
    IO.puts("✅ Compilation succeeded!")
    IO.puts("  - Compilation time: #{compile_info.compilation_time}ms")
    IO.puts("  - BEAM files: #{length(compile_info.beam_files)}")
    IO.puts("  - App file: #{compile_info.app_file}")
    IO.puts("  - Temp dir: #{compile_info.temp_dir}")

    # Show first few BEAM files
    Enum.take(compile_info.beam_files, 3)
    |> Enum.each(fn beam -> IO.puts("    - #{Path.basename(beam)}") end)

    # Test creating a sandbox with disabled validation
    IO.puts("\n🧪 Testing sandbox creation with disabled validation...")

    case OTPSupervisor.Core.SandboxManager.create_sandbox(
           "test-1",
           "OtpSandbox.TestDemoSupervisor",
           validate_beams: false
         ) do
      {:ok, sandbox_info} ->
        IO.puts("✅ Successfully created sandbox: #{sandbox_info.id}")
        IO.puts("  - App name: #{sandbox_info.app_name}")
        IO.puts("  - App PID: #{inspect(sandbox_info.app_pid)}")
        IO.puts("  - Supervisor PID: #{inspect(sandbox_info.supervisor_pid)}")

      {:error, reason} ->
        IO.puts("❌ Failed to create sandbox: #{inspect(reason)}")
    end

  {:error, reason} ->
    IO.puts("❌ Compilation failed: #{inspect(reason)}")
end
