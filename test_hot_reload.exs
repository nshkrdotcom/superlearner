#!/usr/bin/env elixir

# Test hot reload functionality
# This script will:
# 1. Compile the updated sandbox code 
# 2. Extract the BEAM data for the TestDemoSupervisor module
# 3. Use the ModuleVersionManager to hot-swap the module in existing sandbox

IO.puts("🔄 Testing Hot Reload Functionality")
IO.puts("===================================")

# Get current children of web-test-1 before hot reload
IO.puts("\n📊 Checking current state of web-test-1...")
IO.puts("(Skipping JSON parsing for simplicity - check manually if needed)")

# Now compile the updated code
IO.puts("\n🔨 Compiling updated sandbox code...")

# Use the isolated compiler to get updated BEAM files
case OTPSupervisor.Core.IsolatedCompiler.compile_sandbox(
  "/home/home/p/g/n/superlearner/sandbox/examples/otp_sandbox"
) do
  {:ok, compile_info} ->
    IO.puts("✅ Compilation successful (#{compile_info.compilation_time}ms)")
    
    # Find the TestDemoSupervisor BEAM file
    supervisor_beam = Enum.find(compile_info.beam_files, fn beam_file ->
      String.contains?(beam_file, "TestDemoSupervisor.beam")
    end)
    
    if supervisor_beam do
      IO.puts("📦 Found supervisor BEAM: #{Path.basename(supervisor_beam)}")
      
      # Read the BEAM data
      case File.read(supervisor_beam) do
        {:ok, beam_data} ->
          IO.puts("💾 Read BEAM data (#{byte_size(beam_data)} bytes)")
          
          # Perform hot swap
          IO.puts("🔄 Performing hot swap...")
          
          case OTPSupervisor.Core.ModuleVersionManager.hot_swap_module(
            "web-test-1", 
            :"Elixir.OtpSandbox.TestDemoSupervisor", 
            beam_data
          ) do
            {:ok, :hot_swapped} ->
              IO.puts("✅ Hot swap successful!")
              
              # Wait a moment for changes to take effect
              Process.sleep(1000)
              
              # Check the updated children
              IO.puts("\n📊 Hot swap complete!")
              IO.puts("Check manually: curl -s 'http://localhost:4000/api/v1/sandboxes/web-test-1?include_children=true' | jq '.data.children'")
              
            {:ok, :no_change} ->
              IO.puts("ℹ️  No change detected - module already up to date")
              
            {:error, reason} ->
              IO.puts("❌ Hot swap failed: #{inspect(reason)}")
          end
          
        {:error, reason} ->
          IO.puts("❌ Failed to read BEAM file: #{inspect(reason)}")
      end
    else
      IO.puts("❌ Could not find TestDemoSupervisor BEAM file")
    end
    
  {:error, reason} ->
    IO.puts("❌ Compilation failed: #{inspect(reason)}")
end

IO.puts("\n🏁 Hot reload test complete")