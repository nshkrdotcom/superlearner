#!/usr/bin/env elixir

# Test script to verify the port configuration fix works

Mix.install([])

# Set test environment
Application.put_env(:mix, :env, :test)

# Load the project configuration
Code.require_file("config/test.exs")

# Load the project modules
Code.require_file("lib/otp_supervisor/testing/config.ex")
Code.require_file("lib/otp_supervisor/test_cluster/diagnostics.ex")

IO.puts("🔍 Testing port configuration fix...")

# Test 1: Check if Config loads properly
IO.puts("\n1. Testing Config.load_config()...")
config = OTPSupervisor.Testing.Config.load_config()
IO.puts("   ✅ Config loaded successfully")
IO.puts("   HTTP port base: #{config.http_port_base}")
IO.puts("   Dist port base: #{config.dist_port_base}")

# Test 2: Check port ranges
IO.puts("\n2. Testing port ranges...")
port_ranges = OTPSupervisor.Testing.Config.get_port_ranges(config)
IO.puts("   ✅ Port ranges calculated")
IO.puts("   HTTP range: #{inspect(port_ranges.http)}")
IO.puts("   Dist range: #{inspect(port_ranges.distribution)}")

# Test 3: Check port availability
IO.puts("\n3. Testing port availability...")
availability = OTPSupervisor.Testing.Config.check_port_availability(config)
IO.puts("   HTTP ports available: #{availability.http_ports_available}")
IO.puts("   Dist ports available: #{availability.dist_ports_available}")
IO.puts("   All available: #{availability.all_available}")

# Test 4: Test diagnostics with new port checking
IO.puts("\n4. Testing Diagnostics.check_prerequisites()...")
case OTPSupervisor.Testing.Config.load_config() do
  config when is_map(config) ->
    IO.puts("   ✅ Config system working")

    # This should now use the configured ports (4200+) instead of hardcoded (4100+)
    case OTPSupervisor.TestCluster.Diagnostics.check_prerequisites() do
      :ok ->
        IO.puts("   ✅ Prerequisites check passed!")
        IO.puts("   🎉 Port fix is working - no more conflicts with Phoenix app!")

      {:error, failed_checks} ->
        IO.puts("   ⚠️  Some checks failed:")
        Enum.each(failed_checks, fn {name, result} ->
          IO.puts("     #{name}: #{inspect(result)}")
        end)

        # Check if it's still the old port conflict
        port_errors = Enum.filter(failed_checks, fn {name, _} -> name == "Ports" end)
        if not Enum.empty?(port_errors) do
          IO.puts("   🔍 Port errors found - let's see if they're the new ports or old ones...")
          Enum.each(port_errors, fn {_, {:error, msg}} ->
            IO.puts("     Error: #{msg}")
            if String.contains?(msg, "4100") or String.contains?(msg, "4101") do
              IO.puts("     ❌ Still using old hardcoded ports - fix didn't work")
            else
              IO.puts("     ✅ Using new configured ports - fix worked, just other issues")
            end
          end)
        end
    end

  error ->
    IO.puts("   ❌ Config system failed: #{inspect(error)}")
end

IO.puts("\n🏁 Port configuration test complete!")
