#!/usr/bin/env elixir

# Simple test to verify port configuration is working

# Load the project
Code.require_file("mix.exs")

# Set test environment
Application.put_env(:mix, :env, :test)

# Load test config
Code.require_file("config/test.exs")

# Load the PortManager
Code.require_file("lib/otp_supervisor/test_cluster/port_manager.ex")

IO.puts("🔍 Testing PortManager configuration...")

# Test the port allocation
case OTPSupervisor.TestCluster.PortManager.find_available_ports(2) do
  {:ok, port_pairs} ->
    IO.puts("✅ PortManager found ports: #{inspect(port_pairs)}")

    # Check if they're in the expected range (4200+, not 4100+)
    http_ports = Enum.map(port_pairs, fn {http, _dist} -> http end)
    min_http = Enum.min(http_ports)

    if min_http >= 4200 do
      IO.puts("✅ SUCCESS: Using configured ports (4200+) - no conflict with Phoenix!")
      IO.puts("   HTTP ports: #{inspect(http_ports)}")
    else
      IO.puts("❌ FAILURE: Still using old hardcoded ports (#{min_http})")
      IO.puts("   This will conflict with your Phoenix app on 4100-4101")
    end

  {:error, reason} ->
    IO.puts("❌ PortManager failed: #{inspect(reason)}")
end

IO.puts("\n🔍 Checking application config...")
config = Application.get_env(:otp_supervisor, :distributed_testing, [])
http_base = Keyword.get(config, :http_port_base, :not_found)
dist_base = Keyword.get(config, :dist_port_base, :not_found)

IO.puts("   HTTP port base: #{inspect(http_base)}")
IO.puts("   Dist port base: #{inspect(dist_base)}")

if http_base == 4200 and dist_base == 9200 then
  IO.puts("✅ Configuration is correct")
else
  IO.puts("❌ Configuration issue detected")
end
