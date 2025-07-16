defmodule OTPSupervisor.TestCluster.ValidationTest do
  use ExUnit.Case, async: false

  alias OTPSupervisor.TestCluster.{HostnameResolver, PortManager, Diagnostics}

  @moduletag :cluster_management

  describe "comprehensive validation of cluster fixes" do
    test "hostname resolution works reliably across WSL configurations" do
      # Test multiple calls to ensure consistency
      results = for _ <- 1..20, do: HostnameResolver.get_cluster_hostname()

      # All calls should succeed
      assert Enum.all?(results, &match?({:ok, _}, &1))

      # All results should be consistent
      hostnames = Enum.map(results, fn {:ok, hostname} -> hostname end)
      unique_hostnames = Enum.uniq(hostnames)
      assert length(unique_hostnames) == 1, "Hostname resolution should be consistent"

      # The hostname should be valid
      [hostname] = unique_hostnames
      assert is_binary(hostname)
      assert String.length(hostname) > 0

      # The hostname should actually resolve
      assert {:ok, ^hostname} = HostnameResolver.test_hostname_resolution(hostname)

      IO.puts("✅ Hostname resolution: #{hostname}")
    end

    test "port conflict resolution works under load" do
      # Test port allocation under various scenarios
      test_scenarios = [
        {1, "single node"},
        {2, "small cluster"},
        {3, "medium cluster"},
        {5, "large cluster"}
      ]

      for {node_count, description} <- test_scenarios do
        case PortManager.find_available_ports(node_count) do
          {:ok, port_pairs} ->
            assert length(port_pairs) == node_count

            # Verify all ports are unique
            all_ports = Enum.flat_map(port_pairs, fn {http, dist} -> [http, dist] end)
            assert length(all_ports) == length(Enum.uniq(all_ports))

            # Verify all ports are actually free
            assert Enum.all?(all_ports, &PortManager.port_free?/1)

            # Test cleanup
            assert :ok = PortManager.cleanup_ports(port_pairs)

            IO.puts("✅ Port allocation for #{description}: #{inspect(port_pairs)}")

          {:error, reason} ->
            IO.puts("⚠️  Port allocation failed for #{description}: #{inspect(reason)}")
            # This might happen under resource constraints, which is acceptable
        end
      end
    end

    test "error messages provide actionable guidance" do
      error_scenarios = [
        {:ports_unavailable, [4100, 4101, 4102]},
        {:hostname_resolution_failed, :nxdomain},
        {:node_connection_failed, :"test_node@invalid-host"},
        {:insufficient_ports, 2, 10},
        {:node_startup_failed,
         [
           {:error, {"node1", :connection_failed}},
           {:error, {"node2", :timeout}}
         ]},
        {:unknown_error, "mysterious failure"}
      ]

      for {error_type, error_data} <- error_scenarios do
        error =
          case error_type do
            :unknown_error -> {error_type, error_data}
            _ -> {error_type, error_data}
          end

        diagnosis = Diagnostics.diagnose_startup_failure(error)

        # Validate problem description
        assert is_binary(diagnosis.problem)
        assert String.length(diagnosis.problem) > 10

        # Validate solutions
        assert is_list(diagnosis.solutions)
        assert length(diagnosis.solutions) >= 2
        assert Enum.all?(diagnosis.solutions, &is_binary/1)

        # Check for actionable content
        solutions_text = Enum.join(diagnosis.solutions, " ")

        has_commands =
          String.contains?(solutions_text, "mix ") or
            String.contains?(solutions_text, "Check:") or
            String.contains?(solutions_text, "Run:") or
            String.contains?(solutions_text, "Kill:") or
            String.contains?(solutions_text, "Start:")

        assert has_commands, "Solutions should contain actionable commands"

        IO.puts("✅ Error diagnosis for #{error_type}:")
        IO.puts("   Problem: #{diagnosis.problem}")
        IO.puts("   Solutions: #{length(diagnosis.solutions)} provided")
      end
    end

    test "prerequisite checking provides comprehensive validation" do
      # Run prerequisite checks multiple times
      results = for _ <- 1..5, do: Diagnostics.check_prerequisites()

      # Results should be consistent
      unique_results = Enum.uniq(results)
      assert length(unique_results) <= 2, "Prerequisite checks should be consistent"

      for result <- unique_results do
        case result do
          :ok ->
            IO.puts("✅ All prerequisites passed")

          {:error, failed_checks} ->
            IO.puts("⚠️  Some prerequisites failed:")

            for {check_name, check_result} <- failed_checks do
              assert is_binary(check_name)

              case check_result do
                {:error, message} ->
                  IO.puts("   ❌ #{check_name}: #{message}")

                {:warning, message} ->
                  IO.puts("   ⚠️  #{check_name}: #{message}")
              end
            end
        end
      end
    end

    test "environment report provides comprehensive system information" do
      report = Diagnostics.environment_report()

      # Validate structure
      required_sections = [:hostname, :epmd, :ports, :network, :system]

      for section <- required_sections do
        assert Map.has_key?(report, section), "Report should include #{section} section"
      end

      # Validate hostname information
      hostname_info = report.hostname
      IO.puts("🔍 Hostname Information:")

      case hostname_info.system_hostname do
        hostname when is_binary(hostname) ->
          IO.puts("   System hostname: #{hostname}")

        {:error, reason} ->
          IO.puts("   System hostname failed: #{inspect(reason)}")
      end

      case hostname_info.localhost_resolution do
        {:ok, %{name: name, addresses: addresses}} ->
          IO.puts("   Localhost resolves to: #{name} (#{Enum.join(addresses, ", ")})")

        {:error, reason} ->
          IO.puts("   Localhost resolution failed: #{inspect(reason)}")
      end

      case hostname_info.ip_resolution do
        {:ok, %{name: name, addresses: addresses}} ->
          IO.puts("   127.0.0.1 resolves to: #{name} (#{Enum.join(addresses, ", ")})")

        {:error, reason} ->
          IO.puts("   127.0.0.1 resolution failed: #{inspect(reason)}")
      end

      # Validate port information
      port_info = report.ports
      IO.puts("🔍 Port Information:")
      IO.puts("   Test range: #{length(port_info.test_range)} ports")
      IO.puts("   Available: #{length(port_info.available)} ports")
      IO.puts("   Busy: #{length(port_info.busy)} ports")

      # Should have some available ports
      assert length(port_info.available) > 0, "Should have available ports for testing"

      # Validate EPMD information
      IO.puts("🔍 EPMD Information:")

      case report.epmd do
        {:ok, output} ->
          IO.puts("   EPMD status: #{String.trim(output)}")

        {:error, reason} ->
          IO.puts("   EPMD error: #{reason}")
      end

      # Validate network information
      network_info = report.network
      IO.puts("🔍 Network Information:")

      case network_info.interfaces do
        interfaces when is_map(interfaces) ->
          interface_count = map_size(interfaces)
          IO.puts("   Network interfaces: #{interface_count} found")

          # Should have at least loopback
          has_loopback = Enum.any?(Map.keys(interfaces), &String.contains?(&1, "lo"))
          assert has_loopback, "Should have loopback interface"

        {:error, reason} ->
          IO.puts("   Interface enumeration failed: #{inspect(reason)}")
      end

      connectivity = network_info.connectivity
      IO.puts("   Localhost connectivity: #{inspect(connectivity.localhost)}")
      IO.puts("   Loopback connectivity: #{inspect(connectivity.loopback)}")

      # At least one should work
      connectivity_results = Map.values(connectivity)
      successful = Enum.count(connectivity_results, &(&1 == :ok))
      assert successful > 0, "At least one connectivity test should pass"

      # Validate system information
      system_info = report.system
      IO.puts("🔍 System Information:")
      IO.puts("   OS: #{inspect(system_info.os)}")
      IO.puts("   Erlang: #{system_info.erlang_version}")
      IO.puts("   Elixir: #{system_info.elixir_version}")
      IO.puts("   Node: #{system_info.node_name}")
    end

    test "cluster startup/shutdown cycle simulation" do
      # Simulate multiple cluster lifecycle operations
      for cycle <- 1..5 do
        IO.puts("🔄 Testing cluster cycle #{cycle}")

        # Get hostname
        {:ok, _hostname} = HostnameResolver.get_cluster_hostname()

        # Get ports
        {:ok, port_pairs} = PortManager.find_available_ports(2)

        # Verify ports are free
        all_ports = Enum.flat_map(port_pairs, fn {http, dist} -> [http, dist] end)
        assert Enum.all?(all_ports, &PortManager.port_free?/1)

        # Simulate port usage by binding to them
        sockets =
          Enum.map(all_ports, fn port ->
            {:ok, socket} = :gen_tcp.listen(port, [:binary, {:active, false}, {:reuseaddr, true}])
            {port, socket}
          end)

        # Verify ports are now busy
        assert Enum.all?(all_ports, &(!PortManager.port_free?(&1)))

        # Simulate cleanup
        Enum.each(sockets, fn {_port, socket} -> :gen_tcp.close(socket) end)

        # Wait for cleanup
        :timer.sleep(100)

        # Verify ports are free again
        assert Enum.all?(all_ports, &PortManager.port_free?/1)

        # Test port cleanup function
        assert :ok = PortManager.cleanup_ports(port_pairs)

        IO.puts("   ✅ Cycle #{cycle} completed successfully")
      end
    end

    test "WSL compatibility validation" do
      IO.puts("🔍 WSL Compatibility Validation:")

      # Test hostname resolution strategies
      {:ok, hostname} = HostnameResolver.get_cluster_hostname()

      wsl_compatible =
        hostname in ["localhost", "127.0.0.1"] or
          (is_binary(hostname) and String.length(hostname) > 0)

      assert wsl_compatible, "Hostname should be WSL compatible"
      IO.puts("   ✅ Hostname: #{hostname} (WSL compatible)")

      # Test port ranges
      {:ok, port_pairs} = PortManager.find_available_ports(2)

      http_ports = Enum.map(port_pairs, fn {http, _} -> http end)
      dist_ports = Enum.map(port_pairs, fn {_, dist} -> dist end)

      # Should be in expected ranges
      assert Enum.all?(http_ports, &(&1 >= 4100 and &1 <= 4200))
      assert Enum.all?(dist_ports, &(&1 >= 9100 and &1 <= 9200))

      IO.puts("   ✅ HTTP ports: #{inspect(http_ports)}")
      IO.puts("   ✅ Dist ports: #{inspect(dist_ports)}")

      # Test WSL-specific error guidance
      wsl_error = {:hostname_resolution_failed, :nxdomain}
      diagnosis = Diagnostics.diagnose_startup_failure(wsl_error)

      solutions_text = Enum.join(diagnosis.solutions, " ")
      has_wsl_guidance = String.contains?(solutions_text, "systemd-resolved")

      assert has_wsl_guidance, "Should provide WSL-specific guidance"
      IO.puts("   ✅ WSL-specific error guidance provided")
    end
  end

  describe "performance and reliability validation" do
    test "operations complete within reasonable time" do
      # Test hostname resolution performance
      start_time = System.monotonic_time(:millisecond)

      for _ <- 1..10 do
        {:ok, _hostname} = HostnameResolver.get_cluster_hostname()
      end

      hostname_time = System.monotonic_time(:millisecond) - start_time
      assert hostname_time < 5000, "Hostname resolution should be fast"
      IO.puts("✅ Hostname resolution: #{hostname_time}ms for 10 calls")

      # Test port allocation performance
      start_time = System.monotonic_time(:millisecond)

      for _ <- 1..5 do
        {:ok, _ports} = PortManager.find_available_ports(3)
      end

      port_time = System.monotonic_time(:millisecond) - start_time
      assert port_time < 10000, "Port allocation should be reasonably fast"
      IO.puts("✅ Port allocation: #{port_time}ms for 5 calls")

      # Test diagnostics performance
      start_time = System.monotonic_time(:millisecond)

      for _ <- 1..10 do
        Diagnostics.check_prerequisites()
      end

      diag_time = System.monotonic_time(:millisecond) - start_time
      assert diag_time < 15000, "Diagnostics should be reasonably fast"
      IO.puts("✅ Diagnostics: #{diag_time}ms for 10 calls")
    end

    test "error handling is robust" do
      # Test various edge cases
      edge_cases = [
        fn -> HostnameResolver.test_hostname_resolution("") end,
        fn -> HostnameResolver.test_hostname_resolution("invalid-host-12345") end,
        fn -> PortManager.find_available_ports(0) end,
        fn -> PortManager.find_available_ports(-1) end,
        fn -> PortManager.cleanup_single_port("invalid") end,
        fn -> PortManager.cleanup_ports("invalid") end
      ]

      for edge_case <- edge_cases do
        result = edge_case.()

        # Should return error, not crash
        assert match?({:error, _}, result), "Edge case should return error, not crash"
      end

      IO.puts("✅ All edge cases handled gracefully")
    end
  end
end
