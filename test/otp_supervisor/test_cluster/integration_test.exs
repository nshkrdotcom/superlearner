defmodule OTPSupervisor.TestCluster.IntegrationTest do
  # Cluster operations must be sequential
  use ExUnit.Case, async: false

  alias OTPSupervisor.TestCluster.{HostnameResolver, PortManager, Diagnostics}

  @moduletag :cluster_management

  setup do
    # Clean up any existing test processes before each test
    PortManager.cleanup_test_processes()
    # Give cleanup time to complete
    :timer.sleep(500)

    on_exit(fn ->
      # Clean up after each test
      PortManager.cleanup_test_processes()
      :timer.sleep(500)
    end)

    :ok
  end

  describe "complete cluster lifecycle" do
    test "hostname resolution works across different WSL configurations" do
      # Test multiple calls to ensure consistency
      results = for _ <- 1..10, do: HostnameResolver.get_cluster_hostname()

      # All should succeed
      assert Enum.all?(results, &match?({:ok, _}, &1))

      # All should return the same hostname
      hostnames = Enum.map(results, fn {:ok, hostname} -> hostname end)
      unique_hostnames = Enum.uniq(hostnames)
      assert length(unique_hostnames) == 1, "Hostname resolution should be consistent"

      # The hostname should be one of the expected values
      [hostname] = unique_hostnames
      assert hostname in ["localhost", "127.0.0.1"] or is_binary(hostname)

      # The hostname should actually resolve
      assert {:ok, _} = HostnameResolver.test_hostname_resolution(hostname)
    end

    test "port conflict resolution and cleanup works" do
      # Test finding ports for different cluster sizes
      for node_count <- [1, 2, 3] do
        assert {:ok, port_pairs} = PortManager.find_available_ports(node_count)
        assert length(port_pairs) == node_count

        # All ports should be unique
        all_ports = Enum.flat_map(port_pairs, fn {http, dist} -> [http, dist] end)
        assert length(all_ports) == length(Enum.uniq(all_ports))

        # All ports should actually be free
        assert Enum.all?(all_ports, &PortManager.port_free?/1)

        # Test cleanup
        assert :ok = PortManager.cleanup_ports(port_pairs)
      end
    end

    test "error messages provide actionable guidance" do
      # Test various error scenarios
      test_cases = [
        {:ports_unavailable, [4100, 4101]},
        {:hostname_resolution_failed, :nxdomain},
        {:node_connection_failed, :test_node@localhost},
        {:insufficient_ports, 2, 5},
        {:unknown_error, "test error"}
      ]

      for error <- test_cases do
        diagnosis = Diagnostics.diagnose_startup_failure(error)

        # Should have problem description
        assert is_binary(diagnosis.problem)
        assert String.length(diagnosis.problem) > 0

        # Should have actionable solutions
        assert is_list(diagnosis.solutions)
        assert length(diagnosis.solutions) > 0
        assert Enum.all?(diagnosis.solutions, &is_binary/1)

        # Solutions should contain actionable commands or instructions
        solutions_text = Enum.join(diagnosis.solutions, " ")

        has_actionable_content =
          String.contains?(solutions_text, "mix ") or
            String.contains?(solutions_text, "Check:") or
            String.contains?(solutions_text, "Run:") or
            String.contains?(solutions_text, "Kill:") or
            String.contains?(solutions_text, "Start:")

        assert has_actionable_content,
               "Solutions should contain actionable guidance: #{inspect(diagnosis.solutions)}"
      end
    end

    test "prerequisite checking works reliably" do
      # Run prerequisite check multiple times to ensure consistency
      results = for _ <- 1..5, do: Diagnostics.check_prerequisites()

      # All results should be consistent
      unique_results = Enum.uniq(results)
      assert length(unique_results) <= 2, "Prerequisite checks should be consistent"

      # If any checks fail, they should provide useful information
      for result <- unique_results do
        case result do
          :ok ->
            :ok

          {:error, failed_checks} ->
            assert is_list(failed_checks)
            assert length(failed_checks) > 0

            for {check_name, check_result} <- failed_checks do
              assert is_binary(check_name)

              case check_result do
                {:error, _} -> :ok
                {:warning, _} -> :ok
                other -> flunk("Unexpected check result: #{inspect(other)}")
              end
            end
        end
      end
    end

    test "environment report provides comprehensive information" do
      report = Diagnostics.environment_report()

      # Validate hostname information
      hostname_info = report.hostname

      case hostname_info.system_hostname do
        hostname when is_binary(hostname) ->
          assert String.length(hostname) > 0

        {:error, _reason} ->
          # System hostname might not be available in test environment
          :ok
      end

      # At least one hostname resolution method should work
      resolution_results = [
        hostname_info.localhost_resolution,
        hostname_info.ip_resolution
      ]

      successful_resolutions = Enum.filter(resolution_results, &match?({:ok, _}, &1))

      assert length(successful_resolutions) > 0,
             "At least one hostname resolution method should work"

      # Validate port information
      port_info = report.ports
      assert is_list(port_info.test_range)
      assert is_list(port_info.available)
      assert is_list(port_info.busy)

      # Should have some available ports
      assert length(port_info.available) > 0, "Should have some available ports for testing"

      # Validate network information
      network_info = report.network
      assert Map.has_key?(network_info, :interfaces)
      assert Map.has_key?(network_info, :connectivity)

      # Should have at least loopback interface
      case network_info.interfaces do
        interfaces when is_map(interfaces) ->
          interface_names = Map.keys(interfaces)
          has_loopback = Enum.any?(interface_names, &String.contains?(&1, "lo"))
          assert has_loopback, "Should have loopback interface"

        {:error, _reason} ->
          # Interface enumeration might fail in test environment
          :ok
      end

      # Connectivity tests
      connectivity = network_info.connectivity

      # At least one connectivity test should pass
      connectivity_results = Map.values(connectivity)
      successful_connectivity = Enum.filter(connectivity_results, &(&1 == :ok))
      assert length(successful_connectivity) > 0, "At least one connectivity test should pass"
    end

    test "cluster startup/shutdown cycles are reliable" do
      # Test multiple startup/shutdown cycles
      for cycle <- 1..3 do
        # Get hostname and ports
        assert {:ok, _hostname} = HostnameResolver.get_cluster_hostname()
        assert {:ok, port_pairs} = PortManager.find_available_ports(2)

        # Verify ports are actually free before starting
        all_ports = Enum.flat_map(port_pairs, fn {http, dist} -> [http, dist] end)

        assert Enum.all?(all_ports, &PortManager.port_free?/1),
               "Cycle #{cycle}: All ports should be free before starting"

        # Simulate cluster startup (we won't actually start nodes in unit tests)
        # but we can test the port allocation and cleanup cycle

        # Mark ports as "in use" by binding to them temporarily
        sockets =
          Enum.map(all_ports, fn port ->
            {:ok, socket} = :gen_tcp.listen(port, [:binary, {:active, false}, {:reuseaddr, true}])
            {port, socket}
          end)

        # Verify ports are now busy
        assert Enum.all?(all_ports, &(!PortManager.port_free?(&1))),
               "Cycle #{cycle}: Ports should be busy after binding"

        # Clean up sockets (simulating node shutdown)
        Enum.each(sockets, fn {_port, socket} -> :gen_tcp.close(socket) end)

        # Wait a bit for cleanup
        :timer.sleep(100)

        # Verify ports are free again
        assert Enum.all?(all_ports, &PortManager.port_free?/1),
               "Cycle #{cycle}: Ports should be free after cleanup"

        # Test port cleanup function
        assert :ok = PortManager.cleanup_ports(port_pairs)
      end
    end

    test "WSL compatibility features work" do
      # Test that our WSL-specific fixes work

      # 1. Hostname resolution should handle WSL networking
      {:ok, hostname} = HostnameResolver.get_cluster_hostname()

      # Should be one of the WSL-compatible hostnames
      assert hostname in ["localhost", "127.0.0.1"] or
               (is_binary(hostname) and String.length(hostname) > 0)

      # 2. Port management should handle WSL port conflicts
      {:ok, port_pairs} = PortManager.find_available_ports(2)

      # Should find ports in the expected ranges
      http_ports = Enum.map(port_pairs, fn {http, _dist} -> http end)
      dist_ports = Enum.map(port_pairs, fn {_http, dist} -> dist end)

      assert Enum.all?(http_ports, &(&1 >= 4100 and &1 <= 4150))
      assert Enum.all?(dist_ports, &(&1 >= 9100 and &1 <= 9150))

      # 3. Diagnostics should provide WSL-specific guidance
      wsl_error = {:hostname_resolution_failed, :nxdomain}
      diagnosis = Diagnostics.diagnose_startup_failure(wsl_error)

      # Should mention systemd-resolved (WSL-specific)
      solutions_text = Enum.join(diagnosis.solutions, " ")
      assert String.contains?(solutions_text, "systemd-resolved")
    end
  end

  describe "error recovery scenarios" do
    test "handles partial failures gracefully" do
      # Test what happens when some operations succeed and others fail

      # This simulates a scenario where we can get hostname but not ports
      assert {:ok, _hostname} = HostnameResolver.get_cluster_hostname()

      # Try to get an unreasonable number of ports
      case PortManager.find_available_ports(1000) do
        {:ok, _ports} ->
          # If we somehow got 1000 ports, that's fine
          :ok

        {:error, reason} ->
          # Should get a reasonable error
          case reason do
            {:no_http_ports, _, _} -> :ok
            {:no_dist_ports, _, _} -> :ok
            other -> flunk("Unexpected error reason: #{inspect(other)}")
          end

          # Error should be diagnosable
          diagnosis = Diagnostics.diagnose_startup_failure(reason)
          assert is_binary(diagnosis.problem)
          assert is_list(diagnosis.solutions)
      end
    end

    test "provides helpful guidance for common issues" do
      # Test diagnostics for common WSL/development issues
      common_errors = [
        {:ports_unavailable, [4100, 4101, 4102]},
        {:hostname_resolution_failed, :nxdomain},
        {:node_connection_failed, :"test_node@invalid-host"},
        {:insufficient_ports, 1, 5}
      ]

      for error <- common_errors do
        diagnosis = Diagnostics.diagnose_startup_failure(error)

        # Should provide specific, actionable guidance
        assert String.length(diagnosis.problem) > 10
        assert length(diagnosis.solutions) >= 2

        # Solutions should be specific to the error type
        solutions_text = Enum.join(diagnosis.solutions, " ")

        case error do
          {:ports_unavailable, _} ->
            assert String.contains?(solutions_text, "clean")

            assert String.contains?(solutions_text, "netstat") or
                     String.contains?(solutions_text, "pkill")

          {:hostname_resolution_failed, _} ->
            assert String.contains?(solutions_text, "ping")
            assert String.contains?(solutions_text, "systemd-resolved")

          {:node_connection_failed, _} ->
            assert String.contains?(solutions_text, "epmd")

          {:insufficient_ports, _, _} ->
            assert String.contains?(solutions_text, "clean")
        end
      end
    end
  end
end
