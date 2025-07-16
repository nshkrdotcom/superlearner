defmodule OTPSupervisor.TestCluster.DiagnosticsTest do
  use ExUnit.Case, async: true
  @moduletag :distributed

  alias OTPSupervisor.TestCluster.Diagnostics

  describe "diagnose_startup_failure/1" do
    test "diagnoses port unavailability" do
      error = {:ports_unavailable, [4100, 4101]}
      diagnosis = Diagnostics.diagnose_startup_failure(error)

      assert diagnosis.problem =~ "Ports [4100, 4101] are in use"
      assert Enum.any?(diagnosis.solutions, &String.contains?(&1, "mix cluster.test clean"))
      assert Enum.any?(diagnosis.solutions, &String.contains?(&1, "netstat"))
    end

    test "diagnoses hostname resolution failure" do
      error = {:hostname_resolution_failed, :nxdomain}
      diagnosis = Diagnostics.diagnose_startup_failure(error)

      assert diagnosis.problem =~ "Cannot resolve hostname"
      assert Enum.any?(diagnosis.solutions, &String.contains?(&1, "ping localhost"))
      assert Enum.any?(diagnosis.solutions, &String.contains?(&1, "systemd-resolved"))
    end

    test "diagnoses node connection failure" do
      error = {:node_connection_failed, :test_node@localhost}
      diagnosis = Diagnostics.diagnose_startup_failure(error)

      assert diagnosis.problem =~ "Cannot connect to node test_node@localhost"
      assert Enum.any?(diagnosis.solutions, &String.contains?(&1, "epmd"))
      assert Enum.any?(diagnosis.solutions, &String.contains?(&1, "ping localhost"))
    end

    test "diagnoses insufficient ports" do
      error = {:insufficient_ports, 2, 5}
      diagnosis = Diagnostics.diagnose_startup_failure(error)

      assert diagnosis.problem =~ "Only 2 ports available, need 5"
      assert Enum.any?(diagnosis.solutions, &String.contains?(&1, "clean"))
    end

    test "diagnoses node startup failure" do
      failed_nodes = [
        {:error, {"node1", :connection_failed}},
        {:error, {"node2", :timeout}}
      ]

      error = {:node_startup_failed, failed_nodes}
      diagnosis = Diagnostics.diagnose_startup_failure(error)

      assert diagnosis.problem =~ "Failed to start nodes"
      assert Enum.any?(diagnosis.solutions, &String.contains?(&1, "epmd"))
    end

    test "handles unknown errors" do
      error = {:unknown_error, "something went wrong"}
      diagnosis = Diagnostics.diagnose_startup_failure(error)

      assert diagnosis.problem =~ "Unknown cluster startup error"
      assert Enum.any?(diagnosis.solutions, &String.contains?(&1, "clean"))
    end
  end

  describe "check_prerequisites/0" do
    test "returns ok or error with details" do
      case Diagnostics.check_prerequisites() do
        :ok ->
          :ok

        {:error, failed_checks} ->
          assert is_list(failed_checks)

          assert Enum.all?(failed_checks, fn {name, result} ->
                   is_binary(name) and match?({:error, _}, result)
                 end)
      end
    end
  end

  describe "environment_report/0" do
    test "generates comprehensive environment report" do
      report = Diagnostics.environment_report()

      # Check that all expected sections are present
      assert Map.has_key?(report, :hostname)
      assert Map.has_key?(report, :epmd)
      assert Map.has_key?(report, :ports)
      assert Map.has_key?(report, :network)
      assert Map.has_key?(report, :system)

      # Check hostname section
      hostname_info = report.hostname
      assert Map.has_key?(hostname_info, :system_hostname)
      assert Map.has_key?(hostname_info, :localhost_resolution)
      assert Map.has_key?(hostname_info, :ip_resolution)

      # Check ports section
      port_info = report.ports
      assert Map.has_key?(port_info, :test_range)
      assert Map.has_key?(port_info, :available)
      assert Map.has_key?(port_info, :busy)
      assert is_list(port_info.test_range)
      assert is_list(port_info.available)
      assert is_list(port_info.busy)

      # Check system section
      system_info = report.system
      assert Map.has_key?(system_info, :os)
      assert Map.has_key?(system_info, :erlang_version)
      assert Map.has_key?(system_info, :elixir_version)
      assert Map.has_key?(system_info, :node_name)
    end

    test "hostname resolution tests work" do
      report = Diagnostics.environment_report()

      # localhost should resolve
      case report.hostname.localhost_resolution do
        {:ok, %{name: name, addresses: addresses}} ->
          assert is_binary(name)
          assert is_list(addresses)
          assert Enum.all?(addresses, &is_binary/1)

        {:error, _reason} ->
          # Might fail in some test environments
          :ok
      end

      # 127.0.0.1 should resolve
      case report.hostname.ip_resolution do
        {:ok, %{name: name, addresses: addresses}} ->
          assert is_binary(name)
          assert is_list(addresses)

        {:error, _reason} ->
          # Might fail in some test environments
          :ok
      end
    end
  end
end
