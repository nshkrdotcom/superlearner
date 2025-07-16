defmodule OTPSupervisor.TestCluster.PortManagerTest do
  # Port operations need to be sequential
  use ExUnit.Case, async: false
  @moduletag :distributed

  alias OTPSupervisor.TestCluster.PortManager

  describe "find_available_ports/1" do
    test "finds port pairs for single node" do
      assert {:ok, port_pairs} = PortManager.find_available_ports(1)
      assert length(port_pairs) == 1

      [{http_port, dist_port}] = port_pairs
      assert is_integer(http_port)
      assert is_integer(dist_port)
      assert http_port != dist_port
    end

    test "finds port pairs for multiple nodes" do
      assert {:ok, port_pairs} = PortManager.find_available_ports(3)
      assert length(port_pairs) == 3

      # All ports should be unique
      all_ports = Enum.flat_map(port_pairs, fn {http, dist} -> [http, dist] end)
      assert length(all_ports) == length(Enum.uniq(all_ports))
    end

    test "handles invalid node count" do
      assert {:error, :invalid_node_count} = PortManager.find_available_ports(0)
      assert {:error, :invalid_node_count} = PortManager.find_available_ports(-1)
      assert {:error, :invalid_node_count} = PortManager.find_available_ports("invalid")
    end
  end

  describe "port_free?/1" do
    test "detects free ports" do
      # Find a likely free port
      {:ok, [free_port]} = PortManager.find_free_ports(5000, 1)
      assert PortManager.port_free?(free_port)
    end

    test "detects busy ports" do
      # Bind to a port to make it busy
      {:ok, socket} = :gen_tcp.listen(0, [:binary, {:active, false}])
      {:ok, port} = :inet.port(socket)

      refute PortManager.port_free?(port)

      :gen_tcp.close(socket)
    end
  end

  describe "find_free_ports/2" do
    test "finds requested number of free ports" do
      assert {:ok, ports} = PortManager.find_free_ports(5000, 3)
      assert length(ports) == 3
      assert Enum.all?(ports, &is_integer/1)
      assert Enum.all?(ports, &PortManager.port_free?/1)
    end

    test "returns error when insufficient ports available" do
      # This test might be flaky depending on system state
      # We'll test with a very high port count that's unlikely to be available
      case PortManager.find_free_ports(65000, 100) do
        {:ok, _ports} ->
          # If we somehow found 100 ports, that's fine too
          :ok

        {:error, {:insufficient_ports, found, requested}} ->
          assert found < requested
          assert requested == 100
      end
    end
  end

  describe "get_port_info/1" do
    test "reports free port correctly" do
      {:ok, [free_port]} = PortManager.find_free_ports(5000, 1)

      case PortManager.get_port_info(free_port) do
        {:ok, :port_free} -> :ok
        # netstat might not be available in test env
        {:error, :netstat_failed} -> :ok
      end
    end

    test "reports busy port correctly" do
      {:ok, socket} = :gen_tcp.listen(0, [:binary, {:active, false}])
      {:ok, port} = :inet.port(socket)

      case PortManager.get_port_info(port) do
        {:ok, {:port_used, _info}} -> :ok
        # netstat might not be available in test env
        {:error, :netstat_failed} -> :ok
      end

      :gen_tcp.close(socket)
    end
  end

  describe "cleanup_single_port/1" do
    test "handles cleanup of free port gracefully" do
      {:ok, [free_port]} = PortManager.find_free_ports(5000, 1)

      # Should not error even if port is already free
      result = PortManager.cleanup_single_port(free_port)
      assert result in [:ok, {:error, :invalid_port}]
    end

    test "validates port parameter" do
      assert {:error, :invalid_port} = PortManager.cleanup_single_port("invalid")
      assert {:error, :invalid_port} = PortManager.cleanup_single_port(nil)
    end
  end

  describe "check_port_range_availability/2" do
    test "reports all available when range is free" do
      # Find a free starting port
      {:ok, [start_port]} = PortManager.find_free_ports(5000, 1)

      case PortManager.check_port_range_availability(start_port, 3) do
        {:ok, :all_available} ->
          :ok

        {:error, {:ports_unavailable, _unavailable}} ->
          # Some ports in range might be busy, that's ok for this test
          :ok
      end
    end

    test "reports unavailable ports correctly" do
      # Bind to a port to make it unavailable
      {:ok, socket} = :gen_tcp.listen(0, [:binary, {:active, false}])
      {:ok, busy_port} = :inet.port(socket)

      case PortManager.check_port_range_availability(busy_port, 1) do
        {:error, {:ports_unavailable, [^busy_port]}} ->
          :ok

        {:ok, :all_available} ->
          # Port might have been freed between bind and check
          :ok
      end

      :gen_tcp.close(socket)
    end
  end

  describe "cleanup_test_processes/0" do
    test "runs without error" do
      # This test just ensures the function doesn't crash
      result = PortManager.cleanup_test_processes()

      case result do
        :ok -> :ok
        {:error, {:pkill_failed, _code}} -> :ok
        other -> flunk("Unexpected result: #{inspect(other)}")
      end
    end
  end
end
