defmodule OTPSupervisor.TestCluster.HostnameResolverTest do
  use ExUnit.Case, async: true
  @moduletag :distributed

  alias OTPSupervisor.TestCluster.HostnameResolver

  describe "get_cluster_hostname/0" do
    test "returns a valid hostname" do
      assert {:ok, hostname} = HostnameResolver.get_cluster_hostname()
      assert is_binary(hostname)
      assert hostname != ""
    end

    test "returned hostname can be resolved" do
      {:ok, hostname} = HostnameResolver.get_cluster_hostname()

      # The hostname should be resolvable
      hostname_charlist = String.to_charlist(hostname)
      assert {:ok, _hostent} = :inet.gethostbyname(hostname_charlist)
    end

    test "handles different WSL configurations" do
      # Test that we get a consistent result across multiple calls
      results = for _ <- 1..5, do: HostnameResolver.get_cluster_hostname()

      # All results should be successful
      assert Enum.all?(results, &match?({:ok, _}, &1))

      # All results should be the same hostname
      hostnames = Enum.map(results, fn {:ok, hostname} -> hostname end)
      assert length(Enum.uniq(hostnames)) == 1
    end
  end

  describe "test_hostname_resolution/1" do
    test "validates localhost resolution" do
      assert {:ok, "localhost"} = HostnameResolver.test_hostname_resolution("localhost")
    end

    test "validates IP address resolution" do
      assert {:ok, "127.0.0.1"} = HostnameResolver.test_hostname_resolution("127.0.0.1")
    end

    test "fails for invalid hostnames" do
      assert {:error, _reason} =
               HostnameResolver.test_hostname_resolution("invalid-hostname-12345")
    end

    test "handles empty or invalid input" do
      assert {:error, _reason} = HostnameResolver.test_hostname_resolution("")
    end
  end
end
