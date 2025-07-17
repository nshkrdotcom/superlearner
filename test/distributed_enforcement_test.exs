defmodule DistributedEnforcementTest do
  @moduledoc """
  Test to verify that distributed test enforcement is working correctly.

  This test verifies that:
  1. cluster_nodes/0 fails hard when no cluster is available
  2. cluster_size/0 fails hard when no cluster is available  
  3. cluster_healthy?/0 fails hard when no cluster is available
  4. No bypass mechanisms allow tests to pass without clusters
  """

  use ExUnit.Case

  # This is NOT a distributed test - it's testing the enforcement mechanism
  # So we don't use DistributedTestCase or tag it as :distributed

  describe "cluster enforcement without active cluster" do
    test "cluster_nodes/0 raises when no cluster is active" do
      # Ensure no cluster is running by cleaning up all clusters
      try do
        OTPSupervisor.Testing.AutoClusterManager.force_cleanup_all()
      catch
        # Manager might not be running
        :exit, _ -> :ok
      end

      # Now test that cluster_nodes/0 fails hard
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     OTPSupervisor.Testing.DistributedTestCase.cluster_nodes()
                   end
    end

    test "cluster_size/0 raises when no cluster is active" do
      # Ensure no cluster is running
      try do
        OTPSupervisor.Testing.AutoClusterManager.force_cleanup_all()
      catch
        :exit, _ -> :ok
      end

      # cluster_size/0 should fail because it calls cluster_nodes/0
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     OTPSupervisor.Testing.DistributedTestCase.cluster_size()
                   end
    end

    test "cluster_healthy?/0 raises when no cluster is active" do
      # Ensure no cluster is running
      try do
        OTPSupervisor.Testing.AutoClusterManager.force_cleanup_all()
      catch
        :exit, _ -> :ok
      end

      # cluster_healthy?/0 should fail because it calls cluster_nodes/0
      assert_raise RuntimeError,
                   ~r/cluster_nodes\/0 called but no active cluster is available/,
                   fn ->
                     OTPSupervisor.Testing.DistributedTestCase.cluster_healthy?()
                   end
    end
  end

  describe "configuration enforcement" do
    test "skip_distributed_on_failure is disabled" do
      config = OTPSupervisor.Testing.Config.load_config()
      assert config.skip_distributed_on_failure == false
    end

    test "retry_cluster_startup is disabled" do
      config = OTPSupervisor.Testing.Config.load_config()
      assert config.retry_cluster_startup == false
    end

    test "max_startup_retries is zero" do
      config = OTPSupervisor.Testing.Config.load_config()
      assert config.max_startup_retries == 0
    end

    test "get_environment_retries always returns 0" do
      config = OTPSupervisor.Testing.Config.load_config()
      cluster_config = OTPSupervisor.Testing.Config.get_cluster_config(config)
      assert cluster_config.max_retries == 0
    end
  end
end
