defmodule ClusterTestHelper do
  @moduledoc """
  Helper functions for distributed/cluster testing.
  
  Provides utilities to start/stop nodes, wait for cluster formation,
  and verify distributed functionality.
  """
  
  @doc """
  Start a test node with the given name suffix.
  Returns {:ok, node_name} or {:error, reason}.
  """
  def start_test_node(suffix, opts \\ []) do
    node_name = :"test_#{suffix}@127.0.0.1"
    cookie = opts[:cookie] || :test_cluster_cookie
    
    # Use :slave for older OTP versions, :peer for newer
    case start_node_with_fallback(node_name, cookie) do
      {:ok, node} ->
        setup_node(node)
        {:ok, node}
      error ->
        error
    end
  end
  
  @doc """
  Stop a test node gracefully.
  """
  def stop_test_node(node) do
    case node do
      nil -> :ok
      node when is_atom(node) ->
        try do
          :rpc.call(node, :init, :stop, [])
          :timer.sleep(100)
          :ok
        catch
          _, _ -> :ok
        end
    end
  end
  
  @doc """
  Wait for nodes to connect and cluster to stabilize.
  """
  def wait_for_cluster(expected_nodes, timeout \\ 5000) do
    start_time = System.monotonic_time(:millisecond)
    wait_for_cluster_loop(expected_nodes, start_time, timeout)
  end
  
  @doc """
  Wait for a condition to be true with timeout.
  """
  def wait_until(fun, timeout \\ 5000) do
    start_time = System.monotonic_time(:millisecond)
    wait_until_loop(fun, start_time, timeout)
  end
  
  @doc """
  Get the current cluster size.
  """
  def cluster_size do
    length([Node.self() | Node.list()])
  end
  
  @doc """
  Verify that distributed components are working.
  """
  def verify_distributed_components do
    try do
      # Test ToolManager
      cluster_status = OTPSupervisor.Distributed.ToolManager.get_cluster_status()
      
      # Test ClusterStateManager
      topology = OTPSupervisor.Distributed.ClusterStateManager.get_cluster_topology()
      
      # Basic verification
      cluster_status.mode == :multi_node and 
      topology.total_nodes > 1 and
      length(cluster_status.connected_nodes) > 0
    rescue
      _ -> false
    end
  end
  
  # Private functions
  
  defp start_node_with_fallback(node_name, cookie) do
    # Try :peer first (OTP 25+)
    case Code.ensure_loaded?(:peer) do
      true ->
        start_with_peer(node_name, cookie)
      false ->
        start_with_slave(node_name, cookie)
    end
  end
  
  defp start_with_peer(node_name, cookie) do
    case :peer.start_link(%{
      name: node_name,
      host: ~c"127.0.0.1",
      args: [~c"-setcookie", Atom.to_string(cookie)]
    }) do
      {:ok, _peer, node} -> {:ok, node}
      error -> error
    end
  end
  
  defp start_with_slave(node_name, cookie) do
    [name, host] = String.split(Atom.to_string(node_name), "@")
    
    case :slave.start_link(String.to_charlist(host), String.to_atom(name)) do
      {:ok, node} ->
        Node.set_cookie(node, cookie)
        {:ok, node}
      error ->
        error
    end
  end
  
  defp setup_node(node) do
    # Add code paths
    :rpc.call(node, :code, :add_paths, [:code.get_path()])
    
    # Set cookie
    :rpc.call(node, Node, :set_cookie, [Node.get_cookie()])
    
    # Start essential applications
    :rpc.call(node, Application, :ensure_all_started, [:logger])
    
    # Try to start our application (may fail in test environment)
    try do
      :rpc.call(node, Application, :ensure_all_started, [:otp_supervisor])
    catch
      _, _ -> :ok  # Ignore failures in test environment
    end
  end
  
  defp wait_for_cluster_loop(expected_nodes, start_time, timeout) do
    current_time = System.monotonic_time(:millisecond)
    
    if current_time - start_time > timeout do
      {:error, :timeout}
    else
      current_nodes = [Node.self() | Node.list()]
      
      if length(current_nodes) >= expected_nodes do
        :ok
      else
        :timer.sleep(50)
        wait_for_cluster_loop(expected_nodes, start_time, timeout)
      end
    end
  end
  
  defp wait_until_loop(fun, start_time, timeout) do
    current_time = System.monotonic_time(:millisecond)
    
    if current_time - start_time > timeout do
      {:error, :timeout}
    else
      if fun.() do
        :ok
      else
        :timer.sleep(50)
        wait_until_loop(fun, start_time, timeout)
      end
    end
  end
end