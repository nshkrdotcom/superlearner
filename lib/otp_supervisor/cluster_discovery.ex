defmodule OTPSupervisor.ClusterDiscovery do
  @moduledoc """
  Dynamic cluster discovery for N-node clusters.
  
  Provides functions to discover and connect to other nodes in a cluster
  based on naming patterns and cluster size configuration.
  """
  
  require Logger
  
  @doc """
  Discover all nodes in the cluster based on cluster size.
  
  Returns a list of node atoms for all nodes in the cluster (excluding self).
  """
  def discover_nodes(cluster_size \\ nil) do
    cluster_size = cluster_size || get_cluster_size_from_config()
    hostname = get_hostname()
    
    1..cluster_size
    |> Enum.map(&generate_node_name(&1, hostname))
    |> Enum.reject(&(&1 == node()))
  end
  
  @doc """
  Connect to all nodes in the cluster.
  
  Returns {:ok, connected_nodes} or {:error, failed_nodes}
  """
  def connect_cluster(cluster_size \\ nil) do
    nodes = discover_nodes(cluster_size)
    
    Logger.info("Attempting to connect to nodes: #{inspect(nodes)}")
    
    results = Enum.map(nodes, fn node ->
      case Node.connect(node) do
        true -> {:ok, node}
        false -> {:error, node}
        :ignored -> {:ignored, node}
      end
    end)
    
    connected = for {:ok, node} <- results, do: node
    failed = for {:error, node} <- results, do: node
    
    if Enum.empty?(failed) do
      {:ok, connected}
    else
      {:partial, connected, failed}
    end
  end
  
  @doc """
  Wait for cluster formation with all expected nodes.
  
  Waits until all nodes are connected or timeout is reached.
  """
  def wait_for_cluster(cluster_size \\ nil, timeout \\ 30_000) do
    cluster_size = cluster_size || get_cluster_size_from_config()
    expected_nodes = cluster_size - 1  # Excluding self
    
    deadline = System.monotonic_time(:millisecond) + timeout
    
    wait_for_nodes(expected_nodes, deadline)
  end
  
  @doc """
  Get the expected node name for a given index.
  """
  def node_name(index, hostname \\ nil) do
    hostname = hostname || get_hostname()
    generate_node_name(index, hostname)
  end
  
  @doc """
  Get this node's index from its name.
  """
  def node_index do
    node_str = node() |> to_string()
    
    case Regex.run(~r/superlearner(\d+)?@/, node_str) do
      [_, ""] -> 1  # superlearner@ (no number) is node 1
      [_, index] -> String.to_integer(index)
      nil -> 
        # Try test_node pattern
        case Regex.run(~r/test_node(\d+)@/, node_str) do
          [_, index] -> String.to_integer(index)
          nil -> 1  # Default to 1 if pattern doesn't match
        end
    end
  end
  
  @doc """
  Check if all expected nodes are connected.
  """
  def cluster_formed?(cluster_size \\ nil) do
    cluster_size = cluster_size || get_cluster_size_from_config()
    expected_nodes = cluster_size - 1
    
    length(Node.list()) >= expected_nodes
  end
  
  @doc """
  Get information about the current cluster state.
  """
  def cluster_info do
    %{
      self: node(),
      index: node_index(),
      connected_nodes: Node.list(),
      cluster_size: get_cluster_size_from_config(),
      cluster_formed: cluster_formed?()
    }
  end
  
  # Private functions
  
  defp generate_node_name(1, hostname) do
    :"superlearner@#{hostname}"
  end
  
  defp generate_node_name(index, hostname) do
    :"superlearner#{index}@#{hostname}"
  end
  
  defp get_hostname do
    # Try various sources for hostname
    System.get_env("NODE_HOSTNAME") ||
      System.get_env("HOSTNAME") ||
      elem(:inet.gethostname(), 1) |> to_string() ||
      "localhost"
  end
  
  defp get_cluster_size_from_config do
    # Try various configuration sources
    System.get_env("CLUSTER_SIZE", "2") |> String.to_integer() ||
      Application.get_env(:otp_supervisor, :cluster_size, 2) ||
      2
  end
  
  defp wait_for_nodes(expected_count, deadline) do
    current_time = System.monotonic_time(:millisecond)
    
    if current_time >= deadline do
      {:timeout, Node.list()}
    else
      connected = length(Node.list())
      
      if connected >= expected_count do
        {:ok, Node.list()}
      else
        # Log progress periodically
        if rem(current_time, 5000) < 100 do
          Logger.info("Waiting for cluster: #{connected}/#{expected_count} nodes connected")
        end
        
        Process.sleep(100)
        wait_for_nodes(expected_count, deadline)
      end
    end
  end
end