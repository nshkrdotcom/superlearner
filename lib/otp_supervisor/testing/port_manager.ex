defmodule OTPSupervisor.Testing.PortManager do
  @moduledoc """
  Manages port allocation for distributed testing to avoid conflicts
  with development servers and other test instances.
  
  Provides intelligent port allocation, conflict detection, and
  cleanup to ensure reliable distributed testing across different
  environments.
  """
  
  require Logger
  alias OTPSupervisor.Testing.Config
  
  @doc """
  Allocate ports for a cluster of the given size.
  
  Returns a map with port assignments for each node in the cluster.
  """
  def allocate_cluster_ports(cluster_size, config \\ nil) do
    config = config || Config.load_config()
    port_ranges = Config.get_port_ranges(config)
    
    case find_available_port_block(cluster_size, port_ranges, config) do
      {:ok, port_assignments} ->
        {:ok, port_assignments}
        
      {:error, reason} ->
        {:error, {:port_allocation_failed, reason}}
    end
  end
  
  @doc """
  Check if the configured port ranges are available.
  """
  def check_port_availability(config \\ nil) do
    Config.check_port_availability(config)
  end
  
  @doc """
  Find alternative port ranges if the configured ones are not available.
  """
  def find_alternative_ports(cluster_size, config \\ nil) do
    config = config || Config.load_config()
    
    # Try different base ports if the configured ones are not available
    alternative_bases = generate_alternative_port_bases(config)
    
    Enum.find_value(alternative_bases, fn {http_base, dist_base} ->
      alternative_ranges = %{
        http: {http_base, http_base + config.port_range_size - 1},
        distribution: {dist_base, dist_base + config.port_range_size - 1}
      }
      
      case find_available_port_block(cluster_size, alternative_ranges, config) do
        {:ok, port_assignments} -> {:ok, port_assignments}
        {:error, _} -> nil
      end
    end) || {:error, :no_available_ports}
  end
  
  @doc """
  Release ports that were allocated for testing.
  
  This is mainly for cleanup and documentation - the OS will
  automatically release ports when processes exit.
  """
  def release_cluster_ports(port_assignments) do
    Logger.debug("Releasing cluster ports: #{inspect(Map.keys(port_assignments))}")
    :ok
  end
  
  @doc """
  Get port assignments for existing cluster nodes.
  """
  def get_cluster_port_info(nodes) when is_list(nodes) do
    Enum.map(nodes, fn node ->
      {node, get_node_port_info(node)}
    end)
    |> Enum.into(%{})
  end
  
  @doc """
  Validate that port assignments don't conflict with known services.
  """
  def validate_port_assignments(port_assignments, config \\ nil) do
    config = config || Config.load_config()
    
    conflicts = find_port_conflicts(port_assignments, config)
    
    case conflicts do
      [] -> :ok
      conflicts -> {:error, {:port_conflicts, conflicts}}
    end
  end
  
  # Private implementation
  
  defp find_available_port_block(cluster_size, port_ranges, _config) do
    http_ports = find_available_ports_in_range(port_ranges.http, cluster_size)
    dist_ports = find_available_ports_in_range(port_ranges.distribution, cluster_size)
    
    case {http_ports, dist_ports} do
      {{:ok, http_list}, {:ok, dist_list}} ->
        port_assignments = build_port_assignments(cluster_size, http_list, dist_list)
        {:ok, port_assignments}
        
      {{:error, reason}, _} ->
        {:error, {:http_ports_unavailable, reason}}
        
      {_, {:error, reason}} ->
        {:error, {:distribution_ports_unavailable, reason}}
    end
  end
  
  defp find_available_ports_in_range({start_port, end_port}, count) do
    available_ports = 
      start_port..end_port
      |> Enum.filter(&port_available?/1)
      |> Enum.take(count)
    
    if length(available_ports) >= count do
      {:ok, available_ports}
    else
      {:error, {:insufficient_ports, %{needed: count, available: length(available_ports)}}}
    end
  end
  
  defp port_available?(port) do
    case :gen_tcp.listen(port, []) do
      {:ok, socket} ->
        :gen_tcp.close(socket)
        true
      {:error, :eaddrinuse} ->
        false
      {:error, _other} ->
        # Other errors might indicate the port is available but there's another issue
        # For safety, consider it unavailable
        false
    end
  end
  
  defp build_port_assignments(cluster_size, http_ports, dist_ports) do
    1..cluster_size
    |> Enum.with_index()
    |> Enum.map(fn {node_num, index} ->
      node_name = :"test_node_#{node_num}@127.0.0.1"
      
      assignment = %{
        node: node_name,
        http_port: Enum.at(http_ports, index),
        distribution_port: Enum.at(dist_ports, index),
        node_index: node_num
      }
      
      {node_name, assignment}
    end)
    |> Enum.into(%{})
  end
  
  defp generate_alternative_port_bases(config) do
    base_http = config.http_port_base
    base_dist = config.dist_port_base
    range_size = config.port_range_size
    
    # Generate alternative port bases by incrementing in steps
    step_size = range_size + 50  # Add some buffer between ranges
    
    [
      {base_http + step_size, base_dist + step_size},
      {base_http + step_size * 2, base_dist + step_size * 2},
      {base_http + step_size * 3, base_dist + step_size * 3},
      # Try some completely different ranges
      {5000, 10000},
      {6000, 11000},
      {7000, 12000}
    ]
    |> Enum.filter(fn {http, dist} ->
      # Ensure ports are in valid range
      http > 1024 and http < 60000 and dist > 1024 and dist < 60000
    end)
  end
  
  defp get_node_port_info(node) do
    # Try to extract port information from the node name
    # This is a best-effort approach since we may not have direct access
    case Atom.to_string(node) do
      node_string ->
        case String.split(node_string, "@") do
          [_name, host] ->
            %{
              node: node,
              host: host,
              # We can't easily determine the actual ports without more context
              # This would need to be enhanced based on how nodes are actually started
              estimated_ports: :unknown
            }
          _ ->
            %{node: node, host: :unknown, estimated_ports: :unknown}
        end
    end
  end
  
  defp find_port_conflicts(port_assignments, config) do
    all_ports = 
      port_assignments
      |> Map.values()
      |> Enum.flat_map(fn assignment ->
        [assignment.http_port, assignment.distribution_port]
      end)
    
    known_service_ports = get_known_service_ports(config)
    
    conflicts = 
      all_ports
      |> Enum.filter(fn port -> port in known_service_ports end)
      |> Enum.map(fn port ->
        service = get_service_for_port(port, config)
        %{port: port, conflicts_with: service}
      end)
    
    conflicts
  end
  
  defp get_known_service_ports(_config) do
    # Get ports that are commonly used by development services
    base_ports = [
      4000,  # Phoenix dev server default
      4001,  # Phoenix dev server alternative
      4002,  # Phoenix test server (from config)
      3000,  # Common web dev port
      8000,  # Common web dev port
      8080,  # Common web dev port
      5432,  # PostgreSQL default
      3306,  # MySQL default
      6379,  # Redis default
      9100,  # Common Elixir distribution port base
    ]
    
    # Add configured development ports if available
    dev_config_ports = get_development_config_ports()
    
    base_ports ++ dev_config_ports
  end
  
  defp get_development_config_ports do
    # Try to get ports from development configuration
    try do
      dev_endpoint_config = Application.get_env(:otp_supervisor, OtpSupervisorWeb.Endpoint, [])
      http_config = Keyword.get(dev_endpoint_config, :http, [])
      dev_port = Keyword.get(http_config, :port, 4000)
      [dev_port]
    rescue
      _ -> []
    end
  end
  
  defp get_service_for_port(port, _config) do
    case port do
      4000 -> "Phoenix development server"
      4001 -> "Phoenix development server (alternative)"
      4002 -> "Phoenix test server"
      3000 -> "Common web development server"
      8000 -> "Common web development server"
      8080 -> "Common web development server"
      5432 -> "PostgreSQL database"
      3306 -> "MySQL database"
      6379 -> "Redis server"
      9100 -> "Elixir distribution port"
      _ -> "Unknown service"
    end
  end
end