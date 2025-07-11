# Test script to check cluster connectivity
IO.puts("Current node: #{Node.self()}")
IO.puts("Connected nodes: #{inspect(Node.list())}")

# Get the hostname for short names  
{:ok, hostname} = :inet.gethostname()
hostname_str = to_string(hostname)

# Try to connect to both nodes using the detected hostname
node1 = :"superlearner1@#{hostname_str}"
node2 = :"superlearner2@#{hostname_str}"
nodes_to_test = [node1, node2]

IO.puts("Testing connections to nodes with hostname: #{hostname_str}")

for node <- nodes_to_test do
  if node != Node.self() do
    case Node.connect(node) do
      true ->
        IO.puts("✅ Successfully connected to #{node}")

      false ->
        IO.puts("❌ Failed to connect to #{node}")
    end
  end
end

IO.puts("Final connected nodes: #{inspect(Node.list())}")

# Test if both nodes are reachable
if node1 in Node.list() do
  IO.puts("✅ #{node1} is reachable")
end

if node2 in Node.list() do
  IO.puts("✅ #{node2} is reachable")
end
