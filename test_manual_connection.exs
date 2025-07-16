#!/usr/bin/env elixir

# Manual test to verify distributed Erlang connection works

# Start this node in distributed mode
node_name = :"test_manual_#{System.system_time(:millisecond)}@127.0.0.1"

case Node.start(node_name, :longnames) do
  {:ok, _} ->
    Node.set_cookie(:test_cluster_cookie)
    IO.puts("✅ Started distributed node: #{Node.self()}")
    IO.puts("   Cookie: #{Node.get_cookie()}")

    # Try to start a child node manually
    IO.puts("\n🔍 Starting child node manually...")

    env = [
      {"MIX_ENV", "test"},
      {"ERLANG_COOKIE", "test_cluster_cookie"}
    ]

    cmd_args = [
      "--name", "test_child@127.0.0.1",
      "--cookie", "test_cluster_cookie",
      "-e", "IO.puts(\"Child node started: \#{Node.self()}\"); IO.puts(\"Child cookie: \#{Node.get_cookie()}\"); :timer.sleep(30000)"
    ]

    IO.puts("Command: elixir #{Enum.join(cmd_args, " ")}")

    task = Task.async(fn ->
      System.cmd("elixir", cmd_args, env: env, into: IO.stream(:stdio, :line))
    end)

    # Give child time to start
    :timer.sleep(3000)

    # Try to connect
    IO.puts("\n🔍 Attempting to connect to child node...")
    case Node.ping(:"test_child@127.0.0.1") do
      :pong ->
        IO.puts("✅ Successfully connected to child node!")
        IO.puts("   Connected nodes: #{inspect(Node.list())}")

      :pang ->
        IO.puts("❌ Failed to connect to child node")

        # Check EPMD
        case System.cmd("epmd", ["-names"]) do
          {output, 0} ->
            IO.puts("EPMD status:")
            IO.puts(output)
          {error, _} ->
            IO.puts("EPMD error: #{error}")
        end
    end

    # Cleanup
    Task.shutdown(task, :brutal_kill)

  {:error, reason} ->
    IO.puts("❌ Failed to start distributed node: #{inspect(reason)}")
end
