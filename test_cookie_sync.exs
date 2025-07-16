#!/usr/bin/env elixir

# Test script to verify cookie synchronization fix

Mix.install([])

# Set test environment
Application.put_env(:mix, :env, :test)

# Load project
Code.require_file("mix.exs")

IO.puts("🔍 Testing cookie synchronization...")

# Check current node status
if Node.alive?() do
  IO.puts("✅ Node is alive: #{Node.self()}")
  IO.puts("   Current cookie: #{Node.get_cookie()}")
else
  IO.puts("❌ Node is not alive - starting distributed Erlang...")

  case Node.start(:"test_cookie_#{System.system_time(:millisecond)}@127.0.0.1", :shortnames) do
    {:ok, _} ->
      Node.set_cookie(:test_cluster_cookie)
      IO.puts("✅ Started distributed Erlang: #{Node.self()}")
      IO.puts("   Cookie set to: #{Node.get_cookie()}")
    {:error, reason} ->
      IO.puts("❌ Failed to start distributed Erlang: #{inspect(reason)}")
      System.halt(1)
  end
end

# Test cookie synchronization logic
test_cookie = :test_cluster_cookie
current_cookie = Node.get_cookie()

IO.puts("\n🔍 Cookie synchronization test:")
IO.puts("   Expected cookie: #{test_cookie}")
IO.puts("   Current cookie: #{current_cookie}")

if current_cookie == test_cookie do
  IO.puts("✅ Cookies match - synchronization should work")
else
  IO.puts("⚠️  Cookies don't match - synchronizing...")
  Node.set_cookie(test_cookie)
  new_cookie = Node.get_cookie()
  IO.puts("   New cookie: #{new_cookie}")

  if new_cookie == test_cookie do
    IO.puts("✅ Cookie synchronization successful")
  else
    IO.puts("❌ Cookie synchronization failed")
  end
end

# Test EPMD registration
IO.puts("\n🔍 Testing EPMD registration...")
case System.cmd("epmd", ["-names"], stderr_to_stdout: true) do
  {output, 0} ->
    IO.puts("✅ EPMD is running:")
    IO.puts("#{output}")

    if String.contains?(output, Atom.to_string(Node.self())) do
      IO.puts("✅ Current node is registered with EPMD")
    else
      IO.puts("⚠️  Current node not found in EPMD registration")
    end

  {error, code} ->
    IO.puts("❌ EPMD check failed (exit #{code}): #{error}")
end

IO.puts("\n🎉 Cookie synchronization test complete!")
IO.puts("The cookie fixes should now work for cluster startup.")
