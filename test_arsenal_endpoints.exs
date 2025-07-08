#!/usr/bin/env elixir

# Start the application in the background
spawn(fn ->
  Mix.start()
  Mix.env(:dev)
  Application.ensure_all_started(:otp_supervisor)

  # Keep it running
  receive do
    :stop -> :ok
  end
end)

# Wait for startup
Process.sleep(2000)

IO.puts("Testing Arsenal Endpoints...")
IO.puts("=" |> String.duplicate(40))

# Test 1: Arsenal documentation
IO.puts("\n1. Testing /api/v1/arsenal/operations")

case HTTPoison.get("http://localhost:4000/api/v1/arsenal/operations") do
  {:ok, %{status_code: 200, body: body}} ->
    IO.puts("✅ SUCCESS: Got #{String.length(body)} characters")
    operations = Jason.decode!(body)
    IO.puts("   Found #{length(operations["data"])} operations")

  {:ok, %{status_code: code}} ->
    IO.puts("❌ FAILED: HTTP #{code}")

  {:error, reason} ->
    IO.puts("❌ FAILED: #{inspect(reason)}")
end

# Test 2: Process info endpoint
IO.puts("\n2. Testing /api/v1/processes/self/info")
pid_string = inspect(self())

case HTTPoison.get("http://localhost:4000/api/v1/processes/#{URI.encode(pid_string)}/info") do
  {:ok, %{status_code: 200, body: body}} ->
    IO.puts("✅ SUCCESS: Got process info")
    info = Jason.decode!(body)
    IO.puts("   Process status: #{get_in(info, ["data", "status"])}")

  {:ok, %{status_code: code}} ->
    IO.puts("❌ FAILED: HTTP #{code}")

  {:error, reason} ->
    IO.puts("❌ FAILED: #{inspect(reason)}")
end

# Test 3: Supervisors endpoint
IO.puts("\n3. Testing /api/v1/supervisors")

case HTTPoison.get("http://localhost:4000/api/v1/supervisors") do
  {:ok, %{status_code: 200, body: body}} ->
    IO.puts("✅ SUCCESS: Got supervisors list")
    supervisors = Jason.decode!(body)
    IO.puts("   Found #{length(supervisors["data"])} supervisors")

  {:ok, %{status_code: code}} ->
    IO.puts("❌ FAILED: HTTP #{code}")

  {:error, reason} ->
    IO.puts("❌ FAILED: #{inspect(reason)}")
end

IO.puts(("\n" <> "=") |> String.duplicate(40))
IO.puts("Arsenal Endpoint Testing Complete!")
