# Test complete Arsenal integration with isolated compilation
IO.puts("🧪 Testing complete Arsenal integration...")

# Helper function to create sandbox
defmodule ArsenalTest do
  def create_sandbox(id, module) do
    params = %{
      "sandbox_id" => id,
      "supervisor_module" => module,
      "strategy" => "one_for_one",
      "max_restarts" => "3",
      "max_seconds" => "5"
    }

    with {:ok, validated} <- OTPSupervisor.Core.Arsenal.Operations.CreateSandbox.validate_params(params),
         {:ok, sandbox_info} <- OTPSupervisor.Core.Arsenal.Operations.CreateSandbox.execute(validated) do
      formatted = OTPSupervisor.Core.Arsenal.Operations.CreateSandbox.format_response(sandbox_info)
      {:ok, formatted.data}
    end
  end

  def list_sandboxes() do
    params = %{"status" => nil, "page" => 1, "per_page" => 10}
    
    with {:ok, validated} <- OTPSupervisor.Core.Arsenal.Operations.ListSandboxes.validate_params(params),
         {:ok, {sandboxes, meta}} <- OTPSupervisor.Core.Arsenal.Operations.ListSandboxes.execute(validated) do
      formatted = OTPSupervisor.Core.Arsenal.Operations.ListSandboxes.format_response({sandboxes, meta})
      {:ok, formatted.data, meta}
    end
  end

  def get_sandbox_info(id) do
    params = %{"sandbox_id" => id, "include_children" => true, "include_stats" => true}
    
    with {:ok, validated} <- OTPSupervisor.Core.Arsenal.Operations.GetSandboxInfo.validate_params(params),
         {:ok, sandbox_info} <- OTPSupervisor.Core.Arsenal.Operations.GetSandboxInfo.execute(validated) do
      formatted = OTPSupervisor.Core.Arsenal.Operations.GetSandboxInfo.format_response(sandbox_info)
      {:ok, formatted.data}
    end
  end

  def destroy_sandbox(id) do
    params = %{"sandbox_id" => id, "force" => false}
    
    with {:ok, validated} <- OTPSupervisor.Core.Arsenal.Operations.DestroySandbox.validate_params(params),
         {:ok, result} <- OTPSupervisor.Core.Arsenal.Operations.DestroySandbox.execute(validated) do
      formatted = OTPSupervisor.Core.Arsenal.Operations.DestroySandbox.format_response(result)
      {:ok, formatted.data}
    end
  end
end

# Test 1: Create sandbox
IO.puts("\n1. Creating sandbox via Arsenal...")
case ArsenalTest.create_sandbox("test-complete", "OtpSandbox.TestDemoSupervisor") do
  {:ok, sandbox} ->
    IO.puts("  ✅ Created: #{sandbox.id}")
    IO.puts("    - App PID: #{sandbox.app_pid}")
    IO.puts("    - Supervisor PID: #{sandbox.supervisor_pid}")
    IO.puts("    - Status: #{sandbox.status}")
  {:error, reason} ->
    IO.puts("  ❌ Failed: #{inspect(reason)}")
end

# Test 2: List sandboxes
IO.puts("\n2. Listing sandboxes via Arsenal...")
case ArsenalTest.list_sandboxes() do
  {:ok, sandboxes, meta} ->
    IO.puts("  ✅ Found #{length(sandboxes)} sandboxes")
    IO.puts("  📊 Meta: page #{meta.page}, total #{meta.total}")
    Enum.each(sandboxes, fn sandbox ->
      IO.puts("    - #{sandbox.id} (#{sandbox.status})")
    end)
  {:error, reason} ->
    IO.puts("  ❌ Failed: #{inspect(reason)}")
end

# Test 3: Get detailed info
IO.puts("\n3. Getting sandbox info via Arsenal...")
case ArsenalTest.get_sandbox_info("test-complete") do
  {:ok, info} ->
    IO.puts("  ✅ Info retrieved: #{info.id}")
    IO.puts("    - Status: #{info.status}")
    IO.puts("    - Uptime: #{info.uptime_seconds}s")
    if Map.has_key?(info, :children) do
      IO.puts("    - Children: #{length(info.children)}")
      Enum.each(info.children, fn child ->
        IO.puts("      * #{child.id}: #{child.pid} (alive: #{child.alive})")
      end)
    end
    if Map.has_key?(info, :memory_usage) do
      total_mem = info.memory_usage.total_memory_bytes
      IO.puts("    - Total memory: #{total_mem} bytes")
    end
  {:error, reason} ->
    IO.puts("  ❌ Failed: #{inspect(reason)}")
end

# Test 4: Destroy sandbox
IO.puts("\n4. Destroying sandbox via Arsenal...")
case ArsenalTest.destroy_sandbox("test-complete") do
  {:ok, result} ->
    IO.puts("  ✅ Destroyed: #{result.sandbox_id}")
    IO.puts("    - Destroyed at: #{result.destroyed_at}")
  {:error, reason} ->
    IO.puts("  ❌ Failed: #{inspect(reason)}")
end

# Test 5: Verify destruction
IO.puts("\n5. Verifying destruction...")
case ArsenalTest.list_sandboxes() do
  {:ok, sandboxes, meta} ->
    IO.puts("  ✅ Remaining sandboxes: #{length(sandboxes)}")
    IO.puts("  📊 Meta: total #{meta.total}")
  {:error, reason} ->
    IO.puts("  ❌ Failed: #{inspect(reason)}")
end

IO.puts("\n🎯 Arsenal integration test complete!")