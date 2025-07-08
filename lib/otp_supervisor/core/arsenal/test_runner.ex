defmodule OTPSupervisor.Core.Arsenal.TestRunner do
  @moduledoc """
  Simple test runner to verify the arsenal system works correctly.
  """

  alias OTPSupervisor.Core.Arsenal

  alias OTPSupervisor.Core.Arsenal.Operations.{
    GetProcessInfo,
    KillProcess,
    ListSupervisors,
    SendMessage,
    TraceProcess
  }

  def run_basic_tests do
    IO.puts("🔧 Testing Arsenal System...")

    # Test 1: Registry discovers operations
    test_operation_discovery()

    # Test 2: Operation validation works
    test_parameter_validation()

    # Test 3: Operations can be executed
    test_operation_execution()

    # Test 4: API documentation generation
    test_api_documentation()

    IO.puts("✅ Arsenal system tests completed!")
  end

  defp test_operation_discovery do
    IO.puts("  📋 Testing operation discovery...")

    operations = Arsenal.list_operations()
    operation_modules = Enum.map(operations, & &1.module)

    expected_modules = [GetProcessInfo, KillProcess, ListSupervisors, SendMessage, TraceProcess]

    found_modules = Enum.filter(expected_modules, &(&1 in operation_modules))

    IO.puts("    Found #{length(found_modules)}/#{length(expected_modules)} operations")

    Enum.each(found_modules, fn module ->
      {:ok, config} = Arsenal.get_operation_config(module)
      IO.puts("    ✓ #{inspect(module)} - #{config.method} #{config.path}")
    end)
  end

  defp test_parameter_validation do
    IO.puts("  🔍 Testing parameter validation...")

    # Test GetProcessInfo validation
    case GetProcessInfo.validate_params(%{"pid" => "<0.1.0>"}) do
      {:ok, _validated} -> IO.puts("    ✓ GetProcessInfo validation works")
      {:error, reason} -> IO.puts("    ✗ GetProcessInfo validation failed: #{inspect(reason)}")
    end

    # Test invalid PID
    case GetProcessInfo.validate_params(%{"pid" => "invalid"}) do
      {:error, _reason} -> IO.puts("    ✓ GetProcessInfo correctly rejects invalid PID")
      {:ok, _} -> IO.puts("    ✗ GetProcessInfo should reject invalid PID")
    end

    # Test ListSupervisors validation
    case ListSupervisors.validate_params(%{"page" => "1", "per_page" => "10"}) do
      {:ok, _validated} -> IO.puts("    ✓ ListSupervisors validation works")
      {:error, reason} -> IO.puts("    ✗ ListSupervisors validation failed: #{inspect(reason)}")
    end
  end

  defp test_operation_execution do
    IO.puts("  ⚡ Testing operation execution...")

    # Test ListSupervisors execution
    params = %{"include_children" => false, "page" => 1, "per_page" => 10}

    case ListSupervisors.execute(params) do
      {:ok, {supervisors, meta}} when is_list(supervisors) ->
        IO.puts(
          "    ✓ ListSupervisors executed successfully (found #{length(supervisors)} supervisors)"
        )

        IO.puts("    📊 Meta: #{inspect(meta)}")

      {:error, reason} ->
        IO.puts("    ✗ ListSupervisors execution failed: #{inspect(reason)}")
    end

    # Test GetProcessInfo for self
    self_pid = inspect(self())
    params = %{"pid" => self_pid}

    case GetProcessInfo.execute(params) do
      {:ok, info} when is_map(info) ->
        IO.puts("    ✓ GetProcessInfo executed successfully for self()")

      {:error, reason} ->
        IO.puts("    ✗ GetProcessInfo execution failed: #{inspect(reason)}")
    end
  end

  defp test_api_documentation do
    IO.puts("  📚 Testing API documentation generation...")

    docs = Arsenal.generate_api_docs()

    path_count = docs.paths |> Map.keys() |> length()
    IO.puts("    ✓ Generated OpenAPI docs with #{path_count} paths")

    # Show sample paths
    docs.paths
    |> Map.keys()
    |> Enum.take(3)
    |> Enum.each(fn path ->
      methods = docs.paths[path] |> Map.keys() |> Enum.join(", ")
      IO.puts("    📍 #{path} (#{methods})")
    end)
  end

  def show_available_endpoints do
    IO.puts("🌐 Available Arsenal API Endpoints:")

    operations = Arsenal.list_operations()

    operations
    |> Enum.sort_by(& &1.path)
    |> Enum.each(fn operation ->
      method = operation.method |> Atom.to_string() |> String.upcase()
      IO.puts("  #{method} #{operation.path}")
      IO.puts("    📝 #{operation.summary}")

      if Map.has_key?(operation, :parameters) && length(operation.parameters) > 0 do
        IO.puts("    📋 Parameters:")

        Enum.each(operation.parameters, fn param ->
          required = if Map.get(param, :required, false), do: " (required)", else: ""
          IO.puts("      - #{param.name} (#{param.type})#{required}")
        end)
      end

      IO.puts("")
    end)
  end
end
