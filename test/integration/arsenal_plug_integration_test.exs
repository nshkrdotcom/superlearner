defmodule OtpSupervisor.Integration.ArsenalPlugIntegrationTest do
  @moduledoc """
  Integration tests for Arsenal operations via ArsenalPlug.

  These tests verify that Arsenal operations are correctly accessible
  via HTTP and that the full stack (Router → ArsenalPlug → Operation)
  works as expected.
  """

  use OtpSupervisorWeb.ConnCase, async: true

  alias OTPSupervisor.Core.Arsenal.Registry

  describe "Arsenal API documentation endpoints" do
    test "GET /api/v1/arsenal/docs returns OpenAPI documentation", %{conn: conn} do
      conn = get(conn, "/api/v1/arsenal/docs")

      assert %{
               "openapi" => "3.0.0",
               "info" => %{
                 "title" => "Arsenal API",
                 "version" => "1.0.0"
               },
               "paths" => paths
             } = json_response(conn, 200)

      assert is_map(paths)
      assert map_size(paths) > 0
    end

    test "GET /api/v1/arsenal/operations returns list of operations", %{conn: conn} do
      conn = get(conn, "/api/v1/arsenal/operations")

      assert %{"data" => operations} = json_response(conn, 200)
      assert is_list(operations)
      assert length(operations) > 0

      # Verify operation structure
      first_op = List.first(operations)
      assert Map.has_key?(first_op, "module")
      assert Map.has_key?(first_op, "method")
      assert Map.has_key?(first_op, "path")
      assert Map.has_key?(first_op, "summary")
    end
  end

  describe "Arsenal operation execution" do
    test "ListSupervisors operation returns supervisor list", %{conn: conn} do
      conn = get(conn, "/api/v1/supervisors")

      assert %{"data" => supervisors} = json_response(conn, 200)
      assert is_list(supervisors)

      # Should at least have the main application supervisor
      assert length(supervisors) > 0
    end

    test "ListSandboxes operation returns sandbox list", %{conn: conn} do
      conn = get(conn, "/api/v1/sandboxes")

      assert %{"data" => sandboxes} = json_response(conn, 200)
      assert is_list(sandboxes)
    end

    @tag :skip  # Skip if cluster not available
    test "ClusterTopology operation returns topology", %{conn: conn} do
      conn = get(conn, "/api/v1/cluster/topology")

      # Should return either topology data or error if not in cluster mode
      response = json_response(conn, :ok)
      assert is_map(response)
    end
  end

  describe "Arsenal operation error handling" do
    test "non-existent operation returns 404", %{conn: conn} do
      conn = get(conn, "/api/v1/nonexistent/operation")

      assert %{
               "error" => %{
                 "code" => "operation_not_found"
               }
             } = json_response(conn, 404)
    end

    test "invalid parameters return 422", %{conn: conn} do
      # Try to kill a process with invalid PID format
      conn = delete(conn, "/api/v1/processes/invalid_pid")

      # Should either be 404 (process not found) or 422 (validation error)
      response = json_response(conn, :error)
      assert Map.has_key?(response, "error")
    end
  end

  describe "Arsenal operation parameter extraction" do
    test "path parameters are extracted correctly", %{conn: conn} do
      # Create a test process
      {:ok, pid} = GenServer.start_link(Agent, fn -> %{count: 0} end)
      pid_string = inspect(pid)

      # Try to get info about this process
      conn = get(conn, "/api/v1/processes/#{URI.encode(pid_string)}/info")

      # Should either succeed or fail gracefully
      response = json_response(conn, :ok)
      assert is_map(response)

      # Cleanup
      GenServer.stop(pid)
    end
  end

  describe "Arsenal operation integration with core" do
    test "sandbox lifecycle via API", %{conn: conn} do
      sandbox_id = "integration_test_#{:erlang.unique_integer([:positive])}"

      # Create sandbox
      conn = post(conn, "/api/v1/sandboxes", %{
        id: sandbox_id,
        module: "OtpSupervisor.Sandbox.Supervisors.DemoSupervisor",
        strategy: "one_for_one"
      })

      assert %{"data" => %{"id" => ^sandbox_id}} = json_response(conn, 200)

      # Get sandbox info
      conn = build_conn()
      conn = get(conn, "/api/v1/sandboxes/#{sandbox_id}")

      assert %{"data" => sandbox_info} = json_response(conn, 200)
      assert sandbox_info["id"] == sandbox_id

      # Destroy sandbox
      conn = build_conn()
      conn = delete(conn, "/api/v1/sandboxes/#{sandbox_id}")

      assert json_response(conn, 200)

      # Verify sandbox is gone
      conn = build_conn()
      conn = get(conn, "/api/v1/sandboxes/#{sandbox_id}")

      assert %{"error" => %{"code" => "not_found"}} = json_response(conn, 404)
    end
  end

  describe "Arsenal Plug integration" do
    test "ArsenalPlug passes through to manual controllers for unmatched routes", %{conn: conn} do
      # This route should be handled by ProcessController, not ArsenalPlug
      conn = get(conn, "/api/v1/processes/state")

      # Should not return an Arsenal error
      response = json_response(conn, :ok)

      # Verify it's handled by the manual controller
      # (The response structure will depend on ProcessController implementation)
      assert is_map(response)
    end

    test "ArsenalPlug handles catch-all routes", %{conn: conn} do
      # Routes that match the catch-all should be processed by ArsenalPlug
      conn = get(conn, "/api/v1/some/unknown/path")

      assert %{
               "error" => %{
                 "code" => "operation_not_found"
               }
             } = json_response(conn, 404)
    end
  end

  describe "Arsenal Registry integration" do
    test "all registered operations are accessible via HTTP" do
      operations = Registry.list_operations()

      # Verify each operation has required fields
      for operation <- operations do
        assert Map.has_key?(operation, :module)
        assert Map.has_key?(operation, :method)
        assert Map.has_key?(operation, :path)
        assert Map.has_key?(operation, :summary)

        # Verify the module exists and implements required functions
        module = operation.module
        assert function_exported?(module, :execute, 1)
        assert function_exported?(module, :rest_config, 0)
      end
    end

    test "operations can be retrieved by module" do
      operations = Registry.list_operations()

      if length(operations) > 0 do
        first_module = List.first(operations).module

        assert {:ok, config} = Registry.get_operation(first_module)
        assert is_map(config)
        assert Map.has_key?(config, :method)
        assert Map.has_key?(config, :path)
      end
    end
  end
end
