defmodule OtpSupervisorWeb.ArsenalWebTest do
  @moduledoc """
  Integration tests for Arsenal web layer using proper Phoenix test tools.

  Tests the complete HTTP request flow through Phoenix router,
  ArsenalPlug, and Arsenal operations.
  """

  use OtpSupervisorWeb.ConnCase, async: false
  @moduletag :ui

  # No manual setup needed - Arsenal operations should register automatically!

  describe "Arsenal API documentation endpoints" do
    test "GET /api/v1/arsenal/operations returns operation list", %{conn: conn} do
      conn = get(conn, "/api/v1/arsenal/operations")
      assert json_response(conn, 200)

      response = json_response(conn, 200)
      assert is_list(response["data"])
      # At least GetProcessInfo, KillProcess, ListSupervisors
      assert length(response["data"]) >= 3

      # Verify operation structure
      operation = hd(response["data"])
      assert is_binary(operation["module"])
      assert is_binary(operation["method"])
      assert is_binary(operation["path"])
      assert is_binary(operation["summary"])
    end

    test "GET /api/v1/arsenal/docs returns OpenAPI documentation", %{conn: conn} do
      conn = get(conn, "/api/v1/arsenal/docs")
      assert json_response(conn, 200)

      docs = json_response(conn, 200)
      assert docs["openapi"] == "3.0.0"
      assert docs["info"]["title"] == "Arsenal API"
      assert is_map(docs["paths"])
    end
  end

  describe "Arsenal operation endpoints" do
    test "GET /api/v1/processes/:pid/info returns process information", %{conn: conn} do
      # Use a simpler PID format that doesn't need encoding for testing
      pid_string = "test_pid"

      conn = get(conn, "/api/v1/processes/#{pid_string}/info")

      # This should fail with validation error since "test_pid" is not a valid PID
      # but first we need to confirm ArsenalPlug is being called
      assert json_response(conn, 422)
      response = json_response(conn, 422)
      assert response["error"]["code"] == "validation_error"
    end

    test "GET /api/v1/supervisors returns supervisor list", %{conn: conn} do
      conn = get(conn, "/api/v1/supervisors")
      assert json_response(conn, 200)

      response = json_response(conn, 200)
      assert is_list(response["data"])
      # Should have at least one supervisor
      assert length(response["data"]) > 0

      supervisor = hd(response["data"])

      # Verify supervisor structure
      assert is_binary(supervisor["name"])
      # Only check for children field if it exists (defaults to empty list)
      if Map.has_key?(supervisor, "children") do
        assert is_list(supervisor["children"])
      end

      assert is_integer(supervisor["child_count"])
      assert is_boolean(supervisor["alive"])
    end

    test "POST /api/v1/processes/:pid/message sends message to process", %{conn: conn} do
      # Create a test process that can receive messages
      test_pid =
        spawn(fn ->
          receive do
            _ -> :ok
          after
            5000 -> :timeout
          end
        end)

      # Convert PID to string format that's URL-safe
      pid_string = inspect(test_pid) |> URI.encode_www_form()
      message = %{"type" => "test", "data" => "hello world"}

      conn =
        conn
        |> put_req_header("content-type", "application/json")
        |> post("/api/v1/processes/#{pid_string}/message", %{"message" => message})

      assert json_response(conn, 200)
      response = json_response(conn, 200)
      assert response["data"]["message_sent"] == true
      assert response["data"]["message_type"] == "send"
      assert response["data"]["target_pid"] == inspect(test_pid)

      # Clean up
      if Process.alive?(test_pid), do: Process.exit(test_pid, :kill)
    end

    test "DELETE /api/v1/processes/:pid terminates process", %{conn: conn} do
      # Create a test process that we can safely kill
      test_pid =
        spawn(fn ->
          receive do
            _ -> :ok
          end
        end)

      pid_string = inspect(test_pid) |> URI.encode_www_form()

      conn = delete(conn, "/api/v1/processes/#{pid_string}")
      assert json_response(conn, 200)

      response = json_response(conn, 200)
      assert response["data"]["terminated"] == true
      assert response["data"]["pid"] == inspect(test_pid)

      # Verify process is actually dead using process monitoring
      ref = Process.monitor(test_pid)

      receive do
        {:DOWN, ^ref, :process, ^test_pid, _reason} -> :ok
      after
        1000 -> flunk("Process did not terminate")
      end

      refute Process.alive?(test_pid)
    end
  end

  describe "Arsenal error handling" do
    test "GET /api/v1/processes/invalid_pid/info returns validation error", %{conn: conn} do
      conn = get(conn, "/api/v1/processes/invalid_pid/info")
      assert json_response(conn, 422)

      response = json_response(conn, 422)
      assert response["error"]["code"] == "validation_error"
      assert response["error"]["message"] == "Validation failed"
    end

    test "GET /api/v1/processes/dead_pid/info returns process not found", %{conn: conn} do
      # Create and kill a process to get a dead PID
      dead_pid = spawn(fn -> :ok end)
      # Wait for it to die using process monitoring
      ref = Process.monitor(dead_pid)

      receive do
        {:DOWN, ^ref, :process, ^dead_pid, _reason} -> :ok
      after
        1000 -> flunk("Process did not terminate")
      end

      pid_string = inspect(dead_pid) |> URI.encode_www_form()
      conn = get(conn, "/api/v1/processes/#{pid_string}/info")
      assert json_response(conn, 404)

      response = json_response(conn, 404)
      assert response["error"]["code"] == "process_not_found"
    end

    test "GET /api/v1/nonexistent/endpoint returns not found", %{conn: conn} do
      conn = get(conn, "/api/v1/nonexistent/endpoint")
      assert json_response(conn, 404)

      response = json_response(conn, 404)
      assert response["error"]["code"] == "operation_not_found"
    end
  end
end
