defmodule OtpSupervisorWeb.Api.V1.SystemControllerTest do
  use OtpSupervisorWeb.ConnCase, async: true

  describe "GET /api/v1/system/health" do
    test "returns system health information", %{conn: conn} do
      conn = get(conn, "/api/v1/system/health")

      assert %{
               "data" => %{
                 "status" => "healthy",
                 "metrics" => %{
                   "total_processes" => total_processes,
                   "memory_usage" => memory_usage,
                   "message_queue_lengths" => queue_lengths,
                   "supervision_health" => supervision_health
                 },
                 "timestamp" => timestamp
               }
             } = json_response(conn, 200)

      assert is_integer(total_processes)
      assert is_integer(memory_usage)
      assert is_integer(queue_lengths)
      assert is_float(supervision_health)
      assert is_integer(timestamp)
    end
  end

  describe "GET /api/v1/system/graph" do
    test "returns complete process relationship graph", %{conn: conn} do
      conn = get(conn, "/api/v1/system/graph")

      assert %{
               "data" => %{
                 "processes" => processes,
                 "links" => links,
                 "monitors" => monitors
               }
             } = json_response(conn, 200)

      assert is_list(processes)
      assert is_list(links)
      assert is_list(monitors)

      # Verify structure of processes
      if length(processes) > 0 do
        first_process = List.first(processes)
        assert Map.has_key?(first_process, "pid")
        assert Map.has_key?(first_process, "name")
        assert Map.has_key?(first_process, "type")
      end
    end
  end

  describe "GET /api/v1/system/bottlenecks" do
    test "identifies system bottlenecks", %{conn: conn} do
      conn = get(conn, "/api/v1/system/bottlenecks")

      assert %{
               "data" => %{
                 "bottlenecks" => bottlenecks,
                 "analysis" => analysis
               }
             } = json_response(conn, 200)

      assert is_list(bottlenecks)
      assert is_map(analysis)
    end
  end

  describe "GET /api/v1/system/anomalies" do
    test "detects system anomalies", %{conn: conn} do
      conn = get(conn, "/api/v1/system/anomalies")

      assert %{
               "data" => %{
                 "anomalies" => anomalies,
                 "summary" => summary
               }
             } = json_response(conn, 200)

      assert is_list(anomalies)
      assert is_map(summary)
    end
  end
end
