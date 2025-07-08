defmodule OtpSupervisorWeb.Api.V1.SupervisorControllerTest do
  use OtpSupervisorWeb.ConnCase, async: true

  describe "GET /api/v1/supervisors" do
    test "lists all supervisors in the system", %{conn: conn} do
      conn = get(conn, "/api/v1/supervisors")

      assert %{
               "data" => supervisors,
               "meta" => meta
             } = json_response(conn, 200)

      assert is_list(supervisors)
      assert is_map(meta)

      # Verify supervisor structure
      if length(supervisors) > 0 do
        first_supervisor = List.first(supervisors)
        assert Map.has_key?(first_supervisor, "name")
        assert Map.has_key?(first_supervisor, "pid")
        assert Map.has_key?(first_supervisor, "alive")
        assert Map.has_key?(first_supervisor, "child_count")
      end
    end
  end

  describe "GET /api/v1/supervisors/:name" do
    setup do
      %{supervisor: supervisor} = SupervisorTestHelper.setup_isolated_supervisor("api_test")
      {:ok, supervisor: supervisor}
    end

    test "returns detailed supervisor information", %{conn: conn, supervisor: supervisor} do
      conn = get(conn, "/api/v1/supervisors/#{supervisor}")

      assert %{
               "data" => %{
                 "name" => name,
                 "pid" => pid,
                 "alive" => alive,
                 "child_count" => child_count,
                 "children" => children,
                 "strategy" => strategy
               }
             } = json_response(conn, 200)

      assert is_binary(name) or is_atom(name)
      assert is_binary(pid)
      assert is_boolean(alive)
      assert is_integer(child_count)
      assert is_list(children)
      assert is_binary(strategy)
    end

    test "returns 404 for non-existent supervisor", %{conn: conn} do
      conn = get(conn, "/api/v1/supervisors/non_existent_supervisor")

      assert %{
               "error" => %{
                 "message" => "Supervisor not found",
                 "code" => "supervisor_not_found"
               }
             } = json_response(conn, 404)
    end
  end

  describe "GET /api/v1/supervisors/:name/analytics" do
    setup do
      %{supervisor: supervisor} = SupervisorTestHelper.setup_isolated_supervisor("analytics_test")
      {:ok, supervisor: supervisor}
    end

    test "returns supervisor analytics data", %{conn: conn, supervisor: supervisor} do
      conn = get(conn, "/api/v1/supervisors/#{supervisor}/analytics")

      assert %{
               "data" => %{
                 "restart_history" => restart_history,
                 "restart_intensity" => restart_intensity,
                 "restart_storm_risk" => restart_storm_risk,
                 "performance_metrics" => performance_metrics
               }
             } = json_response(conn, 200)

      assert is_list(restart_history)
      assert is_float(restart_intensity)
      assert is_map(restart_storm_risk)
      assert is_map(performance_metrics)
    end
  end

  describe "POST /api/v1/supervisors/:name/pause" do
    setup do
      %{supervisor: supervisor} = SupervisorTestHelper.setup_isolated_supervisor("pause_test")
      {:ok, supervisor: supervisor}
    end

    test "pauses supervisor successfully", %{conn: conn, supervisor: supervisor} do
      conn = post(conn, "/api/v1/supervisors/#{supervisor}/pause")

      assert %{
               "data" => %{
                 "status" => "paused",
                 "supervisor" => supervisor_name
               }
             } = json_response(conn, 200)

      assert supervisor_name == Atom.to_string(supervisor)
    end
  end

  describe "POST /api/v1/supervisors/:name/resume" do
    setup do
      %{supervisor: supervisor} = SupervisorTestHelper.setup_isolated_supervisor("resume_test")
      {:ok, supervisor: supervisor}
    end

    test "resumes supervisor successfully", %{conn: conn, supervisor: supervisor} do
      # First pause the supervisor
      post(conn, "/api/v1/supervisors/#{supervisor}/pause")

      # Then resume it
      conn = post(conn, "/api/v1/supervisors/#{supervisor}/resume")

      assert %{
               "data" => %{
                 "status" => "resumed",
                 "supervisor" => supervisor_name
               }
             } = json_response(conn, 200)

      assert supervisor_name == Atom.to_string(supervisor)
    end
  end

  describe "PUT /api/v1/supervisors/:name/strategy" do
    setup do
      %{supervisor: supervisor} = SupervisorTestHelper.setup_isolated_supervisor("strategy_test")
      {:ok, supervisor: supervisor}
    end

    test "changes supervisor strategy", %{conn: conn, supervisor: supervisor} do
      conn =
        put(conn, "/api/v1/supervisors/#{supervisor}/strategy", %{
          "strategy" => "one_for_all"
        })

      assert %{
               "data" => %{
                 "status" => "strategy_changed",
                 "new_strategy" => "one_for_all",
                 "supervisor" => supervisor_name
               }
             } = json_response(conn, 200)

      assert supervisor_name == Atom.to_string(supervisor)
    end
  end

  describe "POST /api/v1/supervisors/:name/simulate-failure" do
    setup do
      %{supervisor: supervisor} = SupervisorTestHelper.setup_isolated_supervisor("failure_test")
      {:ok, supervisor: supervisor}
    end

    test "simulates failure in supervisor", %{conn: conn, supervisor: supervisor} do
      conn =
        post(conn, "/api/v1/supervisors/#{supervisor}/simulate-failure", %{
          "failure_type" => "child_crash",
          "target" => "random",
          "reason" => "test_crash"
        })

      assert %{
               "data" => %{
                 "status" => "failure_simulated",
                 "failure_type" => "child_crash",
                 "supervisor" => supervisor_name
               }
             } = json_response(conn, 200)

      assert supervisor_name == Atom.to_string(supervisor)
    end
  end
end
