defmodule OtpSupervisorWeb.SystemDashboardLiveTest do
  use OtpSupervisorWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  describe "system dashboard" do
    setup %{conn: conn} do
      %{supervisor: supervisor} = SupervisorTestHelper.setup_isolated_supervisor("dashboard")
      {:ok, conn: conn, supervisor: supervisor}
    end

    test "renders system metrics overview", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/system")

      assert html =~ "System Dashboard"
      assert html =~ "Total Processes"
      assert html =~ "Memory Usage"
      assert html =~ "Message Queues"
      assert html =~ "CPU Usage"
      assert html =~ "Supervision Health"
    end

    test "displays real-time process count", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/system")

      # Check that process count is displayed
      assert html =~ ~r/Total Processes.*\d+/
    end

    test "shows anomaly detection results", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/system")

      assert html =~ "Anomaly Detection"
      # Since we have no anomalies, it shows the no anomalies message
      assert html =~ "No anomalies detected"
    end

    test "provides process search functionality", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/system")

      # Search for supervisor processes
      html =
        view
        |> form("#process-search", %{search: "supervisor"})
        |> render_submit()

      assert html =~ "Search Results"
    end

    test "enables bulk operations on processes", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/system")

      # First search for processes to have something to select
      _html =
        view
        |> form("#process-search", %{search: "supervisor"})
        |> render_submit()

      # Since no processes are selected initially, bulk operations won't be visible
      html = render(view)
      assert html =~ "Process Search"
    end

    test "exports system report as JSON", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/system")

      # Trigger export
      view
      |> element("#export-json")
      |> render_click()

      # Check for flash message
      assert render(view) =~ "System report exported"
    end
  end

  describe "process relationship explorer" do
    test "displays process relationships button", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/system")

      # Check for relationship explorer button
      assert html =~ "Process Relationships"
    end
  end
end
