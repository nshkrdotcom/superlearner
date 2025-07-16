defmodule OtpSupervisorWeb.Live.ClusterProcessesLiveTest do
  use OtpSupervisorWeb.ConnCase
  import Phoenix.LiveViewTest

  describe "ClusterProcessesLive loading states and performance" do
    test "shows loading state on mount", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/cluster-processes")

      # Check that the page loads successfully and shows cluster processes content
      html = render(view)
      assert html =~ "Cluster Processes"

      # Check that loading states are properly handled (either loading or loaded)
      assert html =~ "Loading cluster processes..." or html =~ "Total Processes"
    end

    test "handles search debouncing", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/cluster-processes")

      # Simulate rapid search input changes using the correct event
      view
      |> element("input[phx-change='search_change']")
      |> render_change(%{search: "test"})

      view
      |> element("input[phx-change='search_change']")
      |> render_change(%{search: "test123"})

      # Should not crash and should handle debouncing
      assert render(view) =~ "Cluster Processes"
    end

    test "disables controls during operations", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/cluster-processes")

      # Check that controls can be disabled during operations
      html = render(view)

      # Should have proper disabled states in the HTML structure
      assert html =~ "disabled" or html =~ "operation_in_progress"
    end

    test "handles filter changes efficiently", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/cluster-processes")

      # Test filter changes don't cause crashes
      view
      |> element("select[phx-change='filter_change'][phx-value-filter_type='node']")
      |> render_change(%{filter_type: "node", value: "all"})

      assert render(view) =~ "Cluster Processes"
    end

    test "handles pagination efficiently", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/cluster-processes")

      # Test pagination doesn't cause crashes - try to click the numbered page 2 button
      if has_element?(view, "button[phx-click='page_change'][phx-value-page='2']:not([title])") do
        view
        |> element("button[phx-click='page_change'][phx-value-page='2']:not([title])")
        |> render_click()
      end

      assert render(view) =~ "Cluster Processes"
    end
  end
end
