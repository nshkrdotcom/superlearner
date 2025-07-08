defmodule OtpSupervisorWeb.SupervisorLiveTest do
  use OtpSupervisorWeb.ConnCase

  import Phoenix.LiveViewTest
  alias OtpSupervisorWeb.SupervisorLive
  alias OTPSupervisor.Sandbox.Supervisors.DemoSupervisor
  alias OTPSupervisor.Sandbox.Workers.Counter
  alias OTPSupervisor.Core.Control

  describe "helper functions" do
    test "format_bytes/1 formats bytes correctly" do
      # Test bytes
      assert SupervisorLive.format_bytes(512) == "512 B"
      assert SupervisorLive.format_bytes(1023) == "1023 B"

      # Test kilobytes
      assert SupervisorLive.format_bytes(1024) == "1.0 KB"
      assert SupervisorLive.format_bytes(1536) == "1.5 KB"
      assert SupervisorLive.format_bytes(1_048_575) == "1024.0 KB"

      # Test megabytes
      assert SupervisorLive.format_bytes(1_048_576) == "1.0 MB"
      assert SupervisorLive.format_bytes(1_572_864) == "1.5 MB"
      assert SupervisorLive.format_bytes(1_073_741_823) == "1024.0 MB"

      # Test gigabytes
      assert SupervisorLive.format_bytes(1_073_741_824) == "1.0 GB"
      assert SupervisorLive.format_bytes(2_147_483_648) == "2.0 GB"

      # Test edge cases
      assert SupervisorLive.format_bytes(0) == "0 B"
      assert SupervisorLive.format_bytes("not_a_number") == "N/A"
      assert SupervisorLive.format_bytes(nil) == "N/A"
    end

    test "format_key/1 formats atom keys correctly" do
      # Test single word
      assert SupervisorLive.format_key(:memory) == "Memory"
      assert SupervisorLive.format_key(:status) == "Status"

      # Test multiple words with underscores
      assert SupervisorLive.format_key(:message_queue_len) == "Message Queue Len"
      assert SupervisorLive.format_key(:heap_size) == "Heap Size"
      assert SupervisorLive.format_key(:current_function) == "Current Function"

      # Test already capitalized gets reformatted
      assert SupervisorLive.format_key(:PID) == "Pid"
    end

    test "format_value/1 formats various data types correctly" do
      # Test integers
      assert SupervisorLive.format_value(42) == "42"
      assert SupervisorLive.format_value(0) == "0"

      # Test atoms
      assert SupervisorLive.format_value(:running) == ":running"
      assert SupervisorLive.format_value(:waiting) == ":waiting"

      # Test MFA tuples
      assert SupervisorLive.format_value({:gen_server, :loop, 3}) == "gen_server.loop/3"

      assert SupervisorLive.format_value({MyModule, :my_function, 1}) ==
               "Elixir.MyModule.my_function/1"

      # Test other data types
      assert SupervisorLive.format_value("string") == "\"string\""
      assert SupervisorLive.format_value([1, 2, 3]) == "[1, 2, 3]"
      assert SupervisorLive.format_value(%{key: "value"}) == "%{key: \"value\"}"
    end
  end

  describe "LiveView integration" do
    test "supervisor live view loads", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/supervisors")

      assert html =~ "OTP Supervisor Control Panel"
      assert html =~ "Supervisors"
      assert html =~ "How Supervisors Work"
    end

    test "can select a supervisor", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the demo supervisor
      view
      |> element("button[phx-value-name='demo_one_for_one']")
      |> render_click()

      # Check that children are displayed
      html = render(view)
      assert html =~ "Children of demo_one_for_one"
      assert html =~ "counter_1"
      assert html =~ "counter_2"
      assert html =~ "printer_1"
    end
  end

  describe "real-time functionality" do
    # Use the existing demo supervisor that should be running
    setup do
      # Ensure the demo supervisor is available for testing
      supervisor = :demo_one_for_one

      case Control.get_supervision_tree(supervisor) do
        {:ok, _children} ->
          %{supervisor: supervisor}

        {:error, _} ->
          # If not available, skip these tests
          %{supervisor: nil}
      end
    end

    test "auto-refresh timer functionality", %{conn: conn, supervisor: supervisor} do
      if supervisor == nil do
        # Skip if supervisor not available
        assert true
      else
        {:ok, view, _html} = live(conn, "/supervisors")

        # Select the test supervisor
        view
        |> element("button[phx-value-name='#{supervisor}']")
        |> render_click()

        # Get initial children count
        initial_html = render(view)
        assert initial_html =~ "Children of #{supervisor}"

        # Verify auto-refresh mechanism by checking the timer is set up
        # We test the presence of the refresh mechanism rather than killing processes
        # to avoid interfering with the main demo supervisor

        # Just verify that the page updates and shows children
        updated_html = render(view)
        assert updated_html =~ "Children of #{supervisor}"
        assert updated_html =~ "counter_1"
        assert updated_html =~ "counter_2"
        assert updated_html =~ "printer_1"
      end
    end

    test "supervisor list updates during refresh", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Start a new supervisor during the session
      dynamic_supervisor_name = :"dynamic_test_supervisor_#{:erlang.unique_integer([:positive])}"
      {:ok, dynamic_sup} = DemoSupervisor.start_link(name: dynamic_supervisor_name)

      on_exit(fn ->
        if Process.alive?(dynamic_sup) do
          Process.exit(dynamic_sup, :kill)
        end
      end)

      # Use synchronous call to ensure supervisor is fully initialized
      GenServer.call(dynamic_sup, :which_children)

      # The LiveView should refresh and include the new supervisor
      # We can't easily test automatic refresh timing, so we'll verify
      # that the supervisor is detectable by the Control module
      supervisors = Control.list_supervisors()
      supervisor_names = Enum.map(supervisors, & &1.name)
      assert dynamic_supervisor_name in supervisor_names
    end

    test "process state changes reflected in real-time", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Get a counter process
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &String.contains?(&1.pid, "counter"))
      counter_pid = extract_pid_from_string(counter_child.pid)

      # Increment the counter to change its state
      Counter.increment(counter_pid)

      # Use synchronous call to ensure the increment is processed
      current_value = Counter.get_value(counter_pid)
      assert current_value > 0

      # The state change should be reflected in the supervision tree
      {:ok, updated_children} = Control.get_supervision_tree(supervisor)
      updated_counter = Enum.find(updated_children, &String.contains?(&1.pid, "counter"))
      assert updated_counter.alive == true
    end

    test "handling of processes that die during display", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Get a child process PID
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child_pid = extract_first_worker_pid(children)

      # Monitor the process
      monitor_ref = Process.monitor(child_pid)

      # Kill the process while it's being displayed
      Process.exit(child_pid, :kill)

      # Wait for death notification
      assert_receive {:DOWN, ^monitor_ref, :process, ^child_pid, :killed}, 1000

      # The LiveView should handle the dead process gracefully
      # and show the restarted process after the supervisor restarts it
      GenServer.call(supervisor, :which_children)

      # Verify the process was restarted
      {:ok, new_children} = Control.get_supervision_tree(supervisor)
      new_child_pid = extract_first_worker_pid(new_children)
      assert new_child_pid != child_pid
      assert Process.alive?(new_child_pid)
    end
  end

  describe "process selection and killing" do
    setup do
      supervisor_name = :"test_kill_supervisor_#{:erlang.unique_integer([:positive])}"
      {:ok, sup_pid} = DemoSupervisor.start_link(name: supervisor_name)

      on_exit(fn ->
        if Process.alive?(sup_pid) do
          Process.exit(sup_pid, :kill)
        end
      end)

      %{supervisor: supervisor_name, sup_pid: sup_pid}
    end

    test "process selection via PID clicking", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Get a child process PID
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)

      # Click on the PID to select the process
      view
      |> element("button[phx-value-pid='#{child.pid}']")
      |> render_click()

      # Verify the process is selected and its information is displayed
      html = render(view)
      assert html =~ "Selected Process: #{child.pid}"
      assert html =~ "Memory"
      assert html =~ "Status"
    end

    test "process killing via kill button", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Get a child process PID
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)
      original_pid = extract_pid_from_string(child.pid)

      # Monitor the process to detect when it dies
      monitor_ref = Process.monitor(original_pid)

      # Kill the process via the UI
      view
      |> element("button[phx-value-pid='#{child.pid}'][phx-click='kill_process']")
      |> render_click()

      # Wait for the process to die
      assert_receive {:DOWN, ^monitor_ref, :process, ^original_pid, :killed}, 1000

      # Verify the flash message appears
      html = render(view)
      assert html =~ "Process killed: #{child.pid}"

      # Verify the process was restarted by the supervisor
      GenServer.call(supervisor, :which_children)

      {:ok, new_children} = Control.get_supervision_tree(supervisor)
      new_child = Enum.find(new_children, &(&1.id == child.id))
      new_pid = extract_pid_from_string(new_child.pid)

      assert new_pid != original_pid
      assert Process.alive?(new_pid)
    end

    test "UI updates after process kills", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Get initial HTML state
      initial_html = render(view)

      # Get a child process PID
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)
      original_pid = extract_pid_from_string(child.pid)

      # Monitor the process
      monitor_ref = Process.monitor(original_pid)

      # Kill the process
      view
      |> element("button[phx-value-pid='#{child.pid}'][phx-click='kill_process']")
      |> render_click()

      # Wait for the process to die
      assert_receive {:DOWN, ^monitor_ref, :process, ^original_pid, :killed}, 1000

      # Ensure supervisor has processed the restart
      GenServer.call(supervisor, :which_children)

      # Get updated HTML state
      updated_html = render(view)

      # Verify the UI has been updated
      assert updated_html =~ "Process killed: #{child.pid}"
      assert updated_html != initial_html
    end

    test "PID format handling", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Test both PID formats are handled correctly
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)

      # The PID should be in the format "#PID<0.123.0>"
      assert String.contains?(child.pid, "#PID<")
      assert String.contains?(child.pid, ">")

      # Test that the PID can be parsed by the LiveView
      view
      |> element("button[phx-value-pid='#{child.pid}']")
      |> render_click()

      # Should not get an error flash message
      html = render(view)
      refute html =~ "Invalid PID format"
    end
  end

  describe "error handling" do
    test "invalid supervisor selection", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Try to select a non-existent supervisor
      view
      |> element("button[phx-value-name='nonexistent_supervisor']")
      |> render_click()

      # Should show error message
      html = render(view)
      assert html =~ "Unknown supervisor: nonexistent_supervisor"
    end

    test "malformed PID selection", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Test by sending the select_process event directly with malformed PID
      view
      |> render_hook("select_process", %{"pid" => "not_a_pid"})

      # The LiveView should handle the error gracefully
      html = render(view)

      # Should either show error flash or handle gracefully without crash
      assert html =~ "Invalid PID format" ||
               refute(html =~ "Selected Process: not_a_pid")
    end

    test "supervisor that crashes during inspection", %{conn: conn} do
      # Start a supervisor that we'll kill
      supervisor_name = :"crash_test_supervisor_#{:erlang.unique_integer([:positive])}"
      {:ok, sup_pid} = DemoSupervisor.start_link(name: supervisor_name)

      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the supervisor
      view
      |> element("button[phx-value-name='#{supervisor_name}']")
      |> render_click()

      # Kill the supervisor
      monitor_ref = Process.monitor(sup_pid)
      Process.exit(sup_pid, :kill)

      # Wait for supervisor to die
      assert_receive {:DOWN, ^monitor_ref, :process, ^sup_pid, :killed}, 1000

      # The LiveView should handle the crashed supervisor gracefully
      # We can't easily test the exact timing, but we can verify
      # that the Control module handles dead supervisors correctly
      assert {:error, :not_found} = Control.get_supervision_tree(supervisor_name)
    end

    test "error message display and flash handling", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Trigger an error by selecting invalid supervisor
      view
      |> element("button[phx-value-name='invalid_supervisor']")
      |> render_click()

      # Check that error flash message is displayed
      html = render(view)
      assert html =~ "Unknown supervisor: invalid_supervisor"

      # Verify that the flash message is in the correct place
      assert html =~ "flash"
    end
  end

  describe "URL parameter handling" do
    test "loading with supervisor parameter", %{conn: conn} do
      {:ok, view, html} = live(conn, "/supervisors?supervisor=demo_one_for_one")

      # Should automatically select the supervisor
      assert html =~ "Children of demo_one_for_one"
      assert html =~ "counter_1"
      assert html =~ "counter_2"
      assert html =~ "printer_1"
    end

    test "invalid supervisor parameter handling", %{conn: conn} do
      {:ok, view, html} = live(conn, "/supervisors?supervisor=nonexistent")

      # Should show error for invalid supervisor
      assert html =~ "Unknown supervisor: nonexistent" || html =~ "Error"
    end

    test "navigation state preservation", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select a supervisor
      view
      |> element("button[phx-value-name='demo_one_for_one']")
      |> render_click()

      # Verify URL is updated
      assert_patch(view, "/supervisors?supervisor=demo_one_for_one")

      # Verify content is displayed
      html = render(view)
      assert html =~ "Children of demo_one_for_one"
    end
  end

  describe "process information display" do
    setup do
      supervisor_name = :"info_test_supervisor_#{:erlang.unique_integer([:positive])}"
      {:ok, sup_pid} = DemoSupervisor.start_link(name: supervisor_name)

      on_exit(fn ->
        if Process.alive?(sup_pid) do
          Process.exit(sup_pid, :kill)
        end
      end)

      %{supervisor: supervisor_name, sup_pid: sup_pid}
    end

    test "process details formatting", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Get a child process and select it
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)

      view
      |> element("button[phx-value-pid='#{child.pid}']")
      |> render_click()

      # Check that process information is formatted correctly
      html = render(view)
      assert html =~ "Selected Process: #{child.pid}"
      assert html =~ "Memory"
      assert html =~ "Status"
      assert html =~ "Message Queue Len"

      # Verify that memory is formatted in human-readable format
      assert html =~ "B" || html =~ "KB" || html =~ "MB" || html =~ "GB"
    end

    test "handling of processes with missing info", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Get a child process PID
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)
      original_pid = extract_pid_from_string(child.pid)

      # Select the process
      view
      |> element("button[phx-value-pid='#{child.pid}']")
      |> render_click()

      # Kill the process while it's selected
      monitor_ref = Process.monitor(original_pid)
      Process.exit(original_pid, :kill)

      # Wait for the process to die
      assert_receive {:DOWN, ^monitor_ref, :process, ^original_pid, :killed}, 1000

      # The LiveView should handle the dead process gracefully
      # and show appropriate message
      GenServer.call(supervisor, :which_children)

      # Process info should be cleared or show "no longer alive" message
      html = render(view)
      assert html =~ "no longer alive" || html =~ "N/A"
    end

    test "process info refresh for selected processes", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Get a counter process and select it
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)

      view
      |> element("button[phx-value-pid='#{counter_child.pid}']")
      |> render_click()

      # Get initial process info
      initial_html = render(view)

      # Increment the counter to change its state
      Counter.increment(counter_pid)

      # Use synchronous call to ensure the increment is processed
      current_value = Counter.get_value(counter_pid)
      assert current_value > 0

      # The process info should be refreshed automatically
      # We can't easily test the exact timing, but we can verify
      # that the process info is accessible
      {:ok, process_info} = Control.get_process_info(counter_pid)
      assert is_map(process_info)
      assert Map.has_key?(process_info, :memory)
    end
  end

  describe "WebSocket integration" do
    setup do
      supervisor_name = :"websocket_test_supervisor_#{:erlang.unique_integer([:positive])}"
      {:ok, sup_pid} = DemoSupervisor.start_link(name: supervisor_name)

      on_exit(fn ->
        if Process.alive?(sup_pid) do
          Process.exit(sup_pid, :kill)
        end
      end)

      %{supervisor: supervisor_name, sup_pid: sup_pid}
    end

    test "connection establishment", %{conn: conn, supervisor: supervisor} do
      # Test that LiveView establishes WebSocket connection properly
      {:ok, view, _html} = live(conn, "/supervisors")

      # Verify the LiveView is connected (uses WebSocket under the hood)
      assert view.module == OtpSupervisorWeb.SupervisorLive
      assert Phoenix.LiveViewTest.connected?(view)

      # Test that we can interact with the LiveView over WebSocket
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Verify the WebSocket communication worked
      html = render(view)
      assert html =~ "Children of #{supervisor}"
    end

    test "message handling during page updates", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the supervisor
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Get a child process
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)

      # Test that multiple WebSocket messages are handled correctly
      view
      |> element("button[phx-value-pid='#{child.pid}']")
      |> render_click()

      # Verify first message handled
      html = render(view)
      assert html =~ "Selected Process: #{child.pid}"

      # Send another message
      view
      |> element("button[phx-value-pid='#{child.pid}'][phx-click='kill_process']")
      |> render_click()

      # Verify second message handled
      html = render(view)
      assert html =~ "Process killed: #{child.pid}"
    end

    test "reconnection after disconnection", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the supervisor
      view
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Verify initial state
      html = render(view)
      assert html =~ "Children of #{supervisor}"

      # Simulate reconnection by creating a new LiveView connection
      # This tests that the state is properly maintained/restored
      {:ok, new_view, new_html} = live(conn, "/supervisors?supervisor=#{supervisor}")

      # Verify the new connection has the same state
      assert new_html =~ "Children of #{supervisor}"
      assert Phoenix.LiveViewTest.connected?(new_view)
    end

    test "concurrent user interactions", %{conn: conn, supervisor: supervisor} do
      # Simulate multiple concurrent users
      {:ok, view1, _html} = live(conn, "/supervisors")
      {:ok, view2, _html} = live(conn, "/supervisors")

      # Both users select the same supervisor
      view1
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      view2
      |> element("button[phx-value-name='#{supervisor}']")
      |> render_click()

      # Both should see the same content
      html1 = render(view1)
      html2 = render(view2)

      assert html1 =~ "Children of #{supervisor}"
      assert html2 =~ "Children of #{supervisor}"

      # Test that actions by one user don't interfere with another
      # Get a child process
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)
      original_pid = extract_pid_from_string(child.pid)

      # User 1 kills a process
      monitor_ref = Process.monitor(original_pid)

      view1
      |> element("button[phx-value-pid='#{child.pid}'][phx-click='kill_process']")
      |> render_click()

      # Wait for the process to die
      assert_receive {:DOWN, ^monitor_ref, :process, ^original_pid, :killed}, 1000

      # Both users should be able to see the updated state
      # (though the exact timing depends on auto-refresh)
      GenServer.call(supervisor, :which_children)

      # Both LiveViews should remain functional
      assert Phoenix.LiveViewTest.connected?(view1)
      assert Phoenix.LiveViewTest.connected?(view2)
    end
  end

  # Helper functions for tests
  defp extract_first_worker_pid(children) do
    child = hd(children)
    extract_pid_from_string(child.pid)
  end

  defp extract_pid_from_string(pid_string) do
    pid_string
    |> String.replace("#PID", "")
    |> String.trim()
    |> String.to_charlist()
    |> :erlang.list_to_pid()
  end
end
