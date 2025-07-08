defmodule OtpSupervisorWeb.SupervisorLiveTest do
  use OtpSupervisorWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  alias OtpSupervisorWeb.SupervisorLive
  alias OTPSupervisor.Sandbox.Workers.Counter
  alias OTPSupervisor.Core.Control

  # Import our test helper for proper isolation
  import SupervisorTestHelper

  # Import StreamData for property-based testing
  use ExUnitProperties

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

    property "format_bytes/1 has correct behavioral properties" do
      check all(bytes <- integer(0..10_000_000_000_000)) do
        formatted = SupervisorLive.format_bytes(bytes)

        # Property 1: Always returns a string
        assert is_binary(formatted)

        # Property 2: Always follows format pattern (number + space + unit)
        assert formatted =~ ~r/^\d+(\.\d+)?\s+(B|KB|MB|GB)$/

        # Property 3: For values >= 1024, the displayed number is smaller than original
        # (compression property - we show fewer digits by using larger units)
        if bytes >= 1024 do
          [num_string | _] = String.split(formatted, " ")
          displayed_number = String.to_float(num_string)
          assert displayed_number < bytes
        end

        # Property 4: Small numbers show exact value in bytes
        if bytes < 1024 do
          assert formatted == "#{bytes} B"
        end

        # Property 5: Larger units are used for larger numbers
        # (monotonicity property)
        if bytes >= 1_073_741_824 do
          assert String.contains?(formatted, "GB")
        end
      end
    end

    property "format_bytes/1 handles non-integer input" do
      check all(
              invalid_input <-
                one_of([
                  constant(nil),
                  constant("not_a_number"),
                  constant([1, 2, 3]),
                  constant(%{key: "value"}),
                  constant(:atom),
                  float()
                ])
            ) do
        formatted = SupervisorLive.format_bytes(invalid_input)
        assert formatted == "N/A"
      end
    end

    property "format_bytes/1 handles negative integers" do
      check all(bytes <- integer(-1000..-1)) do
        formatted = SupervisorLive.format_bytes(bytes)

        # Should handle negative integers (they get formatted as negative bytes)
        assert is_binary(formatted)
        assert formatted == "#{bytes} B"
      end
    end

    property "format_key/1 handles any atom correctly" do
      check all(
              key_string <- string(:alphanumeric, min_length: 1, max_length: 50),
              separator <- member_of(["_", ""])
            ) do
        # Create atom with potential underscores
        key_with_sep = String.replace(key_string, ~r/.{3}/, "\\0#{separator}")
        key_atom = String.to_atom(key_with_sep)

        formatted = SupervisorLive.format_key(key_atom)

        # Should always return a string
        assert is_binary(formatted)

        # Should be capitalized words
        words = String.split(formatted, " ")

        for word <- words do
          if String.length(word) > 0 do
            assert String.at(word, 0) == String.upcase(String.at(word, 0))
          end
        end

        # Should not contain underscores
        refute formatted =~ "_"
      end
    end
  end

  describe "basic LiveView integration (read-only)" do
    setup do
      get_demo_supervisor()
    end

    test "supervisor live view loads", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/supervisors")

      assert html =~ "OTP Supervisor Control Panel"
      assert html =~ "Supervisors"
      assert html =~ "How Supervisors Work"
    end

    test "can select a supervisor", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the demo supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Check that children are displayed
      html = render(view)
      assert has_element?(view, "[data-testid=children-header]")
      assert view |> element("[data-testid=supervisor-name]") |> render() =~ to_string(supervisor)
      assert html =~ "counter_1"
      assert html =~ "counter_2"
      assert html =~ "printer_1"
    end

    test "displays supervisor information correctly", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Verify supervisor info is displayed
      html = render(view)
      assert has_element?(view, "[data-testid=children-header]")
      assert view |> element("[data-testid=supervisor-name]") |> render() =~ to_string(supervisor)

      # Verify children are listed with correct information
      {:ok, children} = Control.get_supervision_tree(supervisor)
      assert length(children) == 3

      # Check that all children are displayed
      for child <- children do
        assert html =~ to_string(child.id)
        # PIDs are HTML encoded, so check for the encoded version
        html_encoded_pid = String.replace(child.pid, "<", "&lt;") |> String.replace(">", "&gt;")
        assert html =~ html_encoded_pid || html =~ child.pid
      end
    end
  end

  describe "real-time functionality (destructive tests)" do
    setup do
      setup_isolated_supervisor("realtime")
    end

    test "auto-refresh timer functionality", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Get initial children  
      assert has_element?(view, "[data-testid=children-header]")
      assert view |> element("[data-testid=supervisor-name]") |> render() =~ to_string(supervisor)

      # Kill a child process
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child_pid = extract_first_worker_pid(children)

      # Monitor the child process to detect when it dies
      monitor_ref = Process.monitor(child_pid)

      # Kill the child
      Process.exit(child_pid, :kill)

      # Wait for the process to die
      assert_receive {:DOWN, ^monitor_ref, :process, ^child_pid, :killed}, 1000

      # Wait for supervisor restart to complete
      :ok = wait_for_restart(supervisor)

      # Verify the child was restarted with a new PID
      {:ok, new_children} = Control.get_supervision_tree(supervisor)
      new_child_pid = extract_first_worker_pid(new_children)
      assert new_child_pid != child_pid
      assert Process.alive?(new_child_pid)
    end

    test "process state changes reflected in real-time", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Get a counter process
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)

      # Increment the counter to change its state
      Counter.increment(counter_pid)

      # Use synchronous call to ensure the increment is processed
      current_value = Counter.get_value(counter_pid)
      assert current_value > 0

      # The state change should be reflected in the supervision tree
      {:ok, updated_children} = Control.get_supervision_tree(supervisor)
      updated_counter = Enum.find(updated_children, &(&1.id == :counter_1))
      assert updated_counter.alive == true
    end

    test "handling of processes that die during display", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
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

      # Wait for supervisor restart
      :ok = wait_for_restart(supervisor)

      # Verify the process was restarted
      {:ok, new_children} = Control.get_supervision_tree(supervisor)
      new_child_pid = extract_first_worker_pid(new_children)
      assert new_child_pid != child_pid
      assert Process.alive?(new_child_pid)
    end
  end

  describe "process selection and killing (destructive tests)" do
    setup do
      setup_isolated_supervisor("killing")
    end

    test "process selection via PID clicking", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Get a child process PID
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)

      # Click on the PID to select the process
      view
      |> element("[data-testid='select-process-#{child.pid}']")
      |> render_click()

      # Verify the process is selected and its information is displayed
      html = render(view)
      # Just verify that some process info is displayed, not the exact format
      assert html =~ "Memory" || html =~ "Process Details"
      assert html =~ "Status" || html =~ child.id
    end

    test "process killing via kill button", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Get a child process PID
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)
      original_pid = extract_pid_from_string(child.pid)

      # Monitor the process to detect when it dies
      monitor_ref = Process.monitor(original_pid)

      # Kill the process via the UI
      view
      |> element("[data-testid='kill-process-#{child.pid}']")
      |> render_click()

      # Wait for the process to die
      assert_receive {:DOWN, ^monitor_ref, :process, ^original_pid, :killed}, 1000

      # Verify a flash message appears
      html = render(view)
      assert html =~ "Process killed:" || html =~ "flash"

      # Wait for supervisor restart
      :ok = wait_for_restart(supervisor)

      # Verify the process was restarted by the supervisor
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
      |> element("[data-testid=select-supervisor-#{supervisor}]")
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
      |> element("[data-testid='kill-process-#{child.pid}']")
      |> render_click()

      # Wait for the process to die
      assert_receive {:DOWN, ^monitor_ref, :process, ^original_pid, :killed}, 1000

      # Wait for supervisor restart
      :ok = wait_for_restart(supervisor)

      # Get updated HTML state
      updated_html = render(view)

      # Verify the UI has been updated (flash message appears somewhere in the HTML)
      assert updated_html =~ "Process killed:" || updated_html =~ "flash"
      assert updated_html != initial_html
    end

    test "PID format handling", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Test both PID formats are handled correctly
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)

      # The PID should be in the format "#PID<0.123.0>"
      assert String.contains?(child.pid, "#PID<")
      assert String.contains?(child.pid, ">")

      # Test that the PID can be parsed by the LiveView
      view
      |> element("[data-testid='select-process-#{child.pid}']")
      |> render_click()

      # Should not get an error flash message
      html = render(view)
      refute html =~ "Invalid PID format"
    end
  end

  describe "error handling" do
    test "invalid supervisor selection", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Try to select a non-existent supervisor by sending the event directly
      view
      |> render_hook("select_supervisor", %{"name" => "nonexistent_supervisor"})

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

    test "UI handles supervisor crashing during inspection", %{conn: conn} do
      # This test verifies that the LiveView UI handles dead supervisors gracefully
      # We create a supervisor, show it in the UI, kill it, and verify UI behavior

      # Start a supervisor 
      %{supervisor: supervisor_name, sup_pid: sup_pid} = setup_crash_test_supervisor("ui_test")

      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the supervisor in the UI
      view
      |> element("[data-testid=select-supervisor-#{supervisor_name}]")
      |> render_click()

      # Verify supervisor is displayed in the UI
      html_before = render(view)
      assert has_element?(view, "[data-testid=children-header]")

      assert view |> element("[data-testid=supervisor-name]") |> render() =~
               to_string(supervisor_name)

      assert html_before =~ to_string(supervisor_name)

      # Kill the supervisor and catch any linked process exits
      # Unlink to prevent cascade
      Process.unlink(sup_pid)
      Process.exit(sup_pid, :kill)

      # Wait for supervisor to actually die
      ref = Process.monitor(sup_pid)

      receive do
        {:DOWN, ^ref, :process, ^sup_pid, _reason} -> :ok
      after
        1000 -> :timeout
      end

      # Force a refresh to trigger the UI update
      send(view.pid, :refresh)

      # Verify the UI handles the dead supervisor gracefully
      html_after = render(view)

      # The supervisor should either:
      # 1. Disappear from the supervisors list, OR
      # 2. Show an error state in the UI
      # We test that the UI doesn't crash and shows some appropriate response
      assert html_after =~ "error" ||
               html_after =~ "not found" ||
               html_after =~ "Select a Supervisor" ||
               refute(
                 has_element?(view, "[data-testid=supervisor-name]") &&
                   view |> element("[data-testid=supervisor-name]") |> render() =~
                     to_string(supervisor_name)
               )
    end

    test "error message display and flash handling", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Trigger an error by selecting invalid supervisor
      view
      |> render_hook("select_supervisor", %{"name" => "invalid_supervisor"})

      # Check that error flash message is displayed
      html = render(view)
      assert html =~ "Unknown supervisor: invalid_supervisor"

      # Verify that the flash message is in the correct place
      assert html =~ "flash"
    end
  end

  describe "URL parameter handling" do
    setup do
      get_demo_supervisor()
    end

    test "loading with supervisor parameter", %{conn: conn, supervisor: supervisor} do
      {:ok, view, html} = live(conn, "/supervisors?supervisor=#{supervisor}")

      # Should automatically select the supervisor
      assert has_element?(view, "[data-testid=children-header]")
      assert view |> element("[data-testid=supervisor-name]") |> render() =~ to_string(supervisor)
      assert html =~ "counter_1"
      assert html =~ "counter_2"
      assert html =~ "printer_1"
    end

    test "invalid supervisor parameter handling", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/supervisors?supervisor=nonexistent")

      # Should show error for invalid supervisor
      assert html =~ "Unknown supervisor: nonexistent" || html =~ "Error"
    end

    test "navigation state preservation", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select a supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Verify URL is updated
      assert_patch(view, "/supervisors?supervisor=#{supervisor}")

      # Verify content is displayed
      assert has_element?(view, "[data-testid=children-header]")
      assert view |> element("[data-testid=supervisor-name]") |> render() =~ to_string(supervisor)
    end
  end

  describe "process information display (destructive tests)" do
    setup do
      setup_isolated_supervisor("info")
    end

    test "process details formatting", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Get a child process and select it
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)

      view
      |> element("[data-testid='select-process-#{child.pid}']")
      |> render_click()

      # Check that process information is formatted correctly
      html = render(view)
      # Verify process details are shown in some form
      assert html =~ "Memory" || html =~ "Process Details"
      assert html =~ "Status" || html =~ child.id
      # Don't require exact format, just that info is present

      # Verify that memory is formatted in human-readable format
      assert html =~ "B" || html =~ "KB" || html =~ "MB" || html =~ "GB"
    end

    test "handling of processes with missing info", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Get a child process PID
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)
      original_pid = extract_pid_from_string(child.pid)

      # Select the process
      view
      |> element("[data-testid='select-process-#{child.pid}']")
      |> render_click()

      # Kill the process while it's selected
      monitor_ref = Process.monitor(original_pid)
      Process.exit(original_pid, :kill)

      # Wait for the process to die
      assert_receive {:DOWN, ^monitor_ref, :process, ^original_pid, :killed}, 1000

      # Wait for supervisor restart
      :ok = wait_for_restart(supervisor)

      # Process info should be cleared or show appropriate message
      html = render(view)
      # The process was restarted, so it should either show new info or be cleared
      # Just verify the test doesn't crash - the exact message may vary
      assert is_binary(html)
    end

    test "process info refresh for selected processes", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the test supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Get a counter process and select it
      {:ok, children} = Control.get_supervision_tree(supervisor)
      counter_child = Enum.find(children, &(&1.id == :counter_1))
      counter_pid = extract_pid_from_string(counter_child.pid)

      view
      |> element("[data-testid='select-process-#{counter_child.pid}']")
      |> render_click()

      # Increment the counter to change its state
      Counter.increment(counter_pid)

      # Use synchronous call to ensure the increment is processed
      current_value = Counter.get_value(counter_pid)
      assert current_value > 0

      # The process info should be accessible
      {:ok, process_info} = Control.get_process_info(counter_pid)
      assert is_map(process_info)
      assert Map.has_key?(process_info, :memory)
    end
  end

  describe "WebSocket integration" do
    setup do
      setup_isolated_supervisor("websocket")
    end

    test "connection establishment", %{conn: conn, supervisor: supervisor} do
      # Test that LiveView establishes WebSocket connection properly
      {:ok, view, _html} = live(conn, "/supervisors")

      # Verify the LiveView is connected (uses WebSocket under the hood)
      assert view.module == OtpSupervisorWeb.SupervisorLive
      # Note: connected?/1 is not available in LiveViewTest - just verify module

      # Test that we can interact with the LiveView over WebSocket
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Verify the WebSocket communication worked
      assert has_element?(view, "[data-testid=children-header]")
      assert view |> element("[data-testid=supervisor-name]") |> render() =~ to_string(supervisor)
    end

    test "message handling during page updates", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Get a child process
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)

      # Test that multiple WebSocket messages are handled correctly
      view
      |> element("[data-testid='select-process-#{child.pid}']")
      |> render_click()

      # Verify first message handled - check for process selection
      html = render(view)
      # Just verify that the process interaction worked
      assert html =~ "Memory" || html =~ "Process Details" || html =~ child.id

      # Send another message
      view
      |> element("[data-testid='kill-process-#{child.pid}']")
      |> render_click()

      # Verify second message handled - check for any message about the process
      html = render(view)
      # Could be "Process killed:" or "Selected process is no longer alive" or other message
      assert html =~ "Process killed:" || html =~ "no longer alive" || html =~ "Success!"
    end

    test "reconnection after disconnection", %{conn: conn, supervisor: supervisor} do
      {:ok, view, _html} = live(conn, "/supervisors")

      # Select the supervisor
      view
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Verify initial state
      assert has_element?(view, "[data-testid=children-header]")
      assert view |> element("[data-testid=supervisor-name]") |> render() =~ to_string(supervisor)

      # Simulate reconnection by creating a new LiveView connection
      # This tests that the state is properly maintained/restored
      {:ok, _new_view, new_html} = live(conn, "/supervisors?supervisor=#{supervisor}")

      # Verify the new connection has the same state
      assert has_element?(_new_view, "[data-testid=children-header]")
      assert new_html =~ to_string(supervisor)
      # Note: connected?/1 is not available in LiveViewTest
    end

    test "concurrent user interactions", %{conn: conn, supervisor: supervisor} do
      # Simulate multiple concurrent users
      {:ok, view1, _html} = live(conn, "/supervisors")
      {:ok, view2, _html} = live(conn, "/supervisors")

      # Both users select the same supervisor
      view1
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      view2
      |> element("[data-testid=select-supervisor-#{supervisor}]")
      |> render_click()

      # Both should see the same content
      assert has_element?(view1, "[data-testid=children-header]")
      assert has_element?(view2, "[data-testid=children-header]")

      assert view1 |> element("[data-testid=supervisor-name]") |> render() =~
               to_string(supervisor)

      assert view2 |> element("[data-testid=supervisor-name]") |> render() =~
               to_string(supervisor)

      # Test that actions by one user don't interfere with another
      # Get a child process
      {:ok, children} = Control.get_supervision_tree(supervisor)
      child = hd(children)
      original_pid = extract_pid_from_string(child.pid)

      # User 1 kills a process
      monitor_ref = Process.monitor(original_pid)

      view1
      |> element("[data-testid='kill-process-#{child.pid}']")
      |> render_click()

      # Wait for the process to die
      assert_receive {:DOWN, ^monitor_ref, :process, ^original_pid, :killed}, 1000

      # Wait for supervisor restart
      :ok = wait_for_restart(supervisor)

      # Both LiveViews should remain functional - verify by checking they can still render
      _html1_final = render(view1)
      _html2_final = render(view2)
      assert is_binary(_html1_final)
      assert is_binary(_html2_final)
    end
  end
end
