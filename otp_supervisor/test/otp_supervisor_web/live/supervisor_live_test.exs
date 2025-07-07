defmodule OtpSupervisorWeb.SupervisorLiveTest do
  use OtpSupervisorWeb.ConnCase

  import Phoenix.LiveViewTest
  alias OtpSupervisorWeb.SupervisorLive

  describe "helper functions" do
    test "format_bytes/1 formats bytes correctly" do
      # Test bytes
      assert SupervisorLive.format_bytes(512) == "512 B"
      assert SupervisorLive.format_bytes(1023) == "1023 B"
      
      # Test kilobytes
      assert SupervisorLive.format_bytes(1024) == "1.0 KB"
      assert SupervisorLive.format_bytes(1536) == "1.5 KB"
      assert SupervisorLive.format_bytes(1048575) == "1024.0 KB"
      
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
      assert SupervisorLive.format_value({MyModule, :my_function, 1}) == "Elixir.MyModule.my_function/1"
      
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
end