#!/usr/bin/env elixir

# Debug WebSocket connection and LiveView socket health

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule DebugWebSocket do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Debugging WebSocket connection and LiveView socket health...")
    
    # Clear debug log
    File.rm("/tmp/arsenal_debug.log")
    
    # Debug WebSocket connection
    debug_websocket_connection()
  end

  defp debug_websocket_connection do
    Logger.info("Analyzing WebSocket connection...")
    
    opts = %{
      headless: false,  # Show browser for debugging
      timeout: 60_000   # Longer timeout for debugging
    }
    
    case Playwriter.with_browser(opts, fn page ->
      # Navigate to Arsenal page
      Playwright.Page.goto(page, @arsenal_url)
      
      # Wait for page to load
      Process.sleep(2000)
      
      # Enable console logging
      Playwright.Page.on(page, :console, fn msg ->
        Logger.info("CONSOLE: #{inspect(msg)}")
      end)
      
      # Comprehensive WebSocket debugging
      debug_script = """
      console.log("🔍 WebSocket and LiveView Socket Health Check");
      console.log("===============================================");
      
      // 1. Check for LiveView socket
      if (window.liveSocket) {
        console.log("✅ LiveView socket exists");
        console.log("Socket connected:", window.liveSocket.isConnected());
        
        // Get socket state
        const socket = window.liveSocket.socket;
        if (socket) {
          console.log("Socket state:", socket.connectionState());
          console.log("Socket transport:", socket.transport?.constructor.name);
          console.log("Socket endpoint:", socket.endPoint);
          
          // Check WebSocket connection
          if (socket.conn) {
            console.log("WebSocket readyState:", socket.conn.readyState);
            console.log("WebSocket URL:", socket.conn.url);
            console.log("WebSocket protocol:", socket.conn.protocol);
            
            // WebSocket states: 0=CONNECTING, 1=OPEN, 2=CLOSING, 3=CLOSED
            const states = ['CONNECTING', 'OPEN', 'CLOSING', 'CLOSED'];
            console.log("WebSocket state:", states[socket.conn.readyState]);
          }
        }
        
        // Monitor socket events
        const originalSend = window.liveSocket.socket.send;
        window.liveSocket.socket.send = function(data) {
          console.log("📤 Sending to server:", data);
          return originalSend.call(this, data);
        };
        
        // Monitor incoming messages
        const originalOnMessage = window.liveSocket.socket.onMessage;
        window.liveSocket.socket.onMessage = function(callback) {
          return originalOnMessage.call(this, function(msg) {
            console.log("📨 Received from server:", msg);
            return callback(msg);
          });
        };
        
        // Hook into channel events
        const channels = window.liveSocket.socket.channels;
        console.log("Active channels:", channels.length);
        
        channels.forEach((channel, index) => {
          console.log(`Channel ${index}:`, {
            topic: channel.topic,
            state: channel.state,
            joinedOnce: channel.joinedOnce
          });
          
          // Monitor channel events
          const originalPush = channel.push;
          channel.push = function(event, payload, timeout) {
            console.log(`📡 Channel ${index} push:`, { event, payload, timeout });
            return originalPush.call(this, event, payload, timeout);
          };
        });
        
      } else {
        console.log("❌ LiveView socket not found");
      }
      
      // 2. Check DOM elements
      const operations = document.querySelectorAll('[phx-click="select_operation"]');
      console.log(`Found ${operations.length} operation elements`);
      
      // 3. Test manual event dispatch
      console.log("🎯 Testing manual event dispatch...");
      
      if (operations.length > 0) {
        const firstOp = operations[0];
        console.log("Testing with first operation:", firstOp.getAttribute('phx-value-operation-id'));
        
        // Create a synthetic click event
        const clickEvent = new MouseEvent('click', {
          bubbles: true,
          cancelable: true,
          view: window
        });
        
        console.log("Dispatching click event...");
        firstOp.dispatchEvent(clickEvent);
        
        // Wait a moment and check if anything changed
        setTimeout(() => {
          console.log("Checking for server response...");
          
          // Check if any DOM changes occurred
          const executionPanel = document.querySelector('[data-component="execution-panel"]');
          if (executionPanel) {
            console.log("✅ Execution panel exists");
            console.log("Panel visible:", !executionPanel.classList.contains('hidden'));
          } else {
            console.log("❌ Execution panel not found");
          }
          
          // Check network activity
          console.log("Network activity check complete");
        }, 2000);
      }
      
      // 4. Test direct Phoenix event
      console.log("🔥 Testing direct Phoenix event...");
      
      // Look for LiveView main element
      const liveViewMain = document.querySelector('[data-phx-main]');
      if (liveViewMain) {
        console.log("✅ LiveView main element found");
        console.log("Main element ID:", liveViewMain.getAttribute('data-phx-main'));
        
        // Try to trigger event directly on LiveView
        const liveViewEvent = new CustomEvent('phx:select_operation', {
          detail: { operation_id: 'test_operation' },
          bubbles: true
        });
        
        console.log("Dispatching LiveView event...");
        liveViewMain.dispatchEvent(liveViewEvent);
      }
      
      console.log("🔍 WebSocket debug complete");
      """
      
      Playwright.Page.evaluate(page, debug_script)
      
      # Wait for events to process
      Logger.info("Waiting for WebSocket events to process...")
      Process.sleep(5000)
      
      # Check if any server events were logged
      if File.exists?("/tmp/arsenal_debug.log") do
        Logger.info("Checking server debug log...")
        debug_content = File.read!("/tmp/arsenal_debug.log")
        
        if String.contains?(debug_content, "select_operation") do
          Logger.info("✅ Server received operation selection events")
        else
          Logger.warning("❌ No operation selection events reached server")
        end
      else
        Logger.warning("❌ No server debug log found")
      end
      
      # Try a different approach - direct JavaScript to Phoenix bridge
      Logger.info("Attempting direct JavaScript-to-Phoenix bridge...")
      
      bridge_script = """
      console.log("🌉 Testing JavaScript-to-Phoenix bridge...");
      
      // Try to access Phoenix directly
      if (window.Phoenix) {
        console.log("✅ Phoenix available");
        
        // Try to get the LiveView from Phoenix
        const liveViewEl = document.querySelector('[data-phx-main]');
        if (liveViewEl) {
          const viewId = liveViewEl.getAttribute('data-phx-main');
          console.log("LiveView ID:", viewId);
          
          // Try to push event directly
          if (window.liveSocket && window.liveSocket.getViewByEl) {
            const view = window.liveSocket.getViewByEl(liveViewEl);
            if (view) {
              console.log("✅ LiveView instance found");
              console.log("View state:", view.rendered);
              
              // Try to push event
              view.pushEvent('select_operation', { operation_id: 'test_direct' });
              console.log("📤 Direct event pushed");
            }
          }
        }
      } else {
        console.log("❌ Phoenix not available");
      }
      """
      
      Playwright.Page.evaluate(page, bridge_script)
      
      # Final wait and check
      Process.sleep(3000)
      
      # Take screenshot of final state
      screenshot_path = "/home/home/p/g/n/superlearner/screenshots/websocket_debug.png"
      Playwright.Page.screenshot(page, %{path: screenshot_path})
      Logger.info("Screenshot saved to #{screenshot_path}")
      
    end) do
      {:ok, _} ->
        Logger.info("✅ WebSocket debugging complete")
        
      {:error, reason} ->
        Logger.error("❌ Failed to debug WebSocket: #{inspect(reason)}")
    end
  end
end

# Run the debug
DebugWebSocket.run()