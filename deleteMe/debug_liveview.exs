#!/usr/bin/env elixir

# Debug LiveView events and state

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule DebugLiveView do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Debugging LiveView events...")
    
    # Debug LiveView
    debug_liveview()
  end

  defp debug_liveview do
    Logger.info("Analyzing LiveView state...")
    
    opts = %{
      headless: false,  # Show browser
      timeout: 60_000
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
      
      # Debug LiveView state
      debug_script = """
      console.log("🔍 LiveView Debug Starting...");
      
      // Check LiveView socket
      if (window.liveSocket) {
        console.log("✅ LiveView socket exists");
        console.log("Socket connected:", window.liveSocket.isConnected());
        console.log("Socket state:", window.liveSocket.socket?.connectionState());
        
        // Hook into LiveView events
        window.liveSocket.socket?.onMessage((msg) => {
          console.log("📡 LiveView message:", msg);
        });
        
        // Check for any LiveView errors
        window.liveSocket.socket?.onError((error) => {
          console.log("❌ LiveView error:", error);
        });
        
      } else {
        console.log("❌ No LiveView socket found");
      }
      
      // Check for LiveView components
      const liveComponents = document.querySelectorAll('[data-phx-component]');
      console.log(`Found ${liveComponents.length} LiveView components`);
      
      liveComponents.forEach((component, index) => {
        console.log(`Component ${index}:`, {
          id: component.getAttribute('data-phx-id'),
          component: component.getAttribute('data-phx-component'),
          element: component.tagName
        });
      });
      
      // Check phx-click elements
      const phxClicks = document.querySelectorAll('[phx-click]');
      console.log(`Found ${phxClicks.length} phx-click elements`);
      
      phxClicks.forEach((el, index) => {
        const target = el.getAttribute('phx-target');
        console.log(`phx-click ${index}:`, {
          click: el.getAttribute('phx-click'),
          target: target,
          targetType: target ? 'component' : 'liveview',
          element: el.tagName,
          classes: el.className
        });
      });
      
      // Try to manually trigger a phx-click event
      console.log("🎯 Attempting manual event trigger...");
      
      const firstOp = document.querySelector('[phx-click="select_operation"]');
      if (firstOp) {
        console.log("Found first operation element:", firstOp);
        
        // Create and dispatch a custom event
        const customEvent = new Event('click', {
          bubbles: true,
          cancelable: true
        });
        
        console.log("Dispatching custom click event...");
        firstOp.dispatchEvent(customEvent);
        
        // Also try Phoenix.LiveView.JS.dispatch
        if (window.Phoenix && window.Phoenix.LiveView) {
          console.log("Trying Phoenix.LiveView.JS.dispatch...");
          // This would need to be the actual method if available
        }
      }
      
      console.log("🔍 LiveView debug complete");
      """
      
      Playwright.Page.evaluate(page, debug_script)
      
      # Wait and observe
      Logger.info("Observing LiveView for 30 seconds...")
      Process.sleep(30_000)
      
    end) do
      {:ok, _} ->
        Logger.info("✅ LiveView debugging complete")
        
      {:error, reason} ->
        Logger.error("❌ Failed to debug LiveView: #{inspect(reason)}")
    end
  end
end

# Run the debug
DebugLiveView.run()