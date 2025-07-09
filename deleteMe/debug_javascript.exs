#!/usr/bin/env elixir

# Debug JavaScript events on Arsenal page

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule DebugJavaScript do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Debugging JavaScript events on Arsenal page...")
    
    # Debug JavaScript events
    debug_javascript_events()
  end

  defp debug_javascript_events do
    Logger.info("Analyzing JavaScript events...")
    
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
      
      # Add JavaScript to debug click events
      debug_script = """
      console.log("🔍 Starting click event debugging...");
      
      // Find all operation elements
      const operations = document.querySelectorAll('[phx-click="select_operation"]');
      console.log(`Found ${operations.length} operation elements`);
      
      // Add event listeners to debug
      operations.forEach((op, index) => {
        console.log(`Operation ${index}:`, op);
        console.log(`  - phx-click: ${op.getAttribute('phx-click')}`);
        console.log(`  - phx-target: ${op.getAttribute('phx-target')}`);
        console.log(`  - phx-value-operation-id: ${op.getAttribute('phx-value-operation-id')}`);
        console.log(`  - classes: ${op.className}`);
        console.log(`  - computed style pointer-events: ${window.getComputedStyle(op).pointerEvents}`);
        console.log(`  - computed style z-index: ${window.getComputedStyle(op).zIndex}`);
        
        // Add click listener
        op.addEventListener('click', function(e) {
          console.log(`🎯 CLICK detected on operation ${index}!`);
          console.log('  - Event target:', e.target);
          console.log('  - Current target:', e.currentTarget);
          console.log('  - Operation ID:', this.getAttribute('phx-value-operation-id'));
          console.log('  - Event will bubble:', !e.defaultPrevented);
        }, true); // Use capture phase
      });
      
      // Check for LiveView socket
      if (window.liveSocket) {
        console.log("✅ LiveView socket found");
        console.log("LiveView socket state:", window.liveSocket.isConnected());
      } else {
        console.log("❌ LiveView socket not found");
      }
      
      // Check for any overlaying elements
      const overlay = document.elementFromPoint(400, 300);
      console.log("Element at center point (400, 300):", overlay);
      
      console.log("🔍 Debug setup complete. Click an operation to see events...");
      """
      
      Playwright.Page.evaluate(page, debug_script)
      
      # Wait for user interaction or timeout
      Logger.info("Browser opened for debugging. Click on operations to see console output...")
      Logger.info("Waiting 30 seconds for interaction...")
      Process.sleep(30_000)
      
      # Try to click programmatically after observation
      Logger.info("Attempting programmatic click...")
      first_operation = Playwright.Page.locator(page, "[phx-click='select_operation']")
      
      if Playwright.Locator.count(first_operation) > 0 do
        # Get coordinates and click
        box = Playwright.Locator.bounding_box(first_operation |> Playwright.Locator.first())
        Logger.info("Operation bounding box: #{inspect(box)}")
        
        # Click at the center of the element
        if box do
          x = box["x"] + box["width"] / 2
          y = box["y"] + box["height"] / 2
          Logger.info("Clicking at coordinates: #{x}, #{y}")
          Playwright.Page.click(page, "body", %{position: %{x: x, y: y}})
          
          # Wait to see results
          Process.sleep(2000)
        end
      else
        Logger.warning("No operation elements found")
      end
      
    end) do
      {:ok, _} ->
        Logger.info("✅ JavaScript debugging complete")
        
      {:error, reason} ->
        Logger.error("❌ Failed to debug JavaScript: #{inspect(reason)}")
    end
  end
end

# Run the debug
DebugJavaScript.run()