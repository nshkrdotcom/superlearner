#!/usr/bin/env elixir

# Debug Arsenal panel layout in detail

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule DebugArsenalPanel do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Debugging Arsenal panel layout...")
    
    # Check if execution panel is visible
    check_execution_panel()
  end

  defp check_execution_panel do
    Logger.info("Checking execution panel visibility...")
    
    opts = %{
      headless: false,  # Show browser for debugging
      timeout: 30_000
    }
    
    case Playwriter.with_browser(opts, fn page ->
      # Navigate to Arsenal page
      Playwright.Page.goto(page, @arsenal_url)
      
      # Wait for page to load
      Process.sleep(2000)
      
      # Check initial state
      Logger.info("Initial state - checking panels...")
      
      # Check what panels are visible
      panels_script = """
      const panels = document.querySelectorAll('[data-component]');
      console.log('Found panels:', panels.length);
      panels.forEach((panel, index) => {
        console.log(`Panel ${index}:`, {
          component: panel.getAttribute('data-component'),
          id: panel.id,
          classes: panel.className,
          visible: !panel.classList.contains('hidden'),
          display: window.getComputedStyle(panel).display
        });
      });
      
      // Check for execution panel specifically
      const executionPanel = document.querySelector('[data-component="execution-panel"]');
      if (executionPanel) {
        console.log('Execution panel found:', {
          visible: !executionPanel.classList.contains('hidden'),
          display: window.getComputedStyle(executionPanel).display,
          width: executionPanel.offsetWidth,
          height: executionPanel.offsetHeight
        });
      } else {
        console.log('No execution panel found');
      }
      """
      
      Playwright.Page.evaluate(page, panels_script)
      
      # Click an operation to trigger execution panel
      Logger.info("Clicking operation to trigger execution panel...")
      first_operation = Playwright.Page.locator(page, "[phx-click='select_operation']")
      
      if Playwright.Locator.count(first_operation) > 0 do
        Playwright.Locator.click(first_operation |> Playwright.Locator.first())
        Process.sleep(2000)
        
        # Check panels after click
        Logger.info("After click - checking panels...")
        Playwright.Page.evaluate(page, panels_script)
        
        # Take screenshot
        screenshot_path = "/home/home/p/g/n/superlearner/screenshots/arsenal_panel_debug.png"
        Playwright.Page.screenshot(page, %{path: screenshot_path})
        Logger.info("Screenshot saved to #{screenshot_path}")
      end
      
      # Wait for observation
      Process.sleep(10000)
      
    end) do
      {:ok, _} ->
        Logger.info("✅ Arsenal panel debugging complete")
        
      {:error, reason} ->
        Logger.error("❌ Failed to debug Arsenal panels: #{inspect(reason)}")
    end
  end
end

# Run the debug
DebugArsenalPanel.run()