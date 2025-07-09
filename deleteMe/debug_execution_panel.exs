#!/usr/bin/env elixir

# Debug execution panel rendering

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule DebugExecutionPanel do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Debugging execution panel rendering...")
    
    # Test execution panel rendering
    test_execution_panel_rendering()
  end

  defp test_execution_panel_rendering do
    Logger.info("Testing execution panel rendering...")
    
    opts = %{
      headless: false,
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
      
      # Check DOM structure before clicking
      initial_check = """
      console.log("=== INITIAL DOM CHECK ===");
      const panels = document.querySelectorAll('[data-phx-component]');
      console.log('LiveComponents found:', panels.length);
      
      panels.forEach((panel, index) => {
        console.log(`Panel ${index}:`, {
          id: panel.id,
          component: panel.getAttribute('data-phx-component'),
          classes: panel.className,
          display: window.getComputedStyle(panel).display,
          width: panel.offsetWidth,
          height: panel.offsetHeight
        });
      });
      
      // Check for execution panel specifically
      const executionPanel = document.querySelector('[id*="execution-panel"]');
      console.log('Execution panel element:', executionPanel);
      
      // Check panel layout structure
      const panelLayout = document.querySelector('[id*="arsenal-panel-layout"]');
      console.log('Panel layout element:', panelLayout);
      if (panelLayout) {
        console.log('Panel layout children:', panelLayout.children.length);
        Array.from(panelLayout.children).forEach((child, index) => {
          console.log(`Child ${index}:`, {
            tagName: child.tagName,
            id: child.id,
            classes: child.className,
            display: window.getComputedStyle(child).display
          });
        });
      }
      """
      
      Playwright.Page.evaluate(page, initial_check)
      
      # Click an operation
      Logger.info("Clicking operation...")
      first_operation = Playwright.Page.locator(page, "[phx-click='select_operation']")
      
      if Playwright.Locator.count(first_operation) > 0 do
        Playwright.Locator.click(first_operation |> Playwright.Locator.first())
        Process.sleep(3000)
        
        # Check DOM structure after clicking
        after_click_check = """
        console.log("=== AFTER CLICK DOM CHECK ===");
        const panels = document.querySelectorAll('[data-phx-component]');
        console.log('LiveComponents found:', panels.length);
        
        panels.forEach((panel, index) => {
          console.log(`Panel ${index}:`, {
            id: panel.id,
            component: panel.getAttribute('data-phx-component'),
            classes: panel.className,
            display: window.getComputedStyle(panel).display,
            width: panel.offsetWidth,
            height: panel.offsetHeight
          });
        });
        
        // Check for execution panel specifically
        const executionPanel = document.querySelector('[id*="execution-panel"]');
        console.log('Execution panel element after click:', executionPanel);
        
        // Check panel layout structure
        const panelLayout = document.querySelector('[id*="arsenal-panel-layout"]');
        console.log('Panel layout element after click:', panelLayout);
        if (panelLayout) {
          console.log('Panel layout children after click:', panelLayout.children.length);
          Array.from(panelLayout.children).forEach((child, index) => {
            console.log(`Child ${index}:`, {
              tagName: child.tagName,
              id: child.id,
              classes: child.className,
              display: window.getComputedStyle(child).display
            });
          });
        }
        """
        
        Playwright.Page.evaluate(page, after_click_check)
        
        # Take screenshot
        screenshot_path = "/home/home/p/g/n/superlearner/screenshots/execution_panel_debug.png"
        Playwright.Page.screenshot(page, %{path: screenshot_path})
        Logger.info("Screenshot saved to #{screenshot_path}")
      end
      
      # Wait for observation
      Process.sleep(15000)
      
    end) do
      {:ok, _} ->
        Logger.info("✅ Execution panel debugging complete")
        
      {:error, reason} ->
        Logger.error("❌ Failed to debug execution panel: #{inspect(reason)}")
    end
  end
end

# Run the debug
DebugExecutionPanel.run()