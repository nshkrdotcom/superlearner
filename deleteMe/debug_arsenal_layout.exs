#!/usr/bin/env elixir

# Debug Arsenal page layout after clicking operation

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule DebugArsenalLayout do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Debugging Arsenal page layout after operation click...")
    
    # Debug the page layout
    debug_layout()
  end

  defp debug_layout do
    Logger.info("Analyzing Arsenal page layout...")
    
    opts = %{
      headless: true,
      timeout: 30_000
    }
    
    case Playwriter.with_browser(opts, fn page ->
      # Navigate to Arsenal page
      Playwright.Page.goto(page, @arsenal_url)
      
      # Wait for page to load
      Process.sleep(2000)
      
      # Click on the first operation
      first_operation = Playwright.Page.locator(page, "[phx-click='select_operation']")
      
      if Playwright.Locator.count(first_operation) > 0 do
        Logger.info("Clicking first operation...")
        Playwright.Locator.click(first_operation |> Playwright.Locator.first())
        
        # Wait for the layout to update
        Process.sleep(1000)
        
        # Check the layout container
        layout_container = Playwright.Page.locator(page, "[id='arsenal-panel-layout']")
        if Playwright.Locator.count(layout_container) > 0 do
          layout_html = Playwright.Locator.inner_html(layout_container |> Playwright.Locator.first())
          Logger.info("Layout container HTML (first 1000 chars): #{String.slice(layout_html, 0, 1000)}")
          
          # Check for grid classes
          grid_elements = Playwright.Page.locator(page, ".grid")
          grid_count = Playwright.Locator.count(grid_elements)
          Logger.info("Found #{grid_count} grid elements after click")
          
          # Check for specific grid classes
          grid_cols_3 = Playwright.Page.locator(page, ".grid-cols-3")
          grid_cols_3_count = Playwright.Locator.count(grid_cols_3)
          Logger.info("Found #{grid_cols_3_count} grid-cols-3 elements")
          
          # Check panel count
          panels = Playwright.Page.locator(page, ".bg-gray-900.border.rounded")
          panel_count = Playwright.Locator.count(panels)
          Logger.info("Found #{panel_count} panels")
          
          # Check for specific widgets
          execution_logs = Playwright.Page.locator(page, "*[id*='execution-logs']")
          execution_logs_count = Playwright.Locator.count(execution_logs)
          Logger.info("Found #{execution_logs_count} execution logs widgets")
          
        else
          Logger.warning("Layout container not found")
        end
      else
        Logger.warning("No operations found to click")
      end
    end) do
      {:ok, _} ->
        Logger.info("✅ Arsenal layout debug complete")
        
      {:error, reason} ->
        Logger.error("❌ Failed to debug Arsenal layout: #{inspect(reason)}")
    end
  end
end

# Run the debug
DebugArsenalLayout.run()