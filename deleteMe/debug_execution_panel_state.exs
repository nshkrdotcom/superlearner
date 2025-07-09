#!/usr/bin/env elixir

# Debug Arsenal page execution panel state

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule DebugExecutionPanelState do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Debugging Arsenal execution panel state...")
    
    # Debug the panel state
    debug_panel_state()
  end

  defp debug_panel_state do
    Logger.info("Analyzing execution panel state...")
    
    opts = %{
      headless: true,
      timeout: 30_000
    }
    
    case Playwriter.with_browser(opts, fn page ->
      # Navigate to Arsenal page
      Playwright.Page.goto(page, @arsenal_url)
      
      # Wait for page to load
      Process.sleep(2000)
      
      # Check initial state
      initial_panels = Playwright.Page.locator(page, ".bg-gray-900.border.rounded")
      initial_count = Playwright.Locator.count(initial_panels)
      Logger.info("Initial panel count: #{initial_count}")
      
      # Check if grid layout is applied initially
      initial_grid = Playwright.Page.locator(page, "[class*='grid']")
      initial_grid_count = Playwright.Locator.count(initial_grid)
      Logger.info("Initial grid elements: #{initial_grid_count}")
      
      # Check layout type in DOM
      main_layout = Playwright.Page.locator(page, ".flex.flex-col.h-full")
      if Playwright.Locator.count(main_layout) > 0 do
        layout_classes = Playwright.Locator.get_attribute(main_layout |> Playwright.Locator.first(), "class")
        Logger.info("Main layout classes: #{layout_classes}")
      end
      
      # Click on the first operation
      first_operation = Playwright.Page.locator(page, "[phx-click='select_operation']")
      
      if Playwright.Locator.count(first_operation) > 0 do
        Logger.info("Clicking first operation...")
        Playwright.Locator.click(first_operation |> Playwright.Locator.first())
        
        # Wait for state to update
        Process.sleep(1500)
        
        # Check updated state
        updated_panels = Playwright.Page.locator(page, ".bg-gray-900.border.rounded")
        updated_count = Playwright.Locator.count(updated_panels)
        Logger.info("Updated panel count: #{updated_count}")
        
        # Check if grid layout is applied after click
        updated_grid = Playwright.Page.locator(page, "[class*='grid']")
        updated_grid_count = Playwright.Locator.count(updated_grid)
        Logger.info("Updated grid elements: #{updated_grid_count}")
        
        # Check for grid-cols-3 specifically
        grid_cols_3 = Playwright.Page.locator(page, ".grid-cols-3")
        grid_cols_3_count = Playwright.Locator.count(grid_cols_3)
        Logger.info("Grid-cols-3 elements: #{grid_cols_3_count}")
        
        # Check layout container structure
        layout_container = Playwright.Page.locator(page, ".flex-1.overflow-hidden")
        if Playwright.Locator.count(layout_container) > 0 do
          container_classes = Playwright.Locator.get_attribute(layout_container |> Playwright.Locator.first(), "class")
          Logger.info("Layout container classes: #{container_classes}")
          
          # Get the direct child of the layout container
          layout_child = Playwright.Page.locator(page, ".flex-1.overflow-hidden > div")
          if Playwright.Locator.count(layout_child) > 0 do
            child_classes = Playwright.Locator.get_attribute(layout_child |> Playwright.Locator.first(), "class")
            Logger.info("Layout child classes: #{child_classes}")
          end
        end
        
        # Check for LogViewerWidget specifically
        log_viewer = Playwright.Page.locator(page, "*[id*='execution-logs']")
        log_viewer_count = Playwright.Locator.count(log_viewer)
        Logger.info("LogViewerWidget elements: #{log_viewer_count}")
        
      else
        Logger.warning("No operations found to click")
      end
    end) do
      {:ok, _} ->
        Logger.info("✅ Execution panel state debug complete")
        
      {:error, reason} ->
        Logger.error("❌ Failed to debug execution panel state: #{inspect(reason)}")
    end
  end
end

# Run the debug
DebugExecutionPanelState.run()