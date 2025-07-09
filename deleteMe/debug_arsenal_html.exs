#!/usr/bin/env elixir

# Debug Arsenal page HTML structure

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule DebugArsenalHTML do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Debugging Arsenal page HTML structure...")
    
    # Debug the page HTML
    debug_html()
  end

  defp debug_html do
    Logger.info("Analyzing Arsenal page HTML...")
    
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
        
        # Get the body HTML structure
        body_html = Playwright.Page.locator(page, "body")
        body_content = Playwright.Locator.inner_html(body_html |> Playwright.Locator.first())
        
        # Look for layout-related elements
        layout_section = body_content
          |> String.split("\n")
          |> Enum.filter(fn line -> 
            String.contains?(line, "layout") or 
            String.contains?(line, "grid") or 
            String.contains?(line, "panel") or
            String.contains?(line, "Arsenal")
          end)
          |> Enum.take(20)
          |> Enum.join("\n")
        
        Logger.info("Layout-related HTML:\n#{layout_section}")
        
        # Check for grid classes specifically
        grid_elements = Playwright.Page.locator(page, "[class*='grid']")
        grid_count = Playwright.Locator.count(grid_elements)
        Logger.info("Found #{grid_count} elements with 'grid' in class")
        
        # Check main content div
        main_content = Playwright.Page.locator(page, ".fixed.inset-0")
        if Playwright.Locator.count(main_content) > 0 do
          main_html = Playwright.Locator.inner_html(main_content |> Playwright.Locator.first())
          Logger.info("Main content HTML (first 2000 chars): #{String.slice(main_html, 0, 2000)}")
        end
        
      else
        Logger.warning("No operations found to click")
      end
    end) do
      {:ok, _} ->
        Logger.info("✅ Arsenal HTML debug complete")
        
      {:error, reason} ->
        Logger.error("❌ Failed to debug Arsenal HTML: #{inspect(reason)}")
    end
  end
end

# Run the debug
DebugArsenalHTML.run()