#!/usr/bin/env elixir

# Debug Arsenal page to understand what's being rendered

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule DebugArsenal do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Debugging Arsenal page...")
    
    # Create screenshots directory if it doesn't exist
    File.mkdir_p!("screenshots")
    
    # Debug the page
    debug_page_elements()
  end

  defp debug_page_elements do
    Logger.info("Analyzing Arsenal page elements...")
    
    opts = %{
      headless: true,
      timeout: 30_000
    }
    
    case Playwriter.with_browser(opts, fn page ->
      # Navigate to Arsenal page
      Playwright.Page.goto(page, @arsenal_url)
      
      # Wait for page to load
      Process.sleep(2000)
      
      # Get all buttons
      buttons = Playwright.Page.locator(page, "button")
      button_count = Playwright.Locator.count(buttons)
      Logger.info("Found #{button_count} buttons on the page")
      
      # Get all divs with grid class
      grids = Playwright.Page.locator(page, ".grid")
      grid_count = Playwright.Locator.count(grids)
      Logger.info("Found #{grid_count} grid elements")
      
      # Get all elements with phx-click
      phx_clicks = Playwright.Page.locator(page, "[phx-click]")
      phx_click_count = Playwright.Locator.count(phx_clicks)
      Logger.info("Found #{phx_click_count} elements with phx-click")
      
      # Print some button details
      if button_count > 0 do
        for i <- 0..min(5, button_count - 1) do
          button = Playwright.Locator.nth(buttons, i)
          text = Playwright.Locator.text_content(button)
          Logger.info("Button #{i}: #{inspect(text)}")
        end
      end
      
      # Get HTML content around the grid
      grid_html = if grid_count > 0 do
        first_grid = Playwright.Locator.first(grids)
        Playwright.Locator.inner_html(first_grid)
      else
        "No grid found"
      end
      
      Logger.info("Grid HTML (first 500 chars): #{String.slice(grid_html, 0, 500)}")
      
      # Check if page has finished loading
      page_state = Playwright.Page.evaluate(page, "document.readyState")
      Logger.info("Page state: #{page_state}")
      
      %{
        button_count: button_count,
        grid_count: grid_count,
        phx_click_count: phx_click_count,
        grid_html: grid_html
      }
    end) do
      {:ok, result} ->
        Logger.info("✅ Arsenal page debug complete")
        result
        
      {:error, reason} ->
        Logger.error("❌ Failed to debug Arsenal page: #{inspect(reason)}")
        %{}
    end
  end
end

# Run the debug
DebugArsenal.run()