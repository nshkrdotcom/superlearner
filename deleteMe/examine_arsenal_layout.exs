#!/usr/bin/env elixir

# Examine Arsenal layout issues

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule ExamineArsenalLayout do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Examining Arsenal layout issues...")
    
    # Take screenshot to see current state
    examine_current_layout()
  end

  defp examine_current_layout do
    Logger.info("Taking screenshot of current Arsenal layout...")
    
    opts = %{
      headless: true,
      timeout: 30_000
    }
    
    case Playwriter.with_browser(opts, fn page ->
      # Navigate to Arsenal page
      Playwright.Page.goto(page, @arsenal_url)
      
      # Wait for page to load
      Process.sleep(2000)
      
      # Take full page screenshot
      screenshot_path = "/home/home/p/g/n/superlearner/screenshots/arsenal_layout_issues.png"
      Playwright.Page.screenshot(page, %{path: screenshot_path, full_page: true})
      Logger.info("Screenshot saved to #{screenshot_path}")
      
      # Also click an operation to see the three-panel layout
      first_operation = Playwright.Page.locator(page, "[phx-click='select_operation']")
      
      if Playwright.Locator.count(first_operation) > 0 do
        Playwright.Locator.click(first_operation |> Playwright.Locator.first())
        Process.sleep(2000)
        
        # Take screenshot of three-panel layout
        three_panel_path = "/home/home/p/g/n/superlearner/screenshots/arsenal_three_panel_layout.png"
        Playwright.Page.screenshot(page, %{path: three_panel_path, full_page: true})
        Logger.info("Three-panel layout screenshot saved to #{three_panel_path}")
      end
      
    end) do
      {:ok, _} ->
        Logger.info("✅ Arsenal layout examination complete")
        
      {:error, reason} ->
        Logger.error("❌ Failed to examine Arsenal layout: #{inspect(reason)}")
    end
  end
end

# Run the examination
ExamineArsenalLayout.run()