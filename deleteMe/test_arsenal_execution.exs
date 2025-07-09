#!/usr/bin/env elixir

# Test Arsenal page execution panel trigger
# This script clicks an operation to trigger the execution panel

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule TestArsenalExecution do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Testing Arsenal page execution panel...")
    
    # Create screenshots directory if it doesn't exist
    File.mkdir_p!("screenshots")
    
    # Test the execution panel trigger
    test_execution_panel()
  end

  defp test_execution_panel do
    screenshot_path = "screenshots/arsenal_execution_panel.png"
    
    Logger.info("Testing execution panel on Arsenal page")
    Logger.info("URL: #{@arsenal_url}")
    Logger.info("Output: #{screenshot_path}")
    
    opts = %{
      headless: true,
      timeout: 30_000
    }
    
    case Playwriter.with_browser(opts, fn page ->
      # Navigate to Arsenal page
      Playwright.Page.goto(page, @arsenal_url)
      
      # Wait for page to load
      Process.sleep(2000)
      
      # Click on the first operation button to trigger execution panel
      # The operations are rendered as div elements with phx-click
      first_operation = Playwright.Page.locator(page, "[phx-click='select_operation']")
      
      if Playwright.Locator.count(first_operation) > 0 do
        Logger.info("Clicking first operation to trigger execution panel...")
        Playwright.Locator.click(first_operation |> Playwright.Locator.first())
        
        # Wait for the execution panel to appear
        Process.sleep(1000)
        
        # Take screenshot with execution panel
        Playwright.Page.screenshot(page, %{
          path: screenshot_path,
          full_page: true
        })
        
        Logger.info("✅ Execution panel triggered and screenshot taken")
      else
        Logger.warning("No operation buttons found")
        
        # Take screenshot anyway to see current state
        Playwright.Page.screenshot(page, %{
          path: screenshot_path,
          full_page: true
        })
      end
    end) do
      {:ok, _} ->
        Logger.info("✅ Execution panel test complete: #{screenshot_path}")
        
      {:error, reason} ->
        Logger.error("❌ Failed to test execution panel: #{inspect(reason)}")
    end
  end
end

# Run the test
TestArsenalExecution.run()