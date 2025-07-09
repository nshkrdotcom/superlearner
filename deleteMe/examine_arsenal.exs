#!/usr/bin/env elixir

# Arsenal page examination script
# This script takes a screenshot and examines the Arsenal page layout

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule ExamineArsenal do
  require Logger

  @base_url "http://localhost:4000"
  @arsenal_url "#{@base_url}/arsenal"

  def run do
    Logger.info("Examining Arsenal page layout...")
    
    # Create screenshots directory if it doesn't exist
    File.mkdir_p!("screenshots")
    
    # Take a detailed screenshot of the Arsenal page
    take_detailed_screenshot()
    
    # Examine page elements
    examine_page_elements()
  end

  defp take_detailed_screenshot do
    screenshot_path = "screenshots/arsenal_detailed.png"
    
    Logger.info("Taking detailed screenshot of Arsenal page")
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
      
      # Take a full page screenshot
      Playwright.Page.screenshot(page, %{
        path: screenshot_path,
        full_page: true
      })
    end) do
      {:ok, _} ->
        Logger.info("✅ Detailed screenshot saved: #{screenshot_path}")
        
      {:error, reason} ->
        Logger.error("❌ Failed to take detailed screenshot: #{inspect(reason)}")
    end
  end

  defp examine_page_elements do
    Logger.info("Examining page elements...")
    
    opts = %{
      headless: true,
      timeout: 30_000
    }
    
    case Playwriter.with_browser(opts, fn page ->
      # Navigate to Arsenal page
      Playwright.Page.goto(page, @arsenal_url)
      
      # Wait for page to load
      Process.sleep(2000)
      
      # Get page title
      title = Playwright.Page.title(page)
      Logger.info("Page title: #{title}")
      
      # Check for key elements
      elements_to_check = [
        "h1", "h2", "h3",
        ".grid", ".flex", 
        "[class*='operation']", "[class*='widget']",
        ".bg-gray-900", ".text-green-400",
        "button", "nav"
      ]
      
      element_info = Enum.map(elements_to_check, fn selector ->
        try do
          elements = Playwright.Page.locator(page, selector)
          count = Playwright.Locator.count(elements)
          {selector, count}
        rescue
          _ -> {selector, 0}
        end
      end)
      
      Logger.info("Element counts:")
      Enum.each(element_info, fn {selector, count} ->
        Logger.info("  #{selector}: #{count}")
      end)
      
      # Get viewport size
      viewport = Playwright.Page.viewport_size(page)
      Logger.info("Viewport size: #{inspect(viewport)}")
      
      element_info
    end) do
      {:ok, result} ->
        Logger.info("✅ Page examination complete")
        result
        
      {:error, reason} ->
        Logger.error("❌ Failed to examine page: #{inspect(reason)}")
        []
    end
  end
end

# Run the examination
ExamineArsenal.run()