#!/usr/bin/env elixir

# Screenshot script for OTP Supervisor LiveView pages
# This script uses Playwriter to take screenshots of all four pages

Mix.install([
  {:playwriter, github: "nshkrdotcom/playwriter"}
])

defmodule ScreenshotPages do
  require Logger

  @base_url "http://localhost:4000"
  @pages [
    {"docs", "/docs", "Documentation page with API docs and guides"},
    {"system", "/system", "System dashboard with metrics and monitoring"},
    {"supervisors", "/supervisors", "Supervisor tree and process management"},
    {"arsenal", "/arsenal", "Arsenal command center with operations grid"}
  ]

  def run do
    Logger.info("Starting screenshot process for OTP Supervisor pages...")
    
    # Create screenshots directory if it doesn't exist
    File.mkdir_p!("screenshots")
    
    # Take screenshots of all pages
    results = Enum.map(@pages, fn {name, path, description} ->
      take_screenshot(name, path, description)
    end)
    
    # Report results
    report_results(results)
  end

  defp take_screenshot(name, path, description) do
    url = @base_url <> path
    screenshot_path = "screenshots/#{name}_page.png"
    
    Logger.info("Taking screenshot of #{description}")
    Logger.info("URL: #{url}")
    Logger.info("Output: #{screenshot_path}")
    
    opts = %{
      headless: true,
      timeout: 30_000
    }
    
    case Playwriter.screenshot(url, screenshot_path, opts) do
      {:ok, _} ->
        Logger.info("✅ Screenshot saved: #{screenshot_path}")
        {:ok, name, screenshot_path}
      
      {:error, reason} ->
        Logger.error("❌ Failed to screenshot #{name}: #{inspect(reason)}")
        {:error, name, reason}
    end
  end

  defp report_results(results) do
    Logger.info("\\n=== Screenshot Results ===")
    
    successful = Enum.filter(results, fn {status, _, _} -> status == :ok end)
    failed = Enum.filter(results, fn {status, _, _} -> status == :error end)
    
    Logger.info("✅ Successful: #{length(successful)}")
    Logger.info("❌ Failed: #{length(failed)}")
    
    if length(successful) > 0 do
      Logger.info("\\nSuccessful screenshots:")
      Enum.each(successful, fn {:ok, name, path} ->
        Logger.info("  - #{name}: #{path}")
      end)
    end
    
    if length(failed) > 0 do
      Logger.info("\\nFailed screenshots:")
      Enum.each(failed, fn {:error, name, reason} ->
        Logger.error("  - #{name}: #{inspect(reason)}")
      end)
    end
    
    Logger.info("\\nAll screenshots are saved in the 'screenshots' directory")
  end
end

# Run the screenshot process
ScreenshotPages.run()