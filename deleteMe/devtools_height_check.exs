#!/usr/bin/env elixir

# Use the existing Playwright dependency in the project
Code.append_path("_build/dev/lib/playwright/ebin")

defmodule DevToolsHeightCheck do
  @moduledoc """
  Use the existing Playwright dependency to check actual rendered header heights
  """

  def check_real_heights do
    IO.puts("🔍 Checking ACTUAL rendered header heights using Playwright DevTools...")
    
    pages = [
      %{name: "Docs", url: "http://localhost:4000/docs"},
      %{name: "System Dashboard", url: "http://localhost:4000/system"},
      %{name: "Arsenal", url: "http://localhost:4000/arsenal"},
      %{name: "Supervisor", url: "http://localhost:4000/supervisors"}
    ]

    IO.puts("📊 DevTools Results:")
    IO.puts("=" <> String.duplicate("=", 90))

    # Try to use the existing Playwright setup
    try do
      # Start Playwright
      {:ok, playwright} = Playwright.start()
      {:ok, browser} = Playwright.launch(playwright, :chromium, %{headless: false})
      {:ok, page} = Playwright.Browser.new_page(browser)

      try do
        Enum.each(pages, fn page_info ->
          case measure_header_height(page, page_info) do
            {:ok, measurements} ->
              IO.puts("✅ #{page_info.name} (#{page_info.url})")
              IO.puts("   📏 Actual Rendered Height: #{measurements.actual_height}px")
              IO.puts("   📐 getBoundingClientRect(): #{measurements.bounding_rect_height}px")
              IO.puts("   📊 Computed Style height: #{measurements.computed_height}")
              IO.puts("   🎯 offsetHeight: #{measurements.offset_height}px")
              IO.puts("   📋 clientHeight: #{measurements.client_height}px")
              IO.puts("   🔧 CSS Classes: #{measurements.css_classes}")
              IO.puts("")
              
            {:error, reason} ->
              IO.puts("❌ #{page_info.name} (#{page_info.url})")
              IO.puts("   Error: #{reason}")
              IO.puts("")
          end
        end)
      after
        Playwright.Browser.close(browser)
        Playwright.stop(playwright)
      end
    rescue
      error ->
        IO.puts("❌ Failed to start Playwright: #{inspect(error)}")
        IO.puts("")
        IO.puts("🔄 Falling back to simple curl-based height detection...")
        fallback_height_check(pages)
    end
  end

  defp measure_header_height(page, page_info) do
    try do
      # Navigate to the page
      {:ok, _response} = Playwright.Page.goto(page, page_info.url)
      
      # Wait for the page to fully load
      :timer.sleep(3000)
      
      # Execute JavaScript to get actual DevTools measurements
      {:ok, measurements} = Playwright.Page.evaluate(page, """
        (() => {
          // Find the header element - try multiple selectors
          const selectors = [
            'div[class*="h-16"]',
            'div[class*="h-12"]',
            'div[class*="w-full"][class*="flex"][class*="items-center"][class*="justify-between"][class*="px-4"][class*="border-b"]'
          ];
          
          let headerElement = null;
          let usedSelector = null;
          
          for (const selector of selectors) {
            const elements = document.querySelectorAll(selector);
            if (elements.length > 0) {
              // Take the first one that looks like a header
              for (const el of elements) {
                const classes = el.className.toLowerCase();
                if (classes.includes('flex') && classes.includes('items-center') && 
                    classes.includes('border-b')) {
                  headerElement = el;
                  usedSelector = selector;
                  break;
                }
              }
              if (headerElement) break;
            }
          }
          
          if (!headerElement) {
            return {
              error: "Header element not found",
              selectors_tried: selectors
            };
          }
          
          // Get all the measurements DevTools would show
          const computedStyle = window.getComputedStyle(headerElement);
          const rect = headerElement.getBoundingClientRect();
          
          return {
            actual_height: rect.height,
            bounding_rect_height: rect.height,
            offset_height: headerElement.offsetHeight,
            client_height: headerElement.clientHeight,
            computed_height: computedStyle.height,
            css_classes: headerElement.className,
            selector_used: usedSelector
          };
        })()
      """)
      
      case measurements do
        %{"error" => error} ->
          {:error, "JavaScript error: #{error}"}
          
        %{"actual_height" => _} = result ->
          {:ok, %{
            actual_height: result["actual_height"],
            bounding_rect_height: result["bounding_rect_height"],
            computed_height: result["computed_height"],
            offset_height: result["offset_height"],
            client_height: result["client_height"],
            css_classes: result["css_classes"]
          }}
          
        _ ->
          {:error, "Unexpected measurement result: #{inspect(measurements)}"}
      end
    rescue
      error ->
        {:error, "Exception during measurement: #{inspect(error)}"}
    end
  end

  defp fallback_height_check(pages) do
    Enum.each(pages, fn page ->
      case System.cmd("curl", ["-s", page.url]) do
        {html, 0} ->
          cond do
            String.contains?(html, "h-16") ->
              IO.puts("✅ #{page.name} - Found h-16 class (should be 64px)")
              
            String.contains?(html, "h-12") ->
              IO.puts("❌ #{page.name} - Found h-12 class (should be 48px)")
              
            true ->
              IO.puts("❓ #{page.name} - No height class found")
          end
          
        {_output, _exit_code} ->
          IO.puts("❌ #{page.name} - Failed to fetch page")
      end
    end)
  end
end

# Run the check
DevToolsHeightCheck.check_real_heights()