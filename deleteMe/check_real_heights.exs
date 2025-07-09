#!/usr/bin/env elixir

# Check ACTUAL rendered heights using Playwright DevTools inspection

Mix.install([
  {:playwright, "~> 1.45.0-alpha.1"}
])

defmodule RealHeightChecker do
  @moduledoc """
  Use Playwright to check actual rendered header heights from DevTools
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
            'div[class*="w-full"][class*="flex"][class*="items-center"][class*="justify-between"][class*="px-4"][class*="border-b"]',
            '.terminal-status-bar',
            '[class*="status-bar"]'
          ];
          
          let headerElement = null;
          let usedSelector = null;
          
          for (const selector of selectors) {
            const elements = document.querySelectorAll(selector);
            if (elements.length > 0) {
              // Take the first one that looks like a header (has certain classes)
              for (const el of elements) {
                const classes = el.className.toLowerCase();
                if (classes.includes('flex') && classes.includes('items-center') && 
                    (classes.includes('border-b') || classes.includes('status'))) {
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
              selectors_tried: selectors,
              all_elements: document.querySelectorAll('div').length
            };
          }
          
          // Get all the measurements DevTools would show
          const computedStyle = window.getComputedStyle(headerElement);
          const rect = headerElement.getBoundingClientRect();
          
          return {
            // Actual rendered dimensions
            actual_height: rect.height,
            actual_width: rect.width,
            bounding_rect_height: rect.height,
            bounding_rect_width: rect.width,
            
            // Element properties
            offset_height: headerElement.offsetHeight,
            offset_width: headerElement.offsetWidth,
            client_height: headerElement.clientHeight,
            client_width: headerElement.clientWidth,
            scroll_height: headerElement.scrollHeight,
            scroll_width: headerElement.scrollWidth,
            
            // Computed styles (what DevTools shows)
            computed_height: computedStyle.height,
            computed_width: computedStyle.width,
            computed_min_height: computedStyle.minHeight,
            computed_max_height: computedStyle.maxHeight,
            computed_padding_top: computedStyle.paddingTop,
            computed_padding_bottom: computedStyle.paddingBottom,
            computed_border_top: computedStyle.borderTopWidth,
            computed_border_bottom: computedStyle.borderBottomWidth,
            computed_margin_top: computedStyle.marginTop,
            computed_margin_bottom: computedStyle.marginBottom,
            
            // Element info
            css_classes: headerElement.className,
            tag_name: headerElement.tagName,
            selector_used: usedSelector,
            
            // Position info
            top: rect.top,
            left: rect.left,
            right: rect.right,
            bottom: rect.bottom
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
            css_classes: result["css_classes"],
            full_measurements: result
          }}
          
        _ ->
          {:error, "Unexpected measurement result: #{inspect(measurements)}"}
      end
    rescue
      error ->
        {:error, "Exception during measurement: #{inspect(error)}"}
    end
  end
end

# Run the real height check
RealHeightChecker.check_real_heights()