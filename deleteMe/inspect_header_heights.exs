#!/usr/bin/env elixir

# Script to inspect actual rendered header heights using Playwriter
# This will use browser automation to get the real DOM measurements

Code.require_file("../playwriter/playwriter/lib/playwriter.ex")
Code.require_file("../playwriter/playwriter/lib/playwriter/fetcher.ex")

defmodule HeaderInspector do
  @moduledoc """
  Inspect actual rendered header heights using Playwriter browser automation
  """

  def inspect_header_heights do
    IO.puts("🔍 Inspecting header heights using browser automation...")
    
    # Test pages and their expected header selectors
    pages = [
      %{
        name: "Docs",
        url: "http://localhost:4000/docs",
        expected_size: "small (h-12, 48px)"
      },
      %{
        name: "System Dashboard", 
        url: "http://localhost:4000/system",
        expected_size: "large (h-16, 64px)"
      },
      %{
        name: "Arsenal",
        url: "http://localhost:4000/arsenal", 
        expected_size: "large (h-16, 64px)"
      },
      %{
        name: "Supervisor",
        url: "http://localhost:4000/supervisors",
        expected_size: "small (h-12, 48px)"
      }
    ]

    # Browser options for automation
    browser_opts = %{
      headless: false,  # Show browser for debugging
      use_windows_browser: false  # Use local browser
    }

    IO.puts("📊 Results:")
    IO.puts("=" <> String.duplicate("=", 80))

    Enum.each(pages, fn page ->
      case inspect_page_header(page, browser_opts) do
        {:ok, result} ->
          IO.puts("✅ #{page.name} (#{page.url})")
          IO.puts("   Expected: #{page.expected_size}")
          IO.puts("   Actual:   #{result.actual_height}px")
          IO.puts("   CSS:      #{result.css_classes}")
          IO.puts("   Status:   #{result.status}")
          IO.puts("")
          
        {:error, reason} ->
          IO.puts("❌ #{page.name} (#{page.url})")
          IO.puts("   Error:    #{reason}")
          IO.puts("")
      end
    end)
  end

  defp inspect_page_header(page, browser_opts) do
    Playwriter.with_browser(browser_opts, fn playwright_page ->
      # Navigate to the page
      Playwright.Page.goto(playwright_page, page.url)
      
      # Wait for the page to load
      Process.sleep(2000)
      
      # Find the header element (status bar)
      header_selector = "[class*='TerminalStatusBar'] > div, .terminal-status-bar, [class*='status-bar'], [class*='h-12'], [class*='h-16']"
      
      # Get the actual rendered height using JavaScript
      height_result = Playwright.Page.evaluate(playwright_page, """
        (() => {
          // Try multiple selectors to find the header
          const selectors = [
            'div[class*="h-12"]',
            'div[class*="h-16"]', 
            'div[class*="status-bar"]',
            'div[class*="terminal-status-bar"]',
            'div[class*="w-full"][class*="flex"][class*="items-center"][class*="justify-between"][class*="px-4"][class*="border-b"]'
          ];
          
          let headerElement = null;
          let usedSelector = null;
          
          for (const selector of selectors) {
            headerElement = document.querySelector(selector);
            if (headerElement) {
              usedSelector = selector;
              break;
            }
          }
          
          if (!headerElement) {
            return {
              error: "Header element not found",
              selectors_tried: selectors
            };
          }
          
          const computedStyle = window.getComputedStyle(headerElement);
          const rect = headerElement.getBoundingClientRect();
          
          return {
            actual_height: rect.height,
            computed_height: computedStyle.height,
            css_classes: headerElement.className,
            selector_used: usedSelector,
            client_height: headerElement.clientHeight,
            offset_height: headerElement.offsetHeight,
            scroll_height: headerElement.scrollHeight
          };
        })()
      """)
      
      case height_result do
        %{"error" => error} ->
          {:error, "JavaScript error: #{error}"}
          
        %{"actual_height" => actual_height, "css_classes" => css_classes} = result ->
          status = determine_status(actual_height, page.expected_size)
          
          {:ok, %{
            actual_height: actual_height,
            css_classes: css_classes,
            status: status,
            full_result: result
          }}
          
        _ ->
          {:error, "Unexpected result format: #{inspect(height_result)}"}
      end
    end)
  end

  defp determine_status(actual_height, expected_size) do
    cond do
      String.contains?(expected_size, "h-12") and actual_height >= 47 and actual_height <= 49 ->
        "✅ CORRECT (matches h-12/48px)"
        
      String.contains?(expected_size, "h-16") and actual_height >= 63 and actual_height <= 65 ->
        "✅ CORRECT (matches h-16/64px)"
        
      String.contains?(expected_size, "h-12") and actual_height >= 63 and actual_height <= 65 ->
        "❌ WRONG (expected h-12/48px but got h-16/64px)"
        
      String.contains?(expected_size, "h-16") and actual_height >= 47 and actual_height <= 49 ->
        "❌ WRONG (expected h-16/64px but got h-12/48px)"
        
      true ->
        "❓ UNEXPECTED (#{actual_height}px doesn't match expected range)"
    end
  end
end

# Add playwriter to the path
Mix.install([
  {:playwright, "~> 1.45.0-alpha.1"}
])

# Run the inspection
HeaderInspector.inspect_header_heights()