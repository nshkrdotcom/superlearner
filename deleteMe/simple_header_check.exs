#!/usr/bin/env elixir

# Simple approach to check header heights using basic HTTP requests
# and analyzing the rendered HTML classes

defmodule SimpleHeaderCheck do
  @moduledoc """
  Simple check for header heights by analyzing HTML classes
  """

  def check_header_heights do
    IO.puts("🔍 Checking header heights by analyzing HTML classes...")
    
    # Test pages 
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

    IO.puts("📊 Results:")
    IO.puts("=" <> String.duplicate("=", 80))

    Enum.each(pages, fn page ->
      case check_page_header(page) do
        {:ok, result} ->
          IO.puts("✅ #{page.name} (#{page.url})")
          IO.puts("   Expected: #{page.expected_size}")
          IO.puts("   Found:    #{result.found_class}")
          IO.puts("   Status:   #{result.status}")
          IO.puts("")
          
        {:error, reason} ->
          IO.puts("❌ #{page.name} (#{page.url})")
          IO.puts("   Error:    #{reason}")
          IO.puts("")
      end
    end)
  end

  defp check_page_header(page) do
    case System.cmd("curl", ["-s", page.url]) do
      {html, 0} ->
        # Look for header div with height classes
        cond do
          String.contains?(html, "h-12") ->
            status = if String.contains?(page.expected_size, "h-12") do
              "✅ CORRECT"
            else
              "❌ WRONG (expected large but found small)"
            end
            {:ok, %{found_class: "h-12 (48px)", status: status}}
            
          String.contains?(html, "h-16") ->
            status = if String.contains?(page.expected_size, "h-16") do
              "✅ CORRECT"
            else
              "❌ WRONG (expected small but found large)"
            end
            {:ok, %{found_class: "h-16 (64px)", status: status}}
            
          String.contains?(html, "h-14") ->
            {:ok, %{found_class: "h-14 (56px)", status: "🤔 MEDIUM SIZE"}}
            
          true ->
            {:ok, %{found_class: "no height class found", status: "❓ UNKNOWN"}}
        end
        
      {_output, exit_code} ->
        {:error, "curl failed with exit code #{exit_code}"}
    end
  end
end

# Run the check
SimpleHeaderCheck.check_header_heights()