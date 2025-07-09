#!/usr/bin/env elixir

# Verify that all headers now have uniform height

defmodule UniformHeaderCheck do
  @moduledoc """
  Verify all headers have the same height (h-16, 64px)
  """

  def check_uniform_headers do
    IO.puts("🔍 Verifying all headers have uniform height (h-16, 64px)...")
    
    # Test pages - all should now have the same height
    pages = [
      %{name: "Docs", url: "http://localhost:4000/docs"},
      %{name: "System Dashboard", url: "http://localhost:4000/system"},
      %{name: "Arsenal", url: "http://localhost:4000/arsenal"},
      %{name: "Supervisor", url: "http://localhost:4000/supervisors"}
    ]

    IO.puts("📊 Results:")
    IO.puts("=" <> String.duplicate("=", 80))

    all_correct = Enum.all?(pages, fn page ->
      case check_page_header(page) do
        {:ok, result} ->
          status_icon = if result.has_h16, do: "✅", else: "❌"
          IO.puts("#{status_icon} #{page.name} (#{page.url})")
          IO.puts("   Height: #{result.found_class}")
          IO.puts("   Status: #{result.status}")
          IO.puts("")
          result.has_h16
          
        {:error, reason} ->
          IO.puts("❌ #{page.name} (#{page.url})")
          IO.puts("   Error: #{reason}")
          IO.puts("")
          false
      end
    end)

    IO.puts("=" <> String.duplicate("=", 80))
    if all_correct do
      IO.puts("🎉 SUCCESS: All headers now have uniform height (h-16, 64px)!")
    else
      IO.puts("❌ ISSUE: Some headers don't have uniform height")
    end
  end

  defp check_page_header(page) do
    case System.cmd("curl", ["-s", page.url]) do
      {html, 0} ->
        cond do
          String.contains?(html, "h-16") ->
            {:ok, %{
              found_class: "h-16 (64px)", 
              status: "✅ CORRECT - Uniform height",
              has_h16: true
            }}
            
          String.contains?(html, "h-12") ->
            {:ok, %{
              found_class: "h-12 (48px)", 
              status: "❌ WRONG - Should be h-16",
              has_h16: false
            }}
            
          String.contains?(html, "h-14") ->
            {:ok, %{
              found_class: "h-14 (56px)", 
              status: "❌ WRONG - Should be h-16",
              has_h16: false
            }}
            
          true ->
            {:ok, %{
              found_class: "no height class found", 
              status: "❓ UNKNOWN - No height class",
              has_h16: false
            }}
        end
        
      {_output, exit_code} ->
        {:error, "curl failed with exit code #{exit_code}"}
    end
  end
end

# Run the check
UniformHeaderCheck.check_uniform_headers()