#!/usr/bin/env elixir

IO.puts("🔍 Getting ACTUAL rendered heights...")

{:ok, browser} = Playwright.launch(:chromium, %{headless: false})
{:ok, page} = Playwright.Browser.new_page(browser)

pages = [
  {"Docs", "http://localhost:4000/docs"},
  {"System Dashboard", "http://localhost:4000/system"},
  {"Arsenal", "http://localhost:4000/arsenal"},
  {"Supervisor", "http://localhost:4000/supervisors"}
]

Enum.each(pages, fn {name, url} ->
  Playwright.Page.goto(page, url)
  Process.sleep(3000)
  
  result = Playwright.Page.evaluate(page, """
    (() => {
      const headers = document.querySelectorAll('div[class*="h-1"]');
      let targetHeader = null;
      
      for (const header of headers) {
        if (header.className.includes('flex') && header.className.includes('items-center') && 
            header.className.includes('border-b')) {
          targetHeader = header;
          break;
        }
      }
      
      if (!targetHeader) return {error: 'Header not found'};
      
      const rect = targetHeader.getBoundingClientRect();
      const computed = window.getComputedStyle(targetHeader);
      
      return {
        actualHeight: rect.height,
        computedHeight: computed.height,
        offsetHeight: targetHeader.offsetHeight,
        className: targetHeader.className
      };
    })()
  """)
  
  case result do
    %{"error" => error} ->
      IO.puts("❌ #{name}: #{error}")
    %{"actualHeight" => height} ->
      IO.puts("#{name}: #{height}px ACTUAL rendered height")
  end
end)

Playwright.Browser.close(browser)