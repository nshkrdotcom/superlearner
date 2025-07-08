defmodule OtpSupervisorWeb.ErrorHTMLTest do
  use OtpSupervisorWeb.ConnCase, async: true

  # Bring render_to_string/4 for testing custom views
  import Phoenix.Template

  test "renders 404.html" do
    html = render_to_string(OtpSupervisorWeb.ErrorHTML, "404", "html", [])
    assert html =~ "ERROR 404: OPERATION NOT FOUND"
    assert html =~ "ARSENAL"
  end

  test "renders 500.html" do
    assert render_to_string(OtpSupervisorWeb.ErrorHTML, "500", "html", []) ==
             "Internal Server Error"
  end
end
