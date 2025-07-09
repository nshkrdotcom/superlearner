defmodule OtpSupervisorWeb.PageControllerTest do
  use OtpSupervisorWeb.ConnCase
  @moduletag :ui

  test "GET / redirects to /supervisors", %{conn: conn} do
    conn = get(conn, ~p"/")
    assert redirected_to(conn) == ~p"/supervisors"
  end
end
