defmodule OtpSupervisorWeb.PageController do
  use OtpSupervisorWeb, :controller

  def home(conn, _params) do
    redirect(conn, to: ~p"/supervisors")
  end
end
