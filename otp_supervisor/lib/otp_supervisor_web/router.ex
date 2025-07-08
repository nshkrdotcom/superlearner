defmodule OtpSupervisorWeb.Router do
  use OtpSupervisorWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {OtpSupervisorWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug :put_secure_browser_headers
  end

  scope "/", OtpSupervisorWeb do
    pipe_through :browser

    get "/", PageController, :home
    live "/supervisors", SupervisorLive
    live "/system", SystemDashboardLive
  end

  # API routes
  scope "/api", OtpSupervisorWeb.Api, as: :api do
    pipe_through :api

    scope "/v1", V1, as: :v1 do
      get "/processes", ProcessController, :index
      get "/processes/:pid", ProcessController, :show
      get "/processes/:pid/state", ProcessController, :get_state
      get "/processes/:pid/messages", ProcessController, :get_messages
      post "/processes/:pid/trace", ProcessController, :start_trace
      delete "/processes/:pid/trace", ProcessController, :stop_trace
      post "/processes/:pid/message", ProcessController, :send_message

      get "/system/health", SystemController, :health
      get "/system/graph", SystemController, :graph
      get "/system/bottlenecks", SystemController, :bottlenecks
      get "/system/anomalies", SystemController, :anomalies

      get "/supervisors", SupervisorController, :index
      get "/supervisors/:name", SupervisorController, :show
      get "/supervisors/:name/analytics", SupervisorController, :analytics
      put "/supervisors/:name/strategy", SupervisorController, :change_strategy
      post "/supervisors/:name/simulate-failure", SupervisorController, :simulate_failure
    end
  end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:otp_supervisor, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: OtpSupervisorWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
