Application.ensure_all_started(:otp_supervisor)

# Create a mock socket
socket = %Phoenix.LiveView.Socket{
  assigns: %{__changed__: %{}},
  endpoint: OtpSupervisorWeb.Endpoint
}

IO.puts("Testing SystemDashboardLive mount...")

try do
  result = OtpSupervisorWeb.SystemDashboardLive.mount(%{}, %{}, socket)
  IO.inspect(result, label: "Mount result")

  case result do
    {:ok, socket} ->
      IO.puts("Mount successful!")
      IO.inspect(socket.assigns, label: "Socket assigns", limit: :infinity)

    error ->
      IO.puts("Mount failed with: #{inspect(error)}")
  end
rescue
  error ->
    IO.inspect(error, label: "Mount error")
    IO.inspect(Exception.format_stacktrace(), label: "Stacktrace")
end
