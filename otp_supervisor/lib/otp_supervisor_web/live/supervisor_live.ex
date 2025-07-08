defmodule OtpSupervisorWeb.SupervisorLive do
  @moduledoc """
  LiveView for monitoring and controlling OTP supervisors.

  Provides a real-time view of:
  - All supervisors in the system
  - Children of selected supervisors
  - Process details and statistics
  - Controls to kill processes for testing restart behavior
  """
  use OtpSupervisorWeb, :live_view

  alias OTPSupervisor.Core.Control

  @refresh_interval 1000

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(@refresh_interval, self(), :refresh)
    end

    {:ok,
     socket
     |> assign(:supervisors, Control.list_supervisors())
     |> assign(:selected_supervisor, nil)
     |> assign(:children, [])
     |> assign(:selected_process, nil)
     |> assign(:process_info, nil)}
  end

  @impl true
  def handle_params(params, _url, socket) do
    supervisor_name = params["supervisor"]

    socket =
      if supervisor_name do
        select_supervisor(socket, supervisor_name)
      else
        socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("select_supervisor", %{"name" => name}, socket) do
    {:noreply,
     socket
     |> select_supervisor(name)
     |> push_patch(to: ~p"/supervisors?supervisor=#{name}")}
  end

  @impl true
  def handle_event("kill_process", %{"pid" => pid_string}, socket) do
    case Control.kill_process(pid_string) do
      :ok ->
        # Refresh immediately to show the change
        send(self(), :refresh)
        {:noreply, put_flash(socket, :info, "Process killed: #{pid_string}")}

      {:error, :invalid_pid} ->
        {:noreply, put_flash(socket, :error, "Invalid PID: #{pid_string}")}
    end
  end

  @impl true
  def handle_event("select_process", %{"pid" => pid_string}, socket) do
    case Control.to_pid(pid_string) do
      {:ok, pid} ->
        process_info =
          case Control.get_process_info(pid) do
            {:ok, info} -> info
            {:error, _} -> nil
          end

        {:noreply,
         socket
         |> assign(:selected_process, pid_string)
         |> assign(:process_info, process_info)}

      {:error, :invalid_pid} ->
        {:noreply,
         socket
         |> assign(:selected_process, nil)
         |> assign(:process_info, nil)
         |> put_flash(:error, "Invalid PID format")}
    end
  end

  @impl true
  def handle_info(:refresh, socket) do
    socket =
      socket
      |> assign(:supervisors, Control.list_supervisors())

    socket =
      if socket.assigns.selected_supervisor do
        refresh_children(socket)
      else
        socket
      end

    # Refresh process info if a process is selected
    socket =
      if socket.assigns.selected_process && socket.assigns.process_info do
        refresh_process_info(socket)
      else
        socket
      end

    {:noreply, socket}
  end

  # Private functions

  defp select_supervisor(socket, name) when is_binary(name) do
    try do
      atom_name = String.to_existing_atom(name)
      select_supervisor(socket, atom_name)
    rescue
      ArgumentError ->
        socket
        |> put_flash(:error, "Unknown supervisor: #{name}")
    end
  end

  defp select_supervisor(socket, name) when is_atom(name) do
    case Control.get_supervision_tree(name) do
      {:ok, children} ->
        socket
        |> assign(:selected_supervisor, name)
        |> assign(:children, children)
        |> assign(:selected_process, nil)
        |> assign(:process_info, nil)

      {:error, :not_found} ->
        socket
        |> put_flash(:error, "Supervisor not found: #{name}")

      {:error, :not_supervisor} ->
        socket
        |> put_flash(:error, "Process is not a supervisor: #{name}")

      {:error, reason} ->
        socket
        |> put_flash(:error, "Error: #{inspect(reason)}")
    end
  end

  defp refresh_children(socket) do
    case Control.get_supervision_tree(socket.assigns.selected_supervisor) do
      {:ok, children} ->
        assign(socket, :children, children)

      {:error, _} ->
        # Supervisor might have crashed, clear selection
        socket
        |> assign(:selected_supervisor, nil)
        |> assign(:children, [])
    end
  end

  defp refresh_process_info(socket) do
    case Control.to_pid(socket.assigns.selected_process) do
      {:ok, pid} ->
        case Control.get_process_info(pid) do
          {:ok, info} ->
            assign(socket, :process_info, info)

          {:error, :process_dead} ->
            socket
            |> assign(:process_info, nil)
            |> put_flash(:info, "Selected process is no longer alive")
        end

      {:error, :invalid_pid} ->
        socket
    end
  end

  # Helper functions for the template

  def format_bytes(bytes) when is_integer(bytes) do
    cond do
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 2)} GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 2)} MB"
      bytes >= 1024 -> "#{Float.round(bytes / 1024, 2)} KB"
      true -> "#{bytes} B"
    end
  end

  def format_bytes(_), do: "N/A"

  def format_key(key) when is_atom(key) do
    key
    |> Atom.to_string()
    |> String.split("_")
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end

  def format_value(value) when is_integer(value), do: to_string(value)
  def format_value(value) when is_atom(value), do: inspect(value)

  def format_value({m, f, a}) when is_atom(m) and is_atom(f) and is_integer(a) do
    "#{m}.#{f}/#{a}"
  end

  def format_value(value), do: inspect(value)
end
