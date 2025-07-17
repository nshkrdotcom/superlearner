defmodule OTPSupervisor.TestCluster.ExecWrapper do
  @moduledoc """
  Minimal wrapper around erlexec for robust process management.

  This module provides a simple interface to replace System.cmd usage
  with erlexec for better process control and monitoring.
  """

  require Logger

  @doc """
  Start a command using erlexec with output handling.

  Returns: {:ok, %{exec_pid: pid, os_pid: integer}}
  """
  def start_command(cmd, args, opts \\ []) do
    # Extract options
    env = Keyword.get(opts, :env, [])
    cd = Keyword.get(opts, :cd, File.cwd!())
    monitor = Keyword.get(opts, :monitor, true)
    output_handler = Keyword.get(opts, :output_handler, self())

    # Build erlexec options - simplified for reliability
    exec_opts = [
      {:env, format_env(env)},
      {:cd, cd},
      {:kill_timeout, 5},
      {:stdout, output_handler},
      {:stderr, output_handler}
    ]

    # Add monitor option if requested
    final_opts = if monitor, do: [:monitor | exec_opts], else: exec_opts

    # Log the command for debugging
    Logger.info("Starting command: #{cmd} #{Enum.join(args, " ")}")
    Logger.debug("Working directory: #{cd}")

    # Start the process
    case :exec.run([cmd | args], final_opts) do
      {:ok, exec_pid, os_pid} ->
        result = %{
          exec_pid: exec_pid,
          os_pid: os_pid,
          command: cmd,
          args: args,
          started_at: DateTime.utc_now()
        }

        Logger.info("Started command #{cmd} with OS PID #{os_pid}")
        {:ok, result}

      {:error, reason} ->
        Logger.error("Failed to start command #{cmd}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Stop a process gracefully.
  """
  def stop_process(process_info, _timeout \\ 5000) do
    Logger.info("Stopping process #{process_info.os_pid}")

    case :exec.stop(process_info.os_pid) do
      :ok ->
        Logger.info("Process #{process_info.os_pid} stopped successfully")
        :ok

      {:error, :no_process} ->
        Logger.debug("Process #{process_info.os_pid} already stopped")
        :ok

      {:error, reason} ->
        Logger.warning("Failed to stop process #{process_info.os_pid}: #{inspect(reason)}")
        # Try force kill as fallback
        case :exec.kill(process_info.os_pid, 9) do
          :ok ->
            Logger.info("Force killed process #{process_info.os_pid}")
            :ok

          {:error, kill_reason} ->
            Logger.error(
              "Failed to force kill process #{process_info.os_pid}: #{inspect(kill_reason)}"
            )

            {:error, kill_reason}
        end
    end
  end

  @doc """
  Get status of a process.
  """
  def get_status(os_pid) do
    case :exec.status(os_pid) do
      {:status, status} ->
        {:ok, status}

      {:signal, signal, _core_dumped} ->
        {:ok, {:terminated_by_signal, signal}}
    end
  end

  @doc """
  List all processes managed by erlexec.
  """
  def list_processes do
    children = :exec.which_children()
    {:ok, children}
  end

  # Private functions

  defp format_env(env) do
    Enum.map(env, fn
      {key, value} when is_binary(key) and is_binary(value) ->
        {String.to_charlist(key), String.to_charlist(value)}

      {key, value} when is_atom(key) ->
        {to_charlist(key), String.to_charlist(to_string(value))}

      {key, value} ->
        {String.to_charlist(to_string(key)), String.to_charlist(to_string(value))}
    end)
  end
end
