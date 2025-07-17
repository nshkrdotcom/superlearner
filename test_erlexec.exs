#!/usr/bin/env elixir

# Simple test script for erlexec integration
Mix.install([{:erlexec, path: "./erlexec"}])

defmodule ErlexecTest do
  require Logger

  def test_basic_functionality do
    Logger.info("Starting erlexec...")

    with :ok <- start_erlexec(),
         :ok <- test_simple_command(),
         :ok <- test_wrapper() do
      Logger.info("✅ All tests passed!")
      :ok
    else
      {:error, reason} ->
        Logger.error("❌ Test failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp start_erlexec do
    case Application.start(:erlexec) do
      :ok ->
        Logger.info("Erlexec started successfully")
        :ok

      {:error, {:already_started, :erlexec}} ->
        Logger.info("Erlexec already running")
        :ok

      {:error, reason} ->
        Logger.error("Failed to start erlexec: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp test_simple_command do
    Logger.info("Testing simple command...")

    case :exec.run(["echo", "Hello from erlexec!"], [:sync, :stdout]) do
      {:ok, output} ->
        Logger.info("Command succeeded: #{inspect(output)}")
        :ok

      {:error, reason} ->
        Logger.error("Command failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp test_wrapper do
    # Add the lib path
    Code.append_path("./lib")

    case Code.ensure_loaded(OTPSupervisor.TestCluster.ExecWrapper) do
      {:module, _} ->
        test_wrapper_commands()

      {:error, reason} ->
        Logger.error("Could not load ExecWrapper: #{inspect(reason)}")
        {:error, :module_not_found}
    end
  end

  defp test_wrapper_commands do
    alias OTPSupervisor.TestCluster.ExecWrapper

    # Test starting a simple command
    case ExecWrapper.start_command("echo", ["Hello from wrapper!"], []) do
      {:ok, process_info} ->
        Logger.info("Wrapper started process: #{inspect(process_info)}")

        # Wait a bit for output
        :timer.sleep(1000)

        # Process should already be done since echo exits immediately
        case ExecWrapper.get_status(process_info.os_pid) do
          {:error, :not_running} ->
            Logger.info("Process completed successfully")
            :ok

          {:ok, status} ->
            Logger.info("Process still running: #{inspect(status)}")
            ExecWrapper.stop_process(process_info)
            :ok

          {:error, reason} ->
            Logger.warning("Status check failed: #{inspect(reason)}")
            :ok
        end

      {:error, reason} ->
        Logger.error("Wrapper failed to start process: #{inspect(reason)}")
        {:error, reason}
    end
  end
end

# Run the test
case ErlexecTest.test_basic_functionality() do
  :ok ->
    IO.puts("✅ Erlexec integration test passed!")
    System.halt(0)

  {:error, reason} ->
    IO.puts("❌ Erlexec integration test failed: #{inspect(reason)}")
    System.halt(1)
end
