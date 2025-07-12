defmodule SandboxTestHelper do
  @moduledoc """
  Enhanced test helper module for sandbox testing with proper isolation and cleanup.

  This module provides utilities for:
  - Creating and managing test sandboxes with proper cleanup
  - Ensuring test isolation between sandbox tests
  - Providing timing and synchronization utilities
  - Managing ETS tables and process state
  """

  require Logger

  alias OTPSupervisor.Core.SandboxManager

  # Test configuration
  @default_timeout 5000
  @cleanup_timeout 1000
  @sync_delay 10

  ## Sandbox Lifecycle Helpers

  @doc """
  Creates a test sandbox with proper cleanup registration.

  This function creates a sandbox and registers it for cleanup at the end of the test.
  It also waits for the sandbox to be fully initialized before returning.
  """
  def create_test_sandbox(opts \\ []) do
    module_or_app = Keyword.get(opts, :module_or_app, OtpSandbox.TestDemoSupervisor)
    sandbox_opts = Keyword.drop(opts, [:module_or_app, :cleanup])

    # Generate unique sandbox ID
    sandbox_id = generate_test_sandbox_id()

    # Create sandbox
    case SandboxManager.create_sandbox(sandbox_id, module_or_app, sandbox_opts) do
      {:ok, sandbox_info} ->
        # Register for cleanup if requested (default: true)
        if Keyword.get(opts, :cleanup, true) do
          register_sandbox_for_cleanup(sandbox_id)
        end

        # Wait for sandbox to be fully ready
        case wait_for_sandbox_ready(sandbox_id) do
          :ok ->
            {:ok, sandbox_info}

          {:error, reason} ->
            # Cleanup failed sandbox
            cleanup_sandbox(sandbox_id)
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Destroys a test sandbox with proper cleanup verification.
  """
  def destroy_test_sandbox(sandbox_id) do
    case SandboxManager.destroy_sandbox(sandbox_id) do
      :ok ->
        # Verify cleanup completed
        case wait_for_sandbox_cleanup(sandbox_id) do
          :ok ->
            :ok

          {:error, reason} ->
            Logger.warning("Sandbox cleanup verification failed: #{inspect(reason)}")
            # Don't fail test for cleanup verification issues
            :ok
        end

      {:error, :not_found} ->
        # Already cleaned up
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Waits for a sandbox to be fully ready and functional.
  """
  def wait_for_sandbox_ready(sandbox_id, timeout \\ @default_timeout) do
    start_time = System.monotonic_time(:millisecond)

    wait_loop = fn loop ->
      case SandboxManager.get_sandbox_info(sandbox_id) do
        {:ok, sandbox_info} ->
          # Verify supervisor is alive and functional
          if Process.alive?(sandbox_info.supervisor_pid) do
            case verify_sandbox_functional(sandbox_info) do
              :ok ->
                :ok

              {:error, _} ->
                if System.monotonic_time(:millisecond) - start_time > timeout do
                  {:error, :timeout}
                else
                  Process.sleep(@sync_delay)
                  loop.(loop)
                end
            end
          else
            {:error, :supervisor_not_alive}
          end

        {:error, :not_found} ->
          if System.monotonic_time(:millisecond) - start_time > timeout do
            {:error, :timeout}
          else
            Process.sleep(@sync_delay)
            loop.(loop)
          end
      end
    end

    wait_loop.(wait_loop)
  end

  @doc """
  Waits for a sandbox to be completely cleaned up.
  """
  def wait_for_sandbox_cleanup(sandbox_id, timeout \\ @cleanup_timeout) do
    start_time = System.monotonic_time(:millisecond)

    wait_loop = fn loop ->
      # Check if SandboxManager is alive before calling
      if Process.whereis(SandboxManager) do
        try do
          case SandboxManager.get_sandbox_info(sandbox_id) do
            {:error, :not_found} ->
              :ok

            {:ok, _sandbox_info} ->
              if System.monotonic_time(:millisecond) - start_time > timeout do
                {:error, :timeout}
              else
                Process.sleep(@sync_delay)
                loop.(loop)
              end
          end
        catch
          :exit, _ ->
            # SandboxManager died during the call, consider sandbox cleaned up
            :ok
        end
      else
        # SandboxManager is not alive, consider sandbox cleaned up
        :ok
      end
    end

    wait_loop.(wait_loop)
  end

  @doc """
  Verifies that a sandbox is functional by checking its supervisor and children.
  """
  def verify_sandbox_functional(sandbox_info) do
    supervisor_pid = sandbox_info.supervisor_pid

    # Check supervisor is alive
    unless Process.alive?(supervisor_pid) do
      {:error, :supervisor_dead}
    else
      # Check supervisor can list children
      try do
        children = Supervisor.which_children(supervisor_pid)

        # Verify expected number of children for TestDemoSupervisor
        if length(children) == 3 do
          :ok
        else
          {:error, {:unexpected_child_count, length(children)}}
        end
      rescue
        error -> {:error, {:supervisor_error, error}}
      end
    end
  end

  ## Test Isolation Helpers

  @doc """
  Ensures the SandboxManager is running and properly initialized.
  """
  def ensure_sandbox_manager_running do
    case Process.whereis(SandboxManager) do
      nil ->
        # Start SandboxManager if not running
        {:ok, _pid} = SandboxManager.start_link([])
        wait_for_sandbox_manager_ready()

      pid when is_pid(pid) ->
        # Check if it's responsive
        try do
          case GenServer.call(SandboxManager, :sync, 1000) do
            :ok ->
              :ok

            _ ->
              # Restart if unresponsive
              restart_sandbox_manager(pid)
          end
        catch
          :exit, _ ->
            # Process died or is unresponsive, restart it
            restart_sandbox_manager(pid)
        end
    end
  end

  defp restart_sandbox_manager(old_pid) do
    # Kill the old process if it's still alive
    if Process.alive?(old_pid) do
      Process.exit(old_pid, :kill)
    end

    # Wait a bit for cleanup
    Process.sleep(100)

    # Start a new one
    {:ok, _pid} = SandboxManager.start_link([])
    wait_for_sandbox_manager_ready()
  end

  @doc """
  Cleans up all test sandboxes and resets the sandbox manager state.
  """
  def cleanup_all_sandboxes do
    try do
      # Check if SandboxManager is alive before calling it
      if Process.whereis(SandboxManager) do
        case SandboxManager.list_sandboxes() do
          sandboxes when is_list(sandboxes) ->
            # Clean up all sandboxes
            for sandbox <- sandboxes do
              cleanup_sandbox(sandbox.id)
            end

            # Force application cleanup
            cleanup_sandbox_applications()

          _ ->
            :ok
        end
      else
        # SandboxManager not running, just clean up applications
        cleanup_sandbox_applications()
      end
    rescue
      # SandboxManager may not be running or may have crashed
      _ ->
        cleanup_sandbox_applications()
    end
  end

  @doc """
  Force cleanup of sandbox applications that may be stuck.
  """
  def cleanup_sandbox_applications do
    try do
      # Stop and unload otp_sandbox application if it's running
      case Application.stop(:otp_sandbox) do
        :ok -> :ok
        {:error, {:not_started, :otp_sandbox}} -> :ok
        {:error, _} -> :ok
      end

      case Application.unload(:otp_sandbox) do
        :ok -> :ok
        {:error, {:not_loaded, :otp_sandbox}} -> :ok
        {:error, _} -> :ok
      end

      # Small delay to let cleanup complete
      Process.sleep(50)
    rescue
      _ -> :ok
    end
  end

  @doc """
  Synchronizes with the SandboxManager to ensure all operations are complete.
  """
  def sync_sandbox_manager(timeout \\ @default_timeout) do
    try do
      case GenServer.call(SandboxManager, :sync, timeout) do
        :ok -> :ok
        _ -> {:error, :sync_failed}
      end
    rescue
      _ -> {:error, :manager_not_available}
    end
  end

  ## Performance Testing Helpers

  @doc """
  Measures the time taken to create a sandbox.
  """
  def measure_sandbox_creation_time(opts \\ []) do
    start_time = System.monotonic_time(:microsecond)

    case create_test_sandbox(opts) do
      {:ok, sandbox_info} ->
        end_time = System.monotonic_time(:microsecond)
        duration = end_time - start_time

        {:ok, sandbox_info, duration}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Measures memory usage of a sandbox.
  """
  def measure_sandbox_memory_usage(sandbox_id) do
    case SandboxManager.get_sandbox_info(sandbox_id) do
      {:ok, sandbox_info} ->
        app_memory = get_process_memory(sandbox_info.app_pid)
        supervisor_memory = get_process_memory(sandbox_info.supervisor_pid)

        # Get children memory
        children = Supervisor.which_children(sandbox_info.supervisor_pid)

        children_memory =
          children
          |> Enum.map(fn {_id, pid, _type, _modules} -> get_process_memory(pid) end)
          |> Enum.sum()

        total_memory = app_memory + supervisor_memory + children_memory

        {:ok,
         %{
           total: total_memory,
           app: app_memory,
           supervisor: supervisor_memory,
           children: children_memory
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Stress tests sandbox creation and destruction.
  """
  def stress_test_sandboxes(count, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    parallel = Keyword.get(opts, :parallel, false)

    start_time = System.monotonic_time(:millisecond)

    create_and_destroy = fn i ->
      sandbox_id = "stress_test_#{i}_#{:erlang.unique_integer([:positive])}"

      case SandboxManager.create_sandbox(sandbox_id, OtpSandbox.TestDemoSupervisor, []) do
        {:ok, _sandbox_info} ->
          case SandboxManager.destroy_sandbox(sandbox_id) do
            :ok -> {:ok, i}
            {:error, reason} -> {:error, {i, reason}}
          end

        {:error, reason} ->
          {:error, {i, reason}}
      end
    end

    results =
      if parallel do
        1..count
        |> Task.async_stream(create_and_destroy, timeout: timeout)
        |> Enum.to_list()
      else
        1..count
        |> Enum.map(create_and_destroy)
      end

    end_time = System.monotonic_time(:millisecond)
    duration = end_time - start_time

    {success_count, error_count} =
      results
      |> Enum.reduce({0, 0}, fn
        {:ok, _}, {success, error} -> {success + 1, error}
        {:error, _}, {success, error} -> {success, error + 1}
      end)

    {:ok,
     %{
       total_count: count,
       success_count: success_count,
       error_count: error_count,
       duration_ms: duration,
       ops_per_second: count / (duration / 1000)
     }}
  end

  ## Error Injection Helpers

  @doc """
  Simulates a compilation failure by creating a broken sandbox application.
  """
  def simulate_compilation_failure do
    # This would create a temporary broken sandbox app
    # For now, we'll simulate by trying to load a non-existent application
    {:error, :compilation_simulation_not_implemented}
  end

  @doc """
  Simulates resource exhaustion by creating many sandboxes.
  """
  def simulate_resource_exhaustion(limit \\ 100) do
    sandboxes =
      for i <- 1..limit do
        sandbox_id = "resource_test_#{i}_#{:erlang.unique_integer([:positive])}"

        case SandboxManager.create_sandbox(sandbox_id, OtpSandbox.TestDemoSupervisor, []) do
          {:ok, sandbox_info} -> sandbox_info
          {:error, _} -> nil
        end
      end

    # Clean up created sandboxes
    for sandbox <- sandboxes, sandbox != nil do
      SandboxManager.destroy_sandbox(sandbox.id)
    end

    {:ok, length(Enum.filter(sandboxes, &(&1 != nil)))}
  end

  ## Private Helper Functions

  defp generate_test_sandbox_id do
    test_name =
      case Process.get(:ex_unit_test) do
        %{name: name} -> name |> to_string() |> String.replace(~r/[^a-zA-Z0-9_]/, "_")
        _ -> "test"
      end

    unique_id = :erlang.unique_integer([:positive])
    "#{test_name}_#{unique_id}"
  end

  defp register_sandbox_for_cleanup(sandbox_id) do
    existing_cleanups = Process.get(:sandbox_cleanups, [])
    Process.put(:sandbox_cleanups, [sandbox_id | existing_cleanups])
  end

  defp cleanup_sandbox(sandbox_id) do
    try do
      SandboxManager.destroy_sandbox(sandbox_id)
    rescue
      _ -> :ok
    end
  end

  defp wait_for_sandbox_manager_ready(timeout \\ @default_timeout) do
    start_time = System.monotonic_time(:millisecond)

    wait_loop = fn loop ->
      case GenServer.call(SandboxManager, :sync, 1000) do
        :ok ->
          :ok

        _ ->
          if System.monotonic_time(:millisecond) - start_time > timeout do
            {:error, :timeout}
          else
            Process.sleep(@sync_delay)
            loop.(loop)
          end
      end
    end

    wait_loop.(wait_loop)
  end

  defp get_process_memory(pid) when is_pid(pid) do
    case Process.info(pid, :memory) do
      {:memory, memory} -> memory
      _ -> 0
    end
  end

  defp get_process_memory(_), do: 0

  ## Test Setup and Teardown

  @doc """
  Sets up a test with proper sandbox manager initialization.
  Call this from your test setup.
  """
  def setup_sandbox_test(_context) do
    # Force cleanup of any stuck applications first
    cleanup_sandbox_applications()

    # Ensure SandboxManager is running
    ensure_sandbox_manager_running()

    # Clean up any existing sandboxes
    cleanup_all_sandboxes()

    # Register cleanup callback
    ExUnit.Callbacks.on_exit(fn -> cleanup_test_sandboxes() end)

    :ok
  end

  @doc """
  Cleans up all test sandboxes. Call this from your test teardown or on_exit.
  """
  def cleanup_test_sandboxes do
    # Clean up registered sandboxes
    sandbox_cleanups = Process.get(:sandbox_cleanups, [])

    for sandbox_id <- sandbox_cleanups do
      cleanup_sandbox(sandbox_id)
    end

    # Clear cleanup list
    Process.put(:sandbox_cleanups, [])

    # Clean up any remaining sandboxes
    cleanup_all_sandboxes()

    # Force application cleanup
    cleanup_sandbox_applications()

    # Sync with SandboxManager
    sync_sandbox_manager(1000)
  end
end
