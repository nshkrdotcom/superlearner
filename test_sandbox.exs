#!/usr/bin/env elixir

# Test script to investigate sandbox system behavior

# Mix.install([{:otp_supervisor, path: "."}])

# Start the application
Application.ensure_all_started(:otp_supervisor)

# Wait for system to stabilize
:timer.sleep(1000)

IO.puts("=== SANDBOX SYSTEM INVESTIGATION ===")

# 1. Check if sandbox system starts automatically
IO.puts("\n1. Checking sandbox system startup...")
sandbox_manager_pid = Process.whereis(OTPSupervisor.Core.SandboxManager)
IO.puts("SandboxManager PID: #{inspect(sandbox_manager_pid)}")
IO.puts("SandboxManager alive: #{Process.alive?(sandbox_manager_pid)}")

# 2. List existing sandboxes
IO.puts("\n2. Listing existing sandboxes...")
sandboxes = OTPSupervisor.Core.SandboxManager.list_sandboxes()
IO.puts("Existing sandboxes: #{inspect(sandboxes)}")

# 3. Check if OtpSandbox.Supervisor exists
IO.puts("\n3. Checking OtpSandbox.Supervisor...")
otp_sandbox_pid = Process.whereis(OtpSandbox.Supervisor)
IO.puts("OtpSandbox.Supervisor PID: #{inspect(otp_sandbox_pid)}")
IO.puts("OtpSandbox.Supervisor alive: #{Process.alive?(otp_sandbox_pid)}")

# 4. List all supervisors and check for sandbox-related ones
IO.puts("\n4. Listing all supervisors...")
all_supervisors = OTPSupervisor.Core.Control.list_supervisors()

sandbox_supervisors =
  Enum.filter(all_supervisors, fn supervisor ->
    name_str = to_string(supervisor.name)

    String.contains?(name_str, "sandbox") or
      String.contains?(name_str, "Sandbox") or
      String.contains?(name_str, "OtpSandbox")
  end)

IO.puts("Found #{length(sandbox_supervisors)} sandbox-related supervisors:")

Enum.each(sandbox_supervisors, fn supervisor ->
  IO.puts("  - #{supervisor.name} (PID: #{supervisor.pid}, Children: #{supervisor.child_count})")
end)

# 5. Try to create a sandbox
IO.puts("\n5. Creating a sandbox...")

case OTPSupervisor.Core.Control.create_sandbox(:otp_sandbox) do
  {:ok, sandbox_info} ->
    IO.puts("Successfully created sandbox: #{inspect(sandbox_info)}")

    # Check if new supervisors appear
    IO.puts("\n6. Checking for new supervisors after sandbox creation...")
    new_supervisors = OTPSupervisor.Core.Control.list_supervisors()

    new_sandbox_supervisors =
      Enum.filter(new_supervisors, fn supervisor ->
        name_str = to_string(supervisor.name)

        String.contains?(name_str, "sandbox") or
          String.contains?(name_str, "Sandbox") or
          String.contains?(name_str, "OtpSandbox")
      end)

    IO.puts(
      "Found #{length(new_sandbox_supervisors)} sandbox-related supervisors after creation:"
    )

    Enum.each(new_sandbox_supervisors, fn supervisor ->
      IO.puts(
        "  - #{supervisor.name} (PID: #{supervisor.pid}, Children: #{supervisor.child_count})"
      )
    end)

    # Check supervisor children
    IO.puts("\n7. Checking OtpSandbox.Supervisor children...")

    case OTPSupervisor.Core.Control.get_supervision_tree(OtpSandbox.Supervisor) do
      {:ok, children} ->
        IO.puts("OtpSandbox.Supervisor has #{length(children)} children:")

        Enum.each(children, fn child ->
          IO.puts("  - #{child.id} (PID: #{child.pid}, Type: #{child.type})")
        end)

      {:error, reason} ->
        IO.puts("Failed to get OtpSandbox.Supervisor children: #{inspect(reason)}")
    end

  {:error, reason} ->
    IO.puts("Failed to create sandbox: #{inspect(reason)}")
end

# 8. Check the main application supervisor tree
IO.puts("\n8. Checking main OTP supervisor tree...")

case OTPSupervisor.Core.Control.get_supervision_tree(OtpSupervisor.Supervisor) do
  {:ok, children} ->
    IO.puts("OtpSupervisor.Supervisor has #{length(children)} children:")

    Enum.each(children, fn child ->
      IO.puts("  - #{child.id} (PID: #{child.pid}, Type: #{child.type})")
    end)

  {:error, reason} ->
    IO.puts("Failed to get OtpSupervisor.Supervisor children: #{inspect(reason)}")
end

IO.puts("\n=== INVESTIGATION COMPLETE ===")
