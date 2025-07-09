defmodule OtpSandbox do
  @moduledoc """
  OTP Sandbox - A standalone educational OTP sandbox for learning supervision patterns.

  This module provides various supervisor and worker implementations that can be
  used to experiment with OTP supervision strategies in an isolated environment.
  """

  alias OtpSandbox.Supervisors.DemoSupervisor
  alias OtpSandbox.TestDemoSupervisor
  alias OtpSandbox.Workers.{Counter, Printer}

  @doc """
  Starts a demo supervisor with the given strategy.

  ## Examples

      iex> {:ok, pid} = OtpSandbox.start_demo_supervisor(:one_for_one)
      iex> is_pid(pid)
      true
  """
  def start_demo_supervisor(strategy \\ :one_for_one) do
    DemoSupervisor.start_link(strategy: strategy)
  end

  @doc """
  Starts a test demo supervisor with unique naming.

  ## Examples

      iex> {:ok, pid} = OtpSandbox.start_test_demo_supervisor()
      iex> is_pid(pid)
      true
  """
  def start_test_demo_supervisor(opts \\ []) do
    TestDemoSupervisor.start_link(opts)
  end

  @doc """
  Starts a counter worker.

  ## Examples

      iex> {:ok, pid} = OtpSandbox.start_counter(name: :my_counter)
      iex> is_pid(pid)
      true
  """
  def start_counter(opts \\ []) do
    Counter.start_link(opts)
  end

  @doc """
  Starts a printer worker.

  ## Examples

      iex> {:ok, pid} = OtpSandbox.start_printer(name: :my_printer)
      iex> is_pid(pid)
      true
  """
  def start_printer(opts \\ []) do
    Printer.start_link(opts)
  end
end