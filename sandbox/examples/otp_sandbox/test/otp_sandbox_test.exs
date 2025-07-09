defmodule OtpSandboxTest do
  use ExUnit.Case
  doctest OtpSandbox

  test "can start a demo supervisor" do
    {:ok, pid} = OtpSandbox.start_demo_supervisor(:one_for_one)
    assert is_pid(pid)
    GenServer.stop(pid)
  end

  test "can start a test demo supervisor" do
    {:ok, pid} = OtpSandbox.start_test_demo_supervisor()
    assert is_pid(pid)
    GenServer.stop(pid)
  end
end
