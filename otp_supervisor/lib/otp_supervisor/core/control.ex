defmodule OTPSupervisor.Core.Control do
  @moduledoc """
  Core API for controlling and inspecting Elixir supervisors.

  This module provides functions to:
  - List all supervisors in the system
  - Inspect supervision trees
  - Kill processes
  - Get detailed process information
  """

  @doc """
  Lists all registered supervisors in the system.

  Returns a list of maps containing supervisor information:
  - `:name` - The registered name of the supervisor
  - `:pid` - The PID as a string
  - `:alive` - Whether the process is alive
  - `:child_count` - Number of active children

  ## Examples

      iex> OTPSupervisor.Core.Control.list_supervisors()
      [
        %{
          name: :my_supervisor,
          pid: "#PID<0.123.0>",
          alive: true,
          child_count: 3
        }
      ]
  """
  def list_supervisors do
    Process.registered()
    |> Enum.filter(&is_supervisor?/1)
    |> Enum.map(&format_supervisor_info/1)
  end

  @doc """
  Gets the supervision tree for a given supervisor.

  Can accept either a supervisor name (atom) or PID.

  Returns `{:ok, children}` where children is a list of maps, or
  `{:error, reason}` if the supervisor cannot be found or accessed.

  ## Examples

      iex> {:ok, children} = OTPSupervisor.Core.Control.get_supervision_tree(:my_supervisor)
      iex> hd(children)
      %{
        id: MyWorker,
        pid: "#PID<0.124.0>",
        type: :worker,
        modules: [MyWorker],
        alive: true,
        info: %{memory: 2832, message_queue_len: 0, status: :waiting}
      }
  """
  def get_supervision_tree(supervisor_name) when is_atom(supervisor_name) do
    case Process.whereis(supervisor_name) do
      nil -> {:error, :not_found}
      pid -> get_supervision_tree(pid)
    end
  end

  def get_supervision_tree(supervisor_pid) when is_pid(supervisor_pid) do
    try do
      children = Supervisor.which_children(supervisor_pid)
      {:ok, format_children(children)}
    rescue
      ArgumentError ->
        # This happens when the PID is not a supervisor
        {:error, :not_supervisor}

      FunctionClauseError ->
        # This also happens when the PID is not a supervisor
        {:error, :not_supervisor}

      e ->
        # Any other error
        {:error, Exception.message(e)}
    end
  end

  @doc """
  Kills a process by PID or PID string.

  Accepts either a PID or a string representation of a PID.

  Returns `:ok` after sending the kill signal.

  ## Examples

      iex> pid = spawn(fn -> :timer.sleep(:infinity) end)
      iex> OTPSupervisor.Core.Control.kill_process(pid)
      :ok
      
      iex> OTPSupervisor.Core.Control.kill_process("#PID<0.123.0>")
      :ok
  """
  def kill_process(pid) when is_pid(pid) do
    Process.exit(pid, :kill)
    :ok
  end

  def kill_process(pid_string) when is_binary(pid_string) do
    case to_pid(pid_string) do
      {:ok, pid} -> kill_process(pid)
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Converts a PID string into a PID.

  Handles both "#PID<0.123.0>" and "<0.123.0>" formats.

  Returns `{:ok, pid}` on success or `{:error, :invalid_pid}` on failure.

  ## Examples

      iex> OTPSupervisor.Core.Control.to_pid("#PID<0.123.0>")
      {:ok, #PID<0.123.0>}
      
      iex> OTPSupervisor.Core.Control.to_pid("<0.123.0>")
      {:ok, #PID<0.123.0>}
      
      iex> OTPSupervisor.Core.Control.to_pid("invalid")
      {:error, :invalid_pid}
  """
  def to_pid(pid_string) when is_binary(pid_string) do
    try do
      # Handle both "#PID<0.123.0>" and "<0.123.0>" formats
      cleaned =
        pid_string
        |> String.replace("#PID", "")
        |> String.trim()

      pid =
        cleaned
        |> String.to_charlist()
        |> :erlang.list_to_pid()

      {:ok, pid}
    rescue
      _ -> {:error, :invalid_pid}
    end
  end

  @doc """
  Gets detailed information about a process.

  Returns `{:ok, info}` where info is a map of process attributes, or
  `{:error, :process_dead}` if the process no longer exists.

  The returned information includes:
  - `:memory` - Memory usage in bytes
  - `:message_queue_len` - Number of messages in queue
  - `:status` - Process status (:running, :waiting, etc.)
  - `:heap_size` - Heap size in words
  - `:stack_size` - Stack size in words
  - `:reductions` - Number of reductions executed
  - `:current_function` - Currently executing function

  ## Examples

      iex> {:ok, info} = OTPSupervisor.Core.Control.get_process_info(self())
      iex> Map.keys(info)
      [:current_function, :heap_size, :memory, :message_queue_len, 
       :reductions, :stack_size, :status]
  """
  def get_process_info(pid) when is_pid(pid) do
    keys = [
      :memory,
      :message_queue_len,
      :status,
      :heap_size,
      :stack_size,
      :reductions,
      :current_function
    ]

    case Process.info(pid, keys) do
      nil -> {:error, :process_dead}
      info -> {:ok, Map.new(info)}
    end
  end

  # Private functions

  defp is_supervisor?(name) when is_atom(name) do
    case Process.whereis(name) do
      nil -> false
      pid -> is_supervisor_pid?(pid)
    end
  end

  defp is_supervisor_pid?(pid) when is_pid(pid) do
    case Process.info(pid, :dictionary) do
      nil ->
        false

      {:dictionary, dict} ->
        # Check multiple indicators that this is a supervisor
        # First check the initial call to avoid calling supervisor functions on non-supervisors
        initial_call = Keyword.get(dict, :"$initial_call", false)

        case initial_call do
          {mod, _, _}
          when mod in [
                 :supervisor,
                 Supervisor,
                 DynamicSupervisor,
                 PartitionSupervisor,
                 Task.Supervisor
               ] ->
            true

          _ ->
            false
        end
    end
  end

  defp format_supervisor_info(name) when is_atom(name) do
    case Process.whereis(name) do
      nil ->
        %{
          name: name,
          pid: "nil",
          alive: false,
          child_count: 0
        }

      pid ->
        %{
          name: name,
          pid: inspect(pid),
          alive: Process.alive?(pid),
          child_count: count_children(pid)
        }
    end
  end

  defp count_children(pid) when is_pid(pid) do
    try do
      %{active: active} = Supervisor.count_children(pid)
      active
    rescue
      _ -> 0
    end
  end

  defp format_children(children) when is_list(children) do
    Enum.map(children, fn
      {id, pid, type, modules} when is_pid(pid) ->
        %{
          id: id,
          pid: inspect(pid),
          type: type,
          modules: modules,
          alive: Process.alive?(pid),
          info: get_basic_process_info(pid)
        }

      {id, pid, type, modules} ->
        # Handle case where pid might be :undefined or :restarting
        %{
          id: id,
          pid: inspect(pid),
          type: type,
          modules: modules,
          alive: false,
          info: %{}
        }
    end)
  end

  defp get_basic_process_info(pid) when is_pid(pid) do
    case Process.info(pid, [:memory, :message_queue_len, :status]) do
      nil -> %{}
      info -> Map.new(info)
    end
  end
end
