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
  Converts a PID string or registered name into a PID.

  Handles both "#PID<0.123.0>" and "<0.123.0>" formats for strings,
  and looks up registered names for atoms.

  Returns `{:ok, pid}` on success or `{:error, :invalid_pid}` on failure.

  ## Examples

      iex> OTPSupervisor.Core.Control.to_pid("#PID<0.123.0>")
      {:ok, #PID<0.123.0>}
      
      iex> OTPSupervisor.Core.Control.to_pid("<0.123.0>")
      {:ok, #PID<0.123.0>}
      
      iex> OTPSupervisor.Core.Control.to_pid(:registered_name)
      {:ok, #PID<0.456.0>}
      
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

  def to_pid(name) when is_atom(name) do
    case Process.whereis(name) do
      nil -> {:error, :not_found}
      pid when is_pid(pid) -> {:ok, pid}
      _ -> {:error, :invalid_pid}
    end
  end

  def to_pid(pid) when is_pid(pid) do
    {:ok, pid}
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

  @doc """
  Lists all processes in the system with optional filtering.

  This function provides deep introspection into the Elixir/OTP system by
  examining all running processes and categorizing them by their OTP behavior.
  This is educational for understanding process hierarchies and supervision trees.

  ## Process Type Classification

  Processes are classified based on their initial call and behavior:
  - `:supervisor` - Processes that implement supervisor behavior
  - `:genserver` - Processes using GenServer behavior
  - `:worker` - Other processes (Tasks, Agents, custom processes)
  - `:dead` - Processes that died during inspection

  ## Options

    * `:filter` - Filter by process type (:all, :supervisor, :genserver, :worker)
    * `:limit` - Maximum number of processes to return (:infinity for no limit)

  ## Educational Value

  This function demonstrates:
  - How to safely inspect running processes
  - Process registration and naming patterns
  - The relationship between different OTP behaviors
  - System-wide process management concepts

  ## Examples

      iex> OTPSupervisor.Core.Control.list_all_processes()
      [%{pid: "#PID<0.123.0>", name: :my_process, type: :genserver}]

      iex> OTPSupervisor.Core.Control.list_all_processes(filter: :supervisor)
      [%{pid: "#PID<0.124.0>", name: :my_supervisor, type: :supervisor}]
  """
  def list_all_processes(opts \\ []) do
    filter = Keyword.get(opts, :filter, :all)
    limit = Keyword.get(opts, :limit, :infinity)

    processes =
      Process.list()
      |> Stream.map(&process_info_with_type/1)
      |> Stream.filter(&filter_process(&1, filter))

    limited_processes =
      case limit do
        :infinity -> Enum.to_list(processes)
        n when is_integer(n) and n > 0 -> Enum.take(processes, n)
      end

    Enum.sort_by(limited_processes, & &1.name)
  end

  @doc """
  Gets the state of a GenServer process safely.

  This function demonstrates the proper OTP way to inspect GenServer state
  using the system debugging facilities. This is crucial for understanding
  how to safely debug running systems without breaking encapsulation.

  ## OTP System Interface

  Uses `:sys.get_state/2` with a timeout to safely extract internal state.
  The `:sys` module provides the standard OTP debugging interface that
  works with any process implementing the gen_* behaviors.

  ## Educational Value

  This function teaches:
  - How to safely inspect process state without side effects
  - The difference between GenServer and regular processes
  - Proper timeout handling for system operations
  - OTP debugging conventions and best practices

  ## Safety Considerations

  - Uses a short timeout to avoid hanging on unresponsive processes
  - Handles all possible error conditions gracefully
  - Does not modify the process state in any way
  - Safe to call on production systems

  ## Examples

      iex> {:ok, pid} = GenServer.start_link(MyGenServer, :ok)
      iex> OTPSupervisor.Core.Control.get_process_state(pid)
      {:ok, %{some: :state}}

      iex> pid = spawn(fn -> :ok end)
      iex> OTPSupervisor.Core.Control.get_process_state(pid)
      {:error, :not_a_genserver}
  """
  def get_process_state(pid) when is_pid(pid) do
    try do
      state = :sys.get_state(pid, 100)
      {:ok, state}
    rescue
      _ -> {:error, :not_a_genserver}
    catch
      :exit, _ -> {:error, :not_a_genserver}
    end
  end

  @doc """
  Builds a complete process relationship graph.

  This function creates a comprehensive map of the entire system's process
  topology, showing how processes are connected through links and monitors.
  This is invaluable for understanding OTP supervision principles.

  ## Process Relationships in OTP

  The graph includes three types of relationships:
  - **Processes**: All running processes with their types and names
  - **Links**: Bidirectional failure propagation connections
  - **Monitors**: Unidirectional observation relationships

  ## Educational Value

  This function demonstrates:
  - How OTP processes form supervision hierarchies
  - The difference between links and monitors
  - Process interconnectedness in fault-tolerant systems
  - Visual representation of abstract OTP concepts

  ## Graph Structure

  Returns a map with:
  - `:processes` - List of all processes with metadata
  - `:links` - Process link relationships
  - `:monitors` - Process monitor relationships

  ## Performance Considerations

  This function iterates through all system processes and should be used
  judiciously in production systems with many processes.

  ## Examples

      iex> OTPSupervisor.Core.Control.build_process_graph()
      %{
        processes: [%{pid: "#PID<0.123.0>", name: :worker, type: :genserver}],
        links: [%{pid: "#PID<0.123.0>", links: ["#PID<0.124.0>"]}],
        monitors: [%{pid: "#PID<0.123.0>", monitors: []}]
      }
  """
  def build_process_graph do
    processes = list_all_processes()

    %{
      processes: processes,
      links: build_link_graph(processes),
      monitors: build_monitor_graph(processes)
    }
  end

  # Private functions

  defp process_info_with_type(pid) do
    case Process.info(pid, [:registered_name, :initial_call]) do
      nil ->
        %{pid: inspect(pid), name: nil, type: :dead}

      info ->
        name = get_process_name(info)
        type = determine_process_type(pid, info)

        %{
          pid: inspect(pid),
          name: name,
          type: type
        }
    end
  end

  defp get_process_name(info) do
    case Keyword.get(info, :registered_name) do
      [] -> nil
      name when is_atom(name) -> name
      _ -> nil
    end
  end

  defp determine_process_type(pid, info) do
    cond do
      is_supervisor_pid?(pid) -> :supervisor
      is_genserver_pid?(pid, info) -> :genserver
      true -> :worker
    end
  end

  defp is_genserver_pid?(_pid, info) do
    case Keyword.get(info, :initial_call) do
      {:gen_server, _, _} -> true
      {mod, _, _} when mod in [GenServer] -> true
      _ -> false
    end
  end

  defp filter_process(_process, :all), do: true
  defp filter_process(process, filter), do: process.type == filter

  defp build_link_graph(processes) do
    processes
    |> Enum.map(fn process ->
      case to_pid(process.pid) do
        {:ok, pid} ->
          case Process.info(pid, :links) do
            {:links, links} ->
              %{pid: process.pid, links: Enum.map(links, &inspect/1)}

            _ ->
              %{pid: process.pid, links: []}
          end

        _ ->
          %{pid: process.pid, links: []}
      end
    end)
  end

  defp build_monitor_graph(processes) do
    processes
    |> Enum.map(fn process ->
      case to_pid(process.pid) do
        {:ok, pid} ->
          case Process.info(pid, :monitors) do
            {:monitors, monitors} ->
              monitor_list = Enum.map(monitors, fn {_type, ref_or_pid} -> inspect(ref_or_pid) end)
              %{pid: process.pid, monitors: monitor_list}

            _ ->
              %{pid: process.pid, monitors: []}
          end

        _ ->
          %{pid: process.pid, monitors: []}
      end
    end)
  end

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

  # Advanced Supervisor Control Features







  @doc """
  Simulates a crash with a specific reason.

  This function is useful for testing supervisor restart behavior and
  demonstrating different crash scenarios.

  ## Options

    * `:delay` - Delay before crash in milliseconds (default: 0)

  ## Examples

      iex> pid = spawn(fn -> :timer.sleep(:infinity) end)
      iex> OTPSupervisor.Core.Control.simulate_crash(pid, :custom_reason)
      :ok
  """
  def simulate_crash(pid, reason, opts \\ []) when is_pid(pid) do
    delay = Keyword.get(opts, :delay, 0)

    if delay > 0 do
      Process.send_after(self(), {:crash, pid, reason}, delay)
    else
      Process.exit(pid, reason)
    end

    :ok
  end

  # Private helper functions
end
