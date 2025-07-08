defmodule OTPSupervisor.Core.Arsenal.Operations.Storage.TraceSessionStorage do
  @moduledoc """
  ETS-based storage for trace sessions.

  This module manages the storage and retrieval of trace session metadata,
  providing a clean API for the TraceProcess operation while handling
  all ETS table lifecycle management.

  ## Features

  - Automatic ETS table creation and management
  - Thread-safe operations for concurrent trace sessions
  - Proper cleanup and lifecycle management
  - Integration with Arsenal supervision tree

  ## Storage Schema

  Each trace session is stored as: `{trace_id, trace_info}`

  Where `trace_info` contains:
  - `pid`: The traced process PID
  - `flags`: List of trace flags
  - `collector`: Collector process PID
  - `start_time`: Timestamp when tracing started
  - `duration`: Trace duration in milliseconds
  - `max_events`: Maximum events to collect
  """

  use GenServer

  @table_name :trace_sessions

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Store a new trace session.

  ## Parameters
  - `trace_id`: Unique identifier for the trace session
  - `trace_info`: Map containing session metadata

  ## Returns
  - `:ok` if storage succeeds
  - `{:error, reason}` if storage fails
  """
  def store_session(trace_id, trace_info) when is_binary(trace_id) and is_map(trace_info) do
    GenServer.call(__MODULE__, {:store_session, trace_id, trace_info})
  end

  @doc """
  Retrieve a trace session by ID.

  ## Parameters
  - `trace_id`: The trace session identifier

  ## Returns
  - `{:ok, trace_info}` if session found
  - `{:error, :not_found}` if session doesn't exist
  """
  def get_session(trace_id) when is_binary(trace_id) do
    GenServer.call(__MODULE__, {:get_session, trace_id})
  end

  @doc """
  Delete a trace session.

  ## Parameters
  - `trace_id`: The trace session identifier

  ## Returns
  - `:ok` regardless of whether session existed
  """
  def delete_session(trace_id) when is_binary(trace_id) do
    GenServer.call(__MODULE__, {:delete_session, trace_id})
  end

  @doc """
  List all active trace sessions.

  ## Returns
  - List of `{trace_id, trace_info}` tuples
  """
  def list_sessions do
    GenServer.call(__MODULE__, :list_sessions)
  end

  @doc """
  Get the count of active trace sessions.

  ## Returns
  - Integer count of active sessions
  """
  def session_count do
    GenServer.call(__MODULE__, :session_count)
  end

  @doc """
  Clean up expired trace sessions.

  Removes sessions that have exceeded their duration.

  ## Returns
  - `{:ok, deleted_count}` with number of sessions cleaned up
  """
  def cleanup_expired_sessions do
    GenServer.call(__MODULE__, :cleanup_expired_sessions)
  end

  # GenServer Implementation

  def init(_opts) do
    # Create ETS table with appropriate options
    table =
      :ets.new(@table_name, [
        # Named table for easy access
        :named_table,
        # Allow access from other processes
        :public,
        # Key-value storage (unique keys)
        :set,
        # Optimize for concurrent reads
        {:read_concurrency, true},
        # Optimize for concurrent writes
        {:write_concurrency, true}
      ])

    # Schedule periodic cleanup of expired sessions
    # Every minute
    :timer.send_interval(60_000, self(), :cleanup_expired)

    {:ok, %{table: table}}
  end

  def handle_call({:store_session, trace_id, trace_info}, _from, state) do
    # Add storage timestamp for cleanup purposes
    enriched_info = Map.put(trace_info, :stored_at, System.monotonic_time(:millisecond))

    result =
      try do
        :ets.insert(@table_name, {trace_id, enriched_info})
        :ok
      rescue
        error -> {:error, {:storage_failed, error}}
      end

    {:reply, result, state}
  end

  def handle_call({:get_session, trace_id}, _from, state) do
    result =
      case :ets.lookup(@table_name, trace_id) do
        [{^trace_id, trace_info}] -> {:ok, trace_info}
        [] -> {:error, :not_found}
      end

    {:reply, result, state}
  end

  def handle_call({:delete_session, trace_id}, _from, state) do
    :ets.delete(@table_name, trace_id)
    {:reply, :ok, state}
  end

  def handle_call(:list_sessions, _from, state) do
    sessions = :ets.tab2list(@table_name)
    {:reply, sessions, state}
  end

  def handle_call(:session_count, _from, state) do
    count = :ets.info(@table_name, :size)
    {:reply, count, state}
  end

  def handle_call(:cleanup_expired_sessions, _from, state) do
    deleted_count = cleanup_expired_sessions_internal()
    {:reply, {:ok, deleted_count}, state}
  end

  def handle_info(:cleanup_expired, state) do
    # Periodic cleanup - don't block on this
    spawn(fn -> cleanup_expired_sessions_internal() end)
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  # Internal Functions

  defp cleanup_expired_sessions_internal do
    current_time = System.monotonic_time(:millisecond)

    expired_sessions =
      @table_name
      |> :ets.tab2list()
      |> Enum.filter(fn {_trace_id, trace_info} ->
        is_session_expired?(trace_info, current_time)
      end)

    # Delete expired sessions
    Enum.each(expired_sessions, fn {trace_id, _trace_info} ->
      :ets.delete(@table_name, trace_id)
    end)

    length(expired_sessions)
  end

  defp is_session_expired?(trace_info, current_time) do
    stored_at = Map.get(trace_info, :stored_at, current_time)
    # Default 60 seconds
    duration = Map.get(trace_info, :duration, 60_000)

    current_time - stored_at > duration
  end
end
