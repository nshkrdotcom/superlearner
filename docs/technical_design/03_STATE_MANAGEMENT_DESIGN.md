# State Management System Design Document
## Interactive OTP Sandbox Development Platform

**Version**: 1.0  
**Date**: July 9, 2025  
**Authors**: System Architecture Team  
**Status**: Draft

---

## Table of Contents

1. [Overview](#overview)
2. [System Architecture](#system-architecture)
3. [Core Components](#core-components)
4. [State Persistence](#state-persistence)
5. [State Synchronization](#state-synchronization)
6. [Conflict Resolution](#conflict-resolution)
7. [Performance Optimization](#performance-optimization)
8. [Implementation Details](#implementation-details)
9. [Testing Strategy](#testing-strategy)
10. [Monitoring and Observability](#monitoring-and-observability)

---

## Overview

### Purpose

The State Management System is responsible for capturing, storing, versioning, and synchronizing the complete state of sandbox environments, including process states, code versions, user interactions, and collaboration data. This system enables features like time-travel debugging, collaborative development, and educational checkpoints.

### Design Goals

- **Comprehensive State Capture**: Record all relevant state changes in sandbox environments
- **High Performance**: Minimize impact on sandbox execution performance
- **Collaborative Support**: Enable real-time state synchronization between multiple users
- **Time-Travel Debugging**: Allow developers to step through historical states
- **Educational Features**: Support checkpoint/restore for learning scenarios
- **Fault Tolerance**: Ensure state consistency even during system failures

### Key Features

- Process state snapshots with configurable granularity
- Code version tracking with dependency management
- User interaction history and replay capabilities
- Real-time collaborative state synchronization
- Conflict resolution for concurrent modifications
- Efficient delta compression for state storage
- Time-travel debugging with state restoration

---

## System Architecture

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    State Management Layer                       │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │   State     │  │   State     │  │ Conflict    │  │  State  │ │
│  │  Capture    │  │ Persistence │  │ Resolution  │  │  Sync   │ │
│  │  Engine     │  │   System    │  │   Engine    │  │ Manager │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │   Delta     │  │   Version   │  │   Event     │  │  Query  │ │
│  │ Compression │  │  Control    │  │   Store     │  │ Engine  │ │
│  │   Engine    │  │   System    │  │             │  │         │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │  Storage    │  │   Cache     │  │   Metrics   │  │  API    │ │
│  │  Backend    │  │   Layer     │  │  Collector  │  │Gateway  │ │
│  │             │  │             │  │             │  │         │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

### Component Interactions

```
Sandbox Process → State Capture → Delta Compression → Storage Backend
                       ↓
                 Event Store → Version Control → Conflict Resolution
                       ↓
                 Sync Manager → Real-time Updates → Collaborative UI
                       ↓
                 Query Engine → State Restoration → Time-Travel Debug
```

---

## Core Components

### 1. State Capture Engine

**Purpose**: Efficiently capture state changes from sandbox processes with minimal performance impact.

**Key Features**:
- Process state extraction using `sys:get_state/1` with safety checks
- Code version tracking with AST comparison
- User interaction logging with privacy controls
- Configurable capture granularity (high/medium/low)
- Asynchronous capture to avoid blocking main processes

**Implementation**:

```elixir
defmodule OtpSupervisor.StateManagement.CaptureEngine do
  @moduledoc """
  Captures state changes from sandbox processes with configurable granularity.
  """

  use GenServer
  require Logger

  @capture_interval 1000  # 1 second default
  @max_state_size 1_000_000  # 1MB limit per state

  defstruct [
    :sandbox_id,
    :capture_config,
    :active_processes,
    :state_buffer,
    :last_capture_time
  ]

  def start_link(sandbox_id, config \\ %{}) do
    GenServer.start_link(__MODULE__, {sandbox_id, config}, name: via_tuple(sandbox_id))
  end

  def init({sandbox_id, config}) do
    state = %__MODULE__{
      sandbox_id: sandbox_id,
      capture_config: merge_default_config(config),
      active_processes: MapSet.new(),
      state_buffer: [],
      last_capture_time: System.monotonic_time()
    }

    schedule_capture()
    {:ok, state}
  end

  def handle_info(:capture_state, state) do
    new_state = perform_capture(state)
    schedule_capture()
    {:noreply, new_state}
  end

  defp perform_capture(state) do
    timestamp = System.monotonic_time()
    
    # Capture process states
    process_states = capture_process_states(state)
    
    # Capture code versions
    code_versions = capture_code_versions(state)
    
    # Create state snapshot
    snapshot = %{
      timestamp: timestamp,
      sandbox_id: state.sandbox_id,
      process_states: process_states,
      code_versions: code_versions,
      metadata: %{
        capture_duration: System.monotonic_time() - timestamp,
        process_count: length(process_states)
      }
    }

    # Store snapshot asynchronously
    Task.start(fn -> store_snapshot(snapshot) end)
    
    %{state | last_capture_time: timestamp}
  end

  defp capture_process_states(state) do
    state.active_processes
    |> Enum.map(&capture_single_process_state/1)
    |> Enum.reject(&is_nil/1)
  end

  defp capture_single_process_state(pid) do
    case Process.alive?(pid) do
      true ->
        try do
          process_state = :sys.get_state(pid, 100)  # 100ms timeout
          process_info = Process.info(pid, [:registered_name, :current_function, :message_queue_len])
          
          %{
            pid: pid,
            state: truncate_if_large(process_state),
            info: process_info,
            captured_at: System.monotonic_time()
          }
        rescue
          _ -> nil
        end
      false ->
        nil
    end
  end

  defp capture_code_versions(state) do
    # Capture loaded modules and their versions
    :code.all_loaded()
    |> Enum.filter(fn {module, _} -> 
      module_belongs_to_sandbox?(module, state.sandbox_id) 
    end)
    |> Enum.map(fn {module, beam_path} ->
      %{
        module: module,
        beam_path: beam_path,
        md5: get_module_md5(module),
        version: get_module_version(module)
      }
    end)
  end

  defp truncate_if_large(state) when byte_size(:erlang.term_to_binary(state)) > @max_state_size do
    :state_too_large
  end
  defp truncate_if_large(state), do: state

  defp store_snapshot(snapshot) do
    OtpSupervisor.StateManagement.PersistenceSystem.store_snapshot(snapshot)
  end

  defp schedule_capture do
    Process.send_after(self(), :capture_state, @capture_interval)
  end

  defp via_tuple(sandbox_id) do
    {:via, Registry, {OtpSupervisor.StateManagement.Registry, "capture_#{sandbox_id}"}}
  end
end
```

### 2. State Persistence System

**Purpose**: Efficiently store and retrieve state snapshots with compression and indexing.

**Key Features**:
- Delta compression to minimize storage requirements
- Configurable retention policies
- Fast retrieval with indexed queries
- Automatic cleanup of old snapshots
- Support for different storage backends (ETS, Mnesia, PostgreSQL)

**Implementation**:

```elixir
defmodule OtpSupervisor.StateManagement.PersistenceSystem do
  @moduledoc """
  Handles storage and retrieval of state snapshots with compression and indexing.
  """

  use GenServer
  require Logger

  @compression_threshold 1024  # Compress if larger than 1KB
  @default_retention_days 30
  @index_rebuild_interval 86_400_000  # 24 hours

  defstruct [
    :storage_backend,
    :compression_config,
    :retention_policy,
    :index_cache,
    :cleanup_timer
  ]

  def start_link(config \\ %{}) do
    GenServer.start_link(__MODULE__, config, name: __MODULE__)
  end

  def store_snapshot(snapshot) do
    GenServer.call(__MODULE__, {:store_snapshot, snapshot})
  end

  def get_snapshot(sandbox_id, timestamp) do
    GenServer.call(__MODULE__, {:get_snapshot, sandbox_id, timestamp})
  end

  def list_snapshots(sandbox_id, options \\ []) do
    GenServer.call(__MODULE__, {:list_snapshots, sandbox_id, options})
  end

  def init(config) do
    state = %__MODULE__{
      storage_backend: get_storage_backend(config),
      compression_config: get_compression_config(config),
      retention_policy: get_retention_policy(config),
      index_cache: :ets.new(:snapshot_index, [:set, :protected]),
      cleanup_timer: nil
    }

    # Start cleanup timer
    cleanup_timer = Process.send_after(self(), :cleanup_old_snapshots, @index_rebuild_interval)
    
    {:ok, %{state | cleanup_timer: cleanup_timer}}
  end

  def handle_call({:store_snapshot, snapshot}, _from, state) do
    result = do_store_snapshot(snapshot, state)
    {:reply, result, state}
  end

  def handle_call({:get_snapshot, sandbox_id, timestamp}, _from, state) do
    result = do_get_snapshot(sandbox_id, timestamp, state)
    {:reply, result, state}
  end

  def handle_call({:list_snapshots, sandbox_id, options}, _from, state) do
    result = do_list_snapshots(sandbox_id, options, state)
    {:reply, result, state}
  end

  def handle_info(:cleanup_old_snapshots, state) do
    perform_cleanup(state)
    cleanup_timer = Process.send_after(self(), :cleanup_old_snapshots, @index_rebuild_interval)
    {:noreply, %{state | cleanup_timer: cleanup_timer}}
  end

  defp do_store_snapshot(snapshot, state) do
    # Generate unique ID
    snapshot_id = generate_snapshot_id(snapshot)
    
    # Compress if needed
    compressed_snapshot = maybe_compress(snapshot, state.compression_config)
    
    # Store in backend
    case state.storage_backend.store(snapshot_id, compressed_snapshot) do
      :ok ->
        # Update index
        update_index(snapshot_id, snapshot, state)
        {:ok, snapshot_id}
      error ->
        error
    end
  end

  defp do_get_snapshot(sandbox_id, timestamp, state) do
    case find_snapshot_id(sandbox_id, timestamp, state) do
      {:ok, snapshot_id} ->
        case state.storage_backend.get(snapshot_id) do
          {:ok, compressed_data} ->
            snapshot = maybe_decompress(compressed_data, state.compression_config)
            {:ok, snapshot}
          error ->
            error
        end
      error ->
        error
    end
  end

  defp maybe_compress(snapshot, compression_config) do
    binary_data = :erlang.term_to_binary(snapshot)
    
    if byte_size(binary_data) > @compression_threshold do
      compressed = :zlib.compress(binary_data)
      %{
        compressed: true,
        algorithm: compression_config.algorithm,
        data: compressed
      }
    else
      %{
        compressed: false,
        data: binary_data
      }
    end
  end

  defp maybe_decompress(%{compressed: true, data: compressed_data}, _config) do
    decompressed = :zlib.uncompress(compressed_data)
    :erlang.binary_to_term(decompressed)
  end
  defp maybe_decompress(%{compressed: false, data: binary_data}, _config) do
    :erlang.binary_to_term(binary_data)
  end

  defp generate_snapshot_id(snapshot) do
    content = "#{snapshot.sandbox_id}_#{snapshot.timestamp}"
    :crypto.hash(:sha256, content) |> Base.encode16(case: :lower)
  end

  defp update_index(snapshot_id, snapshot, state) do
    :ets.insert(state.index_cache, {
      {snapshot.sandbox_id, snapshot.timestamp},
      snapshot_id
    })
  end

  defp find_snapshot_id(sandbox_id, timestamp, state) do
    case :ets.lookup(state.index_cache, {sandbox_id, timestamp}) do
      [{_key, snapshot_id}] ->
        {:ok, snapshot_id}
      [] ->
        {:error, :not_found}
    end
  end
end
```

### 3. Delta Compression Engine

**Purpose**: Minimize storage requirements by calculating and storing only differences between states.

**Key Features**:
- Intelligent diff calculation for Elixir terms
- Configurable compression algorithms
- Efficient delta application for state reconstruction
- Support for binary and structured data differences

**Implementation**:

```elixir
defmodule OtpSupervisor.StateManagement.DeltaCompression do
  @moduledoc """
  Provides delta compression for state snapshots to minimize storage requirements.
  """

  @doc """
  Calculate delta between two states.
  """
  def calculate_delta(previous_state, current_state) do
    %{
      type: :delta,
      timestamp: System.monotonic_time(),
      changes: diff_terms(previous_state, current_state)
    }
  end

  @doc """
  Apply delta to reconstruct state.
  """
  def apply_delta(base_state, delta) do
    apply_changes(base_state, delta.changes)
  end

  defp diff_terms(term1, term2) when term1 == term2, do: :no_change

  defp diff_terms(map1, map2) when is_map(map1) and is_map(map2) do
    all_keys = Map.keys(map1) ++ Map.keys(map2) |> Enum.uniq()
    
    changes = 
      all_keys
      |> Enum.reduce(%{}, fn key, acc ->
        case {Map.get(map1, key), Map.get(map2, key)} do
          {val, val} -> acc
          {nil, val} -> Map.put(acc, key, {:added, val})
          {val, nil} -> Map.put(acc, key, {:removed, val})
          {old_val, new_val} -> 
            case diff_terms(old_val, new_val) do
              :no_change -> acc
              change -> Map.put(acc, key, {:changed, change})
            end
        end
      end)
    
    if map_size(changes) == 0, do: :no_change, else: {:map_changes, changes}
  end

  defp diff_terms(list1, list2) when is_list(list1) and is_list(list2) do
    # Simple list diff - in production, use a more sophisticated algorithm
    if list1 == list2 do
      :no_change
    else
      {:list_replaced, list2}
    end
  end

  defp diff_terms(_term1, term2), do: {:replaced, term2}

  defp apply_changes(term, :no_change), do: term

  defp apply_changes(map, {:map_changes, changes}) when is_map(map) do
    Enum.reduce(changes, map, fn {key, change}, acc ->
      case change do
        {:added, val} -> Map.put(acc, key, val)
        {:removed, _val} -> Map.delete(acc, key)
        {:changed, sub_change} -> 
          old_val = Map.get(acc, key)
          new_val = apply_changes(old_val, sub_change)
          Map.put(acc, key, new_val)
      end
    end)
  end

  defp apply_changes(_term, {:replaced, new_term}), do: new_term
  defp apply_changes(_term, {:list_replaced, new_list}), do: new_list
end
```

### 4. Version Control System

**Purpose**: Track and manage versions of code and state with branching and merging capabilities.

**Key Features**:
- Git-like versioning for sandbox states
- Branch creation and merging
- Conflict detection and resolution
- Rollback capabilities
- Tag management for important states

**Implementation**:

```elixir
defmodule OtpSupervisor.StateManagement.VersionControl do
  @moduledoc """
  Provides version control capabilities for sandbox states and code.
  """

  use GenServer
  require Logger

  defstruct [
    :sandbox_id,
    :current_branch,
    :branches,
    :commits,
    :tags,
    :merge_conflicts
  ]

  def start_link(sandbox_id) do
    GenServer.start_link(__MODULE__, sandbox_id, name: via_tuple(sandbox_id))
  end

  def create_branch(sandbox_id, branch_name, from_commit \\ nil) do
    GenServer.call(via_tuple(sandbox_id), {:create_branch, branch_name, from_commit})
  end

  def commit_state(sandbox_id, state, message, author) do
    GenServer.call(via_tuple(sandbox_id), {:commit_state, state, message, author})
  end

  def checkout_branch(sandbox_id, branch_name) do
    GenServer.call(via_tuple(sandbox_id), {:checkout_branch, branch_name})
  end

  def merge_branch(sandbox_id, source_branch, target_branch) do
    GenServer.call(via_tuple(sandbox_id), {:merge_branch, source_branch, target_branch})
  end

  def init(sandbox_id) do
    state = %__MODULE__{
      sandbox_id: sandbox_id,
      current_branch: "main",
      branches: %{"main" => []},
      commits: %{},
      tags: %{},
      merge_conflicts: []
    }
    
    {:ok, state}
  end

  def handle_call({:create_branch, branch_name, from_commit}, _from, state) do
    case create_new_branch(state, branch_name, from_commit) do
      {:ok, new_state} ->
        {:reply, :ok, new_state}
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:commit_state, snapshot, message, author}, _from, state) do
    case create_commit(state, snapshot, message, author) do
      {:ok, commit_id, new_state} ->
        {:reply, {:ok, commit_id}, new_state}
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:checkout_branch, branch_name}, _from, state) do
    case checkout_branch_impl(state, branch_name) do
      {:ok, new_state} ->
        {:reply, :ok, new_state}
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:merge_branch, source_branch, target_branch}, _from, state) do
    case merge_branches(state, source_branch, target_branch) do
      {:ok, new_state} ->
        {:reply, :ok, new_state}
      {:error, :conflicts, conflicts, new_state} ->
        {:reply, {:error, :conflicts, conflicts}, new_state}
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  defp create_new_branch(state, branch_name, from_commit) do
    if Map.has_key?(state.branches, branch_name) do
      {:error, :branch_exists}
    else
      base_commit = from_commit || get_latest_commit(state, state.current_branch)
      new_branches = Map.put(state.branches, branch_name, [base_commit])
      {:ok, %{state | branches: new_branches}}
    end
  end

  defp create_commit(state, snapshot, message, author) do
    commit_id = generate_commit_id(snapshot, message, author)
    
    commit = %{
      id: commit_id,
      snapshot: snapshot,
      message: message,
      author: author,
      timestamp: System.monotonic_time(),
      parent: get_latest_commit(state, state.current_branch)
    }
    
    new_commits = Map.put(state.commits, commit_id, commit)
    new_branches = add_commit_to_branch(state.branches, state.current_branch, commit_id)
    
    new_state = %{state | commits: new_commits, branches: new_branches}
    {:ok, commit_id, new_state}
  end

  defp checkout_branch_impl(state, branch_name) do
    if Map.has_key?(state.branches, branch_name) do
      {:ok, %{state | current_branch: branch_name}}
    else
      {:error, :branch_not_found}
    end
  end

  defp merge_branches(state, source_branch, target_branch) do
    with {:ok, source_commits} <- get_branch_commits(state, source_branch),
         {:ok, target_commits} <- get_branch_commits(state, target_branch),
         {:ok, merge_base} <- find_merge_base(source_commits, target_commits),
         {:ok, merged_state} <- perform_merge(state, source_branch, target_branch, merge_base) do
      {:ok, merged_state}
    else
      {:error, :conflicts, conflicts} ->
        new_state = %{state | merge_conflicts: conflicts}
        {:error, :conflicts, conflicts, new_state}
      error ->
        error
    end
  end

  defp generate_commit_id(snapshot, message, author) do
    content = "#{inspect(snapshot)}#{message}#{author}#{System.monotonic_time()}"
    :crypto.hash(:sha256, content) |> Base.encode16(case: :lower)
  end

  defp get_latest_commit(state, branch_name) do
    case Map.get(state.branches, branch_name) do
      [latest | _] -> latest
      [] -> nil
    end
  end

  defp add_commit_to_branch(branches, branch_name, commit_id) do
    current_commits = Map.get(branches, branch_name, [])
    Map.put(branches, branch_name, [commit_id | current_commits])
  end

  defp via_tuple(sandbox_id) do
    {:via, Registry, {OtpSupervisor.StateManagement.Registry, "version_control_#{sandbox_id}"}}
  end
end
```

---

## State Synchronization

### Real-time Synchronization Architecture

```
User A Changes → Local State → Conflict Detection → State Merge → Broadcast
                                      ↓
User B Changes → Local State → Conflict Detection → State Merge → Broadcast
                                      ↓
                            Synchronized State → All Users
```

### Operational Transform Implementation

```elixir
defmodule OtpSupervisor.StateManagement.OperationalTransform do
  @moduledoc """
  Implements operational transform for real-time collaborative editing.
  """

  @doc """
  Transform operation based on concurrent operations.
  """
  def transform_operation(op1, op2, priority \\ :left) do
    case {op1.type, op2.type} do
      {:insert, :insert} -> transform_insert_insert(op1, op2, priority)
      {:insert, :delete} -> transform_insert_delete(op1, op2)
      {:delete, :insert} -> transform_delete_insert(op1, op2)
      {:delete, :delete} -> transform_delete_delete(op1, op2)
      {:update, :update} -> transform_update_update(op1, op2, priority)
      _ -> handle_complex_transform(op1, op2, priority)
    end
  end

  defp transform_insert_insert(op1, op2, priority) do
    cond do
      op1.position < op2.position -> 
        {op1, %{op2 | position: op2.position + op1.length}}
      op1.position > op2.position -> 
        {%{op1 | position: op1.position + op2.length}, op2}
      priority == :left -> 
        {op1, %{op2 | position: op2.position + op1.length}}
      true -> 
        {%{op1 | position: op1.position + op2.length}, op2}
    end
  end

  defp transform_insert_delete(op1, op2) do
    cond do
      op1.position <= op2.position ->
        {op1, %{op2 | position: op2.position + op1.length}}
      op1.position > op2.position + op2.length ->
        {%{op1 | position: op1.position - op2.length}, op2}
      true ->
        # Insert is within deleted range
        {%{op1 | position: op2.position}, op2}
    end
  end

  defp transform_delete_insert(op1, op2) do
    {op2_transformed, op1_transformed} = transform_insert_delete(op2, op1)
    {op1_transformed, op2_transformed}
  end

  defp transform_delete_delete(op1, op2) do
    cond do
      op1.position + op1.length <= op2.position ->
        {op1, %{op2 | position: op2.position - op1.length}}
      op2.position + op2.length <= op1.position ->
        {%{op1 | position: op1.position - op2.length}, op2}
      true ->
        # Overlapping deletes - merge them
        merge_overlapping_deletes(op1, op2)
    end
  end

  defp merge_overlapping_deletes(op1, op2) do
    start_pos = min(op1.position, op2.position)
    end_pos = max(op1.position + op1.length, op2.position + op2.length)
    
    merged_op = %{
      type: :delete,
      position: start_pos,
      length: end_pos - start_pos,
      merged_from: [op1.id, op2.id]
    }
    
    {merged_op, :no_op}
  end
end
```

---

## Conflict Resolution

### Conflict Detection

```elixir
defmodule OtpSupervisor.StateManagement.ConflictResolution do
  @moduledoc """
  Handles conflict detection and resolution for collaborative editing.
  """

  def detect_conflicts(operations) do
    operations
    |> Enum.with_index()
    |> Enum.flat_map(fn {op, index} ->
      operations
      |> Enum.drop(index + 1)
      |> Enum.with_index(index + 1)
      |> Enum.filter(fn {other_op, _} -> conflicts?(op, other_op) end)
      |> Enum.map(fn {other_op, other_index} ->
        %{
          type: :conflict,
          operation1: {op, index},
          operation2: {other_op, other_index},
          resolution_strategy: determine_resolution_strategy(op, other_op)
        }
      end)
    end)
  end

  defp conflicts?(op1, op2) do
    case {op1.type, op2.type} do
      {:update, :update} -> 
        op1.target == op2.target and op1.timestamp != op2.timestamp
      {:delete, :update} -> 
        op2.target in get_affected_targets(op1)
      {:update, :delete} -> 
        op1.target in get_affected_targets(op2)
      {:delete, :delete} -> 
        overlapping_ranges?(op1, op2)
      _ -> 
        false
    end
  end

  defp determine_resolution_strategy(op1, op2) do
    case {op1.type, op2.type} do
      {:update, :update} -> :merge_updates
      {:delete, :update} -> :delete_wins
      {:update, :delete} -> :delete_wins
      _ -> :manual_resolution
    end
  end

  def resolve_conflict(conflict) do
    case conflict.resolution_strategy do
      :merge_updates -> merge_update_operations(conflict)
      :delete_wins -> apply_delete_operation(conflict)
      :last_write_wins -> apply_latest_operation(conflict)
      :manual_resolution -> {:error, :manual_resolution_required}
    end
  end

  defp merge_update_operations(conflict) do
    {op1, _} = conflict.operation1
    {op2, _} = conflict.operation2
    
    merged_value = deep_merge(op1.value, op2.value)
    
    merged_op = %{
      type: :update,
      target: op1.target,
      value: merged_value,
      timestamp: max(op1.timestamp, op2.timestamp),
      merged_from: [op1.id, op2.id]
    }
    
    {:ok, merged_op}
  end

  defp deep_merge(map1, map2) when is_map(map1) and is_map(map2) do
    Map.merge(map1, map2, fn _k, v1, v2 ->
      case {v1, v2} do
        {m1, m2} when is_map(m1) and is_map(m2) -> deep_merge(m1, m2)
        {_, v2} -> v2  # Last write wins for non-map values
      end
    end)
  end
  defp deep_merge(_v1, v2), do: v2
end
```

---

## Performance Optimization

### Efficient State Storage

```elixir
defmodule OtpSupervisor.StateManagement.PerformanceOptimizer do
  @moduledoc """
  Optimizes state management performance through various techniques.
  """

  # Lazy loading for large states
  def lazy_load_state(sandbox_id, timestamp, fields \\ :all) do
    base_state = get_base_state(sandbox_id, timestamp)
    
    case fields do
      :all -> fully_load_state(base_state)
      field_list -> partially_load_state(base_state, field_list)
    end
  end

  # Incremental synchronization
  def sync_incremental_changes(sandbox_id, last_sync_timestamp) do
    changes = get_changes_since(sandbox_id, last_sync_timestamp)
    
    # Only sync necessary changes
    filtered_changes = filter_relevant_changes(changes)
    
    # Batch similar changes
    batched_changes = batch_similar_changes(filtered_changes)
    
    {:ok, batched_changes}
  end

  # Memory-efficient state diffing
  def efficient_diff(state1, state2) do
    # Use structural sharing to minimize memory usage
    diff_with_sharing(state1, state2, %{})
  end

  defp diff_with_sharing(same, same, _cache), do: :no_change

  defp diff_with_sharing(map1, map2, cache) when is_map(map1) and is_map(map2) do
    cache_key = {map1, map2}
    
    case Map.get(cache, cache_key) do
      nil ->
        result = compute_map_diff(map1, map2, cache)
        new_cache = Map.put(cache, cache_key, result)
        {result, new_cache}
      cached_result ->
        {cached_result, cache}
    end
  end

  defp compute_map_diff(map1, map2, cache) do
    # Efficient map diffing implementation
    all_keys = Map.keys(map1) ++ Map.keys(map2) |> Enum.uniq()
    
    {changes, _final_cache} = 
      Enum.reduce(all_keys, {%{}, cache}, fn key, {acc, current_cache} ->
        val1 = Map.get(map1, key)
        val2 = Map.get(map2, key)
        
        case diff_with_sharing(val1, val2, current_cache) do
          {:no_change, new_cache} -> {acc, new_cache}
          {change, new_cache} -> {Map.put(acc, key, change), new_cache}
        end
      end)
    
    if map_size(changes) == 0, do: :no_change, else: {:map_changes, changes}
  end

  # Compression optimization
  def optimize_compression(data) do
    # Choose compression algorithm based on data characteristics
    case analyze_data_characteristics(data) do
      :highly_repetitive -> use_lz4_compression(data)
      :structured -> use_zstd_compression(data)
      :small -> skip_compression(data)
      :mixed -> use_adaptive_compression(data)
    end
  end

  defp analyze_data_characteristics(data) do
    binary_data = :erlang.term_to_binary(data)
    size = byte_size(binary_data)
    
    cond do
      size < 1024 -> :small
      has_high_repetition?(binary_data) -> :highly_repetitive
      is_structured_data?(data) -> :structured
      true -> :mixed
    end
  end

  defp has_high_repetition?(binary_data) do
    # Simple repetition detection
    sample_size = min(1024, byte_size(binary_data))
    sample = binary_part(binary_data, 0, sample_size)
    
    compressed_sample = :zlib.compress(sample)
    compression_ratio = byte_size(compressed_sample) / byte_size(sample)
    
    compression_ratio < 0.7  # High compression indicates repetition
  end

  defp is_structured_data?(data) when is_map(data), do: true
  defp is_structured_data?(data) when is_list(data), do: true
  defp is_structured_data?(_), do: false
end
```

---

## Implementation Details

### State Management API

```elixir
defmodule OtpSupervisor.StateManagement.API do
  @moduledoc """
  Public API for state management operations.
  """

  # State capture operations
  def start_capture(sandbox_id, config \\ %{}) do
    OtpSupervisor.StateManagement.CaptureEngine.start_link(sandbox_id, config)
  end

  def stop_capture(sandbox_id) do
    OtpSupervisor.StateManagement.CaptureEngine.stop(sandbox_id)
  end

  def capture_snapshot(sandbox_id) do
    OtpSupervisor.StateManagement.CaptureEngine.capture_now(sandbox_id)
  end

  # State persistence operations
  def save_state(sandbox_id, state, metadata \\ %{}) do
    snapshot = %{
      sandbox_id: sandbox_id,
      state: state,
      timestamp: System.monotonic_time(),
      metadata: metadata
    }
    
    OtpSupervisor.StateManagement.PersistenceSystem.store_snapshot(snapshot)
  end

  def load_state(sandbox_id, timestamp) do
    OtpSupervisor.StateManagement.PersistenceSystem.get_snapshot(sandbox_id, timestamp)
  end

  def list_states(sandbox_id, options \\ []) do
    OtpSupervisor.StateManagement.PersistenceSystem.list_snapshots(sandbox_id, options)
  end

  # Version control operations
  def create_checkpoint(sandbox_id, name, description \\ "") do
    with {:ok, current_state} <- get_current_state(sandbox_id),
         {:ok, commit_id} <- OtpSupervisor.StateManagement.VersionControl.commit_state(
           sandbox_id, current_state, description, get_current_user()
         ),
         :ok <- OtpSupervisor.StateManagement.VersionControl.create_tag(
           sandbox_id, commit_id, name
         ) do
      {:ok, commit_id}
    end
  end

  def restore_checkpoint(sandbox_id, checkpoint_name) do
    with {:ok, commit_id} <- OtpSupervisor.StateManagement.VersionControl.get_tag(
           sandbox_id, checkpoint_name
         ),
         {:ok, state} <- OtpSupervisor.StateManagement.VersionControl.get_commit_state(
           sandbox_id, commit_id
         ),
         :ok <- restore_sandbox_state(sandbox_id, state) do
      {:ok, commit_id}
    end
  end

  # Collaboration operations
  def start_collaboration(sandbox_id, users) do
    OtpSupervisor.StateManagement.CollaborationManager.start_session(sandbox_id, users)
  end

  def sync_user_changes(sandbox_id, user_id, changes) do
    OtpSupervisor.StateManagement.CollaborationManager.apply_changes(
      sandbox_id, user_id, changes
    )
  end

  def get_collaboration_status(sandbox_id) do
    OtpSupervisor.StateManagement.CollaborationManager.get_status(sandbox_id)
  end

  # Utility functions
  defp get_current_state(sandbox_id) do
    # Implementation depends on sandbox structure
    case OtpSupervisor.Core.SandboxManager.get_sandbox_state(sandbox_id) do
      {:ok, state} -> {:ok, state}
      error -> error
    end
  end

  defp restore_sandbox_state(sandbox_id, state) do
    # Implementation depends on sandbox structure
    OtpSupervisor.Core.SandboxManager.restore_state(sandbox_id, state)
  end

  defp get_current_user do
    # Get current user from context or session
    "system"  # Placeholder
  end
end
```

---

## Testing Strategy

### Unit Tests

```elixir
defmodule OtpSupervisor.StateManagement.CaptureEngineTest do
  use ExUnit.Case, async: true
  alias OtpSupervisor.StateManagement.CaptureEngine

  describe "state capture" do
    test "captures process states successfully" do
      sandbox_id = "test_sandbox_#{System.unique_integer([:positive])}"
      
      {:ok, _pid} = CaptureEngine.start_link(sandbox_id)
      
      # Wait for initial capture
      Process.sleep(100)
      
      # Verify state was captured
      assert {:ok, snapshots} = list_snapshots(sandbox_id)
      assert length(snapshots) > 0
    end

    test "handles large states gracefully" do
      large_state = create_large_test_state()
      
      # Test that large states are handled without memory issues
      result = CaptureEngine.capture_single_process_state(self(), large_state)
      
      assert result.state == :state_too_large
    end

    test "captures with configurable granularity" do
      config = %{granularity: :high, capture_interval: 100}
      
      {:ok, _pid} = CaptureEngine.start_link("test_sandbox", config)
      
      # Verify high granularity capture
      Process.sleep(200)
      
      assert {:ok, snapshots} = list_snapshots("test_sandbox")
      assert length(snapshots) >= 2
    end
  end

  defp create_large_test_state do
    # Create a state larger than the size limit
    large_data = String.duplicate("x", 2_000_000)
    %{large_field: large_data}
  end
end
```

### Integration Tests

```elixir
defmodule OtpSupervisor.StateManagement.IntegrationTest do
  use ExUnit.Case, async: false
  alias OtpSupervisor.StateManagement.API

  describe "end-to-end state management" do
    test "complete state lifecycle" do
      sandbox_id = "integration_test_#{System.unique_integer([:positive])}"
      
      # Start capture
      {:ok, _capture_pid} = API.start_capture(sandbox_id)
      
      # Create initial state
      initial_state = %{counter: 0, name: "test"}
      {:ok, snapshot_id1} = API.save_state(sandbox_id, initial_state)
      
      # Modify state
      modified_state = %{counter: 1, name: "test_modified"}
      {:ok, snapshot_id2} = API.save_state(sandbox_id, modified_state)
      
      # Create checkpoint
      {:ok, checkpoint_id} = API.create_checkpoint(sandbox_id, "milestone_1")
      
      # Verify we can restore
      {:ok, restored_state} = API.load_state(sandbox_id, snapshot_id1)
      assert restored_state.state.counter == 0
      
      # Verify checkpoint restoration
      {:ok, _} = API.restore_checkpoint(sandbox_id, "milestone_1")
      
      # Cleanup
      API.stop_capture(sandbox_id)
    end

    test "collaborative editing scenario" do
      sandbox_id = "collab_test_#{System.unique_integer([:positive])}"
      
      # Start collaboration session
      {:ok, _session} = API.start_collaboration(sandbox_id, ["user1", "user2"])
      
      # User 1 makes changes
      changes1 = [%{type: :update, target: "field1", value: "user1_value"}]
      {:ok, _} = API.sync_user_changes(sandbox_id, "user1", changes1)
      
      # User 2 makes concurrent changes
      changes2 = [%{type: :update, target: "field2", value: "user2_value"}]
      {:ok, _} = API.sync_user_changes(sandbox_id, "user2", changes2)
      
      # Verify merged state
      {:ok, status} = API.get_collaboration_status(sandbox_id)
      assert status.active_users == ["user1", "user2"]
      assert status.conflicts == []
    end
  end
end
```

---

## Monitoring and Observability

### Metrics Collection

```elixir
defmodule OtpSupervisor.StateManagement.Metrics do
  @moduledoc """
  Collects and reports metrics for state management operations.
  """

  use GenServer
  require Logger

  @metrics [
    :capture_latency,
    :storage_latency,
    :compression_ratio,
    :conflict_resolution_time,
    :sync_latency,
    :state_size,
    :active_captures,
    :storage_usage
  ]

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def record_capture_latency(sandbox_id, latency_ms) do
    GenServer.cast(__MODULE__, {:record_metric, :capture_latency, latency_ms, %{sandbox_id: sandbox_id}})
  end

  def record_storage_operation(operation, latency_ms, size_bytes) do
    GenServer.cast(__MODULE__, {:record_metric, :storage_latency, latency_ms, %{operation: operation}})
    GenServer.cast(__MODULE__, {:record_metric, :state_size, size_bytes, %{operation: operation}})
  end

  def record_compression_ratio(original_size, compressed_size) do
    ratio = compressed_size / original_size
    GenServer.cast(__MODULE__, {:record_metric, :compression_ratio, ratio, %{}})
  end

  def get_metrics_summary do
    GenServer.call(__MODULE__, :get_summary)
  end

  def init([]) do
    # Initialize metrics storage
    metrics_table = :ets.new(:state_management_metrics, [:named_table, :public])
    
    # Initialize counters
    Enum.each(@metrics, fn metric ->
      :ets.insert(metrics_table, {metric, []})
    end)
    
    {:ok, %{table: metrics_table}}
  end

  def handle_cast({:record_metric, metric, value, metadata}, state) do
    timestamp = System.monotonic_time()
    
    # Store metric with timestamp and metadata
    metric_record = {timestamp, value, metadata}
    
    # Update ETS table
    :ets.update_element(state.table, metric, {2, [metric_record | get_metric_history(metric)]})
    
    # Emit telemetry event
    :telemetry.execute(
      [:otp_supervisor, :state_management, metric],
      %{value: value},
      metadata
    )
    
    {:noreply, state}
  end

  def handle_call(:get_summary, _from, state) do
    summary = 
      @metrics
      |> Enum.map(fn metric ->
        history = get_metric_history(metric)
        {metric, calculate_metric_summary(history)}
      end)
      |> Map.new()
    
    {:reply, summary, state}
  end

  defp get_metric_history(metric) do
    case :ets.lookup(:state_management_metrics, metric) do
      [{^metric, history}] -> history
      [] -> []
    end
  end

  defp calculate_metric_summary(history) do
    case history do
      [] -> 
        %{count: 0, avg: 0, min: 0, max: 0, recent: []}
      values ->
        numeric_values = Enum.map(values, fn {_timestamp, value, _metadata} -> value end)
        
        %{
          count: length(numeric_values),
          avg: Enum.sum(numeric_values) / length(numeric_values),
          min: Enum.min(numeric_values),
          max: Enum.max(numeric_values),
          recent: Enum.take(values, 10)
        }
    end
  end
end
```

---

## Configuration

### System Configuration

```elixir
# config/config.exs
config :otp_supervisor, :state_management,
  # Capture settings
  capture_interval: 1000,  # ms
  max_state_size: 1_000_000,  # bytes
  capture_granularity: :medium,  # :high, :medium, :low
  
  # Storage settings
  storage_backend: OtpSupervisor.StateManagement.Storage.ETS,
  compression_enabled: true,
  compression_threshold: 1024,  # bytes
  
  # Retention settings
  retention_days: 30,
  max_snapshots_per_sandbox: 1000,
  
  # Collaboration settings
  max_concurrent_users: 10,
  conflict_resolution_strategy: :automatic,
  
  # Performance settings
  async_operations: true,
  batch_size: 100,
  cache_size: 1000,
  
  # Monitoring settings
  metrics_enabled: true,
  telemetry_enabled: true
```

---

## Conclusion

This State Management System provides a comprehensive foundation for the interactive OTP sandbox platform. The design emphasizes:

- **Performance**: Efficient state capture and storage with minimal impact on sandbox execution
- **Scalability**: Support for multiple concurrent users and large state histories
- **Reliability**: Robust conflict resolution and data consistency mechanisms
- **Observability**: Comprehensive metrics and monitoring capabilities
- **Extensibility**: Modular design allowing for future enhancements

The system integrates seamlessly with the existing OTP Supervisor architecture while providing the advanced state management capabilities needed for collaborative development and educational features.

Next steps include implementing the remaining technical design documents and beginning the development of the core components outlined in this specification.