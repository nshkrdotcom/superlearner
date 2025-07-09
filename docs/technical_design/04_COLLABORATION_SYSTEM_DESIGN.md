# Collaboration System Design Document
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
4. [Real-time Communication](#real-time-communication)
5. [User Management](#user-management)
6. [Session Management](#session-management)
7. [Conflict Resolution](#conflict-resolution)
8. [Permission System](#permission-system)
9. [Implementation Details](#implementation-details)
10. [Testing Strategy](#testing-strategy)
11. [Performance Considerations](#performance-considerations)

---

## Overview

### Purpose

The Collaboration System enables multiple users to work together in real-time on the same OTP sandbox environment. It provides seamless synchronization of code changes, state modifications, and interactive debugging sessions while maintaining data consistency and user experience quality.

### Design Goals

- **Real-time Synchronization**: Instant propagation of changes across all connected users
- **Conflict Resolution**: Automatic and manual conflict resolution mechanisms
- **User Awareness**: Show who is doing what in real-time
- **Session Management**: Robust session handling with reconnection capabilities
- **Scalable Architecture**: Support for multiple concurrent collaboration sessions
- **Educational Focus**: Features tailored for learning and mentoring scenarios

### Key Features

- Multi-user code editing with operational transforms
- Real-time cursor tracking and user presence indicators
- Collaborative debugging with shared breakpoints
- Voice/video integration for remote pair programming
- Session recording and playback capabilities
- Mentoring mode with instructor privileges
- Conflict-free collaborative data structures (CRDTs)
- Offline support with synchronization on reconnection

---

## System Architecture

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                   Collaboration Layer                           │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │   Session   │  │    User     │  │ Real-time   │  │ Conflict│ │
│  │  Manager    │  │ Management  │  │    Sync     │  │ Resolver│ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │ Operational │  │    Event    │  │ Presence    │  │ Permission│ │
│  │ Transform   │  │   System    │  │  Tracker    │  │  System  │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │
│  │ WebSocket   │  │    CRDT     │  │  Recording  │  │ Metrics │ │
│  │  Handler    │  │   Engine    │  │   System    │  │Collector│ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

### Data Flow

```
User Action → Client → WebSocket → Session Manager → Operational Transform
                                          ↓
                                   Conflict Resolution
                                          ↓
                                   State Update → CRDT
                                          ↓
                                   Broadcast to Users
                                          ↓
                                   Client Updates → UI Refresh
```

---

## Core Components

### 1. Session Manager

**Purpose**: Manages collaboration sessions, user connections, and session lifecycle.

**Key Features**:
- Session creation and termination
- User join/leave handling
- Session state persistence
- Automatic cleanup of idle sessions
- Session recording and playback

**Implementation**:

```elixir
defmodule OtpSupervisor.Collaboration.SessionManager do
  @moduledoc """
  Manages collaboration sessions for sandbox environments.
  """

  use GenServer
  require Logger

  @session_timeout 3_600_000  # 1 hour
  @cleanup_interval 300_000   # 5 minutes

  defstruct [
    :session_id,
    :sandbox_id,
    :owner_id,
    :participants,
    :created_at,
    :last_activity,
    :session_state,
    :permissions,
    :recording_enabled
  ]

  def start_link(session_id, sandbox_id, owner_id, options \\ []) do
    GenServer.start_link(__MODULE__, {session_id, sandbox_id, owner_id, options}, 
                        name: via_tuple(session_id))
  end

  def join_session(session_id, user_id, user_info) do
    GenServer.call(via_tuple(session_id), {:join_session, user_id, user_info})
  end

  def leave_session(session_id, user_id) do
    GenServer.call(via_tuple(session_id), {:leave_session, user_id})
  end

  def broadcast_event(session_id, event, exclude_user \\ nil) do
    GenServer.cast(via_tuple(session_id), {:broadcast_event, event, exclude_user})
  end

  def get_session_state(session_id) do
    GenServer.call(via_tuple(session_id), :get_session_state)
  end

  def init({session_id, sandbox_id, owner_id, options}) do
    state = %__MODULE__{
      session_id: session_id,
      sandbox_id: sandbox_id,
      owner_id: owner_id,
      participants: %{},
      created_at: System.monotonic_time(),
      last_activity: System.monotonic_time(),
      session_state: %{},
      permissions: get_default_permissions(options),
      recording_enabled: Keyword.get(options, :recording, false)
    }

    # Schedule cleanup check
    Process.send_after(self(), :cleanup_check, @cleanup_interval)

    # Initialize session recording if enabled
    if state.recording_enabled do
      start_session_recording(session_id)
    end

    {:ok, state}
  end

  def handle_call({:join_session, user_id, user_info}, _from, state) do
    case validate_user_can_join(user_id, state) do
      :ok ->
        new_participant = %{
          user_id: user_id,
          user_info: user_info,
          joined_at: System.monotonic_time(),
          cursor_position: nil,
          active_file: nil,
          permissions: get_user_permissions(user_id, state)
        }

        new_participants = Map.put(state.participants, user_id, new_participant)
        new_state = %{state | 
          participants: new_participants,
          last_activity: System.monotonic_time()
        }

        # Broadcast user joined event
        broadcast_to_participants(new_state, 
          %{type: :user_joined, user: new_participant}, 
          user_id)

        {:reply, {:ok, session_info(new_state)}, new_state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:leave_session, user_id}, _from, state) do
    case Map.get(state.participants, user_id) do
      nil ->
        {:reply, {:error, :not_in_session}, state}

      _participant ->
        new_participants = Map.delete(state.participants, user_id)
        new_state = %{state | 
          participants: new_participants,
          last_activity: System.monotonic_time()
        }

        # Broadcast user left event
        broadcast_to_participants(new_state, 
          %{type: :user_left, user_id: user_id})

        # Check if session should be terminated
        if should_terminate_session?(new_state) do
          {:stop, :normal, :ok, new_state}
        else
          {:reply, :ok, new_state}
        end
    end
  end

  def handle_call(:get_session_state, _from, state) do
    {:reply, session_info(state), state}
  end

  def handle_cast({:broadcast_event, event, exclude_user}, state) do
    broadcast_to_participants(state, event, exclude_user)
    
    # Record event if recording is enabled
    if state.recording_enabled do
      record_session_event(state.session_id, event)
    end

    new_state = %{state | last_activity: System.monotonic_time()}
    {:noreply, new_state}
  end

  def handle_info(:cleanup_check, state) do
    time_since_activity = System.monotonic_time() - state.last_activity
    
    if time_since_activity > @session_timeout do
      Logger.info("Session #{state.session_id} timed out due to inactivity")
      {:stop, :normal, state}
    else
      # Schedule next cleanup check
      Process.send_after(self(), :cleanup_check, @cleanup_interval)
      {:noreply, state}
    end
  end

  defp validate_user_can_join(user_id, state) do
    cond do
      Map.has_key?(state.participants, user_id) ->
        {:error, :already_in_session}
      
      map_size(state.participants) >= max_participants(state) ->
        {:error, :session_full}
      
      not user_has_access?(user_id, state) ->
        {:error, :access_denied}
      
      true ->
        :ok
    end
  end

  defp broadcast_to_participants(state, event, exclude_user \\ nil) do
    state.participants
    |> Map.keys()
    |> Enum.reject(&(&1 == exclude_user))
    |> Enum.each(fn user_id ->
      send_to_user(user_id, event)
    end)
  end

  defp send_to_user(user_id, event) do
    case Registry.lookup(OtpSupervisor.Collaboration.UserRegistry, user_id) do
      [{pid, _}] ->
        send(pid, {:collaboration_event, event})
      [] ->
        Logger.warning("User #{user_id} not found in registry")
    end
  end

  defp session_info(state) do
    %{
      session_id: state.session_id,
      sandbox_id: state.sandbox_id,
      owner_id: state.owner_id,
      participants: state.participants,
      created_at: state.created_at,
      permissions: state.permissions,
      recording_enabled: state.recording_enabled
    }
  end

  defp should_terminate_session?(state) do
    # Terminate if no participants remain
    map_size(state.participants) == 0
  end

  defp max_participants(state) do
    Map.get(state.permissions, :max_participants, 10)
  end

  defp user_has_access?(user_id, state) do
    # Check if user has permission to join the session
    # This would integrate with the permission system
    true  # Placeholder
  end

  defp get_default_permissions(options) do
    %{
      max_participants: Keyword.get(options, :max_participants, 10),
      allow_code_editing: Keyword.get(options, :allow_code_editing, true),
      allow_state_changes: Keyword.get(options, :allow_state_changes, true),
      require_approval: Keyword.get(options, :require_approval, false)
    }
  end

  defp get_user_permissions(user_id, state) do
    # Get user-specific permissions
    # This would integrate with the permission system
    %{
      can_edit_code: true,
      can_modify_state: true,
      can_debug: true
    }
  end

  defp start_session_recording(session_id) do
    OtpSupervisor.Collaboration.RecordingSystem.start_recording(session_id)
  end

  defp record_session_event(session_id, event) do
    OtpSupervisor.Collaboration.RecordingSystem.record_event(session_id, event)
  end

  defp via_tuple(session_id) do
    {:via, Registry, {OtpSupervisor.Collaboration.SessionRegistry, session_id}}
  end
end
```

### 2. Real-time Synchronization Engine

**Purpose**: Handles real-time synchronization of changes across all connected users.

**Key Features**:
- WebSocket-based real-time communication
- Operational transform for concurrent edits
- Conflict-free replicated data types (CRDTs)
- Efficient delta synchronization
- Presence awareness and cursor tracking

**Implementation**:

```elixir
defmodule OtpSupervisor.Collaboration.SyncEngine do
  @moduledoc """
  Handles real-time synchronization of changes across collaboration sessions.
  """

  use GenServer
  require Logger

  defstruct [
    :session_id,
    :document_state,
    :operation_log,
    :vector_clock,
    :pending_operations,
    :user_cursors
  ]

  def start_link(session_id) do
    GenServer.start_link(__MODULE__, session_id, name: via_tuple(session_id))
  end

  def apply_operation(session_id, user_id, operation) do
    GenServer.call(via_tuple(session_id), {:apply_operation, user_id, operation})
  end

  def get_document_state(session_id) do
    GenServer.call(via_tuple(session_id), :get_document_state)
  end

  def update_cursor(session_id, user_id, cursor_position) do
    GenServer.cast(via_tuple(session_id), {:update_cursor, user_id, cursor_position})
  end

  def init(session_id) do
    state = %__MODULE__{
      session_id: session_id,
      document_state: %{},
      operation_log: [],
      vector_clock: %{},
      pending_operations: [],
      user_cursors: %{}
    }

    {:ok, state}
  end

  def handle_call({:apply_operation, user_id, operation}, _from, state) do
    case process_operation(state, user_id, operation) do
      {:ok, new_state, transformed_operation} ->
        # Broadcast the transformed operation to other users
        broadcast_operation(state.session_id, user_id, transformed_operation)
        
        {:reply, {:ok, transformed_operation}, new_state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(:get_document_state, _from, state) do
    {:reply, state.document_state, state}
  end

  def handle_cast({:update_cursor, user_id, cursor_position}, state) do
    new_cursors = Map.put(state.user_cursors, user_id, cursor_position)
    new_state = %{state | user_cursors: new_cursors}
    
    # Broadcast cursor update
    broadcast_cursor_update(state.session_id, user_id, cursor_position)
    
    {:noreply, new_state}
  end

  defp process_operation(state, user_id, operation) do
    # Apply operational transform
    case transform_operation(state, user_id, operation) do
      {:ok, transformed_op} ->
        # Apply to document state
        new_document_state = apply_to_document(state.document_state, transformed_op)
        
        # Update vector clock
        new_vector_clock = increment_vector_clock(state.vector_clock, user_id)
        
        # Add to operation log
        new_operation_log = [
          {user_id, transformed_op, new_vector_clock} | state.operation_log
        ]
        
        new_state = %{state |
          document_state: new_document_state,
          vector_clock: new_vector_clock,
          operation_log: new_operation_log
        }
        
        {:ok, new_state, transformed_op}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp transform_operation(state, user_id, operation) do
    # Get concurrent operations from other users
    concurrent_ops = get_concurrent_operations(state, user_id, operation)
    
    # Apply operational transform
    case OtpSupervisor.Collaboration.OperationalTransform.transform(
      operation, concurrent_ops
    ) do
      {:ok, transformed_op} -> {:ok, transformed_op}
      {:error, reason} -> {:error, reason}
    end
  end

  defp apply_to_document(document_state, operation) do
    case operation.type do
      :insert ->
        insert_text(document_state, operation.position, operation.content)
      
      :delete ->
        delete_text(document_state, operation.position, operation.length)
      
      :update_field ->
        update_field(document_state, operation.field, operation.value)
      
      :add_breakpoint ->
        add_breakpoint(document_state, operation.file, operation.line)
      
      :remove_breakpoint ->
        remove_breakpoint(document_state, operation.file, operation.line)
    end
  end

  defp insert_text(document_state, position, content) do
    file = Map.get(document_state, position.file, "")
    
    {before, after} = String.split_at(file, position.offset)
    new_content = before <> content <> after
    
    Map.put(document_state, position.file, new_content)
  end

  defp delete_text(document_state, position, length) do
    file = Map.get(document_state, position.file, "")
    
    {before, rest} = String.split_at(file, position.offset)
    {_deleted, after} = String.split_at(rest, length)
    new_content = before <> after
    
    Map.put(document_state, position.file, new_content)
  end

  defp update_field(document_state, field, value) do
    Map.put(document_state, field, value)
  end

  defp add_breakpoint(document_state, file, line) do
    breakpoints = Map.get(document_state, :breakpoints, %{})
    file_breakpoints = Map.get(breakpoints, file, MapSet.new())
    
    new_file_breakpoints = MapSet.put(file_breakpoints, line)
    new_breakpoints = Map.put(breakpoints, file, new_file_breakpoints)
    
    Map.put(document_state, :breakpoints, new_breakpoints)
  end

  defp remove_breakpoint(document_state, file, line) do
    breakpoints = Map.get(document_state, :breakpoints, %{})
    file_breakpoints = Map.get(breakpoints, file, MapSet.new())
    
    new_file_breakpoints = MapSet.delete(file_breakpoints, line)
    new_breakpoints = Map.put(breakpoints, file, new_file_breakpoints)
    
    Map.put(document_state, :breakpoints, new_breakpoints)
  end

  defp get_concurrent_operations(state, user_id, operation) do
    # Get operations that happened concurrently with this operation
    user_timestamp = Map.get(state.vector_clock, user_id, 0)
    
    state.operation_log
    |> Enum.filter(fn {op_user_id, _op, op_clock} ->
      op_user_id != user_id and 
      not vector_clock_happened_before?(op_clock, user_timestamp, user_id)
    end)
    |> Enum.map(fn {_user_id, op, _clock} -> op end)
  end

  defp vector_clock_happened_before?(clock1, timestamp2, user_id) do
    Map.get(clock1, user_id, 0) <= timestamp2
  end

  defp increment_vector_clock(clock, user_id) do
    Map.update(clock, user_id, 1, &(&1 + 1))
  end

  defp broadcast_operation(session_id, user_id, operation) do
    OtpSupervisor.Collaboration.SessionManager.broadcast_event(
      session_id,
      %{type: :operation, user_id: user_id, operation: operation},
      user_id
    )
  end

  defp broadcast_cursor_update(session_id, user_id, cursor_position) do
    OtpSupervisor.Collaboration.SessionManager.broadcast_event(
      session_id,
      %{type: :cursor_update, user_id: user_id, cursor: cursor_position},
      user_id
    )
  end

  defp via_tuple(session_id) do
    {:via, Registry, {OtpSupervisor.Collaboration.SyncRegistry, session_id}}
  end
end
```

### 3. Operational Transform Engine

**Purpose**: Handles transformation of concurrent operations to maintain consistency.

**Key Features**:
- Text-based operational transforms
- Structured data transforms
- Conflict detection and resolution
- Undo/redo support
- Composable operations

**Implementation**:

```elixir
defmodule OtpSupervisor.Collaboration.OperationalTransform do
  @moduledoc """
  Implements operational transform algorithms for collaborative editing.
  """

  @doc """
  Transform an operation against a list of concurrent operations.
  """
  def transform(operation, concurrent_operations) do
    Enum.reduce_while(concurrent_operations, {:ok, operation}, fn concurrent_op, {:ok, op} ->
      case transform_pair(op, concurrent_op) do
        {:ok, transformed_op} -> {:cont, {:ok, transformed_op}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  @doc """
  Transform a pair of operations.
  """
  def transform_pair(op1, op2) do
    case {op1.type, op2.type} do
      {:insert, :insert} -> transform_insert_insert(op1, op2)
      {:insert, :delete} -> transform_insert_delete(op1, op2)
      {:delete, :insert} -> transform_delete_insert(op1, op2)
      {:delete, :delete} -> transform_delete_delete(op1, op2)
      {:update_field, :update_field} -> transform_update_update(op1, op2)
      _ -> {:ok, op1}  # Default: no transformation needed
    end
  end

  defp transform_insert_insert(op1, op2) do
    cond do
      op1.position.file != op2.position.file ->
        {:ok, op1}  # Different files, no conflict
      
      op1.position.offset < op2.position.offset ->
        {:ok, op1}  # op1 comes before op2
      
      op1.position.offset > op2.position.offset ->
        # op1 comes after op2, adjust position
        new_position = %{op1.position | 
          offset: op1.position.offset + String.length(op2.content)
        }
        {:ok, %{op1 | position: new_position}}
      
      true ->
        # Same position, use tie-breaking rule (user ID comparison)
        if op1.user_id <= op2.user_id do
          {:ok, op1}
        else
          new_position = %{op1.position | 
            offset: op1.position.offset + String.length(op2.content)
          }
          {:ok, %{op1 | position: new_position}}
        end
    end
  end

  defp transform_insert_delete(op1, op2) do
    cond do
      op1.position.file != op2.position.file ->
        {:ok, op1}  # Different files, no conflict
      
      op1.position.offset <= op2.position.offset ->
        {:ok, op1}  # Insert before delete
      
      op1.position.offset > op2.position.offset + op2.length ->
        # Insert after delete, adjust position
        new_position = %{op1.position | 
          offset: op1.position.offset - op2.length
        }
        {:ok, %{op1 | position: new_position}}
      
      true ->
        # Insert within deleted range
        new_position = %{op1.position | offset: op2.position.offset}
        {:ok, %{op1 | position: new_position}}
    end
  end

  defp transform_delete_insert(op1, op2) do
    cond do
      op1.position.file != op2.position.file ->
        {:ok, op1}  # Different files, no conflict
      
      op2.position.offset <= op1.position.offset ->
        # Insert before delete, adjust delete position
        new_position = %{op1.position | 
          offset: op1.position.offset + String.length(op2.content)
        }
        {:ok, %{op1 | position: new_position}}
      
      op2.position.offset > op1.position.offset + op1.length ->
        {:ok, op1}  # Insert after delete
      
      true ->
        # Insert within delete range, extend delete length
        new_length = op1.length + String.length(op2.content)
        {:ok, %{op1 | length: new_length}}
    end
  end

  defp transform_delete_delete(op1, op2) do
    cond do
      op1.position.file != op2.position.file ->
        {:ok, op1}  # Different files, no conflict
      
      op1.position.offset + op1.length <= op2.position.offset ->
        {:ok, op1}  # op1 completely before op2
      
      op2.position.offset + op2.length <= op1.position.offset ->
        # op2 completely before op1, adjust op1 position
        new_position = %{op1.position | 
          offset: op1.position.offset - op2.length
        }
        {:ok, %{op1 | position: new_position}}
      
      true ->
        # Overlapping deletes, compute the merged delete
        merge_overlapping_deletes(op1, op2)
    end
  end

  defp transform_update_update(op1, op2) do
    cond do
      op1.field != op2.field ->
        {:ok, op1}  # Different fields, no conflict
      
      op1.timestamp > op2.timestamp ->
        {:ok, op1}  # op1 is newer
      
      op1.timestamp < op2.timestamp ->
        {:error, :superseded}  # op1 is older, discard it
      
      true ->
        # Same timestamp, use tie-breaking rule
        if op1.user_id <= op2.user_id do
          {:ok, op1}
        else
          {:error, :superseded}
        end
    end
  end

  defp merge_overlapping_deletes(op1, op2) do
    start_offset = min(op1.position.offset, op2.position.offset)
    end_offset = max(
      op1.position.offset + op1.length,
      op2.position.offset + op2.length
    )
    
    new_position = %{op1.position | offset: start_offset}
    new_length = end_offset - start_offset
    
    {:ok, %{op1 | position: new_position, length: new_length}}
  end

  @doc """
  Compose a sequence of operations into a single operation.
  """
  def compose_operations(operations) do
    case operations do
      [] -> {:ok, nil}
      [single_op] -> {:ok, single_op}
      [first | rest] ->
        Enum.reduce_while(rest, {:ok, first}, fn op, {:ok, composed} ->
          case compose_pair(composed, op) do
            {:ok, new_composed} -> {:cont, {:ok, new_composed}}
            {:error, reason} -> {:halt, {:error, reason}}
          end
        end)
    end
  end

  defp compose_pair(op1, op2) do
    case {op1.type, op2.type} do
      {:insert, :insert} -> compose_insert_insert(op1, op2)
      {:insert, :delete} -> compose_insert_delete(op1, op2)
      {:delete, :insert} -> compose_delete_insert(op1, op2)
      {:delete, :delete} -> compose_delete_delete(op1, op2)
      _ -> {:error, :incompatible_operations}
    end
  end

  defp compose_insert_insert(op1, op2) do
    if op1.position.file == op2.position.file and
       op1.position.offset + String.length(op1.content) == op2.position.offset do
      # Adjacent inserts, merge them
      new_content = op1.content <> op2.content
      {:ok, %{op1 | content: new_content}}
    else
      {:error, :non_adjacent_inserts}
    end
  end

  defp compose_insert_delete(op1, op2) do
    if op1.position.file == op2.position.file and
       op1.position.offset == op2.position.offset and
       String.length(op1.content) == op2.length do
      # Insert followed by delete of same content = no-op
      {:ok, :no_op}
    else
      {:error, :incompatible_operations}
    end
  end

  defp compose_delete_insert(op1, op2) do
    if op1.position.file == op2.position.file and
       op1.position.offset == op2.position.offset then
      # Delete followed by insert at same position = replace
      {:ok, %{
        type: :replace,
        position: op1.position,
        delete_length: op1.length,
        insert_content: op2.content
      }}
    else
      {:error, :incompatible_operations}
    end
  end

  defp compose_delete_delete(op1, op2) do
    if op1.position.file == op2.position.file and
       op1.position.offset == op2.position.offset then
      # Adjacent deletes, merge them
      new_length = op1.length + op2.length
      {:ok, %{op1 | length: new_length}}
    else
      {:error, :incompatible_operations}
    end
  end

  @doc """
  Invert an operation to create an undo operation.
  """
  def invert_operation(operation, document_state) do
    case operation.type do
      :insert ->
        {:ok, %{
          type: :delete,
          position: operation.position,
          length: String.length(operation.content),
          user_id: operation.user_id,
          timestamp: System.monotonic_time()
        }}
      
      :delete ->
        # Need to get the deleted content from document state
        case get_deleted_content(document_state, operation) do
          {:ok, content} ->
            {:ok, %{
              type: :insert,
              position: operation.position,
              content: content,
              user_id: operation.user_id,
              timestamp: System.monotonic_time()
            }}
          
          {:error, reason} ->
            {:error, reason}
        end
      
      :update_field ->
        case get_previous_value(document_state, operation.field) do
          {:ok, previous_value} ->
            {:ok, %{
              type: :update_field,
              field: operation.field,
              value: previous_value,
              user_id: operation.user_id,
              timestamp: System.monotonic_time()
            }}
          
          {:error, reason} ->
            {:error, reason}
        end
      
      _ ->
        {:error, :cannot_invert}
    end
  end

  defp get_deleted_content(document_state, delete_operation) do
    file_content = Map.get(document_state, delete_operation.position.file, "")
    
    if String.length(file_content) >= delete_operation.position.offset + delete_operation.length do
      content = String.slice(file_content, 
        delete_operation.position.offset, 
        delete_operation.length)
      {:ok, content}
    else
      {:error, :content_not_found}
    end
  end

  defp get_previous_value(document_state, field) do
    case Map.get(document_state, field) do
      nil -> {:error, :field_not_found}
      value -> {:ok, value}
    end
  end
end
```

### 4. Presence and Awareness System

**Purpose**: Tracks user presence and provides awareness of what other users are doing.

**Key Features**:
- Real-time cursor tracking
- User activity indicators
- File focus tracking
- Voice/video presence integration
- Idle detection and auto-away status

**Implementation**:

```elixir
defmodule OtpSupervisor.Collaboration.PresenceTracker do
  @moduledoc """
  Tracks user presence and activity in collaboration sessions.
  """

  use GenServer
  require Logger

  @idle_timeout 300_000  # 5 minutes
  @presence_update_interval 1000  # 1 second

  defstruct [
    :session_id,
    :user_presences,
    :last_activity
  ]

  def start_link(session_id) do
    GenServer.start_link(__MODULE__, session_id, name: via_tuple(session_id))
  end

  def update_presence(session_id, user_id, presence_data) do
    GenServer.cast(via_tuple(session_id), {:update_presence, user_id, presence_data})
  end

  def get_all_presences(session_id) do
    GenServer.call(via_tuple(session_id), :get_all_presences)
  end

  def user_activity(session_id, user_id, activity_type) do
    GenServer.cast(via_tuple(session_id), {:user_activity, user_id, activity_type})
  end

  def init(session_id) do
    state = %__MODULE__{
      session_id: session_id,
      user_presences: %{},
      last_activity: %{}
    }

    # Schedule presence updates
    Process.send_after(self(), :update_presence_status, @presence_update_interval)

    {:ok, state}
  end

  def handle_cast({:update_presence, user_id, presence_data}, state) do
    current_time = System.monotonic_time()
    
    new_presence = Map.merge(
      Map.get(state.user_presences, user_id, %{}),
      presence_data
    )
    |> Map.put(:last_seen, current_time)
    |> Map.put(:status, determine_status(presence_data))

    new_presences = Map.put(state.user_presences, user_id, new_presence)
    new_last_activity = Map.put(state.last_activity, user_id, current_time)

    new_state = %{state | 
      user_presences: new_presences,
      last_activity: new_last_activity
    }

    # Broadcast presence update
    broadcast_presence_update(state.session_id, user_id, new_presence)

    {:noreply, new_state}
  end

  def handle_cast({:user_activity, user_id, activity_type}, state) do
    current_time = System.monotonic_time()
    
    # Update last activity
    new_last_activity = Map.put(state.last_activity, user_id, current_time)
    
    # Update presence with activity info
    current_presence = Map.get(state.user_presences, user_id, %{})
    new_presence = Map.merge(current_presence, %{
      last_activity: activity_type,
      last_seen: current_time,
      status: :active
    })
    
    new_presences = Map.put(state.user_presences, user_id, new_presence)
    
    new_state = %{state | 
      user_presences: new_presences,
      last_activity: new_last_activity
    }

    # Broadcast activity update
    broadcast_activity_update(state.session_id, user_id, activity_type)

    {:noreply, new_state}
  end

  def handle_call(:get_all_presences, _from, state) do
    {:reply, state.user_presences, state}
  end

  def handle_info(:update_presence_status, state) do
    current_time = System.monotonic_time()
    
    # Check for idle users
    new_presences = 
      state.user_presences
      |> Enum.map(fn {user_id, presence} ->
        last_activity = Map.get(state.last_activity, user_id, 0)
        time_idle = current_time - last_activity
        
        new_status = cond do
          time_idle > @idle_timeout -> :away
          time_idle > @idle_timeout / 2 -> :idle
          true -> :active
        end
        
        new_presence = Map.put(presence, :status, new_status)
        {user_id, new_presence}
      end)
      |> Map.new()

    new_state = %{state | user_presences: new_presences}

    # Broadcast presence updates for status changes
    broadcast_presence_summary(state.session_id, new_presences)

    # Schedule next update
    Process.send_after(self(), :update_presence_status, @presence_update_interval)

    {:noreply, new_state}
  end

  defp determine_status(presence_data) do
    cond do
      Map.get(presence_data, :typing, false) -> :typing
      Map.get(presence_data, :cursor_position) -> :active
      Map.get(presence_data, :voice_active, false) -> :speaking
      true -> :active
    end
  end

  defp broadcast_presence_update(session_id, user_id, presence) do
    OtpSupervisor.Collaboration.SessionManager.broadcast_event(
      session_id,
      %{type: :presence_update, user_id: user_id, presence: presence}
    )
  end

  defp broadcast_activity_update(session_id, user_id, activity_type) do
    OtpSupervisor.Collaboration.SessionManager.broadcast_event(
      session_id,
      %{type: :activity_update, user_id: user_id, activity: activity_type}
    )
  end

  defp broadcast_presence_summary(session_id, presences) do
    OtpSupervisor.Collaboration.SessionManager.broadcast_event(
      session_id,
      %{type: :presence_summary, presences: presences}
    )
  end

  defp via_tuple(session_id) do
    {:via, Registry, {OtpSupervisor.Collaboration.PresenceRegistry, session_id}}
  end
end
```

---

## Real-time Communication

### WebSocket Handler

```elixir
defmodule OtpSupervisorWeb.Collaboration.WebSocketHandler do
  @moduledoc """
  WebSocket handler for real-time collaboration features.
  """

  require Logger

  def websocket_init(state) do
    Logger.info("WebSocket connection initialized")
    {:ok, state}
  end

  def websocket_handle({:text, message}, state) do
    case Jason.decode(message) do
      {:ok, data} ->
        handle_message(data, state)
      
      {:error, reason} ->
        Logger.error("Invalid JSON message: #{reason}")
        {:reply, {:text, Jason.encode!(%{error: "Invalid JSON"})}, state}
    end
  end

  def websocket_handle({:binary, _data}, state) do
    {:reply, {:text, Jason.encode!(%{error: "Binary messages not supported"})}, state}
  end

  def websocket_info({:collaboration_event, event}, state) do
    message = Jason.encode!(event)
    {:reply, {:text, message}, state}
  end

  def websocket_info(info, state) do
    Logger.debug("Unhandled websocket info: #{inspect(info)}")
    {:ok, state}
  end

  defp handle_message(%{"type" => "join_session", "session_id" => session_id, "user_info" => user_info}, state) do
    user_id = Map.get(state, :user_id, "anonymous")
    
    case OtpSupervisor.Collaboration.SessionManager.join_session(session_id, user_id, user_info) do
      {:ok, session_info} ->
        # Register for collaboration events
        Registry.register(OtpSupervisor.Collaboration.UserRegistry, user_id, self())
        
        new_state = Map.merge(state, %{
          session_id: session_id,
          user_id: user_id,
          joined: true
        })
        
        response = %{
          type: "session_joined",
          session_info: session_info
        }
        
        {:reply, {:text, Jason.encode!(response)}, new_state}
      
      {:error, reason} ->
        response = %{
          type: "error",
          message: "Failed to join session: #{reason}"
        }
        
        {:reply, {:text, Jason.encode!(response)}, state}
    end
  end

  defp handle_message(%{"type" => "leave_session"}, state) do
    if Map.get(state, :joined, false) do
      session_id = Map.get(state, :session_id)
      user_id = Map.get(state, :user_id)
      
      OtpSupervisor.Collaboration.SessionManager.leave_session(session_id, user_id)
      
      # Unregister from collaboration events
      Registry.unregister(OtpSupervisor.Collaboration.UserRegistry, user_id)
      
      new_state = Map.drop(state, [:session_id, :user_id, :joined])
      
      response = %{type: "session_left"}
      {:reply, {:text, Jason.encode!(response)}, new_state}
    else
      {:ok, state}
    end
  end

  defp handle_message(%{"type" => "operation", "operation" => operation}, state) do
    if Map.get(state, :joined, false) do
      session_id = Map.get(state, :session_id)
      user_id = Map.get(state, :user_id)
      
      case OtpSupervisor.Collaboration.SyncEngine.apply_operation(session_id, user_id, operation) do
        {:ok, transformed_op} ->
          response = %{
            type: "operation_applied",
            operation: transformed_op
          }
          
          {:reply, {:text, Jason.encode!(response)}, state}
        
        {:error, reason} ->
          response = %{
            type: "operation_error",
            message: "Failed to apply operation: #{reason}"
          }
          
          {:reply, {:text, Jason.encode!(response)}, state}
      end
    else
      {:ok, state}
    end
  end

  defp handle_message(%{"type" => "cursor_update", "cursor" => cursor}, state) do
    if Map.get(state, :joined, false) do
      session_id = Map.get(state, :session_id)
      user_id = Map.get(state, :user_id)
      
      OtpSupervisor.Collaboration.SyncEngine.update_cursor(session_id, user_id, cursor)
      OtpSupervisor.Collaboration.PresenceTracker.update_presence(
        session_id, user_id, %{cursor_position: cursor}
      )
      
      {:ok, state}
    else
      {:ok, state}
    end
  end

  defp handle_message(%{"type" => "presence_update", "presence" => presence}, state) do
    if Map.get(state, :joined, false) do
      session_id = Map.get(state, :session_id)
      user_id = Map.get(state, :user_id)
      
      OtpSupervisor.Collaboration.PresenceTracker.update_presence(session_id, user_id, presence)
      
      {:ok, state}
    else
      {:ok, state}
    end
  end

  defp handle_message(%{"type" => unknown_type}, state) do
    Logger.warning("Unknown message type: #{unknown_type}")
    
    response = %{
      type: "error",
      message: "Unknown message type: #{unknown_type}"
    }
    
    {:reply, {:text, Jason.encode!(response)}, state}
  end
end
```

---

## Session Management

### Session Persistence

```elixir
defmodule OtpSupervisor.Collaboration.SessionPersistence do
  @moduledoc """
  Handles persistence of collaboration sessions for recovery and analytics.
  """

  use GenServer
  require Logger

  @table_name :collaboration_sessions
  @cleanup_interval 3_600_000  # 1 hour

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def save_session(session_id, session_data) do
    GenServer.cast(__MODULE__, {:save_session, session_id, session_data})
  end

  def load_session(session_id) do
    GenServer.call(__MODULE__, {:load_session, session_id})
  end

  def delete_session(session_id) do
    GenServer.cast(__MODULE__, {:delete_session, session_id})
  end

  def list_active_sessions do
    GenServer.call(__MODULE__, :list_active_sessions)
  end

  def init([]) do
    # Create ETS table for session storage
    :ets.new(@table_name, [:named_table, :public, :set])
    
    # Schedule cleanup
    Process.send_after(self(), :cleanup_expired, @cleanup_interval)
    
    {:ok, %{}}
  end

  def handle_cast({:save_session, session_id, session_data}, state) do
    timestamp = System.monotonic_time()
    
    record = {
      session_id,
      session_data,
      timestamp
    }
    
    :ets.insert(@table_name, record)
    
    {:noreply, state}
  end

  def handle_cast({:delete_session, session_id}, state) do
    :ets.delete(@table_name, session_id)
    {:noreply, state}
  end

  def handle_call({:load_session, session_id}, _from, state) do
    case :ets.lookup(@table_name, session_id) do
      [{^session_id, session_data, _timestamp}] ->
        {:reply, {:ok, session_data}, state}
      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call(:list_active_sessions, _from, state) do
    sessions = 
      :ets.tab2list(@table_name)
      |> Enum.map(fn {session_id, session_data, timestamp} ->
        %{
          session_id: session_id,
          session_data: session_data,
          created_at: timestamp
        }
      end)
    
    {:reply, sessions, state}
  end

  def handle_info(:cleanup_expired, state) do
    current_time = System.monotonic_time()
    expiry_time = current_time - (24 * 60 * 60 * 1000)  # 24 hours ago
    
    expired_sessions = 
      :ets.tab2list(@table_name)
      |> Enum.filter(fn {_session_id, _data, timestamp} ->
        timestamp < expiry_time
      end)
    
    Enum.each(expired_sessions, fn {session_id, _data, _timestamp} ->
      :ets.delete(@table_name, session_id)
    end)
    
    if length(expired_sessions) > 0 do
      Logger.info("Cleaned up #{length(expired_sessions)} expired collaboration sessions")
    end
    
    # Schedule next cleanup
    Process.send_after(self(), :cleanup_expired, @cleanup_interval)
    
    {:noreply, state}
  end
end
```

---

## Testing Strategy

### Unit Tests

```elixir
defmodule OtpSupervisor.Collaboration.SessionManagerTest do
  use ExUnit.Case, async: true
  alias OtpSupervisor.Collaboration.SessionManager

  describe "session lifecycle" do
    test "creates session successfully" do
      session_id = "test_session_#{System.unique_integer([:positive])}"
      sandbox_id = "test_sandbox"
      owner_id = "test_owner"
      
      {:ok, _pid} = SessionManager.start_link(session_id, sandbox_id, owner_id)
      
      assert {:ok, session_info} = SessionManager.get_session_state(session_id)
      assert session_info.session_id == session_id
      assert session_info.sandbox_id == sandbox_id
      assert session_info.owner_id == owner_id
    end

    test "handles user join/leave" do
      session_id = "test_session_#{System.unique_integer([:positive])}"
      
      {:ok, _pid} = SessionManager.start_link(session_id, "sandbox", "owner")
      
      # User joins
      user_info = %{name: "Test User", email: "test@example.com"}
      {:ok, _session_info} = SessionManager.join_session(session_id, "user1", user_info)
      
      # Verify user is in session
      {:ok, session_info} = SessionManager.get_session_state(session_id)
      assert Map.has_key?(session_info.participants, "user1")
      
      # User leaves
      :ok = SessionManager.leave_session(session_id, "user1")
      
      # Verify user is no longer in session
      {:ok, session_info} = SessionManager.get_session_state(session_id)
      refute Map.has_key?(session_info.participants, "user1")
    end
  end

  describe "error handling" do
    test "prevents duplicate user joins" do
      session_id = "test_session_#{System.unique_integer([:positive])}"
      
      {:ok, _pid} = SessionManager.start_link(session_id, "sandbox", "owner")
      
      user_info = %{name: "Test User"}
      
      # First join succeeds
      {:ok, _} = SessionManager.join_session(session_id, "user1", user_info)
      
      # Second join fails
      {:error, :already_in_session} = SessionManager.join_session(session_id, "user1", user_info)
    end
  end
end
```

### Integration Tests

```elixir
defmodule OtpSupervisor.Collaboration.IntegrationTest do
  use ExUnit.Case, async: false
  alias OtpSupervisor.Collaboration.{SessionManager, SyncEngine, PresenceTracker}

  describe "collaborative editing" do
    test "concurrent text edits are synchronized" do
      session_id = "collab_test_#{System.unique_integer([:positive])}"
      
      # Start collaboration components
      {:ok, _session_pid} = SessionManager.start_link(session_id, "sandbox", "owner")
      {:ok, _sync_pid} = SyncEngine.start_link(session_id)
      {:ok, _presence_pid} = PresenceTracker.start_link(session_id)
      
      # Two users join
      {:ok, _} = SessionManager.join_session(session_id, "user1", %{name: "User 1"})
      {:ok, _} = SessionManager.join_session(session_id, "user2", %{name: "User 2"})
      
      # User 1 inserts text
      op1 = %{
        type: :insert,
        position: %{file: "test.ex", offset: 0},
        content: "Hello ",
        user_id: "user1",
        timestamp: System.monotonic_time()
      }
      
      {:ok, _} = SyncEngine.apply_operation(session_id, "user1", op1)
      
      # User 2 inserts text at same position
      op2 = %{
        type: :insert,
        position: %{file: "test.ex", offset: 0},
        content: "World",
        user_id: "user2",
        timestamp: System.monotonic_time()
      }
      
      {:ok, _} = SyncEngine.apply_operation(session_id, "user2", op2)
      
      # Check final document state
      {:ok, document_state} = SyncEngine.get_document_state(session_id)
      content = Map.get(document_state, "test.ex", "")
      
      # Should be either "Hello World" or "WorldHello " depending on transform
      assert String.contains?(content, "Hello")
      assert String.contains?(content, "World")
    end
  end

  describe "presence tracking" do
    test "tracks user activity and presence" do
      session_id = "presence_test_#{System.unique_integer([:positive])}"
      
      {:ok, _session_pid} = SessionManager.start_link(session_id, "sandbox", "owner")
      {:ok, _presence_pid} = PresenceTracker.start_link(session_id)
      
      # User joins
      {:ok, _} = SessionManager.join_session(session_id, "user1", %{name: "User 1"})
      
      # Update presence
      PresenceTracker.update_presence(session_id, "user1", %{
        cursor_position: %{file: "test.ex", line: 10, column: 5},
        typing: true
      })
      
      # Check presence
      presences = PresenceTracker.get_all_presences(session_id)
      user_presence = Map.get(presences, "user1")
      
      assert user_presence.cursor_position.file == "test.ex"
      assert user_presence.cursor_position.line == 10
      assert user_presence.status == :typing
    end
  end
end
```

---

## Performance Considerations

### Optimization Strategies

```elixir
defmodule OtpSupervisor.Collaboration.PerformanceOptimizer do
  @moduledoc """
  Optimizes collaboration system performance.
  """

  # Batch operations to reduce WebSocket traffic
  def batch_operations(operations, batch_size \\ 10) do
    operations
    |> Enum.chunk_every(batch_size)
    |> Enum.map(fn batch ->
      case OtpSupervisor.Collaboration.OperationalTransform.compose_operations(batch) do
        {:ok, composed_op} -> composed_op
        {:error, _} -> batch  # Fall back to individual operations
      end
    end)
  end

  # Compress large messages
  def compress_message(message) when byte_size(message) > 1024 do
    compressed = :zlib.compress(message)
    %{compressed: true, data: compressed}
  end
  def compress_message(message) do
    %{compressed: false, data: message}
  end

  # Throttle cursor updates
  def throttle_cursor_updates(user_id, cursor_position) do
    case get_last_cursor_update(user_id) do
      nil -> 
        store_cursor_update(user_id, cursor_position)
        {:ok, :send}
      
      {last_position, last_time} ->
        current_time = System.monotonic_time()
        
        if current_time - last_time > 100 or  # 100ms throttle
           cursor_moved_significantly?(last_position, cursor_position) do
          store_cursor_update(user_id, cursor_position)
          {:ok, :send}
        else
          {:ok, :throttled}
        end
    end
  end

  defp cursor_moved_significantly?(pos1, pos2) do
    abs(pos1.line - pos2.line) > 1 or abs(pos1.column - pos2.column) > 10
  end

  defp get_last_cursor_update(user_id) do
    case :ets.lookup(:cursor_throttle, user_id) do
      [{^user_id, position, timestamp}] -> {position, timestamp}
      [] -> nil
    end
  end

  defp store_cursor_update(user_id, position) do
    :ets.insert(:cursor_throttle, {user_id, position, System.monotonic_time()})
  end
end
```

---

## Configuration

### System Configuration

```elixir
# config/config.exs
config :otp_supervisor, :collaboration,
  # Session settings
  max_session_duration: 3_600_000,  # 1 hour
  max_participants_per_session: 10,
  session_cleanup_interval: 300_000,  # 5 minutes
  
  # WebSocket settings
  websocket_timeout: 60_000,  # 1 minute
  heartbeat_interval: 30_000,  # 30 seconds
  max_message_size: 10_000,  # 10KB
  
  # Operational transform settings
  operation_buffer_size: 1000,
  transform_timeout: 5_000,  # 5 seconds
  
  # Presence settings
  presence_update_interval: 1000,  # 1 second
  idle_timeout: 300_000,  # 5 minutes
  
  # Recording settings
  recording_enabled: true,
  max_recording_duration: 7_200_000,  # 2 hours
  recording_compression: true,
  
  # Performance settings
  cursor_throttle_interval: 100,  # 100ms
  operation_batch_size: 10,
  message_compression_threshold: 1024
```

---

## Conclusion

This Collaboration System design provides a comprehensive foundation for real-time collaborative development in the OTP sandbox platform. The system emphasizes:

- **Real-time Synchronization**: Immediate propagation of changes across all users
- **Consistency**: Robust conflict resolution and operational transforms
- **User Experience**: Presence awareness and smooth collaborative interactions
- **Scalability**: Efficient handling of multiple concurrent sessions
- **Educational Focus**: Features tailored for learning and mentoring scenarios

The system integrates seamlessly with the existing architecture while providing the collaborative capabilities needed for multi-user development and educational experiences.

Next steps include implementing the remaining technical design documents and beginning development of the collaboration components.