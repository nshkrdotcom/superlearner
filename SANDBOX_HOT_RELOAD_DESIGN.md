# True Hot-Reload Sandbox Design

## Executive Summary

This document outlines the design for implementing true hot-reload isolation in the OTP Supervisor sandbox system. The goal is to enable real-time code compilation and reloading of sandbox applications without affecting the host system or other sandboxes, even when compilation fails or code contains errors.

## Current Architecture Analysis

### Strengths of Current Implementation
- **Application-level isolation**: Each sandbox runs as a complete OTP application
- **Robust process monitoring**: Application master monitoring with cleanup
- **ETS-based management**: Fast lookups and state tracking
- **Proper supervision**: Independent supervision trees per sandbox
- **Resource isolation**: Clean boundaries between sandboxes

### Current Limitations
- **Global code path pollution**: `Code.prepend_path/1` affects entire BEAM VM
- **Compilation coupling**: Path dependency in mix.exs creates single point of failure
- **No hot-reload**: Manual restart required for code changes
- **Limited versioning**: No rollback capability for failed updates
- **Static loading**: Code loaded once at sandbox creation

## True Isolation Requirements

### 1. Fault Isolation
- Compilation failures must not affect host system
- Individual sandbox compilation errors must not affect other sandboxes
- Hot-reload failures must allow graceful rollback
- Memory leaks from failed compiles must be contained

### 2. Performance Isolation
- Compilation should not block other operations
- File watching overhead must be minimal
- Hot-reload should complete in <1 second
- Memory usage must be bounded per sandbox

### 3. Development Experience
- Immediate feedback on code changes
- Preserve process state during reload when possible
- Clear error reporting for compilation failures
- Rollback capability for problematic changes

## Proposed Architecture

### Core Components

#### 1. Isolated Compilation Engine
```elixir
defmodule OTPSupervisor.Core.IsolatedCompiler do
  @moduledoc """
  Handles compilation of sandbox code in complete isolation.
  """
  
  # Compile in separate process with timeout
  def compile_sandbox(sandbox_path, opts \\ [])
  
  # Validate compiled BEAM files before loading
  def validate_compilation(beam_files)
  
  # Generate compilation report with warnings/errors
  def compilation_report(result)
end
```

**Key Features:**
- Compile in separate `:erlang.spawn/1` process with timeout
- Use temporary directories for compilation artifacts
- Validate BEAM files before attempting to load
- Comprehensive error reporting and rollback

#### 2. Module Version Manager
```elixir
defmodule OTPSupervisor.Core.ModuleVersionManager do
  @moduledoc """
  Tracks module versions and handles hot-swapping.
  """
  
  # Track loaded module versions per sandbox
  def register_module_version(sandbox_id, module, version, beam_data)
  
  # Hot-swap module with state preservation
  def hot_swap_module(sandbox_id, module, new_beam_data)
  
  # Rollback to previous version
  def rollback_module(sandbox_id, module, target_version)
  
  # Get dependency graph for reload ordering
  def get_module_dependencies(modules)
end
```

**Key Features:**
- In-memory BEAM storage with versioning
- Dependency graph analysis using AST parsing
- State preservation hooks for GenServers
- Atomic multi-module updates

#### 3. File Watcher System
```elixir
defmodule OTPSupervisor.Core.SandboxFileWatcher do
  @moduledoc """
  Monitors sandbox source files for changes.
  """
  
  # Start watching a sandbox directory
  def watch_sandbox(sandbox_id, sandbox_path)
  
  # Stop watching
  def unwatch_sandbox(sandbox_id)
  
  # Handle file change events
  def handle_file_change(sandbox_id, file_path, event)
end
```

**Key Features:**
- Use `:file_system` library for efficient file watching
- Debounced compilation triggers (300ms delay)
- Incremental compilation for changed files only
- Pattern-based file filtering (.ex, .exs only)

#### 4. State Preservation Manager
```elixir
defmodule OTPSupervisor.Core.StatePreservation do
  @moduledoc """
  Handles process state during hot-reload.
  """
  
  # Capture GenServer state before reload
  def capture_state(pid, module)
  
  # Restore state after hot-reload
  def restore_state(pid, module, preserved_state)
  
  # Define state migration hooks
  def migration_hook(old_state, new_module_version)
end
```

### Implementation Strategy

#### Phase 1: Isolated Compilation (Week 1-2)

**Remove Path Dependency:**
```elixir
# Remove from main mix.exs:
# {:otp_sandbox, path: "./sandbox/examples/otp_sandbox"}

# Replace with dynamic loading in SandboxManager
```

**Implement Isolated Compiler:**
1. Create separate compilation process per sandbox
2. Use temporary directories for build artifacts
3. Validate BEAM files before loading
4. Implement timeout and resource limits

**Benefits:**
- Complete isolation from host system
- Safe compilation failure handling
- No global code path pollution

#### Phase 2: Hot-Reload Infrastructure (Week 3-4)

**File Watching Implementation:**
```elixir
# Add to sandbox creation
{:ok, watcher_pid} = SandboxFileWatcher.watch_sandbox(
  sandbox_id, 
  sandbox_path
)

# Store watcher PID in sandbox record
sandbox_info = %{sandbox_info | file_watcher: watcher_pid}
```

**Module Version Management:**
1. Track BEAM data in ETS table with versions
2. Implement atomic module swapping
3. Create dependency graph analysis
4. Build rollback mechanism

**Benefits:**
- Real-time code updates
- Safe rollback capability
- Dependency-aware reloading

#### Phase 3: State Preservation (Week 5-6)

**GenServer State Hooks:**
```elixir
# Add to generated supervisors
def code_change(old_vsn, state, extra) do
  # Custom state migration logic
  migrated_state = StatePreservation.migration_hook(state, extra)
  {:ok, migrated_state}
end
```

**Process State Capture:**
1. Detect GenServer processes in sandbox
2. Capture state before module reload
3. Restore state after successful reload
4. Handle state migration for schema changes

**Benefits:**
- Zero-downtime updates
- Preserved process state
- Smooth development experience

### Technical Implementation Details

#### Isolated Compilation Process

```elixir
defp compile_in_isolation(sandbox_path, timeout \\ 30_000) do
  parent = self()
  
  compiler_pid = spawn(fn ->
    try do
      # Change to sandbox directory
      File.cd!(sandbox_path)
      
      # Compile with mix
      {result, exit_code} = System.cmd("mix", ["compile"], 
        stderr_to_stdout: true,
        timeout: timeout
      )
      
      send(parent, {:compilation_result, exit_code, result})
    catch
      kind, error ->
        send(parent, {:compilation_error, kind, error})
    end
  end)
  
  receive do
    {:compilation_result, 0, output} -> 
      {:ok, output}
    {:compilation_result, code, output} -> 
      {:error, {:compilation_failed, code, output}}
    {:compilation_error, kind, error} -> 
      {:error, {:compiler_crash, kind, error}}
  after
    timeout ->
      Process.exit(compiler_pid, :kill)
      {:error, :compilation_timeout}
  end
end
```

#### Hot Module Swapping

```elixir
defp hot_swap_module(sandbox_id, module, new_beam_data) do
  with {:ok, old_version} <- get_current_version(sandbox_id, module),
       {:ok, processes} <- find_module_processes(module),
       {:ok, states} <- capture_process_states(processes),
       :ok <- load_new_module(module, new_beam_data),
       :ok <- migrate_process_states(processes, states) do
    
    # Success - register new version
    register_module_version(sandbox_id, module, old_version + 1, new_beam_data)
    {:ok, :hot_swapped}
  else
    {:error, reason} ->
      # Rollback on any failure
      rollback_module(sandbox_id, module, old_version)
      {:error, reason}
  end
end
```

#### File Watching Integration

```elixir
def handle_info({:file_event, watcher_pid, {path, events}}, state) do
  sandbox_id = get_sandbox_by_watcher(state, watcher_pid)
  
  if should_trigger_compilation?(path, events) do
    # Debounce compilation
    Process.send_after(self(), {:compile_sandbox, sandbox_id}, 300)
  end
  
  {:noreply, state}
end

defp should_trigger_compilation?(path, events) do
  Path.extname(path) in [".ex", ".exs"] and
  :modified in events
end
```

### Safety Mechanisms

#### 1. Resource Limits
- Compilation timeout: 30 seconds
- Memory limit per compilation: 256MB
- Maximum module versions: 10 per module
- File watcher limit: 1000 files per sandbox

#### 2. Validation Checks
- BEAM file integrity validation
- Syntax checking before hot-swap
- Dependency cycle detection
- State migration validation

#### 3. Rollback Procedures
- Automatic rollback on hot-swap failure
- Manual rollback via management interface
- State restoration on rollback
- Error logging and alerting

### Configuration

```elixir
# config/dev.exs
config :otp_supervisor, :sandbox_hot_reload,
  enabled: true,
  compilation_timeout: 30_000,
  file_watch_debounce: 300,
  max_module_versions: 10,
  preserve_state: true,
  auto_rollback: true

# config/prod.exs
config :otp_supervisor, :sandbox_hot_reload,
  enabled: false  # Disable in production
```

### User Interface Enhancements

#### Enhanced Sandbox Widget
- Real-time compilation status indicator
- Hot-reload toggle per sandbox
- Version history and rollback buttons
- Compilation error display
- File change notifications

#### Development Tools
- Live compilation logs
- Module dependency visualizer
- State inspection tools
- Performance metrics

### Testing Strategy

#### Unit Tests
- Isolated compilation engine
- Module version management
- State preservation hooks
- File watching system

#### Integration Tests
- End-to-end hot-reload scenarios
- Failure recovery testing
- State migration validation
- Performance benchmarks

#### Load Testing
- Multiple simultaneous hot-reloads
- Large file change batches
- Memory usage under stress
- Compilation timeout scenarios

## Migration Plan

### Phase 1: Foundation (Weeks 1-2)
1. Remove path dependency from mix.exs
2. Implement isolated compilation engine
3. Add basic hot-reload infrastructure
4. Create safety mechanisms

### Phase 2: Core Features (Weeks 3-4)
1. Implement file watching system
2. Add module version management
3. Create rollback mechanisms
4. Build UI integration

### Phase 3: Advanced Features (Weeks 5-6)
1. Add state preservation
2. Implement dependency management
3. Create development tools
4. Performance optimization

### Phase 4: Polish (Week 7)
1. Comprehensive testing
2. Documentation updates
3. Performance tuning
4. UI/UX improvements

## Success Metrics

### Functionality
- ✅ Hot-reload completes in <1 second
- ✅ Compilation failures don't affect other sandboxes
- ✅ State preserved across reloads (when possible)
- ✅ Automatic rollback on failures

### Performance
- ✅ Memory usage <50MB per sandbox with hot-reload
- ✅ File watching overhead <1% CPU
- ✅ Compilation parallelization for multiple sandboxes
- ✅ No memory leaks over extended use

### Developer Experience
- ✅ Immediate visual feedback on changes
- ✅ Clear error messages for compilation failures
- ✅ Easy rollback and version management
- ✅ Intuitive UI controls

## Conclusion

This design provides true hot-reload isolation by:

1. **Complete fault isolation** through separate compilation processes
2. **Zero host system impact** by removing path dependencies
3. **Safe hot-swapping** with automatic rollback
4. **State preservation** for seamless development experience
5. **Performance optimization** through incremental compilation

The implementation maintains the excellent foundation of the current sandbox system while adding the advanced hot-reload capabilities needed for a true development sandbox environment.