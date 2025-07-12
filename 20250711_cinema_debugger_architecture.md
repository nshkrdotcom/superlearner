# Cinema Debugger Technical Architecture

## Executive Summary

The Cinema Debugger is an innovative visualization system designed to make concurrent OTP systems comprehensible through time-based replay and synchronized code-execution visualization. This document outlines the technical architecture, data flow, and implementation strategies for building this system.

## System Architecture Overview

### Core Components

```
┌─────────────────────────────────────────────────────────────┐
│                        Frontend UI                           │
│  ┌─────────────────┐  ┌──────────────┐  ┌───────────────┐  │
│  │   Code Viewer   │  │  Visualizer  │  │ Time Controls │  │
│  │  (Syntax Highlighted) │  │  (D3.js/Canvas)  │  │  (Scrubber)    │  │
│  └─────────────────┘  └──────────────┘  └───────────────┘  │
└────────────────────────────┬────────────────────────────────┘
                             │ WebSocket/Phoenix Channels
┌────────────────────────────┴────────────────────────────────┐
│                    Phoenix Backend                           │
│  ┌─────────────────┐  ┌──────────────┐  ┌───────────────┐  │
│  │ Session Manager │  │ Replay Engine│  │ State Storage │  │
│  └─────────────────┘  └──────────────┘  └───────────────┘  │
└────────────────────────────┬────────────────────────────────┘
                             │
┌────────────────────────────┴────────────────────────────────┐
│                    Tracing Infrastructure                    │
│  ┌─────────────────┐  ┌──────────────┐  ┌───────────────┐  │
│  │  Code Tracer    │  │ State Capture│  │ Event Logger  │  │
│  │  (dbg/recon)    │  │              │  │               │  │
│  └─────────────────┘  └──────────────┘  └───────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Data Flow Architecture

### 1. Capture Phase

During execution, the system captures:

```elixir
defmodule CinemaDebugger.Capture do
  defstruct [
    :timestamp,
    :event_type,
    :process_id,
    :module,
    :function,
    :line_number,
    :arguments,
    :return_value,
    :state_before,
    :state_after,
    :message_queue,
    :links,
    :monitors
  ]
end
```

Event types include:
- `spawn` - New process creation
- `exit` - Process termination
- `send` - Message sending
- `receive` - Message reception
- `call` - Function call entry
- `return` - Function return
- `state_change` - GenServer state mutation
- `supervisor_event` - Supervisor actions

### 2. Storage Schema

Events are stored in a time-series optimized structure:

```elixir
# ETS Tables
:cinema_events        # {timestamp, event_id, event_data}
:cinema_processes     # {pid, process_info}
:cinema_supervision   # {supervisor_pid, child_specs}
:cinema_state_snapshots # {timestamp, pid, state}
```

### 3. Replay Engine

The replay engine maintains a virtual clock and can efficiently seek to any point in the execution timeline:

```elixir
defmodule CinemaDebugger.ReplayEngine do
  def seek_to(timestamp) do
    # 1. Find nearest state snapshot
    # 2. Replay events from snapshot to target
    # 3. Reconstruct complete system state
    # 4. Notify UI of state change
  end
  
  def play_forward(speed_multiplier) do
    # Advance virtual clock at specified speed
    # Emit events to UI via Phoenix Channels
  end
  
  def play_backward(speed_multiplier) do
    # Reverse replay using state snapshots
    # Requires inverse operation tracking
  end
end
```

## Frontend Architecture

### 1. Code Viewer Component

```javascript
class CodeViewer {
  constructor() {
    this.highlightedLines = new Map();
    this.executionPointers = new Map();
  }
  
  highlightExecution(processId, lineNumber, intensity) {
    // Animate highlighting based on execution frequency
    // Show process ID badges on active lines
  }
  
  showStateAnnotation(lineNumber, state) {
    // Display inline state values
    // Show before/after for mutations
  }
}
```

### 2. Process Visualization

The visualization uses a force-directed graph for supervision trees with custom rendering:

```javascript
class ProcessVisualizer {
  constructor() {
    this.nodes = []; // Processes
    this.links = []; // Supervision/Links/Monitors
    this.messages = []; // In-flight messages
  }
  
  renderFrame(timestamp) {
    // Update node positions
    // Animate message passing
    // Show process states
    // Highlight active processes
  }
  
  showProcessDetail(pid) {
    // Display message queue
    // Show current state
    // List links/monitors
  }
}
```

### 3. Timeline Control

```javascript
class TimelineControl {
  constructor(duration, eventCount) {
    this.currentTime = 0;
    this.playbackSpeed = 1.0;
    this.eventDensityMap = this.calculateDensity();
  }
  
  createScrubber() {
    // Show event density heatmap
    // Provide frame-by-frame control
    // Support breakpoint setting
  }
}
```

## Tracing Implementation

### 1. Code Instrumentation

For the hardcoded prototype:

```elixir
defmodule CinemaDebugger.Instrumenter do
  defmacro instrument(module) do
    quote do
      # Wrap all functions with tracing
      # Capture entry/exit/state
      # Minimal performance impact
    end
  end
end
```

### 2. State Capture Strategy

```elixir
defmodule CinemaDebugger.StateCapture do
  def capture_genserver_state(pid) do
    # Use sys:get_state/1 for GenServers
    # Custom protocols for other processes
    # Efficient serialization
  end
  
  def capture_ets_tables(table_refs) do
    # Snapshot ETS state
    # Track modifications
  end
end
```

## Performance Considerations

### 1. Data Compression

- Event deduplication
- Delta compression for states
- Efficient binary encoding

### 2. Streaming Architecture

- Progressive loading of events
- Viewport-based rendering
- Lazy state reconstruction

### 3. Indexing Strategy

```elixir
# Multi-dimensional indexes
:cinema_time_index     # Fast time-based lookup
:cinema_process_index  # Per-process event streams
:cinema_module_index   # Code-to-event mapping
```

## Implementation Phases

### Phase 1: Hardcoded Prototype (Weeks 1-4)

1. **Week 1-2**: Basic event capture for simple GenServer
2. **Week 3**: UI prototype with code highlighting
3. **Week 4**: Timeline control and basic replay

### Phase 2: Enhanced Visualization (Weeks 5-8)

1. **Week 5-6**: Supervision tree visualization
2. **Week 7**: Message passing animation
3. **Week 8**: State inspection UI

### Phase 3: Generalization (Weeks 9-16)

1. **Week 9-10**: Dynamic instrumentation
2. **Week 11-12**: Arbitrary code support
3. **Week 13-14**: Performance optimization
4. **Week 15-16**: Production hardening

## Integration Points

### 1. SANDBOX Integration

```elixir
defmodule CinemaDebugger.SandboxIntegration do
  def start_traced_sandbox(config) do
    # Start sandbox with tracing enabled
    # Automatic instrumentation
    # Isolated event capture
  end
end
```

### 2. Arsenal Tools Integration

- Use Arsenal's process inspection APIs
- Leverage message tracing capabilities
- Integrate with performance metrics

## Technical Challenges & Solutions

### 1. Challenge: High-Frequency Events

**Solution**: Adaptive sampling with importance scoring
- Critical events (crashes, restarts) always captured
- High-frequency events sampled based on significance
- UI provides density indicators

### 2. Challenge: State Size

**Solution**: Hierarchical state representation
- Collapse large data structures
- On-demand expansion in UI
- Structural sharing for efficiency

### 3. Challenge: Causal Ordering

**Solution**: Vector clocks and happened-before tracking
- Maintain causality across processes
- Visualize message ordering
- Handle clock skew in distributed systems

## Future Enhancements

### 1. Distributed System Support
- Multi-node execution capture
- Network partition visualization
- Distributed transaction tracking

### 2. AI-Assisted Analysis
- Anomaly detection in execution patterns
- Suggested optimization points
- Automatic bug detection

### 3. Collaborative Features
- Shared debugging sessions
- Execution annotations
- Teaching mode with guided tours

## Conclusion

The Cinema Debugger architecture provides a solid foundation for understanding concurrent OTP systems through visualization. By carefully balancing capture completeness with performance, and providing intuitive visualization metaphors, we can make concurrent programming more accessible to developers at all skill levels.