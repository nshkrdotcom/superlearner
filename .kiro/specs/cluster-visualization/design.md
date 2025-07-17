# Design Document

## Overview

The cluster visualization feature will be implemented as a new LiveView page that combines data from the existing `ClusterSupervisionTrees` Arsenal operation AND process listings to show both supervised processes (in their hierarchical structure) and standalone processes. The design follows the established patterns in the codebase, using Phoenix LiveView for real-time updates and a JavaScript hook with D3.js for interactive visualization. The implementation prioritizes simplicity and reuses existing components wherever possible.

**Key Insight**: The existing `ClusterSupervisionTrees` operation only shows supervisors and their direct children in supervision hierarchies. To provide a complete cluster view, we need to also include standalone processes that aren't part of supervision trees, such as system processes, application processes not under supervisors, and other runtime processes.

## Architecture

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Browser (Client)                             │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │              ClusterVisualizationLive                   │    │
│  │  ┌─────────────────────────────────────────────────┐    │    │
│  │  │         ClusterVisualization Hook               │    │    │
│  │  │         (D3.js with d3.tree() layout)           │    │    │
│  │  └─────────────────────────────────────────────────┘    │    │
│  └─────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ WebSocket/LiveView (Targeted Updates)
                              │
┌─────────────────────────────────────────────────────────────────┐
│                    Phoenix Server                               │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │         ClusterVisualizationLive                        │    │
│  │  ┌─────────────────────────────────────────────────┐    │    │
│  │  │    ClusterSupervisionTrees Operation            │    │    │
│  │  │         (Existing + Minor Enhancements)         │    │    │
│  │  └─────────────────────────────────────────────────┘    │    │
│  │  ┌─────────────────────────────────────────────────┐    │    │
│  │  │    Distributed.ProcessList Operation            │    │    │
│  │  │    (Existing - handles all cluster RPC calls)   │    │    │
│  │  └─────────────────────────────────────────────────┘    │    │
│  └─────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────┘
```

### Component Responsibilities

- **ClusterVisualizationLive**: Main LiveView module handling state management, data loading, and real-time updates. Calls two Arsenal operations and merges data into unified hierarchical structure for D3.js
- **ClusterSupervisionTrees**: Existing Arsenal operation with minor enhancements for visualization metadata. Provides hierarchical supervisor/worker relationships
- **Distributed.ProcessList**: Existing Arsenal operation that handles distributed RPC calls to get all processes from all cluster nodes
- **ClusterVisualization Hook**: JavaScript module using D3.js d3.tree() layout for rendering interactive visualization
- **TerminalStatusBar**: Existing component for consistent UI header

## Components and Interfaces

### Backend Components

#### Enhanced ClusterSupervisionTrees Operation

The existing operation will be minimally enhanced to include visualization-specific metadata:

```elixir
# Minor additions to existing build_supervision_tree function
defp build_supervision_tree(supervisor, params) do
  base_supervisor = %{
    name: supervisor.name,
    pid: supervisor.pid,
    application: supervisor.application,
    strategy: supervisor.strategy,
    alive: supervisor.alive,
    child_count: supervisor.child_count,
    node: Node.self(),
    # NEW: Add basic visualization metadata
    level: 0,
    type: :supervisor
  }
  
  if include_children do
    children = get_enhanced_supervisor_children(supervisor.pid, params, 1)
    Map.put(base_supervisor, :children, children)
  else
    base_supervisor
  end
end

defp get_enhanced_supervisor_children(supervisor_pid, params, level) do
  # Enhanced to include level and type information
  # Existing logic + level tracking for visualization
end
```

#### ClusterVisualizationLive Module

```elixir
defmodule OtpSupervisorWeb.Live.ClusterVisualizationLive do
  use Phoenix.LiveView
  
  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterSupervisionTrees
  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  
  # State management for visualization data, loading states, and controls
  # Real-time update handling via PubSub
  # Error handling and recovery
end
```

### Frontend Components

#### ClusterVisualization JavaScript Hook

```javascript
const ClusterVisualization = {
  // D3.js-based visualization rendering
  // Tree layout algorithm for hierarchical display
  // Interactive features (hover, click, zoom, pan)
  // Data update handling
}
```

#### Visualization Layout Algorithm

The visualization uses a simple tree layout:

1. **Horizontal Node Positioning**: Cluster nodes distributed evenly across screen width
2. **Vertical Tree Growth**: Each node's supervision tree grows downward
3. **Level-based Positioning**: Processes positioned by their depth level in the supervision tree
4. **Collision Avoidance**: Simple spacing algorithm to prevent overlapping elements

### Data Flow

1. **Initial Load**: LiveView calls both `ClusterSupervisionTrees` operation (with `include_children: true`) AND `Distributed.ProcessList` operation (with `include_details: true`)
2. **Data Merging**: LiveView combines supervision tree data with all processes to identify supervised vs standalone processes
3. **Process Classification**: Creates PIDs set from supervision trees, then classifies all processes as supervised or standalone
4. **Unified Structure Creation**: Transforms data into single hierarchical structure per node with virtual "Standalone Processes" supervisor for D3.js d3.tree() layout
5. **Client Rendering**: JavaScript hook receives unified hierarchical data and uses d3.hierarchy() and d3.tree() for automatic layout
6. **Real-time Updates**: LiveView calculates diffs and pushes targeted events (process_added, process_removed, status_changed) rather than full dataset
7. **User Interaction**: JavaScript handles hover/click events and updates detail panel

## Data Models

### Unified Hierarchical Data Structure for D3.js

The LiveView will transform the two data sources into a single hierarchical structure per node that works optimally with D3.js d3.tree() layout:

```elixir
# Backend processing creates unified structure per node
%{
  cluster_nodes: %{
    "node1@server" => %{
      # Single hierarchical tree per node
      name: "node1@server",
      type: :node,
      children: [
        # Real supervision trees from ClusterSupervisionTrees
        %{
          name: "MyApp.Supervisor",
          pid: "<0.123.0>",
          type: :supervisor,
          alive: true,
          level: 0,
          node: :"node1@server",
          application: :my_app,
          strategy: :one_for_one,
          children: [%{...}]  # Recursive supervised processes
        },
        # Virtual supervisor for standalone processes
        %{
          name: "Standalone Processes",
          type: :virtual_supervisor,
          alive: true,
          level: 0,
          children: [
            %{
              name: "SomeGenServer",
              pid: "<0.456.0>",
              type: :worker,
              alive: true,
              level: 1,  # Relative to virtual supervisor
              node: :"node1@server",
              application: :some_app,
              memory: 1024,
              message_queue_len: 0
            }
            # ...other standalone processes
          ]
        }
      ]
    }
  }
}
```

### Frontend Data Structure (Optimized for D3.js)

```javascript
// Single unified structure per node for d3.hierarchy()
{
  cluster_nodes: {
    "node1@server": {
      name: "node1@server",
      type: "node",
      children: [
        // Real supervision trees
        {
          name: "MyApp.Supervisor",
          pid: "<0.123.0>",
          type: "supervisor",
          alive: true,
          level: 0,
          children: [...]
        },
        // Virtual supervisor for standalone processes
        {
          name: "Standalone Processes",
          type: "virtual_supervisor",
          alive: true,
          level: 0,
          children: [
            {
              name: "SomeGenServer",
              pid: "<0.456.0>",
              type: "worker",
              alive: true,
              level: 1
            }
            // ...other standalone processes
          ]
        }
      ]
    }
  }
}
```

### Data Processing Logic

The LiveView will:
1. **Collect Data**: Call both `ClusterSupervisionTrees` and `Distributed.ProcessList` operations
2. **Extract Supervised PIDs**: Create a Set of all PIDs found in supervision trees
3. **Classify Processes**: Identify standalone processes as those not in the supervised PIDs set
4. **Create Unified Structure**: Build single hierarchical tree per node with virtual "Standalone Processes" supervisor
5. **Optimize for D3**: Structure allows single `d3.hierarchy()` call per node for automatic layout

## Error Handling

### Backend Error Handling

- **Node Unavailable**: Skip failed nodes, continue with available ones
- **RPC Timeout**: 8-second timeout per node with graceful degradation
- **Data Corruption**: Validate data structure before sending to frontend
- **Operation Failure**: Return error state with retry capability

### Frontend Error Handling

- **Invalid Data**: Display error message and provide refresh option
- **Rendering Failure**: Fallback to simple text-based tree view
- **Network Issues**: Show connection status and auto-retry
- **Performance Issues**: Limit visualization complexity for large clusters

## Testing Strategy

### Unit Tests

```elixir
# Test enhanced ClusterSupervisionTrees operation
defmodule ClusterSupervisionTreesVisualizationTest do
  test "includes visualization metadata in response"
  test "handles level calculation correctly"
  test "maintains backward compatibility"
end

# Test LiveView functionality
defmodule ClusterVisualizationLiveTest do
  test "loads initial data successfully"
  test "handles real-time updates"
  test "manages error states properly"
end
```

### Integration Tests

```javascript
// Test JavaScript hook functionality
describe('ClusterVisualization', () => {
  test('renders nodes for multiple cluster nodes')
  test('handles data updates correctly')
  test('provides interactive features')
})
```

### Manual Testing

- Test with 2-node cluster setup
- Verify visualization with different supervision tree structures
- Test real-time updates by starting/stopping processes
- Validate responsive behavior on different screen sizes

## Performance Considerations

### Data Optimization

- **Depth Limiting**: Default maximum depth of 5 levels to prevent performance issues
- **Caching**: Leverage existing 5-second cache in ClusterSupervisionTrees operation
- **Incremental Updates**: Only update changed portions of visualization
- **Initial Load Performance**: Render node containers immediately, then populate each node's tree as data arrives for better perceived performance

### Rendering Optimization

- **SVG Rendering**: Use SVG for crisp visualization at all zoom levels
- **Element Limits**: Graceful degradation for clusters with >100 processes
- **Debounced Updates**: Batch rapid changes to prevent excessive re-rendering
- **Backend Throttling**: Batch burst changes (~100ms) to reduce WebSocket traffic during high-activity periods

### Memory Management

- **Data Cleanup**: Clear old visualization data when updating
- **Event Listeners**: Proper cleanup of D3.js event listeners
- **DOM Management**: Efficient SVG element creation and removal

### UI State Persistence

- **State Management**: Track UI state (expanded supervisors, zoom/pan transform) in LiveView assigns
- **Smooth Updates**: Re-apply UI state after data refreshes to maintain user context instead of resetting view
- **User Experience**: Preserve user's current view position and expansion state during real-time updates

## Implementation Phases

### Phase 1: Backend Foundation
- Enhance ClusterSupervisionTrees with visualization metadata
- Create ClusterVisualizationLive module
- Add route and basic HTML structure

### Phase 2: Frontend Visualization
- Install D3.js dependency
- Implement ClusterVisualization JavaScript hook
- Create basic tree layout rendering

### Phase 3: Interactivity
- Add hover tooltips and click details
- Implement zoom and pan functionality
- Add real-time update handling

### Phase 4: Polish and Testing
- Add loading states and error handling
- Implement responsive design
- Create comprehensive test suite

## Future Enhancement Hooks

The design includes several extension points for future enhancements:

- **Filtering Interface**: Controls can be added to filter by application, status, or node
- **Metrics Overlay**: Node sizes/colors can represent memory usage, CPU, etc.
- **Animation**: Smooth transitions for topology changes
- **Export Functionality**: Save visualization as PNG/SVG
- **Historical View**: Time-slider for topology changes over time

This design provides a solid foundation for the core visualization functionality while maintaining compatibility with the existing codebase and allowing for future enhancements.