# Implementation Plan

- [ ] 1. Enhance backend data collection for complete cluster view
  - Modify ClusterSupervisionTrees operation to include visualization metadata (level, type)
  - Use existing Distributed.ProcessList Arsenal operation to get all cluster processes
  - Create data merging logic in LiveView to combine supervision trees with all processes
  - Add process classification logic: recursively traverse ClusterSupervisionTrees result to collect all PIDs into MapSet, then check each process from ProcessList against this set
  - Ensure process metrics (memory, message_queue_len) are merged into final hierarchical data structure
  - _Requirements: 1.1, 1.2, 1.3_

- [ ] 2. Create ClusterVisualizationLive module with unified data structure
  - Create new LiveView module following existing patterns from SupervisorLive
  - Implement mount/3 function with initial state and data loading
  - Add load_cluster_data/1 function that calls both ClusterSupervisionTrees AND Distributed.ProcessList operations
  - Transform data into single hierarchical structure per node with virtual "Standalone Processes" supervisor for D3.js optimization
  - _Requirements: 1.1, 5.1, 5.2_

- [ ] 3. Add cluster visualization route and basic HTML structure
  - Add route to router.ex: `live "/cluster-visualization", Live.ClusterVisualizationLive`
  - Create basic render/1 function with terminal-style UI using existing patterns
  - Include TerminalStatusBar component for consistency
  - Add basic controls for auto-refresh toggle and manual refresh button
  - _Requirements: 5.1, 5.2, 3.5_

- [ ] 4. Install D3.js and create JavaScript hook foundation
  - Add D3.js dependency to assets/package.json
  - Create ClusterVisualization hook in assets/js/hooks.js
  - Implement basic hook structure with mounted/updated/destroyed lifecycle
  - Add hook registration to existing hooks export
  - _Requirements: 1.1, 4.1_

- [ ] 5. Implement D3.js tree layout visualization rendering
  - Create SVG container with zoom/pan capabilities using D3.js
  - Use single virtual root structure: `{ name: "root", children: [node1_tree, node2_tree, ...] }` for unified zoom/pan behavior
  - Use d3.hierarchy() to convert unified hierarchical data structure per node
  - Implement d3.tree() layout for automatic positioning of supervision hierarchies
  - Render links using d3.linkVertical() for clean tree connections
  - Render nodes as SVG groups containing circles and text labels
  - _Requirements: 1.1, 1.2, 4.1, 4.2, 4.3_

- [ ] 6. Add visual differentiation for process types and states
  - Implement different colors for supervisors vs workers vs standalone processes
  - Add status indicators (green for alive, red for dead processes)
  - Create different node sizes for supervisors vs workers
  - Add visual labels with process names
  - At significant zoom-out levels, hide text labels and show only colored circles/shapes for high-level cluster structure and health
  - _Requirements: 1.3, 1.4, 1.5, 4.5_

- [ ] 7. Implement interactive hover tooltips
  - Add mouseover/mouseout event handlers to visualization elements
  - Create tooltip display with basic process information (name, PID, type, status)
  - Position tooltips dynamically based on mouse location
  - Style tooltips to match terminal theme
  - _Requirements: 2.1, 5.3_

- [ ] 8. Add click-to-show-details functionality
  - Implement click event handlers for nodes
  - Create details panel component in HTML template
  - Add show/hide logic for details panel
  - Display detailed process information including memory and message queue length
  - _Requirements: 2.2, 2.3, 2.4_

- [ ] 9. Implement targeted real-time updates via LiveView
  - Add PubSub subscription for cluster state changes in mount function
  - Create handle_info callbacks that calculate diffs using Map.diff/2 or set-based operations on old and new process lists
  - Implement periodic refresh timer (5 seconds) following SupervisorLive pattern
  - Add diff calculation logic to identify specific changes (process added/removed/status changed)
  - Handle process restart scenarios where same registered name gets new PID (send process_restarted event with old_pid and new_pid)
  - Batch rapid changes (~100ms) to avoid sending 1000s of individual events during burst scenarios
  - Push targeted events (process_added, process_removed, status_changed, process_restarted) instead of full dataset
  - Update JavaScript hook to handle targeted events for efficient D3.js updates
  - _Requirements: 3.1, 3.2, 3.3, 3.4_

- [ ] 10. Add zoom and pan navigation controls
  - Implement D3.js zoom behavior on SVG container
  - Add mouse wheel zoom and drag pan functionality
  - Maintain visual clarity at different zoom levels
  - Add zoom extent limits to prevent excessive zoom in/out
  - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.5_

- [ ] 11. Implement loading states and error handling
  - Add loading spinner/animation using existing patterns
  - Create error display for failed data loading
  - Add retry functionality for failed operations
  - Handle network disconnection gracefully
  - Display appropriate messages for empty clusters
  - If data fetching fails for a specific node, render that node in error state (red border, "data unavailable" message) while other nodes render normally
  - Implement incremental loading: render node containers immediately, then populate each node's tree as data arrives
  - _Requirements: 5.4, 5.5_

- [ ] 12. Create comprehensive test suite
  - Write unit tests for ClusterVisualizationLive data loading and merging logic
  - Add tests for enhanced ClusterSupervisionTrees operation
  - Create JavaScript tests for ClusterVisualization hook rendering
  - Add integration tests for real-time updates
  - Test with multi-node cluster setup
  - _Requirements: All requirements validation_