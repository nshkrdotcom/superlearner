# Implementation Plan

- [x] 1. Enhance backend data collection for complete cluster view







  - Modify ClusterSupervisionTrees operation to include visualization metadata (level, type)
  - Use existing Distributed.ProcessList Arsenal operation to get all cluster processes
  - Create data merging logic in LiveView to combine supervision trees with all processes
  - Add process classification logic: recursively traverse ClusterSupervisionTrees result to collect all PIDs into MapSet, then check each process from ProcessList against this set
  - Ensure process metrics (memory, message_queue_len) are merged into final hierarchical data structure
  - _Requirements: 1.1, 1.2, 1.3_

- [x] 2. Create ClusterVisualizationLive module with unified data structure










  - Create new LiveView module following existing patterns from SupervisorLive
  - Implement mount/3 function with initial state and data loading
  - Add load_cluster_data/1 function that calls both ClusterSupervisionTrees AND Distributed.ProcessList operations
  - Transform data into single hierarchical structure per node with virtual "Standalone Processes" supervisor for D3.js optimization
  - _Requirements: 1.1, 5.1, 5.2_

- [x] 3. Add cluster visualization route and basic HTML structure





  - Add route to router.ex: `live "/cluster-visualization", Live.ClusterVisualizationLive`
  - Create basic render/1 function with terminal-style UI using existing patterns
  - Include TerminalStatusBar component for consistency
  - Add basic controls for auto-refresh toggle and manual refresh button
  - _Requirements: 5.1, 5.2, 3.5_
-

- [x] 4. Create high-density text-based cluster tree display



  - Implement ASCII tree rendering function that converts hierarchical data to indented text format
  - Use Unicode box-drawing characters (├─, └─, │) for clean tree structure
  - Add color coding using ANSI/terminal colors: green for supervisors, blue for workers, red for dead processes
  - Create compact single-line format: `├─ MyWorker [#PID<0.123.0>] (worker) 2.1MB 5msgs`
  - _Requirements: 1.1, 1.2, 1.3, 4.1_

- [x] 5. Implement multi-node text layout with node headers





  - Create node section headers with cluster status: `=== Node: node1@host (connected) ===`
  - Render each node's supervision tree in separate sections
  - Add node-level statistics: total processes, memory usage, message queue totals
  - Implement horizontal separator lines between nodes for visual clarity
  - _Requirements: 1.1, 1.2, 4.2, 4.3_

- [x] 5.1. Add comprehensive filtering and search capabilities like /processes page





  - Implement node filtering dropdown to show/hide specific nodes in visualization
  - Add process type filtering: supervisors, workers, all types with dynamic options from data
  - Create application filtering dropdown populated from actual cluster applications
  - Add search functionality with debounced input for PID, process name, or module matching
  - Implement "Clear Filters" button to reset all filters and expand all nodes
  - Add active filter indicators showing currently applied filters
  - Display filtered results counter: "Showing X of Y processes"
  - Apply filters to tree rendering by filtering cluster_data before passing to ClusterTreeRenderer
  - Add URL parameter support for bookmarkable filter states
  - Ensure filtering works with real-time updates and maintains filter state during refreshes
  - _Requirements: 1.3, 1.4, 1.5, 2.1, 4.2_

- [ ] 6. Add interactive text-based selection and filtering
  - Implement keyboard navigation using arrow keys to highlight processes
  - Add process type filtering: press 's' for supervisors only, 'w' for workers only, 'a' for all
  - Create search functionality: type to filter processes by name/PID
  - Add status filtering: show only alive/dead processes
  - Highlight selected process with background color change
  - _Requirements: 1.3, 1.4, 1.5, 2.1_

- [ ] 7. Create expandable/collapsible text tree sections
  - Add expand/collapse functionality for supervision subtrees using +/- indicators
  - Implement keyboard shortcuts: Enter to toggle, Space to expand all, Backspace to collapse all
  - Store expansion state in LiveView assigns to persist across updates
  - Show process count in collapsed sections: `├─ MySupervisor [+] (12 children)`
  - _Requirements: 2.1, 4.4, 4.5_

- [ ] 8. Add detailed process information panel
  - Create side panel or bottom panel for selected process details
  - Display comprehensive process info: PID, registered name, memory, message queue, links, monitors
  - Add process state information: current function, stack trace if available
  - Include process restart history and supervisor strategy info
  - Format data in readable key-value pairs with proper alignment
  - _Requirements: 2.2, 2.3, 2.4_

- [ ] 9. Implement real-time text updates with change highlighting
  - Add PubSub subscription for cluster state changes in mount function
  - Create handle_info callbacks for periodic refresh (5 seconds) following SupervisorLive pattern
  - Implement change detection: highlight new processes in green, removed in red, changed in yellow
  - Add change indicators in tree: `├─ MyWorker [#PID<0.123.0>] (worker) 2.1MB 5msgs [NEW]`
  - Fade change indicators after 3 seconds using CSS transitions
  - Batch rapid changes to avoid overwhelming the display
  - _Requirements: 3.1, 3.2, 3.3, 3.4_

- [ ] 10. Add text-based navigation and view controls
  - Implement scrolling for large cluster trees using CSS overflow
  - Add view mode toggles: compact vs detailed, flat vs hierarchical
  - Create sorting options: by name, PID, memory usage, message queue length
  - Add refresh controls: auto-refresh toggle, manual refresh button, refresh interval selector
  - Implement full-screen mode for maximum information density
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