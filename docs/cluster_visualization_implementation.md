# Cluster Visualization Implementation Guide

## Overview

This document outlines the implementation of a comprehensive cluster visualization page that displays all nodes, supervision trees, and processes in an interactive D3.js visualization. The goal is to create a clean, hierarchical view of the entire cluster topology with real-time updates.

## Visual Design Concept

### Layout Structure
- **Horizontal Node Layout**: Nodes arranged horizontally across the screen
- **Vertical Supervision Trees**: Each node's supervision tree grows vertically downward
- **Color Coding**: Different colors for nodes, supervisors, workers, and process states
- **Interactive Elements**: Hover effects, click to expand/collapse, zoom/pan capabilities

### Example for N=2 Nodes:
```
┌─────────────────────────────────────────────────────────────────────┐
│                     Cluster Visualization                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌─────────────────┐                    ┌─────────────────┐        │
│  │   Node 1        │                    │   Node 2        │        │
│  │ @node1@server   │                    │ @node2@server   │        │
│  └─────────────────┘                    └─────────────────┘        │
│          │                                       │                 │
│     ┌────▼────┐                             ┌────▼────┐           │
│     │MyApp.Sup│                             │MyApp.Sup│           │
│     └────┬────┘                             └────┬────┘           │
│          │                                       │                 │
│    ┌─────┼─────┐                           ┌─────┼─────┐          │
│    │     │     │                           │     │     │          │
│ ┌──▼─┐ ┌─▼─┐ ┌─▼─┐                      ┌──▼─┐ ┌─▼─┐ ┌─▼─┐       │
│ │Wkr1│ │Sup│ │Wkr│                      │Wkr1│ │Sup│ │Wkr│       │
│ └────┘ └─┬─┘ └───┘                      └────┘ └─┬─┘ └───┘       │
│          │                                       │                 │
│        ┌─▼─┐                                   ┌─▼─┐              │
│        │Wkr│                                   │Wkr│              │
│        └───┘                                   └───┘              │
└─────────────────────────────────────────────────────────────────────┘
```

## Required Arsenal Endpoints

### 1. Enhanced Cluster Supervision Trees (Modify Existing)
**Endpoint**: `GET /api/v1/cluster/supervision-trees`

**Enhancements Needed**:
```elixir
# Add to existing ClusterSupervisionTrees operation
defp build_supervision_tree(supervisor, params) do
  # ... existing code ...
  
  base_supervisor = %{
    name: supervisor.name,
    pid: supervisor.pid,
    application: supervisor.application,
    strategy: supervisor.strategy,
    alive: supervisor.alive,
    child_count: supervisor.child_count,
    node: Node.self(),
    # NEW FIELDS FOR VISUALIZATION
    memory: get_supervisor_memory(supervisor.pid),
    message_queue_len: get_supervisor_queue_length(supervisor.pid),
    uptime: get_supervisor_uptime(supervisor.pid),
    restart_count: get_supervisor_restart_count(supervisor.name),
    coordinates: %{x: 0, y: 0}, # Will be calculated by frontend
    level: 0 # Tree depth level
  }
  
  # Enhanced children with visualization data
  if include_children do
    children = get_enhanced_supervisor_children(supervisor.pid, params, 1)
    Map.put(base_supervisor, :children, children)
  else
    base_supervisor
  end
end

defp get_enhanced_supervisor_children(supervisor_pid, params, level) do
  # Recursively build tree with level tracking
  case Control.get_supervision_tree(supervisor_pid) do
    {:ok, tree} ->
      tree.children
      |> Enum.map(fn child ->
        enhanced_child = %{
          id: child.id,
          pid: child.pid,
          type: child.type,
          restart: child.restart,
          shutdown: child.shutdown,
          alive: child.alive,
          # NEW VISUALIZATION FIELDS
          name: format_child_name(child.id),
          memory: get_process_memory(child.pid),
          message_queue_len: get_process_queue_length(child.pid),
          cpu_usage: get_process_cpu_usage(child.pid),
          level: level,
          coordinates: %{x: 0, y: 0},
          parent_id: supervisor_pid
        }
        
        # Recursively get children if this is a supervisor
        if child.type == :supervisor and child.alive do
          children = get_enhanced_supervisor_children(child.pid, params, level + 1)
          Map.put(enhanced_child, :children, children)
        else
          Map.put(enhanced_child, :children, [])
        end
      end)
    {:error, _} -> []
  end
end
```

### 2. New Cluster Topology Overview
**Endpoint**: `GET /api/v1/cluster/topology-overview`

```elixir
defmodule OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterTopologyOverview do
  use OTPSupervisor.Core.Arsenal.Operation

  def execute(_params) do
    try do
      nodes = get_cluster_nodes()
      
      topology_data = 
        nodes
        |> Enum.map(&get_node_overview/1)
        |> Enum.filter(&(&1 != nil))
      
      {:ok, %{
        nodes: topology_data,
        cluster_health: get_cluster_health(),
        total_nodes: length(nodes),
        total_supervisors: count_total_supervisors(topology_data),
        total_processes: count_total_processes(topology_data),
        timestamp: DateTime.utc_now()
      }}
    rescue
      error -> {:error, "Failed to get cluster topology: #{inspect(error)}"}
    end
  end
  
  defp get_node_overview(node) do
    %{
      name: node,
      alive: Node.ping(node) == :pong,
      memory_usage: get_node_memory(node),
      process_count: get_node_process_count(node),
      supervisor_count: get_node_supervisor_count(node),
      applications: get_node_applications(node),
      uptime: get_node_uptime(node),
      load_average: get_node_load(node)
    }
  end
end
```

### 3. Real-time Updates Endpoint
**Endpoint**: `GET /api/v1/cluster/supervision-trees/stream` (WebSocket or SSE)

```elixir
defmodule OTPSupervisorWeb.ClusterVisualizationChannel do
  use Phoenix.Channel
  
  def join("cluster_visualization", _params, socket) do
    # Subscribe to cluster changes
    ClusterStateManager.subscribe_to_changes()
    
    # Send initial data
    send(self(), :send_initial_data)
    
    {:ok, socket}
  end
  
  def handle_info(:send_initial_data, socket) do
    case ClusterSupervisionTrees.execute(%{"include_children" => true}) do
      {:ok, data} ->
        push(socket, "initial_data", data)
      {:error, _} ->
        push(socket, "error", %{message: "Failed to load initial data"})
    end
    
    # Schedule periodic updates
    Process.send_after(self(), :send_update, 5_000)
    {:noreply, socket}
  end
  
  def handle_info(:send_update, socket) do
    # Send incremental updates
    case get_cluster_changes() do
      {:ok, changes} when changes != %{} ->
        push(socket, "cluster_update", changes)
      _ -> :ok
    end
    
    Process.send_after(self(), :send_update, 5_000)
    {:noreply, socket}
  end
end
```

## Frontend Implementation

### 1. LiveView Page Structure

**File**: `lib/otp_supervisor_web/live/cluster_visualization_live.ex`

```elixir
defmodule OtpSupervisorWeb.Live.ClusterVisualizationLive do
  use Phoenix.LiveView
  
  alias OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterSupervisionTrees
  alias OtpSupervisorWeb.Components.Terminal.TerminalStatusBar
  
  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:page_title, "Cluster Visualization")
      |> assign(:current_page, "cluster-visualization")
      |> assign(:loading, true)
      |> assign(:cluster_data, %{})
      |> assign(:error_message, nil)
      |> assign(:view_mode, "tree") # tree, force, hierarchical
      |> assign(:show_details, true)
      |> assign(:auto_refresh, true)
      |> load_cluster_data()
    
    if connected?(socket) do
      # Subscribe to real-time updates
      Phoenix.PubSub.subscribe(OtpSupervisor.PubSub, "cluster_visualization")
      Process.send_after(self(), :refresh_data, 5_000)
    end
    
    {:ok, socket}
  end
  
  def render(assigns) do
    ~H"""
    <div class="fixed inset-0 bg-gray-900 text-green-400 flex flex-col">
      <!-- Status Bar -->
      <.live_component
        module={TerminalStatusBar}
        id="cluster-viz-status-bar"
        title="Cluster Visualization"
        metrics={status_bar_metrics(assigns)}
        navigation_links={[]}
      />
      
      <!-- Controls -->
      <div class="p-4 border-b border-green-500/30">
        <div class="flex items-center space-x-4">
          <!-- View Mode Toggle -->
          <div class="flex items-center space-x-2">
            <label class="text-green-400/70 font-mono text-sm">View:</label>
            <select
              class="bg-gray-800 border border-green-500/30 text-green-400 font-mono text-sm rounded px-2 py-1"
              phx-change="change_view_mode"
              name="view_mode"
            >
              <option value="tree" selected={@view_mode == "tree"}>Tree Layout</option>
              <option value="force" selected={@view_mode == "force"}>Force Layout</option>
              <option value="hierarchical" selected={@view_mode == "hierarchical"}>Hierarchical</option>
            </select>
          </div>
          
          <!-- Auto Refresh Toggle -->
          <label class="flex items-center space-x-2">
            <input
              type="checkbox"
              checked={@auto_refresh}
              phx-change="toggle_auto_refresh"
              class="rounded bg-gray-800 border-green-500/30"
            />
            <span class="text-green-400/70 font-mono text-sm">Auto Refresh</span>
          </label>
          
          <!-- Refresh Button -->
          <button
            phx-click="refresh_data"
            class="bg-gray-700 hover:bg-gray-600 border border-green-500/30 text-green-400 font-mono text-sm px-3 py-1 rounded"
          >
            Refresh
          </button>
        </div>
      </div>
      
      <!-- Visualization Container -->
      <div class="flex-1 relative">
        <%= if @loading do %>
          <div class="absolute inset-0 flex items-center justify-center">
            <div class="text-center text-green-400/70 font-mono">
              <div class="animate-pulse">Loading cluster visualization...</div>
            </div>
          </div>
        <% else %>
          <div
            id="cluster-visualization"
            class="w-full h-full"
            phx-hook="ClusterVisualization"
            data-cluster-data={Jason.encode!(@cluster_data)}
            data-view-mode={@view_mode}
            data-show-details={@show_details}
          >
            <!-- D3.js will render here -->
          </div>
        <% end %>
      </div>
      
      <!-- Details Panel (Optional) -->
      <div id="details-panel" class="hidden absolute top-16 right-4 w-80 bg-gray-800 border border-green-500/30 rounded p-4">
        <div id="details-content" class="text-green-400 font-mono text-sm">
          <!-- Dynamic content populated by JavaScript -->
        </div>
      </div>
    </div>
    """
  end
end
```

### 2. JavaScript Hook for D3.js Integration

**File**: `assets/js/cluster_visualization.js`

```javascript
import * as d3 from 'd3';

const ClusterVisualization = {
  mounted() {
    this.initializeVisualization();
    this.handleDataUpdate();
  },

  updated() {
    this.handleDataUpdate();
  },

  destroyed() {
    if (this.simulation) {
      this.simulation.stop();
    }
  },

  initializeVisualization() {
    const container = d3.select(this.el);
    const containerRect = this.el.getBoundingClientRect();
    
    this.width = containerRect.width;
    this.height = containerRect.height;
    
    // Create SVG
    this.svg = container
      .append('svg')
      .attr('width', '100%')
      .attr('height', '100%')
      .attr('viewBox', `0 0 ${this.width} ${this.height}`);
    
    // Add zoom behavior
    const zoom = d3.zoom()
      .scaleExtent([0.1, 4])
      .on('zoom', (event) => {
        this.svg.select('.visualization-group')
          .attr('transform', event.transform);
      });
    
    this.svg.call(zoom);
    
    // Main visualization group
    this.vizGroup = this.svg
      .append('g')
      .attr('class', 'visualization-group');
    
    // Define arrow markers for links
    this.svg.append('defs')
      .append('marker')
      .attr('id', 'arrowhead')
      .attr('viewBox', '-0 -5 10 10')
      .attr('refX', 13)
      .attr('refY', 0)
      .attr('orient', 'auto')
      .attr('markerWidth', 13)
      .attr('markerHeight', 13)
      .attr('xoverflow', 'visible')
      .append('svg:path')
      .attr('d', 'M 0,-5 L 10 ,0 L 0,5')
      .attr('fill', '#10b981')
      .style('stroke', 'none');
  },

  handleDataUpdate() {
    const dataAttr = this.el.getAttribute('data-cluster-data');
    const viewMode = this.el.getAttribute('data-view-mode');
    
    if (!dataAttr) return;
    
    try {
      const clusterData = JSON.parse(dataAttr);
      this.renderVisualization(clusterData, viewMode);
    } catch (error) {
      console.error('Failed to parse cluster data:', error);
    }
  },

  renderVisualization(clusterData, viewMode) {
    this.clear();
    
    switch (viewMode) {
      case 'tree':
        this.renderTreeLayout(clusterData);
        break;
      case 'force':
        this.renderForceLayout(clusterData);
        break;
      case 'hierarchical':
        this.renderHierarchicalLayout(clusterData);
        break;
      default:
        this.renderTreeLayout(clusterData);
    }
  },

  renderTreeLayout(clusterData) {
    const nodes = this.flattenClusterData(clusterData);
    const links = this.buildLinks(nodes);
    
    // Calculate node positions
    this.calculateTreePositions(nodes);
    
    // Render links
    this.vizGroup.selectAll('.link')
      .data(links)
      .enter()
      .append('line')
      .attr('class', 'link')
      .attr('x1', d => d.source.x)
      .attr('y1', d => d.source.y)
      .attr('x2', d => d.target.x)
      .attr('y2', d => d.target.y)
      .attr('stroke', '#10b981')
      .attr('stroke-opacity', 0.6)
      .attr('stroke-width', 2)
      .attr('marker-end', 'url(#arrowhead)');
    
    // Render nodes
    const nodeGroups = this.vizGroup.selectAll('.node')
      .data(nodes)
      .enter()
      .append('g')
      .attr('class', 'node')
      .attr('transform', d => `translate(${d.x}, ${d.y})`)
      .on('mouseover', (event, d) => this.showTooltip(event, d))
      .on('mouseout', () => this.hideTooltip())
      .on('click', (event, d) => this.showDetails(d));
    
    // Node circles
    nodeGroups.append('circle')
      .attr('r', d => this.getNodeRadius(d))
      .attr('fill', d => this.getNodeColor(d))
      .attr('stroke', '#10b981')
      .attr('stroke-width', 2);
    
    // Node labels
    nodeGroups.append('text')
      .attr('dy', '0.3em')
      .attr('text-anchor', 'middle')
      .attr('fill', '#10b981')
      .attr('font-family', 'monospace')
      .attr('font-size', '12px')
      .text(d => this.getNodeLabel(d));
    
    // Node status indicators
    nodeGroups.append('circle')
      .attr('r', 4)
      .attr('cx', d => this.getNodeRadius(d) - 8)
      .attr('cy', d => -this.getNodeRadius(d) + 8)
      .attr('fill', d => d.alive ? '#10b981' : '#ef4444');
  },

  calculateTreePositions(nodes) {
    const nodesByLevel = {};
    const nodesByNode = {};
    
    // Group by cluster node and level
    nodes.forEach(node => {
      const nodeKey = node.cluster_node || 'unknown';
      if (!nodesByNode[nodeKey]) {
        nodesByNode[nodeKey] = [];
      }
      nodesByNode[nodeKey].push(node);
      
      const level = node.level || 0;
      if (!nodesByLevel[level]) {
        nodesByLevel[level] = [];
      }
      nodesByLevel[level].push(node);
    });
    
    const clusterNodes = Object.keys(nodesByNode);
    const nodeWidth = this.width / clusterNodes.length;
    const levelHeight = 80;
    
    // Position cluster nodes horizontally
    clusterNodes.forEach((clusterNodeName, nodeIndex) => {
      const nodeX = nodeWidth * (nodeIndex + 0.5);
      const nodeProcesses = nodesByNode[clusterNodeName];
      
      // Sort by level and position vertically
      nodeProcesses.sort((a, b) => (a.level || 0) - (b.level || 0));
      
      const processesByLevel = {};
      nodeProcesses.forEach(process => {
        const level = process.level || 0;
        if (!processesByLevel[level]) {
          processesByLevel[level] = [];
        }
        processesByLevel[level].push(process);
      });
      
      Object.keys(processesByLevel).forEach(level => {
        const levelProcesses = processesByLevel[level];
        const levelY = 100 + (parseInt(level) * levelHeight);
        const processSpacing = Math.min(nodeWidth / (levelProcesses.length + 1), 60);
        
        levelProcesses.forEach((process, processIndex) => {
          process.x = nodeX - (levelProcesses.length - 1) * processSpacing / 2 + processIndex * processSpacing;
          process.y = levelY;
        });
      });
    });
  },

  flattenClusterData(clusterData) {
    const nodes = [];
    
    if (clusterData.supervision_trees) {
      Object.entries(clusterData.supervision_trees).forEach(([nodeName, nodeData]) => {
        if (nodeData.supervisors) {
          nodeData.supervisors.forEach(supervisor => {
            this.addNodeAndChildren(supervisor, nodes, nodeName, 0);
          });
        }
      });
    }
    
    return nodes;
  },

  addNodeAndChildren(node, nodes, clusterNode, level) {
    const flatNode = {
      ...node,
      cluster_node: clusterNode,
      level: level,
      id: `${clusterNode}_${node.pid || node.name}`
    };
    
    nodes.push(flatNode);
    
    if (node.children && node.children.length > 0) {
      node.children.forEach(child => {
        this.addNodeAndChildren(child, nodes, clusterNode, level + 1);
      });
    }
  },

  buildLinks(nodes) {
    const links = [];
    const nodeMap = new Map(nodes.map(node => [node.id, node]));
    
    nodes.forEach(node => {
      if (node.children) {
        node.children.forEach(child => {
          const childId = `${node.cluster_node}_${child.pid || child.name}`;
          const childNode = nodeMap.get(childId);
          
          if (childNode) {
            links.push({
              source: node,
              target: childNode
            });
          }
        });
      }
    });
    
    return links;
  },

  getNodeRadius(node) {
    if (node.type === 'supervisor' || !node.type) {
      return 20;
    }
    return 12;
  },

  getNodeColor(node) {
    if (!node.alive) return '#ef4444';
    
    switch (node.type) {
      case 'supervisor':
        return '#3b82f6';
      case 'worker':
        return '#10b981';
      default:
        return '#8b5cf6';
    }
  },

  getNodeLabel(node) {
    const name = node.name || 'Unknown';
    return name.length > 12 ? name.substring(0, 12) + '...' : name;
  },

  showTooltip(event, node) {
    const tooltip = d3.select('body')
      .append('div')
      .attr('class', 'cluster-tooltip')
      .style('position', 'absolute')
      .style('background', '#1f2937')
      .style('color', '#10b981')
      .style('padding', '8px')
      .style('border', '1px solid #10b981')
      .style('border-radius', '4px')
      .style('font-family', 'monospace')
      .style('font-size', '12px')
      .style('z-index', '1000')
      .style('pointer-events', 'none');
    
    tooltip.html(`
      <div><strong>${node.name}</strong></div>
      <div>PID: ${node.pid}</div>
      <div>Type: ${node.type || 'supervisor'}</div>
      <div>Node: ${node.cluster_node}</div>
      <div>Status: ${node.alive ? 'Alive' : 'Dead'}</div>
      ${node.memory ? `<div>Memory: ${this.formatBytes(node.memory)}</div>` : ''}
    `);
    
    const [x, y] = d3.pointer(event, document.body);
    tooltip
      .style('left', (x + 10) + 'px')
      .style('top', (y - 10) + 'px');
  },

  hideTooltip() {
    d3.selectAll('.cluster-tooltip').remove();
  },

  showDetails(node) {
    const panel = document.getElementById('details-panel');
    const content = document.getElementById('details-content');
    
    content.innerHTML = `
      <h3 class="font-bold mb-2">${node.name}</h3>
      <div class="space-y-1">
        <div>PID: ${node.pid}</div>
        <div>Type: ${node.type || 'supervisor'}</div>
        <div>Node: ${node.cluster_node}</div>
        <div>Status: ${node.alive ? 'Alive' : 'Dead'}</div>
        ${node.memory ? `<div>Memory: ${this.formatBytes(node.memory)}</div>` : ''}
        ${node.message_queue_len !== undefined ? `<div>Queue: ${node.message_queue_len}</div>` : ''}
        ${node.strategy ? `<div>Strategy: ${node.strategy}</div>` : ''}
        ${node.application ? `<div>App: ${node.application}</div>` : ''}
      </div>
    `;
    
    panel.classList.remove('hidden');
  },

  formatBytes(bytes) {
    if (bytes >= 1048576) return Math.round(bytes / 1048576) + ' MB';
    if (bytes >= 1024) return Math.round(bytes / 1024) + ' KB';
    return bytes + ' B';
  },

  clear() {
    this.vizGroup.selectAll('*').remove();
    d3.selectAll('.cluster-tooltip').remove();
  }
};

export default ClusterVisualization;
```

### 3. CSS Styles

**File**: `assets/css/cluster_visualization.css`

```css
/* Cluster Visualization Styles */
.cluster-tooltip {
  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.3);
  backdrop-filter: blur(4px);
}

#cluster-visualization {
  background: radial-gradient(circle at 50% 50%, rgba(16, 185, 129, 0.05) 0%, rgba(17, 24, 39, 1) 70%);
}

.node {
  cursor: pointer;
  transition: all 0.2s ease;
}

.node:hover circle {
  filter: brightness(1.2);
  stroke-width: 3;
}

.link {
  transition: stroke-opacity 0.2s ease;
}

.node:hover ~ .link,
.link:hover {
  stroke-opacity: 1;
  stroke-width: 3;
}

/* Details panel */
#details-panel {
  backdrop-filter: blur(8px);
  background: rgba(31, 41, 55, 0.95);
  box-shadow: 0 8px 16px rgba(0, 0, 0, 0.4);
}

/* Loading animation */
@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.5; }
}

.animate-pulse {
  animation: pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite;
}
```

## Implementation Steps

### Phase 1: Backend Arsenal Enhancements (Day 1-2)
1. **Enhance ClusterSupervisionTrees operation** with visualization fields
2. **Create ClusterTopologyOverview operation** for node-level metrics
3. **Add helper functions** for memory, CPU, queue length data
4. **Test endpoints** with curl/Postman

### Phase 2: LiveView Foundation (Day 2-3)
1. **Create ClusterVisualizationLive module** with basic structure
2. **Add route** in router.ex: `live "/cluster-visualization", ClusterVisualizationLive`
3. **Implement data loading** and state management
4. **Add basic HTML structure** and controls

### Phase 3: D3.js Integration (Day 3-5)
1. **Install D3.js** via npm: `npm install d3`
2. **Create JavaScript hook** for visualization
3. **Implement tree layout algorithm**
4. **Add interactivity** (hover, click, zoom)
5. **Style with CSS** for visual appeal

### Phase 4: Real-time Updates (Day 5-6)
1. **Add WebSocket channel** for live updates
2. **Implement change detection** and incremental updates
3. **Add auto-refresh controls**
4. **Optimize performance** for large clusters

### Phase 5: Testing & Polish (Day 6-7)
1. **Test with 2+ nodes** and various supervision trees
2. **Add error handling** and loading states
3. **Optimize layout** for different screen sizes
4. **Add documentation** and usage instructions

## Testing Strategy

### Unit Tests
```elixir
# test/otp_supervisor/core/arsenal/operations/distributed/cluster_supervision_trees_test.exs
defmodule OTPSupervisor.Core.Arsenal.Operations.Distributed.ClusterSupervisionTreesTest do
  use ExUnit.Case
  
  test "enhanced operation includes visualization fields" do
    {:ok, result} = ClusterSupervisionTrees.execute(%{"include_children" => true})
    
    assert Map.has_key?(result, :supervision_trees)
    
    # Test visualization fields are present
    result.supervision_trees
    |> Enum.each(fn {_node, node_data} ->
      node_data.supervisors
      |> Enum.each(fn supervisor ->
        assert Map.has_key?(supervisor, :memory)
        assert Map.has_key?(supervisor, :coordinates)
        assert Map.has_key?(supervisor, :level)
      end)
    end)
  end
end
```

### Integration Tests
```javascript
// assets/js/test/cluster_visualization_test.js
describe('ClusterVisualization', () => {
  test('renders nodes for multiple cluster nodes', () => {
    const mockData = {
      supervision_trees: {
        'node1@server': { supervisors: [{ name: 'Sup1', pid: '<0.123.0>' }] },
        'node2@server': { supervisors: [{ name: 'Sup2', pid: '<0.124.0>' }] }
      }
    };
    
    const viz = new ClusterVisualization();
    viz.renderVisualization(mockData, 'tree');
    
    // Assert nodes are positioned correctly
    expect(viz.svg.selectAll('.node').size()).toBe(2);
  });
});
```

## Performance Considerations

### Data Optimization
- **Limit depth**: Default max depth of 5 levels to prevent performance issues
- **Lazy loading**: Load children on-demand for large trees
- **Caching**: Cache supervision tree data for 5 seconds
- **Incremental updates**: Only update changed nodes in real-time

### Rendering Optimization
- **Virtual scrolling**: For very large clusters (>100 nodes)
- **Level-of-detail**: Show less detail when zoomed out
- **Debounced updates**: Batch rapid changes together
- **Canvas fallback**: Use Canvas instead of SVG for >1000 elements

## Future Enhancements

1. **Export functionality**: Save visualization as PNG/SVG
2. **Filtering**: Show/hide by application, status, memory usage
3. **Animation**: Smooth transitions for topology changes
4. **Metrics overlay**: Show CPU, memory usage as node colors/sizes
5. **Historical view**: Time-slider to see topology changes over time
6. **Clustering**: Group related supervisors visually
7. **Search**: Find specific processes in large topologies

This comprehensive implementation provides a solid foundation for visualizing complex Elixir cluster topologies with room for future enhancements.