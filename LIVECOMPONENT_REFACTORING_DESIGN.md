# LiveComponent Refactoring Design Document

## Overview

This document outlines the complete approach for refactoring the OTP Supervisor web application from monolithic LiveView pages to reusable, composable LiveComponents. The goal is to eliminate code duplication, improve maintainability, and create a consistent terminal-themed UI system.

## Current State Analysis

### Four Main Pages
1. **DocsLive** - Documentation center with search functionality
2. **SystemDashboardLive** - High-density system monitoring dashboard  
3. **SupervisorLive** - OTP supervisor monitoring and control interface
4. **ArsenalLive** - Arsenal command center for OTP operations

### Code Duplication Issues
- Each page implements its own status bar with metrics
- Repeated terminal-themed styling patterns
- Similar data table implementations
- Duplicate data formatting logic
- Inconsistent navigation implementations

## Proposed Architecture

### Component Organization Structure

```
lib/otp_supervisor_web/components/
├── core_components.ex (existing - keep as is)
├── terminal/
│   ├── terminal_status_bar.ex
│   ├── terminal_metric_widget.ex
│   ├── terminal_table.ex
│   ├── terminal_navigation_links.ex
│   └── terminal_data_formatters.ex
├── layout/
│   ├── terminal_panel_layout.ex
│   └── terminal_page_layout.ex
├── widgets/
│   ├── system_metrics_widget.ex
│   ├── process_list_widget.ex
│   ├── supervisor_tree_widget.ex
│   ├── operation_grid_widget.ex
│   └── execution_panel_widget.ex
└── shared/
    ├── terminal_theme_helpers.ex
    └── terminal_utilities.ex
```

## High Priority Components

### 1. TerminalStatusBar Component
**File**: `lib/otp_supervisor_web/components/terminal/terminal_status_bar.ex`

```elixir
defmodule OtpSupervisorWeb.Components.Terminal.TerminalStatusBar do
  use Phoenix.LiveComponent
  
  # Terminal-themed status bar with metrics and navigation
  attr :title, :string, required: true
  attr :metrics, :list, default: []
  attr :navigation_links, :list, default: []
  attr :border_color, :string, default: "border-green-500/30"
  attr :bg_color, :string, default: "bg-gray-900"
  
  slot :left_section
  slot :right_section
  slot :center_section
end
```

**Usage across pages:**
- DocsLive: Title + search stats
- SystemDashboardLive: System metrics + alerts
- SupervisorLive: Supervisor count + health status
- ArsenalLive: Operation stats + execution status

### 2. TerminalMetricWidget Component
**File**: `lib/otp_supervisor_web/components/terminal/terminal_metric_widget.ex`

```elixir
defmodule OtpSupervisorWeb.Components.Terminal.TerminalMetricWidget do
  use Phoenix.LiveComponent
  
  # Terminal-styled metric display widget with real-time updates
  attr :title, :string, required: true
  attr :metrics, :list, required: true
  attr :size, :atom, values: [:small, :medium, :large], default: :medium
  attr :border_color, :string, default: "border-green-500/30"
  attr :update_interval, :integer, default: 1000
  
  slot :actions
end
```

**Usage:**
- SystemDashboardLive: CPU, Memory, Process counts
- ArsenalLive: Operation metrics, Success rates
- SupervisorLive: Supervisor health metrics

### 3. TerminalTable Component
**File**: `lib/otp_supervisor_web/components/terminal/terminal_table.ex`

```elixir
defmodule OtpSupervisorWeb.Components.Terminal.TerminalTable do
  use Phoenix.LiveComponent
  
  # Terminal-styled data table with sorting, filtering, and actions
  attr :title, :string, required: true
  attr :rows, :list, required: true
  attr :columns, :list, required: true
  attr :sortable, :boolean, default: true
  attr :filterable, :boolean, default: true
  attr :selectable, :boolean, default: false
  attr :max_height, :string, default: "max-h-96"
  
  slot :actions
  slot :row_actions, arg: :row
end
```

**Usage:**
- SystemDashboardLive: Process lists, Resource usage
- SupervisorLive: Supervisor lists, Children processes
- ArsenalLive: Operation grids, Command history

### 4. TerminalDataFormatters Component
**File**: `lib/otp_supervisor_web/components/terminal/terminal_data_formatters.ex`

```elixir
defmodule OtpSupervisorWeb.Components.Terminal.TerminalDataFormatters do
  use Phoenix.Component
  
  # Consistent data formatting across all pages
  def format_bytes(assigns)
  def format_uptime(assigns)
  def format_process_status(assigns)
  def format_number(assigns)
  def format_percentage(assigns)
  def format_timestamp(assigns)
  def format_memory_usage(assigns)
  def format_cpu_usage(assigns)
end
```

## Medium Priority Components

### 5. TerminalPanelLayout Component
**File**: `lib/otp_supervisor_web/components/layout/terminal_panel_layout.ex`

```elixir
defmodule OtpSupervisorWeb.Components.Layout.TerminalPanelLayout do
  use Phoenix.LiveComponent
  
  # Flexible panel layout system for different page structures
  attr :layout_type, :atom, values: [:two_panel, :three_panel, :grid], required: true
  attr :panels, :list, required: true
  attr :resizable, :boolean, default: false
  attr :min_panel_width, :string, default: "min-w-64"
  
  slot :header
  slot :footer
end
```

**Layout Types:**
- DocsLive: `:two_panel` (sidebar + content)
- SystemDashboardLive: `:grid` (complex grid layout)
- SupervisorLive: `:two_panel` with optional bottom panel
- ArsenalLive: `:three_panel` with conditional right panel

### 6. TerminalNavigationLinks Component
**File**: `lib/otp_supervisor_web/components/terminal/terminal_navigation_links.ex`

```elixir
defmodule OtpSupervisorWeb.Components.Terminal.TerminalNavigationLinks do
  use Phoenix.LiveComponent
  
  # Consistent terminal-themed navigation across pages
  attr :current_page, :string, required: true
  attr :links, :list, default: []
  attr :style, :atom, values: [:horizontal, :vertical], default: :horizontal
  
  slot :custom_link, arg: :link
end
```

## Specialized Widget Components

### 7. SystemMetricsWidget Component
**File**: `lib/otp_supervisor_web/components/widgets/system_metrics_widget.ex`

```elixir
defmodule OtpSupervisorWeb.Components.Widgets.SystemMetricsWidget do
  use Phoenix.LiveComponent
  
  # Real-time system metrics with charts and alerts
  attr :metrics, :map, required: true
  attr :show_charts, :boolean, default: true
  attr :compact_mode, :boolean, default: false
  attr :alert_thresholds, :map, default: %{}
end
```

### 8. ProcessListWidget Component
**File**: `lib/otp_supervisor_web/components/widgets/process_list_widget.ex`

```elixir
defmodule OtpSupervisorWeb.Components.Widgets.ProcessListWidget do
  use Phoenix.LiveComponent
  
  # Interactive process list with filtering and actions
  attr :processes, :list, required: true
  attr :show_actions, :boolean, default: false
  attr :filters, :map, default: %{}
  attr :grouping, :atom, values: [:none, :supervisor, :type], default: :none
end
```

### 9. OperationGridWidget Component
**File**: `lib/otp_supervisor_web/components/widgets/operation_grid_widget.ex`

```elixir
defmodule OtpSupervisorWeb.Components.Widgets.OperationGridWidget do
  use Phoenix.LiveComponent
  
  # Arsenal-specific operation grid with status indicators
  attr :operations, :list, required: true
  attr :grid_size, :string, default: "grid-cols-10"
  attr :show_status, :boolean, default: true
  attr :interactive, :boolean, default: true
end
```

## Implementation Strategy

### Phase 1: Foundation Components (Week 1)
1. **TerminalStatusBar** - Create and test with one page
2. **TerminalMetricWidget** - Implement basic metrics display
3. **TerminalDataFormatters** - Extract and centralize formatting logic
4. **TerminalTable** - Create reusable table component

### Phase 2: Layout Components (Week 2)
1. **TerminalPanelLayout** - Implement flexible layout system
2. **TerminalNavigationLinks** - Standardize navigation
3. **TerminalPageLayout** - Create page-level layout wrapper

### Phase 3: Specialized Widgets (Week 3)
1. **SystemMetricsWidget** - Real-time metrics with charts
2. **ProcessListWidget** - Interactive process management
3. **OperationGridWidget** - Arsenal-specific grid display

### Phase 4: Page Refactoring (Week 4)
1. **DocsLive** - Refactor to use new components
2. **SystemDashboardLive** - Replace widgets with LiveComponents
3. **SupervisorLive** - Integrate new table and layout components
4. **ArsenalLive** - Update to use specialized widgets

## Benefits and Expected Outcomes

### Code Quality Improvements
- **60-70% reduction** in duplicate code
- **Centralized styling** for consistent terminal theme
- **Easier maintenance** with single source of truth
- **Better testing** with component-level isolation

### Performance Benefits
- **Independent component lifecycle** management
- **Reduced DOM updates** with targeted re-renders
- **Optimized data flow** with component-specific assigns

### Developer Experience
- **Consistent API** across all terminal components
- **Reusable patterns** for future features
- **Clear separation** of concerns
- **Easier debugging** with component boundaries

## Migration Considerations

### Backward Compatibility
- Existing pages will continue to work during migration
- Incremental adoption of new components
- No breaking changes to existing functionality

### Testing Strategy
- Component-level tests for each LiveComponent
- Integration tests for page-level functionality
- Visual regression tests for terminal styling
- Performance benchmarks for component rendering

### Documentation Requirements
- Component API documentation with examples
- Usage patterns and best practices
- Migration guide for existing pages
- Styling guide for terminal theme consistency

## Success Metrics

### Code Quality
- Lines of code reduction: Target 60-70%
- Cyclomatic complexity reduction: Target 40-50%
- Code duplication elimination: Target 90%+

### Performance
- Page load time improvement: Target 15-20%
- Memory usage reduction: Target 10-15%
- Component render time: Target <50ms

### Maintainability
- Time to add new features: Target 30-40% faster
- Bug fix time: Target 25-35% faster
- Component reusability: Target 80%+ across pages

## Risk Mitigation

### Technical Risks
- **Component complexity**: Keep components focused and single-purpose
- **Performance regression**: Continuous monitoring and optimization
- **State management**: Clear data flow patterns and documentation

### Implementation Risks
- **Scope creep**: Stick to defined component boundaries
- **Timeline delays**: Prioritize high-impact components first
- **Integration issues**: Thorough testing at each phase

## Future Enhancements

### Post-Migration Opportunities
- **Theme system** for multiple terminal color schemes
- **Component library** for rapid prototyping
- **Animation system** for smooth transitions
- **Accessibility improvements** for screen readers
- **Mobile responsive** terminal interface

### Extensibility
- **Plugin system** for custom widgets
- **Configuration-driven** layouts
- **Dynamic component** loading
- **Third-party integration** components

---

This design document provides a comprehensive roadmap for transforming the OTP Supervisor web application into a maintainable, reusable, and performant system using Phoenix LiveComponents while preserving the unique terminal aesthetic and functionality.