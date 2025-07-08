# OTP Supervisor Educational Tool

A comprehensive educational platform for learning OTP (Open Telecom Platform) supervision concepts through practical examples and real-time visualization.

## Overview

This project demonstrates OTP supervision principles through a Phoenix LiveView application that provides:
- **Real-time visualization** of supervision trees
- **Interactive process management** with kill/restart capabilities
- **Message tracing** for debugging communication patterns
- **Educational examples** showing different supervision strategies
- **Process introspection** tools for system analysis

## 🚀 Quick Start

```bash
mix setup                    # Install dependencies
mix phx.server              # Start server → http://localhost:4000
# OR
iex -S mix phx.server       # Start with interactive console
```

Visit `/supervisors` to access the control panel.

## 📋 Essential Dev Commands

```bash
# Server & Testing
mix phx.server              # Start Phoenix server
iex -S mix phx.server       # Start with IEx console
mix test                    # Run all tests
mix test --cover            # Run tests with coverage
mix test test/path/file.exs # Run specific test

# Code Quality
mix compile                 # Compile project
mix compile --warnings-as-errors
mix format                  # Format code
mix credo                   # Code analysis (if installed)

# Interactive Console
iex -S mix                  # Start IEx with project

# IEx Commands (while in console)
Supervisor.which_children(:demo_one_for_one)     # List children
Counter.get_value(:counter_1)                    # Get counter value
Counter.increment(:counter_1)                    # Increment counter
Counter.crash(:counter_1)                        # Trigger crash
Control.list_supervisors()                       # List all supervisors
```

## Architecture

### Core Components

#### 1. Control Module (`lib/otp_supervisor/core/control.ex`)
Provides comprehensive process management capabilities:
- `list_supervisors/0` - Lists all registered supervisors
- `get_supervision_tree/1` - Inspects supervision hierarchies
- `kill_process/1` - Terminates processes by PID
- `list_all_processes/1` - System-wide process analysis
- `get_process_state/1` - GenServer state inspection
- `build_process_graph/0` - Visual process topology mapping

#### 2. Message Tracer (`lib/otp_supervisor/core/message_tracer.ex`)
Advanced message tracing for debugging:
- `trace_messages/2` - Start tracing a process
- `get_message_history/1` - Retrieve captured messages
- `analyze_message_patterns/1` - Pattern analysis for debugging
- `stop_tracing/1` - Clean shutdown of tracers

#### 3. Demo Supervisor (`lib/otp_supervisor/sandbox/supervisors/demo_supervisor.ex`)
Educational supervisor implementing :one_for_one strategy with:
- Counter worker (persistent state)
- Printer worker (message processing)
- Configurable supervision strategies

#### 4. Phoenix LiveView UI (`lib/otp_supervisor_web/live/supervisor_live.ex`)
Interactive web interface providing:
- Real-time supervision tree visualization
- Process management controls
- Live system monitoring
- Educational tooltips and explanations

## 📁 Project Structure

```
lib/
├── otp_supervisor.ex                 # Main module
├── otp_supervisor/
│   ├── application.ex               # OTP application
│   ├── core/
│   │   ├── control.ex               # Process management API
│   │   └── message_tracer.ex        # Message tracing system
│   ├── sandbox/
│   │   ├── supervisors/
│   │   │   └── demo_supervisor.ex   # Educational supervisor
│   │   └── workers/
│   │       ├── counter.ex           # Stateful worker example
│   │       └── printer.ex           # Message processing worker
│   └── mailer.ex                    # Email functionality
├── otp_supervisor_web.ex            # Phoenix web module
└── otp_supervisor_web/
    ├── live/
    │   └── supervisor_live.ex       # LiveView interface
    └── ...                          # Standard Phoenix structure
```

### Test Structure
```
test/
├── otp_supervisor/
│   ├── core/
│   │   ├── control_test.exs         # API testing
│   │   └── message_tracer_test.exs  # Tracing functionality
│   └── sandbox/
│       └── workers/
│           ├── counter_test.exs     # Worker behavior testing
│           └── printer_test.exs     # Message handling testing
├── otp_supervisor_web/
│   └── live/
│       └── supervisor_live_test.exs # LiveView testing
└── support/
    └── supervisor_test_helper.ex    # Test utilities
```

## 🎯 Educational Features

### 1. Supervision Strategies
Demonstrate different OTP supervision strategies:
- **One-for-one**: Failed child restarted individually
- **One-for-all**: All children restarted when one fails
- **Rest-for-one**: Failed child and subsequent children restarted

### 2. Process Management
Interactive tools for:
- Listing all system processes
- Inspecting process hierarchies
- Killing processes to observe restart behavior
- Analyzing process relationships

### 3. Message Tracing
Debug communication patterns:
- Real-time message capture
- Pattern analysis for GenServer calls/casts
- Timing analysis for performance debugging

### 4. Live Visualization
Phoenix LiveView interface showing:
- Dynamic supervision tree updates
- Process state changes in real-time
- Interactive process management controls

## API Reference

### Control Module

#### Process Management
```elixir
# List all supervisors
OTPSupervisor.Core.Control.list_supervisors()

# Get supervision tree
{:ok, children} = OTPSupervisor.Core.Control.get_supervision_tree(:my_supervisor)

# Kill process
OTPSupervisor.Core.Control.kill_process(pid)

# Get process information
{:ok, info} = OTPSupervisor.Core.Control.get_process_info(pid)
```

#### System Analysis
```elixir
# List all processes with filtering
OTPSupervisor.Core.Control.list_all_processes(filter: :supervisor)

# Build process relationship graph
graph = OTPSupervisor.Core.Control.build_process_graph()

# Get GenServer state
{:ok, state} = OTPSupervisor.Core.Control.get_process_state(pid)
```

### Message Tracer

#### Tracing Operations
```elixir
# Start tracing
{:ok, tracer_pid} = OTPSupervisor.Core.MessageTracer.trace_messages(pid)

# Get message history
messages = OTPSupervisor.Core.MessageTracer.get_message_history(pid)

# Analyze patterns
patterns = OTPSupervisor.Core.MessageTracer.analyze_message_patterns(messages)

# Stop tracing
OTPSupervisor.Core.MessageTracer.stop_tracing(pid)
```

## Testing

The project includes comprehensive test coverage (828 lines of tests):

```bash
# Run all tests
mix test

# Run with coverage
mix test --cover

# Run specific test file
mix test test/otp_supervisor/core/control_test.exs
```

### Test Infrastructure
- **Robust synchronization**: Uses `Process.monitor` instead of `Process.sleep`
- **Test isolation**: Unique process names prevent conflicts
- **Comprehensive coverage**: Unit, integration, and LiveView tests
- **Educational focus**: Tests demonstrate OTP concepts

## Development

### Prerequisites
- Elixir 1.14+
- Phoenix 1.7+
- PostgreSQL (for session storage)

### Installation

1. Clone the repository
2. Install dependencies: `mix deps.get`
3. Start the Phoenix server: `mix phx.server`
4. Visit `http://localhost:4000` to access the educational interface

### Code Quality

#### Performance Considerations
- Efficient polling with scheduler yielding
- Automatic cleanup to prevent resource leaks
- Configurable tracing limits to prevent memory issues
- Async testing where appropriate

### Contributing

1. Fork the repository
2. Create a feature branch
3. Write tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

#### Code Style
- Follow standard Elixir conventions
- Write comprehensive documentation
- Include educational comments explaining OTP concepts
- Maintain test coverage

#### Test Requirements
- All new functions must have corresponding tests
- Tests should demonstrate educational value
- Use `SupervisorTestHelper` for consistent test setup
- Ensure proper cleanup to prevent test pollution

## Educational Resources

### OTP Concepts Demonstrated
- **Supervision Trees**: Hierarchical process organization
- **Fault Tolerance**: Let-it-crash philosophy
- **Process Linking**: Bidirectional failure propagation
- **Process Monitoring**: Unidirectional observation
- **GenServer**: Stateful server processes
- **Registry**: Process discovery and registration

### Learning Path
1. Start with the Phoenix LiveView interface
2. Experiment with killing processes to see restart behavior
3. Explore the supervision tree visualization
4. Use message tracing to understand communication patterns
5. Examine the source code to understand implementation details
6. Write your own supervisors and workers using the examples

This tool provides a comprehensive foundation for understanding OTP supervision concepts through practical, interactive examples.