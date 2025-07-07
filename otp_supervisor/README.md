# OTP Supervisor Educational Tool

Interactive Phoenix LiveView application for learning OTP supervisor concepts through visualization and hands-on experimentation.

## 🚀 Quick Start

```bash
mix setup                    # Install dependencies
mix phx.server              # Start server → http://localhost:4000
# OR
iex -S mix phx.server       # Start with interactive console
```

## 📋 Essential Dev Commands

```bash
# Server & Testing
mix phx.server              # Start Phoenix server
iex -S mix phx.server       # Start with IEx console
mix test                    # Run all tests
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

## 🎯 Features

- **Live Supervisor Monitoring** - Real-time visualization of supervision trees
- **Process Control** - Kill processes and watch supervisor restart strategies
- **Auto-Refresh** - Updates every second via LiveView
- **Educational** - Demonstrates one_for_one, all_for_one, rest_for_one strategies

## 📁 Project Structure

```
lib/
├── otp_supervisor/
│   ├── application.ex          # Main OTP app
│   ├── core/
│   │   └── control.ex         # Supervisor control API
│   └── sandbox/
│       ├── supervisors/       # Demo supervisors
│       └── workers/           # Counter & Printer GenServers
└── otp_supervisor_web/
    └── live/
        └── supervisor_live.ex # LiveView UI
```

Visit `/supervisors` to access the control panel.
