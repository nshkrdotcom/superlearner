# OTP Supervisor Learning Curriculum

## Curriculum Overview

This curriculum outlines the learning modules needed to master OTP supervisors through our educational tool. Each module builds on previous knowledge and includes hands-on exercises.

## Module 1: Process Basics (Foundation)

### Learning Objectives
- Understand processes as units of concurrency
- Create and manage basic processes
- Understand process isolation benefits

### Concepts Covered
1. **Process Creation**
   - `spawn/1` and `spawn/3`
   - Process lifecycle
   - Process termination

2. **Process Communication**
   - `send/2` operation
   - `receive` blocks
   - Message patterns

3. **Process Identification**
   - PIDs and their structure
   - `self()`
   - Process naming with `Process.register/2`

### Interactive Exercises
- Spawn a process and watch it execute
- Send messages between processes
- Create a simple echo server
- Observe process death and cleanup

### Sandbox Code Example
```elixir
defmodule ProcessBasics do
  def echo_server do
    receive do
      {from, message} ->
        send(from, {:echo, message})
        echo_server()
    end
  end
  
  def ping_pong_demo do
    # Interactive demo showing message passing
  end
end
```

## Module 2: Process Linking and Monitoring

### Learning Objectives
- Understand failure propagation
- Differentiate links vs monitors
- Implement error handling patterns

### Concepts Covered
1. **Process Links**
   - `spawn_link/1`
   - Bidirectional failure propagation
   - EXIT signals

2. **Process Monitors**
   - `Process.monitor/1`
   - Unidirectional monitoring
   - DOWN messages

3. **Trapping Exits**
   - `Process.flag(:trap_exit, true)`
   - Converting exits to messages
   - Selective failure handling

### Interactive Exercises
- Create linked processes and crash one
- Set up monitors and observe DOWN messages
- Build a simple process registry
- Implement a restart mechanism

### Visual Elements
- Link visualization showing bidirectional connections
- Monitor visualization showing unidirectional arrows
- EXIT signal propagation animation

## Module 3: GenServer Fundamentals

### Learning Objectives
- Understand the GenServer abstraction
- Implement stateful processes
- Handle synchronous and asynchronous operations

### Concepts Covered
1. **GenServer Basics**
   - `use GenServer`
   - `init/1` callback
   - Starting GenServers

2. **Client-Server Interaction**
   - `handle_call/3` for synchronous ops
   - `handle_cast/2` for async ops
   - `handle_info/2` for other messages

3. **State Management**
   - State initialization
   - State updates
   - State recovery patterns

### Interactive Exercises
- Build a counter GenServer
- Implement a key-value store
- Create a GenServer that crashes and recovers
- Explore timeout scenarios

### Sandbox Implementation
```elixir
defmodule LearningGenServer do
  use GenServer
  
  # Visual state inspection
  def get_state_visual(server) do
    GenServer.call(server, :get_state_visual)
  end
  
  # Crash simulation
  def simulate_crash(server, reason) do
    GenServer.cast(server, {:crash, reason})
  end
end
```

## Module 4: Introduction to Supervisors

### Learning Objectives
- Understand supervision principles
- Implement basic supervisors
- Configure restart strategies

### Concepts Covered
1. **Supervisor Basics**
   - `use Supervisor`
   - Child specifications
   - Starting supervisors

2. **One-for-One Strategy**
   - Individual process restarts
   - Use cases
   - Configuration

3. **Child Specifications**
   - `child_spec/1`
   - Restart options (:permanent, :temporary, :transient)
   - Shutdown specifications

### Interactive Exercises
- Create a supervisor with multiple children
- Kill processes and observe restarts
- Modify restart strategies
- Track restart counts

### Visual Elements
- Supervision tree diagram
- Restart animation
- Strategy comparison view

## Module 5: Advanced Supervision Strategies

### Learning Objectives
- Master all supervision strategies
- Design supervision hierarchies
- Handle complex failure scenarios

### Concepts Covered
1. **All-for-One Strategy**
   - Full sibling restart
   - Use cases (dependent processes)
   - Performance implications

2. **Rest-for-One Strategy**
   - Ordered restarts
   - Dependency chains
   - Sequential startup

3. **Supervision Hierarchies**
   - Nested supervisors
   - Tree design patterns
   - Error isolation levels

### Interactive Exercises
- Build a multi-level supervision tree
- Compare strategy behaviors side-by-side
- Design supervision for a chat application
- Implement cascading failure scenarios

### Sandbox Scenarios
```elixir
defmodule SupervisionScenarios do
  # Database connection pool supervisor
  def database_pool_demo do
    # Shows rest_for_one for connection dependencies
  end
  
  # Web server supervisor
  def web_server_demo do
    # Shows one_for_all for tightly coupled components
  end
  
  # Game server supervisor
  def game_server_demo do
    # Shows dynamic supervisor for player processes
  end
end
```

## Module 6: Dynamic Supervisors

### Learning Objectives
- Understand dynamic child management
- Implement runtime process creation
- Handle resource pooling

### Concepts Covered
1. **DynamicSupervisor**
   - Starting children at runtime
   - Child lifecycle management
   - Use cases

2. **Process Pools**
   - Pool supervision patterns
   - Load balancing
   - Resource limits

3. **Temporary Processes**
   - Short-lived process management
   - Cleanup strategies
   - Memory considerations

### Interactive Exercises
- Build a worker pool
- Implement a task processor
- Create a connection pool
- Dynamic process visualization

## Module 7: Real-World Patterns

### Learning Objectives
- Apply supervision to real scenarios
- Avoid common anti-patterns
- Design production systems

### Concepts Covered
1. **Common Patterns**
   - Registry patterns
   - Circuit breakers
   - Bulkheads
   - Back-pressure

2. **Anti-Pattern Recognition**
   - Process bottlenecks
   - Supervision mistakes
   - State management errors

3. **Production Considerations**
   - Error logging
   - Metrics collection
   - Debugging strategies

### Interactive Exercises
- Design a chat application supervision tree
- Implement a job processing system
- Build a game server with player management
- Create a IoT device manager

## Assessment Framework

### Knowledge Checks
- Multiple choice questions after each module
- Code completion exercises
- Architecture design challenges

### Practical Assessments
- Build working supervision trees
- Debug failing systems
- Optimize supervision strategies
- Explain design decisions

### Certification Path
1. **Basic Certification**: Modules 1-4
2. **Advanced Certification**: Modules 5-6
3. **Expert Certification**: All modules + real-world project

## Implementation Notes

### For Phase 1
- Focus on Modules 1-4 basics
- Simple visual representations
- Core GenServer and Supervisor examples

### For Phase 2
- Add Modules 5-6
- Enhanced visualizations
- Complex scenarios

### For Phase 3
- Complete Module 7
- Assessment system
- Certification framework

## Success Metrics

- Users can explain process supervision
- Users can build proper supervision trees
- Users can debug supervision issues
- Users avoid common anti-patterns
- Users feel confident with OTP