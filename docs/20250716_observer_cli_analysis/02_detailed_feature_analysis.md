# Detailed Feature Analysis: Observer CLI

## Core Monitoring Features

### 1. Process Monitoring (`observer_cli_process.erl`)

Observer CLI provides sophisticated process monitoring with multiple sorting strategies:

**Key Features:**
- **Top N Processes** by various metrics:
  - Memory usage (`recon:proc_count(memory, N)`)
  - Reductions (CPU work)
  - Binary memory
  - Total heap size
  - Message queue length
- **Sliding Window Analysis** (`recon:proc_window/3`)
  - Shows process behavior over time intervals
  - Identifies processes with growing resource usage
  - Catches intermittent spikes

**Process Details View:**
- Registered name
- Current/initial function
- Process state (running, waiting, suspended, garbage_collecting)
- Memory breakdown (heap, stack, tables)
- Message queue contents (truncated for safety)
- Process dictionary
- Current stacktrace
- Monitors and links
- Group leader
- Trap exit status

**Implementation Insights:**
```erlang
% Sorting attributes used
-define(STABLE_PROCESS_ATTRS, [
    pid, name, init_call, reductions, memory, 
    message_queue_len, current_function, status
]).
```

### 2. System Monitoring (`observer_cli_system.erl`)

Comprehensive system-wide metrics with focus on production relevance:

**Memory Allocators:**
- Tracks 9 main allocators (binary, driver, eheap, ets, fix, ll, sl, std, temp)
- Shows average block sizes (current vs max)
- SBCS to MBCS ratio (single vs multi-block carriers)
- Cache hit rates for allocator efficiency

**Key Metrics:**
- OS process info (CPU%, memory%, RSS, VSZ)
- Distributed node queue sizes
- Per-scheduler utilization
- System limits usage (process count, port count, atom count)

**Production Insights:**
- Red highlighting when usage > 85% of limits
- Incremental values show growth between refreshes
- Scheduler wall time for true CPU utilization

### 3. Network/Port Monitoring (`observer_cli_port.erl` & `observer_cli_inet.erl`)

**Port Statistics:**
- Input/output bytes
- Queue sizes
- Connected process
- Port type and OS PID
- Memory usage

**Network Socket Analysis:**
- Top consumers by:
  - Bytes sent/received (`oct`)
  - Packets sent/received (`cnt`)
  - Combined metrics
- Socket options inspection
- Peer/local address information
- TCP/UDP/SCTP statistics

### 4. ETS Table Analysis (`observer_cli_ets.erl`)

**Table Metrics:**
- Memory usage per table
- Number of objects
- Table type (set, ordered_set, bag, duplicate_bag)
- Owner process
- Read/write concurrency settings
- Heir process
- Protection level

**Sorting Options:**
- By memory size
- By object count
- By table ID

### 5. Mnesia Monitoring (`observer_cli_mnesia.erl`)

**Table Information:**
- Size and memory usage
- Storage type (ram_copies, disc_copies, disc_only_copies)
- Attributes and indices
- Table properties
- Load order and priority

### 6. Real-time Updating

**Refresh Mechanism:**
- Configurable intervals (minimum 1000ms)
- Smart refresh using timers
- Keyboard controls:
  - `p` - pause
  - `r/rr` - sort by reductions
  - `m/mm` - sort by memory
  - `b/bb` - sort by binary memory
  - Page navigation with `F/B`

## Advanced Features

### 1. Remote Node Monitoring

```erlang
observer_cli:start('target@host', 'cookie').
```
- Hidden node connections
- RPC-based monitoring
- Cookie authentication

### 2. Escriptize Support

- Standalone executable generation
- No Erlang installation required on client
- Command: `observer_cli TARGETNODE [TARGETCOOKIE REFRESHMS]`

### 3. Plugin System (`observer_cli_plugin.erl`)

- Extensible architecture
- Custom view registration
- Plugin-specific refresh rates

### 4. Store System (`observer_cli_store.erl`)

- ETS-based data storage
- Metric history retention
- Cross-view data sharing

## Performance Optimizations

### 1. Data Collection
- Uses `recon` library for safe production introspection
- Avoids expensive operations
- Truncates large data structures
- Timeout protection

### 2. Display Efficiency
- ANSI escape codes for terminal control
- Minimal screen redraws
- Smart cursor positioning
- Color coding for quick scanning

### 3. Resource Usage
- Minimal memory footprint
- No persistent storage
- Efficient data structures
- Garbage collection friendly

## Integration Points for OTP Supervisor

### High-Value Features to Port

1. **Process Sorting Algorithms**
   - Implement recon-style sorting in ProcessListWidget
   - Add reduction rate calculations
   - Binary memory tracking per process

2. **Allocator Dashboard**
   - New LiveView page for memory allocators
   - Cache hit rate monitoring
   - Fragmentation analysis

3. **Network Analytics**
   - Enhance port monitoring with socket statistics
   - Add bandwidth usage tracking
   - Connection pool analysis

4. **System Limits Monitoring**
   - Add progress bars for system limits
   - Alert when approaching limits
   - Historical limit usage

5. **Sliding Window Analysis**
   - Implement time-based process analysis
   - Identify resource usage trends
   - Catch intermittent issues

### Architecture Considerations

1. **Data Collection Service**
   - GenServer for periodic metric collection
   - ETS tables for metric storage
   - PubSub for real-time updates

2. **API Endpoints**
   - `/api/v1/allocators` - Memory allocator stats
   - `/api/v1/ports/:id` - Detailed port info
   - `/api/v1/processes/top/:metric` - Top N processes

3. **LiveView Components**
   - `AllocatorWidget` - Memory allocator visualization
   - `PortDetailWidget` - Port statistics display
   - `SystemLimitsWidget` - Limit usage indicators

4. **Performance Considerations**
   - Cache expensive calculations
   - Batch metric collection
   - Throttle update frequency
   - Progressive data loading