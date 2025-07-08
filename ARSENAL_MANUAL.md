# ARSENAL Operations Manual

**The Complete Guide to OTP Process Management via REST API**

---

## Table of Contents

1. [Overview](#overview)
2. [Getting Started](#getting-started)
3. [Operation Reference](#operation-reference)
   - [GetProcessInfo](#getprocessinfo)
   - [KillProcess](#killprocess)
   - [ListSupervisors](#listsupervisors)
   - [SendMessage](#sendmessage)
   - [TraceProcess](#traceprocess)
4. [Common Patterns](#common-patterns)
5. [Error Handling](#error-handling)
6. [Security Considerations](#security-considerations)
7. [Advanced Usage](#advanced-usage)

---

## Overview

Arsenal is a comprehensive REST API framework for OTP process management that exposes Erlang/Elixir system internals through clean, HTTP-based interfaces. This manual covers the 5 core operations that form the foundation of Arsenal's process management capabilities.

### Core Philosophy

- **Safety First**: Critical process protection prevents accidental system damage
- **Comprehensive Information**: Deep process introspection and monitoring
- **Flexible Communication**: Multiple message passing patterns (send/cast/call)
- **Production Ready**: Built-in error handling, validation, and observability

### Base URL Structure

All Arsenal operations follow the pattern:
```
GET|POST|DELETE /api/v1/{resource}/{identifier?}/{action?}
```

---

## Getting Started

### Prerequisites

- Elixir/Erlang system with OTP processes running
- Arsenal framework installed and configured
- HTTP client (curl, Postman, etc.)

### Quick Example

```bash
# Get information about process <0.1.0>
curl -X GET "http://localhost:4000/api/v1/processes/%3C0.1.0%3E/info"

# List all supervisors in the system
curl -X GET "http://localhost:4000/api/v1/supervisors"
```

---

## Operation Reference

## GetProcessInfo

**Retrieve comprehensive information about any OTP process**

### Endpoint
```
GET /api/v1/processes/:pid/info
```

### Description
GetProcessInfo provides deep introspection into process state, memory usage, message queues, links, monitors, and other OTP-specific information. Essential for debugging, monitoring, and system analysis.

### Parameters

| Parameter | Type | Location | Required | Description |
|-----------|------|----------|----------|-------------|
| `pid` | string | path | ✅ | Process ID in format `<0.1.0>` or `#PID<0.1.0>` |
| `keys` | array | query | ❌ | Specific info keys to retrieve (see available keys below) |

### Available Info Keys

**Memory & Performance:**
- `memory` - Total memory used by process (bytes)
- `total_heap_size` - Total heap size
- `heap_size` - Current heap size
- `stack_size` - Stack size
- `reductions` - Number of reductions consumed

**State & Status:**
- `status` - Process status (running, waiting, etc.)
- `message_queue_len` - Number of messages in mailbox
- `current_function` - Currently executing function
- `current_location` - Current code location
- `initial_call` - Function that started the process

**Relationships:**
- `links` - Linked processes
- `monitors` - Processes this process monitors
- `monitored_by` - Processes monitoring this process
- `group_leader` - Process group leader

**Configuration:**
- `trap_exit` - Whether process traps exits
- `priority` - Process priority level
- `registered_name` - Registered name (if any)
- `min_heap_size` - Minimum heap size
- `max_heap_size` - Maximum heap size

### Request Examples

**Basic process info:**
```bash
curl -X GET "http://localhost:4000/api/v1/processes/%3C0.1.0%3E/info"
```

**Specific keys only:**
```bash
curl -X GET "http://localhost:4000/api/v1/processes/%3C0.1.0%3E/info?keys[]=memory&keys[]=message_queue_len&keys[]=status"
```

### Response Format

```json
{
  "data": {
    "memory": 2840,
    "message_queue_len": 0,
    "status": "waiting",
    "links": ["#PID<0.2.0>", "#PID<0.3.0>"],
    "monitors": [],
    "monitored_by": ["#PID<0.4.0>"],
    "registered_name": "my_process",
    "current_function": "{gen_server, loop, 7}",
    "initial_call": "{MyApp.Server, init, 1}",
    "reductions": 1247,
    "heap_size": 610,
    "total_heap_size": 987,
    "stack_size": 24,
    "trap_exit": false,
    "priority": "normal"
  }
}
```

### Error Responses

| Code | Description | Response |
|------|-------------|----------|
| 400 | Invalid PID format | `{"error": {"message": "Invalid PID format", "code": "invalid_parameter"}}` |
| 404 | Process not found | `{"error": {"message": "Process not found", "code": "process_not_found"}}` |
| 422 | Invalid info keys | `{"error": {"message": "Invalid process info key: invalid_key", "code": "invalid_parameter"}}` |

### Use Cases

- **Performance Monitoring**: Track memory usage and reductions
- **Debugging**: Examine process state and message queues
- **System Analysis**: Understand process relationships and dependencies
- **Health Checks**: Monitor process status and responsiveness

---

## KillProcess

**Safely terminate processes with configurable protection mechanisms**

### Endpoint
```
DELETE /api/v1/processes/:pid
```

### Description
KillProcess provides controlled process termination with built-in safety mechanisms to prevent accidental termination of critical system processes. Supports various termination reasons and force override capabilities.

### Parameters

| Parameter | Type | Location | Required | Description |
|-----------|------|----------|----------|-------------|
| `pid` | string | path | ✅ | Process ID to terminate |
| `reason` | string | body | ❌ | Termination reason (default: "killed") |
| `force` | boolean | body | ❌ | Force termination of critical processes |

### Termination Reasons

**Standard Reasons:**
- `normal` - Normal process termination
- `shutdown` - Graceful shutdown
- `killed` - Explicit kill (default)
- `kill` - Brutal kill (non-trappable)
- Custom atoms - Any string converted to atom

### Critical Process Protection

Arsenal automatically identifies and protects critical system processes:

**System Processes:**
- `kernel_sup` - Kernel supervisor
- `application_controller` - Application controller
- `code_server` - Code server

**Supervisor Detection:**
- Processes with `{Supervisor, :init, 1}` initial call
- System application supervisors
- Main application tree supervisors

### Request Examples

**Basic termination:**
```bash
curl -X DELETE "http://localhost:4000/api/v1/processes/%3C0.123.0%3E"
```

**Custom termination reason:**
```bash
curl -X DELETE "http://localhost:4000/api/v1/processes/%3C0.123.0%3E" \
  -H "Content-Type: application/json" \
  -d '{"reason": "shutdown"}'
```

**Force terminate critical process:**
```bash
curl -X DELETE "http://localhost:4000/api/v1/processes/%3C0.123.0%3E" \
  -H "Content-Type: application/json" \
  -d '{"reason": "kill", "force": true}'
```

### Response Format

```json
{
  "data": {
    "pid": "#PID<0.123.0>",
    "reason": "killed",
    "terminated": true,
    "timestamp": "2024-01-15T14:30:00.000Z"
  }
}
```

### Error Responses

| Code | Description | Response |
|------|-------------|----------|
| 400 | Invalid PID format | `{"error": {"message": "Invalid PID format", "code": "invalid_parameter"}}` |
| 403 | Critical process protection | `{"error": {"message": "Cannot terminate critical process", "code": "critical_process_protection"}}` |
| 404 | Process not found | `{"error": {"message": "Process not found", "code": "process_not_found"}}` |

### Use Cases

- **Process Cleanup**: Remove stuck or problematic processes
- **Resource Management**: Free up system resources
- **Emergency Response**: Force-kill runaway processes
- **Testing**: Simulate process failures for testing

---

## ListSupervisors

**Discover and analyze the supervision tree structure**

### Endpoint
```
GET /api/v1/supervisors
```

### Description
ListSupervisors provides comprehensive discovery and analysis of all supervisors in the system, including their children, strategies, and application associations. Supports filtering, pagination, and detailed child information.

### Parameters

| Parameter | Type | Location | Required | Description |
|-----------|------|----------|----------|-------------|
| `include_children` | boolean | query | ❌ | Include children information for each supervisor |
| `filter_application` | string | query | ❌ | Filter supervisors by application name |
| `page` | integer | query | ❌ | Page number for pagination (default: 1) |
| `per_page` | integer | query | ❌ | Items per page (default: 50, max: 100) |

### Supervisor Types Detected

**Standard Supervisors:**
- `Supervisor` - Basic OTP supervisor
- `DynamicSupervisor` - Dynamic child management
- `Task.Supervisor` - Task-specific supervision

### Request Examples

**Basic supervisor list:**
```bash
curl -X GET "http://localhost:4000/api/v1/supervisors"
```

**With children information:**
```bash
curl -X GET "http://localhost:4000/api/v1/supervisors?include_children=true"
```

**Filter by application:**
```bash
curl -X GET "http://localhost:4000/api/v1/supervisors?filter_application=my_app"
```

**Paginated results:**
```bash
curl -X GET "http://localhost:4000/api/v1/supervisors?page=2&per_page=25"
```

### Response Format

```json
{
  "data": [
    {
      "name": "MyApp.Supervisor",
      "pid": "#PID<0.123.0>",
      "alive": true,
      "child_count": 3,
      "strategy": "one_for_one",
      "application": "my_app",
      "children": [
        {
          "id": "worker_1",
          "pid": "#PID<0.124.0>",
          "type": "worker",
          "modules": ["MyApp.Worker"]
        },
        {
          "id": "worker_2",
          "pid": "#PID<0.125.0>",
          "type": "worker",
          "modules": ["MyApp.Worker"]
        }
      ]
    }
  ],
  "meta": {
    "total": 15,
    "page": 1,
    "per_page": 50,
    "total_pages": 1
  }
}
```

### Supervisor Strategies

- `one_for_one` - Restart only failed child
- `one_for_all` - Restart all children when one fails
- `rest_for_one` - Restart failed child and all started after it
- `simple_one_for_one` - Simple one-for-one dynamic supervision

### Child Information Fields

When `include_children=true`:

| Field | Description |
|-------|-------------|
| `id` | Child specification ID |
| `pid` | Child process PID |
| `type` | Child type (`worker` or `supervisor`) |
| `modules` | List of modules implementing the child |

### Use Cases

- **System Architecture**: Understand supervision tree structure
- **Monitoring**: Track supervisor health and child counts
- **Debugging**: Analyze supervisor restart patterns
- **Documentation**: Generate system architecture diagrams

---

## SendMessage

**Flexible message passing with multiple delivery patterns**

### Endpoint
```
POST /api/v1/processes/:pid/send
```

### Description
SendMessage enables flexible inter-process communication supporting multiple delivery patterns (send, cast, call) with configurable timeouts and response tracking. Essential for debugging, testing, and system integration.

### Parameters

| Parameter | Type | Location | Required | Description |
|-----------|------|----------|----------|-------------|
| `pid` | string | path | ✅ | Target process ID |
| `message` | object | body | ✅ | Message content to send |
| `message_type` | string | body | ❌ | Delivery pattern: "send", "cast", "call" (default: "send") |
| `timeout_ms` | integer | body | ❌ | Timeout for "call" messages (default: 5000) |
| `track_response` | boolean | body | ❌ | Enable response tracking for debugging |

### Message Types

**Send (Asynchronous):**
- Direct message sending using `send/2`
- Fire-and-forget delivery
- No response expected
- Fastest delivery method

**Cast (GenServer Asynchronous):**
- GenServer cast using `GenServer.cast/2`
- Asynchronous GenServer message
- No response expected
- Requires target to be GenServer

**Call (GenServer Synchronous):**
- GenServer call using `GenServer.call/3`
- Synchronous request-response
- Waits for response with timeout
- Returns actual response data

### Message Format

**Simple Message:**
```json
{
  "message": {
    "content": "Hello, World!"
  }
}
```

**Structured Message with Type:**
```json
{
  "message": {
    "type": "get_state",
    "content": {
      "keys": ["status", "data"]
    }
  }
}
```

This becomes the tuple: `{:get_state, %{"keys" => ["status", "data"]}}`

### Request Examples

**Basic send:**
```bash
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/send" \
  -H "Content-Type: application/json" \
  -d '{
    "message": {"action": "ping"}
  }'
```

**GenServer cast:**
```bash
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/send" \
  -H "Content-Type: application/json" \
  -d '{
    "message": {"action": "update_state", "data": {"key": "value"}},
    "message_type": "cast"
  }'
```

**GenServer call with timeout:**
```bash
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/send" \
  -H "Content-Type: application/json" \
  -d '{
    "message": {"action": "get_state"},
    "message_type": "call",
    "timeout_ms": 10000
  }'
```

**Tracked send (for debugging):**
```bash
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/send" \
  -H "Content-Type: application/json" \
  -d '{
    "message": {"debug": "test"},
    "track_response": true
  }'
```

### Response Format

**Send Response:**
```json
{
  "data": {
    "message_sent": true,
    "message_type": "send",
    "target_pid": "#PID<0.123.0>"
  }
}
```

**Call Response (with actual response):**
```json
{
  "data": {
    "message_sent": true,
    "message_type": "call",
    "target_pid": "#PID<0.123.0>",
    "response": {
      "status": "ok",
      "state": {"counter": 42, "name": "my_server"}
    }
  }
}
```

**Tracked Send Response:**
```json
{
  "data": {
    "message_sent": true,
    "message_type": "send",
    "target_pid": "#PID<0.123.0>",
    "tracking_ref": "#Reference<0.1234567890.1234567890.123456>",
    "note": "Message sent but response tracking requires target process cooperation"
  }
}
```

### Error Responses

| Code | Description | Response |
|------|-------------|----------|
| 400 | Invalid message type | `{"error": {"message": "Invalid message type", "code": "invalid_parameter"}}` |
| 404 | Process not found | `{"error": {"message": "Process not found", "code": "process_not_found"}}` |
| 408 | Call timeout | `{"error": {"message": "Call timeout", "code": "call_timeout"}}` |

### Use Cases

- **Testing**: Send test messages to processes during development
- **Debugging**: Inspect process state via GenServer calls
- **Integration**: Trigger actions in external processes
- **Monitoring**: Ping processes to check responsiveness

---

## TraceProcess

**Real-time process execution tracing and debugging**

### Endpoint
```
POST /api/v1/processes/:pid/trace
```

### Description
TraceProcess provides comprehensive real-time tracing of process execution, including message passing, function calls, garbage collection, and other OTP events. Essential for deep debugging and performance analysis.

### Parameters

| Parameter | Type | Location | Required | Description |
|-----------|------|----------|----------|-------------|
| `pid` | string | path | ✅ | Process ID to trace |
| `trace_flags` | array | body | ❌ | Types of events to trace (default: ["send", "receive"]) |
| `duration_ms` | integer | body | ❌ | Trace duration in milliseconds (default: 60000) |
| `max_events` | integer | body | ❌ | Maximum events to collect (default: 1000, max: 10000) |
| `filter_patterns` | array | body | ❌ | Event type patterns to filter |

### Trace Flags

**Message Tracing:**
- `send` - Messages sent by the process
- `receive` - Messages received by the process

**Function Tracing:**
- `call` - Function calls (requires additional setup)
- `procs` - Process creation and termination

**System Tracing:**
- `garbage_collection` - Garbage collection events
- `running` - Process scheduling events
- `set_on_spawn` - Automatically trace spawned processes

### Storage System

TraceProcess uses a dedicated ETS storage system (`TraceSessionStorage`) that:
- Manages trace session metadata
- Provides concurrent access to trace information
- Automatically cleans up expired sessions
- Supports trace session retrieval and management

### Request Examples

**Basic message tracing:**
```bash
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/trace" \
  -H "Content-Type: application/json" \
  -d '{
    "trace_flags": ["send", "receive"],
    "duration_ms": 30000
  }'
```

**Comprehensive tracing:**
```bash
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/trace" \
  -H "Content-Type: application/json" \
  -d '{
    "trace_flags": ["send", "receive", "procs", "garbage_collection"],
    "duration_ms": 120000,
    "max_events": 5000,
    "filter_patterns": ["send", "receive"]
  }'
```

**Short debugging session:**
```bash
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/trace" \
  -H "Content-Type: application/json" \
  -d '{
    "trace_flags": ["send", "receive"],
    "duration_ms": 5000,
    "max_events": 100
  }'
```

### Response Format

```json
{
  "data": {
    "tracing": true,
    "trace_id": "a1b2c3d4e5f6g7h8",
    "flags": ["send", "receive"],
    "duration_ms": 30000,
    "max_events": 1000,
    "collector_pid": "#PID<0.456.0>"
  }
}
```

### Trace Events

Each trace event contains:

| Field | Description |
|-------|-------------|
| `pid` | Source process PID |
| `type` | Event type (send, receive, etc.) |
| `data` | Event-specific data |
| `timestamp` | Erlang timestamp |

### Trace Session Management

**Session Lifecycle:**
1. Trace session created with unique ID
2. Collector process started to gather events
3. Tracing enabled for specified duration
4. Events collected and stored
5. Automatic cleanup after duration expires

**Session Retrieval:**
Trace sessions are stored in ETS and can be retrieved using the trace ID for later analysis.

### Error Responses

| Code | Description | Response |
|------|-------------|----------|
| 400 | Invalid trace flags | `{"error": {"message": "Invalid trace flag", "code": "invalid_parameter"}}` |
| 404 | Process not found | `{"error": {"message": "Process not found", "code": "process_not_found"}}` |
| 422 | Invalid parameters | `{"error": {"message": "Invalid parameter", "code": "invalid_parameter"}}` |

### Use Cases

- **Performance Debugging**: Analyze message passing patterns
- **Bottleneck Detection**: Identify slow operations
- **Protocol Analysis**: Understand inter-process communication
- **System Optimization**: Profile process behavior

---

## Common Patterns

### Process Discovery Workflow

```bash
# 1. List all supervisors to understand system structure
curl -X GET "http://localhost:4000/api/v1/supervisors?include_children=true"

# 2. Get detailed info about specific processes
curl -X GET "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/info"

# 3. Monitor message activity with tracing
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/trace" \
  -H "Content-Type: application/json" \
  -d '{"trace_flags": ["send", "receive"], "duration_ms": 10000}'
```

### Health Check Pattern

```bash
# Check if process is alive and responsive
curl -X GET "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/info?keys[]=status&keys[]=message_queue_len"

# Send ping message to test responsiveness
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/send" \
  -H "Content-Type: application/json" \
  -d '{"message": {"type": "ping"}, "message_type": "call", "timeout_ms": 1000}'
```

### Debug Stuck Process

```bash
# 1. Check process status and message queue
curl -X GET "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/info?keys[]=status&keys[]=message_queue_len&keys[]=current_function"

# 2. Enable tracing to see what's happening
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/trace" \
  -H "Content-Type: application/json" \
  -d '{"trace_flags": ["send", "receive", "running"], "duration_ms": 15000}'

# 3. Try sending a message to unstick
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/send" \
  -H "Content-Type: application/json" \
  -d '{"message": {"action": "status_check"}, "message_type": "cast"}'

# 4. If still stuck, terminate safely
curl -X DELETE "http://localhost:4000/api/v1/processes/%3C0.123.0%3E" \
  -H "Content-Type: application/json" \
  -d '{"reason": "stuck_process"}'
```

---

## Error Handling

### Standard Error Format

All Arsenal operations use consistent error formatting:

```json
{
  "error": {
    "message": "Human readable error description",
    "code": "machine_readable_error_code",
    "details": "Additional context or debug information",
    "timestamp": "2024-01-15T14:30:00.000Z"
  }
}
```

### Common Error Codes

| Code | Description | Operations |
|------|-------------|------------|
| `invalid_parameter` | Parameter validation failed | All |
| `missing_parameter` | Required parameter missing | All |
| `process_not_found` | Target process doesn't exist | All |
| `critical_process_protection` | Cannot modify critical process | KillProcess |
| `call_timeout` | GenServer call timed out | SendMessage |
| `discovery_failed` | System discovery error | ListSupervisors |
| `tracing_failed` | Trace setup failed | TraceProcess |

### Error Recovery Strategies

**Process Not Found:**
- Check if PID format is correct
- Verify process is still alive
- Use ListSupervisors to find processes

**Critical Process Protection:**
- Use `force: true` parameter with caution
- Verify you really need to terminate the process
- Consider alternative approaches

**Call Timeout:**
- Increase timeout value
- Check if target process is responsive
- Use tracing to understand what's blocking

---

## Security Considerations

### Built-in Protection Mechanisms

**Critical Process Protection:**
- Automatic detection of system-critical processes
- Kernel supervisor protection
- Application controller protection
- Supervisor tree protection

**Parameter Validation:**
- Strict PID format validation
- Type checking for all parameters
- Range validation for numeric values
- Whitelist validation for enumerated values

**Resource Limits:**
- Maximum trace events per session
- Timeout limits for calls
- Pagination limits for large result sets
- Memory-bounded trace collection

### Production Deployment

**Access Control:**
Consider implementing additional layers:
- Authentication for API access
- Role-based authorization
- Rate limiting for expensive operations
- Audit logging for all operations

**Network Security:**
- Deploy behind reverse proxy
- Use HTTPS in production
- Implement proper firewall rules
- Monitor for suspicious activity

**Operational Safety:**
- Test operations in development first
- Use read-only operations for monitoring
- Be cautious with KillProcess in production
- Monitor system impact of tracing

---

## Advanced Usage

### Batch Operations

While Arsenal doesn't provide native batch endpoints, you can implement batch patterns:

```bash
#!/bin/bash
# Get info for multiple processes
PIDS=("<0.123.0>" "<0.124.0>" "<0.125.0>")

for pid in "${PIDS[@]}"; do
  encoded_pid=$(echo $pid | sed 's/</\\%3C/g' | sed 's/>/\\%3E/g')
  curl -X GET "http://localhost:4000/api/v1/processes/${encoded_pid}/info" &
done
wait
```

### Integration with Monitoring Systems

**Prometheus Metrics Integration:**
```bash
# Create custom metrics endpoint
curl -X GET "http://localhost:4000/api/v1/supervisors" | \
  jq '.data[] | "supervisor_child_count{name=\"\(.name)\",app=\"\(.application)\"} \(.child_count)"'
```

**Health Check Integration:**
```bash
# System health check script
check_process_health() {
  local pid=$1
  response=$(curl -s -X GET "http://localhost:4000/api/v1/processes/${pid}/info?keys[]=status&keys[]=message_queue_len")
  
  status=$(echo $response | jq -r '.data.status // "unknown"')
  queue_len=$(echo $response | jq -r '.data.message_queue_len // 0')
  
  if [[ "$status" == "waiting" && $queue_len -lt 100 ]]; then
    echo "healthy"
  else
    echo "unhealthy"
  fi
}
```

### Custom Message Protocols

**Implementing Request-Response Pattern:**
```bash
# Send structured request
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/send" \
  -H "Content-Type: application/json" \
  -d '{
    "message": {
      "type": "api_request",
      "content": {
        "method": "get_user",
        "params": {"user_id": 123},
        "request_id": "req_001"
      }
    },
    "message_type": "call",
    "timeout_ms": 5000
  }'
```

### Performance Monitoring

**Long-term Tracing for Performance Analysis:**
```bash
# Start long-running trace for performance analysis
curl -X POST "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/trace" \
  -H "Content-Type: application/json" \
  -d '{
    "trace_flags": ["send", "receive", "garbage_collection"],
    "duration_ms": 300000,
    "max_events": 10000,
    "filter_patterns": []
  }'
```

**Memory Usage Monitoring:**
```bash
# Monitor memory usage over time
while true; do
  timestamp=$(date -Iseconds)
  memory=$(curl -s -X GET "http://localhost:4000/api/v1/processes/%3C0.123.0%3E/info?keys[]=memory" | jq '.data.memory')
  echo "$timestamp,$memory"
  sleep 10
done > memory_usage.csv
```

---

## Conclusion

Arsenal's 5 core operations provide a comprehensive foundation for OTP process management via REST API. These operations enable:

- **Complete System Visibility**: Understand process relationships and system architecture
- **Safe Process Management**: Terminate processes with built-in safety mechanisms  
- **Flexible Communication**: Support multiple message passing patterns
- **Deep Debugging**: Real-time tracing and execution analysis
- **Production Monitoring**: Monitor system health and performance

The combination of these operations enables both development debugging and production monitoring workflows, making Arsenal an essential tool for any Elixir/OTP system management toolkit.

For additional operations and advanced features, refer to the complete Arsenal documentation and consider the extended operation library for specialized use cases.