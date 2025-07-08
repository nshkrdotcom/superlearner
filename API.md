# OTP Supervisor REST API Documentation

This document describes the REST API endpoints for the OTP Supervisor Educational Tool.

## Base URL

All API endpoints are prefixed with `/api/v1`.

## Authentication

Currently, the API uses basic HTTP headers. In production environments, consider implementing:
- API key authentication
- Rate limiting
- IP allowlisting

## Response Format

### Success Responses

```json
{
  "data": {...},
  "meta": {...}  // Optional metadata like pagination
}
```

### Error Responses

```json
{
  "error": {
    "message": "Human-readable error message",
    "code": "machine_readable_error_code",
    "details": "Additional context (optional)"
  }
}
```

## Endpoints

### Process Management

#### GET /api/v1/processes

Lists all processes in the system with optional filtering and pagination.

**Query Parameters:**
- `page` (integer, optional): Page number (default: 1)
- `per_page` (integer, optional): Items per page (default: 50, max: 100)
- `type` (string, optional): Filter by process type (`supervisor`, `genserver`, `worker`)

**Response:**
```json
{
  "data": [
    {
      "pid": "#PID<0.123.0>",
      "name": "my_process",
      "type": "genserver"
    }
  ],
  "meta": {
    "total": 150,
    "page": 1,
    "per_page": 50,
    "total_pages": 3
  }
}
```

#### GET /api/v1/processes/:pid/info

Get comprehensive information about a specific process (Arsenal Operation).

**URL Parameters:**
- `pid` (string): Process ID (URL encoded)

**Query Parameters:**
- `keys` (array, optional): Specific info keys to retrieve

**Response:**
```json
{
  "data": {
    "pid": "#PID<0.123.0>",
    "name": "my_process",
    "type": "genserver",
    "memory": 2832,
    "message_queue_len": 0,
    "links": ["#PID<0.124.0>"],
    "monitors": [],
    "status": "running"
  }
}
```

#### GET /api/v1/processes/:pid/state

Get the internal state of a GenServer process.

**URL Parameters:**
- `pid` (string): Process ID (URL encoded)

**Response:**
```json
{
  "data": {
    "state": {...},
    "type": "genserver"
  }
}
```

**Errors:**
- `422` - Process is not a GenServer

#### POST /api/v1/processes/:pid/trace

Start process tracing (Arsenal Operation).

**URL Parameters:**
- `pid` (string): Process ID (URL encoded)

**Request Body:**
```json
{
  "trace_flags": ["send", "receive", "call"],
  "duration_ms": 60000,
  "max_events": 1000,
  "filter_patterns": ["*cast*"]
}
```

**Response:**
```json
{
  "data": {
    "status": "tracing_started",
    "trace_session_id": "trace_12345",
    "trace_flags": ["send", "receive", "call"],
    "duration_ms": 60000,
    "max_events": 1000
  }
}
```

#### DELETE /api/v1/processes/:pid/trace

Stop message tracing for a process.

**URL Parameters:**
- `pid` (string): Process ID (URL encoded)

**Response:**
```json
{
  "data": {
    "status": "tracing_stopped"
  }
}
```

#### GET /api/v1/processes/:pid/messages

Get message history for a traced process.

**URL Parameters:**
- `pid` (string): Process ID (URL encoded)

**Response:**
```json
{
  "data": {
    "messages": [
      {
        "timestamp": 1234567890,
        "direction": "incoming",
        "content": {...}
      }
    ],
    "total": 10
  }
}
```

#### POST /api/v1/processes/:pid/message

Send a message to a process (Arsenal Operation).

**URL Parameters:**
- `pid` (string): Process ID (URL encoded)

**Request Body:**
```json
{
  "message": "increment",
  "message_type": "cast",
  "timeout_ms": 5000,
  "track_response": true
}
```

**Response:**
```json
{
  "data": {
    "status": "message_sent",
    "message_type": "cast",
    "delivery_time": 1234567890,
    "response": null
  }
}
```

#### DELETE /api/v1/processes/:pid

Kill/terminate a process (Arsenal Operation).

**URL Parameters:**
- `pid` (string): Process ID (URL encoded)

**Request Body:**
```json
{
  "reason": "killed",
  "force": false
}
```

**Response:**
```json
{
  "data": {
    "status": "process_killed",
    "pid": "#PID<0.123.0>",
    "reason": "killed",
    "termination_time": 1234567890
  }
}
```

### System Analysis

#### GET /api/v1/system/health

Get system health information.

**Response:**
```json
{
  "data": {
    "status": "healthy",
    "metrics": {
      "total_processes": 150,
      "memory_usage": 52428800,
      "message_queue_lengths": 0,
      "supervision_health": 95.5
    },
    "timestamp": 1234567890
  }
}
```

#### GET /api/v1/system/graph

Get complete process relationship graph.

**Response:**
```json
{
  "data": {
    "processes": [...],
    "links": [...],
    "monitors": [...]
  }
}
```

#### GET /api/v1/system/bottlenecks

Identify system bottlenecks.

**Response:**
```json
{
  "data": {
    "bottlenecks": [...],
    "analysis": {...}
  }
}
```

#### GET /api/v1/system/anomalies

Detect system anomalies.

**Response:**
```json
{
  "data": {
    "anomalies": [...],
    "summary": {...}
  }
}
```

### Supervisor Management

#### GET /api/v1/supervisors

List all supervisors in the system (Arsenal Operation).

**Query Parameters:**
- `include_children` (boolean, optional): Include children information
- `filter_application` (string, optional): Filter by application name  
- `page` (integer, optional): Page number (default: 1)
- `per_page` (integer, optional): Items per page (default: 50, max: 100)

**Response:**
```json
{
  "data": [
    {
      "name": "my_supervisor",
      "pid": "#PID<0.123.0>",
      "alive": true,
      "child_count": 3,
      "application": "my_app",
      "strategy": "one_for_one",
      "restart_intensity": 3,
      "restart_period": 5
    }
  ],
  "meta": {
    "total": 5,
    "page": 1,
    "per_page": 50,
    "timestamp": 1234567890
  }
}
```

#### GET /api/v1/supervisors/:name

Get detailed supervisor information.

**URL Parameters:**
- `name` (string): Supervisor name

**Response:**
```json
{
  "data": {
    "name": "my_supervisor",
    "pid": "#PID<0.123.0>",
    "alive": true,
    "child_count": 3,
    "children": [...],
    "strategy": "one_for_one"
  }
}
```

#### GET /api/v1/supervisors/:name/analytics

Get supervisor analytics data.

**URL Parameters:**
- `name` (string): Supervisor name

**Response:**
```json
{
  "data": {
    "performance_metrics": {
      "memory_usage": 2832,
      "message_queue_len": 0,
      "reductions": 1000
    }
  }
}
```

#### PUT /api/v1/supervisors/:name/strategy

Change supervisor restart strategy (Traditional API).

**URL Parameters:**
- `name` (string): Supervisor name

**Request Body:**
```json
{
  "strategy": "one_for_all"
}
```

**Response:**
```json
{
  "data": {
    "status": "strategy_changed",
    "new_strategy": "one_for_all",
    "supervisor": "my_supervisor"
  }
}
```

#### POST /api/v1/supervisors/:name/simulate-failure

Simulate failures for testing (Traditional API).

**URL Parameters:**
- `name` (string): Supervisor name

**Request Body:**
```json
{
  "failure_type": "child_crash",
  "target": "random",
  "reason": "test_crash"
}
```

**Response:**
```json
{
  "data": {
    "status": "failure_simulated",
    "failure_type": "child_crash",
    "supervisor": "my_supervisor"
  }
}
```

## Arsenal Documentation Endpoints

#### GET /api/v1/arsenal/docs

Get OpenAPI documentation for all Arsenal operations.

**Response:**
```json
{
  "openapi": "3.0.0",
  "info": {
    "title": "Arsenal Operations API",
    "version": "1.0.0"
  },
  "paths": {
    "/api/v1/processes/{pid}/info": {
      "get": {
        "summary": "Get comprehensive process information",
        "parameters": [...],
        "responses": {...}
      }
    }
  }
}
```

#### GET /api/v1/arsenal/operations

List all available Arsenal operations with metadata.

**Response:**
```json
{
  "data": [
    {
      "module": "OTPSupervisor.Core.Arsenal.Operations.GetProcessInfo",
      "method": "get",
      "path": "/api/v1/processes/:pid/info",
      "summary": "Get comprehensive process information",
      "parameters": [...]
    }
  ],
  "meta": {
    "total": 5,
    "timestamp": 1234567890
  }
}
```

## Error Codes

- `invalid_pid` - Invalid PID format
- `process_not_found` - Process does not exist
- `not_genserver` - Operation requires GenServer process
- `supervisor_not_found` - Supervisor does not exist
- `invalid_params` - Invalid request parameters
- `trace_failed` - Failed to start/stop tracing
- `invalid_strategy` - Invalid supervisor strategy
- `simulation_failed` - Failed to simulate failure

## Rate Limiting

Currently not implemented. In production, consider:
- 1000 requests per hour per IP
- 100 requests per minute for write operations
- Lower limits for computationally expensive operations

## Examples

### List GenServer processes
```bash
curl "http://localhost:4000/api/v1/processes?type=genserver&per_page=10"
```

### Get process details
```bash
curl "http://localhost:4000/api/v1/processes/%23PID%3C0.123.0%3E"
```

### Start tracing
```bash
curl -X POST "http://localhost:4000/api/v1/processes/%23PID%3C0.123.0%3E/trace" \
  -H "Content-Type: application/json" \
  -d '{"max_messages": 50, "duration": 30}'
```

### Send a message
```bash
curl -X POST "http://localhost:4000/api/v1/processes/%23PID%3C0.123.0%3E/message" \
  -H "Content-Type: application/json" \
  -d '{"message": {"type": "cast", "content": "increment"}}'
```