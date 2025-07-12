# Arsenal API Specification

## Overview

Arsenal provides a comprehensive REST API for OTP system inspection, manipulation, and analysis. The API is designed to be the unified interface for all OTP-related development and debugging tools, serving as the backend for the Cinema Debugger and other visualization tools.

## API Architecture

### Base Configuration

```
Base URL: http://localhost:4000/api/v1
Content-Type: application/json
Authentication: Bearer tokens (development), API keys (production)
```

### Response Format

All responses follow a consistent structure:

```json
{
  "status": "success|error",
  "data": {...},
  "meta": {
    "timestamp": "2025-07-12T10:30:00Z",
    "request_id": "uuid",
    "version": "1.0.0"
  },
  "errors": [...]
}
```

## Process Management API

### List Processes

```http
GET /processes
```

Query Parameters:
- `filter`: `all|alive|dead|registered`
- `sort`: `pid|memory|reductions|message_queue_len`
- `limit`: Number (default: 100)
- `offset`: Number (default: 0)

Response:
```json
{
  "status": "success",
  "data": {
    "processes": [
      {
        "pid": "<0.123.0>",
        "name": "my_genserver",
        "status": "running",
        "memory": 2048,
        "reductions": 15432,
        "message_queue_len": 3,
        "current_function": "gen_server:loop/7",
        "initial_call": "MyGenServer:init/1",
        "registered_name": "my_genserver",
        "links": ["<0.45.0>", "<0.67.0>"],
        "monitors": ["<0.89.0>"],
        "trap_exit": false,
        "heap_size": 1024,
        "stack_size": 256,
        "created_at": "2025-07-12T10:25:00Z"
      }
    ],
    "total_count": 1247,
    "alive_count": 1245,
    "dead_count": 2
  }
}
```

### Get Process Details

```http
GET /processes/{pid}
```

Response:
```json
{
  "status": "success",
  "data": {
    "pid": "<0.123.0>",
    "detailed_info": {
      "dictionary": {},
      "group_leader": "<0.44.0>",
      "error_handler": "error_handler",
      "priority": "normal",
      "suspending": [],
      "current_stacktrace": [
        {
          "module": "gen_server",
          "function": "loop",
          "arity": 7,
          "location": "gen_server.erl:374"
        }
      ],
      "state": {
        "counter": 42,
        "config": {"timeout": 5000}
      }
    }
  }
}
```

### Process State History

```http
GET /processes/{pid}/state-history
```

Query Parameters:
- `from`: ISO timestamp
- `to`: ISO timestamp
- `limit`: Number (default: 100)

Response:
```json
{
  "status": "success",
  "data": {
    "state_changes": [
      {
        "timestamp": "2025-07-12T10:30:00Z",
        "state_before": {"counter": 41},
        "state_after": {"counter": 42},
        "trigger": {
          "type": "handle_call",
          "message": "increment",
          "from": "<0.234.0>"
        }
      }
    ]
  }
}
```

## Supervision Tree API

### Get Supervision Tree

```http
GET /supervision-trees
```

Query Parameters:
- `root_supervisor`: PID (optional, defaults to application supervisor)
- `depth`: Number (default: -1 for unlimited)

Response:
```json
{
  "status": "success",
  "data": {
    "supervision_tree": {
      "supervisor": "<0.45.0>",
      "name": "MyApp.Supervisor",
      "strategy": "one_for_one",
      "intensity": 3,
      "period": 5,
      "children": [
        {
          "id": "my_genserver",
          "pid": "<0.123.0>",
          "type": "worker",
          "restart": "permanent",
          "shutdown": 5000,
          "modules": ["MyGenServer"]
        },
        {
          "id": "sub_supervisor",
          "pid": "<0.124.0>",
          "type": "supervisor",
          "children": [...]
        }
      ]
    }
  }
}
```

### Supervision Events

```http
GET /supervision-events
```

Query Parameters:
- `supervisor`: PID
- `from`: ISO timestamp
- `to`: ISO timestamp
- `event_type`: `start|stop|restart|terminate`

Response:
```json
{
  "status": "success",
  "data": {
    "events": [
      {
        "timestamp": "2025-07-12T10:28:00Z",
        "supervisor": "<0.45.0>",
        "child_id": "my_genserver",
        "child_pid": "<0.123.0>",
        "event_type": "restart",
        "reason": "normal",
        "restart_count": 1
      }
    ]
  }
}
```

## Message Tracing API

### Start Message Trace

```http
POST /message-traces
```

Request Body:
```json
{
  "target": {
    "type": "process|module|function",
    "value": "<0.123.0>|MyModule|{MyModule, my_function, 2}"
  },
  "options": {
    "duration": 30000,
    "max_messages": 1000,
    "include_returns": true,
    "include_exceptions": true
  }
}
```

Response:
```json
{
  "status": "success",
  "data": {
    "trace_id": "trace_uuid",
    "started_at": "2025-07-12T10:30:00Z",
    "expires_at": "2025-07-12T10:30:30Z"
  }
}
```

### Get Trace Messages

```http
GET /message-traces/{trace_id}/messages
```

Response:
```json
{
  "status": "success",
  "data": {
    "messages": [
      {
        "timestamp": "2025-07-12T10:30:01.123Z",
        "type": "call",
        "from": "<0.234.0>",
        "to": "<0.123.0>",
        "message": "increment",
        "result": 42,
        "duration_microseconds": 150
      },
      {
        "timestamp": "2025-07-12T10:30:01.125Z",
        "type": "cast",
        "from": "<0.235.0>",
        "to": "<0.123.0>",
        "message": {"reset", "all"}
      }
    ]
  }
}
```

## Performance Metrics API

### System Metrics

```http
GET /metrics/system
```

Response:
```json
{
  "status": "success",
  "data": {
    "memory": {
      "total": 104857600,
      "processes": 52428800,
      "system": 31457280,
      "atom": 1048576,
      "binary": 20971520
    },
    "cpu": {
      "usage_percent": 15.2,
      "schedulers": 8,
      "scheduler_utilization": [0.12, 0.08, 0.23, 0.05, 0.18, 0.11, 0.07, 0.16]
    },
    "processes": {
      "count": 1247,
      "limit": 262144,
      "usage_percent": 0.47
    },
    "ports": {
      "count": 15,
      "limit": 65536
    },
    "ets_tables": {
      "count": 89,
      "memory": 4194304
    }
  }
}
```

### Process Metrics

```http
GET /metrics/processes/{pid}
```

Response:
```json
{
  "status": "success",
  "data": {
    "memory_usage": [
      {"timestamp": "2025-07-12T10:30:00Z", "bytes": 2048},
      {"timestamp": "2025-07-12T10:30:05Z", "bytes": 2056}
    ],
    "message_queue_length": [
      {"timestamp": "2025-07-12T10:30:00Z", "length": 0},
      {"timestamp": "2025-07-12T10:30:05Z", "length": 3}
    ],
    "reductions": [
      {"timestamp": "2025-07-12T10:30:00Z", "count": 15400},
      {"timestamp": "2025-07-12T10:30:05Z", "count": 15432}
    ]
  }
}
```

## ETS Table Management API

### List ETS Tables

```http
GET /ets-tables
```

Response:
```json
{
  "status": "success",
  "data": {
    "tables": [
      {
        "id": 16400,
        "name": "my_cache",
        "owner": "<0.123.0>",
        "type": "set",
        "protection": "public",
        "memory": 1024,
        "size": 150,
        "node": "node@host"
      }
    ]
  }
}
```

### ETS Table Operations

```http
GET /ets-tables/{table_id}/info
POST /ets-tables/{table_id}/lookup
POST /ets-tables/{table_id}/insert
DELETE /ets-tables/{table_id}/delete
```

## Code Analysis API

### Module Information

```http
GET /modules/{module_name}
```

Response:
```json
{
  "status": "success",
  "data": {
    "module": "MyGenServer",
    "file": "/path/to/my_genserver.ex",
    "exports": [
      {"function": "start_link", "arity": 1},
      {"function": "increment", "arity": 1}
    ],
    "attributes": {
      "behaviour": ["gen_server"],
      "vsn": "1.0.0"
    },
    "compile_info": {
      "version": "8.2.1",
      "time": "2025-07-12T10:00:00Z",
      "source": "/path/to/my_genserver.ex"
    }
  }
}
```

### Function Call Graph

```http
GET /modules/{module_name}/call-graph
```

Response:
```json
{
  "status": "success",
  "data": {
    "nodes": [
      {"id": "MyGenServer:init/1", "type": "function"},
      {"id": "MyGenServer:handle_call/3", "type": "function"}
    ],
    "edges": [
      {"from": "MyGenServer:init/1", "to": "gen_server:init_it/6"}
    ]
  }
}
```

## WebSocket Events API

### Real-time Event Stream

```
WS /events/stream
```

Message Types:
- `process_created`
- `process_terminated`
- `message_sent`
- `state_changed`
- `supervisor_event`
- `performance_alert`

Example Message:
```json
{
  "type": "state_changed",
  "timestamp": "2025-07-12T10:30:01.123Z",
  "data": {
    "pid": "<0.123.0>",
    "module": "MyGenServer",
    "state_before": {"counter": 41},
    "state_after": {"counter": 42}
  }
}
```

## Control Operations API

### Process Control

```http
POST /processes/{pid}/suspend
POST /processes/{pid}/resume
POST /processes/{pid}/kill
POST /processes/{pid}/send-message
```

### System Control

```http
POST /system/gc
POST /system/memory-report
POST /system/scheduler-info
```

## Error Handling

### Error Response Format

```json
{
  "status": "error",
  "errors": [
    {
      "code": "PROCESS_NOT_FOUND",
      "message": "Process <0.123.0> not found",
      "field": "pid",
      "details": {}
    }
  ],
  "meta": {
    "timestamp": "2025-07-12T10:30:00Z",
    "request_id": "uuid"
  }
}
```

### Common Error Codes

- `PROCESS_NOT_FOUND`: Process does not exist
- `INSUFFICIENT_PERMISSIONS`: Operation not allowed
- `INVALID_PARAMETERS`: Request parameters invalid
- `TRACE_LIMIT_EXCEEDED`: Too many active traces
- `SYSTEM_OVERLOAD`: System too busy to process request

## Rate Limiting

```
X-RateLimit-Limit: 1000
X-RateLimit-Remaining: 999
X-RateLimit-Reset: 1625616000
```

## Authentication & Authorization

### Development Mode
- No authentication required
- All operations permitted

### Production Mode
- Bearer token authentication
- Role-based access control
- Operation-level permissions

## API Versioning

- Version specified in URL: `/api/v1/`
- Backward compatibility maintained
- Deprecation notices in response headers

## SDK Integration

### Elixir Client

```elixir
# Arsenal client library
{:arsenal_client, "~> 1.0"}

client = Arsenal.Client.new(base_url: "http://localhost:4000")
{:ok, processes} = Arsenal.Client.list_processes(client)
```

### JavaScript Client

```javascript
// Arsenal JS client
import { ArsenalClient } from '@arsenal/client';

const client = new ArsenalClient('http://localhost:4000');
const processes = await client.listProcesses();
```

## Future API Extensions

### Planned Endpoints
- `/distributed-systems/` - Multi-node support
- `/code-coverage/` - Coverage analysis
- `/performance-profiles/` - Profiling data
- `/crash-reports/` - Crash analysis
- `/dependency-graphs/` - Module dependencies

### Experimental Features
- GraphQL endpoint for complex queries
- gRPC interface for high-performance clients
- Plugin system for custom endpoints