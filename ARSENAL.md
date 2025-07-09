# Arsenal: Metaprogrammed OTP Operations Framework

## Overview

Arsenal is a sophisticated metaprogramming framework that automatically generates REST APIs from OTP operation modules. It provides a protocol-driven approach where operations are defined once and automatically exposed as HTTP endpoints with validation, error handling, and documentation.

## Table of Contents

- [Core Architecture](#core-architecture)
- [Operation Protocol](#operation-protocol)
- [Metaprogramming Engine](#metaprogramming-engine)
- [Operation Lifecycle](#operation-lifecycle)
- [Safety Features](#safety-features)
- [Usage Examples](#usage-examples)
- [API Reference](#api-reference)
- [Development Guide](#development-guide)

## Core Architecture

### Registry System

**File**: `lib/otp_supervisor/core/arsenal/registry.ex`

The Arsenal system uses a GenServer-backed ETS table for high-performance operation lookup:

```elixir
# Operations are registered at startup
@known_operations [
  OTPSupervisor.Core.Arsenal.Operations.GetProcessInfo,
  OTPSupervisor.Core.Arsenal.Operations.KillProcess,
  OTPSupervisor.Core.Arsenal.Operations.ListSupervisors,
  OTPSupervisor.Core.Arsenal.Operations.SendMessage,
  OTPSupervisor.Core.Arsenal.Operations.TraceProcess
]
```

**Key Features:**
- Compile-time operation discovery
- ETS-based storage for concurrent access
- Automatic validation of operation contracts
- Runtime operation lookup and metadata storage

### Dynamic HTTP Routing

**File**: `lib/otp_supervisor_web/arsenal_plug.ex`

Arsenal uses runtime metaprogramming for HTTP request routing:

```elixir
def call(conn, _opts) do
  if String.starts_with?(conn.request_path, "/api/v1") do
    case find_matching_operation(conn) do
      {:ok, operation_module} -> execute_arsenal_operation(conn, operation_module)
      {:error, :no_match} -> send_404_response(conn)
    end
  end
end
```

**Route Matching Algorithm:**
1. Extract HTTP method and path from request
2. Query registry for all registered operations
3. Convert operation path patterns to regex (`:param` → `[^/]+`)
4. Find first matching operation based on method and path
5. Execute operation with extracted parameters

## Operation Protocol

### Behavior Definition

**File**: `lib/otp_supervisor/core/arsenal/operation.ex`

All operations must implement the `Operation` behavior:

```elixir
@callback rest_config() :: map()
@callback validate_params(params :: map()) :: {:ok, map()} | {:error, term()}
@callback execute(params :: map()) :: {:ok, term()} | {:error, term()}
@callback format_response(result :: term()) :: map()
```

### Contract Requirements

#### `rest_config/0`
Defines HTTP endpoint configuration:

```elixir
def rest_config do
  %{
    method: :get,                           # HTTP method
    path: "/api/v1/processes/:pid/info",    # URL pattern with parameters
    summary: "Get process information",     # API documentation
    parameters: [                           # Parameter specifications
      %{name: :pid, type: :string, required: true, location: :path}
    ],
    responses: %{                           # Response schemas
      200 => %{description: "Process information"}
    }
  }
end
```

#### `validate_params/1`
Parameter validation and normalization:

```elixir
def validate_params(%{"pid" => pid_string} = params) do
  case parse_pid(pid_string) do
    {:ok, pid} -> {:ok, Map.put(params, "pid", pid)}
    {:error, reason} -> {:error, {:invalid_parameter, :pid, reason}}
  end
end
```

#### `execute/1`
Core business logic:

```elixir
def execute(%{"pid" => pid}) do
  if Process.alive?(pid) do
    case Process.info(pid) do
      nil -> {:error, :process_not_found}
      info -> {:ok, Enum.into(info, %{})}
    end
  else
    {:error, :process_not_found}
  end
end
```

#### `format_response/1`
Response formatting (optional - has default implementation):

```elixir
def format_response(result) do
  %{
    data: result,
    timestamp: DateTime.utc_now(),
    success: true
  }
end
```

## Metaprogramming Engine

### Request Processing Pipeline

The Arsenal Plug processes requests through a comprehensive pipeline:

```elixir
def execute_arsenal_operation(conn, operation_module) do
  with {:ok, config} <- get_operation_config(operation_module),
       {:ok, merged_params} <- merge_request_parameters(conn, config),
       {:ok, validated_params} <- validate_operation_parameters(operation_module, merged_params),
       {:ok, result} <- execute_operation(operation_module, validated_params),
       {:ok, formatted_response} <- format_operation_response(operation_module, result) do
    send_success_response(conn, formatted_response)
  else
    error -> send_error_response(conn, error)
  end
end
```

### Parameter Merging

Arsenal automatically merges parameters from multiple sources:

- **Path parameters**: Extracted from URL patterns (`:pid` → actual PID value)
- **Query parameters**: From URL query string
- **Body parameters**: From JSON request body
- **Header parameters**: From HTTP headers

### Error Handling

Comprehensive error handling with contextual information:

```elixir
defp send_error_response(conn, {:error, {:invalid_parameter, field, reason}}) do
  conn
  |> put_resp_content_type("application/json")
  |> send_resp(400, Jason.encode!(%{
    error: "Invalid parameter",
    field: field,
    reason: reason,
    timestamp: DateTime.utc_now()
  }))
end
```

## Operation Lifecycle

### 1. Registration Phase (Boot Time)

1. Application starts Arsenal Registry GenServer
2. Registry discovers known operations via `@known_operations`
3. Each operation is validated for contract compliance
4. Operations are stored in ETS table with metadata
5. Routes are precompiled for performance

### 2. Request Phase (Runtime)

1. HTTP request arrives at Arsenal Plug
2. Request path and method are extracted
3. Registry is queried for matching operations
4. Path parameters are extracted using regex matching
5. Operation module is selected for execution

### 3. Execution Phase

1. Operation configuration is retrieved
2. Request parameters are merged from all sources
3. Parameters are validated using operation's `validate_params/1`
4. Core business logic is executed via `execute/1`
5. Response is formatted using `format_response/1`
6. HTTP response is sent to client

### 4. Error Recovery

- Parameter validation errors return 400 Bad Request
- Operation execution errors return 500 Internal Server Error
- Missing operations return 404 Not Found
- All errors include contextual information and timestamps

## Safety Features

### Critical Process Protection

**File**: `lib/otp_supervisor/core/arsenal/operations/kill_process.ex`

Arsenal includes built-in safety mechanisms:

```elixir
def execute(%{"pid" => pid, "force" => force}) do
  case is_critical_process?(pid) do
    true when not force ->
      {:error, {:critical_process, "Use force=true to terminate critical processes"}}
    
    _ ->
      Process.exit(pid, :kill)
      {:ok, %{terminated: true, pid: pid}}
  end
end

defp is_critical_process?(pid) do
  # Check against known critical processes
  critical_names = [:application_controller, :init, :kernel_sup]
  
  case Process.info(pid, :registered_name) do
    {:registered_name, name} when name in critical_names -> true
    _ -> false
  end
end
```

### Parameter Validation

Multi-level validation ensures data integrity:

1. **Type Validation**: Ensures parameters match expected types
2. **Range Validation**: Checks numeric ranges and string lengths
3. **Format Validation**: Validates PID formats, atom names, etc.
4. **Business Logic Validation**: Custom validation rules per operation

### Rate Limiting & Throttling

```elixir
# Example implementation (can be added)
def validate_params(params) do
  case check_rate_limit(params["client_id"]) do
    :ok -> validate_business_params(params)
    {:error, :rate_limited} -> {:error, :too_many_requests}
  end
end
```

## Usage Examples

### Simple Process Information

```bash
GET /api/v1/processes/<0.123.0>/info
```

**Response:**
```json
{
  "data": {
    "current_function": ["gen_server", "loop", 7],
    "initial_call": ["proc_lib", "init_p", 5],
    "memory": 2048,
    "message_queue_len": 0,
    "status": "waiting"
  },
  "timestamp": "2025-07-09T10:30:00Z",
  "success": true
}
```

### Process Tracing with Duration

```bash
POST /api/v1/processes/<0.123.0>/trace
Content-Type: application/json

{
  "flags": ["calls", "receive"],
  "duration": 30000
}
```

**Response:**
```json
{
  "data": {
    "session_id": "trace_abc123",
    "pid": "<0.123.0>",
    "flags": ["calls", "receive"],
    "duration": 30000,
    "started_at": "2025-07-09T10:30:00Z"
  },
  "success": true
}
```

### Safe Process Termination

```bash
DELETE /api/v1/processes/<0.123.0>?force=true
```

**Response:**
```json
{
  "data": {
    "terminated": true,
    "pid": "<0.123.0>",
    "was_critical": false
  },
  "success": true
}
```

## API Reference

### Available Operations

| Operation | Method | Path | Description |
|-----------|--------|------|-------------|
| GetProcessInfo | GET | `/api/v1/processes/:pid/info` | Get comprehensive process information |
| KillProcess | DELETE | `/api/v1/processes/:pid` | Terminate a process with safety checks |
| ListSupervisors | GET | `/api/v1/supervisors` | List all supervisors in the system |
| SendMessage | POST | `/api/v1/processes/:pid/messages` | Send message to a process |
| TraceProcess | POST | `/api/v1/processes/:pid/trace` | Enable process tracing |

### Common Parameters

- **`:pid`** - Process identifier (string format: `"<0.123.0>"`)
- **`force`** - Boolean flag for overriding safety checks
- **`timeout`** - Operation timeout in milliseconds
- **`format`** - Response format (`json`, `text`)

### Response Format

All Arsenal operations return consistent response format:

```json
{
  "data": "<operation-specific-data>",
  "timestamp": "2025-07-09T10:30:00Z",
  "success": true,
  "metadata": {
    "operation": "GetProcessInfo",
    "execution_time_ms": 15
  }
}
```

### Error Responses

```json
{
  "error": "Process not found",
  "code": "PROCESS_NOT_FOUND",
  "timestamp": "2025-07-09T10:30:00Z",
  "details": {
    "pid": "<0.999.0>",
    "suggestion": "Check if process is still alive"
  }
}
```

## Development Guide

### Creating New Operations

1. **Create Operation Module**

```elixir
defmodule OTPSupervisor.Core.Arsenal.Operations.MyOperation do
  use OTPSupervisor.Core.Arsenal.Operation

  def rest_config do
    %{
      method: :post,
      path: "/api/v1/my-operation/:param",
      summary: "My custom operation",
      parameters: [
        %{name: :param, type: :string, required: true, location: :path}
      ]
    }
  end

  def validate_params(params) do
    # Add validation logic
    {:ok, params}
  end

  def execute(params) do
    # Add business logic
    {:ok, %{result: "success"}}
  end
end
```

2. **Register Operation**

Add to `@known_operations` in `registry.ex`:

```elixir
@known_operations [
  # ... existing operations
  OTPSupervisor.Core.Arsenal.Operations.MyOperation
]
```

3. **Test Operation**

```elixir
defmodule MyOperationTest do
  use ExUnit.Case

  test "validates parameters correctly" do
    params = %{"param" => "test_value"}
    assert {:ok, _} = MyOperation.validate_params(params)
  end

  test "executes successfully" do
    params = %{"param" => "test_value"}
    assert {:ok, result} = MyOperation.execute(params)
  end
end
```

### Best Practices

1. **Parameter Validation**: Always validate and sanitize input parameters
2. **Error Handling**: Use descriptive error messages with context
3. **Safety Checks**: Implement protection for critical operations
4. **Documentation**: Provide clear summaries and parameter descriptions
5. **Testing**: Write comprehensive tests for all code paths
6. **Performance**: Consider operation performance impact on system
7. **Idempotency**: Make operations idempotent where possible

### Debugging

Enable Arsenal debugging:

```elixir
# In config/dev.exs
config :logger, level: :debug

# In operation modules
require Logger

def execute(params) do
  Logger.debug("Executing operation with params: #{inspect(params)}")
  # ... rest of implementation
end
```

### Performance Monitoring

Arsenal operations can be monitored using telemetry:

```elixir
# Example telemetry integration
def execute(params) do
  :telemetry.span([:arsenal, :operation], %{operation: __MODULE__}, fn ->
    result = do_execute(params)
    {result, %{params: params}}
  end)
end
```

## Architecture Benefits

1. **Developer Productivity**: Add operation → get REST API automatically
2. **Consistency**: All operations follow the same patterns and conventions
3. **Maintainability**: No route files or controller boilerplate to maintain
4. **Extensibility**: Easy to add new operations without touching routing code
5. **Safety**: Built-in protection mechanisms prevent dangerous operations
6. **Documentation**: Auto-generated API docs from operation metadata
7. **Testing**: Consistent testing patterns across all operations
8. **Performance**: ETS-based registry provides fast operation lookup
9. **Error Handling**: Comprehensive error reporting with context
10. **Flexibility**: Operations can be enabled/disabled at runtime

## Future Enhancements

- **Rate Limiting**: Per-client operation throttling
- **Authentication**: Role-based access control for operations
- **Monitoring**: Detailed metrics and health checks
- **Caching**: Response caching for expensive operations
- **Batch Operations**: Support for executing multiple operations atomically
- **WebSocket Support**: Real-time operation streaming
- **Operation Queuing**: Background operation execution
- **Audit Logging**: Complete operation audit trails

---

**Arsenal** represents a sophisticated approach to metaprogramming in Elixir, demonstrating how powerful abstractions can be built on top of OTP primitives while maintaining safety, performance, and developer productivity.