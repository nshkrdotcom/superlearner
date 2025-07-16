# Code Examples and Patterns from Observer CLI

## 1. Efficient Data Collection Patterns

### 1.1 Batch Process Information Gathering

**Observer CLI Pattern:**
```erlang
get_process_info(Pids) ->
    [get_process_info(Pid) || Pid <- Pids].

get_process_info(Pid) ->
    case recon:info(Pid, ?PROCESS_INFO_KEYS) of
        undefined -> [];
        Info -> Info
    end.
```

**OTP Supervisor Adaptation:**
```elixir
defmodule OtpSupervisor.Core.ProcessCollector do
  @process_keys [
    :registered_name, :status, :memory, :reductions,
    :message_queue_len, :current_function, :initial_call,
    :heap_size, :stack_size, :total_heap_size, :binary
  ]

  def collect_all_processes do
    Process.list()
    |> Task.async_stream(&safe_process_info/1, 
         max_concurrency: System.schedulers_online(),
         timeout: 100)
    |> Enum.reduce([], fn
      {:ok, info}, acc when info != nil -> [info | acc]
      _, acc -> acc
    end)
  end

  defp safe_process_info(pid) do
    Process.info(pid, @process_keys)
  rescue
    _ -> nil
  end
end
```

### 1.2 Smart Refresh Pattern

**Observer CLI's Timer Management:**
```erlang
render_worker(Interval, LastTimeRef) ->
    %% Do work
    NextTimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
    receive
        redraw -> render_worker(Interval, NextTimeRef)
    after 0 -> render_worker(Interval, NextTimeRef)
    end.

next_redraw(LastTimeRef, Interval) ->
    TimeOut = Interval - (erlang:monotonic_time(millisecond) - LastTimeRef),
    erlang:send_after(max(TimeOut, 0), self(), redraw).
```

**LiveView Adaptation:**
```elixir
defmodule OtpSupervisorWeb.RefreshableView do
  defmacro __using__(opts) do
    quote do
      def mount(params, session, socket) do
        socket = 
          socket
          |> assign(:refresh_interval, unquote(opts[:interval] || 1000))
          |> assign(:last_refresh, System.monotonic_time(:millisecond))
          
        if connected?(socket) do
          schedule_refresh(socket)
        end
        
        {:ok, socket}
      end
      
      def handle_info(:refresh, socket) do
        socket = refresh_data(socket)
        schedule_refresh(socket)
        {:noreply, socket}
      end
      
      defp schedule_refresh(socket) do
        elapsed = System.monotonic_time(:millisecond) - socket.assigns.last_refresh
        delay = max(socket.assigns.refresh_interval - elapsed, 0)
        Process.send_after(self(), :refresh, delay)
      end
    end
  end
end
```

## 2. Terminal Rendering Patterns

### 2.1 ANSI Code Management

**Observer CLI's Rendering:**
```erlang
-define(CURSOR_TOP, <<"\e[H">>).         % Move cursor to top
-define(CLEAR, <<"\e[2J">>).            % Clear screen
-define(BOLD, <<"\e[1m">>).             % Bold text
-define(RED, <<"\e[31m">>).             % Red color
-define(GREEN, <<"\e[32m">>).           % Green color
-define(RESET, <<"\e[0m">>).            % Reset formatting

render_line(Text, Options) ->
    Color = proplists:get_value(color, Options, default),
    Bold = proplists:get_value(bold, Options, false),
    [
        case Bold of true -> ?BOLD; false -> <<>> end,
        case Color of
            red -> ?RED;
            green -> ?GREEN;
            _ -> <<>>
        end,
        Text,
        ?RESET
    ].
```

**Terminal Mode for OTP Supervisor:**
```elixir
defmodule OtpSupervisor.Terminal.Renderer do
  @cursor_top "\e[H"
  @clear "\e[2J"
  @bold "\e[1m"
  @colors %{
    red: "\e[31m",
    green: "\e[32m",
    yellow: "\e[33m",
    blue: "\e[34m",
    reset: "\e[0m"
  }
  
  def render_screen(components) do
    [
      @cursor_top,
      @clear,
      Enum.map(components, &render_component/1)
    ]
    |> IO.iodata_to_binary()
    |> IO.write()
  end
  
  def render_table(headers, rows, opts \\ []) do
    col_widths = calculate_column_widths(headers, rows)
    
    [
      render_row(headers, col_widths, bold: true),
      render_separator(col_widths),
      Enum.map(rows, &render_row(&1, col_widths, opts))
    ]
  end
  
  defp render_row(cells, widths, opts) do
    cells
    |> Enum.zip(widths)
    |> Enum.map(fn {cell, width} ->
      text = String.pad_trailing(to_string(cell), width)
      colorize(text, opts)
    end)
    |> Enum.join(" | ")
  end
end
```

### 2.2 Keyboard Input Handling

**Observer CLI Pattern:**
```erlang
parse_cmd(ViewOpts, Module, Args) ->
    case observer_cli_lib:get_line("") of
        "q\n" -> quit;
        "p\n" -> pause;
        "r\n" -> {sort, reductions, ascending};
        "rr\n" -> {sort, reductions, descending};
        Input -> handle_custom_input(Input)
    end.
```

**Elixir Terminal Input:**
```elixir
defmodule OtpSupervisor.Terminal.InputHandler do
  def start_input_loop(view_pid) do
    IO.puts("\nCommands: q(quit) p(pause) r(refresh) m(memory sort) h(help)")
    
    spawn(fn -> input_loop(view_pid) end)
  end
  
  defp input_loop(view_pid) do
    case IO.gets("") do
      "q\n" -> 
        send(view_pid, :quit)
      "p\n" -> 
        send(view_pid, :toggle_pause)
      "m\n" -> 
        send(view_pid, {:sort, :memory})
      "r\n" ->
        send(view_pid, {:sort, :reductions})
      "h\n" ->
        send(view_pid, :show_help)
      _ ->
        input_loop(view_pid)
    end
  end
end
```

## 3. Data Formatting Patterns

### 3.1 Smart Number Formatting

**Observer CLI's Approach:**
```erlang
format_bytes(Bytes) when Bytes < 1024 ->
    io_lib:format("~wB", [Bytes]);
format_bytes(Bytes) when Bytes < 1024*1024 ->
    io_lib:format("~.1fKB", [Bytes/1024]);
format_bytes(Bytes) when Bytes < 1024*1024*1024 ->
    io_lib:format("~.1fMB", [Bytes/1024/1024]);
format_bytes(Bytes) ->
    io_lib:format("~.1fGB", [Bytes/1024/1024/1024]).
```

**Enhanced Elixir Version:**
```elixir
defmodule OtpSupervisor.Formatters do
  def format_bytes(bytes) when is_integer(bytes) do
    cond do
      bytes < 1024 ->
        "#{bytes}B"
      bytes < 1024 * 1024 ->
        "#{Float.round(bytes / 1024, 1)}KB"
      bytes < 1024 * 1024 * 1024 ->
        "#{Float.round(bytes / 1024 / 1024, 1)}MB"
      true ->
        "#{Float.round(bytes / 1024 / 1024 / 1024, 2)}GB"
    end
  end
  
  def format_uptime(milliseconds) do
    seconds = div(milliseconds, 1000)
    minutes = div(seconds, 60)
    hours = div(minutes, 60)
    days = div(hours, 24)
    
    cond do
      days > 0 -> "#{days}d #{rem(hours, 24)}h"
      hours > 0 -> "#{hours}h #{rem(minutes, 60)}m"
      minutes > 0 -> "#{minutes}m #{rem(seconds, 60)}s"
      true -> "#{seconds}s"
    end
  end
  
  def format_number(num) when num >= 1_000_000_000 do
    "#{Float.round(num / 1_000_000_000, 1)}B"
  end
  def format_number(num) when num >= 1_000_000 do
    "#{Float.round(num / 1_000_000, 1)}M"
  end
  def format_number(num) when num >= 1_000 do
    "#{Float.round(num / 1_000, 1)}K"
  end
  def format_number(num), do: "#{num}"
end
```

### 3.2 Table Rendering with Alignment

**Observer CLI's Table Pattern:**
```erlang
render_table(Headers, Rows) ->
    Widths = calculate_widths(Headers, Rows),
    [render_header(Headers, Widths),
     render_separator(Widths),
     [render_row(Row, Widths) || Row <- Rows]].
```

**LiveView Table Component:**
```elixir
defmodule OtpSupervisorWeb.Components.DataTable do
  use Phoenix.Component
  
  attr :headers, :list, required: true
  attr :rows, :list, required: true
  attr :sort_by, :atom, default: nil
  attr :sort_order, :atom, default: :asc
  
  def data_table(assigns) do
    ~H"""
    <div class="overflow-x-auto">
      <table class="min-w-full divide-y divide-gray-700">
        <thead class="bg-gray-800">
          <tr>
            <%= for header <- @headers do %>
              <th 
                class="px-6 py-3 text-left text-xs font-medium text-gray-300 uppercase tracking-wider cursor-pointer"
                phx-click="sort"
                phx-value-field={header.field}
              >
                <%= header.label %>
                <%= if @sort_by == header.field do %>
                  <span class="ml-1">
                    <%= if @sort_order == :asc, do: "↑", else: "↓" %>
                  </span>
                <% end %>
              </th>
            <% end %>
          </tr>
        </thead>
        <tbody class="bg-gray-900 divide-y divide-gray-700">
          <%= for row <- @rows do %>
            <tr class="hover:bg-gray-800">
              <%= for header <- @headers do %>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-300">
                  <%= format_cell(row[header.field], header.type) %>
                </td>
              <% end %>
            </tr>
          <% end %>
        </tbody>
      </table>
    </div>
    """
  end
  
  defp format_cell(value, :bytes), do: OtpSupervisor.Formatters.format_bytes(value)
  defp format_cell(value, :number), do: OtpSupervisor.Formatters.format_number(value)
  defp format_cell(value, :uptime), do: OtpSupervisor.Formatters.format_uptime(value)
  defp format_cell(value, _), do: value
end
```

## 4. Performance Monitoring Patterns

### 4.1 Sliding Window Analysis

**Observer CLI's Window Tracking:**
```erlang
%% Track process behavior over time
proc_window(Type, Num, Time) ->
    recon:proc_window(Type, Num, Time).
```

**Elixir Implementation with History:**
```elixir
defmodule OtpSupervisor.Core.SlidingWindow do
  use GenServer
  
  defstruct [:window_ms, :max_samples, :samples]
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end
  
  def track_processes(type \\ :memory) do
    GenServer.call(__MODULE__, {:track, type})
  end
  
  def init(opts) do
    state = %__MODULE__{
      window_ms: opts[:window_ms] || 5000,
      max_samples: opts[:max_samples] || 10,
      samples: []
    }
    
    schedule_sample()
    {:ok, state}
  end
  
  def handle_info(:sample, state) do
    sample = capture_sample()
    
    new_samples = 
      [sample | state.samples]
      |> Enum.take(state.max_samples)
    
    schedule_sample()
    {:noreply, %{state | samples: new_samples}}
  end
  
  def handle_call({:track, type}, _from, state) do
    analysis = analyze_window(state.samples, type)
    {:reply, analysis, state}
  end
  
  defp analyze_window(samples, type) do
    # Compare first and last samples
    # Identify processes with highest growth
    # Calculate rates of change
  end
end
```

### 4.2 Threshold Monitoring

**Observer CLI's Alert Pattern:**
```erlang
check_threshold(Value, Limit, Threshold) ->
    Percentage = (Value / Limit) * 100,
    if
        Percentage > Threshold -> {alert, Percentage};
        true -> {ok, Percentage}
    end.
```

**LiveView Alert System:**
```elixir
defmodule OtpSupervisor.Core.ThresholdMonitor do
  use GenServer
  
  @default_thresholds %{
    process_count: 0.85,
    atom_count: 0.90,
    memory: 0.80,
    scheduler_usage: 0.95
  }
  
  def check_all do
    %{
      process_count: check_process_count(),
      atom_count: check_atom_count(),
      memory: check_memory(),
      schedulers: check_schedulers()
    }
  end
  
  defp check_process_count do
    current = :erlang.system_info(:process_count)
    limit = :erlang.system_info(:process_limit)
    
    check_threshold(current, limit, @default_thresholds.process_count)
  end
  
  defp check_threshold(current, limit, threshold) do
    usage = current / limit
    
    %{
      current: current,
      limit: limit,
      usage_percent: usage * 100,
      status: cond do
        usage > threshold -> :critical
        usage > threshold * 0.8 -> :warning
        true -> :ok
      end
    }
  end
end
```

## 5. Production Safety Patterns

### 5.1 Safe Data Inspection

**Observer CLI's Truncation:**
```erlang
safe_format(Term) ->
    lists:flatten(io_lib:format("~P", [Term, 20])).
```

**Elixir Safe Inspector:**
```elixir
defmodule OtpSupervisor.SafeInspect do
  @max_depth 5
  @max_list_length 20
  @max_binary_size 100
  
  def inspect(term, opts \\ []) do
    opts = Keyword.merge([
      limit: 50,
      printable_limit: @max_binary_size,
      depth: @max_depth
    ], opts)
    
    try do
      Kernel.inspect(term, opts)
    rescue
      _ -> "#InspectError"
    end
  end
  
  def truncate_binary(<<binary::binary>>) when byte_size(binary) > @max_binary_size do
    <<preview::binary-size(@max_binary_size), _::binary>> = binary
    "<<#{inspect(preview)}...>>"
  end
  def truncate_binary(binary), do: inspect(binary)
end
```

### 5.2 Timeout Protection

**Observer CLI's Approach:**
```erlang
safe_call(Fun) ->
    try
        Fun()
    catch
        _:_ -> {error, failed}
    end.
```

**Elixir Task-based Safety:**
```elixir
defmodule OtpSupervisor.SafeExecutor do
  @default_timeout 500
  
  def safe_execute(fun, timeout \\ @default_timeout) do
    task = Task.async(fun)
    
    case Task.yield(task, timeout) || Task.shutdown(task) do
      {:ok, result} -> {:ok, result}
      nil -> {:error, :timeout}
      {:exit, reason} -> {:error, reason}
    end
  end
  
  def safe_process_info(pid, keys) do
    safe_execute(fn -> Process.info(pid, keys) end, 100)
  end
  
  def safe_ets_info(table) do
    safe_execute(fn -> :ets.info(table) end, 200)
  end
end
```

## Implementation Guidelines

1. **Always use timeouts** for external calls
2. **Batch operations** when possible
3. **Cache expensive calculations**
4. **Truncate large data** before sending to UI
5. **Use async operations** for concurrent data collection
6. **Monitor your monitors** - ensure monitoring doesn't impact the system