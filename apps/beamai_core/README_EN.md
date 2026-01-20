# Agent Core

The core module of Agent Framework, providing Agent behavior definitions, data types, graph computation, and general utilities.

## Module Overview

### Agent Behavior and Types

- **beamai_behaviour** - Agent behavior definitions (behaviour)
- **beamai_message** - Message types and conversions
- **beamai_result** - Result type definitions
- **beamai_tool** - Tool definitions and management
- **beamai_types** - General type definitions

### Protocol Support

- **agent_jsonrpc** - JSON-RPC 2.0 encoding/decoding
- **beamai_sse** - Server-Sent Events (SSE) support

### Utility Functions

- **beamai_utils** - General utility functions
- **beamai_http** - HTTP client wrapper

### Graph Computation

Graph computation engine based on LangGraph concepts:

- **graph** - Graph structure core
- **graph_builder** - Graph builder
- **graph_dsl** - Graph DSL
- **graph_node** - Node definitions
- **graph_edge** - Edge definitions
- **graph_runner** - Graph executor
- **graph_state** - State management
- **graph_send** - Message sending

### Pregel Computation Model

Distributed graph computation Pregel model implementation:

- **pregel** - Pregel core
- **pregel_master** - Master node
- **pregel_worker** - Worker node
- **pregel_vertex** - Vertex definitions
- **pregel_graph** - Pregel graph
- **pregel_partition** - Partition management
- **pregel_barrier** - Synchronization barrier
- **pregel_combiner** - Message combiner

### Graph Computation Patterns

- **graph_parallel_experts** - Parallel experts pattern
- **graph_compute** - Graph computation utilities

## API Documentation

### beamai_behaviour

```erlang
%% Define Agent behavior
-callback init(Config :: map()) -> {ok, State :: term()} | {error, Reason :: term()}.
-callback handle_message(Message :: map(), State :: term()) ->
    {reply, Response :: term(), NewState :: term()} | {error, Reason :: term()}.
```

### agent_jsonrpc

```erlang
%% Encode request
agent_jsonrpc:encode_request(Id, Method, Params) -> binary().

%% Encode response
agent_jsonrpc:encode_response(Id, Result) -> binary().

%% Encode error
agent_jsonrpc:encode_error(Id, Code, Message) -> binary().

%% Decode message
agent_jsonrpc:decode(JsonBin) -> {ok, map()} | {error, term()}.
```

### graph_builder

```erlang
%% Create new graph
graph_builder:new() -> builder().

%% Add node
graph_builder:add_node(Builder, Name, NodeFun) -> builder().

%% Add edge
graph_builder:add_edge(Builder, From, To) -> builder().
graph_builder:add_conditional_edges(Builder, From, CondFun, Edges) -> builder().

%% Set entry and finish points
graph_builder:set_entry_point(Builder, NodeName) -> builder().
graph_builder:set_finish_point(Builder, NodeName) -> builder().

%% Compile graph
graph_builder:compile(Builder) -> {ok, graph()} | {error, term()}.
```

## Usage Examples

### Creating a Simple Graph

```erlang
%% Create graph builder
Builder = graph_builder:new(),

%% Add nodes
Builder1 = graph_builder:add_node(Builder, start, fun(State) ->
    io:format("Start node~n"),
    {ok, State#{step => 1}}
end),

Builder2 = graph_builder:add_node(Builder1, process, fun(State) ->
    io:format("Process node~n"),
    {ok, State#{step => 2}}
end),

Builder3 = graph_builder:add_node(Builder2, finish, fun(State) ->
    io:format("Finish node~n"),
    {ok, State}
end),

%% Add edges
Builder4 = graph_builder:add_edge(Builder3, start, process),
Builder5 = graph_builder:add_edge(Builder4, process, finish),

%% Set entry and finish points
Builder6 = graph_builder:set_entry_point(Builder5, start),
Builder7 = graph_builder:set_finish_point(Builder6, finish),

%% Compile and run
{ok, Graph} = graph_builder:compile(Builder7),
{ok, Result} = graph_runner:run(Graph, #{}).
```

### Using JSON-RPC

```erlang
%% Encode request
Request = agent_jsonrpc:encode_request(1, <<"tools/call">>, #{
    <<"name">> => <<"calculator">>,
    <<"arguments">> => #{<<"expression">> => <<"1 + 1">>}
}),

%% Decode response
{ok, Response} = agent_jsonrpc:decode(ResponseBin),
Result = maps:get(<<"result">>, Response).
```

## Dependencies

- jsx - JSON encoding/decoding
- uuid - UUID generation
- hackney - HTTP client

## License

Apache-2.0
