# Agent A2A

English | [中文](README.md)

Agent-to-Agent (A2A) protocol implementation, supporting standardized communication between Agents.

## Features

- A2A protocol server and client implementation
- JSON-RPC 2.0 communication
- Task lifecycle management
- Agent Card discovery
- Push notification support
- Authentication (API Key)
- Rate Limiting
- SSE streaming responses

## Module Overview

### Core Modules

- **beamai_a2a_server** - A2A server core
- **beamai_a2a_client** - A2A client
- **beamai_a2a_task** - Task management
- **beamai_a2a_types** - Type definitions

### Middleware Modules

- **beamai_a2a_handler** - JSON-RPC method handler
- **beamai_a2a_middleware** - Authentication and rate limiting middleware
- **beamai_a2a_convert** - JSON conversion utilities

### Feature Modules

- **beamai_a2a_card** - Agent Card generation
- **beamai_a2a_card_cache** - Agent Card cache
- **beamai_a2a_push** - Push notifications
- **beamai_a2a_auth** - API Key authentication
- **beamai_a2a_rate_limit** - Rate limiting

### Protocol Modules

- **beamai_a2a_jsonrpc** - JSON-RPC encoding/decoding
- **beamai_a2a_http_handler** - HTTP handler
- **beamai_a2a_cowboy_handler** - Cowboy adapter

## API Documentation

### beamai_a2a_server

```erlang
%% Start server
beamai_a2a_server:start_link(Opts) -> {ok, Pid} | {error, Reason}.
beamai_a2a_server:start(Opts) -> {ok, Pid} | {error, Reason}.

%% Stop server
beamai_a2a_server:stop(Server) -> ok.

%% Handle request
beamai_a2a_server:handle_request(Server, Request) -> {ok, Response} | {error, Reason}.
beamai_a2a_server:handle_json(Server, JsonBin) -> {ok, ResponseJson} | {error, Reason}.

%% Handle request with authentication
beamai_a2a_server:handle_json_with_auth(Server, JsonBin, Headers) ->
    {ok, ResponseJson, RateLimitInfo} |
    {error, auth_error, ErrorJson} |
    {error, rate_limited, ErrorJson}.

%% Get Agent Card
beamai_a2a_server:get_agent_card(Server) -> {ok, Card} | {error, Reason}.

%% Task control
beamai_a2a_server:request_input(Server, TaskId, Message) -> ok | {error, Reason}.
beamai_a2a_server:complete_task(Server, TaskId, Result) -> ok | {error, Reason}.
beamai_a2a_server:fail_task(Server, TaskId, Error) -> ok | {error, Reason}.
```

### beamai_a2a_client

```erlang
%% Agent discovery
beamai_a2a_client:discover(BaseUrl) -> {ok, Card} | {error, Reason}.
beamai_a2a_client:discover(BaseUrl, Options) -> {ok, Card} | {error, Reason}.

%% Send message
beamai_a2a_client:send_message(Endpoint, Message) -> {ok, Task} | {error, Reason}.
beamai_a2a_client:send_message(Endpoint, Message, TaskId) -> {ok, Task} | {error, Reason}.
beamai_a2a_client:send_message(Endpoint, Message, TaskId, Options) -> {ok, Task} | {error, Reason}.

%% Task management
beamai_a2a_client:get_task(Endpoint, TaskId) -> {ok, Task} | {error, Reason}.
beamai_a2a_client:cancel_task(Endpoint, TaskId) -> {ok, Task} | {error, Reason}.

%% Streaming request
beamai_a2a_client:send_message_stream(Endpoint, Message, Callback) -> {ok, Result} | {error, Reason}.

%% Convenience functions
beamai_a2a_client:create_text_message(Text) -> Message.
beamai_a2a_client:create_text_message(Role, Text) -> Message.
```

### beamai_a2a_task

```erlang
%% Start task
beamai_a2a_task:start_link(Config) -> {ok, Pid} | {error, Reason}.

%% Get task information
beamai_a2a_task:get_task(Pid) -> {ok, Task} | {error, Reason}.
beamai_a2a_task:get_id(Pid) -> TaskId.
beamai_a2a_task:get_status(Pid) -> Status.
beamai_a2a_task:get_history(Pid) -> History.

%% Update task status
beamai_a2a_task:update_status(Pid, Status) -> ok | {error, Reason}.
beamai_a2a_task:add_message(Pid, Message) -> ok.
beamai_a2a_task:add_artifact(Pid, Artifact) -> ok.
```

## Usage Examples

### Starting an A2A Server

```erlang
%% Configuration
AgentConfig = #{
    name => <<"my-agent">>,
    description => <<"A helpful AI assistant">>,
    url => <<"https://agent.example.com">>,

    %% LLM configuration
    llm => #{
        provider => anthropic,
        model => <<"glm-4">>,
        api_key => os:getenv("ZHIPU_API_KEY"),
        base_url => <<"https://open.bigmodel.cn/api/anthropic/v1">>
    }
},

%% Start server
{ok, Server} = beamai_a2a_server:start_link(#{
    agent_config => AgentConfig
}),

%% Get Agent Card
{ok, Card} = beamai_a2a_server:get_agent_card(Server).
```

### Using the A2A Client

```erlang
%% Discover remote Agent
{ok, Card} = beamai_a2a_client:discover("https://agent.example.com"),

%% View Agent capabilities
Name = maps:get(name, Card),
Skills = maps:get(skills, Card, []),

%% Send message
Message = beamai_a2a_client:create_text_message(<<"Hello, how can you help me?">>),
{ok, Task} = beamai_a2a_client:send_message(
    "https://agent.example.com/a2a",
    Message
),

%% Check task status
TaskId = maps:get(id, Task),
{ok, UpdatedTask} = beamai_a2a_client:get_task(
    "https://agent.example.com/a2a",
    TaskId
),
Status = maps:get(status, UpdatedTask).
```

### Streaming Response

```erlang
%% Streaming callback
Callback = fun
    (#{type := <<"task_status_update">>, task := Task}) ->
        io:format("Status update: ~p~n", [maps:get(status, Task)]);
    (#{type := <<"task_artifact_update">>, artifact := Artifact}) ->
        io:format("Artifact: ~p~n", [Artifact]);
    (Event) ->
        io:format("Event: ~p~n", [Event])
end,

%% Send streaming message
Message = beamai_a2a_client:create_text_message(<<"Tell me a story">>),
{ok, Result} = beamai_a2a_client:send_message_stream(
    "https://agent.example.com/a2a/stream",
    Message,
    Callback
).
```

### Server with Authentication

```erlang
%% Configure authentication
AuthConfig = #{
    api_keys => [
        #{key => <<"sk-xxx">>, name => <<"Client 1">>},
        #{key => <<"sk-yyy">>, name => <<"Client 2">>}
    ]
},

%% Configure rate limiting
RateLimitConfig = #{
    requests_per_minute => 60,
    requests_per_hour => 1000
},

%% Start server with authentication
{ok, Server} = beamai_a2a_server:start_link(#{
    agent_config => AgentConfig,
    auth_config => AuthConfig,
    rate_limit_config => RateLimitConfig
}),

%% Handle request with authentication
Headers = [{<<"authorization">>, <<"Bearer sk-xxx">>}],
case beamai_a2a_server:handle_json_with_auth(Server, RequestJson, Headers) of
    {ok, ResponseJson, _RateLimitInfo} ->
        %% Success
        send_response(200, ResponseJson);
    {error, auth_error, ErrorJson} ->
        %% Authentication failed
        send_response(401, ErrorJson);
    {error, rate_limited, ErrorJson} ->
        %% Rate limited
        send_response(429, ErrorJson)
end.
```

### Cowboy Integration

```erlang
%% Route configuration
Routes = [
    {"/a2a", beamai_a2a_cowboy_handler, #{server => Server}},
    {"/a2a/stream", beamai_a2a_cowboy_handler, #{server => Server, streaming => true}},
    {"/.well-known/agent.json", beamai_a2a_http_handler, #{server => Server}}
],

%% Start Cowboy
{ok, _} = cowboy:start_clear(http_listener,
    [{port, 8080}],
    #{env => #{dispatch => cowboy_router:compile([{'_', Routes}])}}
).
```

## Task State Machine

```
submitted -> working -> completed
                |
                +----> input_required -> working -> completed
                |
                +----> failed

                +----> canceled
```

## Dependencies

- beamai_core
- beamai_llm
- jsx
- hackney
- cowboy (optional, for HTTP service)

## License

Apache-2.0
