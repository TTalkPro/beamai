# Agent MCP

English | [中文](README.md)

Model Context Protocol (MCP) implementation, supporting both MCP servers and clients.

## Features

- Complete MCP protocol implementation
- Client: Connect to external MCP servers
- Server: Expose tools, resources, prompts
- Multiple transport layers: stdio, HTTP, SSE
- Resource subscription and notifications
- Pagination query support

## Module Overview

### Core Modules

- **beamai_mcp_types** - Type definitions and records
- **beamai_mcp_jsonrpc** - JSON-RPC encoding/decoding

### Client

- **beamai_mcp_client** - MCP client (gen_statem)
- **beamai_mcp_client_sup** - Client supervisor

### Server

- **beamai_mcp_server** - MCP server
- **beamai_mcp_server_sup** - Server supervisor
- **beamai_mcp_handler** - Request handler
- **beamai_mcp_cowboy_handler** - Cowboy adapter

### Transport Layer

- **beamai_mcp_transport** - Transport layer behavior
- **beamai_mcp_transport_stdio** - stdio transport
- **beamai_mcp_transport_http** - HTTP transport
- **beamai_mcp_transport_sse** - SSE transport

## API Documentation

### beamai_mcp_client

```erlang
%% Start client
beamai_mcp_client:start_link(Config) -> {ok, Pid} | {error, Reason}.

%% Stop client
beamai_mcp_client:stop(Pid) -> ok.

%% Initialize connection
beamai_mcp_client:initialize(Pid) -> {ok, ServerInfo} | {error, Reason}.

%% Heartbeat
beamai_mcp_client:ping(Pid) -> ok | {error, Reason}.

%% Tool operations
beamai_mcp_client:list_tools(Pid) -> {ok, Tools} | {error, Reason}.
beamai_mcp_client:list_tools(Pid, Options) -> {ok, Tools} | {error, Reason}.
beamai_mcp_client:call_tool(Pid, Name, Args) -> {ok, Result} | {error, Reason}.
beamai_mcp_client:call_tool(Pid, Name, Args, Options) -> {ok, Result} | {error, Reason}.

%% Resource operations
beamai_mcp_client:list_resources(Pid) -> {ok, Resources} | {error, Reason}.
beamai_mcp_client:list_resources(Pid, Options) -> {ok, Resources} | {error, Reason}.
beamai_mcp_client:read_resource(Pid, Uri) -> {ok, Contents} | {error, Reason}.
beamai_mcp_client:subscribe_resource(Pid, Uri) -> ok | {error, Reason}.
beamai_mcp_client:unsubscribe_resource(Pid, Uri) -> ok | {error, Reason}.

%% Prompt operations
beamai_mcp_client:list_prompts(Pid) -> {ok, Prompts} | {error, Reason}.
beamai_mcp_client:get_prompt(Pid, Name, Args) -> {ok, Prompt} | {error, Reason}.

%% State query
beamai_mcp_client:get_state(Pid) -> {ok, State}.
beamai_mcp_client:get_capabilities(Pid) -> {ok, Capabilities} | {error, Reason}.
```

### beamai_mcp_server

```erlang
%% Start server
beamai_mcp_server:start_link(Config) -> {ok, Pid} | {error, Reason}.

%% Stop server
beamai_mcp_server:stop(Pid) -> ok.

%% Handle requests
beamai_mcp_server:handle_request(Pid, Request) -> Response.
beamai_mcp_server:handle_notification(Pid, Notification) -> ok.

%% Register primitives
beamai_mcp_server:register_tool(Pid, Tool) -> ok | {error, Reason}.
beamai_mcp_server:register_resource(Pid, Resource) -> ok | {error, Reason}.
beamai_mcp_server:register_prompt(Pid, Prompt) -> ok | {error, Reason}.

%% Unregister primitives
beamai_mcp_server:unregister_tool(Pid, Name) -> ok.
beamai_mcp_server:unregister_resource(Pid, Uri) -> ok.
beamai_mcp_server:unregister_prompt(Pid, Name) -> ok.

%% Get session
beamai_mcp_server:get_session(Pid) -> {ok, Session} | {error, Reason}.
```

### beamai_mcp_types

```erlang
%% Create tool
beamai_mcp_types:make_tool(Name, Description, InputSchema, Handler) -> #mcp_tool{}.

%% Create resource
beamai_mcp_types:make_resource(Uri, Name, Options) -> #mcp_resource{}.

%% Create prompt
beamai_mcp_types:make_prompt(Name, Description, Arguments) -> #mcp_prompt{}.

%% Record to Map
beamai_mcp_types:tool_to_map(Tool) -> Map.
beamai_mcp_types:resource_to_map(Resource) -> Map.
beamai_mcp_types:prompt_to_map(Prompt) -> Map.
```

## Usage Examples

### Using MCP Client (stdio transport)

```erlang
%% Connect to filesystem MCP server
{ok, Client} = beamai_mcp_client:start_link(#{
    transport => stdio,
    command => "npx",
    args => ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"],
    client_info => #{name => <<"my-app">>, version => <<"1.0">>}
}),

%% Initialize connection
{ok, ServerInfo} = beamai_mcp_client:initialize(Client),
io:format("Connected to: ~s v~s~n", [
    maps:get(name, ServerInfo),
    maps:get(version, ServerInfo)
]),

%% List available tools
{ok, #{tools := Tools}} = beamai_mcp_client:list_tools(Client),
lists:foreach(fun(Tool) ->
    io:format("Tool: ~s - ~s~n", [
        maps:get(name, Tool),
        maps:get(description, Tool)
    ])
end, Tools),

%% Call tool
{ok, Result} = beamai_mcp_client:call_tool(Client,
    <<"read_file">>,
    #{<<"path">> => <<"/tmp/test.txt">>}
),
io:format("File content: ~p~n", [Result]),

%% Close client
beamai_mcp_client:stop(Client).
```

### Creating MCP Server

```erlang
%% Define tool
EchoTool = beamai_mcp_types:make_tool(
    <<"echo">>,
    <<"Echo the input text">>,
    #{
        type => object,
        properties => #{
            text => #{type => string, description => <<"Text to echo">>}
        },
        required => [<<"text">>]
    },
    fun(#{<<"text">> := Text}) ->
        {ok, [#{type => text, text => <<"Echo: ", Text/binary>>}]}
    end
),

%% Define resource
ConfigResource = beamai_mcp_types:make_resource(
    <<"config://app/settings">>,
    <<"Application Settings">>,
    #{
        description => <<"Current application configuration">>,
        mime_type => <<"application/json">>
    }
),

%% Start server
{ok, Server} = beamai_mcp_server:start_link(#{
    tools => [EchoTool],
    resources => [ConfigResource],
    prompts => [],
    server_info => #{
        name => <<"my-mcp-server">>,
        version => <<"1.0.0">>
    }
}),

%% Handle request
RequestJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}">>,
Response = beamai_mcp_server:handle_request(Server, jsx:decode(RequestJson)).
```

### Integration with Cowboy

```erlang
%% Route configuration
Routes = [
    {"/mcp", beamai_mcp_cowboy_handler, #{server => Server}},
    {"/mcp/sse", beamai_mcp_cowboy_handler, #{server => Server, transport => sse}}
],

%% Start HTTP server
{ok, _} = cowboy:start_clear(mcp_http,
    [{port, 8080}],
    #{env => #{dispatch => cowboy_router:compile([{'_', Routes}])}}
).
```

### Connecting via HTTP Transport

```erlang
%% Connect to HTTP MCP server
{ok, Client} = beamai_mcp_client:start_link(#{
    transport => http,
    base_url => <<"http://localhost:8080/mcp">>,
    client_info => #{name => <<"http-client">>, version => <<"1.0">>}
}),

{ok, _} = beamai_mcp_client:initialize(Client),
{ok, Tools} = beamai_mcp_client:list_tools(Client).
```

### Pagination Query

```erlang
%% List tools (with pagination)
{ok, Result1} = beamai_mcp_client:list_tools(Client, #{cursor => undefined}),
#{tools := Tools1, next_cursor := NextCursor} = Result1,

%% Get next page
case NextCursor of
    undefined ->
        %% No more data
        ok;
    Cursor ->
        {ok, Result2} = beamai_mcp_client:list_tools(Client, #{cursor => Cursor}),
        #{tools := Tools2} = Result2
end.
```

### Resource Subscription

```erlang
%% Subscribe to resource changes
ok = beamai_mcp_client:subscribe_resource(Client, <<"file:///tmp/config.json">>),

%% Handle notifications (in client process)
handle_info({mcp_notification, <<"notifications/resources/updated">>, Params}, State) ->
    Uri = maps:get(uri, Params),
    io:format("Resource updated: ~s~n", [Uri]),
    %% Re-read resource
    {ok, Contents} = beamai_mcp_client:read_resource(Client, Uri),
    {noreply, State};

%% Unsubscribe
ok = beamai_mcp_client:unsubscribe_resource(Client, <<"file:///tmp/config.json">>).
```

## Configuration Options

### Client Configuration

```erlang
Config = #{
    %% Transport layer configuration
    transport => stdio | http | sse,

    %% stdio transport options
    command => "npx",
    args => ["-y", "@modelcontextprotocol/server-xxx"],
    env => #{<<"KEY">> => <<"value">>},

    %% HTTP transport options
    base_url => <<"http://localhost:8080">>,
    headers => [{<<"Authorization">>, <<"Bearer xxx">>}],

    %% Client information
    client_info => #{
        name => <<"client-name">>,
        version => <<"1.0.0">>
    },

    %% Timeout settings
    timeout => 30000,

    %% Capability declaration
    capabilities => #{
        roots => #{listChanged => true},
        sampling => #{}
    }
}.
```

### Server Configuration

```erlang
Config = #{
    %% Primitive lists
    tools => [Tool1, Tool2],
    resources => [Resource1, Resource2],
    prompts => [Prompt1, Prompt2],

    %% Server information
    server_info => #{
        name => <<"server-name">>,
        version => <<"1.0.0">>
    },

    %% Capability declaration
    capabilities => #{
        tools => #{listChanged => true},
        resources => #{subscribe => true, listChanged => true},
        prompts => #{listChanged => true}
    }
}.
```

## Dependencies

- beamai_core
- jsx
- hackney

## License

Apache-2.0
