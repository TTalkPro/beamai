# Agent MCP

[English](README_EN.md) | 中文

Model Context Protocol（MCP）实现，支持 MCP 服务器和客户端。

## 特性

- MCP 协议完整实现
- 客户端：连接外部 MCP 服务器
- 服务器：暴露工具、资源、提示
- 多种传输层：stdio、HTTP、SSE
- 资源订阅与通知
- 分页查询支持

## 模块概览

### 核心模块

- **beamai_mcp_types** - 类型定义与记录
- **beamai_mcp_jsonrpc** - JSON-RPC 编解码

### 客户端

- **beamai_mcp_client** - MCP 客户端（gen_statem）
- **beamai_mcp_client_sup** - 客户端监督者

### 服务器

- **beamai_mcp_server** - MCP 服务器
- **beamai_mcp_server_sup** - 服务器监督者
- **beamai_mcp_handler** - 请求处理器
- **beamai_mcp_cowboy_handler** - Cowboy 适配器

### 传输层

- **beamai_mcp_transport** - 传输层行为
- **beamai_mcp_transport_stdio** - stdio 传输
- **beamai_mcp_transport_http** - HTTP 传输
- **beamai_mcp_transport_sse** - SSE 传输

## API 文档

### beamai_mcp_client

```erlang
%% 启动客户端
beamai_mcp_client:start_link(Config) -> {ok, Pid} | {error, Reason}.

%% 停止客户端
beamai_mcp_client:stop(Pid) -> ok.

%% 初始化连接
beamai_mcp_client:initialize(Pid) -> {ok, ServerInfo} | {error, Reason}.

%% 心跳
beamai_mcp_client:ping(Pid) -> ok | {error, Reason}.

%% 工具操作
beamai_mcp_client:list_tools(Pid) -> {ok, Tools} | {error, Reason}.
beamai_mcp_client:list_tools(Pid, Options) -> {ok, Tools} | {error, Reason}.
beamai_mcp_client:call_tool(Pid, Name, Args) -> {ok, Result} | {error, Reason}.
beamai_mcp_client:call_tool(Pid, Name, Args, Options) -> {ok, Result} | {error, Reason}.

%% 资源操作
beamai_mcp_client:list_resources(Pid) -> {ok, Resources} | {error, Reason}.
beamai_mcp_client:list_resources(Pid, Options) -> {ok, Resources} | {error, Reason}.
beamai_mcp_client:read_resource(Pid, Uri) -> {ok, Contents} | {error, Reason}.
beamai_mcp_client:subscribe_resource(Pid, Uri) -> ok | {error, Reason}.
beamai_mcp_client:unsubscribe_resource(Pid, Uri) -> ok | {error, Reason}.

%% 提示操作
beamai_mcp_client:list_prompts(Pid) -> {ok, Prompts} | {error, Reason}.
beamai_mcp_client:get_prompt(Pid, Name, Args) -> {ok, Prompt} | {error, Reason}.

%% 状态查询
beamai_mcp_client:get_state(Pid) -> {ok, State}.
beamai_mcp_client:get_capabilities(Pid) -> {ok, Capabilities} | {error, Reason}.
```

### beamai_mcp_server

```erlang
%% 启动服务器
beamai_mcp_server:start_link(Config) -> {ok, Pid} | {error, Reason}.

%% 停止服务器
beamai_mcp_server:stop(Pid) -> ok.

%% 处理请求
beamai_mcp_server:handle_request(Pid, Request) -> Response.
beamai_mcp_server:handle_notification(Pid, Notification) -> ok.

%% 注册原语
beamai_mcp_server:register_tool(Pid, Tool) -> ok | {error, Reason}.
beamai_mcp_server:register_resource(Pid, Resource) -> ok | {error, Reason}.
beamai_mcp_server:register_prompt(Pid, Prompt) -> ok | {error, Reason}.

%% 注销原语
beamai_mcp_server:unregister_tool(Pid, Name) -> ok.
beamai_mcp_server:unregister_resource(Pid, Uri) -> ok.
beamai_mcp_server:unregister_prompt(Pid, Name) -> ok.

%% 获取会话
beamai_mcp_server:get_session(Pid) -> {ok, Session} | {error, Reason}.
```

### beamai_mcp_types

```erlang
%% 创建工具
beamai_mcp_types:make_tool(Name, Description, InputSchema, Handler) -> #mcp_tool{}.

%% 创建资源
beamai_mcp_types:make_resource(Uri, Name, Options) -> #mcp_resource{}.

%% 创建提示
beamai_mcp_types:make_prompt(Name, Description, Arguments) -> #mcp_prompt{}.

%% 记录转 Map
beamai_mcp_types:tool_to_map(Tool) -> Map.
beamai_mcp_types:resource_to_map(Resource) -> Map.
beamai_mcp_types:prompt_to_map(Prompt) -> Map.
```

## 使用示例

### 使用 MCP 客户端（stdio 传输）

```erlang
%% 连接文件系统 MCP 服务器
{ok, Client} = beamai_mcp_client:start_link(#{
    transport => stdio,
    command => "npx",
    args => ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"],
    client_info => #{name => <<"my-app">>, version => <<"1.0">>}
}),

%% 初始化连接
{ok, ServerInfo} = beamai_mcp_client:initialize(Client),
io:format("连接到: ~s v~s~n", [
    maps:get(name, ServerInfo),
    maps:get(version, ServerInfo)
]),

%% 列出可用工具
{ok, #{tools := Tools}} = beamai_mcp_client:list_tools(Client),
lists:foreach(fun(Tool) ->
    io:format("工具: ~s - ~s~n", [
        maps:get(name, Tool),
        maps:get(description, Tool)
    ])
end, Tools),

%% 调用工具
{ok, Result} = beamai_mcp_client:call_tool(Client,
    <<"read_file">>,
    #{<<"path">> => <<"/tmp/test.txt">>}
),
io:format("文件内容: ~p~n", [Result]),

%% 关闭客户端
beamai_mcp_client:stop(Client).
```

### 创建 MCP 服务器

```erlang
%% 定义工具
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

%% 定义资源
ConfigResource = beamai_mcp_types:make_resource(
    <<"config://app/settings">>,
    <<"Application Settings">>,
    #{
        description => <<"Current application configuration">>,
        mime_type => <<"application/json">>
    }
),

%% 启动服务器
{ok, Server} = beamai_mcp_server:start_link(#{
    tools => [EchoTool],
    resources => [ConfigResource],
    prompts => [],
    server_info => #{
        name => <<"my-mcp-server">>,
        version => <<"1.0.0">>
    }
}),

%% 处理请求
RequestJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}">>,
Response = beamai_mcp_server:handle_request(Server, jsx:decode(RequestJson)).
```

### 与 Cowboy 集成

```erlang
%% 路由配置
Routes = [
    {"/mcp", beamai_mcp_cowboy_handler, #{server => Server}},
    {"/mcp/sse", beamai_mcp_cowboy_handler, #{server => Server, transport => sse}}
],

%% 启动 HTTP 服务器
{ok, _} = cowboy:start_clear(mcp_http,
    [{port, 8080}],
    #{env => #{dispatch => cowboy_router:compile([{'_', Routes}])}}
).
```

### 使用 HTTP 传输连接

```erlang
%% 连接 HTTP MCP 服务器
{ok, Client} = beamai_mcp_client:start_link(#{
    transport => http,
    base_url => <<"http://localhost:8080/mcp">>,
    client_info => #{name => <<"http-client">>, version => <<"1.0">>}
}),

{ok, _} = beamai_mcp_client:initialize(Client),
{ok, Tools} = beamai_mcp_client:list_tools(Client).
```

### 分页查询

```erlang
%% 列出工具（分页）
{ok, Result1} = beamai_mcp_client:list_tools(Client, #{cursor => undefined}),
#{tools := Tools1, next_cursor := NextCursor} = Result1,

%% 获取下一页
case NextCursor of
    undefined ->
        %% 没有更多数据
        ok;
    Cursor ->
        {ok, Result2} = beamai_mcp_client:list_tools(Client, #{cursor => Cursor}),
        #{tools := Tools2} = Result2
end.
```

### 资源订阅

```erlang
%% 订阅资源变更
ok = beamai_mcp_client:subscribe_resource(Client, <<"file:///tmp/config.json">>),

%% 处理通知（在客户端进程中）
handle_info({mcp_notification, <<"notifications/resources/updated">>, Params}, State) ->
    Uri = maps:get(uri, Params),
    io:format("资源已更新: ~s~n", [Uri]),
    %% 重新读取资源
    {ok, Contents} = beamai_mcp_client:read_resource(Client, Uri),
    {noreply, State};

%% 取消订阅
ok = beamai_mcp_client:unsubscribe_resource(Client, <<"file:///tmp/config.json">>).
```

## 配置选项

### 客户端配置

```erlang
Config = #{
    %% 传输层配置
    transport => stdio | http | sse,

    %% stdio 传输选项
    command => "npx",
    args => ["-y", "@modelcontextprotocol/server-xxx"],
    env => #{<<"KEY">> => <<"value">>},

    %% HTTP 传输选项
    base_url => <<"http://localhost:8080">>,
    headers => [{<<"Authorization">>, <<"Bearer xxx">>}],

    %% 客户端信息
    client_info => #{
        name => <<"client-name">>,
        version => <<"1.0.0">>
    },

    %% 超时设置
    timeout => 30000,

    %% 能力声明
    capabilities => #{
        roots => #{listChanged => true},
        sampling => #{}
    }
}.
```

### 服务器配置

```erlang
Config = #{
    %% 原语列表
    tools => [Tool1, Tool2],
    resources => [Resource1, Resource2],
    prompts => [Prompt1, Prompt2],

    %% 服务器信息
    server_info => #{
        name => <<"server-name">>,
        version => <<"1.0.0">>
    },

    %% 能力声明
    capabilities => #{
        tools => #{listChanged => true},
        resources => #{subscribe => true, listChanged => true},
        prompts => #{listChanged => true}
    }
}.
```

## 依赖

- beamai_core
- jsx
- hackney

## 许可证

Apache-2.0
