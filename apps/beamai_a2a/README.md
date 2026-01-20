# Agent A2A

[English](README_EN.md) | 中文

Agent-to-Agent（A2A）协议实现，支持 Agent 间的标准化通信。

## 特性

- A2A 协议服务端和客户端实现
- JSON-RPC 2.0 通信
- 任务生命周期管理
- Agent Card 发现
- Push 通知支持
- 认证（API Key）
- 限流（Rate Limiting）
- SSE 流式响应

## 模块概览

### 核心模块

- **beamai_a2a_server** - A2A 服务器核心
- **beamai_a2a_client** - A2A 客户端
- **beamai_a2a_task** - 任务管理
- **beamai_a2a_types** - 类型定义

### 中间件模块

- **beamai_a2a_handler** - JSON-RPC 方法处理器
- **beamai_a2a_middleware** - 认证限流中间件
- **beamai_a2a_convert** - JSON 转换工具

### 功能模块

- **beamai_a2a_card** - Agent Card 生成
- **beamai_a2a_card_cache** - Agent Card 缓存
- **beamai_a2a_push** - Push 通知
- **beamai_a2a_auth** - API Key 认证
- **beamai_a2a_rate_limit** - 限流

### 协议模块

- **beamai_a2a_jsonrpc** - JSON-RPC 编解码
- **beamai_a2a_http_handler** - HTTP 处理器
- **beamai_a2a_cowboy_handler** - Cowboy 适配器

## API 文档

### beamai_a2a_server

```erlang
%% 启动服务器
beamai_a2a_server:start_link(Opts) -> {ok, Pid} | {error, Reason}.
beamai_a2a_server:start(Opts) -> {ok, Pid} | {error, Reason}.

%% 停止服务器
beamai_a2a_server:stop(Server) -> ok.

%% 处理请求
beamai_a2a_server:handle_request(Server, Request) -> {ok, Response} | {error, Reason}.
beamai_a2a_server:handle_json(Server, JsonBin) -> {ok, ResponseJson} | {error, Reason}.

%% 带认证的请求处理
beamai_a2a_server:handle_json_with_auth(Server, JsonBin, Headers) ->
    {ok, ResponseJson, RateLimitInfo} |
    {error, auth_error, ErrorJson} |
    {error, rate_limited, ErrorJson}.

%% 获取 Agent Card
beamai_a2a_server:get_agent_card(Server) -> {ok, Card} | {error, Reason}.

%% 任务控制
beamai_a2a_server:request_input(Server, TaskId, Message) -> ok | {error, Reason}.
beamai_a2a_server:complete_task(Server, TaskId, Result) -> ok | {error, Reason}.
beamai_a2a_server:fail_task(Server, TaskId, Error) -> ok | {error, Reason}.
```

### beamai_a2a_client

```erlang
%% Agent 发现
beamai_a2a_client:discover(BaseUrl) -> {ok, Card} | {error, Reason}.
beamai_a2a_client:discover(BaseUrl, Options) -> {ok, Card} | {error, Reason}.

%% 发送消息
beamai_a2a_client:send_message(Endpoint, Message) -> {ok, Task} | {error, Reason}.
beamai_a2a_client:send_message(Endpoint, Message, TaskId) -> {ok, Task} | {error, Reason}.
beamai_a2a_client:send_message(Endpoint, Message, TaskId, Options) -> {ok, Task} | {error, Reason}.

%% 任务管理
beamai_a2a_client:get_task(Endpoint, TaskId) -> {ok, Task} | {error, Reason}.
beamai_a2a_client:cancel_task(Endpoint, TaskId) -> {ok, Task} | {error, Reason}.

%% 流式请求
beamai_a2a_client:send_message_stream(Endpoint, Message, Callback) -> {ok, Result} | {error, Reason}.

%% 便捷函数
beamai_a2a_client:create_text_message(Text) -> Message.
beamai_a2a_client:create_text_message(Role, Text) -> Message.
```

### beamai_a2a_task

```erlang
%% 启动任务
beamai_a2a_task:start_link(Config) -> {ok, Pid} | {error, Reason}.

%% 获取任务信息
beamai_a2a_task:get_task(Pid) -> {ok, Task} | {error, Reason}.
beamai_a2a_task:get_id(Pid) -> TaskId.
beamai_a2a_task:get_status(Pid) -> Status.
beamai_a2a_task:get_history(Pid) -> History.

%% 更新任务状态
beamai_a2a_task:update_status(Pid, Status) -> ok | {error, Reason}.
beamai_a2a_task:add_message(Pid, Message) -> ok.
beamai_a2a_task:add_artifact(Pid, Artifact) -> ok.
```

## 使用示例

### 启动 A2A 服务器

```erlang
%% 配置
AgentConfig = #{
    name => <<"my-agent">>,
    description => <<"A helpful AI assistant">>,
    url => <<"https://agent.example.com">>,

    %% LLM 配置
    llm => #{
        provider => anthropic,
        model => <<"glm-4">>,
        api_key => os:getenv("ZHIPU_API_KEY"),
        base_url => <<"https://open.bigmodel.cn/api/anthropic/v1">>
    }
},

%% 启动服务器
{ok, Server} = beamai_a2a_server:start_link(#{
    agent_config => AgentConfig
}),

%% 获取 Agent Card
{ok, Card} = beamai_a2a_server:get_agent_card(Server).
```

### 使用 A2A 客户端

```erlang
%% 发现远程 Agent
{ok, Card} = beamai_a2a_client:discover("https://agent.example.com"),

%% 查看 Agent 能力
Name = maps:get(name, Card),
Skills = maps:get(skills, Card, []),

%% 发送消息
Message = beamai_a2a_client:create_text_message(<<"Hello, how can you help me?">>),
{ok, Task} = beamai_a2a_client:send_message(
    "https://agent.example.com/a2a",
    Message
),

%% 检查任务状态
TaskId = maps:get(id, Task),
{ok, UpdatedTask} = beamai_a2a_client:get_task(
    "https://agent.example.com/a2a",
    TaskId
),
Status = maps:get(status, UpdatedTask).
```

### 流式响应

```erlang
%% 流式回调
Callback = fun
    (#{type := <<"task_status_update">>, task := Task}) ->
        io:format("状态更新: ~p~n", [maps:get(status, Task)]);
    (#{type := <<"task_artifact_update">>, artifact := Artifact}) ->
        io:format("产出物: ~p~n", [Artifact]);
    (Event) ->
        io:format("事件: ~p~n", [Event])
end,

%% 发送流式消息
Message = beamai_a2a_client:create_text_message(<<"Tell me a story">>),
{ok, Result} = beamai_a2a_client:send_message_stream(
    "https://agent.example.com/a2a/stream",
    Message,
    Callback
).
```

### 带认证的服务器

```erlang
%% 配置认证
AuthConfig = #{
    api_keys => [
        #{key => <<"sk-xxx">>, name => <<"Client 1">>},
        #{key => <<"sk-yyy">>, name => <<"Client 2">>}
    ]
},

%% 配置限流
RateLimitConfig = #{
    requests_per_minute => 60,
    requests_per_hour => 1000
},

%% 启动带认证的服务器
{ok, Server} = beamai_a2a_server:start_link(#{
    agent_config => AgentConfig,
    auth_config => AuthConfig,
    rate_limit_config => RateLimitConfig
}),

%% 处理带认证的请求
Headers = [{<<"authorization">>, <<"Bearer sk-xxx">>}],
case beamai_a2a_server:handle_json_with_auth(Server, RequestJson, Headers) of
    {ok, ResponseJson, _RateLimitInfo} ->
        %% 成功
        send_response(200, ResponseJson);
    {error, auth_error, ErrorJson} ->
        %% 认证失败
        send_response(401, ErrorJson);
    {error, rate_limited, ErrorJson} ->
        %% 限流
        send_response(429, ErrorJson)
end.
```

### 使用 Cowboy 集成

```erlang
%% 路由配置
Routes = [
    {"/a2a", beamai_a2a_cowboy_handler, #{server => Server}},
    {"/a2a/stream", beamai_a2a_cowboy_handler, #{server => Server, streaming => true}},
    {"/.well-known/agent.json", beamai_a2a_http_handler, #{server => Server}}
],

%% 启动 Cowboy
{ok, _} = cowboy:start_clear(http_listener,
    [{port, 8080}],
    #{env => #{dispatch => cowboy_router:compile([{'_', Routes}])}}
).
```

## 任务状态机

```
submitted -> working -> completed
                |
                +----> input_required -> working -> completed
                |
                +----> failed

                +----> canceled
```

## 依赖

- beamai_core
- beamai_llm
- jsx
- hackney
- cowboy（可选，用于 HTTP 服务）

## 许可证

Apache-2.0
