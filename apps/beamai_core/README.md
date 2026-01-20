# Agent Core

[English](README_EN.md) | 中文

Agent Framework 的核心模块，提供 Agent 行为定义、数据类型、图计算和通用工具。

## 模块概览

### Agent 行为与类型

- **beamai_behaviour** - Agent 行为定义（behaviour）
- **beamai_message** - 消息类型和转换
- **beamai_result** - 结果类型定义
- **beamai_tool** - 工具定义和管理
- **beamai_types** - 通用类型定义

### 协议支持

- **agent_jsonrpc** - JSON-RPC 2.0 编解码
- **beamai_sse** - Server-Sent Events (SSE) 支持

### 工具函数

- **beamai_utils** - 通用工具函数
- **beamai_http** - HTTP 客户端封装

### 图计算（Graph）

基于 LangGraph 理念的图计算引擎：

- **graph** - 图结构核心
- **graph_builder** - 图构建器
- **graph_dsl** - 图 DSL
- **graph_node** - 节点定义
- **graph_edge** - 边定义
- **graph_runner** - 图执行器
- **graph_state** - 状态管理
- **graph_send** - 消息发送

### Pregel 计算模型

分布式图计算 Pregel 模型实现：

- **pregel** - Pregel 核心
- **pregel_master** - 主控节点
- **pregel_worker** - 工作节点
- **pregel_vertex** - 顶点定义
- **pregel_graph** - Pregel 图
- **pregel_partition** - 分区管理
- **pregel_barrier** - 同步屏障
- **pregel_combiner** - 消息合并

### 图计算模式

- **graph_parallel_experts** - 并行专家模式
- **graph_compute** - 图计算工具

## API 文档

### beamai_behaviour

```erlang
%% 定义 Agent 行为
-callback init(Config :: map()) -> {ok, State :: term()} | {error, Reason :: term()}.
-callback handle_message(Message :: map(), State :: term()) ->
    {reply, Response :: term(), NewState :: term()} | {error, Reason :: term()}.
```

### agent_jsonrpc

```erlang
%% 编码请求
agent_jsonrpc:encode_request(Id, Method, Params) -> binary().

%% 编码响应
agent_jsonrpc:encode_response(Id, Result) -> binary().

%% 编码错误
agent_jsonrpc:encode_error(Id, Code, Message) -> binary().

%% 解码消息
agent_jsonrpc:decode(JsonBin) -> {ok, map()} | {error, term()}.
```

### graph_builder

```erlang
%% 创建新图
graph_builder:new() -> builder().

%% 添加节点
graph_builder:add_node(Builder, Name, NodeFun) -> builder().

%% 添加边
graph_builder:add_edge(Builder, From, To) -> builder().
graph_builder:add_conditional_edges(Builder, From, CondFun, Edges) -> builder().

%% 设置入口和终点
graph_builder:set_entry_point(Builder, NodeName) -> builder().
graph_builder:set_finish_point(Builder, NodeName) -> builder().

%% 编译图
graph_builder:compile(Builder) -> {ok, graph()} | {error, term()}.
```

## 使用示例

### 创建简单图

```erlang
%% 创建图构建器
Builder = graph_builder:new(),

%% 添加节点
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

%% 添加边
Builder4 = graph_builder:add_edge(Builder3, start, process),
Builder5 = graph_builder:add_edge(Builder4, process, finish),

%% 设置入口和终点
Builder6 = graph_builder:set_entry_point(Builder5, start),
Builder7 = graph_builder:set_finish_point(Builder6, finish),

%% 编译并运行
{ok, Graph} = graph_builder:compile(Builder7),
{ok, Result} = graph_runner:run(Graph, #{}).
```

### 使用 JSON-RPC

```erlang
%% 编码请求
Request = agent_jsonrpc:encode_request(1, <<"tools/call">>, #{
    <<"name">> => <<"calculator">>,
    <<"arguments">> => #{<<"expression">> => <<"1 + 1">>}
}),

%% 解码响应
{ok, Response} = agent_jsonrpc:decode(ResponseBin),
Result = maps:get(<<"result">>, Response).
```

## 依赖

- jsx - JSON 编解码
- uuid - UUID 生成
- hackney - HTTP 客户端

## 许可证

Apache-2.0
