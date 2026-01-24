# BeamAI Graph

[English](README_EN.md) | 中文

基于 LangGraph 理念的图计算引擎，支持 Graph 构建、执行和 Pregel 分布式计算。

## 特性

- Graph Builder/DSL 声明式图构建
- 条件边和动态路由
- 状态管理和 Reducer
- Graph Snapshot（状态快照）
- Pregel 分布式图计算模型
- 消息传递和同步屏障

## 模块概览

### Graph 核心

- **graph** - 图结构核心数据类型
- **graph_node** - 节点定义
- **graph_edge** - 边定义（普通边和条件边）

### Builder

- **graph_builder** - 图构建器（Builder 模式）
- **graph_dsl** - 声明式 DSL

### Runner

- **graph_runner** - 图执行器
- **graph_snapshot** - 状态快照管理

### State

- **graph_state** - 图状态管理
- **graph_state_reducer** - 字段级状态合并策略
- **graph_command** - 状态命令
- **graph_dispatch** - 消息分发

### Pregel 分布式计算

- **pregel** - Pregel 核心入口
- **pregel_master** - 主控节点
- **pregel_worker** - 工作节点
- **pregel_vertex** - 顶点定义
- **pregel_graph** - Pregel 图结构
- **pregel_partition** - 分区管理
- **pregel_barrier** - 同步屏障
- **pregel_superstep** - 超步管理
- **pregel_dispatch_worker** - 消息分发工作器
- **pregel_retry** - 重试机制
- **pregel_utils** - Pregel 工具函数
- **graph_compute** - 图计算工具

### 应用入口

- **beamai_graph_app** - OTP 应用回调
- **beamai_graph_sup** - 监督树

## API 文档

### graph_builder

```erlang
%% 创建新图构建器
graph_builder:new() -> builder().

%% 添加节点
graph_builder:add_node(Builder, Name, NodeFun) -> builder().

%% 添加边
graph_builder:add_edge(Builder, From, To) -> builder().

%% 添加条件边
graph_builder:add_conditional_edges(Builder, From, CondFun, Edges) -> builder().

%% 设置入口和终点
graph_builder:set_entry_point(Builder, NodeName) -> builder().
graph_builder:set_finish_point(Builder, NodeName) -> builder().

%% 编译图
graph_builder:compile(Builder) -> {ok, graph()} | {error, term()}.
```

### graph_runner

```erlang
%% 执行图
graph_runner:run(Graph, InitialState) -> {ok, FinalState} | {error, Reason}.
graph_runner:run(Graph, InitialState, Opts) -> {ok, FinalState} | {error, Reason}.
```

### graph_state_reducer

```erlang
%% 应用 delta 到状态
graph_state_reducer:apply_delta(State, Delta, FieldReducers) -> NewState.
graph_state_reducer:apply_deltas(State, [Delta], FieldReducers) -> NewState.

%% 内置 Reducer
graph_state_reducer:append_reducer(Old, New) -> Old ++ New.
graph_state_reducer:merge_reducer(Old, New) -> maps:merge(Old, New).
graph_state_reducer:increment_reducer(Old, Delta) -> Old + Delta.
graph_state_reducer:last_write_win_reducer(_Old, New) -> New.
```

**Reducer 类型：**

| 类型 | 格式 | 行为 |
|------|------|------|
| 普通 Reducer | `fun(Old, New) -> Merged` | 同键合并 |
| 转换型 Reducer | `{transform, TargetKey, ReducerFun}` | 从源键读取，写入目标键 |

## 使用示例

### 创建简单图

```erlang
Builder = graph_builder:new(),

Builder1 = graph_builder:add_node(Builder, start, fun(State) ->
    {ok, State#{step => 1}}
end),

Builder2 = graph_builder:add_node(Builder1, process, fun(State) ->
    {ok, State#{step => 2}}
end),

Builder3 = graph_builder:add_node(Builder2, finish, fun(State) ->
    {ok, State}
end),

Builder4 = graph_builder:add_edge(Builder3, start, process),
Builder5 = graph_builder:add_edge(Builder4, process, finish),

Builder6 = graph_builder:set_entry_point(Builder5, start),
Builder7 = graph_builder:set_finish_point(Builder6, finish),

{ok, Graph} = graph_builder:compile(Builder7),
{ok, Result} = graph_runner:run(Graph, #{}).
```

### 条件边

```erlang
Builder = graph_builder:new(),

Builder1 = graph_builder:add_node(Builder, classify, fun(State) ->
    Score = maps:get(score, State, 0),
    {ok, State#{category => if Score > 80 -> high; true -> low end}}
end),

Builder2 = graph_builder:add_node(Builder1, high_path, fun(State) ->
    {ok, State#{result => <<"优秀"/utf8>>}}
end),

Builder3 = graph_builder:add_node(Builder2, low_path, fun(State) ->
    {ok, State#{result => <<"普通"/utf8>>}}
end),

Builder4 = graph_builder:add_conditional_edges(Builder3, classify,
    fun(State) -> maps:get(category, State) end,
    #{high => high_path, low => low_path}),

Builder5 = graph_builder:set_entry_point(Builder4, classify),
Builder6 = graph_builder:set_finish_point(Builder5, high_path),
Builder7 = graph_builder:set_finish_point(Builder6, low_path),

{ok, Graph} = graph_builder:compile(Builder7),
{ok, Result} = graph_runner:run(Graph, #{score => 90}).
```

### 使用 State Reducer

```erlang
%% 定义字段级 Reducer
FieldReducers = #{
    <<"messages">> => fun graph_state_reducer:append_reducer/2,
    <<"counter">> => fun graph_state_reducer:increment_reducer/2
},

%% 节点返回 delta（不是完整状态）
NodeFun = fun(State) ->
    {ok, #{<<"messages">> => [<<"new message">>], <<"counter">> => 1}}
end,

%% Reducer 会将 delta 合并到全局状态
NewState = graph_state_reducer:apply_delta(
    #{<<"messages">> => [], <<"counter">> => 0},
    #{<<"messages">> => [<<"hello">>], <<"counter">> => 1},
    FieldReducers
).
%% NewState = #{<<"messages">> => [<<"hello">>], <<"counter">> => 1}
```

## 依赖

- beamai_core

## 许可证

Apache-2.0
