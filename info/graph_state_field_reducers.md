# Graph State 字段级 Reducer 机制

## 概述

在 Pregel BSP 模型中，当多个顶点向同一目标顶点发送状态消息时，需要合并这些状态。
本文档描述了字段级 Reducer 机制的设计和实现。

## 核心问题

### 状态消息流转

在 Graph 执行过程中，顶点间传递的消息是 `{state, graph_state()}` 格式：

```erlang
%% graph_compute.erl 中发送状态消息
pregel:send_message(Ctx, TargetNode, {state, State})
```

### 扇入场景的状态合并

当多个节点并行执行后汇聚到同一节点时（fan-in），需要合并多个状态：

```
Node A ──{state, S1}──┐
                      ├──→ Node C (需要合并 S1 和 S2)
Node B ──{state, S2}──┘
```

## 架构设计

### 分层原则

```
┌─────────────────────────────────────────────────────────────┐
│  应用层 (beamai_agent)                                       │
│  - 定义业务特定的字段 Reducer                                │
│  - messages, full_messages, scratchpad, context 等          │
└─────────────────────────────────────────────────────────────┘
                            │ field_reducers
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  框架层 (beamai_core)                                        │
│  - 提供通用 Reducer 工具                                     │
│  - 不包含业务特定配置                                        │
└─────────────────────────────────────────────────────────────┘
```

### 合并点

状态合并在 Pregel Master 的 `state_reducer` 中执行：

```erlang
%% pregel_master.erl
apply_state_reducer(Outbox, Superstep, StateReducer) ->
    %% 1. 按目标顶点分组
    Grouped = group_by_target(Outbox),
    %% 2. 对每组应用 reducer
    maps:fold(fun(TargetId, Messages, Acc) ->
        Context = #{target_vertex => TargetId, superstep => Superstep, messages => Messages},
        Reduced = StateReducer(Context),
        [{TargetId, M} || M <- Reduced] ++ Acc
    end, [], Grouped).
```

## 实现

### 框架层：graph_state_reducer.erl

提供通用的 Reducer 工具，不包含业务配置：

```erlang
-module(graph_state_reducer).

%% 内置 Reducer 策略
-export([
    append_reducer/2,      %% 列表追加
    merge_reducer/2,       %% Map 深度合并
    last_write_win_reducer/2  %% 后值覆盖（默认）
]).

%% API
-export([
    reducer/0,  %% 返回默认 reducer（所有字段 last_write_win）
    reducer/1   %% 返回自定义字段 reducer
]).

%% 类型定义
-type field_reducer() :: fun((OldValue :: term(), NewValue :: term()) -> term()).
-type field_reducers() :: #{binary() => field_reducer()}.
```

### 框架层：graph_runner.erl

接受 `field_reducers` 选项：

```erlang
-type run_options() :: #{
    %% ... 其他选项
    field_reducers => graph_state_reducer:field_reducers(),  %% 字段级 Reducer（推荐）
    state_reducer => pregel_master:state_reducer()           %% 完整 reducer 函数
}.

%% 优先级：state_reducer > field_reducers > 默认
get_state_reducer(Options) ->
    case maps:get(state_reducer, Options, undefined) of
        undefined ->
            case maps:get(field_reducers, Options, undefined) of
                undefined -> graph_state_reducer:reducer();
                FieldReducers -> graph_state_reducer:reducer(FieldReducers)
            end;
        StateReducer ->
            StateReducer
    end.
```

### 应用层：beamai_agent_runner.erl

定义业务特定的字段 Reducer：

```erlang
%% Agent 特定的字段 Reducer 配置
agent_field_reducers() ->
    #{
        <<"messages">> => fun graph_state_reducer:append_reducer/2,
        <<"full_messages">> => fun graph_state_reducer:append_reducer/2,
        <<"scratchpad">> => fun graph_state_reducer:append_reducer/2,
        <<"context">> => fun graph_state_reducer:merge_reducer/2
    }.

build_run_options(Opts) ->
    BaseOpts = maps:with([on_checkpoint, restore_from], Opts),
    BaseOpts#{field_reducers => agent_field_reducers()}.
```

## 使用方式

### 方式一：使用 field_reducers（推荐）

```erlang
MyFieldReducers = #{
    <<"my_list">> => fun graph_state_reducer:append_reducer/2,
    <<"my_config">> => fun graph_state_reducer:merge_reducer/2
    %% 未指定的字段使用 last_write_win
},

graph:run(Graph, InitState, #{field_reducers => MyFieldReducers})
```

### 方式二：使用完整 state_reducer

```erlang
MyReducer = fun(#{messages := Messages}) ->
    %% 自定义合并逻辑
    ...
end,

graph:run(Graph, InitState, #{state_reducer => MyReducer})
```

## 内置 Reducer 策略

| Reducer | 用途 | 行为 |
|---------|------|------|
| `append_reducer/2` | 列表字段 | `Old ++ New` |
| `merge_reducer/2` | Map 字段 | `maps:merge(Old, New)` |
| `last_write_win_reducer/2` | 默认 | `New` |

## 相关文件

| 文件 | 职责 |
|------|------|
| `beamai_core/.../graph_state_reducer.erl` | 通用 Reducer 工具 |
| `beamai_core/.../graph_runner.erl` | 接受并应用 field_reducers |
| `beamai_core/.../graph_compute.erl` | 使用 reducer 合并状态 |
| `beamai_core/.../pregel_master.erl` | BSP 消息合并时调用 state_reducer |
| `beamai_agent/.../beamai_agent_runner.erl` | 定义 Agent 业务字段 Reducer |

## 设计决策

### 为什么在 state_reducer 中实现？

1. **消息即状态**：Pregel 顶点间消息就是 `{state, graph_state()}`
2. **统一合并点**：BSP 消息路由时是合并的最佳时机
3. **框架一致性**：复用 Pregel 已有的 reducer 机制

### 为什么分离框架层和应用层？

1. **职责分离**：框架提供机制，应用定义策略
2. **避免耦合**：`messages`, `context` 等是 Agent 业务字段，不应侵入框架
3. **可扩展性**：其他应用可以定义自己的字段 Reducer
