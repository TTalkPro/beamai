# Graph 的 Pregel 计算函数（全局状态模式）

## 概述

计算函数是 Pregel 执行的核心，定义了每个顶点在每个超级步中如何计算。

**全局状态模式：**
- Master 持有 `global_state`，Worker 是纯计算单元
- 计算函数返回 `#{delta => Map, outbox => List, status => ok}`
- delta 是增量更新，通过 `field_reducers` 合并到 `global_state`
- 顶点只是拓扑结构（id + edges），不含 value

## 计算函数定义

代码位置：`graph_compute.erl`

```erlang
-spec compute_fn(nodes_config()) -> pregel:compute_fn().
compute_fn(NodesConfig) ->
    fun(Ctx) ->
        #{vertex_id := VertexId,
          messages := Messages,
          superstep := Superstep,
          global_state := GlobalState} = Ctx,

        %% 从配置中获取节点定义
        NodeConfig = maps:get(VertexId, NodesConfig, #{}),
        Node = maps:get(node, NodeConfig, undefined),
        Edges = maps:get(edges, NodeConfig, []),

        case VertexId of
            '__start__' ->
                handle_start_node(VertexId, Node, Edges, GlobalState, Superstep);
            '__end__' ->
                handle_end_node(VertexId, GlobalState, Messages);
            _ ->
                handle_regular_node(VertexId, Node, Edges, GlobalState, Messages)
        end
    end.
```

## 计算函数结构图

```
┌─────────────────────────────────────────────────────────────────┐
│                    compute_fn() 计算函数                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  输入: Context (Ctx)                                            │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ vertex_id: 当前顶点ID                                    │   │
│  │ vertex: 当前顶点（纯拓扑结构）                           │   │
│  │ messages: 收到的消息列表                                  │   │
│  │ superstep: 当前超步编号                                   │   │
│  │ global_state: 全局状态（由 Master 传入）                  │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│                              ▼                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │              根据 VertexId 分发处理                       │   │
│  └─────────────────────────────────────────────────────────┘   │
│         │                    │                    │             │
│         ▼                    ▼                    ▼             │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐       │
│  │ __start__   │     │  __end__    │     │ 普通节点     │       │
│  │             │     │             │     │             │       │
│  │ handle_     │     │ handle_     │     │ handle_     │       │
│  │ start_node  │     │ end_node    │     │ regular_node│       │
│  └─────────────┘     └─────────────┘     └─────────────┘       │
│                                                                 │
│  输出: #{delta => Map, outbox => List, status => Status}       │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ delta: 状态增量更新 #{field => value}                    │   │
│  │ outbox: 发送给其他顶点的消息 [{Target, Value}]           │   │
│  │ status: ok | {error, Reason}                            │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## 三种节点处理逻辑

### 1. `__start__` 节点处理

```erlang
handle_start_node(VertexId, Node, Edges, GlobalState, 0) ->
    case graph_state:get(GlobalState, activated, false) of
        true  -> execute_start_node(VertexId, Node, Edges, GlobalState);
        false -> #{delta => #{}, outbox => [], status => ok}
    end;
handle_start_node(_VertexId, _Node, _Edges, _GlobalState, _Superstep) ->
    #{delta => #{}, outbox => [], status => ok}.

execute_start_node(VertexId, Node, Edges, GlobalState) ->
    finish_node_execution(VertexId, Node, Edges, GlobalState).
```

**执行条件：**
- 超步 = 0
- `activated = true`（从 global_state 获取）

**行为：**
- 从 global_state 获取初始状态
- 执行节点逻辑
- 返回 delta 更新和发送消息

### 2. `__end__` 节点处理

```erlang
handle_end_node(VertexId, GlobalState, []) ->
    #{delta => #{}, outbox => [], status => ok};
handle_end_node(VertexId, GlobalState, Messages) ->
    case aggregate_state_messages(Messages) of
        {ok, State} ->
            %% 保存最终状态到 global_state
            Delta = #{result => {ok, State}},
            #{delta => Delta, outbox => [], status => ok};
        {error, _} ->
            #{delta => #{}, outbox => [], status => ok}
    end.
```

**行为：**
- 收集所有前驱节点发来的状态
- 合并状态（如果有多个）
- 返回 delta 更新（result 字段）

### 3. 普通节点处理

```erlang
handle_regular_node(_VertexId, _Node, _Edges, _GlobalState, []) ->
    #{delta => #{}, outbox => [], status => ok};
handle_regular_node(VertexId, Node, Edges, GlobalState, Messages) ->
    case aggregate_state_messages(Messages) of
        {ok, State} ->
            NewGlobalState = graph_state:merge(GlobalState, State),
            finish_node_execution(VertexId, Node, Edges, NewGlobalState);
        {error, no_state_messages} ->
            #{delta => #{}, outbox => [], status => ok}
    end.
```

**执行条件：**
- 收到消息

**行为：**
- 从消息中提取状态
- 执行节点逻辑
- 返回 delta 和发送消息

## 节点执行核心逻辑

```erlang
finish_node_execution(VertexId, Node, Edges, GlobalState) ->
    case graph_node:execute(Node, GlobalState) of
        {ok, NewState} ->
            %% 成功：构建 delta 和 outbox
            Delta = graph_state:diff(GlobalState, NewState),
            Outbox = [{Target, {state, NewState}} || #{target := Target} <- Edges],
            #{delta => Delta, outbox => Outbox, status => ok};
        {error, Reason} ->
            %% 失败：返回错误
            Delta = #{VertexId => {error, Reason}},
            #{delta => Delta, outbox => [], status => {error, Reason}}
    end.
```

**执行步骤：**
1. 调用 `graph_node:execute(Node, GlobalState)` 执行节点逻辑
2. 成功时：计算状态差异 delta，发送新状态到下一节点
3. 失败时：返回错误 delta，不发送消息

## 执行流程示例

```
超级步 0:
┌─────────────────┐
│ __start__       │
│ activated=true  │
└────────┬────────┘
         │ 1. handle_start_node()
         │ 2. graph_node:execute(Node, GlobalState)
         │ 3. 返回 #{delta => D0, outbox => [{llm_call, S0}]}
         ▼

超级步 1:
┌─────────────────┐
│  llm_call       │ ← 收到 {state, S0}
└────────┬────────┘
         │ 1. aggregate_state_messages()
         │ 2. graph_node:execute(Node, S0)
         │ 3. 返回 #{delta => D1, outbox => [{tools, S1}]}
         ▼

超级步 2:
┌─────────────────┐
│ execute_tools   │ ← 收到 {state, S1}
└────────┬────────┘
         │ 1. aggregate_state_messages()
         │ 2. graph_node:execute(Node, S1)
         │ 3. 返回 #{delta => D2, outbox => [{__end__, S2}]}
         ▼

超级步 3:
┌─────────────────┐
│  __end__        │ ← 收到 {state, S2}
└────────┬────────┘
         │ 1. aggregate_state_messages()
         │ 2. 返回 #{delta => #{result => S2}, outbox => []}
         ▼

执行完成，从 global_state.result 提取最终状态
```

## 计算函数特点

| 特点 | 说明 |
|------|------|
| **无状态** | 计算函数本身不持有状态 |
| **依赖 global_state** | 所有状态信息从全局状态获取 |
| **统一入口** | 所有顶点使用同一个计算函数 |
| **分发模式** | 根据 VertexId 分发到不同处理逻辑 |
| **返回 delta** | 返回增量更新而非完整状态 |

## Context 结构

```erlang
-type context() :: #{
    vertex_id := vertex_id(),           %% 当前顶点 ID
    vertex := pregel_vertex:vertex(),   %% 当前顶点（纯拓扑结构）
    messages := [term()],               %% 收到的消息列表
    superstep := non_neg_integer(),     %% 当前超步编号
    global_state := graph_state:state(),%% 全局状态（只读）
    num_vertices := non_neg_integer()   %% 顶点总数
}.
```

## 计算结果结构

```erlang
-type compute_result() :: #{
    delta := #{atom() => term()},       %% 状态增量更新
    outbox := [{vertex_id(), term()}],  %% 发送消息列表
    status := ok | {error, term()}      %% 执行状态
}.
```

## 关键操作

| 操作 | 函数 | 说明 |
|------|------|------|
| **获取顶点ID** | `maps:get(vertex_id, Ctx)` | 从 Context 获取 |
| **获取全局状态** | `maps:get(global_state, Ctx)` | 从 Context 获取 |
| **获取消息** | `maps:get(messages, Ctx)` | 从 Context 获取 |
| **读取状态字段** | `graph_state:get(State, Key)` | 从全局状态读取 |
| **返回 delta** | `#{delta => Map, ...}` | 返回增量更新 |
| **返回消息** | `#{outbox => [{Target, Value}], ...}` | 返回待发消息 |

## Delta 与 Field Reducers

### Delta 格式

```erlang
%% 简单的 key-value 增量更新
Delta = #{
    messages => [NewMessage],    %% 追加到 messages 列表
    context => #{key => value},  %% 合并到 context map
    result => FinalResult        %% 覆盖 result 字段
}.
```

### Field Reducers 配置

```erlang
%% 定义每个字段的合并策略
FieldReducers = #{
    <<"messages">> => fun graph_state_reducer:append_reducer/2,
    <<"context">> => fun graph_state_reducer:merge_reducer/2
    %% 默认使用 last_write_win
}.
```

### 可用的 Reducer

| Reducer | 行为 |
|---------|------|
| `last_write_win` | 新值覆盖旧值（默认） |
| `append_reducer` | 追加到列表 |
| `merge_reducer` | 合并 map |

## 总结

| 组件 | 说明 |
|------|------|
| **计算函数** | `graph_compute:compute_fn/1` |
| **类型** | `fun((context()) -> compute_result())` |
| **特点** | 无状态，从 global_state 读取，返回 delta |
| **分发逻辑** | 根据 VertexId 分发到不同处理函数 |
| **核心操作** | 执行节点、计算 delta、构建 outbox |
| **节点类型** | `__start__`、`__end__`、普通节点 |
