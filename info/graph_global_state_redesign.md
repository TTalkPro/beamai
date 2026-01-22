# Graph 层重新设计：全局状态模式

## 概述

本文档描述 graph 层从"消息传递模式"重构为"全局状态 + 增量更新模式"的设计方案。

## 背景问题

### 当前模式的问题

1. **并发更新冲突**：多个节点同时修改同一字段（如 `context.counter`），合并时可能丢失更新
2. **状态传递复杂**：每个节点都要处理状态的接收和发送
3. **vertex value 冗余**：对于 Agent 场景，节点主要是计算单元，不需要持有状态

### 示例场景

```
SuperStep N:
  NodeA: counter = 10, 想要 +5
  NodeB: counter = 10, 想要 +3

  当前模式结果: counter = 13 或 15（取决于合并顺序，丢失一个更新）
  期望结果: counter = 18
```

## 设计目标

1. 支持增量更新（delta），避免并发冲突
2. 简化节点逻辑，节点只关注计算
3. 支持单顶点重启，保持事务性
4. 删除不必要的向后兼容

## 架构设计

### 模式对比

| 方面 | 消息传递模式（删除） | 全局状态模式（新） |
|------|---------------------|-------------------|
| 状态位置 | 分布在各 vertex.value | Master 持有 global_state |
| 消息内容 | 完整状态 `{state, State}` | 增量 `{delta, Delta}` |
| Reducer 签名 | `(context) -> [msg]` | `(global_state, [delta]) -> global_state` |
| Vertex 角色 | 状态持有者 + 计算单元 | 纯计算单元 |
| 错误处理 | 复杂（需撤销已 apply 的状态） | 简单（延迟提交） |

### 核心组件

```
┌─────────────────────────────────────────────────────────────────┐
│                         Master                                   │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                    global_state                          │   │
│  │  #{messages, context, scratchpad, tools, ...}           │   │
│  └─────────────────────────────────────────────────────────┘   │
│                            │                                    │
│                     global_state_reducer                        │
│                            │                                    │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                   pending_deltas                         │   │
│  │  [delta1, delta2, ...] (出错时暂存)                      │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
           │ broadcast                    │ collect deltas
           ▼                              ▲
┌──────────────────┐            ┌──────────────────┐
│     Worker 1     │            │     Worker 2     │
│  ┌────────────┐  │            │  ┌────────────┐  │
│  │  Node A    │  │            │  │  Node B    │  │
│  │ (compute)  │──┼── delta1 ──┼──│ (compute)  │  │
│  └────────────┘  │            │  └────────────┘  │
└──────────────────┘            └──────────────────┘
```

## 详细设计

### 1. Delta 与 Field Reducer

#### Delta 格式

Delta 是简单的 `#{field_key => value}` map，不包含特殊操作符：

```erlang
%% Delta 类型：简单的字段 => 值映射
-type delta() :: #{atom() | binary() => term()}.

%% 示例
Delta1 = #{messages => [NewMsg], last_response => Response}.
Delta2 = #{counter => 5, context => #{user_id => 123}}.
```

#### Field Reducer

每个字段的合并逻辑由 field_reducer 决定：

```erlang
%% 字段 reducer 类型
-type field_reducer() :: fun((OldValue :: term(), NewValue :: term()) -> term()).

%% 字段 reducer 配置
-type field_reducers() :: #{atom() | binary() => field_reducer()}.

%% 内置 reducer
-spec last_write_win(term(), term()) -> term().
last_write_win(_Old, New) -> New.

-spec append_reducer(list(), list()) -> list().
append_reducer(Old, New) when is_list(Old), is_list(New) -> Old ++ New;
append_reducer(_, New) -> New.

-spec increment_reducer(number(), number()) -> number().
increment_reducer(Old, New) when is_number(Old), is_number(New) -> Old + New;
increment_reducer(_, New) -> New.

-spec merge_reducer(map(), map()) -> map().
merge_reducer(Old, New) when is_map(Old), is_map(New) -> maps:merge(Old, New);
merge_reducer(_, New) -> New.
```

#### Global State Reducer

```erlang
-type global_state_reducer() :: fun((
    GlobalState :: graph_state:state(),
    Deltas :: [delta()],
    FieldReducers :: field_reducers()
) -> graph_state:state()).

%% 默认实现：按字段应用对应的 reducer
-spec default_global_reducer(graph_state:state(), [delta()], field_reducers()) ->
    graph_state:state().
default_global_reducer(State, Deltas, FieldReducers) ->
    lists:foldl(fun(Delta, AccState) ->
        apply_delta(Delta, AccState, FieldReducers)
    end, State, Deltas).

-spec apply_delta(delta(), graph_state:state(), field_reducers()) -> graph_state:state().
apply_delta(Delta, State, FieldReducers) ->
    maps:fold(fun(Field, NewValue, AccState) ->
        OldValue = graph_state:get(AccState, Field),
        Reducer = maps:get(Field, FieldReducers, fun last_write_win/2),
        MergedValue = Reducer(OldValue, NewValue),
        graph_state:set(AccState, Field, MergedValue)
    end, State, Delta).
```

#### Agent 的 Field Reducers 配置

```erlang
%% beamai_agent 的字段 reducer 配置
agent_field_reducers() ->
    #{
        messages => fun append_reducer/2,
        full_messages => fun append_reducer/2,
        scratchpad => fun append_reducer/2,
        context => fun merge_reducer/2
        %% 其他字段使用默认的 last_write_win
    }.
```

### 2. Master 状态结构

```erlang
-record(master_state, {
    %% 图结构
    graph :: graph(),
    compute_fns :: #{vertex_id() => compute_fn()},

    %% 全局状态
    global_state :: graph_state:state(),
    field_reducers :: field_reducers(),

    %% 延迟提交
    pending_deltas :: [delta()] | undefined,
    pending_inbox :: #{vertex_id() => [term()]} | undefined,

    %% Worker 管理
    workers :: #{non_neg_integer() => pid()},
    num_workers :: pos_integer(),

    %% 超步控制
    superstep :: non_neg_integer(),
    max_supersteps :: pos_integer(),
    barrier :: pregel_barrier:t(),

    %% 状态标志
    halted :: boolean()
}).
```

### 3. 超步执行流程

```erlang
complete_superstep(#master_state{
    global_state = GlobalState,
    field_reducers = FieldReducers,
    workers = Workers
} = State) ->
    %% 1. 汇总结果
    Results = pregel_barrier:get_results(State#master_state.barrier),
    AggregatedResults = pregel_barrier:aggregate_results(Results),

    %% 2. 收集所有 deltas
    AllDeltas = maps:get(deltas, AggregatedResults, []),

    %% 3. 检查是否有失败/中断
    FailedCount = maps:get(failed_count, AggregatedResults, 0),
    InterruptedCount = maps:get(interrupted_count, AggregatedResults, 0),
    HasError = FailedCount > 0 orelse InterruptedCount > 0,

    case HasError of
        true ->
            %% 延迟提交：暂存 deltas，不 apply
            handle_deferred_commit(AllDeltas, AggregatedResults, State);
        false ->
            %% 正常提交：apply 并广播
            handle_normal_commit(AllDeltas, GlobalState, FieldReducers, Workers, State)
    end.

handle_normal_commit(Deltas, GlobalState, FieldReducers, Workers, State) ->
    %% Apply field reducers
    NewGlobalState = apply_deltas(GlobalState, Deltas, FieldReducers),

    %% 广播新状态给所有 Worker
    broadcast_global_state(NewGlobalState, Workers),

    State#master_state{
        global_state = NewGlobalState,
        pending_deltas = undefined
    }.

handle_deferred_commit(Deltas, Results, State) ->
    %% 暂存 deltas 和 inbox
    Inbox = maps:get(inbox, Results, #{}),

    State#master_state{
        pending_deltas = Deltas,
        pending_inbox = Inbox
    }.

%% 应用所有 deltas 到全局状态
apply_deltas(State, Deltas, FieldReducers) ->
    lists:foldl(fun(Delta, AccState) ->
        apply_delta(Delta, AccState, FieldReducers)
    end, State, Deltas).
```

### 4. 重试处理

```erlang
handle_retry_success(RetryResults, #master_state{
    pending_deltas = PendingDeltas,
    global_state = GlobalState,
    field_reducers = FieldReducers,
    workers = Workers
} = State) ->
    %% 1. 获取重试产生的新 deltas
    RetryDeltas = collect_deltas_from_results(RetryResults),

    %% 2. 合并 deltas（替换失败节点的 delta）
    MergedDeltas = merge_retry_deltas(PendingDeltas, RetryDeltas, RetryResults),

    %% 3. 现在可以 apply 了
    NewGlobalState = apply_deltas(GlobalState, MergedDeltas, FieldReducers),

    %% 4. 广播
    broadcast_global_state(NewGlobalState, Workers),

    State#master_state{
        global_state = NewGlobalState,
        pending_deltas = undefined,
        pending_inbox = undefined
    }.

%% 合并策略：成功节点保留原 delta，失败节点使用新 delta
merge_retry_deltas(PendingDeltas, RetryDeltas, RetryResults) ->
    RetriedVertices = get_retried_vertex_ids(RetryResults),

    %% 过滤掉被重试节点的旧 delta
    FilteredPending = lists:filter(fun(Delta) ->
        SourceVertex = maps:get(source_vertex, Delta),
        not lists:member(SourceVertex, RetriedVertices)
    end, PendingDeltas),

    %% 合并
    FilteredPending ++ RetryDeltas.
```

### 5. 简化的 Vertex 结构

```erlang
%% 旧结构（删除）
-record(vertex, {
    id :: vertex_id(),
    value :: term(),      %% 不再需要
    edges :: [edge()]
}).

%% 新结构
-record(vertex, {
    id :: vertex_id(),
    edges :: [edge()],
    config :: map()       %% 可选：节点私有配置（不是状态）
}).

%% 或者更简单：vertex 只是 ID
-type vertex_id() :: atom() | binary().
-type graph() :: #{
    vertices := [vertex_id()],
    edges := [{vertex_id(), vertex_id()}],
    compute_fns := #{vertex_id() => compute_fn()}
}.
```

### 6. 计算函数签名

```erlang
%% 旧签名（删除）
-type old_compute_fn() :: fun((pregel_context()) -> pregel_context()).
%% 需要从 context 获取 vertex value，更新 vertex value

%% 新签名
-type compute_fn() :: fun((compute_context()) -> compute_result()).

-type compute_context() :: #{
    vertex_id := vertex_id(),
    global_state := graph_state:state(),
    inbox := [term()],              %% 来自其他节点的消息（非状态）
    superstep := non_neg_integer(),
    config := map()                 %% 节点私有配置
}.

-type compute_result() :: #{
    delta := delta(),                      %% 状态增量
    outbox => [{vertex_id(), term()}],     %% 发送给其他节点的消息
    vote_to_halt => boolean()
}.
```

### 7. 节点实现示例

```erlang
%% LLM 调用节点
llm_call_compute(#{
    global_state := GS,
    config := #{llm_config := LLMConfig}
} = _Ctx) ->
    %% 1. 从全局状态读取需要的数据
    Messages = graph_state:get(GS, messages),
    Tools = graph_state:get(GS, tools),
    SystemPrompt = graph_state:get(GS, system_prompt),

    %% 2. 执行计算
    case llm_client:chat(LLMConfig, SystemPrompt, Messages, Tools) of
        {ok, Response} ->
            %% 3. 返回 delta（简单的字段 => 值映射）
            %% messages 和 full_messages 会被 append_reducer 处理
            #{
                delta => #{
                    messages => [Response],
                    full_messages => [Response],
                    last_response => Response
                },
                outbox => [{route_decision, Response}]
            };
        {error, Reason} ->
            #{
                delta => #{
                    error => Reason
                },
                vote_to_halt => true
            }
    end.

%% 工具执行节点
tool_executor_compute(#{
    global_state := GS,
    inbox := [{route_decision, Response}],
    config := #{tool_handlers := Handlers}
} = _Ctx) ->
    ToolCalls = extract_tool_calls(Response),

    Results = lists:map(fun(ToolCall) ->
        execute_tool(ToolCall, Handlers, GS)
    end, ToolCalls),

    %% delta 中的 messages 和 scratchpad 会被 append_reducer 处理
    #{
        delta => #{
            messages => Results,
            scratchpad => [{tool_results, Results}],
            tool_results => Results
        },
        outbox => [{llm_call, {tool_results, Results}}]
    }.
```

## Checkpoint 设计

### 数据结构

```erlang
-type checkpoint_data() :: #{
    superstep := non_neg_integer(),
    global_state := graph_state:state(),
    pending_deltas := [delta()] | undefined,
    pending_inbox := #{vertex_id() => [term()]} | undefined
}.
```

### 恢复流程

```erlang
restore_from_checkpoint(CheckpointData, State) ->
    #{
        superstep := Superstep,
        global_state := GlobalState,
        pending_deltas := PendingDeltas,
        pending_inbox := PendingInbox
    } = CheckpointData,

    State#master_state{
        superstep = Superstep,
        global_state = GlobalState,
        pending_deltas = PendingDeltas,
        pending_inbox = PendingInbox
    }.
```

## 删除的内容

### 1. 删除消息传递模式相关代码

- `pregel_vertex:value/1`, `pregel_vertex:set_value/2`
- `pregel_context:set_value/2`
- 旧的 `state_reducer` 机制（基于消息列表）

### 2. 删除向后兼容

- 不再支持 `state_reducer` 选项（使用 `global_state_reducer`）
- 不再支持 `{state, State}` 消息格式（使用 `{delta, Delta}`）
- 不再支持 vertex value 的读写

### 3. 简化的模块结构

```
删除或重构:
- pregel_vertex.erl (简化，只保留 id 和 edges)
- graph_state_reducer.erl (重写为 delta reducer)

保留:
- pregel_master.erl (添加 global_state 支持)
- pregel_worker.erl (修改为接收 global_state)
- pregel_barrier.erl (保持不变)
- graph_state.erl (保持不变)
```

## 迁移路径

### Phase 1: 添加新模式（不删除旧代码）

1. 在 master 中添加 `global_state` 和 `global_state_reducer`
2. 添加 `pending_deltas` 延迟提交机制
3. 添加 `broadcast_global_state` 函数
4. 新节点使用新的 compute_fn 签名

### Phase 2: 迁移 beamai_agent

1. 重写 `beamai_node_registry` 中的节点
2. 更新 `beamai_agent_runner` 使用新模式
3. 测试所有功能

### Phase 3: 删除旧代码

1. 删除 vertex value 相关代码
2. 删除旧的 state_reducer
3. 清理向后兼容代码

## API 变更摘要

### pregel_master

```erlang
%% 新增选项
-type opts() :: #{
    global_state => graph_state:state(),           %% 初始全局状态
    field_reducers => field_reducers()             %% 字段级 reducer 配置
}.

%% 新增 API
-spec get_global_state(pid()) -> graph_state:state().
```

### compute_fn

```erlang
%% 旧（删除）
-type compute_fn() :: fun((pregel_context()) -> pregel_context()).

%% 新
-type compute_fn() :: fun((compute_context()) -> compute_result()).
```

### Delta 格式

```erlang
%% Delta 是简单的 field => value 映射
-type delta() :: #{atom() | binary() => term()}.

%% 示例
#{
    messages => [NewMessage],           %% 由 append_reducer 处理
    context => #{key => value},         %% 由 merge_reducer 处理
    last_response => Response,          %% 由 last_write_win 处理
    counter => 5                        %% 由 increment_reducer 处理（如果配置）
}

%% 合并逻辑由 field_reducers 配置决定
field_reducers() ->
    #{
        messages => fun append_reducer/2,
        context => fun merge_reducer/2,
        counter => fun increment_reducer/2
        %% 未配置的字段使用 last_write_win
    }.
```
