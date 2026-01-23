# Pregel 超步激活机制分析

## 概述

本文档描述 graph 引擎在超步 S 结束后，如何决策超步 S+1 中要执行哪些顶点计算。

## 核心决策逻辑

在 `pregel_superstep.erl` 中，激活源按优先级排列：

```erlang
get_activations_for_superstep(PendingActivations, _LastResults)
    when is_list(PendingActivations) ->
    PendingActivations;                          %% 1. 延迟提交的待处理激活（错误恢复场景）
get_activations_for_superstep(undefined, undefined) ->
    [];                                          %% 2. 首次超步：空列表 = 激活所有顶点
get_activations_for_superstep(undefined, LastResults) ->
    maps:get(activations, LastResults, []).       %% 3. 常规情况：使用上一超步的结果
```

## Activations 的产生

### 核心入口

`graph_compute.erl` — 顶点计算完成后，基于路由边构建激活列表：

```erlang
execute_fun_and_route(VertexId, Fun, GlobalState, RoutingEdges) ->
    try Fun(GlobalState) of
        {ok, NewState} ->
            Delta = compute_delta(GlobalState, NewState),
            Activations = build_activations(RoutingEdges, NewState),
            #{delta => Delta, activations => Activations, status => ok};
        ...
    end.
```

### `build_activations/2` 的逻辑

```erlang
build_activations([], _State) ->
    [?END_NODE];                        %% 无出边 → 激活 END 节点
build_activations(Edges, State) ->
    lists:foldl(fun(Edge, Acc) ->
        case graph_edge:resolve(Edge, State) of
            {ok, TargetNode} -> [TargetNode | Acc];
            {ok, TargetNodes} when is_list(TargetNodes) -> TargetNodes ++ Acc;
            {error, _} -> Acc
        end
    end, [], Edges).
```

### 三种边类型决定激活目标

| 边类型 | 解析规则 | 示例 |
|--------|----------|------|
| `direct` | 无条件激活固定目标 | `direct(a, b)` → 总是激活 `b` |
| `fanout` | 激活所有目标（并行分支） | `fanout(a, [b, c, d])` → 同时激活 `b`, `c`, `d` |
| `conditional` | Router 函数根据状态动态决定目标 | `conditional(a, RouterFun)` → 运行时决定走哪条路 |

### Conditional Edge 的动态路由

`graph_edge.erl` 中 conditional edge 的解析：

```erlang
resolve(#{type := conditional, router := RouterFun, route_map := RouteMap}, State) ->
    case RouterFun(State) of
        Key -> maps:get(Key, RouteMap)   %% 通过 key 查表得到目标节点
    end;
resolve(#{type := conditional, router := RouterFun}, State) ->
    RouterFun(State).                    %% 直接返回目标节点 ID
```

Router 函数接收当前全局状态，可以返回：
- 单个节点 ID
- 节点 ID 列表（并行激活多个）
- 路由表的 key（配合 `route_map` 使用）

## 完整流程

### 1. 超步 S 中顶点计算产生激活列表

Worker 在 `pregel_worker.erl` 中执行计算函数，每个顶点的计算通过 routing edges 解析出下一超步要激活的顶点 ID。

### 2. Worker 收集激活结果

```erlang
process_compute_result(_Id, #{status := ok, delta := Delta} = Result,
                       {DeltaAcc, ActAcc, FailedAcc, InterruptedAcc}) ->
    Activations = maps:get(activations, Result, []),
    {NewDelta, Activations ++ ActAcc, FailedAcc, InterruptedAcc};
```

- `status => ok`: Deltas 和 activations 被收集
- `status => {error, ...}`: 错误被记录，activations 不被收集
- `status => {interrupt, ...}`: activations 不被收集，执行暂停

### 3. Barrier 聚合所有 Worker 的激活

`pregel_barrier.erl` 将各 Worker 返回的激活列表拼接：

```erlang
activations => maps:get(activations, WorkerResult, []) ++
               maps:get(activations, Acc)
```

### 4. Master 在下一超步分发激活到对应 Worker

`pregel_superstep.erl` 通过哈希分区将激活分配给 Worker：

```erlang
group_activations_by_worker(Activations, NumWorkers) ->
    lists:foldl(fun(VertexId, Acc) ->
        WorkerId = pregel_partition:worker_id(VertexId, NumWorkers, hash),
        Existing = maps:get(WorkerId, Acc, []),
        Acc#{WorkerId => [VertexId | Existing]}
    end, #{}, Activations).
```

### 5. Worker 过滤活跃顶点

顶点在 S+1 被计算的条件是：
- 它在激活列表中（被其他顶点通过路由边显式激活），**或者**
- 它尚未 halt（`pregel_vertex:is_active(V) = not is_halted(V)`）

## 终止条件

```erlang
Halted = (TotalActive =:= 0) andalso (TotalActivations =:= 0) andalso (not HasError)
MaxReached = Superstep >= MaxSupersteps - 1
IsDone = Halted orelse MaxReached
```

三种终止原因：
1. 所有顶点都已 halt + 没有待处理的激活 + 没有错误 → `completed`
2. 达到最大超步数 → `max_supersteps`
3. 检测到错误或中断 → 提前终止

## 错误场景下的延迟提交

当超步中有顶点失败时：
- Deltas 不立即合并到 global_state
- Activations 保存为 `pending_activations`
- 等待重试成功后再提交

```erlang
case HasError of
    true ->
        {GlobalState, AllDeltas, AllActivations};        %% 延迟提交
    false ->
        UpdatedState = graph_state_reducer:apply_deltas(
            GlobalState, AllDeltas, FieldReducers),
        {UpdatedState, undefined, undefined}             %% 立即提交
end
```

## 流程图

```
顶点 V 在超步 S 计算完成
        │
        ▼
  Fun(GlobalState) → NewState
        │
        ▼
  遍历 V 的 routing_edges
        │
        ├── direct edge       → 固定目标
        ├── fanout edge       → 多个目标
        └── conditional edge  → RouterFun(NewState) 动态决定目标
        │
        ▼
  activations = [target_1, target_2, ...]
        │
        ▼
  Worker 汇报给 Master
        │
        ▼
  Barrier 聚合所有 Worker 的 activations
        │
        ▼
  Master 通过哈希分区分发给各 Worker
        │
        ▼
  超步 S+1 中被激活的顶点执行计算
```

## 关键设计要点

| 机制 | 说明 |
|------|------|
| 激活来源 | 基于图拓扑（routing edges）+ 运行时状态共同决定 |
| Vote-to-halt | 顶点调用 `halt(V)` 进入休眠，除非被显式激活 |
| 全局状态模型 | 无顶点级消息队列，通过 delta + field reducer 合并到全局状态 |
| 延迟提交 | 出错时不立即更新全局状态，保留 pending_activations 供重试 |
| 哈希分区 | 同一顶点 ID 总是路由到同一 Worker |

## 相关文件

- `pregel_master.erl`: Master 协调逻辑和激活分发
- `pregel_worker.erl`: Worker 计算执行和结果收集
- `pregel_superstep.erl`: 超步完成处理和激活优先级
- `pregel_barrier.erl`: 多 Worker 结果聚合
- `graph_compute.erl`: 路由边解析和激活构建
- `graph_edge.erl`: 边类型定义和解析逻辑
- `pregel_vertex.erl`: 顶点状态管理（halt/activate）
