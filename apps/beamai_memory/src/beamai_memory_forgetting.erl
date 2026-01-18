%%%-------------------------------------------------------------------
%%% @doc Memory 智能遗忘模块
%%%
%%% 提供多种遗忘策略，当 Memory 容量达到上限时，智能选择要删除的记忆：
%%%
%%% - importance: 基于重要性遗忘，优先删除低重要性记忆
%%% - lru: 基于最近最少使用（LRU）遗忘
%%% - time_decay: 基于时间衰减遗忘，旧记忆重要性随时间降低
%%% - hybrid: 混合策略，综合多个因素决策
%%%
%%% == 遗忘目标 ==
%%%
%%% 当 Memory 数量超过容量限制时，遗忘策略会：
%%% 1. 评估所有记忆的"保留价值"
%%% 2. 删除保留价值最低的记忆
%%% 3. 保留高价值记忆（重要、频繁访问、新近）
%%%
%%% == 使用场景 ==
%%%
%%% 1. **Memory 容量管理**：自动清理旧记忆
%%% 2. **优先保留重要信息**：避免关键信息被删除
%%% 3. **性能优化**：控制 Memory 大小，提升查询效率
%%%
%%% == 示例 ==
%%%
%%% ```
%%% %% 场景1：基于重要性遗忘
%%% Memories = [M1, M2, M3, M4],
%%% MaxCount = 2,
%%% Filtered = beamai_memory_forgetting:forget(
%%%     Memories,
%%%     MaxCount,
%%%     importance
%%% ).
%%% %% 结果：保留 importance 最高的 2 个记忆
%%%
%%% %% 场景2：时间衰减
%%% OldMemory = #{created_at => 1000, importance => 0.8},
%%% Decayed = beamai_memory_forgetting:decay_importance(
%%%     OldMemory,
%%%     7  %% 7天
%%% ).
%%% %% 结果：importance 降低（如 0.8 -> 0.4）
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_forgetting).

-include_lib("beamai_memory/include/beamai_episodic_memory.hrl").

%%====================================================================
%%% API 导出
%%====================================================================

%% 遗忘策略
-export([
    forget/3,
    batch_forget/4
]).

%% 时间衰减
-export([
    decay_importance/2,
    batch_decay_importance/2
]).

%% 访问跟踪
-export([
    record_access/1,
    record_access/2,
    get_access_info/1
]).

%%====================================================================
%%% 类型定义
%%====================================================================

%% 遗忘策略
-type forgetting_strategy() ::
    importance      %% 基于重要性：优先删除低重要性记忆
    | lru           %% 最近最少使用：优先删除最久未访问的记忆
    | time_decay    %% 时间衰减：优先删除创建时间最早的记忆
    | fifo          %% 先进先出：删除最早创建的记忆（简单模式）
    | hybrid.       %% 混合策略：综合重要性、访问时间、年龄

%% 记忆数据
-type memory() :: #{
    id => binary(),
    content => binary(),
    importance => float(),
    created_at => integer(),
    updated_at => integer(),
    last_accessed_at => integer(),
    access_count => non_neg_integer(),
    metadata => map()
} | #episode{} | #event{}.

%% 遗忘选项
-type forgetting_opts() :: #{
    decay_rate => float(),          %% 衰减率（默认 0.1）
    decay_days => pos_integer(),    %% 衰减天数阈值（默认 30）
    hybrid_weights => map(),        %% 混合策略权重
    access_threshold => integer()   %% 访问阈值（LRU用）
}.

%% 访问信息
-type access_info() :: #{
    last_accessed_at => integer(),
    access_count => non_neg_integer(),
    avg_access_interval => float()
}.

%%====================================================================
%%% API 函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 应用遗忘策略，将记忆列表减少到指定数量
%%
%% 参数：
%% - Memories: 记忆列表（map 或 record 列表）
%% - MaxCount: 保留的最大数量
%% - Strategy: 遗忘策略
%%
%% 返回：过滤后的记忆列表
%%
%% 策略说明：
%% - importance: 按 importance 降序排序，保留 top-k
%% - lru: 按 last_accessed_at 降序排序，保留最近访问的
%% - time_decay: 按 created_at 降序排序，保留最新的
%% - fifo: 按 created_at 升序排序，保留最后加入的
%% - hybrid: 综合评分（详见 calculate_hybrid_score/2）
%%
%% 示例：
%% ```
%% Memories = [
%%     #{id => <<"1">>, importance => 0.3},
%%     #{id => <<"2">>, importance => 0.9},
%%     #{id => <<"3">>, importance => 0.6}
%% ],
%% Filtered = beamai_memory_forgetting:forget(Memories, 2, importance),
%% %% 结果：保留 importance=0.9 和 0.6 的记忆
%% '''
%% @end
%%--------------------------------------------------------------------
-spec forget([memory()], pos_integer(), forgetting_strategy()) -> [memory()].
forget(Memories, MaxCount, _Strategy) when length(Memories) =< MaxCount ->
    %% 未超过容量限制，无需删除
    Memories;

forget(Memories, MaxCount, Strategy) ->
    %% 计算每个记忆的保留分数
    ScoredMemories = lists:map(fun(M) ->
        Score = calculate_retention_score(M, Strategy),
        {Score, M}
    end, Memories),

    %% 按分数降序排序
    SortedScores = lists:sort(fun({A, _}, {B, _}) -> A > B end, ScoredMemories),

    %% 提取前 MaxCount 个记忆
    {_Scores, Filtered} = lists:unzip(lists:sublist(SortedScores, MaxCount)),

    Filtered.

%%--------------------------------------------------------------------
%% @doc 批量应用遗忘策略
%%
%% 参数：
%% - MemoriesGroups: 多个记忆组（按命名空间分组）
%% - MaxCountsPerGroup: 每组的最大数量（map 或 list）
%% - Strategy: 遗忘策略
%% - Opts: 选项
%%
%% 返回：过滤后的记忆组 map
%%
%% 使用场景：
%% 不同命名空间有不同的容量限制
%% ```
%% Groups => #{
%%     [<<"user">>, <<"preferences">>] => [Pref1, Pref2, ...],
%%     [<<"user">>, <<"history">>] => [Hist1, Hist2, ...]
%% },
%% MaxCounts => #{<<"preferences">> => 10, <<"history">> => 100}
%% '''
%% @end
%%--------------------------------------------------------------------
-spec batch_forget(
    {binary(), [memory()]},
    pos_integer() | map(),
    forgetting_strategy(),
    forgetting_opts()
) -> #{binary() => [memory()]}.
batch_forget(MemoryGroups, MaxCounts, Strategy, _Opts) when is_map(MaxCounts) ->
    %% 每个组应用不同的容量限制
    maps:map(fun(_GroupKey, Memories) ->
        MaxCount = maps:get(_GroupKey, MaxCounts, 50),
        forget(Memories, MaxCount, Strategy)
    end, MemoryGroups);

batch_forget(MemoryGroups, MaxCount, Strategy, _Opts) when is_integer(MaxCount) ->
    %% 所有组应用相同的容量限制
    maps:map(fun(_GroupKey, Memories) ->
        forget(Memories, MaxCount, Strategy)
    end, MemoryGroups).

%%--------------------------------------------------------------------
%% @doc 对单个记忆应用时间衰减
%%
%% 参数：
%% - Memory: 记忆数据
%% - DaysElapsed: 经过的天数
%%
%% 返回：衰减后的记忆
%%
%% 衰减公式：
%% NewImportance = OldImportance * e^(-decay_rate * days)
%%
%% 示例：
%% ```
%% Memory = #{importance => 0.9},
%% Decayed = beamai_memory_forgetting:decay_importance(Memory, 30),
%% %% 假设 decay_rate = 0.1，30天后：
%% %% 0.9 * e^(-0.1 * 30) = 0.9 * e^(-3) ≈ 0.045
%% '''
%% @end
%%--------------------------------------------------------------------
-spec decay_importance(memory(), pos_integer()) -> memory().
decay_importance(#{importance := Imp} = Memory, DaysElapsed) when is_map(Memory) ->
    DecayRate = 0.1,  %% 默认衰减率
    NewImportance = Imp * math:exp(-DecayRate * DaysElapsed),
    Memory#{importance := NewImportance};

decay_importance(#episode{importance = Imp} = Episode, DaysElapsed) ->
    DecayRate = 0.1,
    NewImportance = Imp * math:exp(-DecayRate * DaysElapsed),
    Episode#episode{importance = NewImportance};

decay_importance(#event{importance = Imp} = Event, DaysElapsed) ->
    DecayRate = 0.1,
    NewImportance = Imp * math:exp(-DecayRate * DaysElapsed),
    Event#event{importance = NewImportance}.

%%--------------------------------------------------------------------
%% @doc 批量应用时间衰减
%%
%% 参数：
%% - Memories: 记忆列表
%% - DaysElapsed: 经过的天数
%%
%% 返回：衰减后的记忆列表
%%
%% 优化：并行处理多个记忆（可用 parallel_map 替代 lists:map）
%% @end
%%--------------------------------------------------------------------
-spec batch_decay_importance([memory()], pos_integer()) -> [memory()].
batch_decay_importance(Memories, DaysElapsed) ->
    lists:map(fun(M) ->
        decay_importance(M, DaysElapsed)
    end, Memories).

%%--------------------------------------------------------------------
%% @doc 记录记忆访问（使用默认时间戳）
%%
%% 参数：
%% - MemoryId: 记忆 ID
%%
%% 效果：
%% - 更新 last_accessed_at 为当前时间
%% - 增加 access_count
%% - 更新 avg_access_interval
%%
%% 实现说明：
%% 访问信息存储在 ETS 表中（process_forgetting_access）
%% @end
%%--------------------------------------------------------------------
-spec record_access(binary()) -> ok.
record_access(MemoryId) ->
    record_access(MemoryId, erlang:system_time(millisecond)).

%%--------------------------------------------------------------------
%% @doc 记录记忆访问（指定时间戳）
%%
%% 参数：
%% - MemoryId: 记忆 ID
%% - Timestamp: 访问时间戳（毫秒）
%%
%% 礈果：
%% - 更新 last_accessed_at 为指定时间
%% - 增加 access_count
%% - 计算平均访问间隔
%%--------------------------------------------------------------------
-spec record_access(binary(), integer()) -> ok.
record_access(MemoryId, Timestamp) when is_binary(MemoryId), is_integer(Timestamp) ->
    Table = ensure_access_table(),

    case ets:lookup(Table, MemoryId) of
        [{MemoryId, OldCount, OldLastAccess, _OldInterval}] ->
            %% 计算访问间隔
            Interval = Timestamp - OldLastAccess,

            %% 更新平均间隔（简单移动平均）
            NewAvgInterval = case OldCount of
                1 -> Interval;
                _ -> (Interval + _OldInterval) / 2
            end,

            %% 更新访问信息
            ets:insert(Table, {MemoryId, OldCount + 1, Timestamp, NewAvgInterval}),
            ok;
        [] ->
            %% 首次访问
            ets:insert(Table, {MemoryId, 1, Timestamp, 0}),
            ok
    end.

%%--------------------------------------------------------------------
%% @doc 获取记忆的访问信息
%%
%% 参数：
%% - MemoryId: 记忆 ID
%%
%% 返回：访问信息 map 或 undefined
%%
%% 返回值：
%% ```
%% #{
%%     last_accessed_at => 1234567890,
%%     access_count => 5,
%%     avg_access_interval => 10000  %% 毫秒
%% }
%% '''
%%--------------------------------------------------------------------
-spec get_access_info(binary()) -> access_info() | undefined.
get_access_info(MemoryId) ->
    Table = ensure_access_table(),

    case ets:lookup(Table, MemoryId) of
        [{MemoryId, Count, LastAccess, Interval}] ->
            #{
                last_accessed_at => LastAccess,
                access_count => Count,
                avg_access_interval => Interval
            };
        [] ->
            undefined
    end.

%%====================================================================
%%% 内部函数 - 评分策略
%%====================================================================

%%--------------------------------------------------------------------
%% @private 计算记忆的保留分数
%%
%% 分数越高，越应该被保留
%%--------------------------------------------------------------------
calculate_retention_score(Memory, Strategy) ->
    case Strategy of
        importance ->
            %% 策略1：基于重要性
            extract_importance(Memory);
        lru ->
            %% 策略2：基于最近访问时间
            calculate_lru_score(Memory);
        time_decay ->
            %% 策略3：基于创建时间（越新越好）
            calculate_time_score(Memory);
        fifo ->
            %% 策略4：基于加入顺序（与 time_decay 相反）
            calculate_fifo_score(Memory);
        hybrid ->
            %% 策略5：混合评分
            calculate_hybrid_score(Memory)
    end.

%%--------------------------------------------------------------------
%% @private LRU 评分
%%
%% 最近访问的记忆分数高：
%% - 获取 last_accessed_at
%% - 距离当前时间越短，分数越高
%% - 公式：score = 1.0 / (1.0 + hours_since_access)
%%--------------------------------------------------------------------
calculate_lru_score(Memory) ->
    Now = erlang:system_time(millisecond),
    LastAccess = extract_last_accessed_at(Memory, Now),

    %% 计算距离上次访问的小时数
    HoursSinceAccess = (Now - LastAccess) / (1000 * 60 * 60),

    %% 评分：最近访问的分数高
    1.0 / (1.0 + HoursSinceAccess).

%%--------------------------------------------------------------------
%% @private 时间评分
%%
%% 新创建的记忆分数高：
%% - 获取 created_at
%% - 距离当前时间越短，分数越高
%% - 公式：score = 1.0 / (1.0 + days_since_creation)
%%--------------------------------------------------------------------
calculate_time_score(Memory) ->
    Now = erlang:system_time(millisecond),
    CreatedAt = extract_created_at(Memory, Now),

    %% 计算距离创建的天数
    DaysSinceCreation = (Now - CreatedAt) / (1000 * 60 * 60 * 24),

    %% 评分：新创建的分数高
    1.0 / (1.0 + DaysSinceCreation).

%%--------------------------------------------------------------------
%% @private FIFO 评分
%%
%% 最早加入的记忆分数低（与 time_score 相反）
%%--------------------------------------------------------------------
calculate_fifo_score(Memory) ->
    Now = erlang:system_time(millisecond),
    CreatedAt = extract_created_at(Memory, Now),

    %% 计算年龄
    AgeHours = (Now - CreatedAt) / (1000 * 60 * 60),

    %% 评分：年龄越大，分数越低
    1.0 / (1.0 + AgeHours * 10).

%%--------------------------------------------------------------------
%% @private 混合评分
%%
%% 综合多个因素计算保留分数：
%%
%% Score = w1 * importance + w2 * lru_score + w3 * time_score
%%
%% 默认权重：
%% - importance: 0.6（最重要）
%% - lru: 0.3
%% - time: 0.1
%%--------------------------------------------------------------------
calculate_hybrid_score(Memory) ->
    %% 默认权重
    Weights = #{
        importance => 0.6,
        lru => 0.3,
        time => 0.1
    },

    %% 计算各维度评分
    ImpScore = extract_importance(Memory),
    LruScore = calculate_lru_score(Memory),
    TimeScore = calculate_time_score(Memory),

    %% 加权求和
    ImpScore * maps:get(importance, Weights) +
    LruScore * maps:get(lru, Weights) +
    TimeScore * maps:get(time, Weights).

%%====================================================================
%%% 内部函数 - 字段提取
%%====================================================================

%%--------------------------------------------------------------------
%% @private 提取重要性评分
%%--------------------------------------------------------------------
extract_importance(#{importance := Imp}) -> Imp;
extract_importance(#episode{importance = Imp}) -> Imp;
extract_importance(#event{importance = Imp}) -> Imp;
extract_importance(_) -> 0.5.  %% 默认中等重要性

%%--------------------------------------------------------------------
%% @private 提取创建时间
%%--------------------------------------------------------------------
extract_created_at(#{created_at := Ts}, _Default) -> Ts;
extract_created_at(#episode{created_at = Ts}, _Default) -> Ts;
extract_created_at(#event{timestamp = Ts}, _Default) -> Ts;
extract_created_at(_, Default) -> Default.

%%--------------------------------------------------------------------
%% @private 提取最后访问时间
%%--------------------------------------------------------------------
extract_last_accessed_at(#{last_accessed_at := Ts}, _Default) -> Ts;
extract_last_accessed_at(#episode{updated_at = Ts}, _Default) -> Ts;
extract_last_accessed_at(#event{timestamp = Ts}, _Default) -> Ts;
extract_last_accessed_at(_, Default) -> Default.

%%====================================================================
%%% 内部函数 - 访问跟踪表管理
%%====================================================================

%%--------------------------------------------------------------------
%% @private 确保访问跟踪 ETS 表存在
%%--------------------------------------------------------------------
ensure_access_table() ->
    TableName = process_forgetting_access,
    case ets:whereis(TableName) of
        undefined ->
            %% 创建新表
            ets:new(TableName, [
                named_table,
                set,
                public,
                {read_concurrency, true}
            ]);
        _TableId ->
            TableName
    end.
