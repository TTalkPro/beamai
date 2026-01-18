%%%-------------------------------------------------------------------
%%% @doc Pregel 同步屏障模块
%%%
%%% 提供 BSP（批量同步并行）模型的同步屏障实现。
%%% 负责跟踪超步同步状态和汇总 Worker 结果。
%%%
%%% 功能:
%%%   - 创建和管理同步屏障状态
%%%   - 记录 Worker 完成通知
%%%   - 检测屏障完成条件
%%%   - 汇总超步执行结果
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_barrier).

-export([new/1, record_done/2, is_complete/1, get_results/1, reset/2]).
-export([aggregate_results/1]).

%% 同步屏障记录
-record(barrier, {
    expected :: non_neg_integer(),    %% 期望的 Worker 数
    received :: non_neg_integer(),    %% 已收到的完成通知数
    results  :: [map()]               %% Worker 结果列表
}).

-opaque t() :: #barrier{}.
-export_type([t/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 创建新的同步屏障
-spec new(non_neg_integer()) -> t().
new(NumWorkers) ->
    #barrier{
        expected = NumWorkers,
        received = 0,
        results = []
    }.

%% @doc 记录 Worker 完成通知
-spec record_done(map(), t()) -> t().
record_done(Result, #barrier{received = R, results = Rs} = B) ->
    B#barrier{
        received = R + 1,
        results = [Result | Rs]
    }.

%% @doc 检查屏障是否完成
-spec is_complete(t()) -> boolean().
is_complete(#barrier{expected = E, received = R}) ->
    R >= E.

%% @doc 获取所有结果
-spec get_results(t()) -> [map()].
get_results(#barrier{results = Rs}) ->
    Rs.

%% @doc 重置屏障以供下一超步使用
-spec reset(non_neg_integer(), t()) -> t().
reset(NumWorkers, _Barrier) ->
    new(NumWorkers).

%% @doc 汇总 Worker 结果
%% 返回 {活跃顶点数, 消息数}
-spec aggregate_results([map()]) -> {non_neg_integer(), non_neg_integer()}.
aggregate_results(Results) ->
    lists:foldl(
        fun(Result, {AccA, AccM}) ->
            Active = maps:get(active_count, Result, 0),
            Messages = maps:get(message_count, Result, 0),
            {AccA + Active, AccM + Messages}
        end,
        {0, 0},
        Results
    ).
