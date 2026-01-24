%%%-------------------------------------------------------------------
%%% @doc 单顶点重试功能测试（全局状态模式 - 无 inbox 版本）
%%%
%%% 测试 Pregel 层步进式 API 的重试功能
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_single_vertex_retry_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助宏
%%====================================================================

-define(TIMEOUT, 5000).

%%====================================================================
%% 测试数据生成
%%====================================================================

%% 创建简单测试图（三个独立顶点，无边）
simple_graph() ->
    Edges = [],
    InitialValues = #{v1 => 0, v2 => 0, v3 => 0},
    pregel_graph:from_edges(Edges, InitialValues).

%% 总是成功的计算函数（全局状态模式）
success_compute_fn() ->
    fun(#{vertex_id := Id, global_state := GlobalState} = _Ctx) ->
        OldValue = maps:get(Id, GlobalState, 0),
        NewValue = OldValue + 1,
        #{status => ok, delta => #{Id => NewValue}, activations => []}
    end.

%% 第一次失败，第二次成功的计算函数（全局状态模式）
%% 使用 ETS 表跟踪重试次数
fail_then_success_compute_fn(FailVertexId) ->
    %% 创建 ETS 表来跟踪调用次数
    Table = ets:new(retry_tracker, [public, set]),
    fun(#{vertex_id := Id, global_state := _GlobalState} = _Ctx) ->
        case Id =:= FailVertexId of
            true ->
                %% 获取并更新调用次数
                Count = case ets:lookup(Table, Id) of
                    [] -> 0;
                    [{_, N}] -> N
                end,
                ets:insert(Table, {Id, Count + 1}),
                case Count of
                    0 ->
                        %% 第一次：失败
                        #{status => {error, first_attempt_failed},
                          delta => #{},
                          activations => []};
                    _ ->
                        %% 第二次及以后：成功
                        #{status => ok,
                          delta => #{Id => {retried, Count + 1}},
                          activations => []}
                end;
            false ->
                %% 其他顶点：成功
                #{status => ok,
                  delta => #{Id => success},
                  activations => []}
        end
    end.

%% 带激活链的计算函数（失败后重试时激活下游）
%% 使用 ETS 表跟踪重试次数
fail_then_success_with_activations_fn(FailVertexId, DownstreamId) ->
    Table = ets:new(retry_tracker_activations, [public, set]),
    fun(#{vertex_id := Id, global_state := _GlobalState} = _Ctx) ->
        case Id =:= FailVertexId of
            true ->
                Count = case ets:lookup(Table, Id) of
                    [] -> 0;
                    [{_, N}] -> N
                end,
                ets:insert(Table, {Id, Count + 1}),
                case Count of
                    0 ->
                        #{status => {error, first_attempt_failed},
                          delta => #{},
                          activations => []};
                    _ ->
                        %% 重试成功，激活下游顶点
                        #{status => ok,
                          delta => #{Id => {retried_success, Count + 1}},
                          activations => [DownstreamId]}
                end;
            false ->
                %% 其他顶点（如下游顶点）：记录被激活
                #{status => ok,
                  delta => #{Id => activated_by_retry},
                  activations => []}
        end
    end.

%%====================================================================
%% 辅助函数
%%====================================================================

%% 运行步进式 Pregel，遇到失败时重试
run_with_retry(Master) ->
    run_with_retry(Master, 10).  %% 最大重试 10 次

run_with_retry(_Master, MaxRetries) when MaxRetries =< 0 ->
    %% 超过最大重试次数
    {error, max_retries_exceeded};
run_with_retry(Master, MaxRetries) ->
    case pregel_master:step(Master) of
        {continue, #{failed_count := FC, failed_vertices := FVs}} when FC > 0 ->
            %% 有失败的顶点，重试
            VertexIds = [Id || {Id, _} <- FVs],
            case pregel_master:retry(Master, VertexIds) of
                {continue, _Info} ->
                    run_with_retry(Master, MaxRetries - 1);
                {done, _Reason, _Info} ->
                    pregel_master:get_result(Master)
            end;
        {continue, _Info} ->
            %% 无失败，继续下一步
            run_with_retry(Master, MaxRetries);
        {done, _Reason, _Info} ->
            pregel_master:get_result(Master)
    end.

%%====================================================================
%% 测试用例
%%====================================================================

%% 测试：基本重试功能 - 失败顶点可以被重试（全局状态模式）
basic_retry_test() ->
    Graph = simple_graph(),
    ComputeFn = fail_then_success_compute_fn(v1),

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{num_workers => 1}),
    try
        Result = run_with_retry(Master),

        %% 验证执行完成
        ?assertEqual(completed, maps:get(status, Result)),

        %% 验证 v1 被重试成功（通过 global_state 检查）
        GlobalState = maps:get(global_state, Result),
        V1Value = graph_state:get(GlobalState, v1),
        ?assertEqual({retried, 2}, V1Value)
    after
        pregel_master:stop(Master)
    end.

%% 测试：重试成功后 activations 被正确路由
retry_activations_routing_test() ->
    %% 创建图：v1, v2, v3 独立
    Edges = [],
    InitialValues = #{v1 => start, v2 => 0, v3 => 0},
    Graph = pregel_graph:from_edges(Edges, InitialValues),

    %% v1 第一次失败，重试后激活 v2
    ComputeFn = fail_then_success_with_activations_fn(v1, v2),

    {ok, Master} = pregel_master:start_link(Graph, ComputeFn, #{num_workers => 1}),
    try
        Result = run_with_retry(Master),

        ?assertEqual(completed, maps:get(status, Result)),

        %% 验证 v2 被 v1 重试后激活了
        GlobalState = maps:get(global_state, Result),
        V2Value = graph_state:get(GlobalState, v2),
        ?assertEqual(activated_by_retry, V2Value)
    after
        pregel_master:stop(Master)
    end.

%% 测试：pregel:run 简化 API 正常工作（无失败情况）
simple_run_test() ->
    Graph = simple_graph(),
    ComputeFn = success_compute_fn(),

    Result = pregel:run(Graph, ComputeFn, #{num_workers => 1}),

    %% 应该正常完成
    ?assertEqual(completed, maps:get(status, Result)).
