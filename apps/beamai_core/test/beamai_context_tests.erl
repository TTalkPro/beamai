%%%-------------------------------------------------------------------
%%% @doc beamai_context 三分区 + state 折叠单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_context_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 三分区结构
%%====================================================================

new_has_three_partitions_test() ->
    Ctx = beamai_context:new(),
    ?assertEqual(#{}, beamai_context:variables(Ctx)),
    ?assertEqual(#{}, beamai_context:get_state(Ctx)),
    ?assertEqual(undefined, beamai_context:get_kernel(Ctx)),
    ?assertEqual(undefined, beamai_context:conversation_id(Ctx)).

env_vars_roundtrip_test() ->
    Ctx0 = beamai_context:new(#{<<"a">> => 1}),
    ?assertEqual(1, beamai_context:get(Ctx0, <<"a">>)),
    ?assertEqual(1, beamai_context:get(Ctx0, a)),               %% atom key 归一
    Ctx1 = beamai_context:set(Ctx0, b, 2),
    ?assertEqual(2, beamai_context:get(Ctx1, <<"b">>)),
    ?assertEqual(default, beamai_context:get(Ctx1, <<"missing">>, default)),
    ?assert(beamai_context:has_key(Ctx1, <<"a">>)),
    ?assertEqual(lists:sort([<<"a">>, <<"b">>]), lists:sort(beamai_context:keys(Ctx1))).

env_kernel_convid_test() ->
    Ctx0 = beamai_context:new(),
    Ctx1 = beamai_context:with_kernel(Ctx0, my_kernel),
    Ctx2 = beamai_context:with_conversation_id(Ctx1, <<"conv-1">>),
    ?assertEqual(my_kernel, beamai_context:get_kernel(Ctx2)),
    ?assertEqual(<<"conv-1">>, beamai_context:conversation_id(Ctx2)),
    %% env 变更不污染 state
    ?assertEqual(#{}, beamai_context:get_state(Ctx2)).

filter_state_roundtrip_test() ->
    Ctx0 = beamai_context:new(),
    ?assertEqual(#{init => true}, beamai_context:filter_state(Ctx0, <<"f">>, #{init => true})),
    Ctx1 = beamai_context:set_filter_state(Ctx0, <<"f">>, #{count => 3}),
    ?assertEqual(#{count => 3}, beamai_context:filter_state(Ctx1, <<"f">>, #{})),
    %% filter 私有状态与 state 分区互不干扰
    ?assertEqual(#{}, beamai_context:get_state(Ctx1)).

%%====================================================================
%% state API
%%====================================================================

state_get_with_default_test() ->
    Ctx = beamai_context:with_state(beamai_context:new(), #{<<"k">> => 42}),
    ?assertEqual(42, beamai_context:state_get(Ctx, <<"k">>)),
    ?assertEqual(42, beamai_context:state_get(Ctx, k)),
    ?assertEqual(undefined, beamai_context:state_get(Ctx, <<"none">>)),
    ?assertEqual(fallback, beamai_context:state_get(Ctx, <<"none">>, fallback)).

%%====================================================================
%% apply_writes：折叠语义
%%====================================================================

apply_writes_empty_is_noop_test() ->
    Ctx = beamai_context:new(),
    {Ctx1, Conflicts} = beamai_context:apply_writes(Ctx, [{1, #{}}, {2, #{}}], #{}),
    ?assertEqual(#{}, beamai_context:get_state(Ctx1)),
    ?assertEqual([], Conflicts).

apply_writes_last_writer_by_index_test() ->
    %% 未声明槽：按 index 升序折叠，index 大者胜（与输入列表顺序无关）
    Ctx = beamai_context:new(),
    In = [{2, #{<<"k">> => b}}, {1, #{<<"k">> => a}}],   %% 故意乱序输入
    {Ctx1, Conflicts} = beamai_context:apply_writes(Ctx, In, #{}),
    ?assertEqual(b, beamai_context:state_get(Ctx1, <<"k">>)),
    ?assertEqual([<<"k">>], Conflicts).                   %% 同批双写未声明槽 → conflict

apply_writes_declared_reducer_no_conflict_test() ->
    %% 声明 conj 槽：两工具写全收，且不算 conflict
    Slots = #{<<"notes">> => #{init => [], reduce => fun(Acc, V) -> [V | Acc] end}},
    Ctx = beamai_context:new(),
    In = [{1, #{<<"notes">> => a}}, {2, #{<<"notes">> => b}}],
    {Ctx1, Conflicts} = beamai_context:apply_writes(Ctx, In, Slots),
    ?assertEqual([b, a], beamai_context:state_get(Ctx1, <<"notes">>)),  %% fold 1 then 2
    ?assertEqual([], Conflicts).

apply_writes_sum_reducer_test() ->
    Slots = #{<<"budget">> => #{init => 0, reduce => fun erlang:'+'/2}},
    Ctx = beamai_context:new(),
    In = [{1, #{<<"budget">> => 5}}, {2, #{<<"budget">> => 7}}, {3, #{<<"budget">> => 3}}],
    {Ctx1, _} = beamai_context:apply_writes(Ctx, In, Slots),
    ?assertEqual(15, beamai_context:state_get(Ctx1, <<"budget">>)).

apply_writes_accumulates_across_calls_test() ->
    %% 跨轮：第二次折叠在第一次结果之上（reduce 用当前 state 值为 Acc）
    Slots = #{<<"n">> => #{init => 0, reduce => fun erlang:'+'/2}},
    Ctx0 = beamai_context:new(),
    {Ctx1, _} = beamai_context:apply_writes(Ctx0, [{1, #{<<"n">> => 10}}], Slots),
    {Ctx2, _} = beamai_context:apply_writes(Ctx1, [{1, #{<<"n">> => 5}}], Slots),
    ?assertEqual(15, beamai_context:state_get(Ctx2, <<"n">>)).

apply_writes_single_writer_no_conflict_test() ->
    Ctx = beamai_context:new(),
    {_, Conflicts} = beamai_context:apply_writes(Ctx, [{1, #{<<"k">> => v}}, {2, #{<<"other">> => w}}], #{}),
    ?assertEqual([], Conflicts).
