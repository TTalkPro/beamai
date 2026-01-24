-module(beamai_filter_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% new/3,4 Tests
%%====================================================================

new_basic_test() ->
    F = beamai_filter:new(<<"log">>, pre_invocation, fun(Ctx) -> {continue, Ctx} end),
    ?assertEqual(<<"log">>, maps:get(name, F)),
    ?assertEqual(pre_invocation, maps:get(type, F)).

new_with_priority_test() ->
    F = beamai_filter:new(<<"log">>, pre_invocation, fun(Ctx) -> {continue, Ctx} end, 10),
    ?assertEqual(10, maps:get(priority, F)).

%%====================================================================
%% sort_filters/1 Tests
%%====================================================================

sort_filters_test() ->
    F1 = beamai_filter:new(<<"a">>, pre_invocation, fun(C) -> {continue, C} end, 30),
    F2 = beamai_filter:new(<<"b">>, pre_invocation, fun(C) -> {continue, C} end, 10),
    F3 = beamai_filter:new(<<"c">>, pre_invocation, fun(C) -> {continue, C} end, 20),
    Sorted = beamai_filter:sort_filters([F1, F2, F3]),
    [S1, S2, S3] = Sorted,
    ?assertEqual(<<"b">>, maps:get(name, S1)),
    ?assertEqual(<<"c">>, maps:get(name, S2)),
    ?assertEqual(<<"a">>, maps:get(name, S3)).

%%====================================================================
%% apply_pre_filters/4 Tests
%%====================================================================

apply_pre_filters_empty_test() ->
    FuncDef = beamai_function:new(<<"test">>, fun(_) -> {ok, done} end),
    Args = #{key => <<"value">>},
    Ctx = beamai_context:new(),
    ?assertMatch({ok, #{key := <<"value">>}, _},
                 beamai_filter:apply_pre_filters([], FuncDef, Args, Ctx)).

apply_pre_filters_modify_args_test() ->
    Filter = beamai_filter:new(<<"modify">>, pre_invocation,
        fun(#{args := A} = C) ->
            {continue, C#{args => A#{extra => added}}}
        end),
    FuncDef = beamai_function:new(<<"test">>, fun(_) -> {ok, done} end),
    Args = #{key => <<"value">>},
    Ctx = beamai_context:new(),
    {ok, NewArgs, _} = beamai_filter:apply_pre_filters([Filter], FuncDef, Args, Ctx),
    ?assertEqual(added, maps:get(extra, NewArgs)).

apply_pre_filters_skip_test() ->
    Filter = beamai_filter:new(<<"skip">>, pre_invocation,
        fun(_) -> {skip, cached} end),
    FuncDef = beamai_function:new(<<"test">>, fun(_) -> {ok, done} end),
    ?assertEqual({skip, cached},
                 beamai_filter:apply_pre_filters([Filter], FuncDef, #{}, beamai_context:new())).

apply_pre_filters_error_test() ->
    Filter = beamai_filter:new(<<"err">>, pre_invocation,
        fun(_) -> {error, forbidden} end),
    FuncDef = beamai_function:new(<<"test">>, fun(_) -> {ok, done} end),
    ?assertEqual({error, forbidden},
                 beamai_filter:apply_pre_filters([Filter], FuncDef, #{}, beamai_context:new())).

apply_pre_filters_chain_test() ->
    F1 = beamai_filter:new(<<"first">>, pre_invocation,
        fun(#{args := A} = C) ->
            {continue, C#{args => A#{step1 => true}}}
        end, 1),
    F2 = beamai_filter:new(<<"second">>, pre_invocation,
        fun(#{args := A} = C) ->
            {continue, C#{args => A#{step2 => true}}}
        end, 2),
    FuncDef = beamai_function:new(<<"test">>, fun(_) -> {ok, done} end),
    {ok, NewArgs, _} = beamai_filter:apply_pre_filters([F2, F1], FuncDef, #{}, beamai_context:new()),
    ?assertEqual(true, maps:get(step1, NewArgs)),
    ?assertEqual(true, maps:get(step2, NewArgs)).

apply_pre_filters_only_matching_type_test() ->
    PreFilter = beamai_filter:new(<<"pre">>, pre_invocation,
        fun(#{args := A} = C) -> {continue, C#{args => A#{pre => true}}} end),
    PostFilter = beamai_filter:new(<<"post">>, post_invocation,
        fun(#{result := R} = C) -> {continue, C#{result => {modified, R}}} end),
    FuncDef = beamai_function:new(<<"test">>, fun(_) -> {ok, done} end),
    {ok, NewArgs, _} = beamai_filter:apply_pre_filters([PreFilter, PostFilter], FuncDef, #{}, beamai_context:new()),
    ?assertEqual(true, maps:get(pre, NewArgs)).

%%====================================================================
%% apply_post_filters/4 Tests
%%====================================================================

apply_post_filters_empty_test() ->
    FuncDef = beamai_function:new(<<"test">>, fun(_) -> {ok, done} end),
    Ctx = beamai_context:new(),
    ?assertMatch({ok, 42, _}, beamai_filter:apply_post_filters([], FuncDef, 42, Ctx)).

apply_post_filters_modify_result_test() ->
    Filter = beamai_filter:new(<<"double">>, post_invocation,
        fun(#{result := R} = C) ->
            {continue, C#{result => R * 2}}
        end),
    FuncDef = beamai_function:new(<<"test">>, fun(_) -> {ok, done} end),
    ?assertMatch({ok, 84, _},
                 beamai_filter:apply_post_filters([Filter], FuncDef, 42, beamai_context:new())).

apply_post_filters_error_test() ->
    Filter = beamai_filter:new(<<"validate">>, post_invocation,
        fun(#{result := R}) when R < 0 -> {error, negative_result};
           (C) -> {continue, C}
        end),
    FuncDef = beamai_function:new(<<"test">>, fun(_) -> {ok, done} end),
    ?assertEqual({error, negative_result},
                 beamai_filter:apply_post_filters([Filter], FuncDef, -1, beamai_context:new())).

%%====================================================================
%% apply_pre_chat_filters/3 Tests
%%====================================================================

apply_pre_chat_filters_test() ->
    Filter = beamai_filter:new(<<"add_system">>, pre_chat,
        fun(#{messages := Msgs} = C) ->
            SystemMsg = #{role => system, content => <<"Be helpful">>},
            {continue, C#{messages => [SystemMsg | Msgs]}}
        end),
    Messages = [#{role => user, content => <<"Hello">>}],
    {ok, NewMsgs, _} = beamai_filter:apply_pre_chat_filters([Filter], Messages, beamai_context:new()),
    ?assertEqual(2, length(NewMsgs)),
    [First | _] = NewMsgs,
    ?assertEqual(system, maps:get(role, First)).

%%====================================================================
%% Exception Handling Tests
%%====================================================================

filter_exception_test() ->
    Filter = beamai_filter:new(<<"crash">>, pre_invocation,
        fun(_) -> error(boom) end),
    FuncDef = beamai_function:new(<<"test">>, fun(_) -> {ok, done} end),
    ?assertMatch({error, #{class := error, reason := boom}},
                 beamai_filter:apply_pre_filters([Filter], FuncDef, #{}, beamai_context:new())).
