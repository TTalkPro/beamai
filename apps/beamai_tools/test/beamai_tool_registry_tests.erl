%%%-------------------------------------------------------------------
%%% @doc beamai_tool_registry 单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助
%%====================================================================

make_tool(Name) ->
    make_tool(Name, <<"Test tool">>).

make_tool(Name, Desc) ->
    #{
        name => Name,
        description => Desc,
        parameters => #{type => object, properties => #{}, required => []},
        handler => fun(_) -> {ok, <<"result">>} end
    }.

%%====================================================================
%% 基础 API 测试
%%====================================================================

new_test() ->
    Registry = beamai_tool_registry:new(),
    ?assertEqual([], beamai_tool_registry:get_sources(Registry)).

add_tools_test() ->
    Registry = beamai_tool_registry:new(),
    Tools = [make_tool(<<"tool1">>), make_tool(<<"tool2">>)],
    R1 = beamai_tool_registry:add_tools(Registry, Tools),
    Sources = beamai_tool_registry:get_sources(R1),
    ?assertEqual([{tools, 0}], Sources).

build_simple_test() ->
    Registry = beamai_tool_registry:new(),
    Tools = [make_tool(<<"tool1">>), make_tool(<<"tool2">>)],
    R1 = beamai_tool_registry:add_tools(Registry, Tools),
    Result = beamai_tool_registry:build(R1),
    ?assertEqual(2, length(Result)),
    Names = [maps:get(name, T) || T <- Result],
    ?assertEqual([<<"tool1">>, <<"tool2">>], Names).

%%====================================================================
%% 冲突解决测试
%%====================================================================

conflict_first_wins_test() ->
    %% 先添加的优先
    Registry = beamai_tool_registry:new(),
    Tool1 = make_tool(<<"same_name">>, <<"First">>),
    Tool2 = make_tool(<<"same_name">>, <<"Second">>),
    R1 = beamai_tool_registry:add_tools(Registry, [Tool1]),
    R2 = beamai_tool_registry:add_tools(R1, [Tool2]),
    Result = beamai_tool_registry:build(R2, fun beamai_tool_registry:strategy_first_wins/2),
    ?assertEqual(1, length(Result)),
    [Winner] = Result,
    ?assertEqual(<<"First">>, maps:get(description, Winner)).

conflict_last_wins_test() ->
    %% 后添加的优先
    Registry = beamai_tool_registry:new(),
    Tool1 = make_tool(<<"same_name">>, <<"First">>),
    Tool2 = make_tool(<<"same_name">>, <<"Second">>),
    R1 = beamai_tool_registry:add_tools(Registry, [Tool1]),
    R2 = beamai_tool_registry:add_tools(R1, [Tool2]),
    Result = beamai_tool_registry:build(R2, fun beamai_tool_registry:strategy_last_wins/2),
    ?assertEqual(1, length(Result)),
    [Winner] = Result,
    ?assertEqual(<<"Second">>, maps:get(description, Winner)).

conflict_error_test() ->
    %% 冲突时报错
    Registry = beamai_tool_registry:new(),
    Tool1 = make_tool(<<"same_name">>, <<"First">>),
    Tool2 = make_tool(<<"same_name">>, <<"Second">>),
    R1 = beamai_tool_registry:add_tools(Registry, [Tool1]),
    R2 = beamai_tool_registry:add_tools(R1, [Tool2]),
    ?assertThrow({error, {conflict, <<"same_name">>}},
                 beamai_tool_registry:build(R2, fun beamai_tool_registry:strategy_error/2)).

%%====================================================================
%% from_config 测试
%%====================================================================

from_config_tools_only_test() ->
    Tools = [make_tool(<<"tool1">>), make_tool(<<"tool2">>)],
    Result = beamai_tool_registry:from_config(#{tools => Tools}),
    ?assertEqual(2, length(Result)).

from_config_empty_test() ->
    Result = beamai_tool_registry:from_config(#{}),
    ?assertEqual([], Result).

from_config_with_strategy_test() ->
    Tool1 = make_tool(<<"same">>, <<"First">>),
    Tool2 = make_tool(<<"same">>, <<"Second">>),
    Result = beamai_tool_registry:from_config(#{
        tools => [Tool1, Tool2],
        strategy => fun beamai_tool_registry:strategy_last_wins/2
    }),
    ?assertEqual(1, length(Result)),
    [Winner] = Result,
    ?assertEqual(<<"Second">>, maps:get(description, Winner)).

%%====================================================================
%% 多来源合并测试
%%====================================================================

multiple_sources_test() ->
    Registry = beamai_tool_registry:new(),
    Tools1 = [make_tool(<<"tool1">>)],
    Tools2 = [make_tool(<<"tool2">>)],
    Tools3 = [make_tool(<<"tool3">>)],
    R1 = beamai_tool_registry:add_tools(Registry, Tools1),
    R2 = beamai_tool_registry:add_tools(R1, Tools2),
    R3 = beamai_tool_registry:add_tools(R2, Tools3),
    Result = beamai_tool_registry:build(R3),
    ?assertEqual(3, length(Result)),
    Names = [maps:get(name, T) || T <- Result],
    ?assertEqual([<<"tool1">>, <<"tool2">>, <<"tool3">>], Names).

mixed_conflict_test() ->
    %% 多来源中有冲突
    Registry = beamai_tool_registry:new(),
    Tools1 = [make_tool(<<"a">>, <<"First A">>), make_tool(<<"b">>)],
    Tools2 = [make_tool(<<"a">>, <<"Second A">>), make_tool(<<"c">>)],
    R1 = beamai_tool_registry:add_tools(Registry, Tools1),
    R2 = beamai_tool_registry:add_tools(R1, Tools2),
    Result = beamai_tool_registry:build(R2),
    ?assertEqual(3, length(Result)),
    %% a 应该是 First A（先添加的优先）
    [ToolA | _] = [T || T <- Result, maps:get(name, T) =:= <<"a">>],
    ?assertEqual(<<"First A">>, maps:get(description, ToolA)).

%%====================================================================
%% from_providers 测试
%%====================================================================

from_providers_empty_test() ->
    Result = beamai_tool_registry:from_providers([]),
    ?assertEqual([], Result).
