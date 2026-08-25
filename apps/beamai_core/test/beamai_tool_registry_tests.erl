%%%-------------------------------------------------------------------
%%% @doc 工具能力从 ChatClient 收缩出去后的边界测试
%%%
%%% 对照 Spring AI（docs/api/tools.html）的分工：ChatClient 只声明与调用，
%%% 解析/定义/元数据归 registry（ToolCallbackResolver/ToolDefinition/ToolMetadata），
%%% 执行归 executor（ToolCallingManager 的单次原语）。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_registry_tests).

-include_lib("eunit/include/eunit.hrl").

tool(Name) ->
    #{name => Name, description => <<"t">>, parameters => #{},
      handler => fun(#{<<"x">> := X}, _Ctx) -> {ok, X * 2} end}.

registry() ->
    beamai_tool_registry:add_many(beamai_tool_registry:new(),
                                  [tool(<<"a">>), tool(<<"b">>)]).

%%====================================================================
%% ChatClient 的能力边界（收缩后的导出面本身就是契约）
%%====================================================================

chat_client_no_longer_owns_tool_capabilities_test() ->
    {module, _} = code:ensure_loaded(beamai_chat_client),
    Gone = [{invoke_tool, 4}, {get_tool, 2}, {list_tools, 1}, {get_tools_by_tag, 2},
            {get_tool_specs, 1}, {get_tool_schemas, 1}, {get_tool_schemas, 2},
            {serial_tool, 2}, {return_direct_tool, 2}],
    [?assertNot(erlang:function_exported(beamai_chat_client, F, A)) || {F, A} <- Gone],
    Kept = [{new, 2}, {add_tool, 2}, {add_tools, 2}, {add_tool_module, 2},
            {add_chat_model, 2}, {invoke_chat, 3}, {invoke_chat_stream, 4},
            {tools, 1}, {filters, 1}, {chat_model, 1}, {state_slots, 1}],
    [?assert(erlang:function_exported(beamai_chat_client, F, A)) || {F, A} <- Kept].

%%====================================================================
%% registry：脱离 ChatClient 独立可用
%%====================================================================

resolve_test() ->
    R = registry(),
    ?assertMatch({ok, #{name := <<"a">>}}, beamai_tool_registry:resolve(R, <<"a">>)),
    ?assertEqual(error, beamai_tool_registry:resolve(R, <<"ghost">>)).

list_and_by_tag_test() ->
    R0 = beamai_tool_registry:add(beamai_tool_registry:new(),
                                  (tool(<<"tagged">>))#{tag => [<<"math">>]}),
    R = beamai_tool_registry:add(R0, tool(<<"plain">>)),
    ?assertEqual(2, length(beamai_tool_registry:list(R))),
    ?assertMatch([#{name := <<"tagged">>}], beamai_tool_registry:by_tag(R, <<"math">>)),
    ?assertEqual([], beamai_tool_registry:by_tag(R, <<"none">>)).

specs_and_schemas_test() ->
    R = registry(),
    ?assertEqual(2, length(beamai_tool_registry:specs(R))),
    [S | _] = beamai_tool_registry:schemas(R),
    ?assertMatch(#{<<"type">> := <<"function">>}, S),
    [A | _] = beamai_tool_registry:schemas(R, anthropic),
    ?assert(maps:is_key(<<"input_schema">>, A)).

%% 元数据：未注册的名字一律取保守值（不因未知工具退化整批 / 误触发直返）
metadata_unknown_tool_is_conservative_test() ->
    R = registry(),
    ?assertNot(beamai_tool_registry:serial(R, <<"ghost">>)),
    ?assertNot(beamai_tool_registry:return_direct(R, <<"ghost">>)),
    R2 = beamai_tool_registry:add(R, (tool(<<"s">>))#{serial => true}),
    ?assert(beamai_tool_registry:serial(R2, <<"s">>)),
    R3 = beamai_tool_registry:add(R, (tool(<<"d">>))#{return_direct => true}),
    ?assert(beamai_tool_registry:return_direct(R3, <<"d">>)).

from_module_test() ->
    R = beamai_tool_registry:from_module(beamai_tool_registry:new(),
                                         beamai_registry_test_plugin),
    ?assertMatch({ok, _}, beamai_tool_registry:resolve(R, <<"stub_tool">>)).

%%====================================================================
%% executor：执行 + around_tool 洋葱
%%====================================================================

executor_runs_tool_test() ->
    CC = beamai_chat_client:add_tool(beamai_chat_client:new(), tool(<<"a">>)),
    ?assertEqual({ok, 42, #{}},
                 beamai_tool_executor:invoke(CC, <<"a">>, #{<<"x">> => 21},
                                             beamai_context:new())).

executor_unknown_tool_test() ->
    ?assertEqual({error, {tool_not_found, <<"ghost">>}},
                 beamai_tool_executor:invoke(beamai_chat_client:new(), <<"ghost">>, #{},
                                             beamai_context:new())).

%% around_tool 洋葱仍然生效（filter 挂在 ChatClient 上，executor 从它那儿取）
executor_runs_around_tool_chain_test() ->
    Double = beamai_filter:new(<<"double">>, #{
        around_tool => fun(Req, _F, Next) ->
            #{result := V} = Resp = Next(Req),
            Resp#{result => V * 10}
        end
    }),
    CC = beamai_chat_client:add_tool(beamai_chat_client:new(#{}, [Double]), tool(<<"a">>)),
    ?assertEqual({ok, 420, #{}},
                 beamai_tool_executor:invoke(CC, <<"a">>, #{<<"x">> => 21},
                                             beamai_context:new())).

%% 工具写意图（writes）原样透出给调用方折叠
executor_passes_writes_through_test() ->
    W = #{counter => 1},
    T = #{name => <<"w">>, description => <<"w">>, parameters => #{},
          handler => fun(_A, _C) -> {ok, ok, W} end},
    CC = beamai_chat_client:add_tool(beamai_chat_client:new(), T),
    ?assertEqual({ok, ok, W},
                 beamai_tool_executor:invoke(CC, <<"w">>, #{}, beamai_context:new())).
