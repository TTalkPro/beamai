%%%-------------------------------------------------------------------
%%% @doc 测试桩：给 beamai_tool_registry:from_module/2 用的最小工具模块
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_registry_test_plugin).

-export([tools/0]).

tools() ->
    [#{name => <<"stub_tool">>, description => <<"stub">>, parameters => #{},
       handler => fun(_Args, _Ctx) -> {ok, <<"ok">>} end}].
