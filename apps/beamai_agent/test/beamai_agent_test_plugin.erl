%%%-------------------------------------------------------------------
%%% @doc 测试支持模块：同时定义 tools/0 与 filters/0 的插件
%%%
%%% 用于验证 agent 的 plugins 加载整体委托 kernel 原语
%%% （beamai_kernel:add_tool_module/2），filter 只注册一次、
%%% agent 层不重复加载。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_test_plugin).

-export([tools/0, filters/0]).

tools() ->
    [#{name => <<"plugin_tool">>,
       description => <<"test tool">>,
       parameters => #{},
       handler => fun(_Args, _Ctx) -> {ok, <<"ok">>} end}].

filters() ->
    [beamai_filter:new(<<"plugin_filter">>, #{})].
