%%%-------------------------------------------------------------------
%%% @doc 测试支持模块：同时定义 tools/0 与 filters/0 的插件
%%%
%%% 用于验证 plugins 只提供工具：filters/0 特性已删除，即使模块导出
%%% filters/0 也被忽略（filter 一律在构建 kernel 时经 filters 列表给出）。
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
