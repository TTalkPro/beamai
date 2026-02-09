%%%-------------------------------------------------------------------
%%% @doc 工具行为定义（behaviour）
%%%
%%% 定义工具模块的接口契约。工具模块必须实现 `tools/0` 回调函数，
%%% 可以选择性地实现 `tool_info/0` 来提供模块元信息，
%%% 以及 `filters/0` 来注册 kernel 过滤器。
%%%
%%% == 示例 ==
%%%
%%% ```erlang
%%% -module(my_tools).
%%% -behaviour(beamai_tool_behaviour).
%%% -export([tools/0]).
%%%
%%% tools() ->
%%%     [#{name => <<"hello">>,
%%%        handler => fun(_) -> {ok, <<"world">>} end,
%%%        description => <<"Say hello">>,
%%%        tag => <<"greeting">>}].
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_behaviour).

%% @doc （可选）返回工具模块元信息。
%%
%% 提供模块级别的描述和默认标签，用于工具模块的组织和发现。
%% 此回调为可选实现。
%%
%% @returns 包含模块元信息的映射
-callback tool_info() -> #{
    description => binary(),
    tags => [binary()],
    metadata => map()
}.

%% @doc 返回工具定义列表。
%%
%% 返回该模块提供的所有工具定义列表。
%% 每个工具定义至少包含 name 和 handler 字段。
%%
%% @returns 工具定义列表
-callback tools() -> [beamai_tool:tool_spec()].

%% @doc （可选）返回 kernel filter 列表。
%%
%% 如果工具模块需要在 kernel 中注册过滤器（如请求前处理、响应后处理等），
%% 可以实现此回调来返回过滤器定义列表。
%% 此回调为可选实现，不实现时模块不会注册任何过滤器。
%%
%% @returns 过滤器定义列表
-callback filters() -> [beamai_filter:filter_spec()].

-optional_callbacks([tool_info/0, filters/0]).
