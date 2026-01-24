%%%-------------------------------------------------------------------
%%% @doc 插件行为定义（behaviour）
%%%
%%% 定义插件模块的接口契约。插件模块必须实现 `plugin_info/0` 和
%%% `functions/0` 回调函数，可以选择性地实现 `filters/0` 来注册
%%% kernel 过滤器。
%%%
%%% == 示例 ==
%%%
%%% ```erlang
%%% -module(my_plugin).
%%% -behaviour(beamai_plugin_behaviour).
%%% -export([plugin_info/0, functions/0]).
%%%
%%% plugin_info() ->
%%%     #{name => <<"my_plugin">>, description => <<"自定义插件">>}.
%%%
%%% functions() ->
%%%     [beamai_function:new(<<"hello">>, fun(_) -> {ok, <<"world">>} end,
%%%         #{description => <<"打个招呼">>})].
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_plugin_behaviour).

%% @doc 返回插件元数据。
%%
%% 必须包含 `name`（二进制字符串类型的插件名称）。
%% 可选字段：`description`（插件描述）、`metadata`（附加元数据映射）。
%%
%% @returns 包含插件元信息的映射，name 为必填字段
-callback plugin_info() -> #{name := binary(), description => binary(), metadata => map()}.

%% @doc 返回函数定义列表。
%%
%% 返回该插件提供的所有函数（工具）定义列表。
%% 每个函数定义包含名称、处理器和可选的描述信息。
%%
%% @returns 函数定义列表
-callback functions() -> [beamai_function:function_def()].

%% @doc （可选）返回 kernel filter 列表。
%%
%% 如果插件需要在 kernel 中注册过滤器（如请求前处理、响应后处理等），
%% 可以实现此回调来返回过滤器定义列表。
%% 此回调为可选实现，不实现时插件不会注册任何过滤器。
%%
%% @returns 过滤器定义列表
-callback filters() -> [beamai_filter:filter_def()].

-optional_callbacks([filters/0]).
