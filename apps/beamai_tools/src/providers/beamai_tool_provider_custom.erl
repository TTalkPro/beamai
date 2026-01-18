%%%-------------------------------------------------------------------
%%% @doc 自定义工具 Provider
%%%
%%% 支持运行时动态注册和注销工具。
%%% 使用 persistent_term 存储，无需单独进程，VM 重启后清空。
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 注册自定义工具
%%% MyTool = #{
%%%     name => <<"my_calculator">>,
%%%     description => <<"自定义计算器"/utf8>>,
%%%     category => custom,
%%%     parameters => #{
%%%         type => object,
%%%         properties => #{
%%%             <<"a">> => #{type => number},
%%%             <<"b">> => #{type => number}
%%%         }
%%%     },
%%%     handler => fun(#{<<"a">> := A, <<"b">> := B}, _) ->
%%%         {ok, A + B}
%%%     end
%%% },
%%% beamai_tool_provider_custom:register_tool(MyTool),
%%%
%%% %% 注销工具
%%% beamai_tool_provider_custom:unregister_tool(<<"my_calculator">>),
%%%
%%% %% 清空所有自定义工具
%%% beamai_tool_provider_custom:clear_all().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_provider_custom).

-behaviour(beamai_tool_provider).

-include("beamai_tools.hrl").

%% Behaviour 回调
-export([list_tools/1, find_tool/2, info/0, available/0]).

%% 管理 API
-export([
    register_tool/1,
    register_tools/1,
    unregister_tool/1,
    clear_all/0,
    count/0
]).

%% persistent_term 存储键
-define(CUSTOM_TOOLS_KEY, {?MODULE, custom_tools}).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

%% @doc Provider 元信息
-spec info() -> map().
info() ->
    #{
        name => <<"custom">>,
        version => <<"1.0.0">>,
        description => <<"用户自定义工具，支持运行时动态注册"/utf8>>,
        categories => [custom]
    }.

%% @doc 始终可用
-spec available() -> boolean().
available() -> true.

%% @doc 列出所有自定义工具
-spec list_tools(map()) -> {ok, [tool_def()]}.
list_tools(Opts) ->
    AllTools = maps:values(get_tools_map()),

    %% 可选：按分类过滤
    Tools = case maps:get(categories, Opts, all) of
        all ->
            AllTools;
        Categories when is_list(Categories) ->
            [T || T = #{category := Cat} <- AllTools, lists:member(Cat, Categories)];
        Category when is_atom(Category) ->
            [T || T = #{category := Cat} <- AllTools, Cat =:= Category]
    end,
    {ok, Tools}.

%% @doc 按名称查找工具
-spec find_tool(binary(), map()) -> {ok, tool_def()} | {error, not_found}.
find_tool(Name, _Opts) ->
    ToolsMap = get_tools_map(),
    case maps:find(Name, ToolsMap) of
        {ok, Tool} -> {ok, Tool};
        error -> {error, not_found}
    end.

%%====================================================================
%% 管理 API
%%====================================================================

%% @doc 注册单个工具
%%
%% 如果工具名已存在，将被覆盖。
-spec register_tool(tool_def()) -> ok.
register_tool(#{name := Name} = Tool) ->
    ToolsMap = get_tools_map(),
    NewMap = ToolsMap#{Name => Tool},
    put_tools_map(NewMap),
    ok.

%% @doc 批量注册工具
-spec register_tools([tool_def()]) -> ok.
register_tools(Tools) ->
    ToolsMap = get_tools_map(),
    NewMap = lists:foldl(
        fun(#{name := Name} = Tool, Acc) ->
            Acc#{Name => Tool}
        end,
        ToolsMap,
        Tools
    ),
    put_tools_map(NewMap),
    ok.

%% @doc 注销工具
-spec unregister_tool(binary()) -> ok | {error, not_found}.
unregister_tool(Name) ->
    ToolsMap = get_tools_map(),
    case maps:is_key(Name, ToolsMap) of
        true ->
            NewMap = maps:remove(Name, ToolsMap),
            put_tools_map(NewMap),
            ok;
        false ->
            {error, not_found}
    end.

%% @doc 清空所有自定义工具
-spec clear_all() -> ok.
clear_all() ->
    put_tools_map(#{}),
    ok.

%% @doc 获取自定义工具数量
-spec count() -> non_neg_integer().
count() ->
    maps:size(get_tools_map()).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 获取工具 map
-spec get_tools_map() -> #{binary() => tool_def()}.
get_tools_map() ->
    persistent_term:get(?CUSTOM_TOOLS_KEY, #{}).

%% @private 存储工具 map
-spec put_tools_map(#{binary() => tool_def()}) -> ok.
put_tools_map(Map) ->
    persistent_term:put(?CUSTOM_TOOLS_KEY, Map),
    ok.
