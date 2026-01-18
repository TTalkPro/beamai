%%%-------------------------------------------------------------------
%%% @doc 内置工具 Provider
%%%
%%% 提供 beamai_tools 内置的所有静态工具。
%%% 这是默认的 provider，始终可用。
%%%
%%% == 支持的分类 ==
%%%
%%% - file: 文件操作工具
%%% - shell: Shell 命令工具
%%% - todo: 任务列表工具
%%% - human: 人工交互工具
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_provider_builtin).

-behaviour(beamai_tool_provider).

-include("beamai_tools.hrl").

%% Behaviour 回调
-export([list_tools/1, find_tool/2, info/0, available/0]).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

%% @doc Provider 元信息
-spec info() -> map().
info() ->
    #{
        name => <<"builtin">>,
        version => <<"1.0.0">>,
        description => <<"内置工具集，提供文件、Shell、任务和人工交互工具"/utf8>>,
        categories => [file, shell, todo, human]
    }.

%% @doc 始终可用
-spec available() -> boolean().
available() -> true.

%% @doc 列出工具
%%
%% 根据 categories 选项过滤返回的工具。
%% 支持：
%% - all: 返回所有工具
%% - atom(): 返回单个分类
%% - [atom()]: 返回多个分类
-spec list_tools(map()) -> {ok, [tool_def()]}.
list_tools(Opts) ->
    Categories = maps:get(categories, Opts, all),
    Tools = get_tools_by_categories(Categories),
    {ok, Tools}.

%% @doc 按名称查找工具
-spec find_tool(binary(), map()) -> {ok, tool_def()} | {error, not_found}.
find_tool(Name, Opts) ->
    {ok, Tools} = list_tools(Opts),
    find_tool_by_name(Name, Tools).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 按名称查找工具
-spec find_tool_by_name(binary(), [tool_def()]) -> {ok, tool_def()} | {error, not_found}.
find_tool_by_name(_Name, []) ->
    {error, not_found};
find_tool_by_name(Name, [#{name := Name} = Tool | _]) ->
    {ok, Tool};
find_tool_by_name(Name, [_ | Rest]) ->
    find_tool_by_name(Name, Rest).

%% @private 根据分类获取工具
-spec get_tools_by_categories(all | tool_category() | [tool_category()]) -> [tool_def()].
get_tools_by_categories(all) ->
    get_tools_by_categories([file, shell, todo, human]);
get_tools_by_categories(Categories) when is_list(Categories) ->
    lists:flatmap(fun get_category_tools/1, Categories);
get_tools_by_categories(Category) when is_atom(Category) ->
    get_category_tools(Category).

%% @private 获取单个分类的工具
-spec get_category_tools(tool_category()) -> [tool_def()].
get_category_tools(file) -> beamai_tools_file:all();
get_category_tools(shell) -> beamai_tools_shell:all();
get_category_tools(todo) -> beamai_tools_todo:all();
get_category_tools(human) -> beamai_tools_human:all();
get_category_tools(_) -> [].
