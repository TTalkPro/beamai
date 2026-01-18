%%%-------------------------------------------------------------------
%%% @doc Tool Provider 行为定义
%%%
%%% 任何模块实现此行为即可作为工具提供者，支持动态工具发现。
%%%
%%% == 使用场景 ==
%%%
%%% - 内置工具（静态）
%%% - MCP 服务器（远程动态）
%%% - 用户自定义工具（运行时注册）
%%% - 插件系统
%%%
%%% == 实现示例 ==
%%%
%%% ```erlang
%%% -module(my_tool_provider).
%%% -behaviour(beamai_tool_provider).
%%%
%%% -export([list_tools/1, info/0]).
%%%
%%% info() ->
%%%     #{name => <<"my_provider">>, version => <<"1.0.0">>}.
%%%
%%% list_tools(_Opts) ->
%%%     {ok, [#{name => <<"my_tool">>, description => ..., ...}]}.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_provider).

-include("beamai_tools.hrl").

%%====================================================================
%% 回调定义
%%====================================================================

%% @doc 返回此 provider 提供的工具列表
%%
%% Opts 参数：
%% - categories: 请求的工具分类（atom | [atom()] | all）
%% - context: 调用上下文（如用户信息、会话信息）
%%
%% 返回：
%% - {ok, [tool_def()]} 工具列表
%% - {error, term()} 获取失败
-callback list_tools(Opts :: map()) ->
    {ok, [tool_def()]} | {error, term()}.

%% @doc 按名称查找工具
%%
%% 可选回调。默认实现会遍历 list_tools 结果，
%% 但 provider 可以覆盖以提供更高效的查找（如远程 API 支持按名称查询）。
-callback find_tool(Name :: binary(), Opts :: map()) ->
    {ok, tool_def()} | {error, not_found} | {error, term()}.

%% @doc 返回 provider 元信息
%%
%% 可选回调。返回 provider 的名称、版本、描述等信息。
-callback info() -> #{
    name := binary(),
    version := binary(),
    description => binary(),
    categories => [tool_category()]
}.

%% @doc 检查 provider 是否可用
%%
%% 可选回调。用于动态 provider（如 MCP）检查连接状态。
%% 默认返回 true。
-callback available() -> boolean().

%% 可选回调声明
-optional_callbacks([find_tool/2, info/0, available/0]).

%%====================================================================
%% 辅助函数
%%====================================================================

-export([
    is_available/1,
    get_info/1,
    find_tool_in_provider/3
]).

%% @doc 检查 provider 是否可用
%%
%% 如果 provider 没有实现 available/0，默认返回 true。
-spec is_available(module()) -> boolean().
is_available(Provider) ->
    case erlang:function_exported(Provider, available, 0) of
        true ->
            try
                Provider:available()
            catch
                _:_ -> false
            end;
        false ->
            true
    end.

%% @doc 获取 provider 信息
%%
%% 如果 provider 没有实现 info/0，返回默认信息。
-spec get_info(module()) -> map().
get_info(Provider) ->
    case erlang:function_exported(Provider, info, 0) of
        true ->
            try
                Provider:info()
            catch
                _:_ -> default_info(Provider)
            end;
        false ->
            default_info(Provider)
    end.

%% @doc 在 provider 中查找工具
%%
%% 如果 provider 实现了 find_tool/2 则调用它，
%% 否则遍历 list_tools 结果。
-spec find_tool_in_provider(module(), binary(), map()) ->
    {ok, tool_def()} | {error, not_found} | {error, term()}.
find_tool_in_provider(Provider, Name, Opts) ->
    case erlang:function_exported(Provider, find_tool, 2) of
        true ->
            Provider:find_tool(Name, Opts);
        false ->
            %% 降级：遍历 list_tools
            case Provider:list_tools(Opts) of
                {ok, Tools} ->
                    find_tool_by_name(Name, Tools);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @private 按名称在工具列表中查找
-spec find_tool_by_name(binary(), [tool_def()]) -> {ok, tool_def()} | {error, not_found}.
find_tool_by_name(_Name, []) ->
    {error, not_found};
find_tool_by_name(Name, [#{name := Name} = Tool | _]) ->
    {ok, Tool};
find_tool_by_name(Name, [_ | Rest]) ->
    find_tool_by_name(Name, Rest).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 默认 provider 信息
default_info(Provider) ->
    #{
        name => atom_to_binary(Provider),
        version => <<"unknown">>,
        description => <<"No description">>
    }.
