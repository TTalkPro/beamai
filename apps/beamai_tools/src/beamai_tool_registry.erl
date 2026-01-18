%%%-------------------------------------------------------------------
%%% @doc 工具注册表模块
%%%
%%% 提供统一的工具收集和管理机制，支持：
%%% - 直接添加工具
%%% - 从 Provider 获取工具
%%% - 可插拔的冲突解决策略
%%%
%%% == 设计理念 ==
%%%
%%% 1. 关注点分离：将工具收集从 Agent 中独立出来
%%% 2. 策略可插拔：冲突解决策略可自定义
%%% 3. 构建器模式：支持链式调用
%%% 4. 延迟构建：add_* 只记录，build 时才真正收集
%%%
%%% == 使用示例 ==
%%%
%%% === 基本用法 ===
%%% ```erlang
%%% Registry = beamai_tool_registry:new(),
%%% R1 = beamai_tool_registry:add_tools(Registry, [MyTool1, MyTool2]),
%%% R2 = beamai_tool_registry:add_provider(R1, beamai_tool_provider_mcp, #{mcp_tag => file}),
%%% R3 = beamai_tool_registry:add_provider(R2, beamai_tool_provider_builtin),
%%% Tools = beamai_tool_registry:build(R3).
%%% ```
%%%
%%% === 使用便捷函数 ===
%%% ```erlang
%%% Tools = beamai_tool_registry:from_config(#{
%%%     tools => [MyTool],
%%%     providers => [
%%%         {beamai_tool_provider_mcp, #{mcp_tag => file}},
%%%         beamai_tool_provider_builtin
%%%     ]
%%% }).
%%% ```
%%%
%%% === 自定义冲突策略 ===
%%% ```erlang
%%% Tools = beamai_tool_registry:build(Registry, fun strategy_last_wins/2).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_registry).

-include("beamai_tools.hrl").

%% === 构建 API ===
-export([
    new/0,
    add_tools/2,
    add_provider/2,
    add_provider/3,
    build/1,
    build/2
]).

%% === 便捷函数 ===
-export([
    from_config/1,
    from_providers/1,
    from_providers/2
]).

%% === 冲突解决策略 ===
-export([
    strategy_first_wins/2,
    strategy_last_wins/2,
    strategy_error/2
]).

%% === 查询 API ===
-export([
    get_sources/1,
    get_tool_count/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type source() :: {tools, non_neg_integer()} | {provider, module(), map()}.

-type registry() :: #{
    entries := [entry()],
    seq := non_neg_integer()
}.

-type entry() :: #{
    type := tools | provider,
    seq := non_neg_integer(),
    data := [tool_def()] | module(),
    context := map()
}.

-type conflict_strategy() :: fun((tool_def(), tool_def()) -> tool_def() | {error, conflict}).

-export_type([registry/0, conflict_strategy/0]).

%%====================================================================
%% 构建 API
%%====================================================================

%% @doc 创建新的空注册表
-spec new() -> registry().
new() ->
    #{entries => [], seq => 0}.

%% @doc 添加工具列表
%%
%% 工具将按添加顺序记录，在 build 时参与合并。
-spec add_tools(registry(), [tool_def()]) -> registry().
add_tools(#{entries := Entries, seq := Seq} = Registry, Tools) when is_list(Tools) ->
    Entry = #{
        type => tools,
        seq => Seq,
        data => Tools,
        context => #{}
    },
    Registry#{entries => Entries ++ [Entry], seq => Seq + 1}.

%% @doc 添加 Provider（无 context）
-spec add_provider(registry(), module()) -> registry().
add_provider(Registry, Provider) ->
    add_provider(Registry, Provider, #{}).

%% @doc 添加 Provider（带 context）
%%
%% Context 用于传递 Provider 特定的配置，如 MCP 的 tag。
-spec add_provider(registry(), module(), map()) -> registry().
add_provider(#{entries := Entries, seq := Seq} = Registry, Provider, Context) ->
    Entry = #{
        type => provider,
        seq => Seq,
        data => Provider,
        context => Context
    },
    Registry#{entries => Entries ++ [Entry], seq => Seq + 1}.

%% @doc 构建工具列表（使用默认策略：先添加的优先）
-spec build(registry()) -> [tool_def()].
build(Registry) ->
    build(Registry, fun strategy_first_wins/2).

%% @doc 构建工具列表（使用指定策略）
%%
%% 策略函数签名：fun((Existing, New) -> Winner | {error, conflict})
%% - Existing: 已存在的工具定义
%% - New: 新遇到的同名工具定义
%% - Winner: 选择保留的工具定义
-spec build(registry(), conflict_strategy()) -> [tool_def()].
build(#{entries := Entries}, Strategy) ->
    %% 按顺序收集所有工具
    AllTools = collect_all_tools(Entries),
    %% 应用冲突解决策略进行去重
    resolve_conflicts(AllTools, Strategy).

%%====================================================================
%% 便捷函数
%%====================================================================

%% @doc 从配置 map 构建工具列表
%%
%% Config 格式：
%% ```
%% #{
%%%     tools => [Tool1, Tool2],           %% 可选：直接工具列表
%%%     providers => [                      %% 可选：Provider 列表
%%%         {ProviderModule, Context},      %% 带 context 的 provider
%%%         ProviderModule                  %% 无 context 的 provider
%%%     ],
%%%     strategy => fun/2                   %% 可选：冲突解决策略
%%% }
%%% ```
-spec from_config(map()) -> [tool_def()].
from_config(Config) ->
    Registry = new(),

    %% 添加直接工具
    R1 = case maps:get(tools, Config, []) of
        [] -> Registry;
        Tools -> add_tools(Registry, Tools)
    end,

    %% 添加 providers
    R2 = lists:foldl(fun
        ({Provider, Context}, Acc) when is_atom(Provider), is_map(Context) ->
            add_provider(Acc, Provider, Context);
        (Provider, Acc) when is_atom(Provider) ->
            add_provider(Acc, Provider)
    end, R1, maps:get(providers, Config, [])),

    %% 构建
    Strategy = maps:get(strategy, Config, fun strategy_first_wins/2),
    build(R2, Strategy).

%% @doc 仅从 Provider 列表构建工具
-spec from_providers([module() | {module(), map()}]) -> [tool_def()].
from_providers(Providers) ->
    from_providers(Providers, #{}).

%% @doc 从 Provider 列表构建工具（带选项）
-spec from_providers([module() | {module(), map()}], map()) -> [tool_def()].
from_providers(Providers, Opts) ->
    Strategy = maps:get(strategy, Opts, fun strategy_first_wins/2),
    Registry = lists:foldl(fun
        ({Provider, Context}, Acc) -> add_provider(Acc, Provider, Context);
        (Provider, Acc) -> add_provider(Acc, Provider)
    end, new(), Providers),
    build(Registry, Strategy).

%%====================================================================
%% 冲突解决策略
%%====================================================================

%% @doc 先添加的优先（保留已存在的）
-spec strategy_first_wins(tool_def(), tool_def()) -> tool_def().
strategy_first_wins(Existing, _New) ->
    Existing.

%% @doc 后添加的优先（覆盖已存在的）
-spec strategy_last_wins(tool_def(), tool_def()) -> tool_def().
strategy_last_wins(_Existing, New) ->
    New.

%% @doc 冲突时报错
-spec strategy_error(tool_def(), tool_def()) -> {error, conflict}.
strategy_error(#{name := Name}, _New) ->
    {error, {conflict, Name}}.

%%====================================================================
%% 查询 API
%%====================================================================

%% @doc 获取所有来源信息
-spec get_sources(registry()) -> [source()].
get_sources(#{entries := Entries}) ->
    lists:map(fun
        (#{type := tools, seq := Seq}) ->
            {tools, Seq};
        (#{type := provider, data := Provider, context := Context}) ->
            {provider, Provider, Context}
    end, Entries).

%% @doc 获取预期工具数量（实际数量可能因去重而减少）
-spec get_tool_count(registry()) -> non_neg_integer().
get_tool_count(Registry) ->
    length(build(Registry)).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 收集所有工具（按添加顺序）
-spec collect_all_tools([entry()]) -> [tool_def()].
collect_all_tools(Entries) ->
    lists:flatmap(fun collect_entry_tools/1, Entries).

%% @private 从单个 entry 收集工具
-spec collect_entry_tools(entry()) -> [tool_def()].
collect_entry_tools(#{type := tools, data := Tools}) ->
    Tools;
collect_entry_tools(#{type := provider, data := Provider, context := Context}) ->
    fetch_provider_tools(Provider, Context).

%% @private 从 Provider 获取工具
-spec fetch_provider_tools(module(), map()) -> [tool_def()].
fetch_provider_tools(Provider, Context) ->
    case beamai_tool_provider:is_available(Provider) of
        true ->
            ProviderOpts = #{categories => all, context => Context},
            case catch Provider:list_tools(ProviderOpts) of
                {ok, Tools} -> Tools;
                {error, _} -> [];
                {'EXIT', _} -> []
            end;
        false ->
            []
    end.

%% @private 应用冲突解决策略
-spec resolve_conflicts([tool_def()], conflict_strategy()) -> [tool_def()].
resolve_conflicts(Tools, Strategy) ->
    {Result, _} = lists:foldl(fun(Tool, {Acc, NameSet}) ->
        Name = get_tool_name(Tool),
        case sets:is_element(Name, NameSet) of
            false ->
                %% 新工具，直接添加
                {Acc ++ [Tool], sets:add_element(Name, NameSet)};
            true ->
                %% 冲突，应用策略
                case apply_strategy(Tool, Acc, Name, Strategy) of
                    {ok, NewAcc} -> {NewAcc, NameSet};
                    {error, _} = Err -> throw(Err)
                end
        end
    end, {[], sets:new()}, Tools),
    Result.

%% @private 应用冲突策略
-spec apply_strategy(tool_def(), [tool_def()], binary(), conflict_strategy()) ->
    {ok, [tool_def()]} | {error, term()}.
apply_strategy(NewTool, Acc, Name, Strategy) ->
    case find_tool_by_name(Name, Acc) of
        {ok, Existing, Index} ->
            case Strategy(Existing, NewTool) of
                {error, _} = Err ->
                    Err;
                Winner ->
                    %% 替换原位置的工具
                    NewAcc = replace_at(Acc, Index, Winner),
                    {ok, NewAcc}
            end;
        not_found ->
            %% 不应该发生，因为 NameSet 已经检测到冲突
            {ok, Acc ++ [NewTool]}
    end.

%% @private 获取工具名称
-spec get_tool_name(tool_def()) -> binary().
get_tool_name(#{name := Name}) -> Name.

%% @private 按名称查找工具
-spec find_tool_by_name(binary(), [tool_def()]) -> {ok, tool_def(), non_neg_integer()} | not_found.
find_tool_by_name(Name, Tools) ->
    find_tool_by_name(Name, Tools, 1).

find_tool_by_name(_Name, [], _Index) ->
    not_found;
find_tool_by_name(Name, [#{name := Name} = Tool | _], Index) ->
    {ok, Tool, Index};
find_tool_by_name(Name, [_ | Rest], Index) ->
    find_tool_by_name(Name, Rest, Index + 1).

%% @private 替换列表中指定位置的元素
-spec replace_at([T], non_neg_integer(), T) -> [T] when T :: term().
replace_at(List, Index, Element) ->
    {Before, [_ | After]} = lists:split(Index - 1, List),
    Before ++ [Element] ++ After.
