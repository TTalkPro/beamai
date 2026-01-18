%%%-------------------------------------------------------------------
%%% @doc Agent Tools - 公共工具库
%%%
%%% 提供统一的工具定义和管理接口，供 beamai_agent、beamai_deepagent 等模块使用。
%%% 支持 Provider 机制实现动态工具发现。
%%%
%%% == 功能概述 ==
%%%
%%% - 按分类获取工具
%%% - 支持多 Provider（内置、自定义、MCP 等）
%%% - 工具执行
%%% - JSON Schema 转换（供 LLM 使用）
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 获取所有文件工具（使用默认 provider）
%%% FileTools = beamai_tools:get_tools(file),
%%%
%%% %% 使用多个 provider
%%% Tools = beamai_tools:get_tools([file, shell], #{
%%%     providers => [
%%%         beamai_tool_provider_builtin,
%%%         beamai_tool_provider_custom
%%%     ]
%%% }),
%%%
%%% %% 注册自定义工具
%%% beamai_tool_provider_custom:register_tool(MyTool),
%%%
%%% %% 转换为 LLM 函数调用规格
%%% Specs = beamai_tools:to_llm_specs(FileTools),
%%%
%%% %% 执行工具
%%% {ok, Result} = beamai_tools:execute(<<"file_read">>, #{path => <<"/tmp/test.txt">>}).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tools).

-include("beamai_tools.hrl").

%% 工具获取 API（向后兼容）
-export([
    get_tools/1,
    get_all_tools/0,
    find_tool/1,
    find_tool/2
]).

%% 工具获取 API（支持 Provider）
-export([
    get_tools/2,
    get_all_tools/1,
    find_tool_with_opts/2
]).

%% Provider 管理
-export([
    default_providers/0,
    list_providers/1,
    provider_info/1
]).

%% 工具执行 API
-export([
    execute/2,
    execute/3
]).

%% 工具转换 API
-export([
    to_llm_spec/1,
    to_llm_specs/1,
    get_name/1,
    get_handler/1
]).

%% 默认 providers
-define(DEFAULT_PROVIDERS, [
    beamai_tool_provider_builtin
]).

%%====================================================================
%% 工具获取 API（向后兼容）
%%====================================================================

%% @doc 获取指定分类的工具
%%
%% 使用默认 provider（内置工具）。
%% 支持单个分类或分类列表。
%%
%% 示例：
%% ```
%% FileTools = beamai_tools:get_tools(file),
%% AllTools = beamai_tools:get_tools([file, shell, todo, human]).
%% ```
-spec get_tools(tool_category() | [tool_category()]) -> [tool_def()].
get_tools(Categories) ->
    get_tools(Categories, #{}).

%% @doc 获取所有内置工具
-spec get_all_tools() -> [tool_def()].
get_all_tools() ->
    get_all_tools(#{}).

%% @doc 按名称查找工具（在所有内置工具中）
-spec find_tool(binary()) -> {ok, tool_def()} | {error, not_found}.
find_tool(Name) ->
    find_tool_with_opts(Name, #{}).

%% @doc 在指定工具列表中查找工具
-spec find_tool(binary(), [tool_def()]) -> {ok, tool_def()} | {error, not_found}.
find_tool(Name, Tools) ->
    find_tool_by_name(Name, Tools).

%%====================================================================
%% 工具获取 API（支持 Provider）
%%====================================================================

%% @doc 从指定 providers 获取工具
%%
%% Opts 参数：
%% - providers: 工具提供者模块列表，默认 [beamai_tool_provider_builtin]
%% - context: 传递给 provider 的上下文
%% - include_unavailable: 是否包含不可用的 provider，默认 false
%%
%% 示例：
%% ```
%% %% 使用内置和自定义 provider
%% Tools = beamai_tools:get_tools([file, custom], #{
%%%     providers => [
%%%         beamai_tool_provider_builtin,
%%%         beamai_tool_provider_custom
%%%     ]
%%% }).
%% ```
-spec get_tools(Categories, Opts) -> [tool_def()]
    when Categories :: tool_category() | [tool_category()] | all,
         Opts :: #{
             providers => [tool_provider()],
             context => map(),
             include_unavailable => boolean()
         }.
get_tools(Categories, Opts) ->
    Providers = maps:get(providers, Opts, default_providers()),
    Context = maps:get(context, Opts, #{}),
    IncludeUnavailable = maps:get(include_unavailable, Opts, false),

    ProviderOpts = #{categories => Categories, context => Context},

    %% 从所有 provider 收集工具
    lists:flatmap(fun(Provider) ->
        case IncludeUnavailable orelse beamai_tool_provider:is_available(Provider) of
            true ->
                case catch Provider:list_tools(ProviderOpts) of
                    {ok, Tools} -> Tools;
                    {error, _} -> [];
                    {'EXIT', _} -> []
                end;
            false ->
                []
        end
    end, Providers).

%% @doc 获取所有工具（从所有 provider）
-spec get_all_tools(Opts :: map()) -> [tool_def()].
get_all_tools(Opts) ->
    get_tools(all, Opts).

%% @doc 按名称查找工具（支持 provider 选项）
%%
%% 在所有配置的 provider 中按顺序查找。
-spec find_tool_with_opts(binary(), map()) -> {ok, tool_def()} | {error, not_found}.
find_tool_with_opts(Name, Opts) ->
    Providers = maps:get(providers, Opts, default_providers()),
    ProviderOpts = maps:get(context, Opts, #{}),

    find_in_providers(Name, Providers, ProviderOpts).

%%====================================================================
%% Provider 管理
%%====================================================================

%% @doc 获取默认 providers
-spec default_providers() -> [tool_provider()].
default_providers() ->
    application:get_env(beamai_tools, providers, ?DEFAULT_PROVIDERS).

%% @doc 列出 providers 及其信息
-spec list_providers(Opts :: map()) -> [#{module := module(), info := map(), available := boolean()}].
list_providers(Opts) ->
    Providers = maps:get(providers, Opts, default_providers()),
    [#{
        module => P,
        info => beamai_tool_provider:get_info(P),
        available => beamai_tool_provider:is_available(P)
    } || P <- Providers].

%% @doc 获取单个 provider 的信息
-spec provider_info(module()) -> map().
provider_info(Provider) ->
    beamai_tool_provider:get_info(Provider).

%%====================================================================
%% 工具执行 API
%%====================================================================

%% @doc 执行工具
%%
%% 根据工具名称查找并执行工具。
-spec execute(binary(), map()) -> {ok, term()} | {ok, term(), map()} | {error, term()}.
execute(ToolName, Args) ->
    execute(ToolName, Args, #{}).

%% @doc 执行工具（带上下文和选项）
%%
%% 选项：
%% - providers: 用于查找工具的 provider 列表
%% - context: 传递给工具处理器的上下文
-spec execute(binary(), map(), map()) -> {ok, term()} | {ok, term(), map()} | {error, term()}.
execute(ToolName, Args, Opts) ->
    Context = maps:get(context, Opts, #{}),

    case find_tool_with_opts(ToolName, Opts) of
        {ok, #{handler := Handler}} ->
            call_handler(Handler, Args, Context);
        {error, not_found} ->
            {error, {unknown_tool, ToolName}}
    end.

%%====================================================================
%% 工具转换 API
%%====================================================================

%% @doc 将工具转换为 LLM 函数调用规格
%%
%% 输出格式适用于 OpenAI/Anthropic 等 API。
-spec to_llm_spec(tool_def()) -> map().
to_llm_spec(#{name := Name, description := Desc, parameters := Params}) ->
    #{
        name => Name,
        description => Desc,
        input_schema => Params
    }.

%% @doc 批量转换为 LLM 规格
-spec to_llm_specs([tool_def()]) -> [map()].
to_llm_specs(Tools) ->
    [to_llm_spec(T) || T <- Tools].

%% @doc 获取工具名称
-spec get_name(tool_def()) -> binary().
get_name(#{name := Name}) -> Name.

%% @doc 获取工具处理器
-spec get_handler(tool_def()) -> function().
get_handler(#{handler := Handler}) -> Handler.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 在 providers 中按顺序查找工具
-spec find_in_providers(binary(), [tool_provider()], map()) ->
    {ok, tool_def()} | {error, not_found}.
find_in_providers(_Name, [], _Opts) ->
    {error, not_found};
find_in_providers(Name, [Provider | Rest], Opts) ->
    case beamai_tool_provider:is_available(Provider) of
        true ->
            case beamai_tool_provider:find_tool_in_provider(Provider, Name, Opts) of
                {ok, Tool} -> {ok, Tool};
                {error, not_found} -> find_in_providers(Name, Rest, Opts);
                {error, _} -> find_in_providers(Name, Rest, Opts)
            end;
        false ->
            find_in_providers(Name, Rest, Opts)
    end.

%% @private 按名称在工具列表中查找
-spec find_tool_by_name(binary(), [tool_def()]) -> {ok, tool_def()} | {error, not_found}.
find_tool_by_name(_Name, []) ->
    {error, not_found};
find_tool_by_name(Name, [#{name := Name} = Tool | _]) ->
    {ok, Tool};
find_tool_by_name(Name, [_ | Rest]) ->
    find_tool_by_name(Name, Rest).

%% @private 调用处理器
-spec call_handler(function(), map(), map()) -> {ok, term()} | {ok, term(), map()} | {error, term()}.
call_handler(Handler, Args, Context) ->
    try
        case erlang:fun_info(Handler, arity) of
            {arity, 1} -> Handler(Args);
            {arity, 2} -> Handler(Args, Context);
            _ -> Handler(Args)
        end
    catch
        Class:Reason:Stacktrace ->
            logger:error("工具执行失败: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {error, {handler_error, {Class, Reason}}}
    end.
