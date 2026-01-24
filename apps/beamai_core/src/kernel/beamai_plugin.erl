%%%-------------------------------------------------------------------
%%% @doc 插件管理：创建、查找、合并
%%%
%%% 插件是一组相关函数的逻辑分组，支持：
%%% - 手动创建插件（new/2, new/3）
%%% - 从模块自动加载（from_module/1）
%%% - 函数查找和列举
%%% - 生成 tool schema
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_plugin).

%% API
-export([new/2, new/3]).
-export([from_module/1]).
-export([get_function/2]).
-export([list_functions/1]).
-export([to_tool_schemas/1, to_tool_schemas/2]).
-export([get_name/1]).

%% Types
-export_type([plugin/0]).

-type plugin() :: #{
    name := binary(),
    description => binary(),
    functions := [beamai_function:function_def()],
    metadata => map()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc 创建插件（仅名称和函数列表）
%%
%% @param Name 插件名称（如 <<"weather">>）
%% @param Functions 函数定义列表
%% @returns 插件 Map
-spec new(binary(), [beamai_function:function_def()]) -> plugin().
new(Name, Functions) ->
    new(Name, Functions, #{}).

%% @doc 创建插件（带额外选项）
%%
%% 函数会自动标记其所属插件名称（plugin 字段）。
%%
%% @param Name 插件名称
%% @param Functions 函数定义列表
%% @param Opts 额外选项（如 description、metadata）
%% @returns 插件 Map
-spec new(binary(), [beamai_function:function_def()], map()) -> plugin().
new(Name, Functions, Opts) ->
    TaggedFunctions = [F#{plugin => Name} || F <- Functions],
    maps:merge(Opts, #{
        name => Name,
        functions => TaggedFunctions
    }).

%% @doc 从模块自动加载插件
%%
%% 模块需实现 plugin_info/0 和 functions/0 回调。
%% plugin_info/0 返回 #{name := binary(), description => binary()}。
%% functions/0 返回函数定义列表。
%%
%% @param Module 实现了插件回调的模块
%% @returns {ok, Plugin} | {error, Reason}
-spec from_module(module()) -> {ok, plugin()} | {error, term()}.
from_module(Module) ->
    try
        Info = Module:plugin_info(),
        Name = maps:get(name, Info),
        RawFunctions = Module:functions(),
        Functions = [F#{plugin => Name} || F <- RawFunctions],
        Plugin = #{
            name => Name,
            description => maps:get(description, Info, <<"">>),
            functions => Functions,
            metadata => maps:get(metadata, Info, #{})
        },
        {ok, Plugin}
    catch
        error:undef ->
            {error, {module_not_found, Module}};
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% @doc 按名称查找插件中的函数
%%
%% @param Plugin 插件
%% @param FuncName 函数名称
%% @returns {ok, FuncDef} 找到时返回 | error 未找到
-spec get_function(plugin(), binary()) -> {ok, beamai_function:function_def()} | error.
get_function(#{functions := Functions}, FuncName) ->
    case [F || #{name := N} = F <- Functions, N =:= FuncName] of
        [Found | _] -> {ok, Found};
        [] -> error
    end.

%% @doc 列出插件中所有已注册的函数
-spec list_functions(plugin()) -> [beamai_function:function_def()].
list_functions(#{functions := Functions}) -> Functions.

%% @doc 将插件中所有函数转换为 tool schema（默认 OpenAI 格式）
-spec to_tool_schemas(plugin()) -> [map()].
to_tool_schemas(Plugin) ->
    to_tool_schemas(Plugin, openai).

%% @doc 将插件中所有函数转换为指定提供商的 tool schema
%%
%% @param Plugin 插件
%% @param Format 提供商格式（openai | anthropic）
%% @returns tool schema 列表
-spec to_tool_schemas(plugin(), openai | anthropic) -> [map()].
to_tool_schemas(#{functions := Functions}, Format) ->
    [beamai_function:to_tool_schema(F, Format) || F <- Functions].

%% @doc 获取插件名称
-spec get_name(plugin()) -> binary().
get_name(#{name := Name}) -> Name.
