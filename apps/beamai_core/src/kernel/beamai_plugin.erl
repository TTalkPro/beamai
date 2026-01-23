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

-spec new(binary(), [beamai_function:function_def()]) -> plugin().
new(Name, Functions) ->
    new(Name, Functions, #{}).

-spec new(binary(), [beamai_function:function_def()], map()) -> plugin().
new(Name, Functions, Opts) ->
    TaggedFunctions = [F#{plugin => Name} || F <- Functions],
    maps:merge(Opts, #{
        name => Name,
        functions => TaggedFunctions
    }).

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

-spec get_function(plugin(), binary()) -> {ok, beamai_function:function_def()} | error.
get_function(#{functions := Functions}, FuncName) ->
    case [F || #{name := N} = F <- Functions, N =:= FuncName] of
        [Found | _] -> {ok, Found};
        [] -> error
    end.

-spec list_functions(plugin()) -> [beamai_function:function_def()].
list_functions(#{functions := Functions}) -> Functions.

-spec to_tool_schemas(plugin()) -> [map()].
to_tool_schemas(Plugin) ->
    to_tool_schemas(Plugin, openai).

-spec to_tool_schemas(plugin(), openai | anthropic) -> [map()].
to_tool_schemas(#{functions := Functions}, Format) ->
    [beamai_function:to_tool_schema(F, Format) || F <- Functions].

-spec get_name(plugin()) -> binary().
get_name(#{name := Name}) -> Name.
