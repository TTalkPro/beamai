-module(beamai_kernel).

%% Build API
-export([new/0, new/1]).
-export([add_plugin/2, add_plugin/3]).
-export([add_plugin_from_module/2]).
-export([add_service/3]).
-export([add_chat_completion/2]).
-export([add_embedding/2]).
-export([add_filter/2]).

%% Invoke API
-export([invoke/3, invoke/4]).
-export([invoke_chat/3]).
-export([invoke_chat_with_tools/3]).

%% Query API
-export([get_function/2]).
-export([list_functions/1]).
-export([get_tool_schemas/1, get_tool_schemas/2]).
-export([get_service/2]).

%% Types
-export_type([kernel/0, service_type/0, service_config/0, kernel_settings/0,
              chat_opts/0]).

-type kernel() :: #{
    '__kernel__' := true,
    plugins := #{binary() => beamai_plugin:plugin_def()},
    services := #{service_type() => service_config()},
    filters := [beamai_filter:filter_def()],
    settings := kernel_settings()
}.

-type service_type() :: chat_completion | embedding | memory | atom().

-type service_config() :: #{
    type := service_type(),
    connector := module(),
    config := map()
}.

-type kernel_settings() :: #{
    max_tool_iterations => pos_integer(),
    default_timeout => pos_integer(),
    atom() => term()
}.

-type chat_opts() :: #{
    model => binary(),
    temperature => float(),
    max_tokens => pos_integer(),
    tools => [map()],
    tool_choice => auto | none | required,
    stream => boolean(),
    max_tool_iterations => pos_integer(),
    atom() => term()
}.

%%====================================================================
%% Build API
%%====================================================================

-spec new() -> kernel().
new() ->
    new(#{}).

-spec new(kernel_settings()) -> kernel().
new(Settings) ->
    #{
        '__kernel__' => true,
        plugins => #{},
        services => #{},
        filters => [],
        settings => Settings
    }.

-spec add_plugin(kernel(), beamai_plugin:plugin_def()) -> kernel().
add_plugin(#{plugins := Plugins} = Kernel, #{name := Name} = Plugin) ->
    Kernel#{plugins => Plugins#{Name => Plugin}}.

-spec add_plugin(kernel(), binary(), [beamai_function:function_def()]) -> kernel().
add_plugin(Kernel, Name, Functions) ->
    Plugin = beamai_plugin:new(Name, Functions),
    add_plugin(Kernel, Plugin).

-spec add_plugin_from_module(kernel(), module()) -> kernel().
add_plugin_from_module(Kernel, Module) ->
    case beamai_plugin:from_module(Module) of
        {ok, Plugin} -> add_plugin(Kernel, Plugin);
        {error, Reason} -> erlang:error({plugin_load_failed, Module, Reason})
    end.

-spec add_service(kernel(), service_type(), service_config()) -> kernel().
add_service(#{services := Services} = Kernel, Type, Config) ->
    Kernel#{services => Services#{Type => Config#{type => Type}}}.

-spec add_chat_completion(kernel(), map()) -> kernel().
add_chat_completion(Kernel, Config) ->
    ServiceConfig = #{
        type => chat_completion,
        connector => maps:get(connector, Config),
        config => maps:without([connector], Config)
    },
    add_service(Kernel, chat_completion, ServiceConfig).

-spec add_embedding(kernel(), map()) -> kernel().
add_embedding(Kernel, Config) ->
    ServiceConfig = #{
        type => embedding,
        connector => maps:get(connector, Config),
        config => maps:without([connector], Config)
    },
    add_service(Kernel, embedding, ServiceConfig).

-spec add_filter(kernel(), beamai_filter:filter_def()) -> kernel().
add_filter(#{filters := Filters} = Kernel, Filter) ->
    Kernel#{filters => Filters ++ [Filter]}.

%%====================================================================
%% Invoke API
%%====================================================================

-spec invoke(kernel(), binary(), beamai_function:args()) -> beamai_function:function_result().
invoke(Kernel, FuncName, Args) ->
    invoke(Kernel, FuncName, Args, beamai_context:new()).

-spec invoke(kernel(), binary(), beamai_function:args(), beamai_context:t()) -> beamai_function:function_result().
invoke(#{filters := Filters} = Kernel, FuncName, Args, Context0) ->
    case get_function(Kernel, FuncName) of
        {ok, FuncDef} ->
            Context = beamai_context:with_kernel(Context0, Kernel),
            case beamai_filter:apply_pre_filters(Filters, FuncDef, Args, Context) of
                {ok, FilteredArgs, FilteredCtx} ->
                    Result = beamai_function:invoke(FuncDef, FilteredArgs, FilteredCtx),
                    case Result of
                        {ok, Value} ->
                            apply_post_and_return(Filters, FuncDef, Value, FilteredCtx);
                        {ok, Value, NewCtx} ->
                            apply_post_and_return(Filters, FuncDef, Value, NewCtx);
                        {error, _} = Err ->
                            Err
                    end;
                {skip, Value} ->
                    {ok, Value};
                {error, _} = Err ->
                    Err
            end;
        error ->
            {error, {function_not_found, FuncName}}
    end.

-spec invoke_chat(kernel(), [beamai_context:message()], chat_opts()) ->
    {ok, map()} | {error, term()}.
invoke_chat(Kernel, Messages, Opts) ->
    case get_service(Kernel, chat_completion) of
        {ok, ServiceConfig} ->
            #{filters := Filters} = Kernel,
            Context = beamai_context:new(),
            case beamai_filter:apply_pre_chat_filters(Filters, Messages, Context) of
                {ok, FilteredMsgs, FilteredCtx} ->
                    Result = beamai_chat_completion:chat(ServiceConfig, FilteredMsgs, Opts),
                    case Result of
                        {ok, Response} ->
                            case beamai_filter:apply_post_chat_filters(Filters, Response, FilteredCtx) of
                                {ok, FinalResp, _} -> {ok, FinalResp};
                                {error, _} = Err -> Err
                            end;
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        error ->
            {error, no_chat_completion_service}
    end.

-spec invoke_chat_with_tools(kernel(), [beamai_context:message()], chat_opts()) ->
    {ok, map()} | {error, term()}.
invoke_chat_with_tools(Kernel, Messages, Opts) ->
    ToolSchemas = get_tool_schemas(Kernel),
    ChatOpts = Opts#{tools => ToolSchemas, tool_choice => maps:get(tool_choice, Opts, auto)},
    case get_service(Kernel, chat_completion) of
        {ok, ServiceConfig} ->
            MaxIter = maps:get(max_tool_iterations, Opts,
                maps:get(max_tool_iterations, maps:get(settings, Kernel, #{}), 10)),
            tool_calling_loop(Kernel, Messages, ChatOpts, ServiceConfig, MaxIter);
        error ->
            {error, no_chat_completion_service}
    end.

%%====================================================================
%% Query API
%%====================================================================

-spec get_function(kernel(), binary()) -> {ok, beamai_function:function_def()} | error.
get_function(#{plugins := Plugins}, FuncName) ->
    case binary:split(FuncName, <<".">>) of
        [PluginName, LocalName] ->
            case maps:find(PluginName, Plugins) of
                {ok, Plugin} -> beamai_plugin:get_function(Plugin, LocalName);
                error -> error
            end;
        [_Name] ->
            %% Search all plugins
            search_all_plugins(Plugins, FuncName)
    end.

-spec list_functions(kernel()) -> [beamai_function:function_def()].
list_functions(#{plugins := Plugins}) ->
    maps:fold(fun(_Name, Plugin, Acc) ->
        Acc ++ beamai_plugin:list_functions(Plugin)
    end, [], Plugins).

-spec get_tool_schemas(kernel()) -> [map()].
get_tool_schemas(Kernel) ->
    get_tool_schemas(Kernel, openai).

-spec get_tool_schemas(kernel(), openai | anthropic) -> [map()].
get_tool_schemas(#{plugins := Plugins}, Format) ->
    maps:fold(fun(_Name, Plugin, Acc) ->
        Acc ++ beamai_plugin:to_tool_schemas(Plugin, Format)
    end, [], Plugins).

-spec get_service(kernel(), service_type()) -> {ok, service_config()} | error.
get_service(#{services := Services}, Type) ->
    maps:find(Type, Services).

%%====================================================================
%% Internal - Tool Calling Loop
%%====================================================================

tool_calling_loop(_Kernel, _Msgs, _Opts, _Svc, 0) ->
    {error, max_tool_iterations};
tool_calling_loop(Kernel, Msgs, Opts, Svc, N) ->
    case beamai_chat_completion:chat(Svc, Msgs, Opts) of
        {ok, #{tool_calls := TCs} = _Response} when is_list(TCs), TCs =/= [] ->
            ToolResults = execute_tool_calls(Kernel, TCs),
            AssistantMsg = #{role => assistant, content => null, tool_calls => TCs},
            NewMsgs = Msgs ++ [AssistantMsg | ToolResults],
            tool_calling_loop(Kernel, NewMsgs, Opts, Svc, N - 1);
        {ok, Response} ->
            {ok, Response};
        {error, _} = Err ->
            Err
    end.

execute_tool_calls(Kernel, ToolCalls) ->
    lists:map(fun(#{id := Id, function := #{name := Name, arguments := ArgsRaw}}) ->
        Args = parse_tool_args(ArgsRaw),
        ResultContent = case invoke(Kernel, Name, Args) of
            {ok, Value} -> encode_result(Value);
            {ok, Value, _Ctx} -> encode_result(Value);
            {error, Reason} -> encode_result(#{error => Reason})
        end,
        #{role => tool, tool_call_id => Id, content => ResultContent}
    end, ToolCalls).

parse_tool_args(Args) when is_map(Args) -> Args;
parse_tool_args(Args) when is_binary(Args) ->
    case jsx:is_json(Args) of
        true -> jsx:decode(Args, [return_maps, {labels, attempt_atom}]);
        false -> #{raw => Args}
    end;
parse_tool_args(_) -> #{}.

encode_result(Value) when is_binary(Value) -> Value;
encode_result(Value) when is_map(Value) ->
    try jsx:encode(Value)
    catch _:_ -> list_to_binary(io_lib:format("~p", [Value]))
    end;
encode_result(Value) when is_list(Value) ->
    try jsx:encode(Value)
    catch _:_ -> list_to_binary(io_lib:format("~p", [Value]))
    end;
encode_result(Value) when is_number(Value) ->
    list_to_binary(io_lib:format("~p", [Value]));
encode_result(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
encode_result(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).

%%====================================================================
%% Internal - Helpers
%%====================================================================

search_all_plugins(Plugins, FuncName) ->
    Results = maps:fold(fun(_PName, Plugin, Acc) ->
        case beamai_plugin:get_function(Plugin, FuncName) of
            {ok, F} -> [F | Acc];
            error -> Acc
        end
    end, [], Plugins),
    case Results of
        [Found | _] -> {ok, Found};
        [] -> error
    end.

apply_post_and_return(Filters, FuncDef, Value, Context) ->
    case beamai_filter:apply_post_filters(Filters, FuncDef, Value, Context) of
        {ok, FinalValue, _FinalCtx} -> {ok, FinalValue};
        {error, _} = Err -> Err
    end.
