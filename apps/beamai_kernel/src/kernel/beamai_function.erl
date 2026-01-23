-module(beamai_function).

%% API
-export([new/2, new/3]).
-export([validate/1]).
-export([invoke/2, invoke/3]).
-export([to_tool_schema/1, to_tool_schema/2]).
-export([get_name/1, get_full_name/1]).

%% Types
-export_type([function_def/0, handler/0, function_result/0,
              args/0, parameters_schema/0, param_spec/0]).

-type function_def() :: #{
    name := binary(),
    handler := handler(),
    description => binary(),
    parameters => parameters_schema(),
    return_type => return_schema(),
    plugin => binary(),
    timeout => pos_integer(),
    retry => #{max => integer(), delay => integer()},
    filters => [filter_ref()],
    metadata => map()
}.

-type handler() ::
    fun((args()) -> function_result())
    | fun((args(), beamai_context:t()) -> function_result())
    | {module(), atom()}
    | {module(), atom(), [term()]}
    | {service, service_type(), map()}.

-type function_result() ::
    {ok, term()}
    | {ok, term(), beamai_context:t()}
    | {error, term()}.

-type args() :: map().

-type parameters_schema() :: #{
    atom() | binary() => param_spec()
}.

-type param_spec() :: #{
    type := string | integer | float | boolean | array | object,
    description => binary(),
    required => boolean(),
    default => term(),
    enum => [term()],
    items => param_spec(),
    properties => parameters_schema()
}.

-type return_schema() :: #{
    type => atom(),
    description => binary()
}.

-type filter_ref() :: binary() | atom().
-type service_type() :: chat_completion | embedding | atom().

%%====================================================================
%% API
%%====================================================================

-spec new(binary(), handler()) -> function_def().
new(Name, Handler) ->
    #{name => Name, handler => Handler}.

-spec new(binary(), handler(), map()) -> function_def().
new(Name, Handler, Opts) ->
    maps:merge(Opts, #{name => Name, handler => Handler}).

-spec validate(function_def()) -> ok | {error, [term()]}.
validate(#{name := Name, handler := Handler} = _FuncDef) ->
    Errors = lists:flatten([
        validate_name(Name),
        validate_handler(Handler)
    ]),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end;
validate(_) ->
    {error, [missing_required_fields]}.

-spec invoke(function_def(), args()) -> function_result().
invoke(FuncDef, Args) ->
    invoke(FuncDef, Args, beamai_context:new()).

-spec invoke(function_def(), args(), beamai_context:t()) -> function_result().
invoke(#{handler := Handler} = FuncDef, Args, Context) ->
    Timeout = maps:get(timeout, FuncDef, 30000),
    RetryConf = maps:get(retry, FuncDef, #{max => 0, delay => 0}),
    invoke_with_retry(Handler, Args, Context, RetryConf, Timeout).

-spec to_tool_schema(function_def()) -> map().
to_tool_schema(FuncDef) ->
    to_tool_schema(FuncDef, openai).

-spec to_tool_schema(function_def(), openai | anthropic) -> map().
to_tool_schema(#{name := _Name} = FuncDef, openai) ->
    Description = maps:get(description, FuncDef, <<"">>),
    Parameters = build_json_schema(maps:get(parameters, FuncDef, #{})),
    #{
        type => <<"function">>,
        function => #{
            name => full_name(FuncDef),
            description => Description,
            parameters => Parameters
        }
    };
to_tool_schema(#{name := _Name} = FuncDef, anthropic) ->
    Description = maps:get(description, FuncDef, <<"">>),
    Parameters = build_json_schema(maps:get(parameters, FuncDef, #{})),
    #{
        name => full_name(FuncDef),
        description => Description,
        input_schema => Parameters
    }.

-spec get_name(function_def()) -> binary().
get_name(#{name := Name}) -> Name.

-spec get_full_name(function_def()) -> binary().
get_full_name(FuncDef) -> full_name(FuncDef).

%%====================================================================
%% Internal
%%====================================================================

full_name(#{plugin := Plugin, name := Name}) ->
    <<Plugin/binary, ".", Name/binary>>;
full_name(#{name := Name}) ->
    Name.

validate_name(Name) when is_binary(Name), byte_size(Name) > 0 -> [];
validate_name(_) -> [{invalid_name, <<"name must be a non-empty binary">>}].

validate_handler(Fun) when is_function(Fun, 1) -> [];
validate_handler(Fun) when is_function(Fun, 2) -> [];
validate_handler({M, F}) when is_atom(M), is_atom(F) -> [];
validate_handler({M, F, A}) when is_atom(M), is_atom(F), is_list(A) -> [];
validate_handler({service, Type, Config}) when is_atom(Type), is_map(Config) -> [];
validate_handler(_) -> [{invalid_handler, <<"handler must be fun/1, fun/2, {M,F}, {M,F,A}, or {service, Type, Config}">>}].

invoke_with_retry(Handler, Args, Context, #{max := Max, delay := Delay}, Timeout) ->
    invoke_with_retry(Handler, Args, Context, Max, Delay, Timeout).

invoke_with_retry(Handler, Args, Context, RetriesLeft, Delay, Timeout) ->
    case call_handler(Handler, Args, Context, Timeout) of
        {error, _Reason} when RetriesLeft > 0 ->
            timer:sleep(Delay),
            invoke_with_retry(Handler, Args, Context, RetriesLeft - 1, Delay, Timeout);
        Result ->
            Result
    end.

call_handler(Fun, Args, _Context, _Timeout) when is_function(Fun, 1) ->
    try Fun(Args)
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end;
call_handler(Fun, Args, Context, _Timeout) when is_function(Fun, 2) ->
    try Fun(Args, Context)
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end;
call_handler({M, F}, Args, Context, _Timeout) ->
    try M:F(Args, Context)
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end;
call_handler({service, _Type, _Config}, _Args, _Context, _Timeout) ->
    %% Service delegation handled at kernel level
    {error, service_delegation_not_implemented};
call_handler({M, F, ExtraArgs}, Args, Context, _Timeout) ->
    try erlang:apply(M, F, [Args, Context | ExtraArgs])
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end.

build_json_schema(Params) when map_size(Params) =:= 0 ->
    #{type => <<"object">>, properties => #{}};
build_json_schema(Params) ->
    Properties = maps:fold(fun(K, Spec, Acc) ->
        Key = to_binary(K),
        Acc#{Key => param_to_json_schema(Spec)}
    end, #{}, Params),
    Required = maps:fold(fun(K, #{required := true}, Acc) ->
        [to_binary(K) | Acc];
    (_, _, Acc) ->
        Acc
    end, [], Params),
    Schema = #{type => <<"object">>, properties => Properties},
    case Required of
        [] -> Schema;
        _ -> Schema#{required => Required}
    end.

param_to_json_schema(#{type := Type} = Spec) ->
    S0 = #{type => type_to_json(Type)},
    S1 = case maps:find(description, Spec) of
        {ok, D} -> S0#{description => D};
        error -> S0
    end,
    S2 = case maps:find(enum, Spec) of
        {ok, E} -> S1#{enum => E};
        error -> S1
    end,
    S3 = case maps:find(items, Spec) of
        {ok, Items} -> S2#{items => param_to_json_schema(Items)};
        error -> S2
    end,
    case maps:find(properties, Spec) of
        {ok, Props} ->
            SubSchema = build_json_schema(Props),
            S3#{properties => maps:get(properties, SubSchema)};
        error ->
            S3
    end.

type_to_json(string) -> <<"string">>;
type_to_json(integer) -> <<"integer">>;
type_to_json(float) -> <<"number">>;
type_to_json(boolean) -> <<"boolean">>;
type_to_json(array) -> <<"array">>;
type_to_json(object) -> <<"object">>;
type_to_json(Other) -> atom_to_binary(Other, utf8).

to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.
