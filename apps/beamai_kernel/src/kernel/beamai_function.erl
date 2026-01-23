-module(beamai_function).

%% API
-export([new/2, new/3]).
-export([validate/1]).
-export([invoke/2, invoke/3]).
-export([to_tool_spec/1]).
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
    | {module(), atom(), [term()]}.

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
validate(#{name := Name, handler := Handler}) ->
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

%% @doc Convert function_def to unified tool spec
-spec to_tool_spec(function_def()) -> map().
to_tool_spec(FuncDef) ->
    #{
        name => full_name(FuncDef),
        description => maps:get(description, FuncDef, <<"">>),
        parameters => build_json_schema(maps:get(parameters, FuncDef, #{}))
    }.

%% @doc Convert to provider-specific tool schema
-spec to_tool_schema(function_def()) -> map().
to_tool_schema(FuncDef) ->
    to_tool_schema(FuncDef, openai).

-spec to_tool_schema(function_def(), openai | anthropic | atom()) -> map().
to_tool_schema(FuncDef, Provider) ->
    ToolSpec = to_tool_spec(FuncDef),
    tool_spec_to_provider(ToolSpec, Provider).

%%--------------------------------------------------------------------
%% Provider-specific schema conversion
%%--------------------------------------------------------------------

tool_spec_to_provider(ToolSpec, anthropic) ->
    to_anthropic_schema(ToolSpec);
tool_spec_to_provider(ToolSpec, _) ->
    %% OpenAI format (also used by ollama, zhipu, deepseek, etc.)
    to_openai_schema(ToolSpec).

to_openai_schema(#{name := Name, description := Desc, parameters := Params}) ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => Name,
            <<"description">> => Desc,
            <<"parameters">> => Params
        }
    };
to_openai_schema(#{name := Name, description := Desc}) ->
    to_openai_schema(#{name => Name, description => Desc, parameters => default_params()});
to_openai_schema(#{name := Name}) ->
    to_openai_schema(#{name => Name, description => <<"Tool: ", Name/binary>>, parameters => default_params()}).

to_anthropic_schema(#{name := Name, description := Desc, parameters := Params}) ->
    #{
        <<"name">> => Name,
        <<"description">> => Desc,
        <<"input_schema">> => Params
    };
to_anthropic_schema(#{name := Name, description := Desc}) ->
    to_anthropic_schema(#{name => Name, description => Desc, parameters => default_params()}).

default_params() ->
    #{type => object, properties => #{}, required => []}.

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
validate_handler(_) -> [{invalid_handler, <<"handler must be fun/1, fun/2, {M,F}, or {M,F,A}">>}].

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
call_handler({M, F, ExtraArgs}, Args, Context, _Timeout) ->
    try erlang:apply(M, F, [Args, Context | ExtraArgs])
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end.

%% Convert our param_spec format to JSON Schema format
build_json_schema(Params) when map_size(Params) =:= 0 ->
    #{type => object, properties => #{}, required => []};
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
    #{type => object, properties => Properties, required => Required}.

param_to_json_schema(#{type := Type} = Spec) ->
    S0 = #{type => type_to_schema(Type)},
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

type_to_schema(string) -> string;
type_to_schema(integer) -> integer;
type_to_schema(float) -> number;
type_to_schema(boolean) -> boolean;
type_to_schema(array) -> array;
type_to_schema(object) -> object;
type_to_schema(Other) -> Other.

to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.
