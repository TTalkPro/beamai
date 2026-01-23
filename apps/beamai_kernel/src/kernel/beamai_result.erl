-module(beamai_result).

%% API
-export([ok/1, ok/2]).
-export([error/1]).
-export([is_ok/1, is_error/1]).
-export([get_value/1, get_value/2]).
-export([get_error/1]).
-export([map/2, flat_map/2]).
-export([to_json/1]).

%% Types
-export_type([result/0, result/1]).

-type result() :: result(term()).
-type result(T) :: {ok, T} | {ok, T, beamai_context:t()} | {error, term()}.

%%====================================================================
%% API
%%====================================================================

-spec ok(term()) -> {ok, term()}.
ok(Value) -> {ok, Value}.

-spec ok(term(), beamai_context:t()) -> {ok, term(), beamai_context:t()}.
ok(Value, Context) -> {ok, Value, Context}.

-spec error(term()) -> {error, term()}.
error(Reason) -> {error, Reason}.

-spec is_ok(result()) -> boolean().
is_ok({ok, _}) -> true;
is_ok({ok, _, _}) -> true;
is_ok(_) -> false.

-spec is_error(result()) -> boolean().
is_error({error, _}) -> true;
is_error(_) -> false.

-spec get_value(result()) -> term() | undefined.
get_value({ok, Value}) -> Value;
get_value({ok, Value, _Ctx}) -> Value;
get_value(_) -> undefined.

-spec get_value(result(), term()) -> term().
get_value({ok, Value}, _Default) -> Value;
get_value({ok, Value, _Ctx}, _Default) -> Value;
get_value(_, Default) -> Default.

-spec get_error(result()) -> term() | undefined.
get_error({error, Reason}) -> Reason;
get_error(_) -> undefined.

-spec map(result(), fun((term()) -> term())) -> result().
map({ok, Value}, Fun) -> {ok, Fun(Value)};
map({ok, Value, Ctx}, Fun) -> {ok, Fun(Value), Ctx};
map({error, _} = Err, _Fun) -> Err.

-spec flat_map(result(), fun((term()) -> result())) -> result().
flat_map({ok, Value}, Fun) -> Fun(Value);
flat_map({ok, Value, _Ctx}, Fun) -> Fun(Value);
flat_map({error, _} = Err, _Fun) -> Err.

-spec to_json(result()) -> map().
to_json({ok, Value}) ->
    #{status => <<"ok">>, value => term_to_json(Value)};
to_json({ok, Value, _Ctx}) ->
    #{status => <<"ok">>, value => term_to_json(Value)};
to_json({error, Reason}) ->
    #{status => <<"error">>, error => term_to_json(Reason)}.

%%====================================================================
%% Internal
%%====================================================================

term_to_json(V) when is_binary(V) -> V;
term_to_json(V) when is_number(V) -> V;
term_to_json(V) when is_boolean(V) -> V;
term_to_json(V) when is_atom(V) -> atom_to_binary(V, utf8);
term_to_json(V) when is_map(V) -> V;
term_to_json(V) when is_list(V) -> V;
term_to_json(V) -> list_to_binary(io_lib:format("~p", [V])).
