-module(beamai_prompt).

%% API
-export([new/1]).
-export([render/2]).
-export([get_variables/1]).

%% Types
-export_type([prompt_template/0]).

-type prompt_template() :: #{
    template := binary(),
    input_variables := [binary()]
}.

%%====================================================================
%% API
%%====================================================================

-spec new(binary()) -> prompt_template().
new(Template) ->
    Variables = extract_variables(Template),
    #{
        template => Template,
        input_variables => Variables
    }.

-spec render(prompt_template(), map() | beamai_context:t()) -> {ok, binary()} | {error, term()}.
render(#{template := Template}, #{'__context__' := true} = Context) ->
    Vars = maps:get(variables, Context, #{}),
    do_render(Template, Vars);
render(#{template := Template}, Vars) when is_map(Vars) ->
    do_render(Template, Vars).

-spec get_variables(prompt_template()) -> [binary()].
get_variables(#{input_variables := Vars}) -> Vars.

%%====================================================================
%% Internal
%%====================================================================

do_render(Template, Vars) ->
    try
        Result = maps:fold(fun(Key, Value, Acc) ->
            KeyBin = to_binary(Key),
            Pattern = <<"{{", KeyBin/binary, "}}">>,
            ValueBin = to_binary(Value),
            binary:replace(Acc, Pattern, ValueBin, [global])
        end, Template, Vars),
        {ok, Result}
    catch
        _:Reason ->
            {error, {render_failed, Reason}}
    end.

extract_variables(Template) ->
    case re:run(Template, <<"\\{\\{([^}]+)\\}\\}">>, [global, {capture, [1], binary}]) of
        {match, Matches} ->
            lists:usort([V || [V] <- Matches]);
        nomatch ->
            []
    end.

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_float(V) -> float_to_binary(V, [{decimals, 10}, compact]);
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) -> list_to_binary(io_lib:format("~p", [V])).
