-module(beamai_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% new/2,3 Tests
%%====================================================================

new_basic_test() ->
    Funcs = [
        beamai_function:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end),
        beamai_function:new(<<"sub">>, fun(#{a := A, b := B}) -> {ok, A - B} end)
    ],
    Plugin = beamai_plugin:new(<<"math">>, Funcs),
    ?assertEqual(<<"math">>, maps:get(name, Plugin)),
    ?assertEqual(2, length(maps:get(functions, Plugin))).

new_with_opts_test() ->
    Funcs = [beamai_function:new(<<"test">>, fun(_) -> {ok, ok} end)],
    Plugin = beamai_plugin:new(<<"my_plugin">>, Funcs, #{
        description => <<"A test plugin">>
    }),
    ?assertEqual(<<"A test plugin">>, maps:get(description, Plugin)).

new_tags_functions_with_plugin_name_test() ->
    Funcs = [beamai_function:new(<<"add">>, fun(_) -> {ok, 0} end)],
    Plugin = beamai_plugin:new(<<"math">>, Funcs),
    [F | _] = maps:get(functions, Plugin),
    ?assertEqual(<<"math">>, maps:get(plugin, F)).

%%====================================================================
%% get_function/2 Tests
%%====================================================================

get_function_found_test() ->
    Funcs = [
        beamai_function:new(<<"add">>, fun(_) -> {ok, 0} end),
        beamai_function:new(<<"sub">>, fun(_) -> {ok, 0} end)
    ],
    Plugin = beamai_plugin:new(<<"math">>, Funcs),
    {ok, F} = beamai_plugin:get_function(Plugin, <<"add">>),
    ?assertEqual(<<"add">>, maps:get(name, F)).

get_function_not_found_test() ->
    Funcs = [beamai_function:new(<<"add">>, fun(_) -> {ok, 0} end)],
    Plugin = beamai_plugin:new(<<"math">>, Funcs),
    ?assertEqual(error, beamai_plugin:get_function(Plugin, <<"multiply">>)).

%%====================================================================
%% list_functions/1 Tests
%%====================================================================

list_functions_test() ->
    Funcs = [
        beamai_function:new(<<"a">>, fun(_) -> {ok, 0} end),
        beamai_function:new(<<"b">>, fun(_) -> {ok, 0} end),
        beamai_function:new(<<"c">>, fun(_) -> {ok, 0} end)
    ],
    Plugin = beamai_plugin:new(<<"test">>, Funcs),
    ?assertEqual(3, length(beamai_plugin:list_functions(Plugin))).

%%====================================================================
%% to_tool_schemas/1,2 Tests
%%====================================================================

to_tool_schemas_openai_test() ->
    Funcs = [
        beamai_function:new(<<"search">>, fun(_) -> {ok, []} end, #{
            description => <<"Search">>,
            parameters => #{query => #{type => string, required => true}}
        }),
        beamai_function:new(<<"fetch">>, fun(_) -> {ok, <<>>} end, #{
            description => <<"Fetch URL">>
        })
    ],
    Plugin = beamai_plugin:new(<<"web">>, Funcs),
    Schemas = beamai_plugin:to_tool_schemas(Plugin, openai),
    ?assertEqual(2, length(Schemas)),
    [S1 | _] = Schemas,
    ?assertEqual(<<"function">>, maps:get(<<"type">>, S1)).

to_tool_schemas_anthropic_test() ->
    Funcs = [
        beamai_function:new(<<"search">>, fun(_) -> {ok, []} end, #{
            description => <<"Search">>
        })
    ],
    Plugin = beamai_plugin:new(<<"web">>, Funcs),
    Schemas = beamai_plugin:to_tool_schemas(Plugin, anthropic),
    [S1 | _] = Schemas,
    ?assert(maps:is_key(<<"name">>, S1)),
    ?assert(maps:is_key(<<"input_schema">>, S1)).

%%====================================================================
%% from_module/1 Tests
%%====================================================================

from_module_not_found_test() ->
    ?assertMatch({error, {module_not_found, _}},
                 beamai_plugin:from_module(non_existent_module_xyz)).

%%====================================================================
%% get_name/1 Tests
%%====================================================================

get_name_test() ->
    Plugin = beamai_plugin:new(<<"math">>, []),
    ?assertEqual(<<"math">>, beamai_plugin:get_name(Plugin)).
