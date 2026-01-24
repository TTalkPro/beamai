-module(beamai_function_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% new/2 Tests
%%====================================================================

new_basic_test() ->
    F = beamai_function:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end),
    ?assertEqual(<<"add">>, maps:get(name, F)),
    ?assert(is_function(maps:get(handler, F), 1)).

new_with_opts_test() ->
    F = beamai_function:new(<<"search">>, fun(_) -> {ok, []} end, #{
        description => <<"Search the web">>,
        parameters => #{
            query => #{type => string, required => true}
        }
    }),
    ?assertEqual(<<"search">>, maps:get(name, F)),
    ?assertEqual(<<"Search the web">>, maps:get(description, F)).

%%====================================================================
%% validate/1 Tests
%%====================================================================

validate_valid_fun1_test() ->
    F = beamai_function:new(<<"test">>, fun(_) -> {ok, done} end),
    ?assertEqual(ok, beamai_function:validate(F)).

validate_valid_fun2_test() ->
    F = beamai_function:new(<<"test">>, fun(_, _) -> {ok, done} end),
    ?assertEqual(ok, beamai_function:validate(F)).

validate_valid_mfa_test() ->
    F = beamai_function:new(<<"test">>, {erlang, is_atom}),
    ?assertEqual(ok, beamai_function:validate(F)).

validate_valid_mfa_extra_test() ->
    F = beamai_function:new(<<"test">>, {erlang, apply, []}),
    ?assertEqual(ok, beamai_function:validate(F)).

validate_missing_fields_test() ->
    ?assertMatch({error, _}, beamai_function:validate(#{})).

validate_invalid_name_test() ->
    F = #{name => <<>>, handler => fun(_) -> ok end},
    ?assertMatch({error, _}, beamai_function:validate(F)).

validate_invalid_handler_test() ->
    F = #{name => <<"test">>, handler => not_a_handler},
    ?assertMatch({error, _}, beamai_function:validate(F)).

%%====================================================================
%% invoke/2,3 Tests
%%====================================================================

invoke_fun1_test() ->
    F = beamai_function:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end),
    ?assertEqual({ok, 15}, beamai_function:invoke(F, #{a => 7, b => 8})).

invoke_fun2_test() ->
    F = beamai_function:new(<<"greet">>,
        fun(#{name := Name}, Ctx) ->
            Lang = beamai_context:get(Ctx, <<"lang">>, <<"en">>),
            case Lang of
                <<"en">> -> {ok, <<"Hello, ", Name/binary>>};
                <<"fr">> -> {ok, <<"Bonjour, ", Name/binary>>}
            end
        end),
    Ctx = beamai_context:set(beamai_context:new(), <<"lang">>, <<"fr">>),
    ?assertEqual({ok, <<"Bonjour, World">>}, beamai_function:invoke(F, #{name => <<"World">>}, Ctx)).

invoke_error_test() ->
    F = beamai_function:new(<<"fail">>, fun(_) -> {error, something_wrong} end),
    ?assertEqual({error, something_wrong}, beamai_function:invoke(F, #{})).

invoke_exception_test() ->
    F = beamai_function:new(<<"crash">>, fun(_) -> error(boom) end),
    ?assertMatch({error, #{class := error, reason := boom}},
                 beamai_function:invoke(F, #{})).

invoke_with_context_update_test() ->
    F = beamai_function:new(<<"counter">>,
        fun(_, Ctx) ->
            N = beamai_context:get(Ctx, <<"count">>, 0),
            NewCtx = beamai_context:set(Ctx, <<"count">>, N + 1),
            {ok, N + 1, NewCtx}
        end),
    ?assertMatch({ok, 1, _}, beamai_function:invoke(F, #{}, beamai_context:new())).

%%====================================================================
%% to_tool_spec/1 Tests
%%====================================================================

to_tool_spec_test() ->
    F = beamai_function:new(<<"search">>, fun(_) -> {ok, []} end, #{
        description => <<"Search the web">>,
        parameters => #{
            query => #{type => string, required => true, description => <<"Search query">>},
            limit => #{type => integer, default => 10}
        }
    }),
    Spec = beamai_function:to_tool_spec(F),
    ?assertEqual(<<"search">>, maps:get(name, Spec)),
    ?assertEqual(<<"Search the web">>, maps:get(description, Spec)),
    Params = maps:get(parameters, Spec),
    ?assertEqual(object, maps:get(type, Params)),
    Props = maps:get(properties, Params),
    ?assert(maps:is_key(<<"query">>, Props)),
    ?assert(maps:is_key(<<"limit">>, Props)),
    ?assertEqual([<<"query">>], maps:get(required, Params)).

to_tool_spec_with_plugin_test() ->
    F = beamai_function:new(<<"search">>, fun(_) -> {ok, []} end, #{
        plugin => <<"web">>,
        description => <<"Search">>
    }),
    Spec = beamai_function:to_tool_spec(F),
    ?assertEqual(<<"web.search">>, maps:get(name, Spec)).

%%====================================================================
%% to_tool_schema/1,2 Tests
%%====================================================================

to_tool_schema_openai_test() ->
    F = beamai_function:new(<<"search">>, fun(_) -> {ok, []} end, #{
        description => <<"Search the web">>,
        parameters => #{
            query => #{type => string, required => true}
        }
    }),
    Schema = beamai_function:to_tool_schema(F, openai),
    ?assertEqual(<<"function">>, maps:get(<<"type">>, Schema)),
    Func = maps:get(<<"function">>, Schema),
    ?assertEqual(<<"search">>, maps:get(<<"name">>, Func)),
    ?assertEqual(<<"Search the web">>, maps:get(<<"description">>, Func)).

to_tool_schema_anthropic_test() ->
    F = beamai_function:new(<<"search">>, fun(_) -> {ok, []} end, #{
        description => <<"Search">>,
        parameters => #{
            query => #{type => string, required => true}
        }
    }),
    Schema = beamai_function:to_tool_schema(F, anthropic),
    ?assertEqual(<<"search">>, maps:get(<<"name">>, Schema)),
    ?assertEqual(<<"Search">>, maps:get(<<"description">>, Schema)),
    ?assert(maps:is_key(<<"input_schema">>, Schema)).

%%====================================================================
%% get_name/1, get_full_name/1 Tests
%%====================================================================

get_name_test() ->
    F = beamai_function:new(<<"add">>, fun(_) -> {ok, 0} end),
    ?assertEqual(<<"add">>, beamai_function:get_name(F)).

get_full_name_with_plugin_test() ->
    F = beamai_function:new(<<"add">>, fun(_) -> {ok, 0} end, #{plugin => <<"math">>}),
    ?assertEqual(<<"math.add">>, beamai_function:get_full_name(F)).

get_full_name_without_plugin_test() ->
    F = beamai_function:new(<<"add">>, fun(_) -> {ok, 0} end),
    ?assertEqual(<<"add">>, beamai_function:get_full_name(F)).
