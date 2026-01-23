-module(beamai_chat_completion_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tool Schema Generation Tests
%%====================================================================

tool_schema_full_test() ->
    K = beamai_kernel:new(),
    K1 = beamai_kernel:add_plugin(K, <<"tools">>, [
        beamai_function:new(<<"get_weather">>,
            fun(#{location := L}) -> {ok, #{location => L, temp => 22}} end,
            #{
                description => <<"Get current weather for a location">>,
                parameters => #{
                    location => #{type => string, required => true, description => <<"City name">>},
                    unit => #{type => string, enum => [<<"celsius">>, <<"fahrenheit">>], default => <<"celsius">>}
                }
            })
    ]),
    [Schema] = beamai_kernel:get_tool_schemas(K1, openai),
    ?assertEqual(<<"function">>, maps:get(type, Schema)),
    Func = maps:get(function, Schema),
    ?assertEqual(<<"tools.get_weather">>, maps:get(name, Func)),
    Params = maps:get(parameters, Func),
    Props = maps:get(properties, Params),
    ?assert(maps:is_key(<<"location">>, Props)),
    ?assert(maps:is_key(<<"unit">>, Props)),
    LocSpec = maps:get(<<"location">>, Props),
    ?assertEqual(<<"string">>, maps:get(type, LocSpec)),
    ?assertEqual(<<"City name">>, maps:get(description, LocSpec)),
    UnitSpec = maps:get(<<"unit">>, Props),
    ?assertEqual([<<"celsius">>, <<"fahrenheit">>], maps:get(enum, UnitSpec)).

tool_schema_anthropic_format_test() ->
    K = beamai_kernel:new(),
    K1 = beamai_kernel:add_plugin(K, <<"tools">>, [
        beamai_function:new(<<"search">>,
            fun(_) -> {ok, []} end,
            #{
                description => <<"Search the web">>,
                parameters => #{
                    query => #{type => string, required => true}
                }
            })
    ]),
    [Schema] = beamai_kernel:get_tool_schemas(K1, anthropic),
    ?assertEqual(<<"tools.search">>, maps:get(name, Schema)),
    ?assertEqual(<<"Search the web">>, maps:get(description, Schema)),
    InputSchema = maps:get(input_schema, Schema),
    ?assertEqual(<<"object">>, maps:get(type, InputSchema)).

%%====================================================================
%% Chat Completion Service Config Tests
%%====================================================================

chat_service_config_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_chat_completion(K0, #{
        connector => beamai_connector_openai,
        model => <<"gpt-4">>,
        api_key => <<"sk-test">>
    }),
    {ok, Svc} = beamai_kernel:get_service(K1, chat_completion),
    ?assertEqual(beamai_connector_openai, maps:get(connector, Svc)),
    Config = maps:get(config, Svc),
    ?assertEqual(<<"gpt-4">>, maps:get(model, Config)),
    ?assertEqual(<<"sk-test">>, maps:get(api_key, Config)).

%%====================================================================
%% Prompt Template Tests
%%====================================================================

prompt_simple_test() ->
    Template = beamai_prompt:new(<<"Hello, {{name}}!">>),
    {ok, Result} = beamai_prompt:render(Template, #{<<"name">> => <<"World">>}),
    ?assertEqual(<<"Hello, World!">>, Result).

prompt_multiple_vars_test() ->
    Template = beamai_prompt:new(<<"{{greeting}}, {{name}}! You are {{age}} years old.">>),
    Vars = #{<<"greeting">> => <<"Hi">>, <<"name">> => <<"Alice">>, <<"age">> => <<"30">>},
    {ok, Result} = beamai_prompt:render(Template, Vars),
    ?assertEqual(<<"Hi, Alice! You are 30 years old.">>, Result).

prompt_with_context_test() ->
    Template = beamai_prompt:new(<<"User: {{question}}">>),
    Ctx = beamai_context:new(#{<<"question">> => <<"What is AI?">>}),
    {ok, Result} = beamai_prompt:render(Template, Ctx),
    ?assertEqual(<<"User: What is AI?">>, Result).

prompt_get_variables_test() ->
    Template = beamai_prompt:new(<<"{{a}} and {{b}} and {{c}}">>),
    Vars = beamai_prompt:get_variables(Template),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], lists:sort(Vars)).

prompt_missing_variable_test() ->
    %% Missing variables remain as-is (no error)
    Template = beamai_prompt:new(<<"Hello, {{name}}!">>),
    {ok, Result} = beamai_prompt:render(Template, #{}),
    ?assertEqual(<<"Hello, {{name}}!">>, Result).

%%====================================================================
%% Context Tests
%%====================================================================

context_basic_test() ->
    Ctx = beamai_context:new(),
    ?assertEqual(undefined, beamai_context:get(Ctx, <<"key">>)).

context_set_get_test() ->
    Ctx0 = beamai_context:new(),
    Ctx1 = beamai_context:set(Ctx0, <<"name">>, <<"Alice">>),
    ?assertEqual(<<"Alice">>, beamai_context:get(Ctx1, <<"name">>)).

context_default_test() ->
    Ctx = beamai_context:new(),
    ?assertEqual(42, beamai_context:get(Ctx, <<"missing">>, 42)).

context_set_many_test() ->
    Ctx0 = beamai_context:new(),
    Ctx1 = beamai_context:set_many(Ctx0, #{<<"a">> => 1, <<"b">> => 2}),
    ?assertEqual(1, beamai_context:get(Ctx1, <<"a">>)),
    ?assertEqual(2, beamai_context:get(Ctx1, <<"b">>)).

context_history_test() ->
    Ctx0 = beamai_context:new(),
    Msg = #{role => user, content => <<"Hello">>},
    Ctx1 = beamai_context:add_message(Ctx0, Msg),
    ?assertEqual([Msg], beamai_context:get_history(Ctx1)).

context_kernel_test() ->
    Ctx0 = beamai_context:new(),
    K = beamai_kernel:new(),
    Ctx1 = beamai_context:with_kernel(Ctx0, K),
    ?assertEqual(K, beamai_context:get_kernel(Ctx1)).

%%====================================================================
%% Result Tests
%%====================================================================

result_ok_test() ->
    R = beamai_result:ok(42),
    ?assertEqual({ok, 42}, R),
    ?assert(beamai_result:is_ok(R)),
    ?assertNot(beamai_result:is_error(R)),
    ?assertEqual(42, beamai_result:get_value(R)).

result_error_test() ->
    R = beamai_result:error(oops),
    ?assertEqual({error, oops}, R),
    ?assertNot(beamai_result:is_ok(R)),
    ?assert(beamai_result:is_error(R)),
    ?assertEqual(oops, beamai_result:get_error(R)).

result_map_test() ->
    R = beamai_result:ok(21),
    R2 = beamai_result:map(R, fun(V) -> V * 2 end),
    ?assertEqual({ok, 42}, R2).

result_map_error_test() ->
    R = beamai_result:error(oops),
    R2 = beamai_result:map(R, fun(V) -> V * 2 end),
    ?assertEqual({error, oops}, R2).

result_flat_map_test() ->
    R = beamai_result:ok(21),
    R2 = beamai_result:flat_map(R, fun(V) -> {ok, V * 2} end),
    ?assertEqual({ok, 42}, R2).

result_to_json_test() ->
    Json = beamai_result:to_json({ok, <<"hello">>}),
    ?assertEqual(<<"ok">>, maps:get(status, Json)),
    ?assertEqual(<<"hello">>, maps:get(value, Json)).

%%====================================================================
%% invoke_chat_with_tools (mock) Tests
%%====================================================================

%% This test verifies the tool calling loop logic without a real LLM.
%% It uses a mock connector module.

-define(MOCK_CONNECTOR, beamai_mock_connector).

mock_tool_calling_test_() ->
    {setup,
     fun setup_mock/0,
     fun cleanup_mock/1,
     fun(_) ->
         [fun mock_tool_call_loop/0]
     end
    }.

setup_mock() ->
    %% Create a mock module using meck
    meck:new(?MOCK_CONNECTOR, [non_strict]),
    CallCount = atomics:new(1, [{signed, false}]),
    meck:expect(?MOCK_CONNECTOR, chat, fun(_Config, Messages, _Opts) ->
        Count = atomics:add_get(CallCount, 1, 1),
        case Count of
            1 ->
                %% First call: return tool call
                {ok, #{
                    role => assistant,
                    content => null,
                    finish_reason => tool_calls,
                    usage => #{},
                    tool_calls => [#{
                        id => <<"call_1">>,
                        type => function,
                        function => #{
                            name => <<"math.add">>,
                            arguments => <<"{\"a\":7,\"b\":8}">>
                        }
                    }]
                }};
            _ ->
                %% Second call: has tool results, return final answer
                HasToolResult = lists:any(fun(#{role := R}) -> R =:= tool; (_) -> false end, Messages),
                case HasToolResult of
                    true ->
                        {ok, #{
                            role => assistant,
                            content => <<"The answer is 15">>,
                            finish_reason => stop,
                            usage => #{}
                        }};
                    false ->
                        {ok, #{
                            role => assistant,
                            content => <<"I need to calculate">>,
                            finish_reason => stop,
                            usage => #{}
                        }}
                end
        end
    end),
    CallCount.

cleanup_mock(_) ->
    meck:unload(?MOCK_CONNECTOR).

mock_tool_call_loop() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_plugin(K0, <<"math">>, [
        beamai_function:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end, #{
            parameters => #{
                a => #{type => integer, required => true},
                b => #{type => integer, required => true}
            }
        })
    ]),
    K2 = beamai_kernel:add_chat_completion(K1, #{
        connector => ?MOCK_CONNECTOR,
        model => <<"test-model">>
    }),
    Messages = [#{role => user, content => <<"What is 7 + 8?">>}],
    {ok, Response} = beamai_kernel:invoke_chat_with_tools(K2, Messages, #{}),
    ?assertEqual(<<"The answer is 15">>, maps:get(content, Response)).
