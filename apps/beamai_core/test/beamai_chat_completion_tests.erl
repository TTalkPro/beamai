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
    ?assertEqual(<<"function">>, maps:get(<<"type">>, Schema)),
    Func = maps:get(<<"function">>, Schema),
    ?assertEqual(<<"tools.get_weather">>, maps:get(<<"name">>, Func)),
    Params = maps:get(<<"parameters">>, Func),
    Props = maps:get(properties, Params),
    ?assert(maps:is_key(<<"location">>, Props)).

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
    ?assertEqual(<<"tools.search">>, maps:get(<<"name">>, Schema)),
    ?assertEqual(<<"Search the web">>, maps:get(<<"description">>, Schema)),
    ?assert(maps:is_key(<<"input_schema">>, Schema)).

%%====================================================================
%% Service Config Tests
%%====================================================================

llm_service_config_test() ->
    K0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{model => <<"test-model">>}),
    K1 = beamai_kernel:add_service(K0, LlmConfig),
    {ok, Svc} = beamai_kernel:get_service(K1),
    ?assertEqual(mock, maps:get(provider, Svc)),
    ?assertEqual(<<"test-model">>, maps:get(model, Svc)).

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
    Ctx1 = beamai_context:add_history(Ctx0, Msg),
    ?assertEqual([Msg], beamai_context:get_history(Ctx1)).

context_messages_test() ->
    Ctx0 = beamai_context:new(),
    Msg1 = #{role => system, content => <<"You are a helper">>},
    Msg2 = #{role => user, content => <<"Hello">>},
    Ctx1 = beamai_context:append_message(Ctx0, Msg1),
    Ctx2 = beamai_context:append_message(Ctx1, Msg2),
    ?assertEqual([Msg1, Msg2], beamai_context:get_messages(Ctx2)),
    %% set_messages 可以替换（用于 summarize 后重置）
    Summary = #{role => system, content => <<"Summary of conversation">>},
    Ctx3 = beamai_context:set_messages(Ctx2, [Summary]),
    ?assertEqual([Summary], beamai_context:get_messages(Ctx3)).

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
    ?assertEqual(42, beamai_result:unwrap(R)).

result_error_test() ->
    R = beamai_result:error(oops),
    ?assertEqual({error, oops}, R),
    ?assertNot(beamai_result:is_ok(R)),
    ?assert(beamai_result:is_error(R)),
    ?assertEqual(oops, beamai_result:unwrap_error(R)).

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

result_unwrap_or_test() ->
    ?assertEqual(42, beamai_result:unwrap_or({ok, 42}, 0)),
    ?assertEqual(0, beamai_result:unwrap_or({error, oops}, 0)).
