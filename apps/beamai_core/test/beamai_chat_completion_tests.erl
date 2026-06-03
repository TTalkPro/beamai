-module(beamai_chat_completion_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tool Schema Generation Tests
%%====================================================================

tool_schema_full_test() ->
    K = beamai_kernel:new(),
    K1 = beamai_kernel:add_tools(K, [
        beamai_tool:new(<<"get_weather">>,
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
    ?assertEqual(<<"get_weather">>, maps:get(<<"name">>, Func)),
    Params = maps:get(<<"parameters">>, Func),
    Props = maps:get(properties, Params),
    ?assert(maps:is_key(<<"location">>, Props)).

tool_schema_anthropic_format_test() ->
    K = beamai_kernel:new(),
    K1 = beamai_kernel:add_tools(K, [
        beamai_tool:new(<<"search">>,
            fun(_) -> {ok, []} end,
            #{
                description => <<"Search the web">>,
                parameters => #{
                    query => #{type => string, required => true}
                }
            })
    ]),
    [Schema] = beamai_kernel:get_tool_schemas(K1, anthropic),
    ?assertEqual(<<"search">>, maps:get(<<"name">>, Schema)),
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

%% context 不再记录消息/历史，改由 Memory Filter 按 conversation_id 管理
context_conversation_id_test() ->
    Ctx0 = beamai_context:new(),
    ?assertEqual(undefined, beamai_context:conversation_id(Ctx0)),
    Ctx1 = beamai_context:with_conversation_id(Ctx0, <<"conv-1">>),
    ?assertEqual(<<"conv-1">>, beamai_context:conversation_id(Ctx1)).

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
