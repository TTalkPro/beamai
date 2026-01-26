-module(beamai_kernel_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup
%%====================================================================

make_math_kernel() ->
    K0 = beamai_kernel:new(),
    beamai_kernel:add_tools(K0, [
        beamai_tool:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end, #{
            description => <<"Add two numbers">>,
            tag => <<"math">>,
            parameters => #{
                a => #{type => integer, required => true},
                b => #{type => integer, required => true}
            }
        }),
        beamai_tool:new(<<"multiply">>, fun(#{a := A, b := B}) -> {ok, A * B} end, #{
            description => <<"Multiply two numbers">>,
            tag => <<"math">>,
            parameters => #{
                a => #{type => integer, required => true},
                b => #{type => integer, required => true}
            }
        })
    ]).

make_multi_tag_kernel() ->
    K0 = make_math_kernel(),
    beamai_kernel:add_tools(K0, [
        beamai_tool:new(<<"upper">>, fun(#{text := T}) -> {ok, string:uppercase(T)} end, #{
            description => <<"Convert to uppercase">>,
            tag => <<"string">>,
            parameters => #{text => #{type => string, required => true}}
        }),
        beamai_tool:new(<<"length">>, fun(#{text := T}) -> {ok, byte_size(T)} end, #{
            description => <<"Get string length">>,
            tag => <<"string">>,
            parameters => #{text => #{type => string, required => true}}
        })
    ]).

%%====================================================================
%% new/0,1 Tests
%%====================================================================

new_default_test() ->
    K = beamai_kernel:new(),
    ?assertEqual(true, maps:get('__kernel__', K)),
    ?assertEqual(#{}, maps:get(tools, K)),
    ?assertEqual(undefined, maps:get(llm_config, K)),
    ?assertEqual([], maps:get(filters, K)).

new_with_settings_test() ->
    K = beamai_kernel:new(#{max_tool_iterations => 5}),
    ?assertEqual(#{max_tool_iterations => 5}, maps:get(settings, K)).

%%====================================================================
%% add_tool Tests
%%====================================================================

add_tool_test() ->
    K0 = beamai_kernel:new(),
    Tool = beamai_tool:new(<<"add">>, fun(_) -> {ok, 0} end),
    K1 = beamai_kernel:add_tool(K0, Tool),
    ?assert(maps:is_key(<<"add">>, maps:get(tools, K1))).

add_tools_test() ->
    K = make_math_kernel(),
    ?assert(maps:is_key(<<"add">>, maps:get(tools, K))),
    ?assert(maps:is_key(<<"multiply">>, maps:get(tools, K))).

%%====================================================================
%% invoke_tool/4 Tests
%%====================================================================

invoke_add_test() ->
    K = make_math_kernel(),
    ?assertMatch({ok, 15, _}, beamai_kernel:invoke_tool(K, <<"add">>, #{a => 7, b => 8}, beamai_context:new())).

invoke_multiply_test() ->
    K = make_math_kernel(),
    ?assertMatch({ok, 42, _}, beamai_kernel:invoke_tool(K, <<"multiply">>, #{a => 6, b => 7}, beamai_context:new())).

invoke_not_found_test() ->
    K = make_math_kernel(),
    ?assertEqual({error, {tool_not_found, <<"nonexistent">>}},
                 beamai_kernel:invoke_tool(K, <<"nonexistent">>, #{}, beamai_context:new())).

invoke_with_context_test() ->
    K = beamai_kernel:add_tool(beamai_kernel:new(),
        beamai_tool:new(<<"get_var">>,
            fun(#{key := Key}, Ctx) ->
                {ok, beamai_context:get(Ctx, Key)}
            end)),
    Ctx = beamai_context:set(beamai_context:new(), <<"name">>, <<"Alice">>),
    ?assertMatch({ok, <<"Alice">>, _},
                 beamai_kernel:invoke_tool(K, <<"get_var">>, #{key => <<"name">>}, Ctx)).

invoke_multi_tag_test() ->
    K = make_multi_tag_kernel(),
    ?assertMatch({ok, 15, _}, beamai_kernel:invoke_tool(K, <<"add">>, #{a => 7, b => 8}, beamai_context:new())),
    ?assertMatch({ok, <<"HELLO">>, _}, beamai_kernel:invoke_tool(K, <<"upper">>, #{text => <<"hello">>}, beamai_context:new())).

%%====================================================================
%% get_tool/2 Tests
%%====================================================================

get_tool_add_test() ->
    K = make_math_kernel(),
    {ok, T} = beamai_kernel:get_tool(K, <<"add">>),
    ?assertEqual(<<"add">>, maps:get(name, T)).

get_tool_multiply_test() ->
    K = make_math_kernel(),
    {ok, T} = beamai_kernel:get_tool(K, <<"multiply">>),
    ?assertEqual(<<"multiply">>, maps:get(name, T)).

get_tool_not_found_test() ->
    K = make_math_kernel(),
    ?assertEqual(error, beamai_kernel:get_tool(K, <<"nonexistent">>)).

%%====================================================================
%% list_tools/1 Tests
%%====================================================================

list_tools_test() ->
    K = make_multi_tag_kernel(),
    Tools = beamai_kernel:list_tools(K),
    ?assertEqual(4, length(Tools)).

%%====================================================================
%% get_tool_schemas Tests
%%====================================================================

get_tool_schemas_openai_test() ->
    K = make_math_kernel(),
    Schemas = beamai_kernel:get_tool_schemas(K, openai),
    ?assertEqual(2, length(Schemas)),
    [S1 | _] = Schemas,
    ?assertEqual(<<"function">>, maps:get(<<"type">>, S1)).

get_tool_schemas_anthropic_test() ->
    K = make_math_kernel(),
    Schemas = beamai_kernel:get_tool_schemas(K, anthropic),
    ?assertEqual(2, length(Schemas)),
    [S1 | _] = Schemas,
    ?assert(maps:is_key(<<"name">>, S1)),
    ?assert(maps:is_key(<<"input_schema">>, S1)).

get_tool_specs_test() ->
    K = make_math_kernel(),
    Specs = beamai_kernel:get_tool_specs(K),
    ?assertEqual(2, length(Specs)),
    [S1 | _] = Specs,
    %% Unified format: atom keys
    ?assert(maps:is_key(name, S1)),
    ?assert(maps:is_key(description, S1)),
    ?assert(maps:is_key(parameters, S1)).

%%====================================================================
%% Service Tests
%%====================================================================

add_service_test() ->
    K0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{model => <<"test">>}),
    K1 = beamai_kernel:add_service(K0, LlmConfig),
    {ok, Svc} = beamai_kernel:get_service(K1),
    ?assertEqual(mock, maps:get(provider, Svc)).

get_service_not_found_test() ->
    K = beamai_kernel:new(),
    ?assertEqual(error, beamai_kernel:get_service(K)).

no_llm_service_chat_test() ->
    K = make_math_kernel(),
    ?assertEqual({error, no_llm_service},
                 beamai_kernel:invoke_chat(K, [#{role => user, content => <<"hi">>}], #{})).

%%====================================================================
%% Filter Integration Tests
%%====================================================================

invoke_with_pre_filter_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_tool(K0,
        beamai_tool:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end)
    ),
    Filter = beamai_filter:new(<<"doubler">>, pre_invocation,
        fun(#{args := Args} = Ctx) ->
            NewArgs = maps:map(fun(_K, V) when is_number(V) -> V * 2;
                                 (_K, V) -> V end, Args),
            {continue, Ctx#{args => NewArgs}}
        end),
    K2 = beamai_kernel:add_filter(K1, Filter),
    ?assertMatch({ok, 30, _}, beamai_kernel:invoke_tool(K2, <<"add">>, #{a => 7, b => 8}, beamai_context:new())).

invoke_with_post_filter_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_tool(K0,
        beamai_tool:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end)
    ),
    Filter = beamai_filter:new(<<"result_doubler">>, post_invocation,
        fun(#{result := R} = Ctx) ->
            {continue, Ctx#{result => R * 2}}
        end),
    K2 = beamai_kernel:add_filter(K1, Filter),
    ?assertMatch({ok, 30, _}, beamai_kernel:invoke_tool(K2, <<"add">>, #{a => 7, b => 8}, beamai_context:new())).

invoke_with_skip_filter_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_tool(K0,
        beamai_tool:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end)
    ),
    Filter = beamai_filter:new(<<"cache">>, pre_invocation,
        fun(_Ctx) ->
            {skip, cached_result}
        end),
    K2 = beamai_kernel:add_filter(K1, Filter),
    ?assertMatch({ok, cached_result, _}, beamai_kernel:invoke_tool(K2, <<"add">>, #{a => 7, b => 8}, beamai_context:new())).

%%====================================================================
%% Facade API Tests
%%====================================================================

facade_kernel_test() ->
    K = beamai:kernel(),
    ?assertEqual(true, maps:get('__kernel__', K)).

facade_add_tool_test() ->
    K0 = beamai:kernel(),
    K1 = beamai:add_tool(K0,
        beamai:tool(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end, #{
            description => <<"Add">>,
            parameters => #{
                a => #{type => integer, required => true},
                b => #{type => integer, required => true}
            }
        })
    ),
    ?assertMatch({ok, 15, _}, beamai:invoke_tool(K1, <<"add">>, #{a => 7, b => 8}, beamai_context:new())).

facade_add_llm_test() ->
    K0 = beamai:kernel(),
    K1 = beamai:add_llm(K0, mock, #{model => <<"test-model">>}),
    {ok, Config} = beamai_kernel:get_service(K1),
    ?assertEqual(mock, maps:get(provider, Config)).

facade_tools_test() ->
    K = make_math_kernel(),
    Tools = beamai:tools(K),
    ?assertEqual(2, length(Tools)).

facade_tools_by_tag_test() ->
    K = make_multi_tag_kernel(),
    MathTools = beamai:tools_by_tag(K, <<"math">>),
    StringTools = beamai:tools_by_tag(K, <<"string">>),
    ?assertEqual(2, length(MathTools)),
    ?assertEqual(2, length(StringTools)).

facade_context_test() ->
    Ctx = beamai:context(#{<<"key">> => <<"value">>}),
    ?assertEqual(<<"value">>, beamai_context:get(Ctx, <<"key">>)).

facade_render_test() ->
    {ok, Result} = beamai:render(<<"Hello, {{name}}!">>, #{<<"name">> => <<"World">>}),
    ?assertEqual(<<"Hello, World!">>, Result).

%%====================================================================
%% Tool Calling Loop (mock) Tests
%%====================================================================

-define(MOCK_MODULE, beamai_mock_llm_provider).

mock_tool_calling_test_() ->
    {setup,
     fun setup_mock/0,
     fun cleanup_mock/1,
     fun(_) ->
         [fun mock_tool_call_loop/0]
     end
    }.

setup_mock() ->
    meck:new(?MOCK_MODULE, [non_strict]),
    CallCount = atomics:new(1, [{signed, false}]),
    meck:expect(?MOCK_MODULE, name, fun() -> <<"mock">> end),
    meck:expect(?MOCK_MODULE, default_config, fun() -> #{} end),
    meck:expect(?MOCK_MODULE, validate_config, fun(_) -> ok end),
    meck:expect(?MOCK_MODULE, supports_tools, fun() -> true end),
    meck:expect(?MOCK_MODULE, supports_streaming, fun() -> false end),
    meck:expect(?MOCK_MODULE, chat, fun(_Config, Request) ->
        Count = atomics:add_get(CallCount, 1, 1),
        Messages = maps:get(messages, Request, []),
        case Count of
            1 ->
                %% First call: return tool call
                {ok, #{
                    id => <<"resp_1">>,
                    model => <<"mock">>,
                    content => null,
                    finish_reason => <<"tool_calls">>,
                    usage => #{},
                    tool_calls => [#{
                        id => <<"call_1">>,
                        type => function,
                        function => #{
                            name => <<"add">>,
                            arguments => <<"{\"a\":7,\"b\":8}">>
                        }
                    }]
                }};
            _ ->
                %% Subsequent calls: check for tool results
                HasToolResult = lists:any(
                    fun(#{role := R}) -> R =:= tool;
                       (_) -> false
                    end, Messages),
                case HasToolResult of
                    true ->
                        {ok, #{
                            id => <<"resp_2">>,
                            model => <<"mock">>,
                            content => <<"The answer is 15">>,
                            finish_reason => <<"stop">>,
                            usage => #{}
                        }};
                    false ->
                        {ok, #{
                            id => <<"resp_2">>,
                            model => <<"mock">>,
                            content => <<"I need to calculate">>,
                            finish_reason => <<"stop">>,
                            usage => #{}
                        }}
                end
        end
    end),
    CallCount.

cleanup_mock(_) ->
    meck:unload(?MOCK_MODULE).

mock_tool_call_loop() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_tool(K0,
        beamai_tool:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end, #{
            parameters => #{
                a => #{type => integer, required => true},
                b => #{type => integer, required => true}
            }
        })
    ),
    LlmConfig = beamai_chat_completion:create({custom, ?MOCK_MODULE}, #{model => <<"mock">>}),
    K2 = beamai_kernel:add_service(K1, LlmConfig),
    Messages = [#{role => user, content => <<"What is 7 + 8?">>}],
    {ok, Response, _Ctx} = beamai_kernel:invoke(K2, Messages, #{}),
    ?assertEqual(<<"The answer is 15">>, maps:get(content, Response)).
