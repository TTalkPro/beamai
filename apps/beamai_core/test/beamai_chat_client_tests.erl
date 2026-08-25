-module(beamai_chat_client_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup
%%====================================================================

make_math_chat_client() ->
    K0 = beamai_chat_client:new(),
    beamai_chat_client:add_tools(K0, [
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

make_multi_tag_chat_client() ->
    K0 = make_math_chat_client(),
    beamai_chat_client:add_tools(K0, [
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
    K = beamai_chat_client:new(),
    ?assertEqual(true, maps:get('__chat_client__', K)),
    ?assertEqual(#{}, maps:get(tools, K)),
    ?assertEqual(undefined, maps:get(chat_model, K)),
    ?assertEqual([], maps:get(filters, K)).

new_with_settings_test() ->
    K = beamai_chat_client:new(#{max_tool_iterations => 5}),
    ?assertEqual(#{max_tool_iterations => 5}, maps:get(settings, K)).

%%====================================================================
%% add_tool Tests
%%====================================================================

add_tool_test() ->
    K0 = beamai_chat_client:new(),
    Tool = beamai_tool:new(<<"add">>, fun(_) -> {ok, 0} end),
    K1 = beamai_chat_client:add_tool(K0, Tool),
    ?assert(maps:is_key(<<"add">>, maps:get(tools, K1))).

add_tools_test() ->
    K = make_math_chat_client(),
    ?assert(maps:is_key(<<"add">>, maps:get(tools, K))),
    ?assert(maps:is_key(<<"multiply">>, maps:get(tools, K))).

%%====================================================================
%% invoke_tool/4 Tests
%%====================================================================

invoke_add_test() ->
    K = make_math_chat_client(),
    ?assertMatch({ok, 15, _}, beamai_tool_executor:invoke(K, <<"add">>, #{a => 7, b => 8}, beamai_context:new())).

invoke_multiply_test() ->
    K = make_math_chat_client(),
    ?assertMatch({ok, 42, _}, beamai_tool_executor:invoke(K, <<"multiply">>, #{a => 6, b => 7}, beamai_context:new())).

invoke_not_found_test() ->
    K = make_math_chat_client(),
    ?assertEqual({error, {tool_not_found, <<"nonexistent">>}},
                 beamai_tool_executor:invoke(K, <<"nonexistent">>, #{}, beamai_context:new())).

invoke_with_context_test() ->
    K = beamai_chat_client:add_tool(beamai_chat_client:new(),
        beamai_tool:new(<<"get_var">>,
            fun(#{key := Key}, Ctx) ->
                {ok, beamai_context:get(Ctx, Key)}
            end)),
    Ctx = beamai_context:set(beamai_context:new(), <<"name">>, <<"Alice">>),
    ?assertMatch({ok, <<"Alice">>, _},
                 beamai_tool_executor:invoke(K, <<"get_var">>, #{key => <<"name">>}, Ctx)).

invoke_multi_tag_test() ->
    K = make_multi_tag_chat_client(),
    ?assertMatch({ok, 15, _}, beamai_tool_executor:invoke(K, <<"add">>, #{a => 7, b => 8}, beamai_context:new())),
    ?assertMatch({ok, <<"HELLO">>, _}, beamai_tool_executor:invoke(K, <<"upper">>, #{text => <<"hello">>}, beamai_context:new())).

%%====================================================================
%% get_tool/2 Tests
%%====================================================================

get_tool_add_test() ->
    K = make_math_chat_client(),
    {ok, T} = beamai_tool_registry:resolve(beamai_chat_client:tools(K), <<"add">>),
    ?assertEqual(<<"add">>, maps:get(name, T)).

get_tool_multiply_test() ->
    K = make_math_chat_client(),
    {ok, T} = beamai_tool_registry:resolve(beamai_chat_client:tools(K), <<"multiply">>),
    ?assertEqual(<<"multiply">>, maps:get(name, T)).

get_tool_not_found_test() ->
    K = make_math_chat_client(),
    ?assertEqual(error, beamai_tool_registry:resolve(beamai_chat_client:tools(K), <<"nonexistent">>)).

%%====================================================================
%% list_tools/1 Tests
%%====================================================================

list_tools_test() ->
    K = make_multi_tag_chat_client(),
    Tools = beamai_tool_registry:list(beamai_chat_client:tools(K)),
    ?assertEqual(4, length(Tools)).

%%====================================================================
%% get_tool_schemas Tests
%%====================================================================

get_tool_schemas_openai_test() ->
    K = make_math_chat_client(),
    Schemas = beamai_tool_registry:schemas(beamai_chat_client:tools(K), openai),
    ?assertEqual(2, length(Schemas)),
    [S1 | _] = Schemas,
    ?assertEqual(<<"function">>, maps:get(<<"type">>, S1)).

get_tool_schemas_anthropic_test() ->
    K = make_math_chat_client(),
    Schemas = beamai_tool_registry:schemas(beamai_chat_client:tools(K), anthropic),
    ?assertEqual(2, length(Schemas)),
    [S1 | _] = Schemas,
    ?assert(maps:is_key(<<"name">>, S1)),
    ?assert(maps:is_key(<<"input_schema">>, S1)).

get_tool_specs_test() ->
    K = make_math_chat_client(),
    Specs = beamai_tool_registry:specs(beamai_chat_client:tools(K)),
    ?assertEqual(2, length(Specs)),
    [S1 | _] = Specs,
    %% Unified format: atom keys
    ?assert(maps:is_key(name, S1)),
    ?assert(maps:is_key(description, S1)),
    ?assert(maps:is_key(parameters, S1)).

%%====================================================================
%% Service Tests
%%====================================================================

add_chat_model_test() ->
    K0 = beamai_chat_client:new(),
    LlmConfig = beamai_chat_model:create(mock, #{model => <<"test">>}),
    K1 = beamai_chat_client:add_chat_model(K0, LlmConfig),
    {ok, Svc} = beamai_chat_client:chat_model(K1),
    ?assertEqual(mock, maps:get(provider, Svc)).

chat_model_not_found_test() ->
    K = beamai_chat_client:new(),
    ?assertEqual(error, beamai_chat_client:chat_model(K)).

no_chat_model_chat_test() ->
    K = make_math_chat_client(),
    ?assertEqual({error, no_chat_model},
                 beamai_chat_client:invoke_chat(K, [#{role => user, content => <<"hi">>}], #{})).

%% invoke_chat 与 invoke_tool 一致地把 ChatClient 绑进 context，around_chat filter 可取到
context_binds_chat_client_in_chat_test() ->
    Self = self(),
    Filter = beamai_filter:new(<<"capture_chat_client">>, #{
        around_chat => fun(#{context := Ctx} = Req, _F, Next) ->
            Self ! {chat_client_bound, beamai_context:get_chat_client(Ctx) =/= undefined},
            Next(Req)
        end
    }),
    K0 = beamai_chat_client:new(#{}, [Filter]),
    K2 = beamai_chat_client:add_chat_model(K0, beamai_chat_model:create(mock, #{model => <<"m">>})),
    {ok, _Resp, _Ctx} = beamai_chat_client:invoke_chat(K2, [#{role => user, content => <<"hi">>}], #{}),
    receive {chat_client_bound, Bound} -> ?assert(Bound)
    after 1000 -> ?assert(false)
    end.

%%====================================================================
%% Filter Integration Tests
%%====================================================================

invoke_with_pre_tool_filter_test() ->
    %% around_tool 前置：把参数翻倍
    Filter = beamai_filter:new(<<"doubler">>, #{
        around_tool => fun(#{args := Args} = Req, _FCtx, Next) ->
            NewArgs = maps:map(fun(_K, V) when is_number(V) -> V * 2;
                                 (_K, V) -> V end, Args),
            Next(Req#{args => NewArgs})
        end
    }),
    K0 = beamai_chat_client:new(#{}, [Filter]),
    K2 = beamai_chat_client:add_tool(K0,
        beamai_tool:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end)
    ),
    ?assertMatch({ok, 30, _}, beamai_tool_executor:invoke(K2, <<"add">>, #{a => 7, b => 8}, beamai_context:new())).

invoke_with_post_tool_filter_test() ->
    %% around_tool 后置：把结果翻倍
    Filter = beamai_filter:new(<<"result_doubler">>, #{
        around_tool => fun(Req, _FCtx, Next) ->
            #{result := R} = Resp = Next(Req),
            Resp#{result => R * 2}
        end
    }),
    K0 = beamai_chat_client:new(#{}, [Filter]),
    K2 = beamai_chat_client:add_tool(K0,
        beamai_tool:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end)
    ),
    ?assertMatch({ok, 30, _}, beamai_tool_executor:invoke(K2, <<"add">>, #{a => 7, b => 8}, beamai_context:new())).

invoke_with_halt_tool_filter_test() ->
    %% around_tool 短路：不调 Next，直接返回缓存结果
    Filter = beamai_filter:new(<<"cache">>, #{
        around_tool => fun(#{context := Ctx}, _FCtx, _Next) ->
            #{result => cached_result, context => Ctx}
        end
    }),
    K0 = beamai_chat_client:new(#{}, [Filter]),
    K2 = beamai_chat_client:add_tool(K0,
        beamai_tool:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end)
    ),
    ?assertMatch({ok, cached_result, _}, beamai_tool_executor:invoke(K2, <<"add">>, #{a => 7, b => 8}, beamai_context:new())).

%%====================================================================
%% Facade API Tests
%%====================================================================

facade_chat_client_test() ->
    K = beamai:chat_client(),
    ?assertEqual(true, maps:get('__chat_client__', K)).

facade_add_tool_test() ->
    K0 = beamai:chat_client(),
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
    K0 = beamai:chat_client(),
    K1 = beamai:add_chat_model(K0, mock, #{model => <<"test-model">>}),
    {ok, Config} = beamai_chat_client:chat_model(K1),
    ?assertEqual(mock, maps:get(provider, Config)).

facade_tools_test() ->
    K = make_math_chat_client(),
    Tools = beamai:tools(K),
    ?assertEqual(2, length(Tools)).

facade_tools_by_tag_test() ->
    K = make_multi_tag_chat_client(),
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

%% 注：ReAct 工具调用循环已从 ChatClient 移出（属 Agent 层职责），
%% 相应的循环测试见 apps/beamai_agent/test/beamai_agent_tests.erl。
