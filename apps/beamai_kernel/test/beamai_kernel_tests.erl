-module(beamai_kernel_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup
%%====================================================================

make_math_kernel() ->
    K0 = beamai_kernel:new(),
    beamai_kernel:add_plugin(K0, <<"math">>, [
        beamai_function:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end, #{
            description => <<"Add two numbers">>,
            parameters => #{
                a => #{type => integer, required => true},
                b => #{type => integer, required => true}
            }
        }),
        beamai_function:new(<<"multiply">>, fun(#{a := A, b := B}) -> {ok, A * B} end, #{
            description => <<"Multiply two numbers">>,
            parameters => #{
                a => #{type => integer, required => true},
                b => #{type => integer, required => true}
            }
        })
    ]).

make_multi_plugin_kernel() ->
    K0 = make_math_kernel(),
    beamai_kernel:add_plugin(K0, <<"string">>, [
        beamai_function:new(<<"upper">>, fun(#{text := T}) -> {ok, string:uppercase(T)} end, #{
            description => <<"Convert to uppercase">>,
            parameters => #{text => #{type => string, required => true}}
        }),
        beamai_function:new(<<"length">>, fun(#{text := T}) -> {ok, byte_size(T)} end, #{
            description => <<"Get string length">>,
            parameters => #{text => #{type => string, required => true}}
        })
    ]).

%%====================================================================
%% new/0,1 Tests
%%====================================================================

new_default_test() ->
    K = beamai_kernel:new(),
    ?assertEqual(true, maps:get('__kernel__', K)),
    ?assertEqual(#{}, maps:get(plugins, K)),
    ?assertEqual(#{}, maps:get(services, K)),
    ?assertEqual([], maps:get(filters, K)).

new_with_settings_test() ->
    K = beamai_kernel:new(#{max_tool_iterations => 5}),
    ?assertEqual(#{max_tool_iterations => 5}, maps:get(settings, K)).

%%====================================================================
%% add_plugin Tests
%%====================================================================

add_plugin_def_test() ->
    K0 = beamai_kernel:new(),
    Plugin = beamai_plugin:new(<<"math">>, [
        beamai_function:new(<<"add">>, fun(_) -> {ok, 0} end)
    ]),
    K1 = beamai_kernel:add_plugin(K0, Plugin),
    ?assert(maps:is_key(<<"math">>, maps:get(plugins, K1))).

add_plugin_inline_test() ->
    K = make_math_kernel(),
    ?assert(maps:is_key(<<"math">>, maps:get(plugins, K))).

%%====================================================================
%% invoke/3,4 Tests
%%====================================================================

invoke_qualified_name_test() ->
    K = make_math_kernel(),
    ?assertEqual({ok, 15}, beamai_kernel:invoke(K, <<"math.add">>, #{a => 7, b => 8})).

invoke_unqualified_name_test() ->
    K = make_math_kernel(),
    ?assertEqual({ok, 15}, beamai_kernel:invoke(K, <<"add">>, #{a => 7, b => 8})).

invoke_multiply_test() ->
    K = make_math_kernel(),
    ?assertEqual({ok, 42}, beamai_kernel:invoke(K, <<"math.multiply">>, #{a => 6, b => 7})).

invoke_not_found_test() ->
    K = make_math_kernel(),
    ?assertEqual({error, {function_not_found, <<"nonexistent">>}},
                 beamai_kernel:invoke(K, <<"nonexistent">>, #{})).

invoke_with_context_test() ->
    K = beamai_kernel:add_plugin(beamai_kernel:new(), <<"ctx">>, [
        beamai_function:new(<<"get_var">>,
            fun(#{key := Key}, Ctx) ->
                {ok, beamai_context:get(Ctx, Key)}
            end)
    ]),
    Ctx = beamai_context:set(beamai_context:new(), <<"name">>, <<"Alice">>),
    ?assertEqual({ok, <<"Alice">>},
                 beamai_kernel:invoke(K, <<"ctx.get_var">>, #{key => <<"name">>}, Ctx)).

invoke_multi_plugin_test() ->
    K = make_multi_plugin_kernel(),
    ?assertEqual({ok, 15}, beamai_kernel:invoke(K, <<"math.add">>, #{a => 7, b => 8})),
    ?assertEqual({ok, <<"HELLO">>}, beamai_kernel:invoke(K, <<"string.upper">>, #{text => <<"hello">>})).

%%====================================================================
%% get_function/2 Tests
%%====================================================================

get_function_qualified_test() ->
    K = make_math_kernel(),
    {ok, F} = beamai_kernel:get_function(K, <<"math.add">>),
    ?assertEqual(<<"add">>, maps:get(name, F)).

get_function_unqualified_test() ->
    K = make_math_kernel(),
    {ok, F} = beamai_kernel:get_function(K, <<"multiply">>),
    ?assertEqual(<<"multiply">>, maps:get(name, F)).

get_function_not_found_test() ->
    K = make_math_kernel(),
    ?assertEqual(error, beamai_kernel:get_function(K, <<"nonexistent">>)).

%%====================================================================
%% list_functions/1 Tests
%%====================================================================

list_functions_test() ->
    K = make_multi_plugin_kernel(),
    Funcs = beamai_kernel:list_functions(K),
    ?assertEqual(4, length(Funcs)).

%%====================================================================
%% get_tool_schemas Tests
%%====================================================================

get_tool_schemas_openai_test() ->
    K = make_math_kernel(),
    Schemas = beamai_kernel:get_tool_schemas(K, openai),
    ?assertEqual(2, length(Schemas)),
    [S1 | _] = Schemas,
    ?assertEqual(<<"function">>, maps:get(type, S1)).

get_tool_schemas_anthropic_test() ->
    K = make_math_kernel(),
    Schemas = beamai_kernel:get_tool_schemas(K, anthropic),
    ?assertEqual(2, length(Schemas)),
    [S1 | _] = Schemas,
    ?assert(maps:is_key(name, S1)),
    ?assert(maps:is_key(input_schema, S1)).

%%====================================================================
%% Service Tests
%%====================================================================

add_chat_completion_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_chat_completion(K0, #{
        connector => beamai_connector_openai,
        model => <<"gpt-4">>,
        api_key => <<"test-key">>
    }),
    {ok, Svc} = beamai_kernel:get_service(K1, chat_completion),
    ?assertEqual(chat_completion, maps:get(type, Svc)),
    ?assertEqual(beamai_connector_openai, maps:get(connector, Svc)).

add_embedding_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_embedding(K0, #{
        connector => beamai_connector_openai,
        model => <<"text-embedding-3-small">>
    }),
    {ok, Svc} = beamai_kernel:get_service(K1, embedding),
    ?assertEqual(embedding, maps:get(type, Svc)).

get_service_not_found_test() ->
    K = beamai_kernel:new(),
    ?assertEqual(error, beamai_kernel:get_service(K, chat_completion)).

%%====================================================================
%% Filter Integration Tests
%%====================================================================

invoke_with_pre_filter_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_plugin(K0, <<"math">>, [
        beamai_function:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end)
    ]),
    %% Add filter that doubles all values
    Filter = beamai_filter:new(<<"doubler">>, pre_invocation,
        fun(#{args := Args} = Ctx) ->
            NewArgs = maps:map(fun(_K, V) when is_number(V) -> V * 2;
                                 (_K, V) -> V end, Args),
            {continue, Ctx#{args => NewArgs}}
        end),
    K2 = beamai_kernel:add_filter(K1, Filter),
    %% 7*2 + 8*2 = 14 + 16 = 30
    ?assertEqual({ok, 30}, beamai_kernel:invoke(K2, <<"math.add">>, #{a => 7, b => 8})).

invoke_with_post_filter_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_plugin(K0, <<"math">>, [
        beamai_function:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end)
    ]),
    %% Add filter that doubles the result
    Filter = beamai_filter:new(<<"result_doubler">>, post_invocation,
        fun(#{result := R} = Ctx) ->
            {continue, Ctx#{result => R * 2}}
        end),
    K2 = beamai_kernel:add_filter(K1, Filter),
    ?assertEqual({ok, 30}, beamai_kernel:invoke(K2, <<"math.add">>, #{a => 7, b => 8})).

invoke_with_skip_filter_test() ->
    K0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_plugin(K0, <<"math">>, [
        beamai_function:new(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end)
    ]),
    %% Add filter that skips execution and returns cached value
    Filter = beamai_filter:new(<<"cache">>, pre_invocation,
        fun(_Ctx) ->
            {skip, cached_result}
        end),
    K2 = beamai_kernel:add_filter(K1, Filter),
    ?assertEqual({ok, cached_result}, beamai_kernel:invoke(K2, <<"math.add">>, #{a => 7, b => 8})).

%%====================================================================
%% Facade API Tests
%%====================================================================

facade_kernel_test() ->
    K = beamai:kernel(),
    ?assertEqual(true, maps:get('__kernel__', K)).

facade_add_plugin_test() ->
    K0 = beamai:kernel(),
    K1 = beamai:add_plugin(K0, <<"math">>, [
        beamai:function(<<"add">>, fun(#{a := A, b := B}) -> {ok, A + B} end, #{
            description => <<"Add">>,
            parameters => #{
                a => #{type => integer, required => true},
                b => #{type => integer, required => true}
            }
        })
    ]),
    ?assertEqual({ok, 15}, beamai:invoke(K1, <<"math.add">>, #{a => 7, b => 8})).

facade_tools_test() ->
    K = make_math_kernel(),
    Tools = beamai:tools(K),
    ?assertEqual(2, length(Tools)).

facade_functions_test() ->
    K = make_multi_plugin_kernel(),
    ?assertEqual(4, length(beamai:functions(K))).

facade_context_test() ->
    Ctx = beamai:context(#{<<"key">> => <<"value">>}),
    ?assertEqual(<<"value">>, beamai_context:get(Ctx, <<"key">>)).

facade_render_test() ->
    {ok, Result} = beamai:render(<<"Hello, {{name}}!">>, #{<<"name">> => <<"World">>}),
    ?assertEqual(<<"Hello, World!">>, Result).
