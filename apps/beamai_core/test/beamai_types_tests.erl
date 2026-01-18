%%%-------------------------------------------------------------------
%%% @doc Agent Types 测试模块
%%%
%%% 测试 beamai_types 模块的功能。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_types_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 类型转换测试
%%====================================================================

atom_keys_to_binary_simple_test() ->
    Input = #{role => user, content => <<"hello">>},
    Expected = #{<<"role">> => <<"user">>, <<"content">> => <<"hello">>},
    ?assertEqual(Expected, beamai_types:atom_keys_to_binary(Input)).

atom_keys_to_binary_nested_test() ->
    Input = #{
        role => user,
        data => #{
            key1 => <<"value1">>,
            key2 => <<"value2">>
        }
    },
    Expected = #{
        <<"role">> => <<"user">>,
        <<"data">> => #{
            <<"key1">> => <<"value1">>,
            <<"key2">> => <<"value2">>
        }
    },
    ?assertEqual(Expected, beamai_types:atom_keys_to_binary(Input)).

atom_keys_to_binary_list_test() ->
    Input = #{
        items => [
            #{name => <<"item1">>},
            #{name => <<"item2">>}
        ]
    },
    Expected = #{
        <<"items">> => [
            #{<<"name">> => <<"item1">>},
            #{<<"name">> => <<"item2">>}
        ]
    },
    ?assertEqual(Expected, beamai_types:atom_keys_to_binary(Input)).

binary_keys_to_atom_test() ->
    %% 预先引用 atoms 以确保它们存在
    _ = role,
    _ = content,
    Input = #{<<"role">> => <<"user">>, <<"content">> => <<"hello">>},
    Expected = #{role => <<"user">>, content => <<"hello">>},
    ?assertEqual(Expected, beamai_types:binary_keys_to_atom(Input)).

binary_keys_to_atom_nested_test() ->
    %% 预先引用 atoms 以确保它们存在
    _ = role,
    _ = data,
    _ = key1,
    Input = #{
        <<"role">> => <<"user">>,
        <<"data">> => #{
            <<"key1">> => <<"value1">>
        }
    },
    Expected = #{
        role => <<"user">>,
        data => #{
            key1 => <<"value1">>
        }
    },
    ?assertEqual(Expected, beamai_types:binary_keys_to_atom(Input)).

round_trip_conversion_test() ->
    %% 预先引用 atoms 以确保它们存在
    _ = role,
    _ = content,
    _ = nested,
    _ = key,
    _ = list,
    _ = item,
    Original = #{
        role => user,
        content => <<"hello">>,
        nested => #{key => <<"value">>},
        list => [#{item => <<"1">>}, #{item => <<"2">>}]
    },
    Converted = beamai_types:atom_keys_to_binary(Original),
    RoundTrip = beamai_types:binary_keys_to_atom(Converted),
    %% binary_keys_to_atom 只转换 keys，不转换 values
    %% 所以 atom values 会变成 binary
    Expected = #{
        role => <<"user">>,
        content => <<"hello">>,
        nested => #{key => <<"value">>},
        list => [#{item => <<"1">>}, #{item => <<"2">>}]
    },
    ?assertEqual(Expected, RoundTrip).

%%====================================================================
%% 消息构造测试
%%====================================================================

make_system_message_test() ->
    Msg = beamai_types:make_system_message(<<"You are a helpful assistant.">>),
    ?assertEqual(<<"system">>, maps:get(<<"role">>, Msg)),
    ?assertEqual(<<"You are a helpful assistant.">>, maps:get(<<"content">>, Msg)),
    ?assertEqual([], maps:get(<<"tool_calls">>, Msg)).

make_user_message_test() ->
    Msg = beamai_types:make_user_message(<<"Hello!">>),
    ?assertEqual(<<"user">>, maps:get(<<"role">>, Msg)),
    ?assertEqual(<<"Hello!">>, maps:get(<<"content">>, Msg)).

make_assistant_message_test() ->
    ToolCall = beamai_types:make_tool_call(<<"call_123">>, <<"calculate">>, #{<<"expression">> => <<"1+1">>}),
    Msg = beamai_types:make_assistant_message(<<"Result: 2">>, [ToolCall]),
    ?assertEqual(<<"assistant">>, maps:get(<<"role">>, Msg)),
    ?assertEqual(<<"Result: 2">>, maps:get(<<"content">>, Msg)),
    ?assertEqual(1, length(maps:get(<<"tool_calls">>, Msg))).

make_tool_message_test() ->
    Msg = beamai_types:make_tool_message(<<"call_123">>, <<"2">>),
    ?assertEqual(<<"tool">>, maps:get(<<"role">>, Msg)),
    ?assertEqual(<<"2">>, maps:get(<<"content">>, Msg)).

make_tool_call_test() ->
    ToolCall = beamai_types:make_tool_call(<<"call_123">>, <<"calculate">>, #{<<"expression">> => <<"1+1">>}),
    ?assertEqual(<<"call_123">>, maps:get(<<"id">>, ToolCall)),
    ?assertEqual(<<"function">>, maps:get(<<"type">>, ToolCall)),
    Fun = maps:get(<<"function">>, ToolCall),
    ?assertEqual(<<"calculate">>, maps:get(<<"name">>, Fun)),
    ?assertEqual(<<"{\"expression\":\"1+1\"}">>, maps:get(<<"arguments">>, Fun)).

%%====================================================================
%% 验证测试
%%====================================================================

validate_llm_message_valid_test() ->
    Msg = #{
        <<"role">> => <<"user">>,
        <<"content">> => <<"Hello">>
    },
    ?assertEqual(ok, beamai_types:validate_llm_message(Msg)).

validate_llm_message_null_content_test() ->
    Msg = #{
        <<"role">> => <<"assistant">>,
        <<"content">> => null
    },
    ?assertEqual(ok, beamai_types:validate_llm_message(Msg)).

validate_llm_message_invalid_role_test() ->
    Msg = #{
        <<"role">> => <<"invalid">>,
        <<"content">> => <<"Hello">>
    },
    ?assertEqual({error, {invalid_role, <<"invalid">>}}, beamai_types:validate_llm_message(Msg)).

validate_llm_message_missing_role_test() ->
    Msg = #{<<"content">> => <<"Hello">>},
    ?assertEqual({error, missing_role}, beamai_types:validate_llm_message(Msg)).

validate_tool_call_valid_test() ->
    ToolCall = beamai_types:make_tool_call(<<"call_123">>, <<"calculate">>, #{<<"expression">> => <<"1+1">>}),
    ?assertEqual(ok, beamai_types:validate_tool_call(ToolCall)).

validate_tool_call_invalid_id_test() ->
    ToolCall = #{
        <<"id">> => 123,  % should be binary
        <<"type">> => <<"function">>,
        <<"function">> => #{<<"name">> => <<"calc">>, <<"arguments">> => <<"{}">>}
    },
    ?assertMatch({error, {invalid_id, _}}, beamai_types:validate_tool_call(ToolCall)).

validate_tool_call_invalid_format_test() ->
    ToolCall = #{<<"invalid">> => <<"data">>},
    ?assertEqual({error, invalid_tool_call_format}, beamai_types:validate_tool_call(ToolCall)).

%%====================================================================
%% 集成测试
%%====================================================================

tool_call_conversion_workflow_test() ->
    %% 模拟从 LLM API 获取的 atom key 格式
    APIResponse = #{
        id => <<"call_123">>,
        type => function,
        function => #{
            name => <<"calculate">>,
            arguments => <<"{\"expression\":\"1+1\"}">>
        }
    },

    %% 转换为标准格式
    Standardized = beamai_types:atom_keys_to_binary(APIResponse),

    %% 验证格式
    ?assertEqual(ok, beamai_types:validate_tool_call(Standardized)),

    %% 验证可以正确解析
    ?assertEqual(<<"call_123">>, maps:get(<<"id">>, Standardized)),
    ?assertEqual(<<"function">>, maps:get(<<"type">>, Standardized)),

    Fun = maps:get(<<"function">>, Standardized),
    ?assertEqual(<<"calculate">>, maps:get(<<"name">>, Fun)),
    ?assertEqual(<<"{\"expression\":\"1+1\"}">>, maps:get(<<"arguments">>, Fun)).
