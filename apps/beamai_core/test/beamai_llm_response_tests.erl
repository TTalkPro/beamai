%%%-------------------------------------------------------------------
%%% @doc beamai_chat_response 模块单元测试
%%%
%%% 测试 new/1 构造器和统一访问接口。
%%% Provider 特定解析测试已迁移至 beamai_llm_response_parser_tests。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_response_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% new/1 构造器测试
%%====================================================================

new_defaults_test() ->
    Resp = beamai_chat_response:new(#{}),
    ?assertEqual(<<>>, beamai_chat_response:id(Resp)),
    ?assertEqual(<<>>, beamai_chat_response:model(Resp)),
    ?assertEqual(unknown, beamai_chat_response:provider(Resp)),
    ?assertEqual(null, beamai_chat_response:content(Resp)),
    ?assertEqual([], beamai_chat_response:content_blocks(Resp)),
    ?assertEqual([], beamai_chat_response:tool_calls(Resp)),
    ?assertEqual(unknown, beamai_chat_response:finish_reason(Resp)),
    ?assertEqual(#{input_tokens => 0, output_tokens => 0, total_tokens => 0}, beamai_chat_response:usage(Resp)),
    ?assertEqual(#{}, beamai_chat_response:raw(Resp)),
    ?assertEqual(#{}, beamai_chat_response:metadata(Resp)).

new_with_fields_test() ->
    Resp = beamai_chat_response:new(#{
        id => <<"test-123">>,
        model => <<"gpt-4">>,
        provider => openai,
        content => <<"Hello!">>,
        finish_reason => complete,
        usage => #{input_tokens => 10, output_tokens => 20, total_tokens => 30}
    }),
    ?assertEqual(<<"test-123">>, beamai_chat_response:id(Resp)),
    ?assertEqual(<<"gpt-4">>, beamai_chat_response:model(Resp)),
    ?assertEqual(openai, beamai_chat_response:provider(Resp)),
    ?assertEqual(<<"Hello!">>, beamai_chat_response:content(Resp)),
    ?assertEqual(complete, beamai_chat_response:finish_reason(Resp)),
    ?assertEqual(10, beamai_chat_response:input_tokens(Resp)),
    ?assertEqual(20, beamai_chat_response:output_tokens(Resp)),
    ?assertEqual(30, beamai_chat_response:total_tokens(Resp)).

%%====================================================================
%% 访问器测试
%%====================================================================

has_tool_calls_test() ->
    Empty = beamai_chat_response:new(#{}),
    ?assertEqual(false, beamai_chat_response:has_tool_calls(Empty)),

    WithTools = beamai_chat_response:new(#{tool_calls => [#{id => <<"1">>, name => <<"test">>, arguments => #{}, raw_arguments => <<>>}]}),
    ?assertEqual(true, beamai_chat_response:has_tool_calls(WithTools)).

is_complete_test() ->
    Complete = beamai_chat_response:new(#{finish_reason => complete}),
    ?assertEqual(true, beamai_chat_response:is_complete(Complete)),

    Incomplete = beamai_chat_response:new(#{finish_reason => tool_use}),
    ?assertEqual(false, beamai_chat_response:is_complete(Incomplete)).

needs_tool_call_test() ->
    ToolUse = beamai_chat_response:new(#{finish_reason => tool_use}),
    ?assertEqual(true, beamai_chat_response:needs_tool_call(ToolUse)),

    WithTools = beamai_chat_response:new(#{tool_calls => [#{id => <<"1">>, name => <<"test">>, arguments => #{}, raw_arguments => <<>>}]}),
    ?assertEqual(true, beamai_chat_response:needs_tool_call(WithTools)),

    NoTools = beamai_chat_response:new(#{finish_reason => complete}),
    ?assertEqual(false, beamai_chat_response:needs_tool_call(NoTools)).

metadata_test() ->
    Resp = beamai_chat_response:new(#{metadata => #{created => 1234567890}}),
    Meta = beamai_chat_response:metadata(Resp),
    ?assertEqual(1234567890, maps:get(created, Meta)),

    %% 设置新的元数据
    Resp2 = beamai_chat_response:set_metadata(Resp, latency_ms, 150),
    ?assertEqual(150, maps:get(latency_ms, beamai_chat_response:metadata(Resp2))).

to_map_test() ->
    Resp = beamai_chat_response:new(#{id => <<"test-1">>, content => <<"Test">>}),
    Map = beamai_chat_response:to_map(Resp),
    ?assertEqual(false, maps:is_key('__struct__', Map)),
    ?assertEqual(<<"test-1">>, maps:get(id, Map)).

raw_get_test() ->
    RawData = #{<<"key1">> => <<"val1">>, <<"deep">> => #{<<"nested">> => <<"value">>}},
    Resp = beamai_chat_response:new(#{raw => RawData}),

    ?assertEqual(<<"val1">>, beamai_chat_response:raw_get(Resp, <<"key1">>)),
    ?assertEqual(undefined, beamai_chat_response:raw_get(Resp, <<"nonexistent">>)),
    ?assertEqual(default_val, beamai_chat_response:raw_get(Resp, <<"nonexistent">>, default_val)),
    ?assertEqual(<<"value">>, beamai_chat_response:raw_get(Resp, [<<"deep">>, <<"nested">>])),
    ?assertEqual(default_val, beamai_chat_response:raw_get(Resp, [<<"deep">>, <<"missing">>], default_val)).

reasoning_content_test() ->
    NoReasoning = beamai_chat_response:new(#{}),
    ?assertEqual(null, beamai_chat_response:reasoning_content(NoReasoning)),

    WithReasoning = beamai_chat_response:new(#{metadata => #{reasoning_content => <<"thinking...">>}}),
    ?assertEqual(<<"thinking...">>, beamai_chat_response:reasoning_content(WithReasoning)).
