%%%-------------------------------------------------------------------
%%% @doc 流式事件累加与 finalize 单元测试
%%%
%%% 覆盖 beamai_llm_provider_common 的流式累加器：
%%%   - OpenAI 格式：content / 分片 tool_calls / reasoning_content / usage
%%%   - Anthropic 格式：message_start / content_block_* / message_delta
%%% 以及 finalize 后与同步模式一致的统一响应结构。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_stream_accumulator_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助函数
%%====================================================================

%% 依次累加 OpenAI 格式事件
accumulate_openai(Events) ->
    lists:foldl(fun beamai_llm_provider_common:accumulate_openai_event/2,
                beamai_llm_http_client:init_stream_acc(), Events).

%% 依次累加 Anthropic 格式事件
accumulate_anthropic(Events) ->
    lists:foldl(fun beamai_llm_provider_common:accumulate_anthropic_event/2,
                beamai_llm_http_client:init_stream_acc(), Events).

%%====================================================================
%% OpenAI 流式：内容累加 + finalize
%%====================================================================

openai_stream_content_test() ->
    Events = [
        #{<<"id">> => <<"chatcmpl-1">>, <<"model">> => <<"gpt-4">>,
          <<"choices">> => [#{<<"delta">> => #{<<"role">> => <<"assistant">>, <<"content">> => <<"Hello">>},
                              <<"finish_reason">> => null}]},
        #{<<"choices">> => [#{<<"delta">> => #{<<"content">> => <<" World">>},
                              <<"finish_reason">> => null}]},
        #{<<"choices">> => [#{<<"delta">> => #{}, <<"finish_reason">> => <<"stop">>}]}
    ],
    Acc = accumulate_openai(Events),
    {ok, Resp} = beamai_llm_provider_common:finalize_openai_stream(Acc, openai),
    ?assertEqual(<<"Hello World">>, beamai_llm_response:content(Resp)),
    ?assertEqual(<<"chatcmpl-1">>, beamai_llm_response:id(Resp)),
    ?assertEqual(<<"gpt-4">>, beamai_llm_response:model(Resp)),
    ?assertEqual(complete, beamai_llm_response:finish_reason(Resp)),
    ?assertEqual(false, beamai_llm_response:has_tool_calls(Resp)).

%%====================================================================
%% OpenAI 流式：分片工具调用累加
%%====================================================================

openai_stream_tool_calls_test() ->
    %% 模拟 OpenAI 分片工具调用：首个分片携带 id/name，
    %% 后续分片仅携带 arguments 片段，按 index 关联（两个并行调用）
    Events = [
        #{<<"id">> => <<"chatcmpl-2">>, <<"model">> => <<"gpt-4">>,
          <<"choices">> => [#{<<"delta">> => #{<<"tool_calls">> => [
              #{<<"index">> => 0, <<"id">> => <<"call_1">>,
                <<"function">> => #{<<"name">> => <<"get_weather">>, <<"arguments">> => <<>>}}
          ]}, <<"finish_reason">> => null}]},
        #{<<"choices">> => [#{<<"delta">> => #{<<"tool_calls">> => [
              #{<<"index">> => 0, <<"function">> => #{<<"arguments">> => <<"{\"city\":">>}}
          ]}, <<"finish_reason">> => null}]},
        #{<<"choices">> => [#{<<"delta">> => #{<<"tool_calls">> => [
              #{<<"index">> => 0, <<"function">> => #{<<"arguments">> => <<"\"Beijing\"}">>}},
              #{<<"index">> => 1, <<"id">> => <<"call_2">>,
                <<"function">> => #{<<"name">> => <<"get_time">>, <<"arguments">> => <<"{}">>}}
          ]}, <<"finish_reason">> => null}]},
        #{<<"choices">> => [#{<<"delta">> => #{}, <<"finish_reason">> => <<"tool_calls">>}]}
    ],
    Acc = accumulate_openai(Events),
    {ok, Resp} = beamai_llm_provider_common:finalize_openai_stream(Acc, openai),
    ?assertEqual(tool_use, beamai_llm_response:finish_reason(Resp)),
    [TC1, TC2] = beamai_llm_response:tool_calls(Resp),
    ?assertEqual(<<"call_1">>, maps:get(id, TC1)),
    ?assertEqual(<<"get_weather">>, maps:get(name, TC1)),
    ?assertEqual(#{<<"city">> => <<"Beijing">>}, maps:get(arguments, TC1)),
    ?assertEqual(<<"call_2">>, maps:get(id, TC2)),
    ?assertEqual(<<"get_time">>, maps:get(name, TC2)).

%%====================================================================
%% OpenAI 流式：include_usage 末尾 chunk
%%====================================================================

openai_stream_usage_test() ->
    Events = [
        #{<<"id">> => <<"chatcmpl-3">>,
          <<"choices">> => [#{<<"delta">> => #{<<"content">> => <<"Hi">>},
                              <<"finish_reason">> => <<"stop">>}]},
        %% include_usage 模式：末尾 chunk 的 choices 为空，仅携带 usage
        #{<<"id">> => <<"chatcmpl-3">>, <<"choices">> => [],
          <<"usage">> => #{<<"prompt_tokens">> => 10, <<"completion_tokens">> => 5,
                           <<"total_tokens">> => 15}}
    ],
    Acc = accumulate_openai(Events),
    {ok, Resp} = beamai_llm_provider_common:finalize_openai_stream(Acc, openai),
    ?assertEqual(10, beamai_llm_response:input_tokens(Resp)),
    ?assertEqual(5, beamai_llm_response:output_tokens(Resp)),
    ?assertEqual(15, beamai_llm_response:total_tokens(Resp)).

%%====================================================================
%% DeepSeek 流式：reasoning_content 累加
%%====================================================================

deepseek_stream_reasoning_test() ->
    Events = [
        #{<<"id">> => <<"ds-1">>, <<"model">> => <<"deepseek-reasoner">>,
          <<"choices">> => [#{<<"delta">> => #{<<"reasoning_content">> => <<"Let me ">>},
                              <<"finish_reason">> => null}]},
        #{<<"choices">> => [#{<<"delta">> => #{<<"reasoning_content">> => <<"think...">>},
                              <<"finish_reason">> => null}]},
        #{<<"choices">> => [#{<<"delta">> => #{<<"content">> => <<"Answer: 42">>},
                              <<"finish_reason">> => <<"stop">>}]}
    ],
    Acc = accumulate_openai(Events),
    {ok, Resp} = beamai_llm_provider_common:finalize_openai_stream(Acc, deepseek),
    ?assertEqual(deepseek, beamai_llm_response:provider(Resp)),
    ?assertEqual(<<"Answer: 42">>, beamai_llm_response:content(Resp)),
    ?assertEqual(<<"Let me think...">>, beamai_llm_response:reasoning_content(Resp)).

%%====================================================================
%% Anthropic 流式：文本 + 工具调用 + usage
%%====================================================================

anthropic_stream_full_test() ->
    Events = [
        #{<<"type">> => <<"message_start">>,
          <<"message">> => #{<<"id">> => <<"msg_1">>, <<"model">> => <<"claude-sonnet-4-5">>,
                             <<"usage">> => #{<<"input_tokens">> => 20, <<"output_tokens">> => 1}}},
        #{<<"type">> => <<"content_block_start">>, <<"index">> => 0,
          <<"content_block">> => #{<<"type">> => <<"text">>, <<"text">> => <<>>}},
        #{<<"type">> => <<"content_block_delta">>, <<"index">> => 0,
          <<"delta">> => #{<<"type">> => <<"text_delta">>, <<"text">> => <<"I'll check ">>}},
        #{<<"type">> => <<"content_block_delta">>, <<"index">> => 0,
          <<"delta">> => #{<<"type">> => <<"text_delta">>, <<"text">> => <<"the weather.">>}},
        #{<<"type">> => <<"content_block_stop">>, <<"index">> => 0},
        #{<<"type">> => <<"content_block_start">>, <<"index">> => 1,
          <<"content_block">> => #{<<"type">> => <<"tool_use">>, <<"id">> => <<"toolu_1">>,
                                   <<"name">> => <<"get_weather">>, <<"input">> => #{}}},
        #{<<"type">> => <<"content_block_delta">>, <<"index">> => 1,
          <<"delta">> => #{<<"type">> => <<"input_json_delta">>, <<"partial_json">> => <<"{\"city\":">>}},
        #{<<"type">> => <<"content_block_delta">>, <<"index">> => 1,
          <<"delta">> => #{<<"type">> => <<"input_json_delta">>, <<"partial_json">> => <<"\"Beijing\"}">>}},
        #{<<"type">> => <<"content_block_stop">>, <<"index">> => 1},
        #{<<"type">> => <<"message_delta">>,
          <<"delta">> => #{<<"stop_reason">> => <<"tool_use">>, <<"stop_sequence">> => null},
          <<"usage">> => #{<<"output_tokens">> => 30}},
        #{<<"type">> => <<"message_stop">>}
    ],
    Acc = accumulate_anthropic(Events),
    {ok, Resp} = beamai_llm_provider_common:finalize_anthropic_stream(Acc),
    ?assertEqual(<<"msg_1">>, beamai_llm_response:id(Resp)),
    ?assertEqual(<<"claude-sonnet-4-5">>, beamai_llm_response:model(Resp)),
    ?assertEqual(anthropic, beamai_llm_response:provider(Resp)),
    ?assertEqual(<<"I'll check the weather.">>, beamai_llm_response:content(Resp)),
    ?assertEqual(tool_use, beamai_llm_response:finish_reason(Resp)),
    [TC] = beamai_llm_response:tool_calls(Resp),
    ?assertEqual(<<"toolu_1">>, maps:get(id, TC)),
    ?assertEqual(<<"get_weather">>, maps:get(name, TC)),
    ?assertEqual(#{<<"city">> => <<"Beijing">>}, maps:get(arguments, TC)),
    %% usage：input_tokens 来自 message_start，output_tokens 来自 message_delta
    ?assertEqual(20, beamai_llm_response:input_tokens(Resp)),
    ?assertEqual(30, beamai_llm_response:output_tokens(Resp)),
    ?assertEqual(50, beamai_llm_response:total_tokens(Resp)).

%%====================================================================
%% Anthropic 流式：thinking 块累加
%%====================================================================

anthropic_stream_thinking_test() ->
    Events = [
        #{<<"type">> => <<"message_start">>,
          <<"message">> => #{<<"id">> => <<"msg_2">>, <<"model">> => <<"claude-sonnet-4-5">>,
                             <<"usage">> => #{<<"input_tokens">> => 5}}},
        #{<<"type">> => <<"content_block_start">>, <<"index">> => 0,
          <<"content_block">> => #{<<"type">> => <<"thinking">>, <<"thinking">> => <<>>}},
        #{<<"type">> => <<"content_block_delta">>, <<"index">> => 0,
          <<"delta">> => #{<<"type">> => <<"thinking_delta">>, <<"thinking">> => <<"Step 1...">>}},
        #{<<"type">> => <<"content_block_delta">>, <<"index">> => 0,
          <<"delta">> => #{<<"type">> => <<"signature_delta">>, <<"signature">> => <<"sig_abc">>}},
        #{<<"type">> => <<"content_block_stop">>, <<"index">> => 0},
        #{<<"type">> => <<"content_block_start">>, <<"index">> => 1,
          <<"content_block">> => #{<<"type">> => <<"text">>, <<"text">> => <<>>}},
        #{<<"type">> => <<"content_block_delta">>, <<"index">> => 1,
          <<"delta">> => #{<<"type">> => <<"text_delta">>, <<"text">> => <<"Done.">>}},
        #{<<"type">> => <<"message_delta">>,
          <<"delta">> => #{<<"stop_reason">> => <<"end_turn">>},
          <<"usage">> => #{<<"output_tokens">> => 8}}
    ],
    Acc = accumulate_anthropic(Events),
    {ok, Resp} = beamai_llm_provider_common:finalize_anthropic_stream(Acc),
    ?assertEqual(<<"Done.">>, beamai_llm_response:content(Resp)),
    ?assertEqual(<<"Step 1...">>, beamai_llm_response:thinking(Resp)),
    ?assertEqual(complete, beamai_llm_response:finish_reason(Resp)),
    %% thinking 块保留 signature（多轮对话回传需要）
    Blocks = beamai_llm_response:content_blocks(Resp),
    ?assertMatch([#{type := thinking, signature := <<"sig_abc">>} | _], Blocks).

%%====================================================================
%% Anthropic 流式：error 事件
%%====================================================================

anthropic_stream_error_test() ->
    Events = [
        #{<<"type">> => <<"message_start">>,
          <<"message">> => #{<<"id">> => <<"msg_3">>, <<"model">> => <<"claude">>,
                             <<"usage">> => #{<<"input_tokens">> => 5}}},
        #{<<"type">> => <<"error">>,
          <<"error">> => #{<<"type">> => <<"overloaded_error">>, <<"message">> => <<"Overloaded">>}}
    ],
    Acc = accumulate_anthropic(Events),
    ?assertMatch({error, {api_error, #{<<"type">> := <<"overloaded_error">>}}},
                 beamai_llm_provider_common:finalize_anthropic_stream(Acc)).

%%====================================================================
%% Anthropic 流式：ping 等未知事件忽略
%%====================================================================

anthropic_stream_ping_ignored_test() ->
    Acc0 = beamai_llm_http_client:init_stream_acc(),
    Acc1 = beamai_llm_provider_common:accumulate_anthropic_event(#{<<"type">> => <<"ping">>}, Acc0),
    ?assertEqual(Acc0, Acc1).
