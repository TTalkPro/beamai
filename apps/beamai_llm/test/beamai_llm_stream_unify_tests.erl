%%%-------------------------------------------------------------------
%%% @doc zhipu / ollama / dashscope 流式统一响应测试（A）
%%%
%%% 验证这三个 provider 的流式 stream_chat 也返回统一 beamai_llm_response
%%% （与同步路径一致：正确的 provider / content / finish_reason），
%%% 经测试桩 beamai_llm_fake_backend 喂 SSE 分块完成端到端。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_stream_unify_tests).

-include_lib("eunit/include/eunit.hrl").

stream_unify_fixture_test_() ->
    {setup,
     fun() -> beamai_llm_fake_backend:install() end,
     fun(Prev) -> beamai_llm_fake_backend:uninstall(Prev) end,
     fun(_) -> [
        ?_test(zhipu_openai_stream_unified()),
        ?_test(zhipu_openai_stream_tool_calls()),
        ?_test(ollama_stream_unified()),
        ?_test(dashscope_stream_unified())
     ] end}.

%%====================================================================
%% zhipu（OpenAI 兼容模式）
%%====================================================================

zhipu_openai_stream_unified() ->
    beamai_llm_fake_backend:set_stream(openai_text_chunks(), []),
    Config = #{api_key => <<"k">>, model => <<"glm-4.7">>},
    {ok, Resp} = beamai_llm_provider_zhipu:stream_chat(
        Config, #{messages => [#{role => user, content => <<"hi">>}]},
        fun(_E) -> ok end),
    ?assertEqual(zhipu, beamai_llm_response:provider(Resp)),
    ?assertEqual(<<"你好世界"/utf8>>, beamai_llm_response:content(Resp)),
    ?assertEqual(complete, beamai_llm_response:finish_reason(Resp)).

zhipu_openai_stream_tool_calls() ->
    beamai_llm_fake_backend:set_stream(openai_tool_chunks(), []),
    Config = #{api_key => <<"k">>, model => <<"glm-4.7">>},
    {ok, Resp} = beamai_llm_provider_zhipu:stream_chat(
        Config, #{messages => [#{role => user, content => <<"天气"/utf8>>}]},
        fun(_E) -> ok end),
    ?assertEqual(zhipu, beamai_llm_response:provider(Resp)),
    %% 分片工具调用被正确累加（统一格式才有此能力，旧裸 map 没有）
    [TC] = beamai_llm_response:tool_calls(Resp),
    ?assertEqual(<<"get_weather">>, maps:get(name, TC)),
    %% 统一解析器把分片拼接后的 arguments JSON 解码为 map
    ?assertEqual(#{<<"city">> => <<"BJ">>}, maps:get(arguments, TC)),
    ?assertEqual(tool_use, beamai_llm_response:finish_reason(Resp)).

%%====================================================================
%% ollama（OpenAI 兼容端点）
%%====================================================================

ollama_stream_unified() ->
    beamai_llm_fake_backend:set_stream(openai_text_chunks(), []),
    Config = #{model => <<"llama3.2">>},
    {ok, Resp} = beamai_llm_provider_ollama:stream_chat(
        Config, #{messages => [#{role => user, content => <<"hi">>}]},
        fun(_E) -> ok end),
    ?assertEqual(ollama, beamai_llm_response:provider(Resp)),
    ?assertEqual(<<"你好世界"/utf8>>, beamai_llm_response:content(Resp)),
    ?assertEqual(complete, beamai_llm_response:finish_reason(Resp)).

%%====================================================================
%% dashscope（DashScope 原生格式）
%%====================================================================

dashscope_stream_unified() ->
    beamai_llm_fake_backend:set_stream(dashscope_chunks(), []),
    Config = #{api_key => <<"k">>, model => <<"qwen-plus">>},
    {ok, Resp} = beamai_llm_provider_dashscope:stream_chat(
        Config, #{messages => [#{role => user, content => <<"hi">>}]},
        fun(_E) -> ok end),
    ?assertEqual(dashscope, beamai_llm_response:provider(Resp)),
    ?assertEqual(<<"你好世界"/utf8>>, beamai_llm_response:content(Resp)),
    ?assertEqual(complete, beamai_llm_response:finish_reason(Resp)),
    ?assertEqual(3, beamai_llm_response:input_tokens(Resp)),
    ?assertEqual(4, beamai_llm_response:output_tokens(Resp)).

%%====================================================================
%% SSE 分块 fixtures
%%====================================================================

openai_text_chunks() ->
    [
     <<"data: {\"id\":\"c1\",\"model\":\"glm-4.7\",\"choices\":[{\"delta\":{\"content\":\"你好\"},\"finish_reason\":null}]}\n"/utf8>>,
     <<"data: {\"id\":\"c1\",\"model\":\"glm-4.7\",\"choices\":[{\"delta\":{\"content\":\"世界\"},\"finish_reason\":null}]}\n"/utf8>>,
     <<"data: {\"id\":\"c1\",\"model\":\"glm-4.7\",\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}]}\n">>
    ].

openai_tool_chunks() ->
    [
     <<"data: {\"id\":\"c2\",\"model\":\"glm-4.7\",\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_1\",\"function\":{\"name\":\"get_weather\",\"arguments\":\"\"}}]},\"finish_reason\":null}]}\n">>,
     <<"data: {\"id\":\"c2\",\"model\":\"glm-4.7\",\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"{\\\"city\\\":\"}}]},\"finish_reason\":null}]}\n">>,
     <<"data: {\"id\":\"c2\",\"model\":\"glm-4.7\",\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"BJ\\\"}\"}}]},\"finish_reason\":null}]}\n">>,
     <<"data: {\"id\":\"c2\",\"model\":\"glm-4.7\",\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}]}\n">>
    ].

dashscope_chunks() ->
    [
     <<"data: {\"request_id\":\"req-1\",\"output\":{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"你好\"},\"finish_reason\":\"null\"}]},\"usage\":{\"input_tokens\":3,\"output_tokens\":2,\"total_tokens\":5}}\n"/utf8>>,
     <<"data: {\"request_id\":\"req-1\",\"output\":{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"世界\"},\"finish_reason\":\"stop\"}]},\"usage\":{\"input_tokens\":3,\"output_tokens\":4,\"total_tokens\":7}}\n"/utf8>>
    ].
