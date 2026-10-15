%%%-------------------------------------------------------------------
%%% @doc Provider 离线 contract harness
%%%
%%% Harness 只调用 provider 的纯转换边界：request body builder、response
%%% parser、SSE accumulator/finalizer、错误归一化函数。所有用例都可以离线
%%% 运行，不依赖真实 API。新增 provider 后应把模块名加入 providers/0。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_contract).

-export([providers/0, verify/1, assert_error_contract/0,
         assert_multimodal_contract/0]).
-export([style/1, config/1, request/0, tool_request/0]).
-export([stream_chunks/1, feed_sse/3]).

-include_lib("eunit/include/eunit.hrl").

providers() ->
    [beamai_llm_provider_openai,
     beamai_llm_provider_openai_responses,
     beamai_llm_provider_anthropic,
     beamai_llm_provider_deepseek,
     beamai_llm_provider_zhipu,
     beamai_llm_provider_dashscope,
     beamai_llm_provider_ollama,
     beamai_llm_provider_moonshot,
     beamai_llm_provider_openrouter,
     beamai_llm_provider_siliconflow,
     beamai_llm_provider_xai].

style(beamai_llm_provider_anthropic) -> anthropic;
style(beamai_llm_provider_openai_responses) -> responses;
style(beamai_llm_provider_dashscope) -> dashscope;
style(_OpenAICompatible) -> openai.

config(Module) ->
    Defaults = Module:default_config(),
    Overrides = #{max_tokens => 123, temperature => 0.25},
    case Module of
        beamai_llm_provider_ollama -> maps:merge(Defaults, Overrides);
        _ -> maps:merge(Defaults, Overrides#{api_key => <<"contract-key">>})
    end.

request() ->
    beamai_chat_request:new(
      [#{role => system, content => <<"be precise">>},
       #{role => user, content => <<"hello">>}],
      #{max_tokens => 123, temperature => 0.25, tool_choice => auto}).

tool_request() ->
    Tool = #{name => <<"get_city">>,
             description => <<"Get a city">>,
             parameters => #{type => object,
                             properties => #{name => #{type => string}},
                             required => [name]}},
    beamai_chat_request:put_option(request(), tools, [Tool]).

verify(Module) ->
    Config = config(Module),
    ?assertEqual(ok, Module:validate_config(Config)),
    assert_request_contract(Module, Config),
    assert_tool_contract(Module, Config),
    assert_text_response_contract(Module, Config),
    assert_tool_response_contract(Module, Config),
    assert_stream_contract(Module, Config),
    ok.

assert_request_contract(Module, Config) ->
    Body = Module:body(Config, request()),
    Style = case Module of
        beamai_llm_provider_ollama -> ollama;
        _ -> style(Module)
    end,
    ?assert(is_map(Body)),
    ?assert(is_binary(request_model(Module, Config))),
    case Style of
        ollama ->
            Options = maps:get(<<"options">>, Body),
            ?assert(maps:is_key(<<"messages">>, Body)),
            ?assertEqual(123, maps:get(<<"num_predict">>, Options)),
            ?assertEqual(0.25, maps:get(<<"temperature">>, Options));
        _ when Style =:= openai ->
            ?assert(maps:is_key(<<"messages">>, Body)),
            ?assertEqual(123, maps:get(<<"max_tokens">>, Body)),
            ?assertEqual(0.25, maps:get(<<"temperature">>, Body));
        responses ->
            ?assert(maps:is_key(<<"input">>, Body)),
            ?assertEqual(123, maps:get(<<"max_output_tokens">>, Body));
        anthropic ->
            ?assert(maps:is_key(<<"messages">>, Body)),
            ?assertEqual(123, maps:get(<<"max_tokens">>, Body)),
            ?assertEqual(<<"be precise">>, maps:get(<<"system">>, Body));
        dashscope ->
            Parameters = maps:get(<<"parameters">>, Body),
            ?assertMatch(#{<<"input">> := #{<<"messages">> := [_ | _]}}, Body),
            ?assertEqual(123, maps:get(<<"max_tokens">>, Parameters)),
            ?assertEqual(0.25, maps:get(<<"temperature">>, Parameters))
    end.

assert_tool_contract(Module, Config) ->
    Body = Module:body(Config, tool_request()),
    case style(Module) of
        responses ->
            ?assertMatch([#{<<"type">> := <<"function">>,
                            <<"name">> := <<"get_city">>,
                            <<"parameters">> := #{}} | _],
                         maps:get(<<"tools">>, Body));
        dashscope ->
            ?assertMatch([#{<<"type">> := <<"function">>,
                            <<"function">> := #{<<"name">> := <<"get_city">>}} | _],
                         maps:get(<<"tools">>, maps:get(<<"parameters">>, Body)));
        _Style ->
            ?assertMatch([#{name := <<"get_city">>} | _],
                         beamai_llm_tool_adapter:from_provider(
                           maps:get(<<"tools">>, Body), Module))
    end.

assert_text_response_contract(Module, Config) ->
    {ok, Response} = (Module:parser(Config))(text_raw(style(Module))),
    ?assertEqual(<<"contract response">>, beamai_chat_response:content(Response)),
    ?assertEqual(complete, beamai_chat_response:finish_reason(Response)),
    ?assertEqual(17, beamai_chat_response:input_tokens(Response)),
    ?assertEqual(8, beamai_chat_response:output_tokens(Response)),
    ?assertEqual(25, beamai_chat_response:total_tokens(Response)),
    ?assertMatch([#{type := text, text := <<"contract response">>}],
                 beamai_chat_response:content_blocks(Response)).

assert_tool_response_contract(Module, Config) ->
    {ok, Response} = (Module:parser(Config))(tool_raw(style(Module))),
    ?assertEqual(tool_use, beamai_chat_response:finish_reason(Response)),
    ?assertMatch([#{id := <<"call_contract">>,
                    name := <<"get_city">>,
                    arguments := #{<<"name">> := <<"Beijing">>},
                    raw_arguments := <<"{\"name\":\"Beijing\"}">>}],
                 beamai_chat_response:tool_calls(Response)).

assert_stream_contract(Module, Config) ->
    Acc0 = stream_init(style(Module)),
    Accumulator = Module:stream_accumulator(Config),
    Finalizer = Module:stream_finalizer(Config),
    Acc = run_sse(stream_chunks(style(Module)), Acc0, Accumulator),
    {ok, Response} = Finalizer(Acc),
    ?assertEqual(<<"he">>, beamai_chat_response:content(Response)),
    ?assertEqual(complete, beamai_chat_response:finish_reason(Response)),
    ?assertEqual(17, beamai_chat_response:input_tokens(Response)),
    ?assertEqual(8, beamai_chat_response:output_tokens(Response)).

assert_error_contract() ->
    Headers = [{<<"Retry-After">>, <<"7">>},
               {<<"X-RateLimit-Requests-Remaining">>, <<"11">>}],
    Metadata = beamai_llm_provider_common:rate_limit_metadata(Headers),
    ?assertMatch(#{rate_limit := #{<<"retry-after">> := <<"7">>,
                                   <<"requests-remaining">> := <<"11">>}}, Metadata),
    ?assertEqual(7000, beamai_llm_provider_common:retry_after_ms(Headers)),
    Error = beamai_llm_error:from_reason(
              {http_error, 429, <<"rate limited">>,
               #{retry_after_ms => 7000}}, openai),
    ?assertEqual(rate_limit, beamai_llm_error:type(Error)),
    ?assertEqual(429, beamai_llm_error:status(Error)),
    ?assert(beamai_llm_error:retryable(Error)),
    ?assertEqual(7000, beamai_llm_error:retry_after_ms(Error)),
    ?assertNot(beamai_llm_error:retryable(
                 beamai_llm_error:from_reason({http_error, 401, <<"bad">>}, openai))).

assert_multimodal_contract() ->
    Parts = [#{type => text, text => <<"what is this?">>},
             #{type => image,
               source => #{type => url, url => <<"https://example.test/a.png">>}}],
    [OpenAIMessage] = beamai_llm_message_adapter:to_openai(
                        [#{role => user, content => Parts}]),
    ?assertMatch([#{<<"type">> := <<"text">>},
                  #{<<"type">> := <<"image_url">>,
                    <<"image_url">> := #{<<"url">> := <<"https://example.test/a.png">>}}],
                 maps:get(<<"content">>, OpenAIMessage)),
    [AnthropicMessage] = beamai_llm_message_adapter:to_anthropic(
                           [#{role => user, content => Parts}]),
    ?assertMatch([#{<<"type">> := <<"text">>},
                  #{<<"type">> := <<"image">>,
                    <<"source">> := #{<<"type">> := <<"url">>,
                                      <<"url">> := <<"https://example.test/a.png">>}}],
                 maps:get(<<"content">>, AnthropicMessage)).

%%====================================================================
%% Internal
%%====================================================================

request_model(Module, Config) ->
    maps:get(<<"model">>, Module:body(Config, request())).

text_raw(openai) ->
    openai_raw(#{<<"content">> => <<"contract response">>}, <<"stop">>);
text_raw(anthropic) ->
    anthropic_raw([text_block(<<"contract response">>)], <<"end_turn">>);
text_raw(responses) ->
    responses_raw([response_message(<<"contract response">>)], <<"completed">>);
text_raw(dashscope) ->
    dashscope_raw(#{<<"content">> => <<"contract response">>}, <<"stop">>).

tool_raw(openai) ->
    openai_raw(#{<<"content">> => null,
                 <<"tool_calls">> => [openai_tool_call()]}, <<"tool_calls">>);
tool_raw(anthropic) ->
    anthropic_raw([anthropic_tool_block()], <<"tool_use">>);
tool_raw(responses) ->
    responses_raw([response_tool_call()], <<"completed">>);
tool_raw(dashscope) ->
    dashscope_raw(#{<<"content">> => null,
                    <<"tool_calls">> => [openai_tool_call()]}, <<"tool_calls">>).

openai_raw(Message, FinishReason) ->
    #{<<"id">> => <<"resp_contract">>,
      <<"model">> => <<"contract-model">>,
      <<"object">> => <<"chat.completion">>,
      <<"choices">> => [#{<<"index">> => 0,
                          <<"message">> => Message,
                          <<"finish_reason">> => FinishReason}],
      <<"usage">> => openai_usage()}.

anthropic_raw(Content, StopReason) ->
    #{<<"id">> => <<"resp_contract">>,
      <<"model">> => <<"contract-model">>,
      <<"type">> => <<"message">>,
      <<"role">> => <<"assistant">>,
      <<"content">> => Content,
      <<"stop_reason">> => StopReason,
      <<"usage">> => usage()}.

responses_raw(Output, Status) ->
    #{<<"id">> => <<"resp_contract">>,
      <<"model">> => <<"contract-model">>,
      <<"object">> => <<"response">>,
      <<"status">> => Status,
      <<"output">> => Output,
      <<"usage">> => usage()}.

dashscope_raw(Message, FinishReason) ->
    #{<<"request_id">> => <<"resp_contract">>,
      <<"output">> => #{<<"choices">> => [#{<<"message">> => Message,
                                            <<"finish_reason">> => FinishReason}]},
      <<"usage">> => usage()}.

usage() ->
    #{<<"input_tokens">> => 17, <<"output_tokens">> => 8, <<"total_tokens">> => 25}.

openai_usage() ->
    #{<<"prompt_tokens">> => 17, <<"completion_tokens">> => 8,
      <<"total_tokens">> => 25}.

text_block(Text) ->
    #{<<"type">> => <<"text">>, <<"text">> => Text}.

response_message(Text) ->
    #{<<"type">> => <<"message">>,
      <<"role">> => <<"assistant">>,
      <<"content">> => [#{<<"type">> => <<"output_text">>, <<"text">> => Text}]}.

openai_tool_call() ->
    #{<<"id">> => <<"call_contract">>,
      <<"type">> => <<"function">>,
      <<"function">> => #{<<"name">> => <<"get_city">>,
                          <<"arguments">> => <<"{\"name\":\"Beijing\"}">>}}.

anthropic_tool_block() ->
    #{<<"type">> => <<"tool_use">>,
      <<"id">> => <<"call_contract">>,
      <<"name">> => <<"get_city">>,
      <<"input">> => #{<<"name">> => <<"Beijing">>}}.

response_tool_call() ->
    #{<<"type">> => <<"function_call">>,
      <<"call_id">> => <<"call_contract">>,
      <<"name">> => <<"get_city">>,
      <<"arguments">> => <<"{\"name\":\"Beijing\"}">>}.

stream_init(openai) -> #{};
stream_init(anthropic) -> #{};
stream_init(responses) -> #{};
stream_init(dashscope) -> #{finish_reason => <<>>}.

stream_chunks(openai) ->
    Stream = [<<"data: {\"id\":\"resp_contract\",\"model\":\"contract-model\","
                "\"choices\":[{\"index\":0,\"delta\":{\"content\":\"he\"}}]}\n\n">>,
              <<"data: {\"choices\":[{\"index\":0,\"delta\":{},"
                "\"finish_reason\":\"stop\"}]}\n\n">>,
              <<"data: {\"usage\":{\"prompt_tokens\":17,\"completion_tokens\":8,"
                "\"total_tokens\":25}}\n\n">>,
              <<"data: [DONE]\n\n">>],
    split_chunks(Stream);
stream_chunks(anthropic) ->
    Stream = [<<"event: message_start\ndata: {\"type\":\"message_start\","
                "\"message\":{\"id\":\"resp_contract\",\"model\":\"contract-model\","
                "\"usage\":{\"input_tokens\":17}}}\n\n">>,
              <<"event: content_block_start\ndata: {\"type\":\"content_block_start\","
                "\"index\":0,\"content_block\":{\"type\":\"text\"}}\n\n">>,
              <<"event: content_block_delta\ndata: {\"type\":\"content_block_delta\","
                "\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"he\"}}\n\n">>,
              <<"event: message_delta\ndata: {\"type\":\"message_delta\","
                "\"delta\":{\"stop_reason\":\"end_turn\"},"
                "\"usage\":{\"output_tokens\":8}}\n\n">>],
    split_chunks(Stream);
stream_chunks(responses) ->
    Stream = [<<"data: {\"type\":\"response.created\",\"response\":{\"id\":"
                "\"resp_contract\",\"model\":\"contract-model\"}}\n\n">>,
              <<"data: {\"type\":\"response.output_text.delta\",\"output_index\":0,"
                "\"delta\":\"he\"}\n\n">>,
              <<"data: {\"type\":\"response.completed\",\"response\":{\"id\":"
                "\"resp_contract\",\"model\":\"contract-model\",\"status\":"
                "\"completed\",\"output\":[{\"type\":\"message\",\"role\":"
                "\"assistant\",\"content\":[{\"type\":\"output_text\","
                "\"text\":\"he\"}]}],\"usage\":{\"input_tokens\":17,"
                "\"output_tokens\":8,\"total_tokens\":25}}}\n\n">>],
    split_chunks(Stream);
stream_chunks(dashscope) ->
    Stream = [<<"data: {\"request_id\":\"resp_contract\",\"output\":{\"choices\":"
                "[{\"message\":{\"content\":\"he\"},\"finish_reason\":\"stop\"}]},"
                "\"usage\":{\"input_tokens\":17,\"output_tokens\":8,"
                "\"total_tokens\":25}}\n\n">>],
    split_chunks(Stream).

run_sse(Chunks, Acc0, Accumulator) ->
    {_Buffer, Acc} = lists:foldl(
                       fun(Chunk, State) -> feed_sse(Chunk, State, Accumulator) end,
                       {<<>>, Acc0}, Chunks),
    Acc.

split_chunks(Stream) ->
    %% A tiny boundary split exercises SSE buffer reassembly deliberately.
    lists:append([binary_split_every(Chunk, 7) || Chunk <- Stream]).

binary_split_every(Bin, Width) ->
    case Bin of
        <<Part:Width/binary, Rest/binary>> -> [Part | binary_split_every(Rest, Width)];
        _ -> [Bin]
    end.

feed_sse(Chunk, {Buffer, Acc}, Accumulator) ->
    {NewBuffer, Events} = beamai_llm_http_client:parse_sse(<<Buffer/binary, Chunk/binary>>),
    NewAcc = lists:foldl(fun(Event, A) when is_map(Event) -> Accumulator(Event, A);
                            (_, A) -> A
                         end, Acc, Events),
    {NewBuffer, NewAcc};
feed_sse(Chunk, Acc, Accumulator) ->
    feed_sse(Chunk, {<<>>, Acc}, Accumulator).
