%%%-------------------------------------------------------------------
%%% @doc OpenAI Responses API provider 单元测试
%%%
%%% 覆盖：
%%%   - Provider 路由 / 默认配置 / URL
%%%   - 请求体构建（instructions、input 项、参数优先级、reasoning、text.format、
%%%     扁平工具与 tool_choice）
%%%   - 消息适配（assistant 的 reasoning / function_call 回放、tool 结果、多模态）
%%%   - 响应解析（output 项归一、finish_reason、usage 明细、错误）
%%%   - 流式（经 beamai_llm_fake_backend 喂语义事件：完整流、截断流、token 回调）
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_openai_responses_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MESSAGES, [#{role => user, content => <<"hello">>}]).

cfg() -> cfg(#{}).
cfg(Extra) ->
    beamai_chat_model:create(openai_responses, maps:merge(#{api_key => <<"k">>}, Extra)).

body(Config, Messages, Options) ->
    beamai_llm_provider_openai_responses:build_request_body(
        Config, beamai_chat_request:new(Messages, Options)).

%%====================================================================
%% 路由与配置
%%====================================================================

provider_routing_test() ->
    Config = cfg(),
    ?assertEqual(openai_responses, maps:get(provider, Config)),
    ?assertEqual(<<"gpt-5">>, maps:get(model, Config)),
    ?assertEqual(<<"https://api.openai.com/v1/responses">>,
                 beamai_llm_http_provider:url(beamai_llm_provider_openai_responses, Config,
                                              beamai_chat_request:new([]))),
    Proxy = cfg(#{base_url => <<"http://127.0.0.1:4000">>}),
    ?assertEqual(<<"http://127.0.0.1:4000/v1/responses">>,
                 beamai_llm_http_provider:url(beamai_llm_provider_openai_responses, Proxy,
                                              beamai_chat_request:new([]))),
    ?assertEqual({error, missing_api_key}, beamai_llm_provider_openai_responses:validate_config(#{})),
    ?assert(beamai_llm_provider_openai_responses:supports_tools()),
    ?assert(beamai_llm_provider_openai_responses:supports_streaming()).

%%====================================================================
%% 请求体
%%====================================================================

body_basics_test() ->
    Config = cfg(#{max_tokens => 300}),
    Msgs = [#{role => system, content => <<"Be terse.">>},
            #{role => system, content => <<"Answer in English.">>},
            #{role => user, content => <<"hi">>}],
    Body = body(Config, Msgs, #{}),
    ?assertEqual(<<"gpt-5">>, maps:get(<<"model">>, Body)),
    ?assertEqual(<<"Be terse.\n\nAnswer in English.">>, maps:get(<<"instructions">>, Body)),
    ?assertEqual([#{<<"type">> => <<"message">>, <<"role">> => <<"user">>, <<"content">> => <<"hi">>}],
                 maps:get(<<"input">>, Body)),
    ?assertEqual(300, maps:get(<<"max_output_tokens">>, Body)),
    %% 推理模型拒绝 temperature：不配置就不发
    ?assertNot(maps:is_key(<<"temperature">>, Body)),
    ?assertNot(maps:is_key(<<"tools">>, Body)),
    ?assertNot(maps:is_key(<<"stream">>, Body)).

body_options_override_config_test() ->
    Config = cfg(#{temperature => 0.2, max_tokens => 100, store => false,
                   instructions => <<"From config.">>}),
    Body = body(Config, [#{role => system, content => <<"From message.">>} | ?MESSAGES],
                #{model => <<"gpt-4.1">>, temperature => 0.9, max_output_tokens => 50,
                  previous_response_id => <<"resp_1">>, stream => true}),
    ?assertEqual(<<"gpt-4.1">>, maps:get(<<"model">>, Body)),
    ?assertEqual(0.9, maps:get(<<"temperature">>, Body)),
    ?assertEqual(50, maps:get(<<"max_output_tokens">>, Body)),
    ?assertEqual(false, maps:get(<<"store">>, Body)),
    ?assertEqual(<<"resp_1">>, maps:get(<<"previous_response_id">>, Body)),
    ?assertEqual(<<"From config.\n\nFrom message.">>, maps:get(<<"instructions">>, Body)),
    ?assert(maps:get(<<"stream">>, Body)).

body_reasoning_test() ->
    Body1 = body(cfg(#{reasoning_effort => <<"high">>, reasoning_summary => auto}), ?MESSAGES, #{}),
    ?assertEqual(#{<<"effort">> => <<"high">>, <<"summary">> => <<"auto">>}, maps:get(<<"reasoning">>, Body1)),
    Body2 = body(cfg(), ?MESSAGES, #{reasoning => #{effort => <<"low">>}}),
    ?assertEqual(#{<<"effort">> => <<"low">>}, maps:get(<<"reasoning">>, Body2)),
    ?assertNot(maps:is_key(<<"reasoning">>, body(cfg(), ?MESSAGES, #{}))).

body_text_format_test() ->
    Schema = #{<<"type">> => <<"object">>},
    Chat = #{type => json_schema, json_schema => #{name => <<"out">>, schema => Schema, strict => true}},
    Body1 = body(cfg(), ?MESSAGES, #{response_format => Chat}),
    ?assertEqual(#{<<"format">> => #{<<"type">> => <<"json_schema">>, <<"name">> => <<"out">>,
                                     <<"schema">> => Schema, <<"strict">> => true}},
                 maps:get(<<"text">>, Body1)),
    Body2 = body(cfg(), ?MESSAGES, #{response_format => #{type => json_object}}),
    ?assertEqual(#{<<"format">> => #{<<"type">> => <<"json_object">>}}, maps:get(<<"text">>, Body2)),
    %% Responses 原生 text 优先
    Native = #{format => #{<<"type">> => <<"text">>}, verbosity => <<"low">>},
    Body3 = body(cfg(), ?MESSAGES, #{text => Native, response_format => Chat}),
    ?assertEqual(#{<<"format">> => #{<<"type">> => <<"text">>}, <<"verbosity">> => <<"low">>},
                 maps:get(<<"text">>, Body3)).

body_tools_test() ->
    Unified = #{name => <<"get_weather">>, description => <<"Weather">>,
                parameters => #{<<"type">> => <<"object">>}, strict => true},
    %% ChatClient tool registry 产出的 binary 键 Chat 格式
    Registry = #{<<"type">> => <<"function">>,
                 <<"function">> => #{<<"name">> => <<"search">>, <<"description">> => <<"S">>,
                                     <<"parameters">> => #{<<"type">> => <<"object">>}}},
    Builtin = #{<<"type">> => <<"web_search">>},
    Body = body(cfg(), ?MESSAGES, #{tools => [Unified, Registry, Builtin], tool_choice => {tool, <<"search">>}}),
    ?assertEqual([#{<<"type">> => <<"function">>, <<"name">> => <<"get_weather">>,
                    <<"description">> => <<"Weather">>, <<"parameters">> => #{<<"type">> => <<"object">>},
                    <<"strict">> => true},
                  #{<<"type">> => <<"function">>, <<"name">> => <<"search">>,
                    <<"description">> => <<"S">>, <<"parameters">> => #{<<"type">> => <<"object">>}},
                  Builtin],
                 maps:get(<<"tools">>, Body)),
    ?assertEqual(#{<<"type">> => <<"function">>, <<"name">> => <<"search">>}, maps:get(<<"tool_choice">>, Body)),
    Body2 = body(cfg(), ?MESSAGES, #{tools => [Unified], tool_choice => required}),
    ?assertEqual(<<"required">>, maps:get(<<"tool_choice">>, Body2)).

%%====================================================================
%% 消息适配
%%====================================================================

input_conversion_round_trip_test() ->
    Reasoning = #{type => reasoning, id => <<"rs_1">>, text => <<"think">>, summary => [<<"sum">>],
                  encrypted_content => <<"enc">>},
    Msgs = [
        #{role => system, content => <<"sys">>},
        #{role => user, content => <<"weather?">>},
        #{role => assistant, content => null,
          tool_calls => [#{id => <<"call_1">>, type => function,
                           function => #{name => <<"get_weather">>, arguments => <<"{\"city\":\"P\"}">>}}],
          content_blocks => [Reasoning, #{type => tool_use, id => <<"call_1">>, name => <<"get_weather">>, input => #{}}]},
        #{role => tool, tool_call_id => <<"call_1">>, name => <<"get_weather">>, content => <<"18C">>},
        #{role => assistant, content => <<"It is 18C.">>}
    ],
    {Instructions, Items} = beamai_llm_message_adapter:to_openai_responses(Msgs),
    ?assertEqual(<<"sys">>, Instructions),
    ?assertEqual([
        #{<<"type">> => <<"message">>, <<"role">> => <<"user">>, <<"content">> => <<"weather?">>},
        #{<<"type">> => <<"reasoning">>, <<"id">> => <<"rs_1">>, <<"encrypted_content">> => <<"enc">>,
          <<"summary">> => [#{<<"type">> => <<"summary_text">>, <<"text">> => <<"sum">>}],
          <<"content">> => [#{<<"type">> => <<"reasoning_text">>, <<"text">> => <<"think">>}]},
        #{<<"type">> => <<"function_call">>, <<"call_id">> => <<"call_1">>, <<"name">> => <<"get_weather">>,
          <<"arguments">> => <<"{\"city\":\"P\"}">>},
        #{<<"type">> => <<"function_call_output">>, <<"call_id">> => <<"call_1">>, <<"output">> => <<"18C">>},
        #{<<"type">> => <<"message">>, <<"role">> => <<"assistant">>,
          <<"content">> => [#{<<"type">> => <<"output_text">>, <<"text">> => <<"It is 18C.">>}]}
    ], Items).

input_conversion_multimodal_test() ->
    Parts = [beamai_llm_content:text(<<"look">>),
             #{type => image, source => #{type => url, url => <<"https://x/a.png">>}, detail => <<"low">>},
             #{type => document, source => #{type => file_id, file_id => <<"file_1">>}}],
    {undefined, [Item]} = beamai_llm_message_adapter:to_openai_responses([#{role => user, content => Parts}]),
    ?assertEqual([#{<<"type">> => <<"input_text">>, <<"text">> => <<"look">>},
                  #{<<"type">> => <<"input_image">>, <<"image_url">> => <<"https://x/a.png">>, <<"detail">> => <<"low">>},
                  #{<<"type">> => <<"input_file">>, <<"file_id">> => <<"file_1">>}],
                 maps:get(<<"content">>, Item)).

%% map 形态的 tool_call arguments 编码成 JSON 文本
input_conversion_map_arguments_test() ->
    Msg = #{role => assistant, content => null,
            tool_calls => [#{id => <<"c">>, name => <<"f">>, arguments => #{<<"a">> => 1}}]},
    {undefined, [Call]} = beamai_llm_message_adapter:to_openai_responses([Msg]),
    ?assertEqual(#{<<"a">> => 1}, json:decode(maps:get(<<"arguments">>, Call))).

%%====================================================================
%% 响应解析
%%====================================================================

completed_response() ->
    #{<<"id">> => <<"resp_1">>, <<"object">> => <<"response">>, <<"model">> => <<"m">>,
      <<"status">> => <<"completed">>, <<"error">> => null, <<"incomplete_details">> => null,
      <<"created_at">> => 1, <<"previous_response_id">> => null,
      <<"output">> => [
          #{<<"type">> => <<"reasoning">>, <<"id">> => <<"rs_1">>, <<"encrypted_content">> => null,
            <<"summary">> => [], <<"content">> => [#{<<"type">> => <<"reasoning_text">>, <<"text">> => <<"思考"/utf8>>}]},
          #{<<"type">> => <<"message">>, <<"id">> => <<"msg_1">>, <<"role">> => <<"assistant">>,
            <<"content">> => [#{<<"type">> => <<"output_text">>, <<"text">> => <<"查一下"/utf8>>, <<"annotations">> => []}]},
          #{<<"type">> => <<"function_call">>, <<"id">> => <<"fc_1">>, <<"call_id">> => <<"call_1">>,
            <<"name">> => <<"get_weather">>, <<"arguments">> => <<"{\"city\":\"BJ\"}">>}
      ],
      <<"usage">> => #{<<"input_tokens">> => 10, <<"output_tokens">> => 5, <<"total_tokens">> => 15,
                       <<"input_tokens_details">> => #{<<"cached_tokens">> => 4},
                       <<"output_tokens_details">> => #{<<"reasoning_tokens">> => 3}}}.

parse_completed_test() ->
    {ok, Resp} = beamai_llm_response_parser:from_openai_responses(completed_response()),
    ?assertEqual(openai_responses, beamai_chat_response:provider(Resp)),
    ?assertEqual(<<"resp_1">>, beamai_chat_response:id(Resp)),
    ?assertEqual(<<"查一下"/utf8>>, beamai_chat_response:content(Resp)),
    ?assertEqual(tool_use, beamai_chat_response:finish_reason(Resp)),
    [TC] = beamai_chat_response:tool_calls(Resp),
    ?assertEqual(<<"call_1">>, maps:get(id, TC)),
    ?assertEqual(#{<<"city">> => <<"BJ">>}, maps:get(arguments, TC)),
    ?assertEqual([reasoning, text, tool_use],
                 [maps:get(type, B) || B <- beamai_chat_response:content_blocks(Resp)]),
    [#{id := <<"rs_1">>, text := <<"思考"/utf8>>, encrypted_content := null} | _] =
        beamai_chat_response:content_blocks(Resp),
    ?assertEqual(<<"思考"/utf8>>, beamai_chat_response:reasoning_content(Resp)),
    ?assertEqual(15, beamai_chat_response:total_tokens(Resp)),
    ?assertEqual(#{cached_tokens => 4, reasoning_tokens => 3},
                 maps:get(details, beamai_chat_response:usage(Resp))),
    %% 经 parser/1 路由也能到
    ?assertMatch({ok, _}, (beamai_llm_response_parser:parser(openai_responses))(completed_response())).

parse_status_and_errors_test() ->
    Base = completed_response(),
    Text = Base#{<<"output">> => [lists:nth(2, maps:get(<<"output">>, Base))]},
    {ok, R0} = beamai_llm_response_parser:from_openai_responses(Text),
    ?assertEqual(complete, beamai_chat_response:finish_reason(R0)),
    Incomplete = Text#{<<"status">> => <<"incomplete">>,
                       <<"incomplete_details">> => #{<<"reason">> => <<"max_output_tokens">>}},
    {ok, R1} = beamai_llm_response_parser:from_openai_responses(Incomplete),
    ?assertEqual(length_limit, beamai_chat_response:finish_reason(R1)),
    Filtered = Incomplete#{<<"incomplete_details">> => #{<<"reason">> => <<"content_filter">>}},
    {ok, R2} = beamai_llm_response_parser:from_openai_responses(Filtered),
    ?assertEqual(content_filtered, beamai_chat_response:finish_reason(R2)),
    Refusal = Base#{<<"output">> => [#{<<"type">> => <<"message">>, <<"role">> => <<"assistant">>,
                                       <<"content">> => [#{<<"type">> => <<"refusal">>, <<"refusal">> => <<"no">>}]}]},
    {ok, R3} = beamai_llm_response_parser:from_openai_responses(Refusal),
    ?assertEqual(refusal, beamai_chat_response:finish_reason(R3)),
    ?assertEqual(null, beamai_chat_response:content(R3)),
    ?assertEqual(<<"no">>, maps:get(refusal, beamai_chat_response:metadata(R3))),
    ?assertEqual({error, {api_error, #{<<"message">> => <<"bad">>}}},
                 beamai_llm_response_parser:from_openai_responses(#{<<"error">> => #{<<"message">> => <<"bad">>}})),
    ?assertMatch({error, {invalid_response, _}},
                 beamai_llm_response_parser:from_openai_responses(#{<<"choices">> => []})).

%%====================================================================
%% 流式（fake backend）
%%====================================================================

stream_fixture_test_() ->
    {setup,
     fun() -> beamai_llm_fake_backend:install() end,
     fun(Prev) -> beamai_llm_fake_backend:uninstall(Prev) end,
     fun(_) -> [
        ?_test(stream_completed()),
        ?_test(stream_truncated_rebuilds()),
        ?_test(stream_token_callback_only_output_text()),
        ?_test(sync_via_chat_model())
     ] end}.

sse(Map) ->
    <<"data: ", (beamai_utils:encode_json(Map))/binary, "\n">>.

stream_chunks() ->
    Final = completed_response(),
    [sse(#{<<"type">> => <<"response.created">>, <<"response">> => #{<<"id">> => <<"resp_1">>, <<"model">> => <<"m">>}}),
     sse(#{<<"type">> => <<"response.output_item.added">>, <<"output_index">> => 0,
           <<"item">> => #{<<"type">> => <<"reasoning">>, <<"id">> => <<"rs_1">>, <<"content">> => []}}),
     sse(#{<<"type">> => <<"response.reasoning_text.delta">>, <<"output_index">> => 0, <<"delta">> => <<"思"/utf8>>}),
     sse(#{<<"type">> => <<"response.reasoning_text.delta">>, <<"output_index">> => 0, <<"delta">> => <<"考"/utf8>>}),
     sse(#{<<"type">> => <<"response.output_item.added">>, <<"output_index">> => 1,
           <<"item">> => #{<<"type">> => <<"message">>, <<"role">> => <<"assistant">>, <<"content">> => []}}),
     sse(#{<<"type">> => <<"response.output_text.delta">>, <<"output_index">> => 1, <<"delta">> => <<"查"/utf8>>}),
     sse(#{<<"type">> => <<"response.output_text.delta">>, <<"output_index">> => 1, <<"delta">> => <<"一下"/utf8>>}),
     sse(#{<<"type">> => <<"response.output_item.added">>, <<"output_index">> => 2,
           <<"item">> => #{<<"type">> => <<"function_call">>, <<"call_id">> => <<"call_1">>,
                           <<"name">> => <<"get_weather">>, <<"arguments">> => <<>>}}),
     sse(#{<<"type">> => <<"response.function_call_arguments.delta">>, <<"output_index">> => 2, <<"delta">> => <<"{\"city\":">>}),
     sse(#{<<"type">> => <<"response.function_call_arguments.delta">>, <<"output_index">> => 2, <<"delta">> => <<"\"BJ\"}">>}),
     sse(#{<<"type">> => <<"response.output_item.done">>, <<"output_index">> => 2,
           <<"item">> => lists:nth(3, maps:get(<<"output">>, Final))}),
     sse(#{<<"type">> => <<"response.completed">>, <<"response">> => Final})].

stream_completed() ->
    beamai_llm_fake_backend:set_stream(stream_chunks(), []),
    {ok, Resp} = beamai_llm_provider_openai_responses:stream_chat(
        cfg(), beamai_chat_request:new(?MESSAGES), fun(_E) -> ok end),
    ?assertEqual(openai_responses, beamai_chat_response:provider(Resp)),
    ?assertEqual(<<"查一下"/utf8>>, beamai_chat_response:content(Resp)),
    ?assertEqual(tool_use, beamai_chat_response:finish_reason(Resp)),
    [#{name := <<"get_weather">>, arguments := #{<<"city">> := <<"BJ">>}}] = beamai_chat_response:tool_calls(Resp),
    ?assertEqual(15, beamai_chat_response:total_tokens(Resp)).

%% 没等到 response.completed：用 added 项 + delta 片段重建，finish 标为 length_limit
stream_truncated_rebuilds() ->
    Chunks = lists:droplast(lists:droplast(stream_chunks())),
    beamai_llm_fake_backend:set_stream(Chunks, []),
    {ok, Resp} = beamai_llm_provider_openai_responses:stream_chat(
        cfg(), beamai_chat_request:new(?MESSAGES), fun(_E) -> ok end),
    ?assertEqual(<<"resp_1">>, beamai_chat_response:id(Resp)),
    ?assertEqual(<<"查一下"/utf8>>, beamai_chat_response:content(Resp)),
    ?assertEqual(<<"思考"/utf8>>, beamai_chat_response:reasoning_content(Resp)),
    [#{id := <<"call_1">>, arguments := #{<<"city">> := <<"BJ">>}}] = beamai_chat_response:tool_calls(Resp),
    ?assertEqual(length_limit, beamai_chat_response:finish_reason(Resp)).

%% chat_model 的 token 回调只投递 output_text.delta，推理 / 参数片段不投
stream_token_callback_only_output_text() ->
    beamai_llm_fake_backend:set_stream(stream_chunks(), []),
    Self = self(),
    {ok, _} = beamai_chat_model:stream_chat(cfg(), ?MESSAGES, fun(_E) -> ok end,
        #{on_llm_new_token => fun(Tok, _Meta) -> Self ! {tok, Tok} end}),
    ?assertEqual([<<"查"/utf8>>, <<"一下"/utf8>>], drain([])).

sync_via_chat_model() ->
    beamai_llm_fake_backend:set_response(completed_response(), []),
    {ok, Resp} = beamai_chat_model:chat(cfg(), ?MESSAGES, #{max_retries => 0}),
    ?assertEqual(openai_responses, beamai_chat_response:provider(Resp)),
    ?assertEqual(<<"查一下"/utf8>>, beamai_chat_response:content(Resp)),
    %% from_response 保留 reasoning 块与文本，回放时适配器能拼回 reasoning/message 项
    Msg = beamai_message:from_response(Resp),
    ?assertMatch([#{type := reasoning, id := <<"rs_1">>} | _], beamai_message:content_blocks(Msg)),
    {_, Items} = beamai_llm_message_adapter:to_openai_responses([Msg]),
    ?assertEqual([<<"reasoning">>, <<"message">>, <<"function_call">>],
                 [maps:get(<<"type">>, I) || I <- Items]).

drain(Acc) ->
    receive {tok, T} -> drain([T | Acc]) after 0 -> lists:reverse(Acc) end.
