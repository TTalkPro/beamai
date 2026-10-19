%%%-------------------------------------------------------------------
%%% @doc DeepSeek Provider 特有机制单元测试
%%%
%%% 覆盖：
%%%   - 思考模式开关（thinking）与思考强度（reasoning_effort）的请求构建
%%%   - 思考模式下 tool_choice 的降级、response_format 的 json_schema 剔除
%%%   - 对话前缀续写（Beta）的端点路由与 prefix 透传
%%%   - FIM 填空补全的请求体构建与流式累加
%%%   - 缓存统计（prompt_cache_hit/miss_tokens、prompt_tokens_details.cached_tokens）提取
%%%   - 扩展 finish_reason（insufficient_system_resource / aborted）归一化
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_deepseek_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 思考模式测试
%%====================================================================

thinking_disabled_test() ->
    Config = #{model => <<"deepseek-flash">>, thinking => #{type => disabled}},
    Body = beamai_llm_provider_deepseek:build_request_body(Config, beamai_chat_request:new([])),
    ?assertEqual(#{<<"type">> => <<"disabled">>}, maps:get(<<"thinking">>, Body)),
    ?assertNot(maps:is_key(<<"reasoning_effort">>, Body)).

thinking_bare_atom_test() ->
    Config = #{model => <<"deepseek-flash">>, thinking => disabled},
    Body = beamai_llm_provider_deepseek:build_request_body(Config, beamai_chat_request:new([])),
    ?assertEqual(#{<<"type">> => <<"disabled">>}, maps:get(<<"thinking">>, Body)).

thinking_request_overrides_config_test() ->
    Config = #{model => <<"deepseek-flash">>, thinking => enabled},
    Request = beamai_chat_request:new([], #{thinking => #{type => disabled}}),
    Body = beamai_llm_provider_deepseek:build_request_body(Config, Request),
    ?assertEqual(#{<<"type">> => <<"disabled">>}, maps:get(<<"thinking">>, Body)).

reasoning_effort_aliases_test() ->
    Cases = [{minimal, <<"low">>}, {medium, <<"high">>}, {xhigh, <<"high">>},
             {max, <<"max">>}, {<<"low">>, <<"low">>}],
    lists:foreach(
      fun({Input, Expected}) ->
              Config = #{model => <<"deepseek-flash">>, reasoning_effort => Input},
              Body = beamai_llm_provider_deepseek:build_request_body(
                       Config, beamai_chat_request:new([])),
              ?assertEqual(Expected, maps:get(<<"reasoning_effort">>, Body))
      end, Cases).

thinking_mode_keeps_sampling_params_test() ->
    Config = #{model => <<"deepseek-flash">>, temperature => 0.7, top_p => 0.95,
               logprobs => true, top_logprobs => 5, user_id => <<"user-1">>},
    Body = beamai_llm_provider_deepseek:build_request_body(Config, beamai_chat_request:new([])),
    ?assertEqual(0.7, maps:get(<<"temperature">>, Body)),
    ?assertEqual(0.95, maps:get(<<"top_p">>, Body)),
    ?assertEqual(true, maps:get(<<"logprobs">>, Body)),
    ?assertEqual(5, maps:get(<<"top_logprobs">>, Body)),
    ?assertEqual(<<"user-1">>, maps:get(<<"user_id">>, Body)).

deprecated_params_never_sent_test() ->
    Config = #{model => <<"deepseek-flash">>, frequency_penalty => 0.5, presence_penalty => 0.5},
    Body = beamai_llm_provider_deepseek:build_request_body(Config, beamai_chat_request:new([])),
    ?assertNot(maps:is_key(<<"frequency_penalty">>, Body)),
    ?assertNot(maps:is_key(<<"presence_penalty">>, Body)).

json_schema_response_format_dropped_test() ->
    Config = #{model => <<"deepseek-flash">>, response_format => #{type => json_schema}},
    Body = beamai_llm_provider_deepseek:build_request_body(Config, beamai_chat_request:new([])),
    ?assertNot(maps:is_key(<<"response_format">>, Body)).

json_object_response_format_kept_test() ->
    Config = #{model => <<"deepseek-flash">>, response_format => #{type => json_object}},
    Body = beamai_llm_provider_deepseek:build_request_body(Config, beamai_chat_request:new([])),
    ?assertEqual(#{<<"type">> => <<"json_object">>}, maps:get(<<"response_format">>, Body)).

thinking_downgrades_required_tool_choice_test() ->
    Config = #{model => <<"deepseek-flash">>},
    Request = beamai_chat_request:new([], #{tool_choice => required}),
    Body = beamai_llm_provider_deepseek:build_request_body(Config, Request),
    ?assertEqual(<<"auto">>, maps:get(<<"tool_choice">>, Body)).

thinking_downgrades_named_tool_choice_test() ->
    Config = #{model => <<"deepseek-flash">>},
    Request = beamai_chat_request:new([], #{tool_choice => #{type => <<"function">>,
                                                     function => #{name => <<"search">>}}}),
    Body = beamai_llm_provider_deepseek:build_request_body(Config, Request),
    ?assertEqual(<<"auto">>, maps:get(<<"tool_choice">>, Body)).

non_thinking_keeps_required_tool_choice_test() ->
    Config = #{model => <<"deepseek-flash">>, thinking => disabled},
    Request = beamai_chat_request:new([], #{tool_choice => required}),
    Body = beamai_llm_provider_deepseek:build_request_body(Config, Request),
    ?assertEqual(<<"required">>, maps:get(<<"tool_choice">>, Body)).

reasoning_effort_none_disables_thinking_restriction_test() ->
    Config = #{model => <<"deepseek-flash">>, reasoning_effort => none},
    Request = beamai_chat_request:new([], #{tool_choice => required}),
    Body = beamai_llm_provider_deepseek:build_request_body(Config, Request),
    ?assertEqual(<<"required">>, maps:get(<<"tool_choice">>, Body)).

%%====================================================================
%% Chat Prefix Completion（beta）测试
%%====================================================================

prefix_routes_to_beta_endpoint_test() ->
    Request = beamai_chat_request:new([
        #{role => user, content => <<"写一首诗"/utf8>>},
        #{role => assistant, content => <<"床前明月光，"/utf8>>, prefix => true}
    ]),
    ?assertEqual(<<"/beta/chat/completions">>,
                 beamai_llm_provider_deepseek:chat_endpoint(Request)).

no_prefix_uses_default_endpoint_test() ->
    Request = beamai_chat_request:new([#{role => user, content => <<"hi">>}]),
    ?assertEqual(<<"/chat/completions">>,
                 beamai_llm_provider_deepseek:chat_endpoint(Request)).

prefix_passthrough_in_message_adapter_test() ->
    Messages = [
        #{role => user, content => <<"q">>},
        #{role => assistant, content => <<"```python\n">>, prefix => true}
    ],
    [UserMsg, AssistantMsg] = beamai_llm_message_adapter:to_openai(Messages),
    %% 普通消息不携带 prefix 字段
    ?assertNot(maps:is_key(<<"prefix">>, UserMsg)),
    %% prefix => true 透传到 API 格式
    ?assertEqual(true, maps:get(<<"prefix">>, AssistantMsg)).

%%====================================================================
%% FIM 填空补全测试
%%====================================================================

fim_request_body_test() ->
    Config = #{model => <<"deepseek-flash">>, temperature => 0.0, max_tokens => 128},
    Request = #{
        prompt => <<"def fib(n):">>,
        suffix => <<"    return fib(n-1) + fib(n-2)">>,
        echo => false,
        logprobs => 5
    },
    Body = beamai_llm_provider_deepseek:build_fim_request_body(Config, Request),
    ?assertEqual(<<"def fib(n):">>, maps:get(<<"prompt">>, Body)),
    ?assertEqual(<<"    return fib(n-1) + fib(n-2)">>, maps:get(<<"suffix">>, Body)),
    ?assertEqual(false, maps:get(<<"echo">>, Body)),
    ?assertEqual(5, maps:get(<<"logprobs">>, Body)),
    ?assertEqual(128, maps:get(<<"max_tokens">>, Body)),
    ?assertEqual(0.0, maps:get(<<"temperature">>, Body)),
    %% 非流式时不携带 stream
    ?assertNot(maps:is_key(<<"stream">>, Body)).

fim_stream_accumulate_test() ->
    %% Completions 格式流式：文本片段直接在 choices[0].text
    Events = [
        #{<<"id">> => <<"fim-1">>, <<"model">> => <<"deepseek-flash">>,
          <<"choices">> => [#{<<"text">> => <<"    if n <= 1:">>, <<"finish_reason">> => null}]},
        #{<<"choices">> => [#{<<"text">> => <<" return n\n">>, <<"finish_reason">> => null}]},
        #{<<"choices">> => [#{<<"text">> => <<>>, <<"finish_reason">> => <<"stop">>}],
          <<"usage">> => #{<<"prompt_tokens">> => 8, <<"completion_tokens">> => 12,
                           <<"total_tokens">> => 20}}
    ],
    Acc = lists:foldl(fun beamai_llm_provider_common:accumulate_completions_event/2,
                      beamai_llm_http_client:init_stream_acc(), Events),
    {ok, Resp} = beamai_llm_provider_common:finalize_completions_stream(
                     Acc, beamai_llm_response_parser:parser_deepseek_fim()),
    ?assertEqual(deepseek, beamai_chat_response:provider(Resp)),
    ?assertEqual(<<"    if n <= 1: return n\n">>, beamai_chat_response:content(Resp)),
    ?assertEqual(complete, beamai_chat_response:finish_reason(Resp)),
    ?assertEqual(8, beamai_chat_response:input_tokens(Resp)),
    ?assertEqual(12, beamai_chat_response:output_tokens(Resp)).

fim_endpoint_uses_beta_base_test() ->
    %% FIM 要求 base_url=https://api.deepseek.com/beta，本实现用 <base_url>/beta/completions 表达
    ?assertEqual(<<"https://api.deepseek.com/beta/completions">>,
                 beamai_llm_provider_deepseek:build_fim_url(#{})).

%%====================================================================
%% 缓存统计与 finish_reason 测试
%%====================================================================

cache_stats_in_usage_details_test() ->
    Raw = #{
        <<"id">> => <<"ds-cache">>,
        <<"model">> => <<"deepseek-flash">>,
        <<"choices">> => [#{
            <<"message">> => #{<<"role">> => <<"assistant">>, <<"content">> => <<"hi">>},
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{
            <<"prompt_tokens">> => 100,
            <<"completion_tokens">> => 10,
            <<"total_tokens">> => 110,
            <<"prompt_cache_hit_tokens">> => 64,
            <<"prompt_cache_miss_tokens">> => 36,
            <<"prompt_tokens_details">> => #{<<"cached_tokens">> => 64},
            <<"completion_tokens_details">> => #{<<"reasoning_tokens">> => 7}
        }
    },
    {ok, Resp} = beamai_llm_response_parser:from_deepseek(Raw),
    Usage = beamai_chat_response:usage(Resp),
    ?assertEqual(100, maps:get(input_tokens, Usage)),
    Details = maps:get(details, Usage),
    ?assertEqual(64, maps:get(prompt_cache_hit_tokens, Details)),
    ?assertEqual(36, maps:get(prompt_cache_miss_tokens, Details)),
    ?assertEqual(64, maps:get(cached_tokens, Details)).

no_cache_stats_no_details_test() ->
    Raw = #{
        <<"id">> => <<"ds-nocache">>,
        <<"model">> => <<"deepseek-flash">>,
        <<"choices">> => [#{
            <<"message">> => #{<<"role">> => <<"assistant">>, <<"content">> => <<"hi">>},
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{<<"prompt_tokens">> => 5, <<"completion_tokens">> => 5,
                         <<"total_tokens">> => 10}
    },
    {ok, Resp} = beamai_llm_response_parser:from_deepseek(Raw),
    ?assertNot(maps:is_key(details, beamai_chat_response:usage(Resp))).

extended_finish_reason_test() ->
    Raws = [{<<"insufficient_system_resource">>, insufficient_resource},
            {<<"aborted">>, aborted},
            {<<"length">>, length_limit}],
    lists:foreach(
      fun({Raw, Expected}) ->
              Raw0 = #{
                  <<"id">> => <<"ds-finish">>,
                  <<"model">> => <<"deepseek-flash">>,
                  <<"choices">> => [#{
                      <<"message">> => #{<<"role">> => <<"assistant">>, <<"content">> => <<"x">>},
                      <<"finish_reason">> => Raw
                  }],
                  <<"usage">> => #{<<"prompt_tokens">> => 1, <<"completion_tokens">> => 1,
                                   <<"total_tokens">> => 2}
              },
              {ok, Resp} = beamai_llm_response_parser:from_deepseek(Raw0),
              ?assertEqual(Expected, beamai_chat_response:finish_reason(Resp))
      end, Raws).
