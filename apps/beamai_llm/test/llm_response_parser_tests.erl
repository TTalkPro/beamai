%%%-------------------------------------------------------------------
%%% @doc llm_response_parser 模块单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(llm_response_parser_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% OpenAI 格式测试
%%====================================================================

from_openai_basic_test() ->
    Raw = #{
        <<"id">> => <<"chatcmpl-123">>,
        <<"object">> => <<"chat.completion">>,
        <<"created">> => 1234567890,
        <<"model">> => <<"gpt-4">>,
        <<"choices">> => [#{
            <<"index">> => 0,
            <<"message">> => #{
                <<"role">> => <<"assistant">>,
                <<"content">> => <<"Hello, world!">>
            },
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{
            <<"prompt_tokens">> => 10,
            <<"completion_tokens">> => 20,
            <<"total_tokens">> => 30
        }
    },
    {ok, Resp} = llm_response_parser:from_openai(Raw),

    %% 基本字段
    ?assertEqual(<<"chatcmpl-123">>, llm_response:id(Resp)),
    ?assertEqual(<<"gpt-4">>, llm_response:model(Resp)),
    ?assertEqual(openai, llm_response:provider(Resp)),

    %% 内容
    ?assertEqual(<<"Hello, world!">>, llm_response:content(Resp)),
    ?assertEqual([#{type => text, text => <<"Hello, world!">>}], llm_response:content_blocks(Resp)),

    %% 工具调用
    ?assertEqual([], llm_response:tool_calls(Resp)),
    ?assertEqual(false, llm_response:has_tool_calls(Resp)),

    %% 状态
    ?assertEqual(complete, llm_response:finish_reason(Resp)),
    ?assertEqual(true, llm_response:is_complete(Resp)),
    ?assertEqual(false, llm_response:needs_tool_call(Resp)),

    %% Token 统计
    ?assertEqual(10, llm_response:input_tokens(Resp)),
    ?assertEqual(20, llm_response:output_tokens(Resp)),
    ?assertEqual(30, llm_response:total_tokens(Resp)),

    %% 原始数据
    ?assertEqual(Raw, llm_response:raw(Resp)),
    ?assertEqual(<<"gpt-4">>, llm_response:raw_get(Resp, <<"model">>)),
    ?assertEqual(1234567890, llm_response:raw_get(Resp, [<<"created">>])).

from_openai_with_tool_calls_test() ->
    Raw = #{
        <<"id">> => <<"chatcmpl-456">>,
        <<"model">> => <<"gpt-4">>,
        <<"choices">> => [#{
            <<"message">> => #{
                <<"role">> => <<"assistant">>,
                <<"content">> => null,
                <<"tool_calls">> => [#{
                    <<"id">> => <<"call_abc123">>,
                    <<"type">> => <<"function">>,
                    <<"function">> => #{
                        <<"name">> => <<"get_weather">>,
                        <<"arguments">> => <<"{\"city\": \"Beijing\"}">>
                    }
                }]
            },
            <<"finish_reason">> => <<"tool_calls">>
        }],
        <<"usage">> => #{
            <<"prompt_tokens">> => 15,
            <<"completion_tokens">> => 25,
            <<"total_tokens">> => 40
        }
    },
    {ok, Resp} = llm_response_parser:from_openai(Raw),

    ?assertEqual(null, llm_response:content(Resp)),
    ?assertEqual(true, llm_response:has_tool_calls(Resp)),
    ?assertEqual(tool_use, llm_response:finish_reason(Resp)),
    ?assertEqual(true, llm_response:needs_tool_call(Resp)),

    [ToolCall] = llm_response:tool_calls(Resp),
    ?assertEqual(<<"call_abc123">>, maps:get(id, ToolCall)),
    ?assertEqual(<<"get_weather">>, maps:get(name, ToolCall)),
    ?assertEqual(#{<<"city">> => <<"Beijing">>}, maps:get(arguments, ToolCall)),
    ?assertEqual(<<"{\"city\": \"Beijing\"}">>, maps:get(raw_arguments, ToolCall)).

from_openai_length_limit_test() ->
    Raw = #{
        <<"id">> => <<"chatcmpl-789">>,
        <<"model">> => <<"gpt-4">>,
        <<"choices">> => [#{
            <<"message">> => #{<<"content">> => <<"Truncated...">>},
            <<"finish_reason">> => <<"length">>
        }],
        <<"usage">> => #{}
    },
    {ok, Resp} = llm_response_parser:from_openai(Raw),
    ?assertEqual(length_limit, llm_response:finish_reason(Resp)),
    ?assertEqual(false, llm_response:is_complete(Resp)).

from_openai_error_test() ->
    Raw = #{<<"error">> => #{<<"message">> => <<"Rate limit exceeded">>}},
    ?assertMatch({error, {api_error, _}}, llm_response_parser:from_openai(Raw)).

%%====================================================================
%% Anthropic 格式测试
%%====================================================================

from_anthropic_basic_test() ->
    Raw = #{
        <<"id">> => <<"msg_123">>,
        <<"type">> => <<"message">>,
        <<"role">> => <<"assistant">>,
        <<"model">> => <<"claude-3-5-sonnet-20241022">>,
        <<"content">> => [
            #{<<"type">> => <<"text">>, <<"text">> => <<"Hello, world!">>}
        ],
        <<"stop_reason">> => <<"end_turn">>,
        <<"usage">> => #{
            <<"input_tokens">> => 10,
            <<"output_tokens">> => 20
        }
    },
    {ok, Resp} = llm_response_parser:from_anthropic(Raw),

    %% 基本字段
    ?assertEqual(<<"msg_123">>, llm_response:id(Resp)),
    ?assertEqual(<<"claude-3-5-sonnet-20241022">>, llm_response:model(Resp)),
    ?assertEqual(anthropic, llm_response:provider(Resp)),

    %% 内容
    ?assertEqual(<<"Hello, world!">>, llm_response:content(Resp)),
    ?assertEqual([#{type => text, text => <<"Hello, world!">>}], llm_response:content_blocks(Resp)),

    %% 状态
    ?assertEqual(complete, llm_response:finish_reason(Resp)),
    ?assertEqual(true, llm_response:is_complete(Resp)),

    %% Token 统计
    ?assertEqual(10, llm_response:input_tokens(Resp)),
    ?assertEqual(20, llm_response:output_tokens(Resp)),
    ?assertEqual(30, llm_response:total_tokens(Resp)).

from_anthropic_with_tool_use_test() ->
    Raw = #{
        <<"id">> => <<"msg_456">>,
        <<"model">> => <<"claude-3-5-sonnet">>,
        <<"content">> => [
            #{<<"type">> => <<"text">>, <<"text">> => <<"Let me check the weather.">>},
            #{
                <<"type">> => <<"tool_use">>,
                <<"id">> => <<"toolu_abc123">>,
                <<"name">> => <<"get_weather">>,
                <<"input">> => #{<<"city">> => <<"Shanghai">>}
            }
        ],
        <<"stop_reason">> => <<"tool_use">>,
        <<"usage">> => #{
            <<"input_tokens">> => 50,
            <<"output_tokens">> => 30
        }
    },
    {ok, Resp} = llm_response_parser:from_anthropic(Raw),

    ?assertEqual(<<"Let me check the weather.">>, llm_response:content(Resp)),
    ?assertEqual(true, llm_response:has_tool_calls(Resp)),
    ?assertEqual(tool_use, llm_response:finish_reason(Resp)),
    ?assertEqual(true, llm_response:needs_tool_call(Resp)),

    [ToolCall] = llm_response:tool_calls(Resp),
    ?assertEqual(<<"toolu_abc123">>, maps:get(id, ToolCall)),
    ?assertEqual(<<"get_weather">>, maps:get(name, ToolCall)),
    ?assertEqual(#{<<"city">> => <<"Shanghai">>}, maps:get(arguments, ToolCall)),

    %% 检查内容块保留了顺序
    Blocks = llm_response:content_blocks(Resp),
    ?assertEqual(2, length(Blocks)),
    [TextBlock, ToolBlock] = Blocks,
    ?assertEqual(text, maps:get(type, TextBlock)),
    ?assertEqual(tool_use, maps:get(type, ToolBlock)).

from_anthropic_with_cache_tokens_test() ->
    Raw = #{
        <<"id">> => <<"msg_789">>,
        <<"model">> => <<"claude-3-5-sonnet">>,
        <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => <<"Cached response">>}],
        <<"stop_reason">> => <<"end_turn">>,
        <<"usage">> => #{
            <<"input_tokens">> => 100,
            <<"output_tokens">> => 50,
            <<"cache_creation_input_tokens">> => 80,
            <<"cache_read_input_tokens">> => 20
        }
    },
    {ok, Resp} = llm_response_parser:from_anthropic(Raw),

    Usage = llm_response:usage(Resp),
    ?assertEqual(100, maps:get(input_tokens, Usage)),
    ?assertEqual(50, maps:get(output_tokens, Usage)),

    %% 检查详细统计
    Details = maps:get(details, Usage),
    ?assertEqual(80, maps:get(cache_creation_input_tokens, Details)),
    ?assertEqual(20, maps:get(cache_read_input_tokens, Details)),

    %% 通过原始数据访问
    ?assertEqual(80, llm_response:raw_get(Resp, [<<"usage">>, <<"cache_creation_input_tokens">>])).

from_anthropic_max_tokens_test() ->
    Raw = #{
        <<"id">> => <<"msg_abc">>,
        <<"model">> => <<"claude-3-5-sonnet">>,
        <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => <<"Truncated...">>}],
        <<"stop_reason">> => <<"max_tokens">>,
        <<"usage">> => #{}
    },
    {ok, Resp} = llm_response_parser:from_anthropic(Raw),
    ?assertEqual(length_limit, llm_response:finish_reason(Resp)),
    ?assertEqual(false, llm_response:is_complete(Resp)).

from_anthropic_error_test() ->
    Raw = #{<<"error">> => #{<<"type">> => <<"rate_limit_error">>}},
    ?assertMatch({error, {api_error, _}}, llm_response_parser:from_anthropic(Raw)).

%%====================================================================
%% 通用接口测试
%%====================================================================

from_provider_test() ->
    OpenAIRaw = #{
        <<"id">> => <<"test-1">>,
        <<"model">> => <<"gpt-4">>,
        <<"choices">> => [#{
            <<"message">> => #{<<"content">> => <<"Test">>},
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{}
    },

    %% OpenAI 兼容的 providers
    {ok, Resp1} = llm_response_parser:from_provider(OpenAIRaw, openai),
    ?assertEqual(openai, llm_response:provider(Resp1)),

    {ok, Resp2} = llm_response_parser:from_provider(OpenAIRaw, deepseek),
    ?assertEqual(openai, llm_response:provider(Resp2)),  % 使用 OpenAI 解析器

    %% Zhipu 有专用解析器，设置正确的 provider
    {ok, Resp3} = llm_response_parser:from_provider(OpenAIRaw, zhipu),
    ?assertEqual(zhipu, llm_response:provider(Resp3)).

%%====================================================================
%% 边界情况测试
%%====================================================================

empty_content_test() ->
    Raw = #{
        <<"id">> => <<"test">>,
        <<"model">> => <<"claude">>,
        <<"content">> => [],
        <<"stop_reason">> => <<"end_turn">>,
        <<"usage">> => #{}
    },
    {ok, Resp} = llm_response_parser:from_anthropic(Raw),
    ?assertEqual(null, llm_response:content(Resp)),
    ?assertEqual([], llm_response:content_blocks(Resp)).

malformed_json_arguments_test() ->
    Raw = #{
        <<"id">> => <<"test">>,
        <<"model">> => <<"gpt-4">>,
        <<"choices">> => [#{
            <<"message">> => #{
                <<"content">> => null,
                <<"tool_calls">> => [#{
                    <<"id">> => <<"call_1">>,
                    <<"function">> => #{
                        <<"name">> => <<"broken_tool">>,
                        <<"arguments">> => <<"not valid json">>
                    }
                }]
            },
            <<"finish_reason">> => <<"tool_calls">>
        }],
        <<"usage">> => #{}
    },
    {ok, Resp} = llm_response_parser:from_openai(Raw),
    [ToolCall] = llm_response:tool_calls(Resp),
    %% 解析失败应返回空 map
    ?assertEqual(#{}, maps:get(arguments, ToolCall)),
    %% 但原始参数仍然保留
    ?assertEqual(<<"not valid json">>, maps:get(raw_arguments, ToolCall)).

%%====================================================================
%% 智谱 AI 格式测试
%%====================================================================

from_zhipu_basic_test() ->
    Raw = #{
        <<"id">> => <<"chatcmpl-zhipu-123">>,
        <<"model">> => <<"glm-4.7">>,
        <<"choices">> => [#{
            <<"message">> => #{
                <<"role">> => <<"assistant">>,
                <<"content">> => <<"Hello from GLM!">>
            },
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{
            <<"prompt_tokens">> => 10,
            <<"completion_tokens">> => 20,
            <<"total_tokens">> => 30
        }
    },
    {ok, Resp} = llm_response_parser:from_zhipu(Raw),

    ?assertEqual(zhipu, llm_response:provider(Resp)),
    ?assertEqual(<<"glm-4.7">>, llm_response:model(Resp)),
    ?assertEqual(<<"Hello from GLM!">>, llm_response:content(Resp)),
    ?assertEqual(complete, llm_response:finish_reason(Resp)).

from_zhipu_with_reasoning_content_test() ->
    Raw = #{
        <<"id">> => <<"chatcmpl-zhipu-456">>,
        <<"model">> => <<"glm-4.6">>,
        <<"choices">> => [#{
            <<"message">> => #{
                <<"role">> => <<"assistant">>,
                <<"content">> => <<>>,
                <<"reasoning_content">> => <<"This is the thinking process...">>
            },
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{}
    },
    {ok, Resp} = llm_response_parser:from_zhipu(Raw),

    %% 当 content 为空但有 reasoning_content，使用 reasoning_content
    ?assertEqual(<<"This is the thinking process...">>, llm_response:content(Resp)),
    %% 可以通过 reasoning_content/1 访问
    ?assertEqual(<<"This is the thinking process...">>, llm_response:reasoning_content(Resp)).

%%====================================================================
%% Ollama 格式测试
%%====================================================================

from_ollama_native_format_test() ->
    Raw = #{
        <<"model">> => <<"llama3.2">>,
        <<"created_at">> => <<"2024-01-15T10:30:00Z">>,
        <<"message">> => #{
            <<"role">> => <<"assistant">>,
            <<"content">> => <<"Hello from Ollama!">>
        },
        <<"done">> => true,
        <<"done_reason">> => <<"stop">>,
        <<"prompt_eval_count">> => 15,
        <<"eval_count">> => 25
    },
    {ok, Resp} = llm_response_parser:from_ollama(Raw),

    ?assertEqual(ollama, llm_response:provider(Resp)),
    ?assertEqual(<<"llama3.2">>, llm_response:model(Resp)),
    ?assertEqual(<<"Hello from Ollama!">>, llm_response:content(Resp)),
    ?assertEqual(complete, llm_response:finish_reason(Resp)),
    ?assertEqual(15, llm_response:input_tokens(Resp)),
    ?assertEqual(25, llm_response:output_tokens(Resp)).

from_ollama_openai_format_test() ->
    %% Ollama 也支持 OpenAI 兼容格式
    Raw = #{
        <<"id">> => <<"ollama-123">>,
        <<"model">> => <<"llama3.2">>,
        <<"choices">> => [#{
            <<"message">> => #{<<"content">> => <<"OpenAI format">>},
            <<"finish_reason">> => <<"stop">>
        }],
        <<"usage">> => #{
            <<"prompt_tokens">> => 10,
            <<"completion_tokens">> => 5,
            <<"total_tokens">> => 15
        }
    },
    {ok, Resp} = llm_response_parser:from_ollama(Raw),

    ?assertEqual(ollama, llm_response:provider(Resp)),
    ?assertEqual(<<"OpenAI format">>, llm_response:content(Resp)).

%%====================================================================
%% DashScope 格式测试
%%====================================================================

from_dashscope_basic_test() ->
    Raw = #{
        <<"request_id">> => <<"req-12345">>,
        <<"output">> => #{
            <<"choices">> => [#{
                <<"message">> => #{
                    <<"role">> => <<"assistant">>,
                    <<"content">> => <<"Hello from Qwen!">>
                },
                <<"finish_reason">> => <<"stop">>
            }]
        },
        <<"usage">> => #{
            <<"input_tokens">> => 20,
            <<"output_tokens">> => 30,
            <<"total_tokens">> => 50
        }
    },
    {ok, Resp} = llm_response_parser:from_dashscope(Raw),

    ?assertEqual(bailian, llm_response:provider(Resp)),
    ?assertEqual(<<"req-12345">>, llm_response:id(Resp)),
    ?assertEqual(<<"Hello from Qwen!">>, llm_response:content(Resp)),
    ?assertEqual(complete, llm_response:finish_reason(Resp)),
    ?assertEqual(20, llm_response:input_tokens(Resp)),
    ?assertEqual(30, llm_response:output_tokens(Resp)).

from_dashscope_legacy_format_test() ->
    %% DashScope 旧格式：text 直接在 output 下
    Raw = #{
        <<"request_id">> => <<"req-legacy">>,
        <<"output">> => #{
            <<"text">> => <<"Legacy format response">>,
            <<"finish_reason">> => <<"stop">>
        },
        <<"usage">> => #{
            <<"input_tokens">> => 10,
            <<"output_tokens">> => 15
        }
    },
    {ok, Resp} = llm_response_parser:from_dashscope(Raw),

    ?assertEqual(bailian, llm_response:provider(Resp)),
    ?assertEqual(<<"Legacy format response">>, llm_response:content(Resp)).
