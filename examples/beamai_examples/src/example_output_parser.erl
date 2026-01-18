%%%-------------------------------------------------------------------
%%% @doc Output Parser 使用示例
%%%
%%% 展示如何在 Agent 中使用 Output Parser 功能。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_output_parser).

%% 简单示例
-export([simple_json_parser/0]).
-export([extract_json_from_text/0]).
-export([parser_with_schema/0]).
-export([retry_parser_example/0]).

%% Agent 集成示例
-export([structured_agent/0]).
-export([json_mode_agent/0]).
-export([data_extraction_example/0]).
-export([classification_example/0]).

%%====================================================================
%% 基础示例
%%====================================================================

%% @doc 简单 JSON 解析示例
simple_json_parser() ->
    %% 创建 JSON Parser
    Parser = beamai_output_parser:json(),

    %% LLM 输出
    LLMOutput = <<"{\"name\": \"Alice\", \"age\": 30, \"city\": \"NYC\"}">>,

    %% 解析
    case beamai_output_parser:parse(Parser, LLMOutput) of
        {ok, Result} ->
            io:format("Parsed JSON: ~p~n", [Result]),
            #{<<"name">> := Name, <<"age">> := Age} = Result,
            io:format("Name: ~s, Age: ~p~n", [Name, Age]);
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason])
    end.

%% @doc 从混合文本中提取 JSON
extract_json_from_text() ->
    Parser = beamai_output_parser:json(#{extract_codeblock => true}),

    %% LLM 可能返回带有多余文本的输出
    Text1 = <<"Here's the result:\n```json\n{\"status\": \"success\"}\n```">>,
    {ok, Result1} = beamai_output_parser:parse(Parser, Text1),
    io:format("Extracted from codeblock: ~p~n", [Result1]),

    %% 或者 JSON 嵌在普通文本中
    Text2 = <<"Analysis complete. Data: {\"score\": 95, \"pass\": true}">>,
    {ok, Result2} = beamai_output_parser:parse(Parser, Text2),
    io:format("Extracted from text: ~p~n", [Result2]).

%% @doc 使用 JSON Schema 约束输出
parser_with_schema() ->
    %% 定义期望的输出格式
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"title">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Article title">>
            },
            <<"summary">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Brief summary">>
            },
            <<"tags">> => #{
                <<"type">> => <<"array">>,
                <<"items">> => #{<<"type">> => <<"string">>},
                <<"description">> => <<"Related tags">>
            }
        },
        <<"required">> => [<<"title">>, <<"summary">>]
    },

    %% 创建带 Schema 的 Parser
    Parser = beamai_output_parser:json(#{schema => Schema}),

    %% 获取格式指令（可以添加到 prompt 中）
    Instructions = beamai_output_parser:get_instructions(json, #{schema => Schema}),
    io:format("Format instructions:~n~s~n~n", [Instructions]),

    %% 解析 LLM 输出
    LLMOutput = <<"{\"title\": \"Erlang/OTP\", \"summary\": \"A programming language\", \"tags\": [\"erlang\", \"otp\"]}">>,
    {ok, Result} = beamai_output_parser:parse(Parser, LLMOutput),
    io:format("Parsed result: ~p~n", [Result]).

%% @doc 重试机制示例
retry_parser_example() ->
    Parser = beamai_output_parser:json(),

    %% 设置重试回调
    RetryCallback = fun(Error, Attempt) ->
        io:format("Attempt ~p failed: ~p~n", [Attempt, Error])
    end,

    %% LLM 输出（可能有格式问题）
    BadText = <<"{\"name\": \"Bob\", \"age\": 25,}">>,  %% 尾随逗号

    %% 带重试的解析（最多 3 次）
    case beamai_output_parser:parse_with_retry(Parser, BadText, 3, #{on_retry => RetryCallback}) of
        {ok, Result} ->
            io:format("Successfully parsed after retries: ~p~n", [Result]);
        {error, {max_retries_exceeded, Errors}} ->
            io:format("Failed after all retries. Errors: ~p~n", [Errors])
    end.

%%====================================================================
%% Agent 集成示例
%%====================================================================

%% @doc 结构化输出 Agent
%% 展示如何在 Agent 中集成 Output Parser
structured_agent() ->
    %% 配置 Agent
    _Config = #{
        system_prompt => get_system_prompt_with_format(),
        response_format => #{
            type => json,
            schema => get_analysis_schema()
        }
    },

    %% 运行 Agent
    Query = <<"Analyze the following text: Erlang is a programming language...">>,

    %% Agent 会在 pre_llm_call 中自动添加格式指令
    %% 并在 post_llm_call 中自动解析输出
    io:format("Query: ~s~n", [Query]),
    io:format("Expected output format: JSON with sentiment, keywords, summary~n").

%% @doc 使用原生 JSON Mode
%% 注意：智谱 GLM-4.7 也支持 JSON 格式输出，但方式略有不同
json_mode_agent() ->
    %% 配置智谱 GLM-4.7（通过 Anthropic 兼容 API）
    _Config = #{
        llm => #{
            provider => anthropic,
            model => <<"glm-4.7">>,
            base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
            api_key => list_to_binary(os:getenv("ZHIPU_API_KEY", "")),
            max_tokens => 2048
        },
        response_format => #{
            type => json
        }
    },

    %% 使用结构化输出格式
    io:format("Using GLM-4.7 JSON mode~n").

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 获取带格式指令的系统提示词
get_system_prompt_with_format() ->
    Base = <<"You are a text analysis assistant.">>,
    Instructions = beamai_output_parser:get_instructions(json, #{
        schema => get_analysis_schema()
    }),
    <<Base/binary, "\n\n", Instructions/binary>>.

%% @private 获取分析任务的 Schema
get_analysis_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"sentiment">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"positive">>, <<"neutral">>, <<"negative">>],
                <<"description">> => <<"Overall sentiment">>
            },
            <<"confidence">> => #{
                <<"type">> => <<"number">>,
                <<"minimum">> => 0.0,
                <<"maximum">> => 1.0,
                <<"description">> => <<"Confidence score">>
            },
            <<"keywords">> => #{
                <<"type">> => <<"array">>,
                <<"items">> => #{<<"type">> => <<"string">>},
                <<"description">> => <<"Key terms extracted">>
            },
            <<"summary">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Brief summary">>
            }
        },
        <<"required">> => [<<"sentiment">>, <<"keywords">>, <<"summary">>]
    }.

%%====================================================================
%% 实际使用场景示例
%%====================================================================

%% @doc 数据提取 Agent
data_extraction_example() ->
    %% 场景：从非结构化文本中提取结构化数据
    Text = <<"Customer: John Doe, Email: john@example.com, Plan: Premium">>,

    %% 创建期望的 Schema
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"customer_name">> => #{<<"type">> => <<"string">>},
            <<"email">> => #{<<"type">> => <<"string">>, <<"format">> => <<"email">>},
            <<"plan">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"Basic">>, <<"Premium">>, <<"Enterprise">>]}
        },
        <<"required">> => [<<"customer_name">>, <<"email">>, <<"plan">>]
    },

    _Parser = beamai_output_parser:json(#{schema => Schema}),

    %% 构建提示词
    Instructions = beamai_output_parser:get_instructions(json, #{schema => Schema}),
    Prompt = <<"Extract customer information from: ", Text/binary, "\n\n", Instructions/binary>>,

    io:format("Prompt: ~s~n", [Prompt]),
    %% 发送到 LLM，然后解析返回值
    ok.

%% @doc 分类任务 Agent
classification_example() ->
    %% 场景：将文本分类到预定义类别
    _Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"category">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"tech">>, <<"finance">>, <<"health">>, <<"sports">>]
            },
            <<"confidence">> => #{<<"type">> => <<"number">>},
            <<"reasoning">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"category">>, <<"confidence">>]
    },

    io:format("Classification schema defined~n").
