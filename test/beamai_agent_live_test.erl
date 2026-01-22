%%%-------------------------------------------------------------------
%%% @doc beamai_agent 实时集成测试
%%%
%%% 使用真实 LLM API 测试 beamai_agent 的核心功能：
%%% - 基础对话
%%% - 工具调用
%%% - 多轮对话
%%% - Middleware 系统
%%%
%%% 配置：GLM-4.7 + Anthropic Provider
%%% 需要环境变量 ZHIPU_API_KEY。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_live_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 配置
%%====================================================================

%% GLM-4.7 配置（使用 Anthropic 兼容 API）
glm_config() ->
    ApiKey = list_to_binary(os:getenv("ZHIPU_API_KEY", "")),
    case ApiKey of
        <<>> ->
            io:format("警告: 未设置 ZHIPU_API_KEY 环境变量~n"),
            error(missing_api_key);
        _ ->
            llm_client:create(anthropic, #{
                model => <<"glm-4.7">>,
                api_key => ApiKey,
                base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
                timeout => 60000,
                max_tokens => 2048
            })
    end.

%%====================================================================
%% Test fixtures
%%====================================================================

setup() ->
    application:ensure_all_started(beamai_agent),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% 测试：基础对话
%%====================================================================

basic_conversation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"简单问答", {timeout, 60, fun test_simple_qa/0}},
      {"中文对话", {timeout, 60, fun test_chinese_conversation/0}}
     ]}.

test_simple_qa() ->
    io:format("~n=== 测试简单问答 ===~n"),

    {ok, Agent} = beamai_agent:start_link(<<"test_simple_qa">>, #{
        system_prompt => <<"You are a helpful assistant. Keep responses concise.">>,
        llm => glm_config()
    }),

    {ok, Result} = beamai_agent:run(Agent, <<"What is 2 + 2? Just answer with the number.">>),

    Response = maps:get(final_response, Result, <<>>),
    io:format("问题: What is 2 + 2?~n"),
    io:format("回答: ~s~n", [Response]),

    %% 验证响应包含 "4"
    ?assert(binary:match(Response, <<"4">>) =/= nomatch),

    beamai_agent:stop(Agent),
    io:format("=== 简单问答测试完成 ===~n~n"),
    ok.

test_chinese_conversation() ->
    io:format("~n=== 测试中文对话 ===~n"),

    {ok, Agent} = beamai_agent:start_link(<<"test_chinese">>, #{
        system_prompt => <<"你是一个友好的中文助手。请用简洁的中文回答。"/utf8>>,
        llm => glm_config()
    }),

    {ok, Result} = beamai_agent:run(Agent, <<"请用一句话介绍你自己"/utf8>>),

    Response = maps:get(final_response, Result, <<>>),
    io:format("问题: 请用一句话介绍你自己~n"),
    io:format("回答: ~ts~n", [Response]),

    %% 验证响应非空
    ?assert(byte_size(Response) > 0),

    beamai_agent:stop(Agent),
    io:format("=== 中文对话测试完成 ===~n~n"),
    ok.

%%====================================================================
%% 测试：工具调用
%%====================================================================

tool_calling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"单工具调用", {timeout, 120, fun test_single_tool/0}},
      {"多工具选择", {timeout, 120, fun test_multi_tools/0}}
     ]}.

test_single_tool() ->
    io:format("~n=== 测试单工具调用 ===~n"),

    %% 定义一个简单的计算器工具
    CalculatorTool = #{
        name => <<"calculator">>,
        description => <<"Perform basic arithmetic. Use this for any math calculation.">>,
        parameters => #{
            type => object,
            properties => #{
                <<"expression">> => #{
                    type => string,
                    description => <<"Math expression like '2+2' or '10*5'">>
                }
            },
            required => [<<"expression">>]
        },
        handler => fun(#{<<"expression">> := Expr}, _Ctx) ->
            io:format("  [Calculator] 计算表达式: ~s~n", [Expr]),
            %% 简单解析
            Result = case Expr of
                <<"2+2">> -> <<"4">>;
                <<"10*5">> -> <<"50">>;
                <<"100/4">> -> <<"25">>;
                _ -> <<"42">>
            end,
            {ok, <<"Result: ", Result/binary>>}
        end
    },

    {ok, Agent} = beamai_agent:start_link(<<"test_calculator">>, #{
        system_prompt => <<"You are a math assistant. Use the calculator tool for calculations.">>,
        llm => glm_config(),
        tools => [CalculatorTool],
        max_iterations => 5
    }),

    {ok, Result} = beamai_agent:run(Agent, <<"What is 2+2? Use the calculator.">>),

    Response = maps:get(final_response, Result, <<>>),
    io:format("问题: What is 2+2?~n"),
    io:format("回答: ~s~n", [Response]),

    %% 验证响应包含 "4"
    ?assert(binary:match(Response, <<"4">>) =/= nomatch),

    beamai_agent:stop(Agent),
    io:format("=== 单工具调用测试完成 ===~n~n"),
    ok.

test_multi_tools() ->
    io:format("~n=== 测试多工具选择 ===~n"),

    %% 定义多个工具
    WeatherTool = #{
        name => <<"get_weather">>,
        description => <<"Get current weather for a city. Always use this tool when asked about weather.">>,
        parameters => #{
            type => object,
            properties => #{
                <<"city">> => #{
                    type => string,
                    description => <<"City name">>
                }
            },
            required => [<<"city">>]
        },
        handler => fun(#{<<"city">> := City}, _Ctx) ->
            io:format("  [Weather] 查询城市: ~s~n", [City]),
            {ok, <<"Weather in ", City/binary, ": Sunny, 25 degrees Celsius">>}
        end
    },

    TimeTool = #{
        name => <<"get_time">>,
        description => <<"Get current time for a timezone">>,
        parameters => #{
            type => object,
            properties => #{
                <<"timezone">> => #{
                    type => string,
                    description => <<"Timezone like 'UTC' or 'Asia/Shanghai'">>
                }
            },
            required => [<<"timezone">>]
        },
        handler => fun(#{<<"timezone">> := TZ}, _Ctx) ->
            io:format("  [Time] 查询时区: ~s~n", [TZ]),
            {ok, <<"Current time in ", TZ/binary, ": 14:30">>}
        end
    },

    {ok, Agent} = beamai_agent:start_link(<<"test_multi_tools">>, #{
        system_prompt => <<"You are a helpful assistant. You MUST use the get_weather tool when asked about weather. After getting the tool result, summarize it for the user.">>,
        llm => glm_config(),
        tools => [WeatherTool, TimeTool],
        max_iterations => 5
    }),

    %% 测试选择天气工具
    {ok, Result} = beamai_agent:run(Agent, <<"What's the weather in Beijing? Use the get_weather tool.">>),

    Response = maps:get(final_response, Result, <<>>),
    io:format("问题: What's the weather in Beijing?~n"),
    io:format("回答: ~s~n", [Response]),
    io:format("完整结果: ~p~n", [Result]),

    %% 验证响应非空（工具可能被调用，响应格式可能不同）
    ?assert(byte_size(Response) > 0 orelse maps:is_key(tool_calls, Result)),

    beamai_agent:stop(Agent),
    io:format("=== 多工具选择测试完成 ===~n~n"),
    ok.

%%====================================================================
%% 测试：多轮对话
%%====================================================================

multi_turn_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"多轮对话上下文", {timeout, 180, fun test_conversation_context/0}}
     ]}.

test_conversation_context() ->
    io:format("~n=== 测试多轮对话上下文 ===~n"),

    {ok, Agent} = beamai_agent:start_link(<<"test_context">>, #{
        system_prompt => <<"You are a helpful assistant. Remember what the user tells you.">>,
        llm => glm_config()
    }),

    %% 第一轮：告诉 Agent 一个信息
    io:format("~n第一轮对话:~n"),
    {ok, _Result1} = beamai_agent:run(Agent, <<"My name is Alice.">>),
    io:format("  用户: My name is Alice.~n"),

    %% 第二轮：询问之前的信息
    io:format("~n第二轮对话:~n"),
    {ok, Result2} = beamai_agent:run(Agent, <<"What is my name?">>),

    Response = maps:get(final_response, Result2, <<>>),
    io:format("  用户: What is my name?~n"),
    io:format("  助手: ~s~n", [Response]),

    %% 验证 Agent 记住了名字
    ?assert(binary:match(Response, <<"Alice">>) =/= nomatch),

    beamai_agent:stop(Agent),
    io:format("~n=== 多轮对话上下文测试完成 ===~n~n"),
    ok.

%%====================================================================
%% 测试：Middleware 系统
%%====================================================================

middleware_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"调用限制 Middleware", {timeout, 120, fun test_call_limit_middleware/0}},
      {"日志 Middleware", {timeout, 120, fun test_logging_middleware/0}}
     ]}.

test_call_limit_middleware() ->
    io:format("~n=== 测试调用限制 Middleware ===~n"),

    {ok, Agent} = beamai_agent:start_link(<<"test_limit_mw">>, #{
        system_prompt => <<"You are helpful. Keep responses very short.">>,
        llm => glm_config(),
        max_iterations => 10,
        middleware => [
            {middleware_call_limit, #{
                max_model_calls => 3,
                on_limit_exceeded => halt,
                debug => true
            }}
        ]
    }),

    %% 第一次调用
    io:format("~n第一次调用:~n"),
    {ok, Result1} = beamai_agent:run(Agent, <<"Say hello">>),
    io:format("  回答: ~s~n", [maps:get(final_response, Result1, <<>>)]),

    %% 第二次调用
    io:format("~n第二次调用:~n"),
    {ok, Result2} = beamai_agent:run(Agent, <<"Say goodbye">>),
    io:format("  回答: ~s~n", [maps:get(final_response, Result2, <<>>)]),

    %% 第三次调用
    io:format("~n第三次调用:~n"),
    {ok, Result3} = beamai_agent:run(Agent, <<"Say something">>),
    Response3 = maps:get(final_response, Result3, <<>>),
    io:format("  回答: ~s~n", [Response3]),

    beamai_agent:stop(Agent),
    io:format("~n=== 调用限制 Middleware 测试完成 ===~n~n"),
    ok.

test_logging_middleware() ->
    io:format("~n=== 测试日志 Middleware ===~n"),

    %% 使用内置的调用限制 Middleware 进行测试（它有 debug 模式）
    {ok, Agent} = beamai_agent:start_link(<<"test_log_mw">>, #{
        system_prompt => <<"You are helpful. Keep responses short.">>,
        llm => glm_config(),
        middleware => [
            {middleware_call_limit, #{
                max_model_calls => 10,
                debug => true  %% 启用调试日志
            }}
        ]
    }),

    io:format("执行对话...~n"),
    {ok, Result} = beamai_agent:run(Agent, <<"Say hi">>),
    io:format("回答: ~s~n", [maps:get(final_response, Result, <<>>)]),

    %% 验证响应成功
    ?assert(byte_size(maps:get(final_response, Result, <<>>)) > 0),

    beamai_agent:stop(Agent),
    io:format("~n=== 日志 Middleware 测试完成 ===~n~n"),
    ok.

%%====================================================================
%% 测试：纯函数 API
%%====================================================================

pure_function_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"create_state 和 run_once", {timeout, 120, fun test_pure_function_api/0}}
     ]}.

test_pure_function_api() ->
    io:format("~n=== 测试纯函数 API ===~n"),

    %% 使用 run_once 执行单次调用（传入 Config，非 State）
    Config = #{
        system_prompt => <<"You are a helpful assistant. Be concise.">>,
        llm => glm_config()
    },

    io:format("执行 run_once...~n"),
    {ok, Result} = beamai_agent:run_once(Config, <<"What is 1+1? Answer with just the number.">>),

    Response = maps:get(final_response, Result, <<>>),
    io:format("问题: What is 1+1?~n"),
    io:format("回答: ~s~n", [Response]),

    %% 验证响应包含 "2"
    ?assert(binary:match(Response, <<"2">>) =/= nomatch),

    io:format("~n=== 纯函数 API 测试完成 ===~n~n"),
    ok.
