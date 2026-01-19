%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @doc 使用百炼 Provider 测试 beamai_agent 和 beamai_coordinator
%%%
%%% 使用方法:
%%% ```
%%% %% 设置环境变量
%%% export BAILIAN_API_KEY=your-api-key
%%%
%%% %% 运行测试
%%% example_test_bailian:test_all().
%%%
%%% %% 单独测试
%%% example_test_bailian:test_agent().
%%% example_test_bailian:test_pipeline().
%%% example_test_bailian:test_orchestrator().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_test_bailian).

-export([test_all/0]).
-export([test_agent/0, test_pipeline/0, test_orchestrator/0]).

%% 百炼配置常量
-define(DEFAULT_MODEL, <<"qwen-plus">>).
-define(DEFAULT_MAX_TOKENS, 2048).

%%====================================================================
%% 公共 API
%%====================================================================

%% @doc 运行所有测试
test_all() ->
    io:format("~n========================================~n"),
    io:format("  百炼 Provider 测试~n"),
    io:format("========================================~n~n"),

    io:format(">>> 测试 1: beamai_agent~n"),
    io:format("----------------------------------------~n"),
    test_agent(),

    io:format("~n>>> 测试 2: beamai_coordinator (Pipeline)~n"),
    io:format("----------------------------------------~n"),
    test_pipeline(),

    io:format("~n>>> 测试 3: beamai_coordinator (Orchestrator)~n"),
    io:format("----------------------------------------~n"),
    test_orchestrator(),

    io:format("~n========================================~n"),
    io:format("  所有测试完成~n"),
    io:format("========================================~n"),
    ok.

%%====================================================================
%% 测试 beamai_agent
%%====================================================================

test_agent() ->
    LLM = get_llm_config(),

    %% 创建一个带工具的 Agent
    Tools = [
        #{
            name => <<"add">>,
            description => <<"将两个数字相加"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"a">> => #{type => number, description => <<"第一个数"/utf8>>},
                    <<"b">> => #{type => number, description => <<"第二个数"/utf8>>}
                },
                required => [<<"a">>, <<"b">>]
            },
            handler => fun(Args) ->
                A = maps:get(<<"a">>, Args),
                B = maps:get(<<"b">>, Args),
                io:format("  [Tool] add(~p, ~p) = ~p~n", [A, B, A + B]),
                #{result => A + B}
            end
        },
        #{
            name => <<"multiply">>,
            description => <<"将两个数字相乘"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"a">> => #{type => number, description => <<"第一个数"/utf8>>},
                    <<"b">> => #{type => number, description => <<"第二个数"/utf8>>}
                },
                required => [<<"a">>, <<"b">>]
            },
            handler => fun(Args) ->
                A = maps:get(<<"a">>, Args),
                B = maps:get(<<"b">>, Args),
                io:format("  [Tool] multiply(~p, ~p) = ~p~n", [A, B, A * B]),
                #{result => A * B}
            end
        }
    ],

    AgentOpts = #{
        name => <<"计算器助手"/utf8>>,
        system_prompt => <<"你是一个计算器助手，使用工具来完成数学计算。"/utf8>>,
        tools => Tools,
        llm => LLM
    },

    io:format("创建 Agent...~n"),
    {ok, Agent} = beamai_agent:start_link(<<"test_agent">>, AgentOpts),

    io:format("发送计算请求: 计算 (3 + 5) * 2~n"),
    case beamai_agent:run(Agent, <<"请计算 (3 + 5) * 2"/utf8>>, #{}) of
        {ok, Result} ->
            Response = maps:get(final_response, Result, <<>>),
            io:format("回答: ~ts~n", [Response]);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,

    %% 测试 meta API
    io:format("~n测试 Meta API...~n"),
    beamai_agent:put_meta(Agent, test_key, <<"test_value">>),
    Meta = beamai_agent:get_meta(Agent),
    io:format("Meta: ~p~n", [Meta]),

    beamai_agent:stop(Agent),
    io:format("Agent 测试完成~n"),
    ok.

%%====================================================================
%% 测试 beamai_coordinator (Pipeline 模式)
%%====================================================================

test_pipeline() ->
    LLM = get_llm_config(),

    %% 定义 Pipeline 中的 Agents
    %% 场景: 翻译流水线 (中文 -> 英文 -> 优化)
    Agents = [
        #{
            name => <<"translator">>,
            system_prompt => <<"你是一个翻译专家，将中文翻译成英文。只输出翻译结果，不要其他内容。"/utf8>>
        },
        #{
            name => <<"polisher">>,
            system_prompt => <<"你是一个英文润色专家，优化英文表达使其更自然流畅。只输出润色后的结果，不要其他内容。"/utf8>>
        }
    ],

    io:format("创建 Pipeline 协调器...~n"),
    {ok, Coordinator} = beamai_coordinator:start_pipeline(<<"test_pipeline">>, #{
        agents => Agents,
        llm => LLM,
        system_prompt => <<"你是一个翻译流程协调器。收到文本后，先调用 translator 翻译，然后调用 polisher 润色。最后返回润色后的结果。"/utf8>>
    }),

    %% 验证 meta 存储
    io:format("验证 Meta 存储...~n"),
    {ok, Workers} = beamai_coordinator:get_workers(Coordinator),
    io:format("Workers: ~p~n", [maps:keys(Workers)]),

    %% 测试 delegate 直接调用 worker
    io:format("~n测试 delegate 直接调用 translator...~n"),
    case beamai_coordinator:delegate(Coordinator, <<"translator">>, <<"你好世界"/utf8>>) of
        {ok, TranslateResult} ->
            io:format("翻译结果: ~ts~n", [TranslateResult]);
        {error, Reason1} ->
            io:format("错误: ~p~n", [Reason1])
    end,

    %% 测试通过协调器运行完整流程
    io:format("~n测试完整 Pipeline 流程...~n"),
    io:format("输入: 今天天气很好，我们去公园散步吧~n"),
    case beamai_agent:run(Coordinator, <<"今天天气很好，我们去公园散步吧"/utf8>>, #{}) of
        {ok, Result} ->
            Response = maps:get(final_response, Result, <<>>),
            io:format("最终结果: ~ts~n", [Response]);
        {error, Reason2} ->
            io:format("错误: ~p~n", [Reason2])
    end,

    beamai_coordinator:stop(Coordinator),
    io:format("Pipeline 测试完成~n"),
    ok.

%%====================================================================
%% 测试 beamai_coordinator (Orchestrator 模式)
%%====================================================================

test_orchestrator() ->
    LLM = get_llm_config(),

    %% 定义 Orchestrator 中的专家 Agents
    %% 场景: 多专家咨询系统
    Agents = [
        #{
            name => <<"tech_expert">>,
            system_prompt => <<"你是一个技术专家，从技术角度分析问题。回答要简洁，控制在100字以内。"/utf8>>
        },
        #{
            name => <<"business_expert">>,
            system_prompt => <<"你是一个商业专家，从商业角度分析问题。回答要简洁，控制在100字以内。"/utf8>>
        },
        #{
            name => <<"legal_expert">>,
            system_prompt => <<"你是一个法律专家，从法律角度分析问题。回答要简洁，控制在100字以内。"/utf8>>
        }
    ],

    io:format("创建 Orchestrator 协调器...~n"),
    {ok, Coordinator} = beamai_coordinator:start_orchestrator(<<"test_orchestrator">>, #{
        agents => Agents,
        llm => LLM
    }),

    %% 验证 meta 存储
    io:format("验证 Meta 存储...~n"),
    Meta = beamai_agent:get_meta(Coordinator),
    io:format("Coordinator Type: ~p~n", [maps:get(coordinator_type, Meta)]),
    {ok, Workers} = beamai_coordinator:get_workers(Coordinator),
    io:format("Workers: ~p~n", [maps:keys(Workers)]),

    %% 测试并行委托
    io:format("~n测试 delegate_parallel...~n"),
    Question = <<"人工智能对社会的影响"/utf8>>,
    io:format("问题: ~ts~n", [Question]),
    case beamai_coordinator:delegate_parallel(Coordinator,
            [<<"tech_expert">>, <<"business_expert">>], Question) of
        {ok, ParallelResults} ->
            maps:foreach(fun(Name, Result) ->
                io:format("~n[~ts]~n", [Name]),
                case Result of
                    {ok, Content} -> io:format("~ts~n", [Content]);
                    {error, Err} -> io:format("错误: ~p~n", [Err])
                end
            end, ParallelResults);
        {error, Reason1} ->
            io:format("错误: ~p~n", [Reason1])
    end,

    %% 测试通过协调器运行（让 LLM 决定如何编排）
    io:format("~n测试 Orchestrator 自动编排...~n"),
    io:format("问题: 我想开发一个AI产品，需要考虑哪些方面？~n"),
    case beamai_agent:run(Coordinator,
            <<"我想开发一个AI产品，需要考虑哪些方面？请咨询各位专家。"/utf8>>, #{}) of
        {ok, Result} ->
            Response = maps:get(final_response, Result, <<>>),
            io:format("综合回答:~n~ts~n", [Response]);
        {error, Reason2} ->
            io:format("错误: ~p~n", [Reason2])
    end,

    beamai_coordinator:stop(Coordinator),
    io:format("Orchestrator 测试完成~n"),
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 获取百炼 LLM 配置
get_llm_config() ->
    case os:getenv("BAILIAN_API_KEY") of
        false ->
            io:format("错误: 未设置 BAILIAN_API_KEY 环境变量~n"),
            io:format("请运行: export BAILIAN_API_KEY=your-api-key~n"),
            erlang:error(missing_api_key);
        Key ->
            llm_client:create(bailian, #{
                api_key => list_to_binary(Key),
                model => ?DEFAULT_MODEL,
                max_tokens => ?DEFAULT_MAX_TOKENS
            })
    end.
