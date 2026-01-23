%%%-------------------------------------------------------------------
%%% @doc Dispatch 动态并行分发示例
%%%
%%% 演示如何使用 graph_dispatch 实现动态并行分发模式。
%%% 同时展示两种 Provider 配置方式：
%%%   - GLM-4.6 使用 zhipu_provider（原生智谱 API）
%%%   - GLM-4.7 使用 anthropic_provider（Anthropic 兼容 API）
%%%
%%% 场景: 将用户问题动态分发给多个专家并行分析，汇总结果。
%%%
%%% 架构:
%%% <pre>
%%% [router] --dispatch--> [expert] (并行多个实例)
%%%                              \
%%%                               --> [summarize] --> [__end__]
%%% </pre>
%%%
%%% 使用方法:
%%% ```
%%% %% 设置环境变量
%%% export ZHIPU_API_KEY=your-api-key
%%%
%%% %% 运行示例
%%% example_dispatch:run().
%%%
%%% %% Mock 模式（不调用 LLM）
%%% example_dispatch:run_mock().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_dispatch).

-export([run/0, run/1, run_mock/0]).
-export([build_graph/0]).

%%====================================================================
%% 公共 API
%%====================================================================

%% @doc 运行示例（使用环境变量配置）
-spec run() -> ok | {error, term()}.
run() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            io:format("错误: 请设置 ZHIPU_API_KEY 环境变量~n"),
            {error, no_api_key};
        Key ->
            ApiKey = list_to_binary(Key),
            run(ApiKey)
    end.

%% @doc 运行示例（指定 API Key）
-spec run(binary()) -> ok.
run(ApiKey) ->
    io:format("=== Dispatch 动态并行分发示例 ===~n~n"),

    %% 展示两种 Provider 配置
    io:format("--- Provider 配置 ---~n"),
    io:format("1. GLM-4.6 使用 zhipu_provider（原生智谱 API）~n"),
    io:format("2. GLM-4.7 使用 anthropic_provider（Anthropic 兼容 API）~n~n"),

    %% 创建两种 LLM 配置
    LLM_GLM46 = llm_client:create(zhipu, #{
        model => <<"glm-4.6">>,
        api_key => ApiKey,
        max_tokens => 1024
    }),
    LLM_GLM47 = llm_client:create(anthropic, #{
        model => <<"glm-4.7">>,
        api_key => ApiKey,
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
        max_tokens => 1024
    }),

    %% 构建并运行图
    {ok, Graph} = build_graph(),

    InitState = graph:state(#{
        question => <<"如何设计一个高可用的微服务架构？"/utf8>>,
        llm_glm46 => LLM_GLM46,
        llm_glm47 => LLM_GLM47,
        mock_mode => false
    }),

    io:format("问题: ~ts~n~n", [<<"如何设计一个高可用的微服务架构？"/utf8>>]),

    Result = graph:run(Graph, InitState, #{
        workers => 4,
        field_reducers => #{
            <<"expert_results">> => fun graph_state_reducer:append_reducer/2
        }
    }),

    case maps:get(status, Result) of
        completed ->
            FinalState = maps:get(final_state, Result),
            Summary = graph:get(FinalState, summary, <<"无总结"/utf8>>),
            io:format("~n--- 最终总结 ---~n~ts~n", [Summary]);
        Status ->
            io:format("执行失败: ~p~n", [Status])
    end,
    ok.

%% @doc Mock 模式运行（不调用 LLM）
-spec run_mock() -> ok.
run_mock() ->
    io:format("=== Dispatch 示例 (Mock 模式) ===~n~n"),

    {ok, Graph} = build_graph(),

    InitState = graph:state(#{
        question => <<"如何设计一个高可用的微服务架构？"/utf8>>,
        mock_mode => true
    }),

    Result = graph:run(Graph, InitState, #{
        workers => 4,
        field_reducers => #{
            <<"expert_results">> => fun graph_state_reducer:append_reducer/2
        }
    }),

    case maps:get(status, Result) of
        completed ->
            FinalState = maps:get(final_state, Result),
            ExpertResults = graph:get(FinalState, expert_results, []),
            Summary = graph:get(FinalState, summary, <<"无总结"/utf8>>),
            io:format("专家结果数量: ~p~n", [length(ExpertResults)]),
            lists:foreach(fun(#{expert := Name, analysis := Analysis}) ->
                io:format("  [~ts]: ~ts~n", [Name, Analysis])
            end, ExpertResults),
            io:format("~n总结: ~ts~n", [Summary]);
        Status ->
            io:format("执行失败: ~p~n", [Status])
    end,
    ok.

%%====================================================================
%% 图构建
%%====================================================================

%% @doc 构建 Dispatch 图
-spec build_graph() -> {ok, map()} | {error, term()}.
build_graph() ->
    B0 = graph:builder(#{max_iterations => 20}),

    %% router 节点: 根据问题动态创建 dispatch 列表
    B1 = graph:add_node(B0, router, make_router_node()),

    %% expert 节点: 每个 dispatch 实例独立执行
    B2 = graph:add_node(B1, expert, make_expert_node()),

    %% summarize 节点: 汇总所有专家结果
    B3 = graph:add_node(B2, summarize, make_summarize_node()),

    %% 边: router 使用条件边（返回 dispatch 列表）
    %% expert 完成后到 summarize，summarize 到终点
    B4 = graph:add_edge(B3, expert, summarize),
    B5 = graph:add_edge(B4, summarize, '__end__'),

    %% 入口
    B6 = graph:set_entry(B5, router),
    graph:compile(B6).

%%====================================================================
%% 节点实现
%%====================================================================

%% @doc 路由节点 - 动态创建 Dispatch 列表
%%
%% 根据问题类型决定需要哪些专家参与分析，
%% 使用 graph_dispatch:fan_out 创建并行分发指令。
make_router_node() ->
    fun(State, _VertexInput) ->
        Question = graph:get(State, question, <<>>),

        %% 定义专家列表（实际场景可动态决定）
        Experts = [
            #{name => <<"架构专家"/utf8>>,
              prompt => <<"你是一位资深软件架构师。请从架构设计角度分析以下问题: "/utf8>>},
            #{name => <<"运维专家"/utf8>>,
              prompt => <<"你是一位 DevOps 专家。请从运维和部署角度分析以下问题: "/utf8>>},
            #{name => <<"安全专家"/utf8>>,
              prompt => <<"你是一位安全工程师。请从安全性角度分析以下问题: "/utf8>>}
        ],

        %% 使用 fan_out 创建并行分发
        Dispatches = graph_dispatch:fan_out(expert, Experts,
            fun(Expert) ->
                #{expert_name => maps:get(name, Expert),
                  expert_prompt => maps:get(prompt, Expert),
                  question => Question}
            end),

        io:format("路由节点: 分发给 ~p 位专家~n", [length(Dispatches)]),

        %% 返回 Command，goto 为 dispatch 列表
        Cmd = graph_command:goto(Dispatches, #{expert_count => length(Dispatches)}),
        {command, Cmd}
    end.

%% @doc 专家节点 - 每个 dispatch 实例独立执行
%%
%% 从 VertexInput 获取该分支的输入参数，
%% 调用 LLM 生成分析结果。
make_expert_node() ->
    fun(State, VertexInput) ->
        %% 从 dispatch input 获取专家配置
        ExpertName = maps:get(expert_name, VertexInput, <<"未知专家"/utf8>>),
        ExpertPrompt = maps:get(expert_prompt, VertexInput, <<>>),
        Question = maps:get(question, VertexInput, <<>>),

        MockMode = graph:get(State, mock_mode, true),

        Analysis = case MockMode of
            true ->
                %% Mock 模式
                iolist_to_binary([ExpertName, <<": 对于\""/utf8>>,
                                  Question, <<"\"，建议采用分布式方案。"/utf8>>]);
            false ->
                %% 交替使用两种 Provider
                LLMConfig = choose_llm(State, ExpertName),
                call_expert_llm(LLMConfig, ExpertPrompt, Question)
        end,

        io:format("  专家 [~ts] 完成分析~n", [ExpertName]),

        %% 返回 delta，expert_results 使用 append_reducer 合并
        NewState = graph:set(State, expert_results,
            [#{expert => ExpertName, analysis => Analysis}]),
        {ok, NewState}
    end.

%% @doc 汇总节点 - 合并所有专家结果
make_summarize_node() ->
    fun(State, _VertexInput) ->
        ExpertResults = graph:get(State, expert_results, []),
        MockMode = graph:get(State, mock_mode, true),

        Summary = case MockMode of
            true ->
                ExpertNames = [maps:get(expert, R) || R <- ExpertResults],
                iolist_to_binary([<<"综合 "/utf8>>,
                    integer_to_binary(length(ExpertResults)),
                    <<" 位专家意见（"/utf8>>,
                    lists:join(<<"、"/utf8>>, ExpertNames),
                    <<"），建议采用渐进式微服务化策略。"/utf8>>]);
            false ->
                LLMConfig = graph:get(State, llm_glm47, #{}),
                summarize_with_llm(LLMConfig, ExpertResults)
        end,

        io:format("汇总节点: 综合 ~p 位专家意见~n", [length(ExpertResults)]),

        NewState = graph:set(State, summary, Summary),
        {ok, NewState}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 选择 LLM 配置（交替使用两种 Provider）
choose_llm(State, ExpertName) ->
    case binary:match(ExpertName, <<"架构"/utf8>>) of
        nomatch ->
            %% 非架构专家使用 GLM-4.6 (zhipu_provider)
            graph:get(State, llm_glm46, #{});
        _ ->
            %% 架构专家使用 GLM-4.7 (anthropic_provider)
            graph:get(State, llm_glm47, #{})
    end.

%% @private 调用专家 LLM
call_expert_llm(LLMConfig, Prompt, Question) ->
    Messages = [
        #{role => system, content => Prompt},
        #{role => user, content => Question}
    ],
    case llm_client:chat(LLMConfig, Messages) of
        {ok, #{content := Content}} when Content =/= null ->
            Content;
        {ok, _} ->
            <<"分析结果为空"/utf8>>;
        {error, Reason} ->
            iolist_to_binary([<<"LLM 调用失败: "/utf8>>,
                              io_lib:format("~p", [Reason])])
    end.

%% @private 使用 LLM 汇总结果
summarize_with_llm(LLMConfig, ExpertResults) ->
    ResultText = lists:foldl(fun(#{expert := Name, analysis := Analysis}, Acc) ->
        <<Acc/binary, "\n[", Name/binary, "]: ", Analysis/binary>>
    end, <<>>, ExpertResults),

    Messages = [
        #{role => system, content => <<"你是一位总结专家。请综合以下专家意见，给出简洁的总结和建议。"/utf8>>},
        #{role => user, content => ResultText}
    ],
    case llm_client:chat(LLMConfig, Messages) of
        {ok, #{content := Content}} when Content =/= null ->
            Content;
        _ ->
            <<"汇总失败"/utf8>>
    end.
