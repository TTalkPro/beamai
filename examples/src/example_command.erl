%%%-------------------------------------------------------------------
%%% @doc Command 模式示例
%%%
%%% 演示如何使用 graph_command 在节点返回值中同时指定
%%% 状态增量更新（delta）和路由目标（goto）。
%%%
%%% 同时展示两种 Provider 配置方式：
%%%   - GLM-4.6 使用 zhipu_provider（原生智谱 API）
%%%   - GLM-4.7 使用 anthropic_provider（Anthropic 兼容 API）
%%%
%%% 场景: 多轮对话决策系统
%%% - classify 节点: 分类用户意图，通过 Command 动态路由
%%% - chat 节点: 普通对话（使用 GLM-4.7）
%%% - code 节点: 代码生成（使用 GLM-4.6）
%%% - review 节点: 审查结果，决定是否需要修正
%%%
%%% 架构:
%%% <pre>
%%% [classify] --goto:chat--> [chat] --> [__end__]
%%%            --goto:code--> [code] --> [review]
%%%                                        |
%%%                              goto:code (修正) / goto:__end__ (通过)
%%% </pre>
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% example_command:run().
%%% example_command:run_mock().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_command).

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
            run(list_to_binary(Key))
    end.

%% @doc 运行示例（指定 API Key）
-spec run(binary()) -> ok.
run(ApiKey) ->
    io:format("=== Command 模式示例 ===~n~n"),

    %% 创建两种 LLM 配置
    LLM_GLM46 = llm_client:create(zhipu, #{
        model => <<"glm-4.6">>,
        api_key => ApiKey,
        max_tokens => 2048
    }),
    LLM_GLM47 = llm_client:create(anthropic, #{
        model => <<"glm-4.7">>,
        api_key => ApiKey,
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
        max_tokens => 2048
    }),

    {ok, Graph} = build_graph(),

    %% 测试 1: 代码生成请求
    io:format("--- 测试 1: 代码请求 ---~n"),
    run_query(Graph, <<"帮我写一个 Erlang 的 GenServer 模板"/utf8>>,
              LLM_GLM46, LLM_GLM47, false),

    %% 测试 2: 普通对话请求
    io:format("~n--- 测试 2: 普通对话 ---~n"),
    run_query(Graph, <<"Erlang 的 OTP 是什么？"/utf8>>,
              LLM_GLM46, LLM_GLM47, false),
    ok.

%% @doc Mock 模式运行
-spec run_mock() -> ok.
run_mock() ->
    io:format("=== Command 示例 (Mock 模式) ===~n~n"),

    {ok, Graph} = build_graph(),

    %% 测试 1: 代码请求
    io:format("--- 测试 1: 代码请求 ---~n"),
    run_query(Graph, <<"帮我写一个快速排序函数"/utf8>>, #{}, #{}, true),

    %% 测试 2: 普通对话
    io:format("~n--- 测试 2: 普通对话 ---~n"),
    run_query(Graph, <<"什么是函数式编程？"/utf8>>, #{}, #{}, true),
    ok.

%%====================================================================
%% 图构建
%%====================================================================

%% @doc 构建 Command 示例图
-spec build_graph() -> {ok, map()} | {error, term()}.
build_graph() ->
    B0 = graph:builder(#{max_iterations => 10}),

    %% 添加节点
    B1 = graph:add_node(B0, classify, make_classify_node()),
    B2 = graph:add_node(B1, chat, make_chat_node()),
    B3 = graph:add_node(B2, code, make_code_node()),
    B4 = graph:add_node(B3, review, make_review_node()),

    %% 边: chat 和 review 的出边
    %% classify 和 review 使用 Command 的 goto 进行路由，无需边
    B5 = graph:add_edge(B4, chat, '__end__'),
    B6 = graph:add_edge(B5, code, review),

    %% 入口
    B7 = graph:set_entry(B6, classify),
    graph:compile(B7).

%%====================================================================
%% 节点实现
%%====================================================================

%% @doc 分类节点 - 判断用户意图并路由
%%
%% 使用 Command 同时设置分类结果和路由目标:
%% - 代码类请求 -> goto code
%% - 对话类请求 -> goto chat
make_classify_node() ->
    fun(State, _VertexInput) ->
        Query = graph:get(State, query, <<>>),
        MockMode = graph:get(State, mock_mode, true),

        Category = case MockMode of
            true ->
                classify_by_keywords(Query);
            false ->
                LLMConfig = graph:get(State, llm_glm47, #{}),
                classify_with_llm(LLMConfig, Query)
        end,

        io:format("  分类结果: ~ts~n", [Category]),

        %% 使用 Command 同时更新状态和路由
        Target = case Category of
            <<"code">> -> code;
            _ -> chat
        end,

        Cmd = graph_command:goto(Target, #{
            category => Category,
            classify_done => true
        }),
        {command, Cmd}
    end.

%% @doc 对话节点 - 使用 GLM-4.7 (anthropic_provider) 回答
make_chat_node() ->
    fun(State, _VertexInput) ->
        Query = graph:get(State, query, <<>>),
        MockMode = graph:get(State, mock_mode, true),

        Response = case MockMode of
            true ->
                <<"[GLM-4.7 回答] 这是一个关于编程概念的回答。"/utf8>>;
            false ->
                LLMConfig = graph:get(State, llm_glm47, #{}),
                chat_with_llm(LLMConfig, Query)
        end,

        io:format("  对话节点 (GLM-4.7): 已生成回答~n"),

        NewState = graph:set(State, response, Response),
        {ok, NewState}
    end.

%% @doc 代码节点 - 使用 GLM-4.6 (zhipu_provider) 生成代码
make_code_node() ->
    fun(State, _VertexInput) ->
        Query = graph:get(State, query, <<>>),
        MockMode = graph:get(State, mock_mode, true),
        Attempt = graph:get(State, code_attempt, 0),

        Code = case MockMode of
            true ->
                <<"```erlang\n-module(example).\n-export([hello/0]).\nhello() -> ok.\n```"/utf8>>;
            false ->
                LLMConfig = graph:get(State, llm_glm46, #{}),
                generate_code(LLMConfig, Query, Attempt)
        end,

        io:format("  代码节点 (GLM-4.6): 生成代码 (尝试 #~p)~n", [Attempt + 1]),

        State1 = graph:set(State, generated_code, Code),
        State2 = graph:set(State1, code_attempt, Attempt + 1),
        {ok, State2}
    end.

%% @doc 审查节点 - 检查代码质量，决定是否需要修正
%%
%% 使用 Command 根据审查结果动态路由:
%% - 质量不达标 & 尝试次数 < 2 -> goto code（重新生成）
%% - 质量达标或超过尝试次数 -> goto '__end__'
make_review_node() ->
    fun(State, _VertexInput) ->
        Code = graph:get(State, generated_code, <<>>),
        Attempt = graph:get(State, code_attempt, 0),
        MockMode = graph:get(State, mock_mode, true),

        {PassReview, Feedback} = case MockMode of
            true ->
                %% Mock: 第一次不通过，第二次通过
                case Attempt >= 2 of
                    true -> {true, <<"代码审查通过"/utf8>>};
                    false -> {false, <<"缺少错误处理，请补充 try-catch"/utf8>>}
                end;
            false ->
                LLMConfig = graph:get(State, llm_glm47, #{}),
                review_code(LLMConfig, Code)
        end,

        io:format("  审查节点: ~ts~n", [Feedback]),

        case PassReview orelse Attempt >= 3 of
            true ->
                %% 通过或超过重试次数 -> 结束
                Cmd = graph_command:goto('__end__', #{
                    review_passed => PassReview,
                    review_feedback => Feedback,
                    response => Code
                }),
                {command, Cmd};
            false ->
                %% 不通过 -> 回到 code 节点重新生成
                Cmd = graph_command:goto(code, #{
                    review_feedback => Feedback
                }),
                {command, Cmd}
        end
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 运行单个查询
run_query(Graph, Query, LLM46, LLM47, MockMode) ->
    InitState = graph:state(#{
        query => Query,
        llm_glm46 => LLM46,
        llm_glm47 => LLM47,
        mock_mode => MockMode
    }),

    io:format("  输入: ~ts~n", [Query]),

    Result = graph:run(Graph, InitState, #{workers => 1}),

    case maps:get(status, Result) of
        completed ->
            FinalState = maps:get(final_state, Result),
            Category = graph:get(FinalState, category, <<"unknown">>),
            Response = graph:get(FinalState, response, <<"无响应"/utf8>>),
            io:format("  类别: ~ts~n", [Category]),
            io:format("  响应: ~ts~n", [truncate(Response, 100)]);
        Status ->
            io:format("  执行失败: ~p~n", [Status])
    end.

%% @private 关键词分类
classify_by_keywords(Query) ->
    CodeKeywords = [<<"写">>, <<"代码">>, <<"函数">>, <<"实现">>,
                    <<"模板">>, <<"GenServer">>, <<"排序">>],
    HasCodeKeyword = lists:any(fun(Kw) ->
        binary:match(Query, Kw) =/= nomatch
    end, CodeKeywords),
    case HasCodeKeyword of
        true -> <<"code">>;
        false -> <<"chat">>
    end.

%% @private LLM 分类
classify_with_llm(LLMConfig, Query) ->
    Messages = [
        #{role => system, content => <<"判断用户意图。如果是要求生成代码或编程实现，回答 code；否则回答 chat。只回答一个词。"/utf8>>},
        #{role => user, content => Query}
    ],
    case llm_client:chat(LLMConfig, Messages) of
        {ok, #{content := Content}} when Content =/= null ->
            Trimmed = string:trim(Content),
            case binary:match(Trimmed, <<"code">>) of
                nomatch -> <<"chat">>;
                _ -> <<"code">>
            end;
        _ ->
            <<"chat">>
    end.

%% @private 对话
chat_with_llm(LLMConfig, Query) ->
    Messages = [
        #{role => system, content => <<"你是一个技术助手，简洁回答问题。"/utf8>>},
        #{role => user, content => Query}
    ],
    case llm_client:chat(LLMConfig, Messages) of
        {ok, #{content := Content}} when Content =/= null -> Content;
        _ -> <<"回答生成失败"/utf8>>
    end.

%% @private 代码生成
generate_code(LLMConfig, Query, Attempt) ->
    Prompt = case Attempt of
        0 -> Query;
        _ -> <<Query/binary, "\n请注意添加完整的错误处理。"/utf8>>
    end,
    Messages = [
        #{role => system, content => <<"你是一个 Erlang 编程专家。生成简洁、正确的 Erlang 代码。"/utf8>>},
        #{role => user, content => Prompt}
    ],
    case llm_client:chat(LLMConfig, Messages) of
        {ok, #{content := Content}} when Content =/= null -> Content;
        _ -> <<"代码生成失败"/utf8>>
    end.

%% @private 代码审查
review_code(LLMConfig, Code) ->
    Messages = [
        #{role => system, content => <<"审查以下代码。如果代码质量合格回答 PASS，否则回答 FAIL 并说明原因。"/utf8>>},
        #{role => user, content => Code}
    ],
    case llm_client:chat(LLMConfig, Messages) of
        {ok, #{content := Content}} when Content =/= null ->
            case binary:match(Content, <<"PASS">>) of
                nomatch -> {false, Content};
                _ -> {true, Content}
            end;
        _ ->
            {true, <<"审查跳过"/utf8>>}
    end.

%% @private 截断文本（UTF-8 安全）
truncate(Text, MaxLen) when is_binary(Text) ->
    case string:length(Text) > MaxLen of
        true ->
            Short = string:slice(Text, 0, MaxLen),
            <<(unicode:characters_to_binary(Short))/binary, "...">>;
        false ->
            Text
    end;
truncate(_, _) ->
    <<"...">>.
