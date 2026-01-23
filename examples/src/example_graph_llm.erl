%%%-------------------------------------------------------------------
%%% @doc 基本 LLM + Tool 图执行示例
%%%
%%% 演示如何使用纯图执行方式构建一个带有 LLM 调用和工具执行的 Agent。
%%% 不使用 beamai_agent 高级抽象，直接操作图节点。
%%%
%%% Provider 配置：
%%%   - GLM-4.6 使用 zhipu_provider（原生智谱 API）
%%%   - GLM-4.7 使用 anthropic_provider（Anthropic 兼容 API）
%%%
%%% 场景: 简单的工具调用 Agent
%%% - llm 节点: 调用 LLM，获取回答或工具调用指令
%%% - tools 节点: 执行工具，将结果反馈给 LLM
%%% - 条件边: 根据 LLM 返回决定是执行工具还是结束
%%%
%%% 架构:
%%% <pre>
%%% [llm] --has_tool_calls--> [tools] ---> [llm] (循环)
%%%       --no_tool_calls---> [__end__]
%%% </pre>
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% example_graph_llm:run().
%%% example_graph_llm:run_mock().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_graph_llm).

-export([run/0, run/1, run_mock/0]).
-export([build_graph/1, build_graph/2]).

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
    io:format("=== 基本 LLM + Tool 图执行示例 ===~n~n"),

    %% GLM-4.6 使用 zhipu_provider（原生智谱 API）
    LLM_GLM46 = llm_client:create(zhipu, #{
        model => <<"glm-4.6">>,
        api_key => ApiKey,
        max_tokens => 2048
    }),

    %% GLM-4.7 使用 anthropic_provider（Anthropic 兼容 API）
    LLM_GLM47 = llm_client:create(anthropic, #{
        model => <<"glm-4.7">>,
        api_key => ApiKey,
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
        max_tokens => 2048
    }),

    %% 定义工具
    Tools = define_tools(),
    ToolHandlers = build_tool_handlers(),

    %% 测试 1: 使用 GLM-4.7 (anthropic_provider)
    io:format("--- 测试 1: GLM-4.7 (anthropic_provider) ---~n"),
    run_agent(LLM_GLM47, Tools, ToolHandlers,
              <<"北京今天天气怎么样？气温多少度？"/utf8>>, false),

    %% 测试 2: 使用 GLM-4.6 (zhipu_provider)
    io:format("~n--- 测试 2: GLM-4.6 (zhipu_provider) ---~n"),
    run_agent(LLM_GLM46, Tools, ToolHandlers,
              <<"帮我计算 (15 + 27) * 3 的结果"/utf8>>, false),
    ok.

%% @doc Mock 模式运行
-spec run_mock() -> ok.
run_mock() ->
    io:format("=== LLM + Tool 图执行 (Mock 模式) ===~n~n"),

    Tools = define_tools(),
    ToolHandlers = build_tool_handlers(),

    %% 测试: Mock LLM 返回工具调用
    io:format("--- Mock 测试: 工具调用循环 ---~n"),
    run_agent(#{}, Tools, ToolHandlers,
              <<"北京的天气如何？"/utf8>>, true),
    ok.

%%====================================================================
%% 图构建
%%====================================================================

%% @doc 构建 LLM + Tool 图
-spec build_graph(map()) -> {ok, map()} | {error, term()}.
build_graph(ToolHandlers) ->
    build_graph(ToolHandlers, #{}).

%% @doc 构建 LLM + Tool 图（带选项）
-spec build_graph(map(), map()) -> {ok, map()} | {error, term()}.
build_graph(ToolHandlers, _Opts) ->
    B0 = graph:builder(#{max_iterations => 10}),

    %% LLM 节点: 调用 LLM
    B1 = graph:add_node(B0, llm, make_llm_node()),

    %% Tool 节点: 执行工具调用
    B2 = graph:add_node(B1, tools, make_tool_node(ToolHandlers)),

    %% 条件边: LLM 后根据是否有工具调用决定下一步
    B3 = graph:add_conditional_edge(B2, llm,
        fun(State) ->
            ToolCalls = graph:get(State, pending_tool_calls, []),
            case ToolCalls of
                [] -> '__end__';
                _ -> tools
            end
        end),

    %% 工具执行后回到 LLM
    B4 = graph:add_edge(B3, tools, llm),

    %% 入口
    B5 = graph:set_entry(B4, llm),
    graph:compile(B5).

%%====================================================================
%% 节点实现
%%====================================================================

%% @doc LLM 调用节点
%%
%% 从状态中读取消息列表，调用 LLM，处理响应:
%% - 如果返回工具调用: 设置 pending_tool_calls
%% - 如果返回文本: 设置 final_response
make_llm_node() ->
    fun(State, _VertexInput) ->
        MockMode = graph:get(State, mock_mode, false),
        Messages = graph:get(State, messages, []),
        Iteration = graph:get(State, iteration, 0),

        {ResponseContent, ResponseToolCalls, FinishReason} = case MockMode of
            true ->
                mock_llm_response(Messages, Iteration);
            false ->
                LLMConfig = graph:get(State, llm_config, #{}),
                Tools = graph:get(State, tools, []),
                call_llm(LLMConfig, Messages, Tools)
        end,

        io:format("  LLM 节点 (迭代 ~p): finish_reason=~ts~n",
                  [Iteration, FinishReason]),

        %% 构建 assistant 消息
        AssistantMsg = build_assistant_message(ResponseContent, ResponseToolCalls),

        %% 更新状态
        State1 = graph:set(State, messages, Messages ++ [AssistantMsg]),
        State2 = graph:set(State1, iteration, Iteration + 1),

        State3 = case ResponseToolCalls of
            [] ->
                %% 无工具调用: 设置最终响应
                S = graph:set(State2, pending_tool_calls, []),
                graph:set(S, final_response, ResponseContent);
            _ ->
                %% 有工具调用: 设置待执行列表
                graph:set(State2, pending_tool_calls, ResponseToolCalls)
        end,

        {ok, State3}
    end.

%% @doc 工具执行节点
%%
%% 读取 pending_tool_calls，逐个执行工具，
%% 将结果作为 tool 消息追加到消息列表。
make_tool_node(ToolHandlers) ->
    fun(State, _VertexInput) ->
        ToolCalls = graph:get(State, pending_tool_calls, []),
        Messages = graph:get(State, messages, []),

        io:format("  工具节点: 执行 ~p 个工具调用~n", [length(ToolCalls)]),

        %% 执行每个工具调用
        ToolMessages = lists:map(fun(ToolCall) ->
            #{name := Name, arguments := ArgsStr} = ToolCall,
            ToolId = maps:get(id, ToolCall, <<"unknown">>),

            %% 解析参数
            Args = try jsx:decode(ArgsStr, [return_maps])
                   catch _:_ -> #{}
                   end,

            %% 查找并执行 handler
            Result = case maps:find(Name, ToolHandlers) of
                {ok, Handler} ->
                    try Handler(Args)
                    catch _:Err -> iolist_to_binary(io_lib:format("错误: ~p", [Err]))
                    end;
                error ->
                    <<"未知工具"/utf8>>
            end,

            io:format("    工具 [~ts]: ~ts~n", [Name, truncate(Result, 60)]),

            %% 构建 tool 结果消息
            #{role => tool,
              tool_call_id => ToolId,
              name => Name,
              content => Result}
        end, ToolCalls),

        %% 更新状态: 追加 tool 消息，清空待执行列表
        State1 = graph:set(State, messages, Messages ++ ToolMessages),
        State2 = graph:set(State1, pending_tool_calls, []),
        {ok, State2}
    end.

%%====================================================================
%% 工具定义
%%====================================================================

%% @doc 定义可用工具
-spec define_tools() -> [map()].
define_tools() ->
    [
        #{
            name => <<"get_weather">>,
            description => <<"获取指定城市的天气信息"/utf8>>,
            parameters => #{
                type => <<"object">>,
                properties => #{
                    <<"city">> => #{
                        type => <<"string">>,
                        description => <<"城市名称"/utf8>>
                    }
                },
                required => [<<"city">>]
            }
        },
        #{
            name => <<"calculator">>,
            description => <<"执行数学计算"/utf8>>,
            parameters => #{
                type => <<"object">>,
                properties => #{
                    <<"expression">> => #{
                        type => <<"string">>,
                        description => <<"数学表达式"/utf8>>
                    }
                },
                required => [<<"expression">>]
            }
        }
    ].

%% @doc 构建工具处理器
-spec build_tool_handlers() -> #{binary() => function()}.
build_tool_handlers() ->
    #{
        <<"get_weather">> => fun handle_get_weather/1,
        <<"calculator">> => fun handle_calculator/1
    }.

%% @private 天气工具处理器
handle_get_weather(Args) ->
    City = maps:get(<<"city">>, Args, <<"未知"/utf8>>),
    %% 模拟天气数据
    iolist_to_binary([
        City, <<": 晴天，气温 22°C，湿度 45%，东北风 3 级"/utf8>>
    ]).

%% @private 计算器工具处理器
handle_calculator(Args) ->
    Expression = maps:get(<<"expression">>, Args, <<"0">>),
    try
        %% 简单表达式求值
        {ok, Tokens, _} = erl_scan:string(binary_to_list(<<Expression/binary, ".">>)),
        {ok, Parsed} = erl_parse:parse_exprs(Tokens),
        {value, Result, _} = erl_eval:exprs(Parsed, []),
        iolist_to_binary(io_lib:format("~p", [Result]))
    catch
        _:_ ->
            <<"计算失败，请检查表达式格式"/utf8>>
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 运行 Agent
run_agent(LLMConfig, Tools, ToolHandlers, Query, MockMode) ->
    {ok, Graph} = build_graph(ToolHandlers),

    %% 初始消息
    InitMessages = [#{role => user, content => Query}],

    InitState = graph:state(#{
        messages => InitMessages,
        tools => Tools,
        llm_config => LLMConfig,
        mock_mode => MockMode,
        pending_tool_calls => [],
        iteration => 0
    }),

    io:format("  用户: ~ts~n", [Query]),

    Result = graph:run(Graph, InitState, #{workers => 1}),

    case maps:get(status, Result) of
        completed ->
            FinalState = maps:get(final_state, Result),
            Response = graph:get(FinalState, final_response, <<"无响应"/utf8>>),
            Iterations = graph:get(FinalState, iteration, 0),
            io:format("  回答 (~p 轮): ~ts~n", [Iterations, truncate(Response, 200)]);
        Status ->
            io:format("  执行失败: ~p~n", [Status])
    end.

%% @private 调用 LLM
call_llm(LLMConfig, Messages, Tools) ->
    Opts = case Tools of
        [] -> #{};
        _ -> #{tools => Tools}
    end,
    case llm_client:chat(LLMConfig, Messages, Opts) of
        {ok, #{content := Content, tool_calls := ToolCalls, finish_reason := FR}} ->
            ParsedCalls = [#{
                id => maps:get(id, TC, <<"tc_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>),
                name => maps:get(name, TC),
                arguments => maps:get(arguments, TC)
            } || TC <- ToolCalls, maps:get(name, TC, <<>>) =/= <<>>],
            {Content, ParsedCalls, FR};
        {ok, #{content := Content, finish_reason := FR}} ->
            {Content, [], FR};
        {error, Reason} ->
            {iolist_to_binary(io_lib:format("LLM 错误: ~p", [Reason])), [], <<"error">>}
    end.

%% @private Mock LLM 响应
mock_llm_response(Messages, Iteration) ->
    %% 检查最后一条消息是否是 tool 结果
    LastMsg = lists:last(Messages),
    case maps:get(role, LastMsg, user) of
        tool ->
            %% 工具结果返回后，生成最终回答
            ToolResult = maps:get(content, LastMsg, <<>>),
            {<<"根据查询结果: "/utf8, ToolResult/binary>>, [], <<"stop">>};
        _ when Iteration =:= 0 ->
            %% 第一轮: 返回工具调用
            %% 注意: 使用 utf8 修饰符确保中文正确编码
            CityJson = iolist_to_binary([<<"{\"city\":\"">>, <<"北京"/utf8>>, <<"\"}">>]),
            {null, [#{
                id => <<"call_001">>,
                name => <<"get_weather">>,
                arguments => CityJson
            }], <<"tool_calls">>};
        _ ->
            %% 其他情况: 直接结束
            {<<"完成"/utf8>>, [], <<"stop">>}
    end.

%% @private 构建 assistant 消息
build_assistant_message(Content, []) ->
    #{role => assistant, content => Content};
build_assistant_message(Content, ToolCalls) ->
    #{role => assistant,
      content => Content,
      tool_calls => [#{
          id => maps:get(id, TC),
          type => <<"function">>,
          function => #{
              name => maps:get(name, TC),
              arguments => maps:get(arguments, TC)
          }
      } || TC <- ToolCalls]}.

%% @private 截断文本（UTF-8 安全）
truncate(null, _) -> <<"null">>;
truncate(Text, MaxLen) when is_binary(Text) ->
    case string:length(Text) > MaxLen of
        true ->
            Short = string:slice(Text, 0, MaxLen),
            <<(unicode:characters_to_binary(Short))/binary, "...">>;
        false ->
            Text
    end;
truncate(Other, _) ->
    iolist_to_binary(io_lib:format("~p", [Other])).
