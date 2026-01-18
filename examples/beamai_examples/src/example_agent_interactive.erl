%%%-------------------------------------------------------------------
%%% @doc 交互式 Deep Agent 示例
%%%
%%% 这个例子演示如何创建一个可以持续交互的 Deep Agent。
%%% Agent 会保持运行状态，可以与用户进行多轮对话。
%%%
%%% 特性:
%%%   - 支持 Planning（计划）
%%%   - 支持 Reflection（反思）
%%%   - 支持子任务派生
%%%   - 持久化对话历史
%%%   - 实时显示工具调用
%%%   - 支持退出命令
%%%
%%% 使用方法:
%%% ```erlang
%%% %% 启动交互式 agent
%%% interactive_deep_agent:start().
%%%
%%% %% 或者直接运行
%%% interactive_deep_agent:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agent_interactive).
-export([run/0, start/0]).
-export([create_assistant/0, create_assistant/1]).

%% @doc 主入口 - 创建并启动交互式 agent
run() ->
    io:format("=== 交互式 Deep Agent ===~n"),
    io:format("正在启动 Agent...~n~n"),
    start().

%% @doc 启动交互式会话
start() ->
    case create_assistant() of
        {ok, Agent} ->
            io:format("✅ Agent 已启动！~n"),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
            io:format("提示:~n"),
            io:format("  - 输入消息与 Agent 对话~n"),
            io:format("  - 输入 'quit' 或 'exit' 退出~n"),
            io:format("  - 输入 'trace' 查看执行轨迹~n"),
            io:format("  - 输入 'plan' 查看当前计划~n"),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n~n"),

            %% 启动interactive_loop
           interactive_loop(Agent),

            %% 清理
            beamai_deepagent:stop(Agent),
            io:format("~nAgent 已停止。再见！~n");

        {error, Reason} ->
            io:format("❌ 启动 Agent 失败: ~p~n", [Reason])
    end.

%% @doc 创建智能助手 Agent
create_assistant() ->
    create_assistant(#{}).

%% @doc 创建智能助手 Agent（带额外配置）
create_assistant(ExtraOpts) ->
    AgentId = <<"interactive_assistant_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% 使用 example_utils 获取 LLM 配置
    {ok, LLMConfig} = example_utils:get_llm_config(),
    ApiKey = maps:get(api_key, LLMConfig),

    %% 定义工具集
    Tools = [
        %% 搜索工具（使用真实搜索）
        #{
            name => <<"search_web">>,
            description => <<"搜索网络获取信息。使用 DuckDuckGo API 进行实时搜索。"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"query">> => #{
                        type => string,
                        description => <<"搜索关键词，支持中英文"/utf8>>
                    }
                },
                required => [<<"query">>]
            },
            handler => fun(Args) ->
                Query = maps:get(<<"query">>, Args),
                io:format("  🔍 [搜索] ~ts...~n", [Query]),
                %% 模拟搜索结果（实际项目可集成真实搜索 API）
                Results = simulate_search(Query),
                io:format("  ✅ 找到 ~p 条结果~n", [length(Results)]),
                #{
                    query => Query,
                    results => Results,
                    source => <<"Simulated Search">>
                }
            end
        },

        %% 计算工具
        #{
            name => <<"calculate">>,
            description => <<"执行数学计算">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"expression">> => #{
                        type => string,
                        description => <<"数学表达式，如 '2 + 3 * 4'">>
                    }
                },
                required => [<<"expression">>]
            },
            handler => fun(Args) ->
                Expr = maps:get(<<"expression">>, Args),
                io:format("  🧮 [计算] ~ts~n", [Expr]),
                try
                    %% 简单的计算表达式解析（实际项目中应使用更完善的解析器）
                    Result = evaluate_expression(Expr),
                    #{expression => Expr, result => Result}
                catch
                    _:_ ->
                        #{expression => Expr, error => <<"无法计算此表达式">>}
                end
            end
        },

        %% 时间查询工具
        #{
            name => <<"get_current_time">>,
            description => <<"获取当前时间">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"timezone">> => #{
                        type => string,
                        description => <<"时区（可选），如 'Asia/Shanghai'">>
                    }
                }
            },
            handler => fun(Args) ->
                Timezone = maps:get(<<"timezone">>, Args, <<"UTC">>),
                io:format("  🕐 [时间] 时区: ~ts~n", [Timezone]),
                {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
                #{
                    timezone => Timezone,
                    utc => io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                        [Year, Month, Day, Hour, Minute, Second]),
                    timestamp => erlang:system_time(second)
                }
            end
        },

        %% 笔记工具
        #{
            name => <<"save_note">>,
            description => <<"保存笔记到内存">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"title">> => #{type => string},
                    <<"content">> => #{type => string}
                },
                required => [<<"title">>, <<"content">>]
            },
            handler => fun(Args) ->
                Title = maps:get(<<"title">>, Args),
                Content = maps:get(<<"content">>, Args),
                io:format("  📝 [笔记] 保存: ~ts~n", [Title]),
                #{
                    saved => true,
                    title => Title,
                    length => byte_size(Content),
                    timestamp => erlang:system_time(second)
                }
            end
        }
    ],

    %% Agent 配置
    BaseOpts = #{
        name => <<"智能助手">>,
        max_depth => 2,                      %% 允许子任务
        planning_enabled => true,             %% 启用计划
        reflection_enabled => true,           %% 启用反思
        tools => Tools,
        max_iterations => 15,                 %% 最大迭代次数
        system_prompt => <<
            "你是一个智能助手，具有以下能力：~n"
            "- 搜索网络信息~n"
            "- 执行数学计算~n"
            "- 查询时间~n"
            "- 保存笔记~n"
            "- 制定计划并执行复杂任务~n"
            "- 反思和调整策略~n"
            "~n"
            "请根据用户需求，使用合适的工具来完成任务。~n"
            "回答要简洁、准确、有帮助。~n"
            "可以主动提出计划，征求用户意见。"/utf8
        >>
    },

    %% LLM 配置（使用智谱 Anthropic 兼容 API）
    LLMOpts = #{
        provider => anthropic,
        api_key => ApiKey,
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
        model => <<"glm-4.7">>,
        max_tokens => 2048,
        timeout => 120000
    },

    %% 合并配置，包含 LLM 配置
    Opts = maps:merge(BaseOpts#{llm => LLMOpts}, ExtraOpts),

    %% 保存 LLM 配置到 process dictionary，供后续使用
    put(llm_config, LLMOpts),

    beamai_deepagent:start_link(AgentId, Opts).

%%====================================================================
%% interactive_loop
%%====================================================================

%% @private interactive_loop
interactive_loop(Agent) ->
    io:format(">>> "),
    case io:get_line("") of
        eof ->
            io:format("~n");
        {error, Reason} ->
            io:format("输入错误: ~p~n", [Reason]);
        Line ->
            Input = string:trim(Line, both, "\n"),
            InputStr = string:trim(Input, both),

            case handle_command(InputStr, Agent) of
                continue ->
                    interactive_loop(Agent);
                stop ->
                    ok
            end
    end.

%% @private 处理用户命令
handle_command("", _Agent) ->
    io:format(""),
    continue;

handle_command("quit", _Agent) ->
    io:format("正在退出...~n"),
    stop;

handle_command("exit", _Agent) ->
    io:format("正在退出...~n"),
    stop;

handle_command("trace", Agent) ->
    case beamai_deepagent:get_execution_trace(Agent) of
        {ok, Trace} ->
            io:format("~n执行轨迹 (~p 条):~n", [length(Trace)]),
            lists:foreach(fun(Entry) ->
                io:format("  - ~p~n", [Entry])
            end, lists:sublist(Trace, 10));
        {error, Reason} ->
            io:format("获取轨迹失败: ~p~n", [Reason])
    end,
    continue;

handle_command("plan", Agent) ->
    case beamai_deepagent:get_plan(Agent) of
        {ok, Plan} ->
            io:format("~n当前计划:~n"),
            io:format("  目标: ~ts~n", [maps:get(goal, Plan, <<"无">>)]),
            case maps:get(steps, Plan, []) of
                [] -> io:format("  (无步骤)~n");
                Steps ->
                    lists:foreach(fun(Step) ->
                        Id = maps:get(id, Step, undefined),
                        Status = maps:get(status, Step, unknown),
                        Desc = maps:get(description, Step, <<>>),
                        io:format("  [~p] ~ts: ~ts~n", [Id, Status, Desc])
                    end, Steps)
            end;
        {error, no_plan} ->
            io:format("当前没有活跃的计划。~n");
        {error, Reason} ->
            io:format("获取计划失败: ~p~n", [Reason])
    end,
    continue;

handle_command(Input, Agent) ->
    InputBin = unicode:characters_to_binary(Input),

    io:format("~n[思考中...]~n~n"),

    %% 获取 LLM 配置
    LLMConfig = case get(llm_config) of
        undefined -> #{};
        Config -> Config
    end,

    case beamai_deepagent:run(Agent, InputBin, LLMConfig) of
        {ok, Result} ->
            %% 显示最终回复
            case maps:get(final_response, Result, no_response) of
                no_response ->
                    io:format("(Agent 未给出回复)~n");
                Response ->
                    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
                    io:format("~ts~n", [Response]),
                    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n")
            end,

            %% 显示统计信息
            Iterations = maps:get(iterations, Result, 0),
            ToolCalls = maps:get(total_tool_calls, Result, 0),
            case {Iterations, ToolCalls} of
                {0, 0} -> ok;
                _ -> io:format("~n[统计: 迭代 ~p 次，工具调用 ~p 次]~n", [Iterations, ToolCalls])
            end;

        {error, Reason} ->
            io:format("❌ 错误: ~p~n", [Reason])
    end,
    io:format("~n"),
    continue.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 简单的表达式求值（仅支持基本运算）
evaluate_expression(Expr) ->
    %% 移除所有空格
    ExprStr = re:replace(Expr, "\\s+", "", [global, {return, list}]),

    %% 提取数字和运算符
    case parse_expression(ExprStr) of
        {ok, {A, Op, B}} ->
            case Op of
                "+" -> A + B;
                "-" -> A - B;
                "*" -> A * B;
                "/" when B =/= 0 -> A / B;
                "/" -> error(division_by_zero);
                _ -> error({unknown_operator, Op})
            end;
        {error, _} ->
            error(invalid_expression)
    end.

%% @private 解析简单表达式 (如 "2+3", "10*5")
parse_expression(Str) ->
    case string:to_integer(Str) of
        {A, Rest} when A >= 0 ->
            case Rest of
                [Op | BStr] when Op =:= $+; Op =:= $-; Op =:= $*; Op =:= $/ ->
                    case string:to_integer(BStr) of
                        {B, []} -> {ok, {A, [Op], B}};
                        _ -> {error, invalid}
                    end;
                _ ->
                    {error, invalid}
            end;
        _ ->
            {error, invalid}
    end.

%% @private 格式化错误信息
format_error(Reason) ->
    iolist_to_binary(io_lib:format("搜索错误: ~p", [Reason])).

%% @private 模拟搜索结果（示例用）
simulate_search(Query) ->
    %% 根据查询关键词返回模拟结果
    LowerQuery = string:lowercase(binary_to_list(Query)),
    BaseResults = [
        #{title => <<"相关信息 1"/utf8>>,
          snippet => <<"这是关于您查询内容的第一条模拟结果..."/utf8>>,
          url => <<"https://example.com/1">>},
        #{title => <<"相关信息 2"/utf8>>,
          snippet => <<"这是关于您查询内容的第二条模拟结果..."/utf8>>,
          url => <<"https://example.com/2">>}
    ],
    case string:find(LowerQuery, "erlang") of
        nomatch ->
            BaseResults;
        _ ->
            [
                #{title => <<"Erlang 编程语言"/utf8>>,
                  snippet => <<"Erlang 是一种通用的、并发的、函数式编程语言，由爱立信开发..."/utf8>>,
                  url => <<"https://www.erlang.org">>},
                #{title => <<"Erlang/OTP 文档"/utf8>>,
                  snippet => <<"Erlang/OTP 是一套开发高可用、容错分布式系统的平台..."/utf8>>,
                  url => <<"https://erlang.org/doc/">>}
            | BaseResults]
    end.
