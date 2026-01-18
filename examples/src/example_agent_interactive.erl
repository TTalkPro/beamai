%%%-------------------------------------------------------------------
%%% @doc 交互式 Deep Agent 示例
%%%
%%% 这个例子演示如何创建一个可以持续交互的 Deep Agent。
%%% Agent 会保持对话状态，可以与用户进行多轮对话。
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
%%% example_agent_interactive:start().
%%%
%%% %% 或者直接运行
%%% example_agent_interactive:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agent_interactive).
-export([run/0, start/0]).
-export([create_config/0, create_config/1]).

%% @doc 主入口 - 创建并启动交互式 agent
run() ->
    io:format("=== 交互式 Deep Agent ===~n"),
    io:format("正在启动 Agent...~n~n"),
    start().

%% @doc 启动交互式会话
start() ->
    case create_config() of
        {ok, Config} ->
            io:format("Agent 已启动！~n"),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
            io:format("提示:~n"),
            io:format("  - 输入消息与 Agent 对话~n"),
            io:format("  - 输入 'quit' 或 'exit' 退出~n"),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n~n"),

            %% 启动 interactive_loop，初始历史为空
            interactive_loop(Config, []),

            io:format("~nAgent 已停止。再见！~n");

        {error, Reason} ->
            io:format("启动 Agent 失败: ~p~n", [Reason])
    end.

%% @doc 创建 Agent 配置
create_config() ->
    create_config(#{}).

%% @doc 创建 Agent 配置（带额外配置）
create_config(ExtraOpts) ->
    %% 使用 example_utils 获取 LLM 配置
    case example_utils:get_llm_config() of
        {ok, LLMConfig} ->
            %% 定义工具集
            Tools = create_tools(),

            %% Agent 配置
            BaseOpts = #{
                llm => LLMConfig,
                max_depth => 2,
                planning_enabled => false,
                reflection_enabled => false,
                tools => Tools,
                max_iterations => 10,
                system_prompt => <<
                    "你是一个智能助手，具有以下能力：\n"
                    "- 搜索网络信息\n"
                    "- 执行数学计算\n"
                    "- 查询时间\n"
                    "- 保存笔记\n"
                    "\n"
                    "请根据用户需求，使用合适的工具来完成任务。\n"
                    "回答要简洁、准确、有帮助。"/utf8
                >>
            },

            %% 合并配置
            Config = beamai_deepagent:new(maps:merge(BaseOpts, ExtraOpts)),
            {ok, Config};

        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建工具集
create_tools() ->
    [
        %% 搜索工具
        #{
            name => <<"search_web">>,
            description => <<"搜索网络获取信息"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"query">> => #{
                        type => string,
                        description => <<"搜索关键词"/utf8>>
                    }
                },
                required => [<<"query">>]
            },
            handler => fun(Args, _State) ->
                Query = maps:get(<<"query">>, Args),
                io:format("  [搜索] ~ts...~n", [Query]),
                Results = simulate_search(Query),
                io:format("  找到 ~p 条结果~n", [length(Results)]),
                #{query => Query, results => Results}
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
            handler => fun(Args, _State) ->
                Expr = maps:get(<<"expression">>, Args),
                io:format("  [计算] ~ts~n", [Expr]),
                try
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
                properties => #{}
            },
            handler => fun(_Args, _State) ->
                io:format("  [时间] 查询当前时间~n"),
                {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
                TimeStr = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                       [Year, Month, Day, Hour, Minute, Second]),
                #{time => iolist_to_binary(TimeStr)}
            end
        },

        %% 笔记工具
        #{
            name => <<"save_note">>,
            description => <<"保存笔记">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"title">> => #{type => string},
                    <<"content">> => #{type => string}
                },
                required => [<<"title">>, <<"content">>]
            },
            handler => fun(Args, _State) ->
                Title = maps:get(<<"title">>, Args),
                Content = maps:get(<<"content">>, Args),
                io:format("  [笔记] 保存: ~ts~n", [Title]),
                #{saved => true, title => Title, length => byte_size(Content)}
            end
        }
    ].

%%====================================================================
%% interactive_loop
%%====================================================================

%% @private interactive_loop
interactive_loop(Config, History) ->
    io:format(">>> "),
    case io:get_line("") of
        eof ->
            io:format("~n");
        {error, Reason} ->
            io:format("输入错误: ~p~n", [Reason]);
        Line ->
            Input = string:trim(Line, both, "\n"),
            InputStr = string:trim(Input, both),

            case handle_input(InputStr, Config, History) of
                {continue, NewHistory} ->
                    interactive_loop(Config, NewHistory);
                stop ->
                    ok
            end
    end.

%% @private 处理用户输入
handle_input("", _Config, History) ->
    {continue, History};

handle_input("quit", _Config, _History) ->
    io:format("正在退出...~n"),
    stop;

handle_input("exit", _Config, _History) ->
    io:format("正在退出...~n"),
    stop;

handle_input(Input, Config, History) ->
    InputBin = unicode:characters_to_binary(Input),

    io:format("~n[思考中...]~n~n"),

    %% 构建包含历史的消息
    HistoryMessages = lists:map(fun({Role, Content}) ->
        #{role => Role, content => Content}
    end, History),

    %% 添加当前用户消息
    UserMsg = #{role => user, content => InputBin},
    AllMessages = HistoryMessages ++ [UserMsg],

    %% 更新配置中的初始消息
    ConfigWithHistory = Config#{messages => AllMessages},

    case beamai_deepagent:run(ConfigWithHistory, InputBin) of
        {ok, Result} ->
            %% 获取响应
            Response = maps:get(response, Result,
                       maps:get(final_response, Result, <<"(无响应)">>)),

            %% 显示响应
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
            io:format("~ts~n", [Response]),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),

            %% 显示统计
            Iterations = maps:get(iterations, Result, 0),
            case Iterations of
                0 -> ok;
                _ -> io:format("~n[迭代 ~p 次]~n", [Iterations])
            end,

            %% 更新历史
            NewHistory = History ++ [{user, InputBin}, {assistant, Response}],
            io:format("~n"),
            {continue, NewHistory};

        {error, Reason} ->
            io:format("错误: ~p~n~n", [Reason]),
            {continue, History}
    end.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 简单的表达式求值
evaluate_expression(Expr) ->
    ExprStr = binary_to_list(Expr),
    %% 使用 Erlang 表达式求值
    case erl_scan:string(ExprStr ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Exprs} ->
                    {value, Value, _} = erl_eval:exprs(Exprs, []),
                    Value;
                _ ->
                    error(parse_error)
            end;
        _ ->
            error(scan_error)
    end.

%% @private 模拟搜索结果
simulate_search(Query) ->
    LowerQuery = string:lowercase(binary_to_list(Query)),
    BaseResults = [
        #{title => <<"相关信息 1"/utf8>>,
          snippet => <<"这是关于您查询内容的第一条模拟结果..."/utf8>>},
        #{title => <<"相关信息 2"/utf8>>,
          snippet => <<"这是关于您查询内容的第二条模拟结果..."/utf8>>}
    ],
    case string:find(LowerQuery, "erlang") of
        nomatch ->
            BaseResults;
        _ ->
            [
                #{title => <<"Erlang 编程语言"/utf8>>,
                  snippet => <<"Erlang 是一种并发的函数式编程语言..."/utf8>>}
            | BaseResults]
    end.
