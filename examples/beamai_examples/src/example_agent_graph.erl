%%%-------------------------------------------------------------------
%%% @doc Graph Deep Agent 使用示例
%%%
%%% 展示如何使用基于图引擎的 Deep Agent。
%%%
%%% 运行方式：
%%% ```
%%% rebar3 compile
%%% rebar3 shell
%%% graph_deep_agent_example:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agent_graph).

-export([
    run/0,
    run_simple/0,
    run_with_plan/0,
    run_parallel/0
]).

%%====================================================================
%% 示例入口
%%====================================================================

%% @doc 运行所有示例
run() ->
    io:format("~n========================================~n"),
    io:format("Graph Deep Agent Examples~n"),
    io:format("========================================~n~n"),

    io:format("1. Simple Query Example~n"),
    io:format("----------------------------------------~n"),
    run_simple(),

    io:format("~n2. Planning Example~n"),
    io:format("----------------------------------------~n"),
    run_with_plan(),

    io:format("~n3. Parallel Tasks Example~n"),
    io:format("----------------------------------------~n"),
    run_parallel(),

    io:format("~n========================================~n"),
    io:format("All examples completed!~n"),
    io:format("========================================~n"),
    ok.

%%====================================================================
%% 示例 1: 简单查询
%%====================================================================

%% @doc 简单查询示例
run_simple() ->
    %% 创建配置
    Config = beamai_deepagent:new(#{
        llm => llm_config(),
        tools => [search_tool(), calculate_tool()],
        planning_enabled => false,  %% 禁用计划
        max_iterations => 10
    }),

    %% 运行
    Message = <<"What is 25 * 4 + 100?">>,
    io:format("User: ~s~n", [Message]),

    case beamai_deepagent:run(Config, Message) of
        {ok, Result} ->
            Response = maps:get(response, Result),
            Iterations = maps:get(iterations, Result),
            io:format("Assistant: ~s~n", [Response]),
            io:format("[Completed in ~p iterations]~n", [Iterations]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

%%====================================================================
%% 示例 2: 带计划的任务
%%====================================================================

%% @doc 带计划的任务示例
run_with_plan() ->
    %% 创建配置
    Config = beamai_deepagent:new(#{
        llm => llm_config(),
        tools => [search_tool(), save_note_tool()],
        planning_enabled => true,
        reflection_enabled => true,
        max_iterations => 20
    }),

    %% 运行研究任务
    Message = <<"Research the history of Erlang programming language and summarize the key milestones.">>,
    io:format("User: ~s~n", [Message]),

    case beamai_deepagent:run(Config, Message) of
        {ok, Result} ->
            Response = maps:get(response, Result),
            Plan = beamai_deepagent:get_plan(Result),
            Trace = beamai_deepagent:get_trace(Result),
            Iterations = maps:get(iterations, Result),

            io:format("~nAssistant: ~s~n", [Response]),
            io:format("~n[Plan Status]~n"),
            print_plan(Plan),
            io:format("~n[Trace: ~p entries]~n", [length(Trace)]),
            io:format("[Completed in ~p iterations]~n", [Iterations]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

%%====================================================================
%% 示例 3: 并行任务
%%====================================================================

%% @doc 并行任务示例
run_parallel() ->
    %% 创建配置（支持子任务）
    Config = beamai_deepagent:new(#{
        llm => llm_config(),
        tools => [search_tool(), analyze_tool()],
        planning_enabled => true,
        max_depth => 2,  %% 允许一层子任务
        max_iterations => 30
    }),

    %% 运行需要并行的任务
    Message = <<"Compare three programming languages: Erlang, Elixir, and Go. ",
                "For each language, analyze: 1) Concurrency model, 2) Use cases, 3) Ecosystem.">>,
    io:format("User: ~s~n", [Message]),

    case beamai_deepagent:run(Config, Message) of
        {ok, Result} ->
            Response = maps:get(response, Result),
            Plan = beamai_deepagent:get_plan(Result),
            Iterations = maps:get(iterations, Result),

            io:format("~nAssistant: ~s~n", [Response]),
            io:format("~n[Plan Status]~n"),
            print_plan(Plan),
            io:format("[Completed in ~p iterations]~n", [Iterations]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.

%%====================================================================
%% 工具定义
%%====================================================================

%% @doc LLM 配置（使用 example_utils）
llm_config() ->
    example_utils:llm_config().

%% @doc 搜索工具
search_tool() ->
    #{
        name => <<"search_web">>,
        description => <<"Search the web for information">>,
        parameters => #{
            type => object,
            properties => #{
                <<"query">> => #{
                    type => string,
                    description => <<"The search query">>
                }
            },
            required => [<<"query">>]
        },
        handler => fun(Args, _State) ->
            Query = maps:get(<<"query">>, Args, <<>>),
            io:format("  [Search] ~s~n", [Query]),
            %% 模拟搜索结果
            #{
                query => Query,
                results => [
                    #{title => <<"Result 1">>, snippet => <<"Relevant information...">>},
                    #{title => <<"Result 2">>, snippet => <<"More details...">>}
                ]
            }
        end
    }.

%% @doc 计算工具
calculate_tool() ->
    #{
        name => <<"calculate">>,
        description => <<"Perform mathematical calculations">>,
        parameters => #{
            type => object,
            properties => #{
                <<"expression">> => #{
                    type => string,
                    description => <<"The mathematical expression to evaluate">>
                }
            },
            required => [<<"expression">>]
        },
        handler => fun(Args, _State) ->
            Expr = maps:get(<<"expression">>, Args, <<"0">>),
            io:format("  [Calculate] ~s~n", [Expr]),
            %% 简单计算（实际应该用解析器）
            Result = try
                {ok, Tokens, _} = erl_scan:string(binary_to_list(<<Expr/binary, ".">>)),
                {ok, Parsed} = erl_parse:parse_exprs(Tokens),
                {value, Value, _} = erl_eval:exprs(Parsed, []),
                Value
            catch
                _:_ -> <<"Error: Invalid expression">>
            end,
            #{expression => Expr, result => Result}
        end
    }.

%% @doc 保存笔记工具
save_note_tool() ->
    #{
        name => <<"save_note">>,
        description => <<"Save a note for later reference">>,
        parameters => #{
            type => object,
            properties => #{
                <<"title">> => #{
                    type => string,
                    description => <<"Title of the note">>
                },
                <<"content">> => #{
                    type => string,
                    description => <<"Content of the note">>
                }
            },
            required => [<<"title">>, <<"content">>]
        },
        handler => fun(Args, _State) ->
            Title = maps:get(<<"title">>, Args, <<>>),
            Content = maps:get(<<"content">>, Args, <<>>),
            io:format("  [Save Note] ~s~n", [Title]),
            #{
                saved => true,
                title => Title,
                content_length => byte_size(Content)
            }
        end
    }.

%% @doc 分析工具
analyze_tool() ->
    #{
        name => <<"analyze">>,
        description => <<"Analyze a topic or data">>,
        parameters => #{
            type => object,
            properties => #{
                <<"topic">> => #{
                    type => string,
                    description => <<"The topic to analyze">>
                },
                <<"aspects">> => #{
                    type => array,
                    items => #{type => string},
                    description => <<"Aspects to analyze">>
                }
            },
            required => [<<"topic">>]
        },
        handler => fun(Args, _State) ->
            Topic = maps:get(<<"topic">>, Args, <<>>),
            Aspects = maps:get(<<"aspects">>, Args, []),
            io:format("  [Analyze] ~s (aspects: ~p)~n", [Topic, length(Aspects)]),
            #{
                topic => Topic,
                analysis => <<"Analysis results for ", Topic/binary>>,
                aspects_covered => Aspects
            }
        end
    }.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @doc 打印计划状态
print_plan(undefined) ->
    io:format("  No plan created~n");
print_plan(Plan) ->
    case beamai_deepagent_plan:format_status(Plan) of
        Status when is_binary(Status) ->
            io:format("~s~n", [Status]);
        _ ->
            io:format("  Plan: ~p~n", [Plan])
    end.
