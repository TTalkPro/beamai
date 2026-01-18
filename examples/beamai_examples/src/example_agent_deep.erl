%%%-------------------------------------------------------------------
%%% @doc Deep Agent 示例
%%%
%%% 演示如何创建和使用带有计划、反思和递归子任务能力的 Deep Agent。
%%%
%%% 使用方法:
%%% ```
%%% %% 设置环境变量
%%% export ZHIPU_API_KEY=your-api-key
%%%
%%% %% 运行示例
%%% rebar3 shell
%%% deep_agent_example:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agent_deep).

-export([run/0, run/1]).
-export([create_research_agent/0, create_research_agent/1]).
-export([create_code_analyzer_agent/0, create_code_analyzer_agent/1]).

%% @doc 运行所有示例（从环境变量获取 API Key）
run() ->
    case example_utils:get_llm_config() of
        {ok, LLMConfig} ->
            run(LLMConfig);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason]),
            io:format("请设置环境变量 ZHIPU_API_KEY~n"),
            {error, Reason}
    end.

%% @doc 运行所有示例（使用指定的 LLM 配置）
run(LLMConfig) ->
    io:format("=== Deep Agent 示例 ===~n~n"),

    io:format("1. 研究型 Deep Agent~n"),
    research_example(LLMConfig),

    io:format("~n2. 代码分析 Deep Agent~n"),
    code_analyzer_example(LLMConfig),

    io:format("~n示例运行完成!~n"),
    ok.

%% @doc 示例：带有计划和反思的研究型 Agent
research_example(LLMConfig) ->
    {ok, Agent} = create_research_agent(LLMConfig),

    %% 运行复杂的研究任务
    RunOpts = #{llm => LLMConfig},
    case beamai_deepagent:run(Agent,
        <<"研究 Erlang 编程语言的历史和影响。",
          "包括它的起源、关键特性和著名的使用案例。"/utf8>>, RunOpts) of
        {ok, Result} ->
            io:format("结果: ~ts~n", [maps:get(final_response, Result, <<"无响应"/utf8>>)]),

            %% 获取执行计划
            case beamai_deepagent:get_plan(Agent) of
                {ok, Plan} ->
                    io:format("执行计划: ~p~n", [Plan]);
                {error, no_plan} ->
                    io:format("未创建计划~n")
            end,

            %% 获取执行轨迹
            {ok, Trace} = beamai_deepagent:get_execution_trace(Agent),
            io:format("执行轨迹条目: ~p~n", [length(Trace)]);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,

    %% 清理
    beamai_deepagent:stop(Agent).

%% @doc 示例：带有子任务派生的代码分析 Agent
code_analyzer_example(LLMConfig) ->
    {ok, Agent} = create_code_analyzer_agent(LLMConfig),

    %% 分析代码结构
    RunOpts = #{llm => LLMConfig},
    case beamai_deepagent:run(Agent,
        <<"分析典型 Web 应用程序的架构。",
          "将分析分解为前端、后端和数据库层。"/utf8>>, RunOpts) of
        {ok, Result} ->
            io:format("结果: ~ts~n", [maps:get(final_response, Result, <<"无响应"/utf8>>)]);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,

    %% 清理
    beamai_deepagent:stop(Agent).

%% @doc 创建研究型 Deep Agent（从环境变量获取 API Key）
create_research_agent() ->
    case example_utils:get_llm_config() of
        {ok, LLMConfig} -> create_research_agent(LLMConfig);
        Error -> Error
    end.

%% @doc 创建研究型 Deep Agent（使用指定的 LLM 配置）
create_research_agent(LLMConfig) ->
    AgentId = <<"research_deep_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    Tools = [
        #{
            name => <<"search_web">>,
            description => <<"Search the web for information">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"query">> => #{type => string, description => <<"Search query">>}
                },
                required => [<<"query">>]
            },
            handler => fun(Args) ->
                Query = maps:get(<<"query">>, Args),
                %% Simulated search results
                Results = case binary:match(Query, <<"erlang">>) of
                    nomatch ->
                        [#{title => <<"Generic Result">>, snippet => <<"Some information...">>}];
                    _ ->
                        [
                            #{title => <<"Erlang History">>,
                              snippet => <<"Developed at Ericsson in 1986 by Joe Armstrong...">>},
                            #{title => <<"Erlang Features">>,
                              snippet => <<"Concurrency, fault-tolerance, hot code swapping...">>},
                            #{title => <<"Erlang Use Cases">>,
                              snippet => <<"WhatsApp, Discord, RabbitMQ, CouchDB...">>}
                        ]
                end,
                #{query => Query, results => Results}
            end
        },
        #{
            name => <<"fetch_article">>,
            description => <<"Fetch and read an article">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"url">> => #{type => string, description => <<"Article URL">>}
                },
                required => [<<"url">>]
            },
            handler => fun(Args) ->
                Url = maps:get(<<"url">>, Args),
                %% Simulated article content
                #{
                    url => Url,
                    title => <<"Article Title">>,
                    content => <<"This is the full article content with detailed information...">>
                }
            end
        },
        #{
            name => <<"save_notes">>,
            description => <<"Save research notes">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"topic">> => #{type => string},
                    <<"notes">> => #{type => string}
                },
                required => [<<"topic">>, <<"notes">>]
            },
            handler => fun(Args) ->
                Topic = maps:get(<<"topic">>, Args),
                Notes = maps:get(<<"notes">>, Args),
                #{saved => true, topic => Topic, notes_length => byte_size(Notes)}
            end
        }
    ],

    Opts = #{
        name => <<"Research Agent">>,
        max_depth => 2,
        planning_enabled => true,
        reflection_enabled => true,
        tools => Tools,
        max_iterations => 15,
        llm => LLMConfig
    },

    beamai_deepagent:start_link(AgentId, Opts).

%% @doc 创建代码分析 Deep Agent（从环境变量获取 API Key）
create_code_analyzer_agent() ->
    case example_utils:get_llm_config() of
        {ok, LLMConfig} -> create_code_analyzer_agent(LLMConfig);
        Error -> Error
    end.

%% @doc 创建代码分析 Deep Agent（使用指定的 LLM 配置）
create_code_analyzer_agent(LLMConfig) ->
    AgentId = <<"code_analyzer_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    Tools = [
        #{
            name => <<"analyze_layer">>,
            description => <<"Analyze a specific architectural layer">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"layer">> => #{
                        type => string,
                        description => <<"Layer to analyze (frontend, backend, database)">>
                    }
                },
                required => [<<"layer">>]
            },
            handler => fun(Args) ->
                Layer = maps:get(<<"layer">>, Args),
                Analysis = case Layer of
                    <<"frontend">> ->
                        #{
                            components => [<<"React/Vue/Angular">>, <<"State Management">>, <<"Routing">>],
                            patterns => [<<"Component-based">>, <<"Reactive">>, <<"SPA">>],
                            concerns => [<<"User Experience">>, <<"Responsiveness">>, <<"Accessibility">>]
                        };
                    <<"backend">> ->
                        #{
                            components => [<<"API Layer">>, <<"Business Logic">>, <<"Data Access">>],
                            patterns => [<<"MVC">>, <<"Microservices">>, <<"Event-driven">>],
                            concerns => [<<"Scalability">>, <<"Security">>, <<"Performance">>]
                        };
                    <<"database">> ->
                        #{
                            components => [<<"Schema">>, <<"Indexes">>, <<"Queries">>],
                            patterns => [<<"Normalization">>, <<"Sharding">>, <<"Replication">>],
                            concerns => [<<"Data Integrity">>, <<"Query Performance">>, <<"Backup">>]
                        };
                    _ ->
                        #{error => <<"Unknown layer">>}
                end,
                Analysis#{layer => Layer}
            end
        },
        #{
            name => <<"find_dependencies">>,
            description => <<"Find dependencies between components">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"component">> => #{type => string}
                },
                required => [<<"component">>]
            },
            handler => fun(Args) ->
                Component = maps:get(<<"component">>, Args),
                #{
                    component => Component,
                    depends_on => [<<"core">>, <<"utils">>, <<"config">>],
                    depended_by => [<<"api">>, <<"worker">>]
                }
            end
        },
        #{
            name => <<"generate_report">>,
            description => <<"Generate analysis report">>,
            parameters => #{
                type => object,
                properties => #{
                    <<"sections">> => #{
                        type => array,
                        items => #{type => string}
                    },
                    <<"format">> => #{
                        type => string,
                        enum => [<<"summary">>, <<"detailed">>, <<"executive">>]
                    }
                },
                required => [<<"sections">>]
            },
            handler => fun(Args) ->
                Sections = maps:get(<<"sections">>, Args),
                Format = maps:get(<<"format">>, Args, <<"summary">>),
                #{
                    report_id => integer_to_binary(erlang:unique_integer([positive])),
                    sections => Sections,
                    format => Format,
                    status => <<"generated">>
                }
            end
        }
    ],

    Opts = #{
        name => <<"Code Analyzer">>,
        max_depth => 3,
        planning_enabled => true,
        reflection_enabled => true,
        tools => Tools,
        max_iterations => 20,
        llm => LLMConfig
    },

    beamai_deepagent:start_link(AgentId, Opts).

%% Note: LLM 配置函数已移至 example_utils 模块
