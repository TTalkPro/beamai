%%%-------------------------------------------------------------------
%%% @doc DeepAgent 使用示例
%%%
%%% 演示 beamai_deepagent 的核心用法：
%%%   - run_simple/0: 简单任务直接执行（mock LLM）
%%%   - run_simple_live/0: 简单任务直接执行（真实 LLM）
%%%   - run_planned/0: 计划分解后执行（mock LLM）
%%%   - run_planned_live/0: 计划分解后执行（真实 LLM）
%%%   - run_parallel_live/0: 并行执行独立步骤（真实 LLM）
%%%   - run_with_reflection_live/0: 带反思的完整流程（真实 LLM）
%%%   - run_with_callbacks/0: 事件回调演示（mock LLM）
%%%   - run_trace_inspection/0: 执行轨迹查看（mock LLM）
%%%
%%% 使用方法:
%%% ```
%%% %% Mock 版本（无需 API Key）
%%% cd examples
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_deepagent:run_simple().
%%% example_deepagent:run_planned().
%%% example_deepagent:run_with_callbacks().
%%% example_deepagent:run_trace_inspection().
%%%
%%% %% 真实 LLM 版本（需要设置 ZHIPU_API_KEY）
%%% export ZHIPU_API_KEY=your-api-key
%%% example_deepagent:run_simple_live().
%%% example_deepagent:run_planned_live().
%%% example_deepagent:run_parallel_live().
%%% example_deepagent:run_with_reflection_live().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_deepagent).

-export([
    run_simple/0, run_simple_live/0,
    run_planned/0, run_planned_live/0,
    run_parallel_live/0,
    run_with_reflection_live/0,
    run_with_callbacks/0,
    run_trace_inspection/0
]).

%%====================================================================
%% 示例 1：简单任务直接执行
%%====================================================================

%% @doc 简单任务直接执行（使用 mock LLM，无需 API Key）
%%
%% 展示 DeepAgent 最基础的用法：
%%   1. 配置 planning_enabled => false，跳过规划
%%   2. 任务作为单一步骤直接执行
%%   3. 返回结果包含 status, response, step_results
-spec run_simple() -> ok.
run_simple() ->
    io:format("=== DeepAgent: Simple Direct Execution (Mock) ===~n~n"),

    %% 1. 创建配置（禁用规划和反思）
    Config = beamai_deepagent:new(#{
        llm => {mock, #{}},
        planning_enabled => false,
        reflection_enabled => false,
        timeout => 10000
    }),

    %% 2. 运行任务
    io:format("Task: What is 2+2?~n"),
    {ok, Result} = beamai_deepagent:run(Config, <<"What is 2+2? Answer with just the number.">>),

    %% 3. 输出结果
    print_result(Result),
    ok.

%% @doc 简单任务直接执行（使用真实 LLM）
%%
%% 使用 GLM-4.7 的 OpenAI 兼容接口执行简单任务。
%% 需要设置环境变量 ZHIPU_API_KEY。
-spec run_simple_live() -> ok.
run_simple_live() ->
    io:format("=== DeepAgent: Simple Direct Execution (Live) ===~n~n"),

    case create_live_llm() of
        {ok, LLM} ->
            Config = beamai_deepagent:new(#{
                llm => LLM,
                planning_enabled => false,
                reflection_enabled => false,
                timeout => 30000
            }),

            Task = <<"用一句话解释什么是 Erlang 的 OTP 框架。"/utf8>>,
            io:format("Task: ~ts~n", [Task]),
            {ok, Result} = beamai_deepagent:run(Config, Task),
            print_result(Result);
        skip ->
            io:format("Skipped: ZHIPU_API_KEY not set~n")
    end,
    ok.

%%====================================================================
%% 示例 2：计划分解后执行
%%====================================================================

%% @doc 计划分解后执行（使用 mock LLM）
%%
%% 展示 DeepAgent 的规划能力（mock 版本通过 meck 模拟）：
%%   1. Planner 子代理分解任务为步骤
%%   2. 依赖分析确定执行顺序
%%   3. 按层逐步执行
%%   4. 最终返回包含计划和步骤结果的完整信息
-spec run_planned() -> ok.
run_planned() ->
    io:format("=== DeepAgent: Planned Execution (Mock) ===~n~n"),

    %% 使用 meck 模拟 beamai_agent 行为
    setup_planning_mock(),

    try
        Config = beamai_deepagent:new(#{
            llm => {mock, #{}},
            planning_enabled => true,
            reflection_enabled => false,
            max_parallel => 2,
            timeout => 10000
        }),

        Task = <<"Research Erlang history and write a summary.">>,
        io:format("Task: ~ts~n~n", [Task]),
        {ok, Result} = beamai_deepagent:run(Config, Task),

        print_result(Result),

        %% 显示计划详情
        case beamai_deepagent:get_plan(Result) of
            undefined ->
                io:format("No plan created (direct execution)~n");
            Plan ->
                io:format("~n--- Plan Status ---~n"),
                io:format("~ts~n", [beamai_deepagent_plan:format_status(Plan)]),
                io:format("Plan complete: ~p~n", [beamai_deepagent_plan:is_complete(Plan)])
        end
    after
        teardown_mock()
    end,
    ok.

%% @doc 计划分解后执行（使用真实 LLM）
%%
%% 让 GLM-4.7 真正分解任务并逐步执行。
%% 展示完整的 规划→执行→汇总 流程。
-spec run_planned_live() -> ok.
run_planned_live() ->
    io:format("=== DeepAgent: Planned Execution (Live) ===~n~n"),

    case create_live_llm() of
        {ok, LLM} ->
            Config = beamai_deepagent:new(#{
                llm => LLM,
                planning_enabled => true,
                reflection_enabled => false,
                max_tool_iterations => 3,
                max_parallel => 2,
                timeout => 60000
            }),

            Task = <<"写一首关于编程的俳句，然后解释为什么它符合俳句的格式要求。"/utf8>>,
            io:format("Task: ~ts~n~n", [Task]),
            {ok, Result} = beamai_deepagent:run(Config, Task),

            print_result(Result),
            print_plan_info(Result),
            print_step_results(Result);
        skip ->
            io:format("Skipped: ZHIPU_API_KEY not set~n")
    end,
    ok.

%%====================================================================
%% 示例 3：并行执行独立步骤
%%====================================================================

%% @doc 并行执行独立步骤（使用真实 LLM）
%%
%% 通过特定的 planner_prompt 引导规划器生成无依赖的步骤，
%% 使多个步骤可以同时并行执行。
%% 展示 DeepAgent 的并行调度能力。
-spec run_parallel_live() -> ok.
run_parallel_live() ->
    io:format("=== DeepAgent: Parallel Execution (Live) ===~n~n"),

    case create_live_llm() of
        {ok, LLM} ->
            Config = beamai_deepagent:new(#{
                llm => LLM,
                planning_enabled => true,
                reflection_enabled => false,
                max_tool_iterations => 3,
                max_parallel => 3,
                timeout => 60000,
                planner_prompt => <<"You are a task planner. Break tasks into INDEPENDENT "
                                    "parallel steps when possible. Steps that don't depend "
                                    "on each other should have empty dependencies lists. "
                                    "Always use the create_plan tool.">>
            }),

            Task = <<"给我三个独立的知识点："
                     "1) 法国的首都是什么，"
                     "2) 水的化学式是什么，"
                     "3) Erlang 语言是哪一年发明的。"
                     "每个知识点作为独立步骤回答。"/utf8>>,
            io:format("Task: ~ts~n~n", [Task]),
            {ok, Result} = beamai_deepagent:run(Config, Task),

            print_result(Result),
            print_plan_info(Result),
            print_step_results(Result);
        skip ->
            io:format("Skipped: ZHIPU_API_KEY not set~n")
    end,
    ok.

%%====================================================================
%% 示例 4：带反思的完整流程
%%====================================================================

%% @doc 带反思的完整流程（使用真实 LLM）
%%
%% 启用反思功能后的完整流程：
%%   1. Planner 分解任务
%%   2. 按层并行执行
%%   3. 每层执行后 Reflector 分析进展
%%   4. 所有步骤完成后进行最终反思，生成汇总回复
%%
%% 反思能帮助 DeepAgent 识别问题并调整策略。
-spec run_with_reflection_live() -> ok.
run_with_reflection_live() ->
    io:format("=== DeepAgent: Full Flow with Reflection (Live) ===~n~n"),

    case create_live_llm() of
        {ok, LLM} ->
            Config = beamai_deepagent:new(#{
                llm => LLM,
                planning_enabled => true,
                reflection_enabled => true,
                max_tool_iterations => 3,
                max_parallel => 2,
                timeout => 60000
            }),

            Task = <<"列出 3 种编程范式，每种用一句话描述其核心思想。"/utf8>>,
            io:format("Task: ~ts~n~n", [Task]),
            {ok, Result} = beamai_deepagent:run(Config, Task),

            print_result(Result),
            print_plan_info(Result),

            %% 显示执行轨迹
            case beamai_deepagent:get_trace(Result) of
                undefined ->
                    ok;
                Trace ->
                    io:format("~n--- Execution Trace ---~n"),
                    io:format("~ts~n", [beamai_deepagent_trace:format(Trace)])
            end;
        skip ->
            io:format("Skipped: ZHIPU_API_KEY not set~n")
    end,
    ok.

%%====================================================================
%% 示例 5：事件回调
%%====================================================================

%% @doc 事件回调演示（使用 mock LLM）
%%
%% 展示如何使用 callbacks 监控 DeepAgent 的执行过程：
%%   - on_start: 任务开始时触发
%%   - on_complete: 任务完成时触发
%%
%% 回调适用于：日志记录、进度通知、指标收集等场景。
-spec run_with_callbacks() -> ok.
run_with_callbacks() ->
    io:format("=== DeepAgent: Event Callbacks (Mock) ===~n~n"),

    %% 使用 meck 模拟 beamai_agent
    setup_simple_mock(),

    try
        Config = beamai_deepagent:new(#{
            llm => {mock, #{}},
            planning_enabled => false,
            reflection_enabled => false,
            timeout => 10000,
            callbacks => #{
                on_start => fun(Task) ->
                    io:format("  [callback] on_start: task=~ts~n", [Task])
                end,
                on_complete => fun({ok, Result}) ->
                    io:format("  [callback] on_complete: status=~p~n",
                              [maps:get(status, Result)])
                end
            }
        }),

        Task = <<"Hello, DeepAgent!">>,
        io:format("Task: ~ts~n~n", [Task]),
        {ok, Result} = beamai_deepagent:run(Config, Task),

        io:format("~n"),
        print_result(Result)
    after
        teardown_mock()
    end,
    ok.

%%====================================================================
%% 示例 6：执行轨迹查看
%%====================================================================

%% @doc 执行轨迹查看（使用 mock LLM）
%%
%% DeepAgent 会记录完整的执行轨迹，包括：
%%   - task_started: 任务开始
%%   - planning_started / plan_created: 规划阶段
%%   - layers_analyzed: 依赖分析完成
%%   - layer_started / layer_completed: 层级执行
%%   - execution_completed: 执行完成
%%
%% 轨迹可用于调试、审计和性能分析。
-spec run_trace_inspection() -> ok.
run_trace_inspection() ->
    io:format("=== DeepAgent: Trace Inspection (Mock) ===~n~n"),

    %% 使用 meck 模拟 beamai_agent
    setup_planning_mock(),

    try
        Config = beamai_deepagent:new(#{
            llm => {mock, #{}},
            planning_enabled => true,
            reflection_enabled => false,
            max_parallel => 2,
            timeout => 10000
        }),

        Task = <<"Analyze and summarize this topic.">>,
        io:format("Task: ~ts~n~n", [Task]),
        {ok, Result} = beamai_deepagent:run(Config, Task),

        %% 显示完整执行轨迹
        case beamai_deepagent:get_trace(Result) of
            undefined ->
                io:format("No trace available~n");
            Trace ->
                io:format("--- Full Execution Trace ---~n"),
                io:format("~ts~n~n", [beamai_deepagent_trace:format(Trace)]),

                %% 显示最近 3 条轨迹记录
                Recent = beamai_deepagent_trace:get_recent(Trace, 3),
                io:format("--- Last 3 Trace Entries ---~n"),
                lists:foreach(fun(Entry) ->
                    io:format("  ~p~n", [Entry])
                end, Recent)
        end
    after
        teardown_mock()
    end,
    ok.

%%====================================================================
%% 内部函数 - LLM 配置
%%====================================================================

%% @doc 创建真实 LLM 配置（GLM-4.7 Coding API）
create_live_llm() ->
    case os:getenv("ZHIPU_API_KEY") of
        false -> skip;
        "" -> skip;
        ApiKey ->
            LLM = beamai_chat_completion:create(openai, #{
                model => <<"glm-4.7">>,
                api_key => list_to_binary(ApiKey),
                base_url => <<"https://open.bigmodel.cn/api/coding/paas/v4">>,
                endpoint => <<"/chat/completions">>
            }),
            {ok, LLM}
    end.

%%====================================================================
%% 内部函数 - Mock 设置
%%====================================================================

%% @doc 设置简单的 mock（直接执行模式）
setup_simple_mock() ->
    meck:new(beamai_agent, [passthrough]),
    meck:expect(beamai_agent, new, fun(_Config) ->
        {ok, fake_agent_state()}
    end),
    meck:expect(beamai_agent, run, fun(_Agent, Msg) ->
        Response = <<"I received your message: ", Msg/binary>>,
        {ok, #{content => Response, tool_calls_made => []}, fake_agent_state()}
    end).

%% @doc 设置规划模式的 mock
%%
%% 第一次调用 run 返回带 create_plan 工具调用的规划结果，
%% 后续调用返回步骤执行结果。
setup_planning_mock() ->
    meck:new(beamai_agent, [passthrough]),
    CallCount = atomics:new(1, [{signed, false}]),

    meck:expect(beamai_agent, new, fun(_Config) ->
        {ok, fake_agent_state()}
    end),
    meck:expect(beamai_agent, run, fun(_Agent, _Msg) ->
        N = atomics:add_get(CallCount, 1, 1),
        case N of
            1 ->
                %% 第一次调用：Planner 返回计划
                {ok, #{
                    content => <<"Plan created successfully.">>,
                    tool_calls_made => [
                        #{name => <<"deepagent_plan.create_plan">>,
                          args => #{
                            <<"goal">> => <<"Complete the research task">>,
                            <<"steps">> => [
                                #{<<"description">> => <<"Research the topic">>,
                                  <<"dependencies">> => []},
                                #{<<"description">> => <<"Gather supporting details">>,
                                  <<"dependencies">> => []},
                                #{<<"description">> => <<"Write the summary">>,
                                  <<"dependencies">> => [1, 2]}
                            ]
                          }}
                    ]
                }, fake_agent_state()};
            _ ->
                %% 后续调用：Executor 返回步骤结果
                StepN = N - 1,
                Content = iolist_to_binary([
                    <<"Step ">>, integer_to_binary(StepN),
                    <<" completed: Mock result for this step.">>
                ]),
                {ok, #{content => Content, tool_calls_made => []}, fake_agent_state()}
        end
    end).

%% @doc 清理 mock
teardown_mock() ->
    meck:unload(beamai_agent).

%% @doc 生成假的 agent state（满足类型要求）
fake_agent_state() ->
    #{mock_agent => true, kernel => undefined, messages => [],
      turn_count => 0, callbacks => #{}, max_tool_iterations => 10,
      system_prompt => <<>>, name => <<"mock">>, id => <<"m1">>,
      metadata => #{}, interrupt_state => undefined, auto_save => false}.

%%====================================================================
%% 内部函数 - 结果输出
%%====================================================================

%% @doc 打印运行结果摘要
print_result(Result) ->
    io:format("~n--- Result ---~n"),
    io:format("Status: ~p~n", [maps:get(status, Result)]),
    Response = maps:get(response, Result, <<>>),
    case byte_size(Response) > 500 of
        true ->
            io:format("Response (truncated): ~ts...~n", [binary:part(Response, 0, 500)]);
        false ->
            io:format("Response: ~ts~n", [Response])
    end.

%% @doc 打印计划信息
print_plan_info(Result) ->
    case beamai_deepagent:get_plan(Result) of
        undefined ->
            io:format("~nNo plan (direct execution)~n");
        Plan ->
            io:format("~n--- Plan ---~n"),
            io:format("~ts~n", [beamai_deepagent_plan:format_status(Plan)]),
            io:format("Complete: ~p~n", [beamai_deepagent_plan:is_complete(Plan)])
    end.

%% @doc 打印各步骤执行结果
print_step_results(Result) ->
    StepResults = maps:get(step_results, Result, []),
    case StepResults of
        [] ->
            ok;
        _ ->
            io:format("~n--- Step Results (~p steps) ---~n", [length(StepResults)]),
            lists:foreach(fun(SR) ->
                StepId = maps:get(step_id, SR, 0),
                Status = maps:get(status, SR, unknown),
                TimeMs = maps:get(time_ms, SR, 0),
                StepResult = maps:get(result, SR, <<>>),
                Truncated = case byte_size(StepResult) > 200 of
                    true -> <<(binary:part(StepResult, 0, 200))/binary, "...">>;
                    false -> StepResult
                end,
                io:format("  Step ~p [~p] (~pms): ~ts~n",
                          [StepId, Status, TimeMs, Truncated])
            end, StepResults)
    end.
