%%%-------------------------------------------------------------------
%%% @doc DeepAgent 工具集成示例
%%%
%%% 演示 DeepAgent 如何配合工具插件执行复杂任务：
%%%   - run_with_plugins_live/0: 配备文件和 Shell 插件的深度代理
%%%   - run_custom_plugins_live/0: 使用自定义插件的深度代理
%%%   - run_code_analysis_live/0: 代码分析场景示例
%%%
%%% DeepAgent 的执行器（Executor）可以配备工具插件，
%%% 使每个步骤都能调用工具完成实际操作（如读写文件、执行命令等）。
%%%
%%% 使用方法:
%%% ```
%%% cd examples
%%% export ZHIPU_API_KEY=your-api-key
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_deepagent_tools:run_with_plugins_live().
%%% example_deepagent_tools:run_custom_plugins_live().
%%% example_deepagent_tools:run_code_analysis_live().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_deepagent_tools).

-export([
    run_with_plugins_live/0,
    run_custom_plugins_live/0,
    run_code_analysis_live/0
]).

%%====================================================================
%% 示例 1：配备内置插件的深度代理
%%====================================================================

%% @doc 使用内置文件和 Shell 插件执行任务
%%
%% DeepAgent 默认会检测系统中可用的插件模块
%% （beamai_plugin_file, beamai_plugin_shell, beamai_plugin_todo），
%% 但也可以显式指定 plugins 列表。
%%
%% 此示例让 DeepAgent 使用文件插件读取并分析一个文件。
-spec run_with_plugins_live() -> ok.
run_with_plugins_live() ->
    io:format("=== DeepAgent: Built-in Plugins (Live) ===~n~n"),

    case create_live_llm() of
        {ok, LLM} ->
            %% 显式指定使用文件插件
            Plugins = available_plugins(),
            io:format("Available plugins: ~p~n~n", [Plugins]),

            Config = beamai_deepagent:new(#{
                llm => LLM,
                plugins => Plugins,
                planning_enabled => false,
                reflection_enabled => false,
                max_tool_iterations => 5,
                timeout => 60000
            }),

            Task = <<"读取当前目录下的 rebar.config 文件，"
                     "列出其中定义的依赖项。"/utf8>>,
            io:format("Task: ~ts~n", [Task]),
            {ok, Result} = beamai_deepagent:run(Config, Task),

            print_result(Result),
            print_tool_calls(Result);
        skip ->
            io:format("Skipped: ZHIPU_API_KEY not set~n")
    end,
    ok.

%%====================================================================
%% 示例 2：使用自定义插件的深度代理
%%====================================================================

%% @doc 使用自定义天气查询插件执行多步骤任务
%%
%% 展示如何将自定义插件集成到 DeepAgent 的执行器中。
%% 规划器会分解任务，执行器会使用提供的插件工具。
%%
%% 此示例使用 example_weather_plugin（如果可用）。
-spec run_custom_plugins_live() -> ok.
run_custom_plugins_live() ->
    io:format("=== DeepAgent: Custom Plugins (Live) ===~n~n"),

    case create_live_llm() of
        {ok, LLM} ->
            %% 检查天气插件是否可用
            CustomPlugins = case code:ensure_loaded(example_weather_plugin) of
                {module, _} -> [example_weather_plugin];
                _ ->
                    io:format("Note: example_weather_plugin not loaded, using empty plugin list~n"),
                    []
            end,

            Config = beamai_deepagent:new(#{
                llm => LLM,
                plugins => CustomPlugins,
                planning_enabled => true,
                reflection_enabled => false,
                max_tool_iterations => 3,
                max_parallel => 2,
                timeout => 60000
            }),

            Task = <<"查询北京和上海的天气情况，然后对比两座城市的温度差异。"/utf8>>,
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
%% 示例 3：代码分析场景
%%====================================================================

%% @doc 代码分析场景示例
%%
%% 展示 DeepAgent 在软件工程场景中的应用：
%%   1. Planner 将代码分析任务分解为多个子任务
%%   2. Executor 使用文件插件读取源代码
%%   3. 各步骤独立分析不同方面
%%   4. 最终反思汇总分析结果
%%
%% 这是 DeepAgent 的典型应用场景之一。
-spec run_code_analysis_live() -> ok.
run_code_analysis_live() ->
    io:format("=== DeepAgent: Code Analysis Scenario (Live) ===~n~n"),

    case create_live_llm() of
        {ok, LLM} ->
            Plugins = available_plugins(),

            Config = beamai_deepagent:new(#{
                llm => LLM,
                plugins => Plugins,
                planning_enabled => true,
                reflection_enabled => true,
                max_tool_iterations => 5,
                max_parallel => 2,
                timeout => 90000,
                executor_prompt => <<"You are a code analysis agent. "
                                     "Use the available file tools to read source code "
                                     "and provide detailed analysis. "
                                     "Focus on code quality, patterns, and potential issues.">>
            }),

            Task = <<"分析 src/example_deepagent.erl 文件的代码结构，"
                     "包括：1) 模块导出了哪些函数，"
                     "2) 主要的代码组织方式，"
                     "3) 是否有改进建议。"/utf8>>,
            io:format("Task: ~ts~n~n", [Task]),
            {ok, Result} = beamai_deepagent:run(Config, Task),

            print_result(Result),
            print_plan_info(Result),
            print_step_results(Result),

            %% 显示执行轨迹
            case beamai_deepagent:get_trace(Result) of
                undefined -> ok;
                Trace ->
                    io:format("~n--- Execution Trace ---~n"),
                    io:format("~ts~n", [beamai_deepagent_trace:format(Trace)])
            end;
        skip ->
            io:format("Skipped: ZHIPU_API_KEY not set~n")
    end,
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 创建真实 LLM 配置
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

%% @doc 获取系统中可用的工具插件列表
available_plugins() ->
    Candidates = [beamai_plugin_file, beamai_plugin_shell, beamai_plugin_todo],
    [M || M <- Candidates, code:ensure_loaded(M) =:= {module, M}].

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
        [] -> ok;
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

%% @doc 打印工具调用记录
print_tool_calls(Result) ->
    StepResults = maps:get(step_results, Result, []),
    ToolCalls = lists:flatmap(fun(SR) ->
        maps:get(tool_calls_made, SR, [])
    end, StepResults),
    case ToolCalls of
        [] ->
            io:format("~nNo tool calls made~n");
        _ ->
            io:format("~n--- Tool Calls (~p total) ---~n", [length(ToolCalls)]),
            lists:foreach(fun(TC) ->
                Name = maps:get(name, TC, <<"unknown">>),
                io:format("  - ~ts~n", [Name])
            end, ToolCalls)
    end.
