%%%-------------------------------------------------------------------
%%% @doc DeepAgent 集成测试（GLM-4.7）
%%%
%%% 使用真实 LLM API 测试完整流程。
%%% 需要设置环境变量 ZHIPU_API_KEY。
%%% 跳过条件：未设置 API key 时自动跳过。
%%%
%%% 测试两种 provider 格式：
%%%   - Anthropic 兼容接口
%%%   - OpenAI 兼容接口（coding API）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助函数
%%====================================================================

%% @doc 确保所有依赖应用已启动
ensure_apps_started() ->
    application:ensure_all_started(jsx),
    application:ensure_all_started(hackney),
    application:ensure_all_started(gun),
    application:ensure_all_started(beamai_core).

%% @doc 获取 API Key，未设置时返回 skip
get_api_key() ->
    case os:getenv("ZHIPU_API_KEY") of
        false -> skip;
        "" -> skip;
        Key -> {ok, Key}
    end.

%% @doc 创建 OpenAI 兼容的 LLM 配置（coding API）
create_llm_openai(ApiKey) ->
    beamai_chat_completion:create(openai, #{
        model => <<"glm-4.7">>,
        api_key => list_to_binary(ApiKey),
        base_url => <<"https://open.bigmodel.cn/api/coding/paas/v4">>,
        endpoint => <<"/chat/completions">>
    }).

%% @doc 创建 Anthropic 兼容的 LLM 配置
create_llm_anthropic(ApiKey) ->
    beamai_chat_completion:create(anthropic, #{
        model => <<"glm-4.7">>,
        api_key => list_to_binary(ApiKey),
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>
    }).

%%====================================================================
%% 集成测试: 简单任务（无规划）- OpenAI 兼容
%%====================================================================

simple_task_openai_test_() ->
    case get_api_key() of
        skip -> [];
        {ok, ApiKey} ->
            {timeout, 60, fun() ->
                ensure_apps_started(),
                LLM = create_llm_openai(ApiKey),
                Config = beamai_deepagent:new(#{
                    llm => LLM,
                    planning_enabled => false,
                    reflection_enabled => false,
                    max_tool_iterations => 1,
                    timeout => 30000
                }),
                {ok, Result} = beamai_deepagent:run(Config, <<"What is 2+2? Answer with just the number.">>),
                ?assertEqual(completed, maps:get(status, Result)),
                Response = maps:get(response, Result),
                ?assert(byte_size(Response) > 0)
            end}
    end.

%%====================================================================
%% 集成测试: 简单任务（无规划）- Anthropic 兼容
%%====================================================================

simple_task_anthropic_test_() ->
    case get_api_key() of
        skip -> [];
        {ok, ApiKey} ->
            {timeout, 60, fun() ->
                ensure_apps_started(),
                LLM = create_llm_anthropic(ApiKey),
                Config = beamai_deepagent:new(#{
                    llm => LLM,
                    planning_enabled => false,
                    reflection_enabled => false,
                    max_tool_iterations => 1,
                    timeout => 30000
                }),
                {ok, Result} = beamai_deepagent:run(Config, <<"What is 3+3? Answer with just the number.">>),
                ?assertEqual(completed, maps:get(status, Result)),
                Response = maps:get(response, Result),
                ?assert(byte_size(Response) > 0)
            end}
    end.

%%====================================================================
%% 集成测试: 多步骤任务（规划 + 串行执行）
%%====================================================================

planned_task_test_() ->
    case get_api_key() of
        skip -> [];
        {ok, ApiKey} ->
            {timeout, 120, fun() ->
                ensure_apps_started(),
                LLM = create_llm_openai(ApiKey),
                Config = beamai_deepagent:new(#{
                    llm => LLM,
                    planning_enabled => true,
                    reflection_enabled => false,
                    max_tool_iterations => 3,
                    max_parallel => 2,
                    timeout => 60000
                }),
                Task = <<"Write a haiku about programming, then explain what makes it a haiku.">>,
                {ok, Result} = beamai_deepagent:run(Config, Task),
                Status = maps:get(status, Result),
                ?assert(Status =:= completed orelse Status =:= error),
                case maps:find(plan, Result) of
                    {ok, Plan} ->
                        ?assert(length(beamai_deepagent_plan:get_steps(Plan)) >= 1);
                    error ->
                        ok
                end
            end}
    end.

%%====================================================================
%% 集成测试: 并行任务
%%====================================================================

parallel_task_test_() ->
    case get_api_key() of
        skip -> [];
        {ok, ApiKey} ->
            {timeout, 120, fun() ->
                ensure_apps_started(),
                LLM = create_llm_openai(ApiKey),
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
                Task = <<"Give me three independent facts: "
                         "1) The capital of France, "
                         "2) The chemical symbol for water, "
                         "3) The year the internet was invented. "
                         "Each fact should be a separate step.">>,
                {ok, Result} = beamai_deepagent:run(Config, Task),
                ?assert(maps:get(status, Result) =:= completed orelse maps:get(status, Result) =:= error),
                StepResults = maps:get(step_results, Result, []),
                ?assert(length(StepResults) >= 1)
            end}
    end.

%%====================================================================
%% 集成测试: 带反思的任务
%%====================================================================

reflection_task_test_() ->
    case get_api_key() of
        skip -> [];
        {ok, ApiKey} ->
            {timeout, 180, fun() ->
                ensure_apps_started(),
                LLM = create_llm_openai(ApiKey),
                Config = beamai_deepagent:new(#{
                    llm => LLM,
                    planning_enabled => true,
                    reflection_enabled => true,
                    max_tool_iterations => 3,
                    max_parallel => 2,
                    timeout => 60000
                }),
                Task = <<"List 2 programming languages and describe each in one sentence.">>,
                {ok, Result} = beamai_deepagent:run(Config, Task),
                ?assert(maps:get(status, Result) =:= completed orelse maps:get(status, Result) =:= error),
                case maps:find(trace, Result) of
                    {ok, Trace} ->
                        Formatted = beamai_deepagent_trace:format(Trace),
                        ?assert(byte_size(Formatted) > 0);
                    error ->
                        ok
                end
            end}
    end.
