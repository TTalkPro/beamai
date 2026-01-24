%%%-------------------------------------------------------------------
%%% @doc DeepAgent 反思子代理
%%%
%%% 使用纯分析代理（无工具）对执行进展进行反思分析。
%%% 反思失败不影响主流程（non-fatal）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_reflector).

-export([reflect/4, final_reflection/3]).

%%====================================================================
%% API
%%====================================================================

%% @doc 对当前层的执行结果进行反思分析
%%
%% 创建纯分析代理，输入计划状态和本层结果，输出进展评估。
%% 反思异常不会传播到调用方（non-fatal）。
%%
%% Plan: 当前计划状态
%% Results: 本层执行结果列表
%% LayerIdx: 当前层索引
%% Config: 协调器配置（需包含 llm 字段）
-spec reflect(beamai_deepagent_plan:t(), [map()], pos_integer(), map()) ->
    {ok, binary()} | {error, term()}.
reflect(Plan, Results, LayerIdx, Config) ->
    Prompt = build_layer_prompt(Plan, Results, LayerIdx),
    safe_run_reflector(Config, Prompt).

%% @doc 对整体执行结果进行最终反思
%%
%% 创建纯分析代理，汇总所有步骤结果，生成面向用户的最终回复。
%% 反思异常不会传播到调用方（non-fatal）。
%%
%% Plan: 完成后的计划
%% AllResults: 所有步骤结果列表
%% Config: 协调器配置（需包含 llm 字段）
-spec final_reflection(beamai_deepagent_plan:t(), [map()], map()) ->
    {ok, binary()} | {error, term()}.
final_reflection(Plan, AllResults, Config) ->
    Prompt = build_final_prompt(Plan, AllResults),
    safe_run_reflector(Config, Prompt).

%%====================================================================
%% 内部函数 - 代理运行
%%====================================================================

%% @doc 安全运行反思代理，捕获所有异常
%%
%% 构建无工具的分析代理配置，通过 utils:run_agent 执行。
%% 任何异常（包括进程崩溃）都被捕获并返回 error tuple。
-spec safe_run_reflector(map(), binary()) -> {ok, binary()} | {error, term()}.
safe_run_reflector(Config, Prompt) ->
    try
        LLM = maps:get(llm, Config),
        ReflectorPrompt = maps:get(reflector_prompt, Config, default_reflector_prompt()),
        AgentConfig = #{
            llm => LLM,
            plugins => [],
            system_prompt => ReflectorPrompt,
            max_tool_iterations => 1
        },
        beamai_deepagent_utils:run_agent(AgentConfig, Prompt, reflector)
    catch
        Class:Reason:_Stack ->
            {error, {reflection_exception, Class, Reason}}
    end.

%%====================================================================
%% 内部函数 - 提示词构建
%%====================================================================

%% @doc 构建层级反思提示词
%%
%% 将计划状态和本层结果格式化为结构化提示词，
%% 引导 LLM 评估进展、识别问题并提出建议。
-spec build_layer_prompt(beamai_deepagent_plan:t(), [map()], pos_integer()) -> binary().
build_layer_prompt(Plan, Results, LayerIdx) ->
    PlanStatus = beamai_deepagent_plan:format_status(Plan),
    ResultsSummary = beamai_deepagent_utils:format_step_results(Results),
    LayerBin = integer_to_binary(LayerIdx),

    iolist_to_binary([
        <<"Analyze the execution progress after completing layer ">>,
        LayerBin, <<".\n\n">>,
        <<"## Plan Status\n">>, PlanStatus, <<"\n\n">>,
        <<"## Layer ">>, LayerBin, <<" Results\n">>,
        ResultsSummary, <<"\n\n">>,
        <<"Please provide:\n">>,
        <<"1. Progress assessment - what has been accomplished\n">>,
        <<"2. Issues identified - any failures or concerns\n">>,
        <<"3. Recommendations - adjustments for remaining steps">>
    ]).

%% @doc 构建最终反思提示词
%%
%% 将完成的计划和所有步骤结果格式化为结构化提示词，
%% 引导 LLM 生成面向用户的总结性回复。
-spec build_final_prompt(beamai_deepagent_plan:t(), [map()]) -> binary().
build_final_prompt(Plan, AllResults) ->
    PlanStatus = beamai_deepagent_plan:format_status(Plan),
    ResultsSummary = beamai_deepagent_utils:format_step_results(AllResults),

    iolist_to_binary([
        <<"Provide a final summary of the task execution.\n\n">>,
        <<"## Final Plan Status\n">>, PlanStatus, <<"\n\n">>,
        <<"## All Step Results\n">>, ResultsSummary, <<"\n\n">>,
        <<"Please provide:\n">>,
        <<"1. Overall outcome summary\n">>,
        <<"2. Key accomplishments\n">>,
        <<"3. Any unresolved issues\n">>,
        <<"4. A concise final response suitable for the user">>
    ]).

%%====================================================================
%% 内部函数 - 默认提示词
%%====================================================================

%% @doc 默认的反思代理系统提示词
-spec default_reflector_prompt() -> binary().
default_reflector_prompt() ->
    <<"You are an analytical reflection agent. Your role is to assess execution progress, "
      "identify issues, and provide insights. You do not execute any actions - only analyze "
      "and report.\n\n"
      "Be concise and actionable in your analysis.">>.
