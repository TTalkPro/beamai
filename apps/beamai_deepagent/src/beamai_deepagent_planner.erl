%%%-------------------------------------------------------------------
%%% @doc DeepAgent 规划子代理
%%%
%%% 使用 beamai_agent 实例作为规划代理，配备 plan_plugin 工具，
%%% 让 LLM 分解任务为可执行步骤并输出结构化计划。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_planner).

-export([create_plan/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc 使用规划代理创建执行计划
%%
%% 1. 创建配备 plan_plugin 的代理
%% 2. 运行代理，让 LLM 分析任务并调用 create_plan 工具
%% 3. 从运行结果的 tool_calls_made 中提取计划参数
%% 4. 构建并返回 plan 数据结构
%%
%% Config: 深度代理配置（需包含 llm 字段）
%% Task: 用户任务描述
-spec create_plan(map(), binary()) -> {ok, beamai_deepagent_plan:t()} | {error, term()}.
create_plan(Config, Task) ->
    LLM = maps:get(llm, Config),
    PlannerPrompt = maps:get(planner_prompt, Config, default_planner_prompt()),
    MaxIter = maps:get(max_tool_iterations, Config, 3),

    AgentConfig = #{
        llm => LLM,
        plugins => [beamai_deepagent_plan_plugin],
        system_prompt => PlannerPrompt,
        max_tool_iterations => MaxIter
    },

    case beamai_agent:new(AgentConfig) of
        {ok, Agent} ->
            PlanMessage = <<"Please analyze this task and create a plan:\n\n", Task/binary>>,
            case beamai_agent:run(Agent, PlanMessage) of
                {ok, Result, _NewAgent} ->
                    extract_plan(Result);
                {error, Reason} ->
                    {error, {planner_run_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {planner_init_failed, Reason}}
    end.

%%====================================================================
%% 内部函数 - 计划提取
%%====================================================================

%% @doc 从代理运行结果中提取计划
%%
%% 在 tool_calls_made 列表中查找 create_plan 调用，
%% 提取其参数构建 plan 数据结构。
-spec extract_plan(map()) -> {ok, beamai_deepagent_plan:t()} | {error, term()}.
extract_plan(#{tool_calls_made := ToolCalls}) ->
    case find_create_plan_call(ToolCalls) of
        {ok, #{<<"goal">> := Goal, <<"steps">> := Steps}} ->
            {ok, beamai_deepagent_plan:new(Goal, Steps)};
        not_found ->
            {error, planner_no_plan_created}
    end;
extract_plan(_) ->
    {error, planner_no_plan_created}.

%% @doc 在工具调用列表中查找 create_plan 调用
%%
%% 遍历 tool_calls_made 列表，匹配名称为 "deepagent_plan.create_plan" 的调用。
-spec find_create_plan_call([map()]) -> {ok, map()} | not_found.
find_create_plan_call([]) ->
    not_found;
find_create_plan_call([#{name := <<"deepagent_plan.create_plan">>, args := Args} | _]) ->
    {ok, Args};
find_create_plan_call([_ | Rest]) ->
    find_create_plan_call(Rest).

%%====================================================================
%% 内部函数 - 默认提示词
%%====================================================================

%% @doc 默认的规划代理系统提示词
-spec default_planner_prompt() -> binary().
default_planner_prompt() ->
    <<"You are a task planning agent. Your job is to break down complex tasks into "
      "atomic, executable steps.\n\n"
      "Guidelines:\n"
      "- Decompose the task into small, actionable steps\n"
      "- Each step should be independently executable\n"
      "- Mark dependencies between steps (steps that must complete before others can start)\n"
      "- Steps without mutual dependencies can be executed in parallel\n"
      "- Set requires_deep=true for steps that need complex reasoning or multi-step tool use\n"
      "- Use the create_plan tool to output your plan\n\n"
      "Always use the create_plan tool to submit your structured plan.">>.
