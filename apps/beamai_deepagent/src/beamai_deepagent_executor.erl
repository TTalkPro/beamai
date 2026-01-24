%%%-------------------------------------------------------------------
%%% @doc DeepAgent 执行子代理
%%%
%%% 负责执行计划中的单个步骤。
%%% 使用 beamai_agent 实例配备工具插件，在超时保护下运行。
%%% 结果统一为 step_result() 格式。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_executor).

-export([execute_step/3]).

-type step_result() :: #{
    step_id := pos_integer(),
    status := completed | failed | crashed | timeout,
    result := binary(),
    tool_calls_made := [map()],
    time_ms := non_neg_integer()
}.

-export_type([step_result/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 执行计划中的单个步骤
%%
%% 创建配备工具的子代理，在超时保护下运行步骤描述。
%% 将子代理的执行结果统一封装为 step_result()。
%%
%% Step: 步骤信息（需包含 id, description 字段）
%% CoordState: 协调器状态（需包含 config 字段）
%% Context: 上下文信息（可包含 previous_results 字段）
-spec execute_step(map(), map(), map()) -> {ok, step_result()} | {error, term()}.
execute_step(Step, CoordState, Context) ->
    StartTime = erlang:system_time(millisecond),
    Config = maps:get(config, CoordState),
    StepId = maps:get(id, Step),

    AgentConfig = build_agent_config(Config, Step, Context),
    Description = maps:get(description, Step),
    Timeout = maps:get(timeout, Config, 300000),

    case run_with_timeout(AgentConfig, Description, Timeout) of
        {ok, Content, ToolCalls} ->
            Elapsed = erlang:system_time(millisecond) - StartTime,
            {ok, beamai_deepagent_utils:make_step_result(StepId, completed, Content,
                #{tool_calls_made => ToolCalls, time_ms => Elapsed})};
        {error, timeout} ->
            Elapsed = erlang:system_time(millisecond) - StartTime,
            {ok, beamai_deepagent_utils:make_step_result(StepId, timeout,
                <<"Step execution timed out">>, #{time_ms => Elapsed})};
        {error, Reason} ->
            Elapsed = erlang:system_time(millisecond) - StartTime,
            ErrMsg = beamai_deepagent_utils:format_error(Reason),
            {ok, beamai_deepagent_utils:make_step_result(StepId, failed,
                ErrMsg, #{time_ms => Elapsed})}
    end.

%%====================================================================
%% 内部函数 - 代理配置构建
%%====================================================================

%% @doc 根据全局配置和步骤上下文构建执行代理的配置
%%
%% 将系统提示词、插件列表、LLM 配置组装为 beamai_agent 可接受的格式。
build_agent_config(Config, Step, Context) ->
    LLM = maps:get(llm, Config),
    Plugins = maps:get(plugins, Config, beamai_deepagent_utils:available_plugins()),
    MaxIter = maps:get(max_tool_iterations, Config, 10),
    BasePrompt = maps:get(executor_prompt, Config, default_executor_prompt()),
    SystemPrompt = build_system_prompt(BasePrompt, Step, Context),

    #{
        llm => LLM,
        plugins => Plugins,
        system_prompt => SystemPrompt,
        max_tool_iterations => MaxIter
    }.

%% @doc 构建包含步骤描述和前序结果的系统提示词
%%
%% 将基础提示词、当前步骤信息、已完成步骤结果组合为完整提示词。
build_system_prompt(BasePrompt, Step, Context) ->
    StepDesc = maps:get(description, Step, <<>>),
    StepId = maps:get(id, Step, 0),
    PrevResults = maps:get(previous_results, Context, []),

    ContextSection = format_context_section(PrevResults),

    iolist_to_binary([
        BasePrompt, <<"\n\n">>,
        <<"## Current Step\n">>,
        <<"Step ">>, integer_to_binary(StepId), <<": ">>, StepDesc, <<"\n\n">>,
        ContextSection
    ]).

%% @doc 格式化前序步骤结果为提示词段落
%%
%% 如果无前序结果返回空二进制，否则格式化为带标题的结果列表。
format_context_section([]) ->
    <<>>;
format_context_section(Results) ->
    Summary = beamai_deepagent_utils:format_step_results(Results),
    iolist_to_binary([<<"## Previous Step Results\n">>, Summary, <<"\n\n">>]).

%%====================================================================
%% 内部函数 - 超时执行
%%====================================================================

%% @doc 在超时保护下运行子代理
%%
%% 通过 spawn + receive 实现超时控制。
%% 超时时终止工作进程并返回 timeout 错误。
run_with_timeout(AgentConfig, Message, Timeout) ->
    Self = self(),
    Ref = make_ref(),

    Pid = spawn(fun() ->
        Result = execute_agent(AgentConfig, Message),
        Self ! {Ref, Result}
    end),

    receive
        {Ref, Result} -> Result
    after Timeout ->
        exit(Pid, kill),
        {error, timeout}
    end.

%% @doc 创建并运行执行代理，提取结果
%%
%% 与 utils:run_agent 不同，此函数还提取 tool_calls_made 信息。
execute_agent(AgentConfig, Message) ->
    case beamai_agent:new(AgentConfig) of
        {ok, Agent} ->
            case beamai_agent:run(Agent, Message) of
                {ok, RunResult, _} ->
                    Content = maps:get(content, RunResult, <<>>),
                    ToolCalls = maps:get(tool_calls_made, RunResult, []),
                    {ok, Content, ToolCalls};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数 - 默认提示词
%%====================================================================

%% @doc 默认的执行代理系统提示词
default_executor_prompt() ->
    <<"You are a task execution agent. Execute the given step precisely and completely.\n\n"
      "Guidelines:\n"
      "- Focus on completing the current step's objective\n"
      "- Use the available tools as needed\n"
      "- Report your results clearly\n"
      "- If you encounter errors, try to recover or report the issue clearly">>.
