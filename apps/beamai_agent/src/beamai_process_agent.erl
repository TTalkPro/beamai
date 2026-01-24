%%%-------------------------------------------------------------------
%%% @doc Process-native Agent
%%%
%%% 基于 Process Framework 实现的 Agent，与 beamai_agent 并存。
%%% 核心区别：Tool 调用通过 Process 事件路由，对外可见可控。
%%%
%%% == 何时使用 ==
%%%   - 需要在 tool 调用间插入审批、日志等步骤
%%%   - 需要 Process 级别的 snapshot/restore
%%%   - 需要与其他 Process step 组合编排
%%%   - 需要细粒度的 tool 执行监控
%%%
%%% == 何时使用 beamai_agent ==
%%%   - 简单的单次或多轮对话
%%%   - 不需要外部介入 tool loop
%%%   - 需要最小启动开销（无需 Process 基础设施）
%%%
%%% == 内部结构 ==
%%% 自动构建包含两个 step 和循环 binding 的 Process：
%%% ```
%%%   user_message -> [llm_step] --tool_request--> [tool_step]
%%%                       ^                             |
%%%                       |________tool_results_________|
%%%                       |
%%%                   agent_done --> (output)
%%% ```
%%%
%%% == 使用示例 ==
%%% ```
%%% Config = #{
%%%     system_prompt => <<"你是一个助手"/utf8>>,
%%%     llm => {zhipu, #{model => <<"glm-4-flash">>}},
%%%     plugins => [my_tools_plugin]
%%% },
%%% {ok, Result} = beamai_process_agent:run_sync(Config, <<"你好"/utf8>>).
%%% io:format("~s~n", [maps:get(response, Result)]).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_agent).

-export([
    %% 构建 API
    build/1,

    %% 运行 API
    start/1,
    start/2,
    send_message/2,
    run_sync/2,
    run_sync/3,

    %% 控制 API
    resume/2,
    stop/1,
    get_status/1
]).

%%====================================================================
%% 构建 API
%%====================================================================

%% @doc 从配置构建 Process 定义
%%
%% 自动创建 LLM step + Tool step + 循环 bindings。
%%
%% Config 选项：
%%   system_prompt — 系统提示词
%%   kernel — 预构建的 kernel（与 llm/plugins 互斥）
%%   llm — LLM 配置（{Provider, Opts} 或 config map）
%%   plugins — 插件模块列表
%%   middlewares — 中间件列表
%%   max_tool_iterations — 最大 tool 迭代次数（默认 10）
%%   output_event — 完成事件名（默认 agent_done）
%%   on_tool_call — tool 调用钩子 fun((Name, Args) -> ok | {pause, Reason})
%%   extra_steps — 额外的 step 定义列表 [{StepId, Module, Config}]
%%   extra_bindings — 额外的 event bindings
%%
%% @param Config 配置 map
%% @returns {ok, {ProcessSpec, Context}} | {error, Reason}
-spec build(map()) -> {ok, {beamai_process_builder:process_spec(), beamai_context:t()}} | {error, term()}.
build(Config) ->
    try
        %% 1. 构建 Kernel
        Kernel = beamai_agent_state:build_kernel(Config),

        %% 2. 构建 Context（携带 Kernel）
        Context = beamai_context:with_kernel(beamai_context:new(), Kernel),

        %% 3. 提取配置
        SystemPrompt = maps:get(system_prompt, Config, undefined),
        MaxIter = maps:get(max_tool_iterations, Config, 10),
        OutputEvent = maps:get(output_event, Config, agent_done),
        OnToolCall = maps:get(on_tool_call, Config, undefined),

        %% 4. 构建 Process
        LlmStepConfig = #{
            system_prompt => SystemPrompt,
            max_tool_iterations => MaxIter,
            output_event => OutputEvent,
            required_inputs => []  %% 由 can_activate 决定激活条件
        },
        ToolStepConfig = #{
            on_tool_call => OnToolCall,
            required_inputs => [tool_request]
        },

        B0 = beamai_process:builder(process_agent),
        B1 = beamai_process:add_step(B0, llm_step, beamai_process_agent_llm_step, LlmStepConfig),
        B2 = beamai_process:add_step(B1, tool_step, beamai_process_agent_tool_step, ToolStepConfig),

        %% 5. 核心 bindings: user_message -> llm, tool_request -> tool, tool_results -> llm
        B3 = beamai_process:on_event(B2, user_message, llm_step, user_message),
        B4 = beamai_process:on_event(B3, tool_request, tool_step, tool_request),
        B5 = beamai_process:on_event(B4, tool_results, llm_step, tool_results),

        %% 6. 添加额外 steps 和 bindings
        B6 = add_extra_steps(B5, maps:get(extra_steps, Config, [])),
        B7 = add_extra_bindings(B6, maps:get(extra_bindings, Config, [])),

        %% 7. 编译
        case beamai_process:build(B7) of
            {ok, ProcessSpec} ->
                {ok, {ProcessSpec, Context}};
            {error, Errors} ->
                {error, {build_failed, Errors}}
        end
    catch
        error:Reason:Stack ->
            {error, {build_exception, Reason, Stack}}
    end.

%%====================================================================
%% 运行 API
%%====================================================================

%% @doc 启动 Process Agent
%%
%% 构建并启动 Process，返回运行时 PID。
%% 启动后需调用 send_message/2 发送用户消息。
-spec start(map()) -> {ok, pid()} | {error, term()}.
start(Config) ->
    start(Config, #{}).

-spec start(map(), map()) -> {ok, pid()} | {error, term()}.
start(Config, Opts) ->
    case build(Config) of
        {ok, {ProcessSpec, Context}} ->
            RuntimeOpts = Opts#{context => Context},
            beamai_process:start(ProcessSpec, RuntimeOpts);
        {error, _} = Error ->
            Error
    end.

%% @doc 向运行中的 Process Agent 发送用户消息
-spec send_message(pid(), binary()) -> ok.
send_message(Pid, Message) ->
    Event = beamai_process_event:new(user_message, Message),
    beamai_process:send_event(Pid, Event).

%% @doc 同步执行：构建、启动、发送消息、等待完成
%%
%% 最简单的使用方式，适合不需要交互控制的场景。
%%
%% @param Config Agent 配置
%% @param UserMessage 用户消息
%% @returns {ok, Result} | {error, Reason}
%%   Result 包含 response, tool_calls_made, turn_count 等字段
-spec run_sync(map(), binary()) -> {ok, map()} | {error, term()}.
run_sync(Config, UserMessage) ->
    run_sync(Config, UserMessage, #{}).

-spec run_sync(map(), binary(), map()) -> {ok, map()} | {error, term()}.
run_sync(Config, UserMessage, Opts) ->
    Timeout = maps:get(timeout, Opts, 60000),
    case build(Config) of
        {ok, {ProcessSpec, Context}} ->
            %% 设置初始事件为 user_message
            InitEvent = beamai_process_event:new(user_message, UserMessage),
            ProcessSpecWithInit = ProcessSpec#{
                initial_events => [InitEvent]
            },
            RuntimeOpts = #{context => Context, caller => self()},
            case beamai_process:start(ProcessSpecWithInit, RuntimeOpts) of
                {ok, Pid} ->
                    MonRef = monitor(process, Pid),
                    receive
                        {process_completed, Pid, StepsState} ->
                            demonitor(MonRef, [flush]),
                            extract_result(StepsState);
                        {process_failed, Pid, Reason} ->
                            demonitor(MonRef, [flush]),
                            {error, Reason};
                        {'DOWN', MonRef, process, Pid, Reason} ->
                            {error, {process_died, Reason}}
                    after Timeout ->
                        demonitor(MonRef, [flush]),
                        beamai_process:stop(Pid),
                        {error, timeout}
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 控制 API
%%====================================================================

%% @doc 恢复暂停的 Process Agent
-spec resume(pid(), term()) -> ok | {error, term()}.
resume(Pid, Data) ->
    beamai_process:resume(Pid, Data).

%% @doc 停止 Process Agent
-spec stop(pid()) -> ok.
stop(Pid) ->
    beamai_process:stop(Pid).

%% @doc 获取 Process Agent 状态
-spec get_status(pid()) -> {ok, map()}.
get_status(Pid) ->
    beamai_process:get_status(Pid).

%%====================================================================
%% 内部函数
%%====================================================================

add_extra_steps(Builder, []) -> Builder;
add_extra_steps(Builder, [{StepId, Module, Config} | Rest]) ->
    B1 = beamai_process:add_step(Builder, StepId, Module, Config),
    add_extra_steps(B1, Rest);
add_extra_steps(Builder, [{StepId, Module} | Rest]) ->
    B1 = beamai_process:add_step(Builder, StepId, Module),
    add_extra_steps(B1, Rest).

add_extra_bindings(Builder, []) -> Builder;
add_extra_bindings(Builder, [Binding | Rest]) ->
    B1 = beamai_process_builder:add_binding(Builder, Binding),
    add_extra_bindings(B1, Rest).

%% @private 从 Process 完成状态中提取 Agent 结果
extract_result(StepsState) ->
    %% 从 llm_step 的 state 中提取结果
    case maps:find(llm_step, StepsState) of
        {ok, #{state := LlmState}} ->
            #{turn_count := TurnCount,
              all_tool_calls := AllToolCalls} = LlmState,
            Response = maps:get(last_response, LlmState, <<>>),
            {ok, #{
                response => Response,
                tool_calls_made => AllToolCalls,
                turn_count => TurnCount
            }};
        error ->
            {error, no_llm_step_state}
    end.
