%%%-------------------------------------------------------------------
%%% @doc 流程步骤激活与执行逻辑
%%%
%%% 管理输入收集、激活条件检查和步骤执行。
%%% 每个步骤维护独立的运行时状态（输入缓冲、内部状态、激活计数）。
%%%
%%% == 运行时状态结构 ==
%%%
%%% step_runtime_state() 包含：
%%% - step_spec: 步骤规格定义（模块、配置、必需输入）
%%% - state: 步骤模块 init/1 返回的内部状态
%%% - collected_inputs: 已收集的输入值 Map
%%% - activation_count: 步骤被激活的累计次数
%%%
%%% == 执行流程 ==
%%%
%%% init_step/1 -> collect_input/4 -> check_activation/2 -> execute/3
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_step).

-export([
    init_step/1,
    collect_input/4,
    check_activation/2,
    execute/3,
    clear_inputs/1,
    invoke_with_kernel/4,
    chat_with_kernel/3
]).

-export_type([step_runtime_state/0]).

-type step_runtime_state() :: #{
    '__step_runtime__' := true,
    step_spec := beamai_process_builder:step_spec(),
    state := term(),
    collected_inputs := #{atom() => term()},
    activation_count := non_neg_integer()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc 从步骤规格初始化运行时状态
%%
%% 调用步骤模块的 init/1 回调，传入配置。
%%
%% @param StepSpec 步骤规格 Map
%% @returns {ok, 运行时状态} | {error, 原因}
-spec init_step(beamai_process_builder:step_spec()) ->
    {ok, step_runtime_state()} | {error, term()}.
init_step(#{module := Module, config := Config} = StepSpec) ->
    case Module:init(Config) of
        {ok, State} ->
            {ok, #{
                '__step_runtime__' => true,
                step_spec => StepSpec,
                state => State,
                collected_inputs => #{},
                activation_count => 0
            }};
        {error, Reason} ->
            {error, {step_init_failed, maps:get(id, StepSpec), Reason}}
    end.

%% @doc 收集步骤的一个输入值
%%
%% 将值存入指定步骤的 collected_inputs 中。
%% 步骤不存在时静默忽略。
%%
%% @param StepId 步骤标识
%% @param InputName 输入名称
%% @param Value 输入值
%% @param StepsState 所有步骤的状态 Map
%% @returns 更新后的步骤状态 Map
-spec collect_input(atom(), atom(), term(), #{atom() => step_runtime_state()}) ->
    #{atom() => step_runtime_state()}.
collect_input(StepId, InputName, Value, StepsState) ->
    case maps:find(StepId, StepsState) of
        {ok, #{collected_inputs := Inputs} = StepState} ->
            StepsState#{StepId => StepState#{
                collected_inputs => Inputs#{InputName => Value}
            }};
        error ->
            StepsState
    end.

%% @doc 检查步骤是否可被激活（所有必需输入就绪）
%%
%% 先检查 required_inputs 是否全部存在于 collected_inputs 中，
%% 再调用步骤模块的 can_activate/2 回调进行自定义判断。
%%
%% @param StepState 步骤运行时状态
%% @param StepSpec 步骤规格（未使用，保持接口一致）
%% @returns 是否可激活
-spec check_activation(step_runtime_state(), beamai_process_builder:step_spec()) -> boolean().
check_activation(#{collected_inputs := Inputs, state := State,
                   step_spec := #{module := Module, required_inputs := Required}}, _StepSpec) ->
    AllPresent = lists:all(
        fun(InputName) -> maps:is_key(InputName, Inputs) end,
        Required
    ),
    case AllPresent of
        true -> Module:can_activate(Inputs, State);
        false -> false
    end.

%% @doc 执行步骤
%%
%% 调用步骤模块的 on_activate/3 回调。
%% 返回值可以是：
%% - {events, 事件列表, 更新后状态}：正常完成，产生后续事件
%% - {pause, 原因, 更新后状态}：请求暂停流程
%% - {error, 原因}：执行失败
%%
%% @param StepRuntimeState 步骤运行时状态
%% @param Inputs 已收集的输入 Map
%% @param Context 执行上下文
%% @returns 执行结果
-spec execute(step_runtime_state(), #{atom() => term()}, beamai_context:t()) ->
    {events, [beamai_process_event:event()], step_runtime_state()} |
    {pause, term(), step_runtime_state()} |
    {error, term()}.
execute(#{step_spec := #{module := Module, id := StepId},
          state := State, activation_count := Count} = StepRuntimeState,
        Inputs, Context) ->
    try Module:on_activate(Inputs, State, Context) of
        {ok, #{events := Events, state := NewState}} ->
            TaggedEvents = tag_events(Events, StepId),
            {events, TaggedEvents, StepRuntimeState#{
                state => NewState,
                activation_count => Count + 1
            }};
        {ok, #{state := NewState}} ->
            {events, [], StepRuntimeState#{
                state => NewState,
                activation_count => Count + 1
            }};
        {ok, #{events := Events}} ->
            TaggedEvents = tag_events(Events, StepId),
            {events, TaggedEvents, StepRuntimeState#{
                activation_count => Count + 1
            }};
        {error, Reason} ->
            {error, {step_execution_failed, StepId, Reason}};
        {pause, Reason, NewState} ->
            {pause, Reason, StepRuntimeState#{
                state => NewState,
                activation_count => Count + 1
            }}
    catch
        Class:Error:Stacktrace ->
            {error, {step_exception, StepId, {Class, Error, Stacktrace}}}
    end.

%% @doc 清除已收集的输入（激活后重置，支持循环执行）
-spec clear_inputs(step_runtime_state()) -> step_runtime_state().
clear_inputs(StepRuntimeState) ->
    StepRuntimeState#{collected_inputs => #{}}.

%% @doc 辅助函数：在步骤内通过 Kernel 调用工具
%%
%% @param Context 当前上下文（需包含 kernel 引用）
%% @param ToolName 工具函数名称
%% @param Args 调用参数
%% @param Opts 选项（未使用）
%% @returns {ok, 结果, 更新后上下文} | {error, 原因}
-spec invoke_with_kernel(beamai_context:t(), binary(), map(), map()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.
invoke_with_kernel(Context, ToolName, Args, _Opts) ->
    case beamai_context:get_kernel(Context) of
        undefined ->
            {error, no_kernel_in_context};
        Kernel ->
            beamai_kernel:invoke_tool(Kernel, ToolName, Args, Context)
    end.

%% @doc 辅助函数：在步骤内通过 Kernel 与 LLM 对话
%%
%% @param Context 当前上下文（需包含 kernel 引用）
%% @param Messages 消息列表
%% @param Opts Chat 选项
%% @returns {ok, 响应, 更新后上下文} | {error, 原因}
-spec chat_with_kernel(beamai_context:t(), [beamai_context:message()], map()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.
chat_with_kernel(Context, Messages, Opts) ->
    case beamai_context:get_kernel(Context) of
        undefined ->
            {error, no_kernel_in_context};
        Kernel ->
            beamai_kernel:invoke(Kernel, Messages, Opts)
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 为无来源的事件标记来源步骤 ID
tag_events(Events, StepId) ->
    lists:map(
        fun(#{source := undefined} = E) -> E#{source => StepId};
           (E) -> E
        end,
        Events
    ).
