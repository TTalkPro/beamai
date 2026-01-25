%%%-------------------------------------------------------------------
%%% @doc 流程步骤行为定义模块
%%%
%%% 定义流程框架中步骤模块必须实现的回调接口。
%%% 步骤（Step）是流程工作流的基本构建块。
%%%
%%% == 设计说明 ==
%%%
%%% 每个步骤是一个独立的处理单元，具有以下特性：
%%% - 可配置：通过 init/1 接收配置参数
%%% - 可激活：通过 can_activate/2 判断是否满足激活条件
%%% - 可执行：通过 on_activate/3 执行业务逻辑
%%% - 可暂停：执行过程中可请求暂停，等待外部恢复
%%%
%%% == 回调函数 ==
%%%
%%% 必选回调：
%%% - init/1: 初始化步骤状态
%%% - can_activate/2: 检查激活条件
%%% - on_activate/3: 执行步骤逻辑
%%%
%%% 可选回调：
%%% - on_resume/3: 从暂停状态恢复时调用
%%% - on_terminate/2: 步骤终止时的清理回调
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% -module(my_step).
%%% -behaviour(beamai_step_behaviour).
%%%
%%% -export([init/1, can_activate/2, on_activate/3]).
%%%
%%% init(Config) ->
%%%     {ok, #{config => Config, count => 0}}.
%%%
%%% can_activate(#{input := _}, _State) -> true;
%%% can_activate(_, _) -> false.
%%%
%%% on_activate(#{input := Data}, State, Context) ->
%%%     Result = process(Data),
%%%     Events = [beamai_process_event:new(output, Result)],
%%%     {ok, #{events => Events, state => State#{count => 1}}}.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_step_behaviour).

%%====================================================================
%% 类型定义
%%====================================================================

%% @type config() :: map().
%% 步骤配置 Map，包含初始化步骤所需的参数。
%% 具体字段由各步骤实现自行定义。
-type config() :: map().

%% @type state() :: term().
%% 步骤内部状态，可以是任意类型。
%% 通常使用 Map 存储多个状态字段。
-type state() :: term().

%% @type inputs() :: #{atom() => term()}.
%% 步骤输入 Map，键为输入端口名称（atom），值为输入数据。
%% 当所有必需输入端口都有数据时，步骤可被激活。
-type inputs() :: #{atom() => term()}.

%% @type context() :: beamai_context:t().
%% 流程执行上下文，包含共享数据和配置。
%% 在整个流程执行过程中传递。
-type context() :: beamai_context:t().

%% @type event() :: beamai_process_event:event().
%% 流程事件，用于在步骤间传递数据。
%% 由步骤执行产生，通过 bindings 路由到其他步骤。
-type event() :: beamai_process_event:event().

%% @type activate_result().
%% 步骤激活执行的返回结果：
%% - {ok, #{events, state}}: 执行成功，返回产生的事件和新状态
%% - {error, Reason}: 执行失败
%% - {pause, Reason, State}: 请求暂停，等待外部恢复
-type activate_result() ::
    {ok, #{events => [event()], state => state()}} |
    {error, term()} |
    {pause, term(), state()}.

%% @type resume_result().
%% 从暂停状态恢复的返回结果：
%% - {ok, #{events, state}}: 恢复成功，可选返回事件
%% - {error, Reason}: 恢复失败
-type resume_result() ::
    {ok, #{events => [event()], state => state()}} |
    {error, term()}.

-export_type([config/0, state/0, inputs/0, activate_result/0, resume_result/0]).

%%====================================================================
%% 必选回调定义
%%====================================================================

%% @doc 初始化步骤状态
%%
%% 在流程启动时调用，用于初始化步骤的内部状态。
%% 可以进行资源分配、配置验证等操作。
%%
%% @param Config 步骤配置参数
%% @returns {ok, State} 初始状态 | {error, Reason} 初始化失败
-callback init(Config :: config()) -> {ok, state()} | {error, term()}.

%% @doc 检查步骤是否可以被激活
%%
%% 根据当前输入和状态判断步骤是否满足激活条件。
%% 通常检查所有必需的输入端口是否已有数据。
%%
%% @param Inputs 当前收集到的输入数据
%% @param State 当前步骤状态
%% @returns true 可以激活 | false 条件不满足
-callback can_activate(Inputs :: inputs(), State :: state()) -> boolean().

%% @doc 激活执行步骤逻辑
%%
%% 当 can_activate/2 返回 true 时被调用。
%% 执行步骤的核心业务逻辑，产生输出事件。
%%
%% @param Inputs 输入数据
%% @param State 当前状态
%% @param Context 执行上下文
%% @returns activate_result()
-callback on_activate(Inputs :: inputs(), State :: state(), Context :: context()) ->
    activate_result().

%%====================================================================
%% 可选回调定义
%%====================================================================

%% @doc 从暂停状态恢复
%%
%% 当步骤处于暂停状态并收到外部恢复信号时调用。
%% 用于处理人机交互、异步等待等场景。
%%
%% @param Data 恢复时传入的数据
%% @param State 暂停前的状态
%% @param Context 执行上下文
%% @returns resume_result()
-callback on_resume(Data :: term(), State :: state(), Context :: context()) ->
    resume_result().

%% @doc 步骤终止清理
%%
%% 在流程终止时调用，用于释放资源、清理状态。
%%
%% @param Reason 终止原因
%% @param State 当前状态
%% @returns ok
-callback on_terminate(Reason :: term(), State :: state()) -> ok.

-optional_callbacks([on_resume/3, on_terminate/2]).
