%%%-------------------------------------------------------------------
%%% @doc Agent Middleware 行为定义
%%%
%%% 定义 Middleware 的标准接口，支持在 Agent 执行的各个阶段
%%% 进行拦截、修改和控制。
%%%
%%% == 生命周期钩子 ==
%%%
%%% ```
%%%                    ┌─────────────────┐
%%%                    │  before_agent   │
%%%                    └────────┬────────┘
%%%                             │
%%%            ┌────────────────┼────────────────┐
%%%            │                ▼                │
%%%            │    ┌─────────────────────┐      │
%%%            │    │    before_model     │      │
%%%            │    └──────────┬──────────┘      │
%%%            │               │                 │
%%%            │               ▼                 │
%%%            │    ┌─────────────────────┐      │
%%%            │    │     LLM Call        │      │
%%%            │    └──────────┬──────────┘      │
%%%            │               │                 │
%%%            │               ▼                 │
%%%            │    ┌─────────────────────┐      │
%%%            │    │     after_model     │      │
%%%            │    └──────────┬──────────┘      │
%%%  Agent     │               │                 │
%%%  Loop      │               ▼                 │
%%%            │    ┌─────────────────────┐      │
%%%            │    │    before_tools     │      │
%%%            │    └──────────┬──────────┘      │
%%%            │               │                 │
%%%            │               ▼                 │
%%%            │    ┌─────────────────────┐      │
%%%            │    │    Tool Execution   │      │
%%%            │    └──────────┬──────────┘      │
%%%            │               │                 │
%%%            │               ▼                 │
%%%            │    ┌─────────────────────┐      │
%%%            │    │     after_tools     │      │
%%%            │    └──────────┬──────────┘      │
%%%            │               │                 │
%%%            └───────────────┴─────────────────┘
%%%                            │
%%%                            ▼
%%%                    ┌─────────────────┐
%%%                    │   after_agent   │
%%%                    └─────────────────┘
%%% ```
%%%
%%% == Middleware 返回值 ==
%%%
%%% - `ok` - 无修改，继续执行
%%% - `{update, StateUpdates}` - 更新图状态
%%% - `{goto, Node}` - 跳转到指定节点 (model | tools | '__end__')
%%% - `{update_goto, StateUpdates, Node}` - 更新状态并跳转
%%% - `{halt, Reason}` - 中止执行并返回错误
%%% - `{interrupt, Action}` - 中断等待用户确认
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% -module(my_middleware).
%%% -behaviour(beamai_middleware).
%%%
%%% -export([init/1, before_model/2]).
%%%
%%% init(Opts) ->
%%%     #{max_calls => maps:get(max_calls, Opts, 10)}.
%%%
%%% before_model(State, #{max_calls := MaxCalls} = _MwState) ->
%%%     Count = graph:get(State, model_call_count, 0),
%%%     case Count >= MaxCalls of
%%%         true -> {halt, model_call_limit_exceeded};
%%%         false -> {update, #{model_call_count => Count + 1}}
%%%     end.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_middleware).

%% 类型导出
-export_type([
    middleware/0,
    middleware_result/0,
    middleware_state/0,
    hook_name/0
]).

%%====================================================================
%% 类型定义
%%====================================================================

%% Middleware 实例
-type middleware() :: #{
    module := module(),           %% Middleware 模块
    state := middleware_state(),  %% Middleware 内部状态
    priority => integer()         %% 优先级 (越小越先执行，默认 100)
}.

%% Middleware 内部状态
-type middleware_state() :: map().

%% 钩子名称
-type hook_name() :: before_agent | after_agent |
                     before_model | after_model |
                     before_tools | after_tools.

%% Middleware 返回结果
-type middleware_result() ::
    ok |                                      %% 无修改
    {update, map()} |                         %% 更新图状态
    {goto, goto_target()} |                   %% 跳转到指定节点
    {update_goto, map(), goto_target()} |     %% 更新并跳转
    {halt, term()} |                          %% 中止执行
    {interrupt, interrupt_action()}.          %% 中断等待确认

%% 跳转目标
-type goto_target() :: model | tools | '__end__'.

%% 中断动作
-type interrupt_action() :: #{
    type := tool_approval | custom,
    data => term(),
    timeout => pos_integer()
}.

%%====================================================================
%% 行为回调定义
%%====================================================================

%% @doc 初始化 Middleware
%%
%% 在 Agent 创建时调用，返回 Middleware 内部状态。
%% Opts 来自用户配置。
-callback init(Opts :: map()) -> middleware_state().

%% @doc Agent 执行开始前
%%
%% 在整个 Agent 执行开始前调用。
%% State: 图状态
%% MwState: Middleware 内部状态
-callback before_agent(State :: map(), MwState :: middleware_state()) ->
    middleware_result().

%% @doc Agent 执行结束后
%%
%% 在整个 Agent 执行结束后调用（无论成功或失败）。
-callback after_agent(State :: map(), MwState :: middleware_state()) ->
    middleware_result().

%% @doc 模型调用前
%%
%% 每次 LLM 调用前执行。可用于：
%% - 修改 messages
%% - 检查调用限制
%% - 添加上下文
-callback before_model(State :: map(), MwState :: middleware_state()) ->
    middleware_result().

%% @doc 模型调用后
%%
%% LLM 返回后执行。可用于：
%% - 修改响应
%% - 记录日志
%% - 触发后续动作
-callback after_model(State :: map(), MwState :: middleware_state()) ->
    middleware_result().

%% @doc 工具执行前
%%
%% 工具执行前调用。可用于：
%% - 人工审批
%% - 参数验证
%% - 工具过滤
-callback before_tools(State :: map(), MwState :: middleware_state()) ->
    middleware_result().

%% @doc 工具执行后
%%
%% 工具执行后调用。可用于：
%% - 结果验证
%% - 失败重试
%% - 结果转换
-callback after_tools(State :: map(), MwState :: middleware_state()) ->
    middleware_result().

%% 所有回调都是可选的
-optional_callbacks([
    init/1,
    before_agent/2, after_agent/2,
    before_model/2, after_model/2,
    before_tools/2, after_tools/2
]).
