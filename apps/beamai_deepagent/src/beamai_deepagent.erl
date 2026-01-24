%%%-------------------------------------------------------------------
%%% @doc DeepAgent - Sub-agent Orchestrator
%%%
%%% 基于 beamai_agent 子代理的深度执行引擎。
%%% 通过协调器编排 Planner/Executor/Reflector 子代理实现复杂任务分解与并行执行。
%%%
%%% 核心特性：
%%% - 子代理驱动：用 beamai_agent 实例作为 planner/executor/reflector
%%% - 并行执行：依赖分析后按层并行调度
%%% - 可选规划：支持直接执行或规划后执行
%%% - 反思分析：可选的执行进度反思
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent).

%%====================================================================
%% API Exports
%%====================================================================

-export([new/0, new/1]).
-export([run/2, run/3]).
-export([get_plan/1, get_trace/1]).

%%====================================================================
%% 类型定义
%%====================================================================

-type config() :: #{
    llm := beamai_chat_completion:config(),
    plugins => [module()],
    custom_tools => [map()],
    max_depth => pos_integer(),
    max_parallel => pos_integer(),
    planning_enabled => boolean(),
    reflection_enabled => boolean(),
    system_prompt => binary(),
    planner_prompt => binary(),
    executor_prompt => binary(),
    reflector_prompt => binary(),
    max_tool_iterations => pos_integer(),
    timeout => pos_integer(),
    callbacks => callbacks()
}.

-type callbacks() :: #{
    on_plan_created => fun((beamai_deepagent_plan:t()) -> ok),
    on_step_started => fun((map()) -> ok),
    on_step_completed => fun((beamai_deepagent_executor:step_result()) -> ok),
    on_layer_completed => fun((pos_integer(), [beamai_deepagent_executor:step_result()]) -> ok),
    on_reflection => fun((binary()) -> ok)
}.

-type run_result() :: #{
    status := completed | error | timeout,
    response := binary(),
    plan => beamai_deepagent_plan:t(),
    trace := beamai_deepagent_trace:t(),
    step_results := [beamai_deepagent_executor:step_result()]
}.

-export_type([config/0, run_result/0, callbacks/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 创建默认配置的 DeepAgent
%%
%% 使用空配置调用 new/1，需要后续设置 llm 才能运行。
-spec new() -> config().
new() ->
    new(#{}).

%% @doc 创建 DeepAgent 配置
%%
%% 将用户选项与默认值合并，生成完整配置。
%% 运行前必须包含 llm 字段。
%%
%% 支持的选项：
%%   llm - LLM 配置（必填）
%%   plugins - 执行器使用的工具插件列表
%%   max_depth - 最大递归深度（默认 3）
%%   max_parallel - 最大并行数（默认 5）
%%   planning_enabled - 是否启用规划（默认 true）
%%   reflection_enabled - 是否启用反思（默认 true）
%%   max_tool_iterations - 每个子代理最大工具迭代次数（默认 10）
%%   timeout - 每步超时毫秒数（默认 300000）
%%   callbacks - 事件回调
-spec new(map()) -> config().
new(Opts) ->
    Defaults = #{
        max_depth => 3,
        max_parallel => 5,
        planning_enabled => true,
        reflection_enabled => true,
        max_tool_iterations => 10,
        timeout => 300000,
        callbacks => #{}
    },
    maps:merge(Defaults, Opts).

%% @doc 运行 DeepAgent（默认选项）
-spec run(config(), binary()) -> {ok, run_result()} | {error, term()}.
run(Config, Task) ->
    run(Config, Task, #{}).

%% @doc 运行 DeepAgent
%%
%% 验证配置后通过协调器编排子代理执行任务。
%% 支持 on_start 和 on_complete 回调通知。
%%
%% Config: 深度代理配置（必须包含 llm）
%% Task: 用户任务描述
%% Opts: 额外选项（传给协调器）
-spec run(config(), binary(), map()) -> {ok, run_result()} | {error, term()}.
run(Config, Task, Opts) ->
    case validate_config(Config) of
        ok ->
            invoke_callback(on_start, [Task], Config),
            Result = beamai_deepagent_coordinator:execute(Config, Task, Opts),
            invoke_callback(on_complete, [Result], Config),
            Result;
        {error, _} = Err ->
            Err
    end.

%% @doc 从运行结果中获取计划
-spec get_plan(run_result()) -> beamai_deepagent_plan:t() | undefined.
get_plan(#{plan := Plan}) -> Plan;
get_plan(_) -> undefined.

%% @doc 从运行结果中获取执行轨迹
-spec get_trace(run_result()) -> beamai_deepagent_trace:t() | undefined.
get_trace(#{trace := Trace}) -> Trace;
get_trace(_) -> undefined.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 验证配置中必须包含 llm 字段
-spec validate_config(map()) -> ok | {error, missing_llm_config}.
validate_config(#{llm := _}) -> ok;
validate_config(_) -> {error, missing_llm_config}.

%% @doc 安全调用事件回调
%%
%% 从 callbacks map 中查找对应事件的回调函数并执行。
%% 回调异常被静默吞掉，不影响主流程。
-spec invoke_callback(atom(), [term()], map()) -> ok.
invoke_callback(Event, Args, #{callbacks := Callbacks}) ->
    case maps:find(Event, Callbacks) of
        {ok, Fun} when is_function(Fun) ->
            try erlang:apply(Fun, Args)
            catch _:_ -> ok
            end;
        _ -> ok
    end;
invoke_callback(_Event, _Args, _Config) ->
    ok.
