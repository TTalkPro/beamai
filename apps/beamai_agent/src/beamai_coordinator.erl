%%%-------------------------------------------------------------------
%%% @doc 协调器功能模块
%%%
%%% 提供便捷的 API 来创建多 Agent 协调器。
%%% 基于 beamai_agent 实现，支持 Pipeline 和 Orchestrator 两种协调模式。
%%%
%%% == 协调模式 ==
%%%
%%% === Pipeline 模式（顺序协调）===
%%% 任务在 workers 间依次传递，每个 worker 的输出作为下一个的输入。
%%% 适合：内容生产流程、代码审查流程、数据分析流程。
%%%
%%% === Orchestrator 模式（编排协调）===
%%% 协调器可以委托、路由、并行调用多个 workers，并综合结果。
%%% 适合：复杂任务分解、多角度分析、专家咨询。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_coordinator).

%% API
-export([start_link/2]).
-export([start_pipeline/2, start_orchestrator/2]).
-export([stop/1]).

%% 状态导出/导入 API
-export([export_coordinator/1, import_coordinator/2]).
-export([get_workers/1, get_worker/2]).

%% 便捷函数
-export([delegate/3, delegate_parallel/3]).

-include_lib("beamai_core/include/beamai_common.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc 启动协调器（通用接口）
%%
%% 根据配置自动选择 Pipeline 或 Orchestrator 模式。
%%
%% @param Id 协调器 ID
%% @param Opts 配置选项
%%   - type: pipeline | orchestrator（默认 pipeline）
%%   - 其他选项传递给 start_pipeline 或 start_orchestrator
%% @returns {ok, Pid} 或 {error, Reason}
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Id, Opts) ->
    Type = maps:get(type, Opts, pipeline),
    case Type of
        pipeline -> start_pipeline(Id, Opts);
        orchestrator -> start_orchestrator(Id, Opts);
        _ -> {error, {unknown_type, Type}}
    end.

%% @doc 启动 Pipeline 模式协调器
%%
%% 创建一个顺序协调的 Agent，任务在 workers 间依次传递。
%% 适合场景：内容生产流程、代码审查流程、数据分析流程。
%%
%% @param Id 协调器 ID
%% @param Opts 配置选项
%%   - agents: Agent 定义列表
%%   - llm: LLM 配置
%%   - system_prompt: 可选，系统提示词
%%   - max_iterations: 可选，最大迭代次数
%% @returns {ok, Pid} 或 {error, Reason}
-spec start_pipeline(binary(), map()) -> {ok, pid()} | {error, term()}.
start_pipeline(Id, Opts) ->
    Agents = maps:get(agents, Opts, []),
    LLMConfig = maps:get(llm, Opts, #{}),
    SystemPrompt = maps:get(system_prompt, Opts, beamai_coordinator_common:build_pipeline_prompt()),

    %% 启动 workers（使用公共模块）
    {_Count, Pids} = beamai_coordinator_common:start_workers(Id, Agents, LLMConfig),

    %% 构建委托工具（使用公共模块）
    Tools = beamai_coordinator_common:build_delegate_tools(Agents, Pids),

    %% 启动协调器 beamai_agent
    AgentOpts = #{
        system_prompt => SystemPrompt,
        tools => Tools,
        llm => LLMConfig,
        max_iterations => maps:get(max_iterations, Opts, 10)
    },

    case beamai_agent:start_link(Id, AgentOpts) of
        {ok, Pid} ->
            %% 将协调器元数据存储到 agent 的 meta 字段中
            beamai_agent:set_meta(Pid, #{
                coordinator_type => pipeline,
                agents => Agents,
                workers => Pids,
                llm_config => LLMConfig,
                created_at => erlang:system_time(millisecond)
            }),
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc 启动 Orchestrator 模式协调器
%%
%% 创建一个编排器 Agent，可以委托、路由、并行调用多个 workers。
%% 适合场景：复杂任务分解、多角度分析、专家咨询。
%%
%% @param Id 协调器 ID
%% @param Opts 配置选项
%%   - agents: Agent 定义列表
%%   - llm: LLM 配置
%%   - system_prompt: 可选，系统提示词
%%   - max_iterations: 可选，最大迭代次数
%% @returns {ok, Pid} 或 {error, Reason}
-spec start_orchestrator(binary(), map()) -> {ok, pid()} | {error, term()}.
start_orchestrator(Id, Opts) ->
    Agents = maps:get(agents, Opts, []),
    LLMConfig = maps:get(llm, Opts, #{}),
    SystemPrompt = maps:get(system_prompt, Opts, beamai_coordinator_common:build_orchestrator_prompt()),

    %% 启动 workers（使用公共模块）
    {_Count, Pids} = beamai_coordinator_common:start_workers(Id, Agents, LLMConfig),

    %% 构建工具（委托 + 路由 + 并行 + 综合）
    DelegateTools = beamai_coordinator_common:build_delegate_tools(Agents, Pids),
    RouterTool = beamai_coordinator_common:build_router_tool(Agents, Pids),
    ParallelTool = beamai_coordinator_common:build_parallel_tool(Agents, Pids),
    SynthesizeTool = beamai_coordinator_common:build_synthesize_tool(),

    Tools = DelegateTools ++ [RouterTool, ParallelTool, SynthesizeTool],

    %% 启动协调器 beamai_agent
    AgentOpts = #{
        system_prompt => SystemPrompt,
        tools => Tools,
        llm => LLMConfig,
        max_iterations => maps:get(max_iterations, Opts, 10)
    },

    case beamai_agent:start_link(Id, AgentOpts) of
        {ok, Pid} ->
            %% 将协调器元数据存储到 agent 的 meta 字段中
            beamai_agent:set_meta(Pid, #{
                coordinator_type => orchestrator,
                agents => Agents,
                workers => Pids,
                llm_config => LLMConfig,
                created_at => erlang:system_time(millisecond)
            }),
            {ok, Pid};
        Error ->
            Error
    end.

%%====================================================================
%% 状态导出/导入
%%====================================================================

%% @doc 导出协调器完整状态
%%
%% 导出协调器及其所有 workers 的状态，用于持久化或迁移。
%%
%% @param CoordinatorPid 协调器进程 PID
%% @returns {ok, ExportedData} 或 {error, Reason}
%%
%% 导出数据格式：
%% #{
%%   type => pipeline | orchestrator,
%%   agents => [...],              %% Agent 定义
%%   llm_config => #{...},         %% LLM 配置
%%   coordinator_state => #{...},  %% 协调器 agent 状态
%%   workers_states => #{          %% 所有 worker 状态
%%     WorkerName => #{...}
%%   }
%% }
-spec export_coordinator(pid()) -> {ok, map()} | {error, term()}.
export_coordinator(CoordinatorPid) ->
    Meta = beamai_agent:get_meta(CoordinatorPid),
    case maps:get(coordinator_type, Meta, undefined) of
        undefined ->
            {error, not_a_coordinator};
        Type ->
            Agents = maps:get(agents, Meta, []),
            Workers = maps:get(workers, Meta, #{}),
            LLMConfig = maps:get(llm_config, Meta, #{}),

            %% 导出协调器状态
            CoordinatorState = beamai_agent:export_state(CoordinatorPid),

            %% 导出所有 workers 状态
            WorkersStates = maps:map(fun(_Name, WorkerPid) ->
                beamai_agent:export_state(WorkerPid)
            end, Workers),

            ExportData = #{
                type => Type,
                agents => Agents,
                llm_config => LLMConfig,
                coordinator_state => CoordinatorState,
                workers_states => WorkersStates,
                exported_at => erlang:system_time(millisecond)
            },
            {ok, ExportData}
    end.

%% @doc 导入协调器状态
%%
%% 从导出的数据恢复协调器及其所有 workers。
%%
%% @param Id 新的协调器 ID（可以与原 ID 不同）
%% @param ExportedData 通过 export_coordinator 导出的数据
%% @returns {ok, Pid} 或 {error, Reason}
-spec import_coordinator(binary(), map()) -> {ok, pid()} | {error, term()}.
import_coordinator(Id, ExportedData) ->
    #{
        type := Type,
        agents := Agents,
        llm_config := LLMConfig,
        coordinator_state := CoordinatorState,
        workers_states := WorkersStates
    } = ExportedData,

    %% 根据类型启动协调器
    StartFun = case Type of
        pipeline -> fun start_pipeline/2;
        orchestrator -> fun start_orchestrator/2
    end,

    case StartFun(Id, #{agents => Agents, llm => LLMConfig}) of
        {ok, Pid} ->
            %% 恢复协调器状态
            beamai_agent:import_state(Pid, CoordinatorState),

            %% 恢复所有 workers 状态
            Workers = beamai_agent:get_meta(Pid, workers, #{}),
            maps:foreach(fun(Name, WorkerState) ->
                case maps:find(Name, Workers) of
                    {ok, WorkerPid} ->
                        beamai_agent:import_state(WorkerPid, WorkerState);
                    error ->
                        ok  %% Worker 不存在，跳过
                end
            end, WorkersStates),
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc 获取协调器的所有 workers
%%
%% @param CoordinatorPid 协调器进程 PID
%% @returns {ok, Workers} 或 {error, Reason}
%%   Workers 是 #{WorkerName => WorkerPid} 的映射
-spec get_workers(pid()) -> {ok, map()} | {error, term()}.
get_workers(CoordinatorPid) ->
    Meta = beamai_agent:get_meta(CoordinatorPid),
    case maps:get(coordinator_type, Meta, undefined) of
        undefined -> {error, not_a_coordinator};
        _ -> {ok, maps:get(workers, Meta, #{})}
    end.

%% @doc 获取协调器的指定 worker
%%
%% @param CoordinatorPid 协调器进程 PID
%% @param WorkerName Worker 名称
%% @returns {ok, WorkerPid} 或 {error, Reason}
-spec get_worker(pid(), binary()) -> {ok, pid()} | {error, term()}.
get_worker(CoordinatorPid, WorkerName) ->
    case get_workers(CoordinatorPid) of
        {ok, Workers} ->
            case maps:find(WorkerName, Workers) of
                {ok, WorkerPid} -> {ok, WorkerPid};
                error -> {error, worker_not_found}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 停止协调器及其所有 workers
%%
%% @param CoordinatorPid 协调器进程 PID
%% @returns ok 或 {error, Reason}
-spec stop(pid()) -> ok | {error, term()}.
stop(CoordinatorPid) ->
    Meta = beamai_agent:get_meta(CoordinatorPid),
    case maps:get(coordinator_type, Meta, undefined) of
        undefined ->
            {error, not_a_coordinator};
        _ ->
            Workers = maps:get(workers, Meta, #{}),
            %% 停止所有 workers
            maps:foreach(fun(_Name, WorkerPid) ->
                catch beamai_agent:stop(WorkerPid)
            end, Workers),

            %% 停止协调器
            catch beamai_agent:stop(CoordinatorPid),
            ok
    end.

%%====================================================================
%% 便捷函数
%%====================================================================

%% @doc 委托任务给指定 Worker
%%
%% 直接调用某个 worker 执行任务。
%%
%% @param CoordinatorPid 协调器进程 PID
%% @param WorkerName Worker 名称
%% @param Task 任务描述
%% @returns {ok, Result} 或 {error, Reason}
-spec delegate(pid(), binary(), binary()) -> {ok, binary()} | {error, term()}.
delegate(CoordinatorPid, WorkerName, Task) ->
    case get_worker(CoordinatorPid, WorkerName) of
        {ok, WorkerPid} ->
            case beamai_agent:run(WorkerPid, Task, #{timeout => 120000}) of
                {ok, Result} ->
                    {ok, maps:get(final_response, Result, <<>>)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 并行委托任务
%%
%% 同时调用多个 workers 执行相同任务。
%%
%% @param CoordinatorPid 协调器进程 PID
%% @param WorkerNames Worker 名称列表
%% @param Task 任务描述
%% @returns {ok, Results} 或 {error, Reason}
%%   Results 是 #{WorkerName => Result} 的映射
-spec delegate_parallel(pid(), [binary()], binary()) -> {ok, map()} | {error, term()}.
delegate_parallel(CoordinatorPid, WorkerNames, Task) ->
    case get_workers(CoordinatorPid) of
        {ok, AllWorkers} ->
            Results = lists:foldl(fun(Name, Acc) ->
                case maps:find(Name, AllWorkers) of
                    {ok, WorkerPid} ->
                        Result = case beamai_agent:run(WorkerPid, Task, #{timeout => 120000}) of
                            {ok, R} -> {ok, maps:get(final_response, R, <<>>)};
                            {error, Reason} -> {error, Reason}
                        end,
                        Acc#{Name => Result};
                    error ->
                        Acc#{Name => {error, not_found}}
                end
            end, #{}, WorkerNames),
            {ok, Results};
        {error, Reason} ->
            {error, Reason}
    end.
