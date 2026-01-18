%%%-------------------------------------------------------------------
%%% @doc Deep Agent 任务执行节点模块
%%%
%%% 负责执行单个子任务。
%%%
%%% 核心功能：
%%% - 从子任务定义创建执行图
%%% - 执行子任务
%%% - 收集执行结果
%%%
%%% 设计原则：
%%% - 纯函数：无副作用
%%% - 单一职责：只负责任务执行
%%% - 可测试：易于单元测试
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_task_node).

%% 导入公共工具
-import(beamai_deepagent_utils, [
    state_get/2, state_get/3, state_set/3
]).

%% API
-export([make_task_executor/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 创建任务执行器节点
%%
%% 返回一个节点函数，用于执行单个子任务。
-spec make_task_executor(map()) -> fun((graph_state:state()) -> {ok, graph_state:state()}).
make_task_executor(Config) ->
    fun(State) -> execute_task(State, Config) end.

%%====================================================================
%% 节点执行
%%====================================================================

%% @private 执行任务节点
-spec execute_task(graph_state:state(), map()) -> {ok, graph_state:state()}.
execute_task(State, Config) ->
    %% 1. 获取当前子任务
    case state_get(State, current_subtask, undefined) of
        undefined ->
            %% 没有待执行的子任务，直接返回
            {ok, State};
        TaskDef ->
            %% 2. 执行子任务
            {Result, State1} = do_execute_task(TaskDef, Config, State),

            %% 3. 保存结果
            State2 = save_task_result(State1, TaskDef, Result),

            %% 4. 清理当前子任务
            State3 = state_set(State2, current_subtask, undefined),

            {ok, State3}
    end.

%%====================================================================
%% 私有函数
%%====================================================================

%% @private 实际执行子任务
%%
%% 为子任务创建一个临时的图并执行。
-spec do_execute_task(map(), map(), graph_state:state()) -> {map(), graph_state:state()}.
do_execute_task(TaskDef, Config, State) ->
    %% 1. 提取任务定义
    TaskId = maps:get(task_id, TaskDef),
    Message = maps:get(message, TaskDef),

    %% 2. 创建子图
    {ok, SubGraph} = build_subgraph(TaskDef, Config),

    %% 3. 创建子状态（继承当前状态的关键字段）
    SubState = create_substate(State, TaskDef),

    %% 4. 执行子图
    SubOpts = #{
        engine => pregel,
        trace => true
    },

    case graph:run(SubGraph, SubState, SubOpts) of
        #{status := completed, final_state := FinalSubState} ->
            %% 成功：提取结果
            Result = extract_task_result(FinalSubState),
            {#{success => true, task_id => TaskId, result => Result}, merge_substate(State, FinalSubState)};
        #{status := error, error := Reason} ->
            %% 失败：返回错误
            {#{success => false, task_id => TaskId, error => Reason}, State}
    end.

%% @private 构建子图
%%
%% 为子任务创建一个简化的执行图。
-spec build_subgraph(map(), map()) -> {ok, graph:graph()}.
build_subgraph(TaskDef, Config) ->
    %% 子任务使用简化的图：只有 LLM 节点
    Nodes = #{
        llm_node => beamai_deepagent_llm_node:create(Config)
    },

    GraphSpec = [
        {node, llm_node, maps:get(llm_node, Nodes)},
        {entry, llm_node}
    ],

    graph:build(GraphSpec).

%% @private 创建子状态
%%
%% 从父状态创建子状态，继承必要的字段。
-spec create_substate(graph_state:state(), map()) -> graph_state:state().
create_substate(ParentState, TaskDef) ->
    %% 继承关键配置
    BaseState = #{
        system_prompt => state_get(ParentState, system_prompt, <<>>),
        messages => maps:get(messages, TaskDef, []),
        depth => state_get(ParentState, depth, 0) + 1,
        max_depth => state_get(ParentState, max_depth, 3)
    },

    graph_state:new(BaseState).

%% @private 提取任务结果
%%
%% 从子状态中提取执行结果。
-spec extract_task_result(graph_state:state()) -> map().
extract_task_result(SubState) ->
    #{
        response => extract_response(SubState),
        iterations => state_get(SubState, iterations, 0)
    }.

%% @private 提取响应内容
-spec extract_response(graph_state:state()) -> binary().
extract_response(State) ->
    case state_get(State, final_response, undefined) of
        undefined ->
            Messages = state_get(State, messages, []),
            last_assistant_message(Messages);
        Response ->
            Response
    end.

%% @private 获取最后的助手消息
-spec last_assistant_message([map()]) -> binary().
last_assistant_message(Messages) ->
    case lists:reverse(Messages) of
        [#{role := assistant, content := Content} | _] -> Content;
        _ -> <<>>
    end.

%% @private 保存任务结果
%%
%% 将任务结果添加到状态的结果列表中。
-spec save_task_result(graph_state:state(), map(), map()) -> graph_state:state().
save_task_result(State, TaskDef, Result) ->
    %% 保存到 subtask_results
    CurrentResults = state_get(State, subtask_results, []),
    TaskResult = Result#{task_id => maps:get(task_id, TaskDef)},
    state_set(State, subtask_results, [TaskResult | CurrentResults]).

%% @private 合并子状态
%%
%% 将子状态的关键字段合并回父状态。
-spec merge_substate(graph_state:state(), graph_state:state()) -> graph_state:state().
merge_substate(ParentState, SubState) ->
    %% 通常不需要合并，因为子任务是独立的
    %% 如果需要，可以合并 messages、trace 等
    ParentState.
