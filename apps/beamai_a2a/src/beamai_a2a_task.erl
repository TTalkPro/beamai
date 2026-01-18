%%%-------------------------------------------------------------------
%%% @doc A2A Task 管理模块
%%%
%%% 管理 A2A 任务的生命周期，包括创建、状态转换、产出物管理。
%%% 每个 Task 是一个 gen_server 进程，维护自己的状态。
%%%
%%% == Task 生命周期 ==
%%%
%%% ```
%%%                    ┌──────────────┐
%%%                    │  submitted   │  任务已提交
%%%                    └──────┬───────┘
%%%                           │
%%%                           ▼
%%%                    ┌──────────────┐
%%%             ┌──────│   working    │──────┐
%%%             │      └──────┬───────┘      │
%%%             │             │              │
%%%             ▼             ▼              ▼
%%%    ┌────────────────┐  ┌──────────┐  ┌────────────┐
%%%    │ input-required │  │completed │  │   failed   │
%%%    └────────┬───────┘  └──────────┘  └────────────┘
%%%             │                               │
%%%             │ (用户提供输入)                   │
%%%             └────────────────┬──────────────┘
%%%                              ▼
%%%                    ┌──────────────┐
%%%                    │   canceled   │  (可随时取消)
%%%                    └──────────────┘
%%% ```
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 创建任务
%%% {ok, TaskPid} = beamai_a2a_task:start_link(#{
%%%     message => Message,
%%%     context_id => ContextId
%%% }).
%%%
%%% %% 获取任务状态
%%% {ok, Task} = beamai_a2a_task:get(TaskPid).
%%%
%%% %% 更新状态
%%% ok = beamai_a2a_task:update_status(TaskPid, working).
%%% ok = beamai_a2a_task:update_status(TaskPid, {completed, Result}).
%%%
%%% %% 添加产出物
%%% ok = beamai_a2a_task:add_artifact(TaskPid, Artifact).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_task).

-behaviour(gen_server).

%% 避免与 erlang:get/1 冲突
-compile({no_auto_import,[get/1]}).

%% API 导出
-export([
    start_link/1,
    start/1,
    create/1,
    get/1,
    get_id/1,
    update_status/2,
    add_message/2,
    add_artifact/2,
    cancel/1,
    stop/1
]).

%% gen_server 回调
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% 包含类型定义
-include_lib("beamai_core/include/beamai_common.hrl").

%% Task 状态记录
-record(task_state, {
    id :: binary(),
    context_id :: binary() | undefined,
    status :: beamai_a2a_types:task_state(),
    status_message :: map() | undefined,
    history :: [map()],
    artifacts :: [map()],
    messages :: [map()],
    metadata :: map(),
    agent_ref :: pid() | undefined,  %% 关联的 Agent 进程
    created_at :: non_neg_integer(),
    updated_at :: non_neg_integer()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Task 进程（带链接）
%%
%% @param Opts 配置选项
%%   - message: 初始消息（可选）
%%   - context_id: 上下文 ID（可选）
%%   - task_id: 任务 ID（可选，默认自动生成）
%%   - metadata: 元数据（可选）
%% @returns {ok, Pid} | {error, Reason}
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc 启动 Task 进程（不带链接）
-spec start(map()) -> {ok, pid()} | {error, term()}.
start(Opts) ->
    gen_server:start(?MODULE, Opts, []).

%% @doc 创建任务（便捷函数）
%%
%% 创建任务并返回任务数据（非进程）。
%% 用于需要立即获取任务数据的场景。
-spec create(map()) -> {ok, map()}.
create(Opts) ->
    {ok, Pid} = start(Opts),
    get(Pid).

%% @doc 获取任务当前状态
%%
%% @param TaskRef Task 进程引用（pid 或注册名）
%% @returns {ok, TaskMap} | {error, Reason}
-spec get(pid() | atom()) -> {ok, map()} | {error, term()}.
get(TaskRef) ->
    try
        gen_server:call(TaskRef, get_task, ?DEFAULT_TIMEOUT)
    catch
        exit:{noproc, _} -> {error, task_not_found};
        exit:{timeout, _} -> {error, timeout}
    end.

%% @doc 获取任务 ID
-spec get_id(pid() | atom()) -> {ok, binary()} | {error, term()}.
get_id(TaskRef) ->
    try
        gen_server:call(TaskRef, get_id, ?DEFAULT_TIMEOUT)
    catch
        exit:{noproc, _} -> {error, task_not_found};
        exit:{timeout, _} -> {error, timeout}
    end.

%% @doc 更新任务状态
%%
%% @param TaskRef Task 进程引用
%% @param NewStatus 新状态，可以是：
%%   - atom: working, completed, failed, canceled, input_required, auth_required
%%   - {State, Message}: 带消息的状态
%%   - {State, Message, Artifacts}: 带消息和产出物的状态
%% @returns ok | {error, Reason}
-spec update_status(pid() | atom(), term()) -> ok | {error, term()}.
update_status(TaskRef, NewStatus) ->
    try
        gen_server:call(TaskRef, {update_status, NewStatus}, ?DEFAULT_TIMEOUT)
    catch
        exit:{noproc, _} -> {error, task_not_found};
        exit:{timeout, _} -> {error, timeout}
    end.

%% @doc 添加消息到任务历史
%%
%% @param TaskRef Task 进程引用
%% @param Message 消息 map
%% @returns ok | {error, Reason}
-spec add_message(pid() | atom(), map()) -> ok | {error, term()}.
add_message(TaskRef, Message) ->
    try
        gen_server:call(TaskRef, {add_message, Message}, ?DEFAULT_TIMEOUT)
    catch
        exit:{noproc, _} -> {error, task_not_found};
        exit:{timeout, _} -> {error, timeout}
    end.

%% @doc 添加产出物
%%
%% @param TaskRef Task 进程引用
%% @param Artifact 产出物 map
%% @returns ok | {error, Reason}
-spec add_artifact(pid() | atom(), map()) -> ok | {error, term()}.
add_artifact(TaskRef, Artifact) ->
    try
        gen_server:call(TaskRef, {add_artifact, Artifact}, ?DEFAULT_TIMEOUT)
    catch
        exit:{noproc, _} -> {error, task_not_found};
        exit:{timeout, _} -> {error, timeout}
    end.

%% @doc 取消任务
%%
%% @param TaskRef Task 进程引用
%% @returns ok | {error, Reason}
-spec cancel(pid() | atom()) -> ok | {error, term()}.
cancel(TaskRef) ->
    update_status(TaskRef, canceled).

%% @doc 停止任务进程
-spec stop(pid() | atom()) -> ok.
stop(TaskRef) ->
    gen_server:stop(TaskRef, normal, ?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @private
init(Opts) ->
    Now = erlang:system_time(millisecond),

    %% 生成或使用提供的 Task ID
    TaskId = maps:get(task_id, Opts, generate_task_id()),
    ContextId = maps:get(context_id, Opts, undefined),
    Metadata = maps:get(metadata, Opts, #{}),

    %% 初始消息
    InitMessage = maps:get(message, Opts, undefined),
    Messages = case InitMessage of
        undefined -> [];
        Msg -> [Msg]
    end,

    State = #task_state{
        id = TaskId,
        context_id = ContextId,
        status = submitted,
        status_message = undefined,
        history = [],
        artifacts = [],
        messages = Messages,
        metadata = Metadata,
        agent_ref = undefined,
        created_at = Now,
        updated_at = Now
    },

    {ok, State}.

%% @private
handle_call(get_task, _From, State) ->
    TaskMap = state_to_map(State),
    {reply, {ok, TaskMap}, State};

handle_call(get_id, _From, #task_state{id = Id} = State) ->
    {reply, {ok, Id}, State};

handle_call({update_status, NewStatus}, _From, State) ->
    case do_update_status(NewStatus, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({add_message, Message}, _From, State) ->
    #task_state{messages = Messages} = State,
    NewState = State#task_state{
        messages = Messages ++ [Message],
        updated_at = erlang:system_time(millisecond)
    },
    {reply, ok, NewState};

handle_call({add_artifact, Artifact}, _From, State) ->
    #task_state{artifacts = Artifacts} = State,
    %% 确保 Artifact 有 ID
    ArtifactWithId = ensure_artifact_id(Artifact),
    NewState = State#task_state{
        artifacts = Artifacts ++ [ArtifactWithId],
        updated_at = erlang:system_time(millisecond)
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 生成任务 ID
generate_task_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFFFFFF),
    iolist_to_binary(io_lib:format("task-~.16b-~.8b", [Timestamp, Random])).

%% @private 确保 Artifact 有 ID
ensure_artifact_id(Artifact) ->
    case maps:is_key(artifact_id, Artifact) of
        true -> Artifact;
        false ->
            Id = generate_artifact_id(),
            Artifact#{artifact_id => Id}
    end.

%% @private 生成 Artifact ID
generate_artifact_id() ->
    Random = rand:uniform(16#FFFFFFFF),
    iolist_to_binary(io_lib:format("art-~.8b", [Random])).

%% @private 执行状态更新
do_update_status(NewStatus, State) ->
    #task_state{status = CurrentStatus} = State,

    %% 检查是否允许状态转换
    case can_transition(CurrentStatus, extract_state(NewStatus)) of
        true ->
            Now = erlang:system_time(millisecond),

            %% 构建历史记录
            HistoryEntry = #{
                state => CurrentStatus,
                timestamp => State#task_state.updated_at
            },

            %% 解析新状态
            {NewState, NewMessage, NewArtifacts} = parse_new_status(NewStatus),

            %% 更新状态
            UpdatedState = State#task_state{
                status = NewState,
                status_message = NewMessage,
                history = State#task_state.history ++ [HistoryEntry],
                artifacts = case NewArtifacts of
                    undefined -> State#task_state.artifacts;
                    _ -> State#task_state.artifacts ++ NewArtifacts
                end,
                updated_at = Now
            },
            {ok, UpdatedState};
        false ->
            {error, {invalid_transition, CurrentStatus, extract_state(NewStatus)}}
    end.

%% @private 从复合状态提取状态原子
extract_state(State) when is_atom(State) -> State;
extract_state({State, _}) -> State;
extract_state({State, _, _}) -> State.

%% @private 解析新状态
parse_new_status(State) when is_atom(State) ->
    {State, undefined, undefined};
parse_new_status({State, Message}) ->
    {State, Message, undefined};
parse_new_status({State, Message, Artifacts}) ->
    {State, Message, Artifacts}.

%% @private 检查状态转换是否有效
%%
%% 规则：
%% - 终态（completed, failed, canceled, rejected）不能转换到其他状态
%% - submitted 只能转到 working, rejected, canceled
%% - working 可以转到 input_required, auth_required, completed, failed, canceled
%% - input_required 可以转到 working, canceled
%% - auth_required 可以转到 working, canceled
can_transition(CurrentState, NewState) ->
    case {CurrentState, NewState} of
        %% 终态不能转换
        {completed, _} -> false;
        {failed, _} -> false;
        {canceled, _} -> false;
        {rejected, _} -> false;

        %% submitted 的有效转换
        {submitted, working} -> true;
        {submitted, rejected} -> true;
        {submitted, canceled} -> true;

        %% working 的有效转换
        {working, input_required} -> true;
        {working, auth_required} -> true;
        {working, completed} -> true;
        {working, failed} -> true;
        {working, canceled} -> true;

        %% input_required 的有效转换
        {input_required, working} -> true;
        {input_required, canceled} -> true;
        {input_required, completed} -> true;
        {input_required, failed} -> true;

        %% auth_required 的有效转换
        {auth_required, working} -> true;
        {auth_required, canceled} -> true;

        %% 其他转换无效
        _ -> false
    end.

%% @private 将状态记录转换为 map
state_to_map(#task_state{} = State) ->
    #{
        id => State#task_state.id,
        context_id => State#task_state.context_id,
        status => #{
            state => State#task_state.status,
            message => State#task_state.status_message,
            timestamp => State#task_state.updated_at
        },
        history => State#task_state.history,
        artifacts => State#task_state.artifacts,
        messages => State#task_state.messages,
        metadata => State#task_state.metadata,
        created_at => State#task_state.created_at,
        updated_at => State#task_state.updated_at
    }.
