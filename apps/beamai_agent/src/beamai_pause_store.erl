%%%-------------------------------------------------------------------
%%% @doc 暂停快照存储 Behaviour（跨进程 HITL）
%%%
%%% 把 agent 的"暂停点"一致快照按 conversation_id 持久化，使中断可跨进程重启
%%% 续跑（见 design/hitl_timeline_serial_errors.md §5）。每 conversation_id 至多
%%% 一份（再暂停覆盖）——store 始终镜像"该会话是否有未决暂停"。
%%%
%%% 句柄约定 `{Module, Ref}`（与 beamai_chat_memory 一致）。
%%%
%%% **只存暂停点快照**（纯数据），不存 kernel/tools/callbacks（代码侧 resume 重建）、
%%% 不存对话历史（ChatMemory 已管——跨重启配持久 store）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_pause_store).

%% 调度 API
-export([pause_save/3, pause_load/2, pause_clear/2]).

-export_type([handle/0, snapshot/0]).

-type handle() :: {module(), term()}.
-type snapshot() :: map().  %% #{version, conversation_id, paused_at, pause_reason, pending_tool, interrupt_state}

%%====================================================================
%% Behaviour 回调
%%====================================================================

%% @doc 保存某会话的暂停快照（覆盖既有）
-callback pause_save(Ref :: term(), ConvId :: binary(), Snapshot :: snapshot()) -> ok.

%% @doc 读取某会话的暂停快照（无则 none）
-callback pause_load(Ref :: term(), ConvId :: binary()) -> {ok, snapshot()} | none.

%% @doc 清除某会话的暂停快照
-callback pause_clear(Ref :: term(), ConvId :: binary()) -> ok.

%%====================================================================
%% 调度 API
%%====================================================================

-spec pause_save(handle(), binary(), snapshot()) -> ok.
pause_save({Module, Ref}, ConvId, Snapshot) ->
    Module:pause_save(Ref, ConvId, Snapshot).

-spec pause_load(handle(), binary()) -> {ok, snapshot()} | none.
pause_load({Module, Ref}, ConvId) ->
    Module:pause_load(Ref, ConvId).

-spec pause_clear(handle(), binary()) -> ok.
pause_clear({Module, Ref}, ConvId) ->
    Module:pause_clear(Ref, ConvId).
