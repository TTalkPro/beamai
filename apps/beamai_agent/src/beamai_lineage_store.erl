%%%-------------------------------------------------------------------
%%% @doc 分支血缘存储 Behaviour（Timeline / 多分支）
%%%
%%% 记录会话分支的血缘（parent / fork_point），支撑 fork-as-new-conversation
%%% 的时间线模型（见 design/hitl_timeline_serial_errors.md §6）。对话历史本身是
%%% timeline（ChatMemory append-only），血缘只记"谁从谁的第几条分出来"。
%%%
%%% 句柄约定 `{Module, Ref}`（与 beamai_chat_memory / beamai_pause_store 一致）。
%%%
%%% 血缘记录（纯数据）：`#{parent := binary()|undefined, fork_point := non_neg_integer()|all,
%%% created_at := integer()}`。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_lineage_store).

%% 调度 API
-export([record/3, get/2, children/2, delete/2]).

-export_type([handle/0, lineage_record/0]).

-type handle() :: {module(), term()}.
-type lineage_record() :: #{
    parent := binary() | undefined,
    fork_point := non_neg_integer() | all,
    created_at := integer()
}.

%%====================================================================
%% Behaviour 回调
%%====================================================================

%% @doc 记录某会话的血缘（覆盖既有）
-callback record(Ref :: term(), ConvId :: binary(), Record :: lineage_record()) -> ok.

%% @doc 读取某会话的血缘记录（无则 none）
-callback get(Ref :: term(), ConvId :: binary()) -> {ok, lineage_record()} | none.

%% @doc 列出以 ConvId 为 parent 的直接子会话 id
-callback children(Ref :: term(), ConvId :: binary()) -> [binary()].

%% @doc 删除某会话的血缘记录
-callback delete(Ref :: term(), ConvId :: binary()) -> ok.

%%====================================================================
%% 调度 API
%%====================================================================

-spec record(handle(), binary(), lineage_record()) -> ok.
record({Module, Ref}, ConvId, Rec) ->
    Module:record(Ref, ConvId, Rec).

-spec get(handle(), binary()) -> {ok, lineage_record()} | none.
get({Module, Ref}, ConvId) ->
    Module:get(Ref, ConvId).

-spec children(handle(), binary()) -> [binary()].
children({Module, Ref}, ConvId) ->
    Module:children(Ref, ConvId).

-spec delete(handle(), binary()) -> ok.
delete({Module, Ref}, ConvId) ->
    Module:delete(Ref, ConvId).
