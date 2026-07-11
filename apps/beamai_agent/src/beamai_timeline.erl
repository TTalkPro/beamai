%%%-------------------------------------------------------------------
%%% @doc Timeline 与多分支：fork-as-new-conversation
%%%
%%% 对话历史（ChatMemory append-only）本身就是 timeline，无需快照版本链
%%% （见 design/hitl_timeline_serial_errors.md §6）。分支 = 前缀复制到新
%%% conversation_id + 一条血缘记录；所有既有组件（memory / pause / react 循环）
%%% 按 conversation_id 工作，无组件感知"树"，换分支 = 换 conv-id 建 agent。
%%%
%%% 合法的 fork/rollback 点只有两种：**turn 边界、暂停点**（一致性不变量）。
%%%
%%% deps 显式传入 `#{memory := Handle, branch := Handle, pause_store => Handle}`：
%%%   - memory / branch 必填；pause_store 可选（缺省则暂停快照不参与 fork/清理）。
%%%
%%% 操作集（对照旧 process Timeline，词汇减半——没有"当前位置"可变状态，
%%% switch/goto/back/forward 全退化为换 conv-id）：
%%%   fork/3、rollback/3、lineage/2、ancestry/2、prune/2。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_timeline).

-export([fork/2, fork/3, rollback/3, lineage/2, ancestry/2, prune/2]).

-type deps() :: #{
    memory := beamai_chat_memory:handle(),
    branch := beamai_branch_store:handle(),
    pause_store => beamai_pause_store:handle()
}.

-export_type([deps/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 全量分支（复制整段历史到新会话）
-spec fork(deps(), binary()) -> {ok, binary()}.
fork(Deps, Source) ->
    fork(Deps, Source, #{}).

%% @doc 分支：前缀复制到新 conversation_id + 血缘记录
%%
%% Opts：
%%   `as`  — 指定新 conversation_id（缺省自动生成）；
%%   `at`  — 前缀长度 N（复制前 N 条消息，fork_point=N）；缺省全量（fork_point=all）。
%%
%% 全量分支且源处于暂停（配置了 pause_store 且有快照）→ **连带复制暂停快照**到新
%% 会话（使分支可独立 resume）；部分前缀 fork 不带（暂停属于日志尖端）。
%%
%% @returns {ok, NewConvId}
-spec fork(deps(), binary(), map()) -> {ok, binary()}.
fork(#{memory := Mem, branch := Br} = Deps, Source, Opts) ->
    NewConvId = maps:get(as, Opts, gen_conv_id()),
    AllMsgs = beamai_chat_memory:mem_get(Mem, Source),
    {Prefix, ForkPoint, Full} = case maps:get(at, Opts, undefined) of
        undefined -> {AllMsgs, all, true};
        N when is_integer(N), N >= 0 -> {lists:sublist(AllMsgs, N), N, false}
    end,
    %% 复制前缀到新会话（先清空防御已有数据）
    ok = beamai_chat_memory:mem_clear(Mem, NewConvId),
    ok = beamai_chat_memory:mem_add(Mem, NewConvId, Prefix),
    ok = beamai_branch_store:record(Br, NewConvId,
        #{parent => Source, fork_point => ForkPoint,
          created_at => erlang:system_time(millisecond)}),
    %% 全量 fork 且源暂停 → 连带复制暂停快照
    Full andalso maybe_copy_pause(Deps, Source, NewConvId),
    {ok, NewConvId}.

%% @doc 破坏性截断到前 N 条消息（"重新生成"用）
%%
%% clear + 重写前缀实现（无需扩展 ChatMemory 协议）；清除该会话暂停快照
%% （尖端已变，未决暂停失效）。
-spec rollback(deps(), binary(), non_neg_integer()) -> ok.
rollback(#{memory := Mem} = Deps, ConvId, N) ->
    AllMsgs = beamai_chat_memory:mem_get(Mem, ConvId),
    Prefix = lists:sublist(AllMsgs, N),
    ok = beamai_chat_memory:mem_clear(Mem, ConvId),
    ok = beamai_chat_memory:mem_add(Mem, ConvId, Prefix),
    clear_pause(Deps, ConvId),
    ok.

%% @doc 查某会话的血缘记录（无则 none）
-spec lineage(deps(), binary()) -> {ok, beamai_branch_store:branch_record()} | none.
lineage(#{branch := Br}, ConvId) ->
    beamai_branch_store:get(Br, ConvId).

%% @doc 沿 parent 回溯到根，返回**根在前**的祖先链 [Root, ..., Parent, ConvId]
-spec ancestry(deps(), binary()) -> [binary()].
ancestry(#{branch := Br}, ConvId) ->
    ancestry_acc(Br, ConvId, [ConvId]).

%% @doc 删分支（历史 + 暂停快照 + 血缘）；有子分支时拒绝
-spec prune(deps(), binary()) -> ok | {error, {has_children, [binary()]}}.
prune(#{memory := Mem, branch := Br} = Deps, ConvId) ->
    case beamai_branch_store:children(Br, ConvId) of
        [] ->
            ok = beamai_chat_memory:mem_clear(Mem, ConvId),
            clear_pause(Deps, ConvId),
            ok = beamai_branch_store:delete(Br, ConvId),
            ok;
        Kids ->
            {error, {has_children, Kids}}
    end.

%%====================================================================
%% 内部
%%====================================================================

%% 前缀累积（prepend parent）天然得到根在前的顺序，无需反转
ancestry_acc(Br, ConvId, Acc) ->
    case beamai_branch_store:get(Br, ConvId) of
        {ok, #{parent := Parent}} when is_binary(Parent) ->
            ancestry_acc(Br, Parent, [Parent | Acc]);
        _ ->
            Acc
    end.

%% @private 全量 fork 时若源有暂停快照，复制到新会话（更新其 conversation_id）
maybe_copy_pause(#{pause_store := PS}, Source, NewConvId) ->
    case beamai_pause_store:pause_load(PS, Source) of
        {ok, Snapshot} ->
            beamai_pause_store:pause_save(PS, NewConvId,
                Snapshot#{conversation_id => NewConvId});
        none ->
            ok
    end;
maybe_copy_pause(_Deps, _Source, _NewConvId) ->
    ok.

%% @private 清除会话暂停快照（无 pause_store 则 no-op）
clear_pause(#{pause_store := PS}, ConvId) ->
    beamai_pause_store:pause_clear(PS, ConvId);
clear_pause(_Deps, _ConvId) ->
    ok.

gen_conv_id() ->
    beamai_id:gen_id(<<"conv">>).
