%%%-------------------------------------------------------------------
%%% @doc Agent 暂停持久化集成（opt-in 全自动）
%%%
%%% 把 agent 的中断态（interrupt_state）经 pause_store 落库/回读/清除，使 HITL
%%% 中断可跨进程重启续跑（见 design/hitl_timeline_serial_errors.md §5）。
%%% 未配置 pause_store（缺省）时全部为 no-op / none，行为不变。
%%%
%%%   - save/1：agent 处于中断态时，构建纯数据快照并 pause_save（覆盖）；
%%%   - load/1：本进程无中断态时，从 store 回读快照重建 interrupt_state；
%%%   - clear/1：任何终态 / 新 chat / resume 成功后清除（store 始终镜像"是否有
%%%     未决暂停"）。
%%%
%%% 快照（纯数据，version=1）：conversation_id / paused_at / pause_reason /
%%% pending_tool / interrupt_state。存档前校验 term 可序列化（防御，warn 并放行）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_pause).

-export([save/1, load/1, clear/1]).

-define(SNAPSHOT_VERSION, 1).

%%====================================================================
%% API
%%====================================================================

%% @doc 若配置了 pause_store 且 agent 处于中断态，落库暂停快照
-spec save(map()) -> ok.
save(Agent) ->
    case {store(Agent), interrupt_state(Agent)} of
        {undefined, _} -> ok;
        {_Store, undefined} -> ok;
        {Store, IntState} ->
            ConvId = conv_id(Agent),
            Snapshot = build_snapshot(ConvId, IntState),
            case check_serializable(Snapshot) of
                ok -> ok;
                {warn, Bad} ->
                    logger:warning("beamai_agent_pause: 暂停快照含疑似不可序列化项 ~p"
                                   "（仍尝试落库；跨重启可能失败）", [Bad])
            end,
            beamai_pause_store:pause_save(Store, ConvId, Snapshot)
    end.

%% @doc 从 pause_store 回读快照重建中断态（无 store / 无快照返回 none）
-spec load(map()) -> {ok, map()} | none.
load(Agent) ->
    case store(Agent) of
        undefined -> none;
        Store ->
            ConvId = conv_id(Agent),
            case beamai_pause_store:pause_load(Store, ConvId) of
                {ok, #{interrupt_state := IntState}} ->
                    {ok, Agent#{interrupt_state => IntState}};
                none ->
                    none
            end
    end.

%% @doc 清除该会话的暂停快照（无 store 则 no-op）
-spec clear(map()) -> ok.
clear(Agent) ->
    case store(Agent) of
        undefined -> ok;
        Store -> beamai_pause_store:pause_clear(Store, conv_id(Agent))
    end.

%%====================================================================
%% 内部
%%====================================================================

store(Agent) -> maps:get(pause_store, Agent, undefined).

interrupt_state(Agent) -> maps:get(interrupt_state, Agent, undefined).

conv_id(Agent) -> maps:get(conversation_id, Agent, <<>>).

build_snapshot(ConvId, IntState) ->
    #{
        version => ?SNAPSHOT_VERSION,
        conversation_id => ConvId,
        paused_at => erlang:system_time(millisecond),
        pause_reason => maps:get(reason, IntState, undefined),
        pending_tool => maps:get(interrupted_tool_call, IntState, undefined),
        interrupt_state => IntState
    }.

%% @private 校验快照可经 term_to_binary/binary_to_term 往返（不可序列化如 fun/pid
%% 会破坏跨重启恢复）。interrupt_state 本应为纯数据；此为防御性告警。
check_serializable(Snapshot) ->
    try
        _ = binary_to_term(term_to_binary(Snapshot)),
        ok
    catch
        _:_ -> {warn, non_serializable}
    end.
