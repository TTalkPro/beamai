%%%-------------------------------------------------------------------
%%% @doc 暂停快照存储 - DETS 持久化实现
%%%
%%% 基于 DETS 文件的 beamai_pause_store 实现。与 ETS 版（beamai_pause_store_ets）
%%% 的区别只有一个，但很关键：**节点重启后未决的暂停还在**。
%%%
%%% ETS 版的表属主是那个 gen_server，进程一死表就回收 —— 它只解决"同一节点内
%%% 跨 agent 实例"的 HITL。可人回话可能是十分钟后、也可能是运维重启之后的事，
%%% 那时 ETS 里什么都不剩，卡在半路的会话只能作废。
%%%
%%% 存储结构（set 表，每会话至多一份，再暂停即覆盖 —— 与 behaviour 的约定一致）：
%%%   {ConvId, Snapshot}
%%%
%%% 每次写后 `dets:sync` 落盘：一个没来得及落盘的暂停，恰恰是节点崩溃时你最需要
%%% 的那个。代价是每次暂停多一次 fsync —— 暂停本身是"等人"级别的低频事件，
%%% 这点开销无关紧要。
%%%
%%% 句柄：`{beamai_pause_store_dets, Name}`，Name 为注册名。
%%%
%%% == 快照必须可序列化 ==
%%%
%%% 快照按约定是纯数据（beamai_agent_pause 存档前有 term_to_binary 往返校验，
%%% 不过它只 warn 不拦）。真混进 fun / pid 时：fun 能写进 DETS 但换版本就解不回来，
%%% pid 重启后指向虚无 —— 两者都是**读得回来但语义已错**，比写失败更难查。
%%% 所以这里的错误处理只能覆盖写失败（磁盘满、表已关），语义正确性靠上游那道校验。
%%%
%%% == 与 ETS 版不同的一件事：得清理 ==
%%%
%%% ETS 版进程一死就干净了；持久版不会 —— 用户开了个话题、agent 停下来问，
%%% 人再也没回来，这条快照会永远躺在磁盘上。故额外提供 prune/2（按 paused_at
%%% 清理陈旧暂停）与 conversations/1，behaviour 之外，纯运维用。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_pause_store_dets).

-behaviour(gen_server).
-behaviour(beamai_pause_store).

%% API
-export([start_link/2, stop/1, handle/1]).
%% 运维（behaviour 之外）
-export([conversations/1, prune/2]).

%% beamai_pause_store 回调
-export([pause_save/3, pause_load/2, pause_clear/2]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {table :: dets:tab_name()}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 DETS 暂停存储
%%
%% Name 同时作为 gen_server 注册名和句柄 Ref。
%% Opts 必须含 file（DETS 文件路径，binary 或 string）。
-spec start_link(atom(), #{file := file:name_all(), _ => _}) ->
          {ok, pid()} | {error, term()}.
start_link(Name, #{file := _} = Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Opts], []).

%% @doc 停止暂停存储（关闭 DETS 表，自动落盘）
-spec stop(atom()) -> ok.
stop(Name) ->
    gen_server:stop(Name).

%% @doc 构造 beamai_pause_store 句柄
-spec handle(atom()) -> beamai_pause_store:handle().
handle(Name) ->
    {?MODULE, Name}.

%% @doc 当前有未决暂停的会话列表（运维查看用）
-spec conversations(atom()) -> [binary()].
conversations(Name) ->
    gen_server:call(Name, conversations).

%% @doc 清理陈旧暂停：paused_at 早于 Now - MaxAgeMs 的一律删除
%%
%% 持久化带来的新问题：被放弃的暂停不会自己消失。没有 paused_at 的快照
%% （理论上不该有）当作陈旧处理 —— 一份连什么时候存的都说不清的快照，
%% 留着也没法判断该不该恢复。
-spec prune(atom(), non_neg_integer()) -> {ok, non_neg_integer()}.
prune(Name, MaxAgeMs) ->
    gen_server:call(Name, {prune, MaxAgeMs}).

%%====================================================================
%% beamai_pause_store 回调
%%====================================================================

-spec pause_save(atom(), binary(), beamai_pause_store:snapshot()) -> ok.
pause_save(Name, ConvId, Snapshot) ->
    gen_server:call(Name, {pause_save, ConvId, Snapshot}).

-spec pause_load(atom(), binary()) -> {ok, beamai_pause_store:snapshot()} | none.
pause_load(Name, ConvId) ->
    gen_server:call(Name, {pause_load, ConvId}).

-spec pause_clear(atom(), binary()) -> ok.
pause_clear(Name, ConvId) ->
    gen_server:call(Name, {pause_clear, ConvId}).

%%====================================================================
%% gen_server 回调
%%====================================================================

init([Name, #{file := File}]) ->
    case dets:open_file(Name, [{file, to_list(File)}, {type, set}]) of
        {ok, Table} -> {ok, #state{table = Table}};
        {error, Reason} -> {stop, Reason}
    end.

handle_call({pause_save, ConvId, Snapshot}, _From, #state{table = T} = State) ->
    %% 写失败只能记账：behaviour 契约是 -> ok，上游 beamai_agent_pause:save/1
    %% 也不看返回值。丢一个暂停比让整个 run 崩在收尾上强，但必须留下痕迹。
    case dets:insert(T, {ConvId, Snapshot}) of
        ok ->
            ok = dets:sync(T);
        {error, Reason} ->
            logger:error("beamai_pause_store_dets: 暂停快照落库失败 conv=~ts reason=~p"
                         "（该会话将无法 resume）", [ConvId, Reason])
    end,
    {reply, ok, State};
handle_call({pause_load, ConvId}, _From, #state{table = T} = State) ->
    Reply = case dets:lookup(T, ConvId) of
        [{ConvId, Snapshot}] -> {ok, Snapshot};
        _ -> none
    end,
    {reply, Reply, State};
handle_call({pause_clear, ConvId}, _From, #state{table = T} = State) ->
    ok = dets:delete(T, ConvId),
    ok = dets:sync(T),
    {reply, ok, State};
handle_call(conversations, _From, #state{table = T} = State) ->
    {reply, lists:sort(dets:foldl(fun({ConvId, _}, Acc) -> [ConvId | Acc] end, [], T)),
     State};
handle_call({prune, MaxAgeMs}, _From, #state{table = T} = State) ->
    Cutoff = erlang:system_time(millisecond) - MaxAgeMs,
    Stale = dets:foldl(fun(Entry, Acc) ->
        case stale(Entry, Cutoff) of
            true -> [element(1, Entry) | Acc];
            false -> Acc
        end
    end, [], T),
    _ = [dets:delete(T, ConvId) || ConvId <- Stale],
    ok = dets:sync(T),
    {reply, {ok, length(Stale)}, State};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{table = T}) ->
    _ = dets:close(T),
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 没有 paused_at 的快照当作陈旧（说不清什么时候存的，判断不了该不该留）
stale({_ConvId, #{paused_at := At}}, Cutoff) when is_integer(At) -> At < Cutoff;
stale(_Entry, _Cutoff) -> true.

to_list(File) when is_binary(File) -> binary_to_list(File);
to_list(File) -> File.
