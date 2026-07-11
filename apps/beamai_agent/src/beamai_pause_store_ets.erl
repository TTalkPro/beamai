%%%-------------------------------------------------------------------
%%% @doc 暂停快照存储 - ETS 内存实现（第一个适配）
%%%
%%% 基于 ETS 表的 beamai_pause_store 实现，按 conversation_id 存暂停快照，每
%%% 会话至多一份（再暂停覆盖）。进程持有 ETS 表所有权，进程退出表自动回收。
%%%
%%% 句柄：`{beamai_pause_store_ets, Name}`。
%%%
%%% 注：ETS 是**进程内**存储——"跨进程 HITL"在同一 BEAM 节点内跨 agent 实例
%%% 成立（重建 agent 共享同一 store 进程）；真正跨 OS 进程/重启需持久后端
%%% （如 SQLite），可另实现本 behaviour。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_pause_store_ets).

-behaviour(gen_server).
-behaviour(beamai_pause_store).

%% API
-export([start_link/1, start_link/2, stop/1, handle/1]).

%% beamai_pause_store 回调
-export([pause_save/3, pause_load/2, pause_clear/2]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {table :: ets:tid()}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(Name) ->
    start_link(Name, #{}).

-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(Name, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Opts], []).

-spec stop(atom()) -> ok.
stop(Name) ->
    gen_server:stop(Name).

%% @doc 构造 beamai_pause_store 句柄
-spec handle(atom()) -> beamai_pause_store:handle().
handle(Name) ->
    {?MODULE, Name}.

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

init([Name, _Opts]) ->
    Table = ets:new(Name, [set, protected]),
    {ok, #state{table = Table}}.

handle_call({pause_save, ConvId, Snapshot}, _From, #state{table = T} = State) ->
    ets:insert(T, {ConvId, Snapshot}),
    {reply, ok, State};
handle_call({pause_load, ConvId}, _From, #state{table = T} = State) ->
    Reply = case ets:lookup(T, ConvId) of
        [{ConvId, Snapshot}] -> {ok, Snapshot};
        [] -> none
    end,
    {reply, Reply, State};
handle_call({pause_clear, ConvId}, _From, #state{table = T} = State) ->
    ets:delete(T, ConvId),
    {reply, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
