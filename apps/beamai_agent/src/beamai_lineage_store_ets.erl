%%%-------------------------------------------------------------------
%%% @doc 分支血缘存储 - ETS 内存实现（第一个适配）
%%%
%%% 基于 ETS 表的 beamai_lineage_store 实现，按 conversation_id 存血缘记录。
%%% 进程持有 ETS 表所有权，进程退出表自动回收。
%%%
%%% 句柄：`{beamai_lineage_store_ets, Name}`。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_lineage_store_ets).

-behaviour(gen_server).
-behaviour(beamai_lineage_store).

%% API
-export([start_link/1, start_link/2, stop/1, handle/1]).

%% beamai_lineage_store 回调
-export([record/3, get/2, children/2, delete/2]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {table :: ets:tid()}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(Name) -> start_link(Name, #{}).

-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(Name, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Opts], []).

-spec stop(atom()) -> ok.
stop(Name) -> gen_server:stop(Name).

%% @doc 构造 beamai_lineage_store 句柄
-spec handle(atom()) -> beamai_lineage_store:handle().
handle(Name) -> {?MODULE, Name}.

%%====================================================================
%% beamai_lineage_store 回调
%%====================================================================

-spec record(atom(), binary(), beamai_lineage_store:lineage_record()) -> ok.
record(Name, ConvId, Rec) ->
    gen_server:call(Name, {record, ConvId, Rec}).

-spec get(atom(), binary()) -> {ok, beamai_lineage_store:lineage_record()} | none.
get(Name, ConvId) ->
    gen_server:call(Name, {get, ConvId}).

-spec children(atom(), binary()) -> [binary()].
children(Name, ConvId) ->
    gen_server:call(Name, {children, ConvId}).

-spec delete(atom(), binary()) -> ok.
delete(Name, ConvId) ->
    gen_server:call(Name, {delete, ConvId}).

%%====================================================================
%% gen_server 回调
%%====================================================================

init([Name, _Opts]) ->
    Table = ets:new(Name, [set, protected]),
    {ok, #state{table = Table}}.

handle_call({record, ConvId, Rec}, _From, #state{table = T} = State) ->
    ets:insert(T, {ConvId, Rec}),
    {reply, ok, State};
handle_call({get, ConvId}, _From, #state{table = T} = State) ->
    Reply = case ets:lookup(T, ConvId) of
        [{ConvId, Rec}] -> {ok, Rec};
        [] -> none
    end,
    {reply, Reply, State};
handle_call({children, ConvId}, _From, #state{table = T} = State) ->
    Kids = ets:foldl(fun({Cid, #{parent := P}}, Acc) when P =:= ConvId -> [Cid | Acc];
                        (_, Acc) -> Acc
                     end, [], T),
    {reply, Kids, State};
handle_call({delete, ConvId}, _From, #state{table = T} = State) ->
    ets:delete(T, ConvId),
    {reply, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
