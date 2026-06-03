%%%-------------------------------------------------------------------
%%% @doc 会话消息存储 - ETS 内存实现（默认后端）
%%%
%%% 基于 ETS 表的 beamai_chat_memory 实现，按 conversation_id 追加存储
%%% 会话消息。进程持有 ETS 表所有权，进程退出表自动回收。
%%%
%%% 存储结构：ETS 表 {ConvId, [message()]}（消息正序）。
%%%
%%% 句柄：`{beamai_chat_memory_ets, Name}`，Name 为注册名。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_chat_memory_ets).

-behaviour(gen_server).
-behaviour(beamai_chat_memory).

%% API
-export([start_link/1, start_link/2, stop/1, handle/1]).

%% beamai_chat_memory 回调
-export([mem_get/2, mem_add/3, mem_clear/2]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-type message() :: beamai_message:message().

-record(state, {table :: ets:tid()}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 ETS 会话存储，返回 {ok, Pid}
%%
%% Name 同时作为 gen_server 注册名和句柄 Ref。
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(Name) ->
    start_link(Name, #{}).

%% @doc 启动 ETS 会话存储（带选项，预留）
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(Name, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Opts], []).

%% @doc 停止会话存储
-spec stop(atom()) -> ok.
stop(Name) ->
    gen_server:stop(Name).

%% @doc 构造 beamai_chat_memory 句柄
-spec handle(atom()) -> beamai_chat_memory:handle().
handle(Name) ->
    {?MODULE, Name}.

%%====================================================================
%% beamai_chat_memory 回调
%%====================================================================

-spec mem_get(atom(), binary()) -> [message()].
mem_get(Name, ConvId) ->
    gen_server:call(Name, {mem_get, ConvId}).

-spec mem_add(atom(), binary(), [message()]) -> ok.
mem_add(Name, ConvId, Msgs) ->
    gen_server:call(Name, {mem_add, ConvId, Msgs}).

-spec mem_clear(atom(), binary()) -> ok.
mem_clear(Name, ConvId) ->
    gen_server:call(Name, {mem_clear, ConvId}).

%%====================================================================
%% gen_server 回调
%%====================================================================

init([Name, _Opts]) ->
    Table = ets:new(Name, [set, protected]),
    {ok, #state{table = Table}}.

handle_call({mem_get, ConvId}, _From, #state{table = T} = State) ->
    Msgs = case ets:lookup(T, ConvId) of
        [{ConvId, Stored}] -> Stored;
        [] -> []
    end,
    {reply, Msgs, State};
handle_call({mem_add, ConvId, NewMsgs}, _From, #state{table = T} = State) ->
    Existing = case ets:lookup(T, ConvId) of
        [{ConvId, Stored}] -> Stored;
        [] -> []
    end,
    ets:insert(T, {ConvId, Existing ++ NewMsgs}),
    {reply, ok, State};
handle_call({mem_clear, ConvId}, _From, #state{table = T} = State) ->
    ets:delete(T, ConvId),
    {reply, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
