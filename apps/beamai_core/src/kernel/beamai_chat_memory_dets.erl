%%%-------------------------------------------------------------------
%%% @doc 会话消息存储 - DETS 持久化实现
%%%
%%% 基于 DETS 文件的 beamai_chat_memory 实现，按 conversation_id 持久化
%%% 会话消息，进程/节点重启后历史仍可恢复（对照 clj-agent 的 sqlite-store，
%%% 用 OTP 自带的 DETS 免去外部依赖）。
%%%
%%% 存储结构（set 表，逐条存储，追加为 O(新消息数)）：
%%%   {{msg, ConvId, Seq}, message()}  -- 单条消息，Seq 单调递增
%%%   {{seq, ConvId}, NextSeq}         -- 该会话的下一个序号
%%% 读取时按 Seq 排序还原正序（等价 SQLite 的 autoincrement + ORDER BY id）。
%%%
%%% 每次写操作后 dets:sync 落盘，保证进程崩溃不丢已确认的写入。
%%%
%%% 句柄：`{beamai_chat_memory_dets, Name}`，Name 为注册名。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_chat_memory_dets).

-behaviour(gen_server).
-behaviour(beamai_chat_memory).

%% API
-export([start_link/2, stop/1, handle/1]).

%% beamai_chat_memory 回调
-export([mem_get/2, mem_add/3, mem_clear/2]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-type message() :: beamai_message:message().

-record(state, {table :: dets:tab_name()}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 DETS 会话存储，返回 {ok, Pid}
%%
%% Name 同时作为 gen_server 注册名和句柄 Ref。
%% Opts 必须含 file（DETS 文件路径，binary 或 string）。
-spec start_link(atom(), #{file := file:name_all(), _ => _}) ->
          {ok, pid()} | {error, term()}.
start_link(Name, #{file := _} = Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Opts], []).

%% @doc 停止会话存储（关闭 DETS 表，自动落盘）
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

init([Name, #{file := File}]) ->
    case dets:open_file(Name, [{file, to_list(File)}, {type, set}]) of
        {ok, Table} -> {ok, #state{table = Table}};
        {error, Reason} -> {stop, Reason}
    end.

handle_call({mem_get, ConvId}, _From, #state{table = T} = State) ->
    Pairs = dets:match(T, {{msg, ConvId, '$1'}, '$2'}),
    Sorted = lists:keysort(1, [{Seq, Msg} || [Seq, Msg] <- Pairs]),
    {reply, [Msg || {_Seq, Msg} <- Sorted], State};
handle_call({mem_add, _ConvId, []}, _From, State) ->
    {reply, ok, State};
handle_call({mem_add, ConvId, NewMsgs}, _From, #state{table = T} = State) ->
    Seq0 = case dets:lookup(T, {seq, ConvId}) of
        [{_, Next}] -> Next;
        [] -> 0
    end,
    {Objects, SeqN} = lists:mapfoldl(
        fun(Msg, Seq) -> {{{msg, ConvId, Seq}, Msg}, Seq + 1} end,
        Seq0, NewMsgs),
    ok = dets:insert(T, [{{seq, ConvId}, SeqN} | Objects]),
    ok = dets:sync(T),
    {reply, ok, State};
handle_call({mem_clear, ConvId}, _From, #state{table = T} = State) ->
    ok = dets:match_delete(T, {{msg, ConvId, '_'}, '_'}),
    ok = dets:delete(T, {seq, ConvId}),
    ok = dets:sync(T),
    {reply, ok, State};
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

%% @private DETS 的 file 选项只收 string
to_list(File) when is_binary(File) -> binary_to_list(File);
to_list(File) when is_list(File) -> File;
to_list(File) when is_atom(File) -> atom_to_list(File).
