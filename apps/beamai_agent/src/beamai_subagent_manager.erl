%%%-------------------------------------------------------------------
%%% @doc 子 Agent 管理器（异步会话注册表）
%%%
%%% 对标 OpenClaw `sessions_spawn`：把子 agent 做成**受管异步会话**。常驻
%%% gen_server,维护注册表 `id => 条目`,每个子 agent 在 spawn_monitor 的独立进程
%%% 里跑,管理器记录其状态/结果,并提供查询/取结果/kill/restart/drop。
%%%
%%% 状态机：running → done | failed | killed。
%%% - 正常完成：worker 回 {worker_done, Id, 结果}，状态 done。
%%% - 崩溃：'DOWN' 非 normal，状态 failed，结果 {error,{sub_agent_crashed,_}}。
%%% - kill：状态 killed，结果 {error, sub_agent_killed}。
%%% - restart：复用存的 spec 重跑（同 Id，状态回 running）。
%%%
%%% 结果回收：done/failed/killed 后按 TTL 自动 drop(默认 infinity 即不自动)，
%%% 或调用方显式 drop/1。
%%%
%%% `await/2` 为同步便利：订阅某 Id,完成即收到结果(delegate/fanout 在其上做"一次
%%% 调用拿结果"的同步封装)。管理器本身只做簿记,子 agent 的实际工作都在 worker
%%% 进程,故不是瓶颈。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_subagent_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, ensure_started/0]).
-export([spawn/1, await/2, result/1, list/0, list/1, kill/1, restart/1, drop/1]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([spec/0, id/0, status/0, info/0, outcome/0]).

-type id() :: binary().
-type status() :: running | done | failed | killed.
-type outcome() :: {ok, binary()} | {error, term()}.
-type spec() :: #{
    subagent := map(),                       %% beamai_agent:new/1 配置
    prompt := binary(),                      %% 子 agent 的输入
    result => fun((map()) -> binary()),      %% 从 run 结果提取(默认取 content)
    owner => term()                          %% 归属标识(用于 list/1 过滤)
}.
-type info() :: #{id := id(), status := status(), owner := term(),
                  started_at := integer(), finished_at := integer() | undefined}.

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> start_link(#{}).

%% Opts: #{ttl => infinity | pos_integer()}  完成条目自动回收的毫秒数
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc 确保管理器在线（幂等，返回 pid）。
%%
%% 正常由 beamai_agent_sup 启动并监督;当 beamai_agent 未作为 OTP 应用启动时
%% (库式调用 / 裸 eunit)懒启动一个 unlink 的孤儿单例,使委派在无 app 下也可用。
-spec ensure_started() -> pid().
ensure_started() ->
    case whereis(?SERVER) of
        Pid when is_pid(Pid) ->
            Pid;
        undefined ->
            case whereis(beamai_agent_sup) of
                Sup when is_pid(Sup) ->
                    %% 监督树在线：作为动态子进程纳入
                    beamai_agent_sup:ensure_manager();
                undefined ->
                    %% app 未启动：懒启动孤儿单例
                    case start_link(#{}) of
                        {ok, Pid} -> unlink(Pid), Pid;
                        {error, {already_started, Pid}} -> Pid
                    end
            end
    end.

%% @doc 异步起一个子 agent,立即返回 id。
-spec spawn(spec()) -> {ok, id()}.
spawn(Spec) -> gen_server:call(?SERVER, {spawn, Spec}).

%% @doc 同步等待某子 agent 完成(便利)。Timeout 到则放弃等待(**不** kill,子仍在跑)。
-spec await(id(), timeout()) -> outcome() | {error, timeout} | {error, not_found}.
await(Id, Timeout) ->
    case gen_server:call(?SERVER, {subscribe, Id, self()}) of
        {ready, Outcome} -> Outcome;
        {error, _} = Err -> Err;
        subscribed ->
            receive {subagent_result, Id, Outcome} -> Outcome
            after Timeout ->
                gen_server:cast(?SERVER, {unsubscribe, Id, self()}),
                {error, timeout}
            end
    end.

%% @doc 取已完成子 agent 的结果（running 返回 not_ready）。
-spec result(id()) -> {ok, outcome()} | {error, not_ready | not_found}.
result(Id) -> gen_server:call(?SERVER, {result, Id}).

%% @doc 列出所有 / 指定 owner 的子 agent 信息。
-spec list() -> [info()].
list() -> gen_server:call(?SERVER, {list, all}).
-spec list(term()) -> [info()].
list(Owner) -> gen_server:call(?SERVER, {list, {owner, Owner}}).

%% @doc kill 指定子 agent（running 才有意义）。
-spec kill(id()) -> ok | {error, not_found}.
kill(Id) -> gen_server:call(?SERVER, {kill, Id}).

%% @doc 用存的 spec 重跑指定子 agent（同 Id，状态回 running）。
-spec restart(id()) -> {ok, id()} | {error, not_found}.
restart(Id) -> gen_server:call(?SERVER, {restart, Id}).

%% @doc 从注册表移除（running 会先被 kill）。
-spec drop(id()) -> ok.
drop(Id) -> gen_server:call(?SERVER, {drop, Id}).

%%====================================================================
%% gen_server 回调
%%====================================================================

init(Opts) ->
    {ok, #{agents => #{}, waiters => #{}, ttl => maps:get(ttl, Opts, infinity)}}.

handle_call({spawn, Spec}, _From, St) ->
    Id = beamai_id:gen_id(<<"sub">>),
    {reply, {ok, Id}, start_entry(Id, Spec, St)};

handle_call({subscribe, Id, Pid}, _From, #{agents := Agents, waiters := W} = St) ->
    case maps:find(Id, Agents) of
        error -> {reply, {error, not_found}, St};
        {ok, #{status := running}} ->
            {reply, subscribed, St#{waiters := add_waiter(Id, Pid, W)}};
        {ok, #{result := Outcome}} ->
            {reply, {ready, Outcome}, St}
    end;

handle_call({result, Id}, _From, #{agents := Agents} = St) ->
    Reply = case maps:find(Id, Agents) of
        error -> {error, not_found};
        {ok, #{status := running}} -> {error, not_ready};
        {ok, #{result := Outcome}} -> {ok, Outcome}
    end,
    {reply, Reply, St};

handle_call({list, Filter}, _From, #{agents := Agents} = St) ->
    Infos = [info(E) || E <- maps:values(Agents), match_owner(Filter, E)],
    {reply, Infos, St};

handle_call({kill, Id}, _From, #{agents := Agents} = St) ->
    case maps:find(Id, Agents) of
        error -> {reply, {error, not_found}, St};
        {ok, #{status := running} = E} ->
            stop_worker(E),
            {reply, ok, finish(Id, killed, {error, sub_agent_killed}, St)};
        {ok, _} -> {reply, ok, St}
    end;

handle_call({restart, Id}, _From, #{agents := Agents} = St) ->
    case maps:find(Id, Agents) of
        error -> {reply, {error, not_found}, St};
        {ok, #{spec := Spec} = E} ->
            case maps:get(status, E) of
                running -> stop_worker(E);
                _ -> ok
            end,
            {reply, {ok, Id}, start_entry(Id, Spec, St)}
    end;

handle_call({drop, Id}, _From, #{agents := Agents, waiters := W} = St) ->
    case maps:find(Id, Agents) of
        {ok, #{status := running} = E} -> stop_worker(E);
        _ -> ok
    end,
    {reply, ok, St#{agents := maps:remove(Id, Agents), waiters := maps:remove(Id, W)}}.

handle_cast({unsubscribe, Id, Pid}, #{waiters := W} = St) ->
    {noreply, St#{waiters := del_waiter(Id, Pid, W)}};
handle_cast(_, St) ->
    {noreply, St}.

handle_info({worker_done, Id, Outcome}, #{agents := Agents} = St) ->
    case maps:find(Id, Agents) of
        {ok, #{status := running, mref := MRef}} ->
            erlang:demonitor(MRef, [flush]),
            {noreply, finish(Id, done_or_failed(Outcome), Outcome, St)};
        _ -> {noreply, St}   %% 已被 kill/drop/restart，忽略迟到结果
    end;

handle_info({'DOWN', MRef, process, _Pid, Reason}, #{agents := Agents} = St) ->
    case find_by_mref(MRef, Agents) of
        {ok, Id, #{status := running}} ->
            {noreply, finish(Id, failed, {error, {sub_agent_crashed, Reason}}, St)};
        _ -> {noreply, St}
    end;

handle_info({ttl_drop, Id, FinishedAt}, #{agents := Agents, waiters := W} = St) ->
    case maps:find(Id, Agents) of
        {ok, #{status := S, finished_at := FinishedAt}} when S =/= running ->
            {noreply, St#{agents := maps:remove(Id, Agents), waiters := maps:remove(Id, W)}};
        _ -> {noreply, St}   %% 已 restart / drop / finished_at 变了 → 忽略
    end;

handle_info(_, St) ->
    {noreply, St}.

%%====================================================================
%% 内部
%%====================================================================

%% @private 起一个 worker 并登记/重置条目（同 Id 覆盖即 restart）
start_entry(Id, Spec, #{agents := Agents} = St) ->
    {Pid, MRef} = spawn_worker(Id, Spec),
    Entry = #{id => Id, pid => Pid, mref => MRef, status => running,
              spec => Spec, result => undefined,
              owner => maps:get(owner, Spec, undefined),
              started_at => now_ms(), finished_at => undefined},
    St#{agents := maps:put(Id, Entry, Agents)}.

%% @private worker：独立进程跑子 agent，完成把结果回传管理器
spawn_worker(Id, Spec) ->
    Mgr = self(),
    spawn_monitor(fun() -> Mgr ! {worker_done, Id, do_run(Spec)} end).

stop_worker(#{pid := Pid, mref := MRef}) when is_pid(Pid) ->
    erlang:demonitor(MRef, [flush]),
    exit(Pid, kill),
    ok;
stop_worker(_) -> ok.

%% @private 实际建子 agent 并跑一轮，归一为 outcome()
do_run(#{subagent := Cfg, prompt := Prompt} = Spec) ->
    ResultFn = maps:get(result, Spec, fun default_result/1),
    case beamai_agent:new(Cfg) of
        {ok, Sub} ->
            case beamai_agent:run(Sub, Prompt) of
                {ok, Res, _Sub1} -> {ok, ResultFn(Res)};
                {interrupt, _Info, _Sub1} -> {error, sub_agent_interrupted};
                {error, Reason} -> {error, {sub_agent_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {sub_agent_init_failed, Reason}}
    end.

default_result(#{content := C}) when is_binary(C) -> C;
default_result(_) -> <<>>.

%% @private 落定终态：更新条目、通知 waiters、安排 TTL 回收
finish(Id, Status, Outcome, #{agents := Agents, waiters := W, ttl := Ttl} = St) ->
    Now = now_ms(),
    Entry0 = maps:get(Id, Agents),
    Entry = Entry0#{status => Status, result => Outcome, finished_at => Now,
                    pid => undefined, mref => undefined},
    notify(Id, Outcome, W),
    schedule_ttl(Id, Now, Ttl),
    St#{agents := maps:put(Id, Entry, Agents), waiters := maps:remove(Id, W)}.

notify(Id, Outcome, Waiters) ->
    lists:foreach(fun(P) -> P ! {subagent_result, Id, Outcome} end,
                  maps:get(Id, Waiters, [])).

schedule_ttl(_Id, _Now, infinity) -> ok;
schedule_ttl(Id, Now, Ttl) when is_integer(Ttl) ->
    erlang:send_after(Ttl, self(), {ttl_drop, Id, Now}), ok.

add_waiter(Id, Pid, W) -> maps:update_with(Id, fun(L) -> [Pid | L] end, [Pid], W).
del_waiter(Id, Pid, W) ->
    case maps:find(Id, W) of
        {ok, L} -> maps:put(Id, lists:delete(Pid, L), W);
        error -> W
    end.

find_by_mref(MRef, Agents) ->
    maps:fold(fun(Id, #{mref := M} = E, _) when M =:= MRef -> {ok, Id, E};
                 (_, _, Acc) -> Acc
              end, error, Agents).

match_owner(all, _E) -> true;
match_owner({owner, O}, #{owner := O}) -> true;
match_owner({owner, _}, _E) -> false.

info(#{id := Id, status := S, owner := O, started_at := SA, finished_at := FA}) ->
    #{id => Id, status => S, owner => O, started_at => SA, finished_at => FA}.

done_or_failed({ok, _}) -> done;
done_or_failed({error, _}) -> failed.

now_ms() -> erlang:system_time(millisecond).
