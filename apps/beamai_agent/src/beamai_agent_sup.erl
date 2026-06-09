%%%-------------------------------------------------------------------
%%% @doc beamai_agent 顶层 Supervisor
%%%
%%% 监督 agent 的会话历史 store（filter-memory 后端）。store 作为**动态子进程**
%%% 按需加入（ensure_store/1），permanent 重启策略，崩溃后自动重启——取代原先
%%% unlink 的孤儿单例（不受监督、崩溃静默丢失记忆）。
%%%
%%% 仅当 beamai_agent 作为 OTP 应用被启动时本 supervisor 才在线；库式直接调用
%%% （或裸 eunit）时由 beamai_agent_state 回退到懒启动孤儿 store。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, ensure_store/1]).

%% Supervisor 回调
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc 确保指定名字的会话 store 在监督树下运行（幂等），返回其句柄。
%%
%% 首次调用时把 store 作为 permanent 动态子进程加入；已运行则直接复用；
%% 子规格已存在但进程不在时重启之。
-spec ensure_store(atom()) -> beamai_chat_memory:handle().
ensure_store(Name) ->
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            ok;
        undefined ->
            case supervisor:start_child(?MODULE, store_spec(Name)) of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                {error, already_present} ->
                    _ = supervisor:restart_child(?MODULE, Name),
                    ok
            end
    end,
    beamai_chat_memory_ets:handle(Name).

%%====================================================================
%% Supervisor 回调
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    %% store 按需通过 ensure_store/1 动态加入，无静态子进程
    {ok, {SupFlags, []}}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 会话 store 子进程规格（ETS 后端，permanent）
store_spec(Name) ->
    #{
        id => Name,
        start => {beamai_chat_memory_ets, start_link, [Name]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_chat_memory_ets]
    }.
