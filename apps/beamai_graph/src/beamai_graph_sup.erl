%%%-------------------------------------------------------------------
%%% @doc BeamAI Graph Top-level Supervisor
%%%
%%% Manages the dispatch worker pool for parallel graph execution.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @doc Initialize the supervisor
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    Children = get_children(),

    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Get child specs based on configuration
get_children() ->
    case should_start_dispatch_pool() of
        true -> [dispatch_pool_spec()];
        false -> []
    end.

%% @private Check if dispatch pool should be started
should_start_dispatch_pool() ->
    case application:get_env(beamai_graph, dispatch_pool_enabled, true) of
        false -> false;
        true ->
            case code:which(poolboy) of
                non_existing -> false;
                _ -> true
            end
    end.

%% @private Dispatch worker pool child spec
dispatch_pool_spec() ->
    PoolConfig = application:get_env(beamai_graph, dispatch_pool, #{}),
    Size = maps:get(size, PoolConfig, 20),
    MaxOverflow = maps:get(max_overflow, PoolConfig, 40),
    PoolArgs = [
        {name, {local, beamai_dispatch_pool}},
        {worker_module, pregel_dispatch_worker},
        {size, Size},
        {max_overflow, MaxOverflow},
        {strategy, fifo}
    ],
    poolboy:child_spec(beamai_dispatch_pool, PoolArgs, []).
