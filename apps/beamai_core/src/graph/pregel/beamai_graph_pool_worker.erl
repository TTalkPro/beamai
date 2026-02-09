%%%-------------------------------------------------------------------
%%% @doc Graph Pool Worker
%%%
%%% poolboy 管理的无状态 worker 进程。
%%% 接收 {execute, ComputeFn, Context} 调用，try-catch 包裹执行。
%%% 执行完毕后 worker 归还池中复用。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_pool_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([execute/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% @doc 启动 worker（由 poolboy 调用）
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @doc 在指定 worker 中执行计算函数
-spec execute(pid(), fun((map()) -> map()), map()) -> {ok, map()} | {error, term()}.
execute(Worker, ComputeFn, Context) ->
    gen_server:call(Worker, {execute, ComputeFn, Context}, infinity).

init(_Args) ->
    {ok, #{}}.

handle_call({execute, ComputeFn, Context}, _From, State) ->
    Result = try
        {ok, ComputeFn(Context)}
    catch
        Class:Reason:Stack ->
            {error, {compute_error, {Class, Reason, Stack}}}
    end,
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
