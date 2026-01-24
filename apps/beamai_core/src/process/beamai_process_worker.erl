%%%-------------------------------------------------------------------
%%% @doc poolboy 工作进程 - 步骤执行器
%%%
%%% 每个 worker 执行单个步骤的 on_activate 回调。
%%% 从连接池取出使用，执行完毕后归还。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_worker).

-behaviour(gen_server).

%% API
-export([execute_step/4]).

%% poolboy / gen_server 回调
-export([start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2]).

-define(POOL_NAME, beamai_process_pool).

%%====================================================================
%% API
%%====================================================================

%% @doc 通过 worker 进程执行步骤
%%
%% @param Worker worker 进程 PID
%% @param StepRuntimeState 步骤运行时状态
%% @param Inputs 已收集的输入 Map
%% @param Context 执行上下文
%% @returns 步骤执行结果
-spec execute_step(pid(), beamai_process_step:step_runtime_state(),
                   #{atom() => term()}, beamai_context:t()) ->
    {events, [beamai_process_event:event()], beamai_process_step:step_runtime_state()} |
    {pause, term(), beamai_process_step:step_runtime_state()} |
    {error, term()}.
execute_step(Worker, StepRuntimeState, Inputs, Context) ->
    gen_server:call(Worker, {execute_step, StepRuntimeState, Inputs, Context}, infinity).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @private 启动 worker 进程
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @private 初始化 worker 状态（无状态）
init(_Args) ->
    {ok, #{}}.

%% @private 处理步骤执行请求
handle_call({execute_step, StepRuntimeState, Inputs, Context}, _From, State) ->
    Result = beamai_process_step:execute(StepRuntimeState, Inputs, Context),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private 忽略异步消息
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private 忽略系统消息
handle_info(_Info, State) ->
    {noreply, State}.

%% @private 进程终止回调
terminate(_Reason, _State) ->
    ok.
