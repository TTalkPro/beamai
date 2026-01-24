%%%-------------------------------------------------------------------
%%% @doc DeepAgent 并行执行模块
%%%
%%% 管理一个 layer 中多个步骤的并行执行。
%%% 按 max_parallel 分批，使用 spawn_monitor 启动执行器，
%%% 收集结果并处理超时和崩溃。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_parallel).

-export([execute_layer/2]).

-define(DEFAULT_MAX_PARALLEL, 5).
-define(DEFAULT_STEP_TIMEOUT, 300000). %% 5 minutes per step

%%====================================================================
%% API
%%====================================================================

%% @doc 并行执行一个 layer 中的所有步骤
%%
%% 将步骤按 max_parallel 分批，每批内并行执行，批间串行。
%% 每批结果累积到协调器状态的 completed_results 中，
%% 供后续批次作为上下文使用。
%%
%% Steps: 本层所有步骤列表
%% CoordState: 协调器状态（包含 config, completed_results 等）
-spec execute_layer([map()], map()) ->
    {ok, [beamai_deepagent_executor:step_result()], map()} | {error, term()}.
execute_layer([], CoordState) ->
    {ok, [], CoordState};
execute_layer(Steps, CoordState) ->
    Config = maps:get(config, CoordState),
    MaxParallel = maps:get(max_parallel, Config, ?DEFAULT_MAX_PARALLEL),
    StepTimeout = maps:get(timeout, Config, ?DEFAULT_STEP_TIMEOUT),
    Batches = split_into_batches(Steps, MaxParallel),
    execute_batches(Batches, CoordState, StepTimeout, []).

%%====================================================================
%% 内部函数 - 批次执行
%%====================================================================

%% @doc 逐批执行步骤，累积结果到协调器状态
%%
%% 每批执行完成后将结果追加到 completed_results，
%% 使后续批次的执行器能获取前序结果作为上下文。
-spec execute_batches([[map()]], map(), pos_integer(), [map()]) ->
    {ok, [map()], map()} | {error, term()}.
execute_batches([], CoordState, _Timeout, AccResults) ->
    {ok, lists:reverse(AccResults), CoordState};
execute_batches([Batch | Rest], CoordState, Timeout, AccResults) ->
    Context = #{previous_results => maps:get(completed_results, CoordState, [])},
    case execute_batch(Batch, CoordState, Context, Timeout) of
        {ok, BatchResults} ->
            NewCompleted = maps:get(completed_results, CoordState, []) ++ BatchResults,
            NewState = CoordState#{completed_results => NewCompleted},
            execute_batches(Rest, NewState, Timeout, BatchResults ++ AccResults);
        {error, _} = Err ->
            Err
    end.

%% @doc 并行执行单批步骤，收集所有结果
%%
%% 为每个步骤 spawn_monitor 一个工作进程，
%% 然后统一收集结果，处理完成、失败、崩溃和超时情况。
-spec execute_batch([map()], map(), map(), pos_integer()) -> {ok, [map()]} | {error, term()}.
execute_batch(Steps, CoordState, Context, Timeout) ->
    Workers = [spawn_step_worker(Step, CoordState, Context) || Step <- Steps],
    collect_results(Workers, Timeout, []).

%%====================================================================
%% 内部函数 - 工作进程管理
%%====================================================================

%% @doc 启动步骤执行工作进程
%%
%% 使用 spawn_monitor 创建受监控的工作进程，
%% 执行完成后通过消息将结果发回父进程。
%% 返回 #{pid, monitor_ref, step_id} 描述的工作进程信息。
-spec spawn_step_worker(map(), map(), map()) -> map().
spawn_step_worker(Step, CoordState, Context) ->
    Parent = self(),
    StepId = maps:get(id, Step),
    {Pid, MonRef} = spawn_monitor(fun() ->
        Result = beamai_deepagent_executor:execute_step(Step, CoordState, Context),
        Parent ! {step_done, self(), Result}
    end),
    #{pid => Pid, monitor_ref => MonRef, step_id => StepId}.

%% @doc 收集所有工作进程的执行结果
%%
%% 通过 receive 循环等待结果消息和监控通知，
%% 处理四种情况：正常完成、执行失败、进程崩溃、超时。
%% 超时时终止所有剩余工作进程并标记为 timeout。
-spec collect_results([map()], pos_integer(), [map()]) -> {ok, [map()]}.
collect_results([], _Timeout, Acc) ->
    {ok, lists:reverse(Acc)};
collect_results(Workers, Timeout, Acc) ->
    receive
        {step_done, Pid, {ok, StepResult}} ->
            Remaining = remove_worker(pid, Pid, Workers),
            collect_results(Remaining, Timeout, [StepResult | Acc]);

        {step_done, Pid, {error, Reason}} ->
            StepId = find_step_id(pid, Pid, Workers),
            Remaining = remove_worker(pid, Pid, Workers),
            ErrMsg = beamai_deepagent_utils:format_error(Reason),
            FailResult = beamai_deepagent_utils:make_step_result(StepId, failed, ErrMsg, #{}),
            collect_results(Remaining, Timeout, [FailResult | Acc]);

        {'DOWN', MonRef, process, _Pid, Reason} when Reason =/= normal ->
            StepId = find_step_id(monitor_ref, MonRef, Workers),
            Remaining = remove_worker(monitor_ref, MonRef, Workers),
            ErrMsg = beamai_deepagent_utils:format_error({process_crashed, Reason}),
            CrashResult = beamai_deepagent_utils:make_step_result(StepId, crashed, ErrMsg, #{}),
            collect_results(Remaining, Timeout, [CrashResult | Acc]);

        {'DOWN', MonRef, process, _Pid, normal} ->
            Remaining = remove_worker(monitor_ref, MonRef, Workers),
            collect_results(Remaining, Timeout, Acc)

    after Timeout ->
        TimeoutResults = [make_timeout_result(W) || W <- Workers],
        kill_workers(Workers),
        {ok, lists:reverse(Acc) ++ TimeoutResults}
    end.

%% @doc 从工作进程列表中按指定字段移除匹配的进程
%%
%% Field: 匹配字段名（pid | monitor_ref）
%% Value: 匹配值
%% Workers: 工作进程列表
-spec remove_worker(atom(), term(), [map()]) -> [map()].
remove_worker(Field, Value, Workers) ->
    [W || W <- Workers, maps:get(Field, W) =/= Value].

%% @doc 在工作进程列表中按指定字段查找步骤 ID
%%
%% 如果未找到匹配的工作进程，返回 0 作为占位值。
-spec find_step_id(atom(), term(), [map()]) -> non_neg_integer().
find_step_id(Field, Value, Workers) ->
    case [maps:get(step_id, W) || W <- Workers, maps:get(Field, W) =:= Value] of
        [StepId | _] -> StepId;
        [] -> 0
    end.

%% @doc 为超时的工作进程创建 timeout 结果
-spec make_timeout_result(map()) -> map().
make_timeout_result(#{step_id := StepId}) ->
    beamai_deepagent_utils:make_step_result(StepId, timeout,
        <<"Step execution timed out">>, #{}).

%% @doc 终止所有剩余工作进程并清除监控消息
-spec kill_workers([map()]) -> ok.
kill_workers(Workers) ->
    lists:foreach(fun(#{pid := Pid, monitor_ref := MonRef}) ->
        exit(Pid, kill),
        demonitor(MonRef, [flush])
    end, Workers).

%%====================================================================
%% 内部函数 - 列表分批
%%====================================================================

%% @doc 将列表按指定大小分为多个子列表
%%
%% 用于将一层的步骤按 max_parallel 分批执行。
-spec split_into_batches([T], pos_integer()) -> [[T]] when T :: term().
split_into_batches([], _Size) ->
    [];
split_into_batches(List, Size) when Size >= length(List) ->
    [List];
split_into_batches(List, Size) ->
    {Batch, Rest} = lists:split(Size, List),
    [Batch | split_into_batches(Rest, Size)].
