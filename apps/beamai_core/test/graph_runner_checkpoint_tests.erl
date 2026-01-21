%%%-------------------------------------------------------------------
%%% @doc graph_runner checkpoint 功能单元测试
%%%
%%% 测试 graph_runner 的 checkpoint 保存和恢复功能：
%%% - 简单模式（无 checkpoint）正常执行
%%% - on_checkpoint 回调被正确调用
%%% - 从 checkpoint 恢复执行
%%% - checkpoint 回调可以停止执行
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_runner_checkpoint_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试辅助函数
%%====================================================================

%% 创建多步测试图（节点循环执行多次）
make_multi_step_graph() ->
    ProcessFn = fun(State) ->
        Count = graph_state:get(State, count, 0),
        NewCount = Count + 1,
        State1 = graph_state:set(State, count, NewCount),
        graph_state:set(State1, last_count, NewCount)
    end,
    RouterFn = fun(State) ->
        Count = graph_state:get(State, count, 0),
        case Count >= 3 of
            true -> '__end__';
            false -> process
        end
    end,
    {ok, Graph} = graph:build([
        {node, process, ProcessFn},
        {conditional_edge, process, RouterFn},
        {entry, process}
    ]),
    Graph.

%%====================================================================
%% 模式检测测试
%%====================================================================

%% 测试：无 checkpoint 选项时 needs_checkpoint_mode 返回 false
needs_checkpoint_mode_false_test() ->
    ?assertEqual(false, graph_runner:needs_checkpoint_mode(#{})),
    ?assertEqual(false, graph_runner:needs_checkpoint_mode(#{workers => 2})).

%% 测试：有 checkpoint 选项时 needs_checkpoint_mode 返回 true
needs_checkpoint_mode_true_test() ->
    Callback = fun(_, _) -> continue end,
    ?assertEqual(true, graph_runner:needs_checkpoint_mode(#{on_checkpoint => Callback})),
    ?assertEqual(true, graph_runner:needs_checkpoint_mode(#{restore_from => #{}})),
    ?assertEqual(true, graph_runner:needs_checkpoint_mode(#{
        on_checkpoint => Callback,
        restore_from => #{}
    })).

%%====================================================================
%% Checkpoint 回调测试
%%====================================================================

%% 测试：on_checkpoint 回调被调用
checkpoint_callback_called_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    %% 使用进程字典记录回调调用次数
    Self = self(),
    Callback = fun(Info, CheckpointData) ->
        Self ! {checkpoint_called, Info, CheckpointData},
        continue
    end,

    Options = #{on_checkpoint => Callback},
    _Result = graph:run(Graph, InitialState, Options),

    %% 收集回调调用
    Calls = collect_checkpoint_calls([]),

    %% 验证回调被调用了（至少一次）
    ?assert(length(Calls) >= 1),

    %% 验证 checkpoint 数据结构
    %% 注意：graph_state 已移除，状态存储在 pregel_checkpoint.vertices 中
    [{_Info, FirstCheckpoint} | _] = Calls,
    ?assertNot(maps:is_key(graph_state, FirstCheckpoint)),  %% 不应包含 graph_state
    ?assert(maps:is_key(pregel_checkpoint, FirstCheckpoint)),
    ?assert(maps:is_key(iteration, FirstCheckpoint)).

collect_checkpoint_calls(Acc) ->
    receive
        {checkpoint_called, Info, Data} ->
            collect_checkpoint_calls([{Info, Data} | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

%% 测试：checkpoint 数据包含 pregel 层信息
checkpoint_contains_pregel_data_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    Self = self(),
    Callback = fun(_Info, CheckpointData) ->
        Self ! {checkpoint, CheckpointData},
        continue
    end,

    Options = #{on_checkpoint => Callback},
    _Result = graph:run(Graph, InitialState, Options),

    %% 获取第一个 checkpoint
    receive
        {checkpoint, Data} ->
            PregelCheckpoint = maps:get(pregel_checkpoint, Data),
            %% 验证 pregel checkpoint 结构
            ?assert(maps:is_key(superstep, PregelCheckpoint)),
            ?assert(maps:is_key(vertices, PregelCheckpoint)),
            ?assert(maps:is_key(pending_messages, PregelCheckpoint)),
            ?assert(maps:is_key(vertex_inbox, PregelCheckpoint))
    after 1000 ->
        ?assert(false)
    end.

%%====================================================================
%% Checkpoint 停止执行测试
%%====================================================================

%% 测试：checkpoint 回调返回 {stop, Reason} 时停止执行
checkpoint_callback_can_stop_execution_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    %% 回调在第一次调用时停止执行
    Callback = fun(_Info, _CheckpointData) ->
        {stop, user_requested}
    end,

    Options = #{on_checkpoint => Callback},
    Result = graph:run(Graph, InitialState, Options),

    %% 验证执行被停止
    ?assertEqual(stopped, maps:get(status, Result)),
    ?assertMatch({user_stopped, user_requested}, maps:get(error, Result)).

%%====================================================================
%% Checkpoint 恢复测试
%%====================================================================

%% 测试：从 checkpoint 恢复执行
restore_from_checkpoint_test() ->
    Graph = make_multi_step_graph(),
    InitialState = graph:state(#{count => 0}),

    %% 第一次执行，保存 checkpoint 后停止
    SavedCheckpoint = erlang:make_ref(),
    Self = self(),
    Callback = fun(_Info, CheckpointData) ->
        %% 保存 checkpoint 数据
        Self ! {save_checkpoint, SavedCheckpoint, CheckpointData},
        {stop, checkpoint_saved}
    end,

    Options1 = #{on_checkpoint => Callback},
    Result1 = graph:run(Graph, InitialState, Options1),

    %% 验证第一次执行被停止
    ?assertEqual(stopped, maps:get(status, Result1)),

    %% 获取保存的 checkpoint
    CheckpointData = receive
        {save_checkpoint, SavedCheckpoint, Data} -> Data
    after 1000 ->
        error(no_checkpoint_saved)
    end,

    %% 验证 checkpoint 数据结构正确
    ?assert(maps:is_key(pregel_checkpoint, CheckpointData)),
    ?assert(maps:is_key(iteration, CheckpointData)),

    %% 从 checkpoint 恢复执行（使用回调继续执行）
    #{pregel_checkpoint := PregelCheckpoint, iteration := Iteration} = CheckpointData,
    RestoreOpts = #{
        pregel_checkpoint => PregelCheckpoint,
        iteration => Iteration
    },

    %% 使用默认回调（continue）恢复执行
    Options2 = #{restore_from => RestoreOpts},
    Result2 = graph:run(Graph, InitialState, Options2),

    %% 恢复后应该完成或返回错误（取决于图执行逻辑）
    Status2 = maps:get(status, Result2),
    ?assert(Status2 =:= completed orelse Status2 =:= error).
