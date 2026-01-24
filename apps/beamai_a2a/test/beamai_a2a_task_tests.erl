%%%-------------------------------------------------------------------
%%% @doc beamai_a2a_task 模块单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_task_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Task 创建测试
%%====================================================================

create_task_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),
    ?assert(is_pid(Pid)),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assert(is_map(Task)),
    ?assertEqual(submitted, maps:get(state, maps:get(status, Task))),

    beamai_a2a_task:stop(Pid).

create_task_with_message_test() ->
    Message = #{
        role => user,
        parts => [#{kind => text, text => <<"Hello">>}]
    },
    {ok, Pid} = beamai_a2a_task:start(#{message => Message}),

    {ok, Task} = beamai_a2a_task:get(Pid),
    Messages = maps:get(messages, Task),
    ?assertEqual(1, length(Messages)),

    beamai_a2a_task:stop(Pid).

create_task_with_context_id_test() ->
    ContextId = <<"ctx-123">>,
    {ok, Pid} = beamai_a2a_task:start(#{context_id => ContextId}),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual(ContextId, maps:get(context_id, Task)),

    beamai_a2a_task:stop(Pid).

%%====================================================================
%% Task ID 测试
%%====================================================================

get_task_id_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    {ok, TaskId} = beamai_a2a_task:get_id(Pid),
    ?assert(is_binary(TaskId)),
    ?assertMatch(<<"task_", _/binary>>, TaskId),

    beamai_a2a_task:stop(Pid).

custom_task_id_test() ->
    CustomId = <<"my-custom-task-id">>,
    {ok, Pid} = beamai_a2a_task:start(#{task_id => CustomId}),

    {ok, TaskId} = beamai_a2a_task:get_id(Pid),
    ?assertEqual(CustomId, TaskId),

    beamai_a2a_task:stop(Pid).

%%====================================================================
%% 状态转换测试
%%====================================================================

update_status_to_working_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    ok = beamai_a2a_task:update_status(Pid, working),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual(working, maps:get(state, maps:get(status, Task))),

    beamai_a2a_task:stop(Pid).

update_status_to_completed_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    %% 先转到 working
    ok = beamai_a2a_task:update_status(Pid, working),
    %% 再转到 completed
    ok = beamai_a2a_task:update_status(Pid, completed),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual(completed, maps:get(state, maps:get(status, Task))),

    beamai_a2a_task:stop(Pid).

update_status_to_input_required_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    ok = beamai_a2a_task:update_status(Pid, working),
    ok = beamai_a2a_task:update_status(Pid, input_required),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual(input_required, maps:get(state, maps:get(status, Task))),

    beamai_a2a_task:stop(Pid).

invalid_transition_from_submitted_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    %% submitted 不能直接转到 completed
    Result = beamai_a2a_task:update_status(Pid, completed),
    ?assertMatch({error, {invalid_transition, submitted, completed}}, Result),

    beamai_a2a_task:stop(Pid).

invalid_transition_from_terminal_state_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    ok = beamai_a2a_task:update_status(Pid, working),
    ok = beamai_a2a_task:update_status(Pid, completed),

    %% 终态不能再转换
    Result = beamai_a2a_task:update_status(Pid, working),
    ?assertMatch({error, {invalid_transition, completed, working}}, Result),

    beamai_a2a_task:stop(Pid).

%%====================================================================
%% 消息和产出物测试
%%====================================================================

add_message_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    Message = #{
        role => agent,
        parts => [#{kind => text, text => <<"Response">>}]
    },
    ok = beamai_a2a_task:add_message(Pid, Message),

    {ok, Task} = beamai_a2a_task:get(Pid),
    Messages = maps:get(messages, Task),
    ?assertEqual(1, length(Messages)),

    beamai_a2a_task:stop(Pid).

add_artifact_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    Artifact = #{
        name => <<"result">>,
        parts => [#{kind => data, data => #{<<"key">> => <<"value">>}}]
    },
    ok = beamai_a2a_task:add_artifact(Pid, Artifact),

    {ok, Task} = beamai_a2a_task:get(Pid),
    Artifacts = maps:get(artifacts, Task),
    ?assertEqual(1, length(Artifacts)),

    %% 应该自动生成 artifact_id
    [A] = Artifacts,
    ?assert(maps:is_key(artifact_id, A)),

    beamai_a2a_task:stop(Pid).

%%====================================================================
%% 取消测试
%%====================================================================

cancel_task_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    ok = beamai_a2a_task:update_status(Pid, working),
    ok = beamai_a2a_task:cancel(Pid),

    {ok, Task} = beamai_a2a_task:get(Pid),
    ?assertEqual(canceled, maps:get(state, maps:get(status, Task))),

    beamai_a2a_task:stop(Pid).

%%====================================================================
%% 历史记录测试
%%====================================================================

status_history_test() ->
    {ok, Pid} = beamai_a2a_task:start(#{}),

    ok = beamai_a2a_task:update_status(Pid, working),
    ok = beamai_a2a_task:update_status(Pid, input_required),
    ok = beamai_a2a_task:update_status(Pid, working),
    ok = beamai_a2a_task:update_status(Pid, completed),

    {ok, Task} = beamai_a2a_task:get(Pid),
    History = maps:get(history, Task),

    %% 历史应该有 4 条记录（每次状态变化前的状态）
    ?assertEqual(4, length(History)),

    %% 验证历史顺序
    [H1, H2, H3, H4] = History,
    ?assertEqual(submitted, maps:get(state, H1)),
    ?assertEqual(working, maps:get(state, H2)),
    ?assertEqual(input_required, maps:get(state, H3)),
    ?assertEqual(working, maps:get(state, H4)),

    beamai_a2a_task:stop(Pid).
