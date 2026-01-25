%%%-------------------------------------------------------------------
%%% @doc beamai_a2a_types 模块单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_types_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Task State 转换测试
%%====================================================================

task_state_binary_to_atom_test_() ->
    [
        ?_assertEqual(submitted, beamai_a2a_types:binary_to_task_state(<<"submitted">>)),
        ?_assertEqual(working, beamai_a2a_types:binary_to_task_state(<<"working">>)),
        ?_assertEqual(input_required, beamai_a2a_types:binary_to_task_state(<<"input-required">>)),
        ?_assertEqual(auth_required, beamai_a2a_types:binary_to_task_state(<<"auth-required">>)),
        ?_assertEqual(completed, beamai_a2a_types:binary_to_task_state(<<"completed">>)),
        ?_assertEqual(failed, beamai_a2a_types:binary_to_task_state(<<"failed">>)),
        ?_assertEqual(canceled, beamai_a2a_types:binary_to_task_state(<<"canceled">>)),
        ?_assertEqual(rejected, beamai_a2a_types:binary_to_task_state(<<"rejected">>)),
        %% 未知状态默认为 submitted
        ?_assertEqual(submitted, beamai_a2a_types:binary_to_task_state(<<"unknown">>))
    ].

task_state_atom_to_binary_test_() ->
    [
        ?_assertEqual(<<"submitted">>, beamai_a2a_types:task_state_to_binary(submitted)),
        ?_assertEqual(<<"working">>, beamai_a2a_types:task_state_to_binary(working)),
        ?_assertEqual(<<"input-required">>, beamai_a2a_types:task_state_to_binary(input_required)),
        ?_assertEqual(<<"auth-required">>, beamai_a2a_types:task_state_to_binary(auth_required)),
        ?_assertEqual(<<"completed">>, beamai_a2a_types:task_state_to_binary(completed)),
        ?_assertEqual(<<"failed">>, beamai_a2a_types:task_state_to_binary(failed)),
        ?_assertEqual(<<"canceled">>, beamai_a2a_types:task_state_to_binary(canceled)),
        ?_assertEqual(<<"rejected">>, beamai_a2a_types:task_state_to_binary(rejected))
    ].

is_terminal_state_test_() ->
    [
        ?_assertEqual(true, beamai_a2a_types:is_terminal_state(completed)),
        ?_assertEqual(true, beamai_a2a_types:is_terminal_state(failed)),
        ?_assertEqual(true, beamai_a2a_types:is_terminal_state(canceled)),
        ?_assertEqual(true, beamai_a2a_types:is_terminal_state(rejected)),
        ?_assertEqual(false, beamai_a2a_types:is_terminal_state(submitted)),
        ?_assertEqual(false, beamai_a2a_types:is_terminal_state(working)),
        ?_assertEqual(false, beamai_a2a_types:is_terminal_state(input_required)),
        ?_assertEqual(false, beamai_a2a_types:is_terminal_state(auth_required))
    ].

%%====================================================================
%% Role 转换测试
%%====================================================================

role_conversion_test_() ->
    [
        ?_assertEqual(user, beamai_a2a_types:binary_to_role(<<"user">>)),
        ?_assertEqual(agent, beamai_a2a_types:binary_to_role(<<"agent">>)),
        ?_assertEqual(user, beamai_a2a_types:binary_to_role(<<"unknown">>)),
        ?_assertEqual(<<"user">>, beamai_a2a_types:role_to_binary(user)),
        ?_assertEqual(<<"agent">>, beamai_a2a_types:role_to_binary(agent))
    ].

%%====================================================================
%% Part Kind 转换测试
%%====================================================================

part_kind_conversion_test_() ->
    [
        ?_assertEqual(text, beamai_a2a_types:binary_to_part_kind(<<"text">>)),
        ?_assertEqual(file, beamai_a2a_types:binary_to_part_kind(<<"file">>)),
        ?_assertEqual(data, beamai_a2a_types:binary_to_part_kind(<<"data">>)),
        ?_assertEqual(text, beamai_a2a_types:binary_to_part_kind(<<"unknown">>)),
        ?_assertEqual(<<"text">>, beamai_a2a_types:part_kind_to_binary(text)),
        ?_assertEqual(<<"file">>, beamai_a2a_types:part_kind_to_binary(file)),
        ?_assertEqual(<<"data">>, beamai_a2a_types:part_kind_to_binary(data))
    ].

%%====================================================================
%% Push 事件转换测试（安全版本）
%%====================================================================

push_event_binary_to_atom_test_() ->
    [
        %% 有效的事件名称
        ?_assertEqual(submitted, beamai_a2a_types:binary_to_push_event(<<"submitted">>)),
        ?_assertEqual(working, beamai_a2a_types:binary_to_push_event(<<"working">>)),
        ?_assertEqual(input_required, beamai_a2a_types:binary_to_push_event(<<"input_required">>)),
        ?_assertEqual(input_required, beamai_a2a_types:binary_to_push_event(<<"input-required">>)),
        ?_assertEqual(auth_required, beamai_a2a_types:binary_to_push_event(<<"auth_required">>)),
        ?_assertEqual(auth_required, beamai_a2a_types:binary_to_push_event(<<"auth-required">>)),
        ?_assertEqual(completed, beamai_a2a_types:binary_to_push_event(<<"completed">>)),
        ?_assertEqual(failed, beamai_a2a_types:binary_to_push_event(<<"failed">>)),
        ?_assertEqual(canceled, beamai_a2a_types:binary_to_push_event(<<"canceled">>)),
        ?_assertEqual(rejected, beamai_a2a_types:binary_to_push_event(<<"rejected">>)),
        ?_assertEqual(all, beamai_a2a_types:binary_to_push_event(<<"all">>)),
        %% 无效的事件名称返回 undefined（安全处理）
        ?_assertEqual(undefined, beamai_a2a_types:binary_to_push_event(<<"unknown">>)),
        ?_assertEqual(undefined, beamai_a2a_types:binary_to_push_event(<<"malicious_event">>)),
        ?_assertEqual(undefined, beamai_a2a_types:binary_to_push_event(not_binary))
    ].

push_event_atom_to_binary_test_() ->
    [
        ?_assertEqual(<<"submitted">>, beamai_a2a_types:push_event_to_binary(submitted)),
        ?_assertEqual(<<"working">>, beamai_a2a_types:push_event_to_binary(working)),
        ?_assertEqual(<<"input-required">>, beamai_a2a_types:push_event_to_binary(input_required)),
        ?_assertEqual(<<"auth-required">>, beamai_a2a_types:push_event_to_binary(auth_required)),
        ?_assertEqual(<<"completed">>, beamai_a2a_types:push_event_to_binary(completed)),
        ?_assertEqual(<<"failed">>, beamai_a2a_types:push_event_to_binary(failed)),
        ?_assertEqual(<<"canceled">>, beamai_a2a_types:push_event_to_binary(canceled)),
        ?_assertEqual(<<"rejected">>, beamai_a2a_types:push_event_to_binary(rejected)),
        ?_assertEqual(<<"all">>, beamai_a2a_types:push_event_to_binary(all)),
        ?_assertEqual(<<"unknown">>, beamai_a2a_types:push_event_to_binary(invalid_event))
    ].

is_valid_push_event_test_() ->
    [
        %% 有效的 binary 事件
        ?_assertEqual(true, beamai_a2a_types:is_valid_push_event(<<"completed">>)),
        ?_assertEqual(true, beamai_a2a_types:is_valid_push_event(<<"failed">>)),
        ?_assertEqual(true, beamai_a2a_types:is_valid_push_event(<<"all">>)),
        %% 有效的 atom 事件
        ?_assertEqual(true, beamai_a2a_types:is_valid_push_event(completed)),
        ?_assertEqual(true, beamai_a2a_types:is_valid_push_event(failed)),
        ?_assertEqual(true, beamai_a2a_types:is_valid_push_event(all)),
        %% 无效的事件
        ?_assertEqual(false, beamai_a2a_types:is_valid_push_event(<<"unknown">>)),
        ?_assertEqual(false, beamai_a2a_types:is_valid_push_event(unknown_atom)),
        ?_assertEqual(false, beamai_a2a_types:is_valid_push_event(123))
    ].
