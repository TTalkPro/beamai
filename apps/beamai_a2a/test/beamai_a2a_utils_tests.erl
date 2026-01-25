%%%-------------------------------------------------------------------
%%% @doc beamai_a2a_utils 模块单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% format_error/1 测试
%%====================================================================

format_error_binary_test_() ->
    [
        %% binary 直接返回
        ?_assertEqual(<<"some error">>, beamai_a2a_utils:format_error(<<"some error">>)),
        ?_assertEqual(<<>>, beamai_a2a_utils:format_error(<<>>))
    ].

format_error_atom_test_() ->
    [
        %% atom 转换为 binary
        ?_assertEqual(<<"not_found">>, beamai_a2a_utils:format_error(not_found)),
        ?_assertEqual(<<"timeout">>, beamai_a2a_utils:format_error(timeout)),
        ?_assertEqual(<<"error">>, beamai_a2a_utils:format_error(error))
    ].

format_error_list_test_() ->
    [
        %% 字符串列表转换为 binary
        ?_assertEqual(<<"hello">>, beamai_a2a_utils:format_error("hello")),
        ?_assertEqual(<<"test error">>, beamai_a2a_utils:format_error("test error"))
    ].

format_error_tuple_test_() ->
    [
        %% 复杂类型使用 io_lib:format
        ?_assertMatch(<<"{http_error,500,", _/binary>>,
                      beamai_a2a_utils:format_error({http_error, 500, "Internal Error"})),
        ?_assertMatch(<<"{error,", _/binary>>,
                      beamai_a2a_utils:format_error({error, some_reason}))
    ].

%%====================================================================
%% generate_id/0 测试
%%====================================================================

generate_id_test_() ->
    [
        %% ID 应该是 binary
        ?_assert(is_binary(beamai_a2a_utils:generate_id())),
        %% ID 长度应该是 36（UUID 格式）
        ?_assertEqual(36, byte_size(beamai_a2a_utils:generate_id())),
        %% 两次生成的 ID 应该不同
        ?_assertNotEqual(beamai_a2a_utils:generate_id(),
                         beamai_a2a_utils:generate_id())
    ].

generate_id_with_prefix_test_() ->
    [
        %% 带前缀的 ID
        ?_assertMatch(<<"task_", _/binary>>, beamai_a2a_utils:generate_id(<<"task_">>)),
        ?_assertMatch(<<"msg_", _/binary>>, beamai_a2a_utils:generate_id(<<"msg_">>)),
        %% 长度应该是 前缀长度 + 36
        ?_assertEqual(5 + 36, byte_size(beamai_a2a_utils:generate_id(<<"task_">>)))
    ].

%%====================================================================
%% timestamp/0 测试
%%====================================================================

timestamp_test_() ->
    [
        %% 时间戳应该是正整数
        ?_assert(is_integer(beamai_a2a_utils:timestamp())),
        ?_assert(beamai_a2a_utils:timestamp() > 0),
        %% 时间戳应该在合理范围内（2024年之后，毫秒级）
        ?_assert(beamai_a2a_utils:timestamp() > 1704067200000)
    ].

%%====================================================================
%% timestamp_iso8601/0 测试
%%====================================================================

timestamp_iso8601_test_() ->
    [
        %% 应该是 binary
        ?_assert(is_binary(beamai_a2a_utils:timestamp_iso8601())),
        %% 格式应该包含 T 和 Z
        ?_assertMatch(<<_:4/binary, "-", _:2/binary, "-", _:2/binary, "T",
                        _:2/binary, ":", _:2/binary, ":", _:2/binary, ".",
                        _:3/binary, "Z">>,
                      beamai_a2a_utils:timestamp_iso8601())
    ].
