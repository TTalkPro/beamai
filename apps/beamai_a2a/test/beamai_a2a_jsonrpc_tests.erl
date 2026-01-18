%%%-------------------------------------------------------------------
%%% @doc beamai_a2a_jsonrpc 模块单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_jsonrpc_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 编码测试
%%====================================================================

encode_request_test() ->
    JsonBin = beamai_a2a_jsonrpc:encode_request(1, <<"message/send">>, #{<<"text">> => <<"hello">>}),
    ?assert(is_binary(JsonBin)),

    Decoded = jsx:decode(JsonBin, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(1, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"message/send">>, maps:get(<<"method">>, Decoded)),
    ?assertEqual(#{<<"text">> => <<"hello">>}, maps:get(<<"params">>, Decoded)).

encode_notification_test() ->
    JsonBin = beamai_a2a_jsonrpc:encode_notification(<<"tasks/update">>, #{<<"taskId">> => <<"t1">>}),
    ?assert(is_binary(JsonBin)),

    Decoded = jsx:decode(JsonBin, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertNot(maps:is_key(<<"id">>, Decoded)),
    ?assertEqual(<<"tasks/update">>, maps:get(<<"method">>, Decoded)).

encode_response_test() ->
    JsonBin = beamai_a2a_jsonrpc:encode_response(1, #{<<"status">> => <<"ok">>}),
    ?assert(is_binary(JsonBin)),

    Decoded = jsx:decode(JsonBin, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(1, maps:get(<<"id">>, Decoded)),
    ?assertEqual(#{<<"status">> => <<"ok">>}, maps:get(<<"result">>, Decoded)).

encode_error_test() ->
    JsonBin = beamai_a2a_jsonrpc:encode_error(1, -32600, <<"Invalid Request">>),
    ?assert(is_binary(JsonBin)),

    Decoded = jsx:decode(JsonBin, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(1, maps:get(<<"id">>, Decoded)),

    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32600, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Invalid Request">>, maps:get(<<"message">>, Error)).

encode_error_with_data_test() ->
    JsonBin = beamai_a2a_jsonrpc:encode_error(1, -32001, <<"Task not found">>, #{<<"taskId">> => <<"t1">>}),

    Decoded = jsx:decode(JsonBin, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(#{<<"taskId">> => <<"t1">>}, maps:get(<<"data">>, Error)).

%%====================================================================
%% 解码测试
%%====================================================================

decode_request_test() ->
    JsonBin = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"message/send">>,
        <<"params">> => #{<<"text">> => <<"hello">>}
    }, []),

    {ok, {Id, Method, Params}} = beamai_a2a_jsonrpc:decode_request(JsonBin),
    ?assertEqual(1, Id),
    ?assertEqual(<<"message/send">>, Method),
    ?assertEqual(#{<<"text">> => <<"hello">>}, Params).

decode_request_without_params_test() ->
    JsonBin = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"method">> => <<"tasks/get">>
    }, []),

    {ok, {Id, Method, Params}} = beamai_a2a_jsonrpc:decode_request(JsonBin),
    ?assertEqual(2, Id),
    ?assertEqual(<<"tasks/get">>, Method),
    ?assertEqual(#{}, Params).

decode_notification_test() ->
    JsonBin = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notify">>,
        <<"params">> => #{}
    }, []),

    {ok, {Id, Method, _}} = beamai_a2a_jsonrpc:decode_request(JsonBin),
    ?assertEqual(null, Id),  %% notification 没有 id
    ?assertEqual(<<"notify">>, Method).

decode_invalid_json_test() ->
    Result = beamai_a2a_jsonrpc:decode(<<"not valid json">>),
    ?assertMatch({error, parse_error}, Result).

decode_response_as_request_test() ->
    JsonBin = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{<<"status">> => <<"ok">>}
    }, []),

    Result = beamai_a2a_jsonrpc:decode_request(JsonBin),
    ?assertMatch({error, not_a_request}, Result).

%%====================================================================
%% 类型检测测试
%%====================================================================

is_request_test() ->
    Request = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test">>},
    Notification = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"test">>},
    Response = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => #{}},

    ?assert(beamai_a2a_jsonrpc:is_request(Request)),
    ?assertNot(beamai_a2a_jsonrpc:is_request(Notification)),
    ?assertNot(beamai_a2a_jsonrpc:is_request(Response)).

is_notification_test() ->
    Request = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test">>},
    Notification = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"test">>},

    ?assertNot(beamai_a2a_jsonrpc:is_notification(Request)),
    ?assert(beamai_a2a_jsonrpc:is_notification(Notification)).

is_response_test() ->
    Response = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => #{}},
    Error = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"error">> => #{}},

    ?assert(beamai_a2a_jsonrpc:is_response(Response)),
    ?assertNot(beamai_a2a_jsonrpc:is_response(Error)).

is_error_test() ->
    Response = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => #{}},
    Error = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"error">> => #{}},

    ?assertNot(beamai_a2a_jsonrpc:is_error(Response)),
    ?assert(beamai_a2a_jsonrpc:is_error(Error)).

%%====================================================================
%% 标准错误测试
%%====================================================================

parse_error_test() ->
    JsonBin = beamai_a2a_jsonrpc:parse_error(null),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32700, maps:get(<<"code">>, Error)).

invalid_request_test() ->
    JsonBin = beamai_a2a_jsonrpc:invalid_request(1),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32600, maps:get(<<"code">>, Error)).

method_not_found_test() ->
    JsonBin = beamai_a2a_jsonrpc:method_not_found(1, <<"unknown/method">>),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"unknown/method">>, maps:get(<<"method">>, maps:get(<<"data">>, Error))).

invalid_params_test() ->
    JsonBin = beamai_a2a_jsonrpc:invalid_params(1, <<"Missing taskId">>),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)).

internal_error_test() ->
    JsonBin = beamai_a2a_jsonrpc:internal_error(1),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32603, maps:get(<<"code">>, Error)).

%%====================================================================
%% A2A 特定错误测试
%%====================================================================

task_not_found_error_test() ->
    JsonBin = beamai_a2a_jsonrpc:task_not_found(1, <<"task-123">>),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32001, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"task-123">>, maps:get(<<"taskId">>, maps:get(<<"data">>, Error))).

task_already_completed_error_test() ->
    JsonBin = beamai_a2a_jsonrpc:task_already_completed(1, <<"task-456">>),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32002, maps:get(<<"code">>, Error)).

invalid_state_transition_error_test() ->
    JsonBin = beamai_a2a_jsonrpc:invalid_state_transition(1, completed, working),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32003, maps:get(<<"code">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(<<"completed">>, maps:get(<<"from">>, Data)),
    ?assertEqual(<<"working">>, maps:get(<<"to">>, Data)).

authentication_required_error_test() ->
    JsonBin = beamai_a2a_jsonrpc:authentication_required(1),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32004, maps:get(<<"code">>, Error)).

%%====================================================================
%% 批处理测试
%%====================================================================

decode_batch_test() ->
    BatchJson = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"m1">>, <<"params">> => #{}},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"method">> => <<"m2">>, <<"params">> => #{}}
    ], []),

    {ok, Result} = beamai_a2a_jsonrpc:decode(BatchJson),
    ?assert(beamai_a2a_jsonrpc:is_batch(Result)),

    {batch, Requests} = Result,
    ?assertEqual(2, length(Requests)).
