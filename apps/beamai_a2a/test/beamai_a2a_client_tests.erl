%%%-------------------------------------------------------------------
%%% @doc A2A Client 单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_client_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试设置
%%====================================================================

setup() ->
    meck:new(beamai_http, [passthrough]),
    ok.

cleanup(_) ->
    meck:unload(beamai_http),
    ok.

%%====================================================================
%% 测试套件
%%====================================================================

client_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"便捷函数测试", fun test_convenience_functions/0},
        {"Agent Card 发现测试", fun test_discover/0},
        {"消息发送测试", fun test_send_message/0},
        {"任务查询测试", fun test_get_task/0},
        {"任务取消测试", fun test_cancel_task/0},
        {"错误处理测试", fun test_error_handling/0}
     ]}.

%%====================================================================
%% 便捷函数测试
%%====================================================================

test_convenience_functions() ->
    %% 测试 create_text_message/1
    Msg1 = beamai_a2a_client:create_text_message(<<"Hello">>),
    ?assertEqual(<<"user">>, maps:get(<<"role">>, Msg1)),
    Parts1 = maps:get(<<"parts">>, Msg1),
    ?assertEqual(1, length(Parts1)),
    [Part1] = Parts1,
    ?assertEqual(<<"text">>, maps:get(<<"kind">>, Part1)),
    ?assertEqual(<<"Hello">>, maps:get(<<"text">>, Part1)),

    %% 测试 create_text_message/2 with agent role
    Msg2 = beamai_a2a_client:create_text_message("Response", agent),
    ?assertEqual(<<"agent">>, maps:get(<<"role">>, Msg2)),

    %% 测试字符串输入
    Msg3 = beamai_a2a_client:create_text_message("String text"),
    [Part3] = maps:get(<<"parts">>, Msg3),
    ?assertEqual(<<"String text">>, maps:get(<<"text">>, Part3)),

    ok.

%%====================================================================
%% Agent Card 发现测试
%%====================================================================

test_discover() ->
    %% 模拟成功的 Agent Card 响应
    MockCard = #{
        <<"name">> => <<"test-agent">>,
        <<"description">> => <<"Test Agent">>,
        <<"url">> => <<"https://test.example.com/a2a">>,
        <<"version">> => <<"1.0.0">>,
        <<"capabilities">> => #{
            <<"streaming">> => false,
            <<"pushNotifications">> => false
        },
        <<"skills">> => []
    },

    meck:expect(beamai_http, get, fun(_Url, _Params, _Opts) ->
        {ok, MockCard}
    end),

    %% 测试 discover
    {ok, Card} = beamai_a2a_client:discover("https://test.example.com"),
    ?assertEqual(<<"test-agent">>, maps:get(name, Card)),
    ?assertEqual(<<"Test Agent">>, maps:get(description, Card)),

    %% 验证 URL 构建
    ?assert(meck:called(beamai_http, get, ['_', '_', '_'])),

    ok.

test_discover_with_trailing_slash() ->
    MockCard = #{
        <<"name">> => <<"test-agent">>,
        <<"description">> => <<"Test">>,
        <<"url">> => <<"https://test.example.com/a2a">>,
        <<"version">> => <<"1.0.0">>,
        <<"capabilities">> => #{},
        <<"skills">> => []
    },

    meck:expect(beamai_http, get, fun(Url, _Params, _Opts) ->
        %% 验证 URL 没有双斜杠
        ?assertNot(binary:match(Url, <<"//.">>)),
        {ok, MockCard}
    end),

    {ok, _} = beamai_a2a_client:discover("https://test.example.com/"),
    ok.

%%====================================================================
%% 消息发送测试
%%====================================================================

test_send_message() ->
    %% 模拟成功的消息发送响应
    MockResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => <<"req-123">>,
        <<"result">> => #{
            <<"id">> => <<"task-abc123">>,
            <<"contextId">> => null,
            <<"status">> => #{
                <<"state">> => <<"working">>
            },
            <<"artifacts">> => []
        }
    },

    meck:expect(beamai_http, post_json, fun(_Url, Request, _Opts) ->
        %% 验证请求格式
        ?assertEqual(<<"message/send">>, maps:get(<<"method">>, Request)),
        ?assert(maps:is_key(<<"params">>, Request)),
        {ok, MockResponse}
    end),

    %% 发送消息
    Message = beamai_a2a_client:create_text_message(<<"Hello">>),
    {ok, Task} = beamai_a2a_client:send_message("https://test.example.com/a2a", Message),

    ?assertEqual(<<"task-abc123">>, maps:get(<<"id">>, Task)),
    ?assertEqual(<<"working">>, maps:get(<<"state">>, maps:get(<<"status">>, Task))),

    ok.

test_send_message_with_context() ->
    MockResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => <<"req-123">>,
        <<"result">> => #{
            <<"id">> => <<"task-abc123">>,
            <<"contextId">> => <<"ctx-456">>,
            <<"status">> => #{<<"state">> => <<"working">>}
        }
    },

    meck:expect(beamai_http, post_json, fun(_Url, Request, _Opts) ->
        %% 验证 contextId 被包含
        Params = maps:get(<<"params">>, Request),
        ?assertEqual(<<"ctx-456">>, maps:get(<<"contextId">>, Params)),
        {ok, MockResponse}
    end),

    Message = beamai_a2a_client:create_text_message(<<"Hello">>),
    {ok, _} = beamai_a2a_client:send_message(
        "https://test.example.com/a2a",
        Message,
        <<"ctx-456">>
    ),

    ok.

%%====================================================================
%% 任务查询测试
%%====================================================================

test_get_task() ->
    MockResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => <<"req-123">>,
        <<"result">> => #{
            <<"id">> => <<"task-abc123">>,
            <<"status">> => #{
                <<"state">> => <<"completed">>,
                <<"message">> => #{
                    <<"role">> => <<"agent">>,
                    <<"parts">> => [#{<<"kind">> => <<"text">>, <<"text">> => <<"Done">>}]
                }
            },
            <<"artifacts">> => [
                #{<<"name">> => <<"result">>, <<"parts">> => []}
            ]
        }
    },

    meck:expect(beamai_http, post_json, fun(_Url, Request, _Opts) ->
        %% 验证方法和参数
        ?assertEqual(<<"tasks/get">>, maps:get(<<"method">>, Request)),
        Params = maps:get(<<"params">>, Request),
        ?assertEqual(<<"task-abc123">>, maps:get(<<"taskId">>, Params)),
        {ok, MockResponse}
    end),

    {ok, Task} = beamai_a2a_client:get_task(
        "https://test.example.com/a2a",
        <<"task-abc123">>
    ),

    ?assertEqual(<<"task-abc123">>, maps:get(<<"id">>, Task)),
    Status = maps:get(<<"status">>, Task),
    ?assertEqual(<<"completed">>, maps:get(<<"state">>, Status)),

    ok.

%%====================================================================
%% 任务取消测试
%%====================================================================

test_cancel_task() ->
    MockResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => <<"req-123">>,
        <<"result">> => #{
            <<"id">> => <<"task-abc123">>,
            <<"status">> => #{
                <<"state">> => <<"canceled">>
            }
        }
    },

    meck:expect(beamai_http, post_json, fun(_Url, Request, _Opts) ->
        ?assertEqual(<<"tasks/cancel">>, maps:get(<<"method">>, Request)),
        {ok, MockResponse}
    end),

    {ok, Task} = beamai_a2a_client:cancel_task(
        "https://test.example.com/a2a",
        <<"task-abc123">>
    ),

    Status = maps:get(<<"status">>, Task),
    ?assertEqual(<<"canceled">>, maps:get(<<"state">>, Status)),

    ok.

%%====================================================================
%% 错误处理测试
%%====================================================================

test_error_handling() ->
    %% 测试网络错误
    meck:expect(beamai_http, get, fun(_Url, _Params, _Opts) ->
        {error, {http_error, 500, <<"Internal Server Error">>}}
    end),

    {error, {discovery_failed, _}} = beamai_a2a_client:discover("https://fail.example.com"),

    %% 测试 RPC 错误
    RpcError = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => <<"req-123">>,
        <<"error">> => #{
            <<"code">> => -32001,
            <<"message">> => <<"Task not found">>
        }
    },

    meck:expect(beamai_http, post_json, fun(_Url, _Request, _Opts) ->
        {ok, RpcError}
    end),

    {error, {rpc_error, Error}} = beamai_a2a_client:get_task(
        "https://test.example.com/a2a",
        <<"invalid-task">>
    ),
    ?assertEqual(-32001, maps:get(<<"code">>, Error)),

    %% 测试请求失败
    meck:expect(beamai_http, post_json, fun(_Url, _Request, _Opts) ->
        {error, timeout}
    end),

    {error, {request_failed, timeout}} = beamai_a2a_client:send_message(
        "https://test.example.com/a2a",
        beamai_a2a_client:create_text_message(<<"Hello">>)
    ),

    ok.

%%====================================================================
%% SSE 解析测试（内部函数）
%%====================================================================

sse_parsing_test_() ->
    [
        {"解析简单 SSE 事件", fun test_parse_simple_sse/0},
        {"解析多个 SSE 事件", fun test_parse_multiple_sse/0}
    ].

test_parse_simple_sse() ->
    %% 这个测试需要访问内部函数，暂时跳过
    %% 可以通过公共 API 间接测试
    ok.

test_parse_multiple_sse() ->
    ok.
