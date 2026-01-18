%%%-------------------------------------------------------------------
%%% @doc beamai_mcp_handler 单元测试
%%%-------------------------------------------------------------------
-module(beamai_mcp_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamai_mcp.hrl").

%%====================================================================
%% 测试组
%%====================================================================

beamai_mcp_handler_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_init/1,
      fun test_handle_post_json/1,
      fun test_handle_post_sse/1,
      fun test_handle_sse_init/1,
      fun test_handle_sse_message/1,
      fun test_format_sse_response/1,
      fun test_close/1,
      fun test_parse_error/1
     ]}.

setup() ->
    %% 创建测试工具
    EchoTool = #mcp_tool{
        name = <<"echo">>,
        description = <<"Echo the input">>,
        input_schema = #{},
        handler = fun(#{<<"text">> := Text}) -> {ok, Text} end
    },

    Config = #{
        tools => [EchoTool],
        resources => [],
        prompts => [],
        server_info => #{<<"name">> => <<"test-handler">>, <<"version">> => <<"1.0">>},
        sse_endpoint => <<"http://localhost:8080/mcp/sse/message">>
    },

    State = beamai_mcp_handler:init(Config),
    {State, Config}.

cleanup({State, _Config}) ->
    beamai_mcp_handler:close(State).

%%====================================================================
%% 初始化测试
%%====================================================================

test_init({State, _Config}) ->
    {"初始化测试", fun() ->
        ?assertNotEqual(undefined, State),
        %% 初始状态没有 session_id
        ?assertEqual(undefined, beamai_mcp_handler:get_session_id(State))
    end}.

%%====================================================================
%% POST 处理测试
%%====================================================================

test_handle_post_json({State, _Config}) ->
    {"POST JSON 响应测试", fun() ->
        %% 构造初始化请求
        InitRequest = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 1,
            <<"method">> => <<"initialize">>,
            <<"params">> => #{
                <<"protocolVersion">> => <<"2025-11-25">>,
                <<"capabilities">> => #{},
                <<"clientInfo">> => #{<<"name">> => <<"test">>}
            }
        }),

        %% 不接受 SSE 的头
        Headers = [{<<"content-type">>, <<"application/json">>}],

        {json, Response, NewState} = beamai_mcp_handler:handle_post(InitRequest, Headers, State),

        ?assert(is_binary(Response)),
        Decoded = jsx:decode(Response, [return_maps]),
        ?assertEqual(1, maps:get(<<"id">>, Decoded)),
        ?assert(maps:is_key(<<"result">>, Decoded)),

        %% 应该有 session_id
        ?assertNotEqual(undefined, beamai_mcp_handler:get_session_id(NewState))
    end}.

test_handle_post_sse({State, _Config}) ->
    {"POST SSE 响应测试", fun() ->
        %% 先初始化
        {_, _, State1} = do_initialize(State),

        %% 发送 ping 请求，接受 SSE
        PingRequest = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 2,
            <<"method">> => <<"ping">>,
            <<"params">> => #{}
        }),

        Headers = [
            {<<"content-type">>, <<"application/json">>},
            {<<"accept">>, <<"text/event-stream, application/json">>}
        ],

        {sse, Response, _NewState} = beamai_mcp_handler:handle_post(PingRequest, Headers, State1),

        %% SSE 响应应该是 iolist
        ResponseBin = iolist_to_binary(Response),
        ?assert(binary:match(ResponseBin, <<"event:">>) =/= nomatch orelse
                binary:match(ResponseBin, <<"data:">>) =/= nomatch)
    end}.

%%====================================================================
%% SSE 初始化测试
%%====================================================================

test_handle_sse_init({State, _Config}) ->
    {"SSE 初始化测试", fun() ->
        Headers = [{<<"accept">>, <<"text/event-stream">>}],

        {ok, InitialData, NewState} = beamai_mcp_handler:handle_sse_init(Headers, State),

        %% 应该返回 endpoint 事件
        InitialBin = iolist_to_binary(InitialData),
        ?assert(binary:match(InitialBin, <<"endpoint">>) =/= nomatch),

        %% 应该有 session_id
        ?assertNotEqual(undefined, beamai_mcp_handler:get_session_id(NewState))
    end}.

%%====================================================================
%% SSE 消息测试
%%====================================================================

test_handle_sse_message({State, _Config}) ->
    {"SSE 消息处理测试", fun() ->
        %% 先初始化
        {_, _, State1} = do_initialize(State),

        %% 发送工具列表请求
        Request = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 3,
            <<"method">> => <<"tools/list">>,
            <<"params">> => #{}
        }),

        Headers = [],
        {ok, Response, _NewState} = beamai_mcp_handler:handle_sse_message(Request, Headers, State1),

        %% 响应应该是 SSE 格式
        ResponseBin = iolist_to_binary(Response),
        ?assert(binary:match(ResponseBin, <<"event:">>) =/= nomatch orelse
                binary:match(ResponseBin, <<"data:">>) =/= nomatch)
    end}.

%%====================================================================
%% SSE 格式化测试
%%====================================================================

test_format_sse_response(_) ->
    {"SSE 响应格式化测试", fun() ->
        %% 测试 binary 数据
        Response1 = beamai_mcp_handler:format_sse_response(<<"message">>, <<"{\"test\":true}">>),
        Bin1 = iolist_to_binary(Response1),
        ?assert(binary:match(Bin1, <<"event: message">>) =/= nomatch),
        ?assert(binary:match(Bin1, <<"data: {\"test\":true}">>) =/= nomatch),

        %% 测试 map 数据（会被 JSON 编码）
        Response2 = beamai_mcp_handler:format_sse_response(<<"result">>, #{<<"key">> => <<"value">>}),
        Bin2 = iolist_to_binary(Response2),
        ?assert(binary:match(Bin2, <<"event: result">>) =/= nomatch)
    end}.

%%====================================================================
%% 关闭测试
%%====================================================================

test_close({State, _Config}) ->
    {"关闭测试", fun() ->
        %% 关闭应该不会出错
        ?assertEqual(ok, beamai_mcp_handler:close(State))
    end}.

%%====================================================================
%% 解析错误测试
%%====================================================================

test_parse_error({State, _Config}) ->
    {"解析错误测试", fun() ->
        %% 发送无效 JSON
        InvalidJson = <<"not valid json">>,
        Headers = [{<<"content-type">>, <<"application/json">>}],

        {json, Response, _NewState} = beamai_mcp_handler:handle_post(InvalidJson, Headers, State),

        Decoded = jsx:decode(Response, [return_maps]),
        ?assert(maps:is_key(<<"error">>, Decoded)),
        Error = maps:get(<<"error">>, Decoded),
        ?assertEqual(?MCP_ERROR_PARSE, maps:get(<<"code">>, Error))
    end}.

%%====================================================================
%% 辅助函数
%%====================================================================

do_initialize(State) ->
    InitRequest = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 0,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => <<"2025-11-25">>,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{<<"name">> => <<"test">>}
        }
    }),
    Headers = [{<<"content-type">>, <<"application/json">>}],
    beamai_mcp_handler:handle_post(InitRequest, Headers, State).
