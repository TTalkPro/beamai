%%%-------------------------------------------------------------------
%%% @doc beamai_mcp_jsonrpc 单元测试
%%%-------------------------------------------------------------------
-module(beamai_mcp_jsonrpc_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamai_mcp.hrl").

%%====================================================================
%% 测试组
%%====================================================================

beamai_mcp_jsonrpc_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"编码请求测试", fun encode_request_test/0},
      {"编码通知测试", fun encode_notification_test/0},
      {"编码响应测试", fun encode_response_test/0},
      {"编码错误测试", fun encode_error_test/0},
      {"解码测试", fun decode_test/0},
      {"解码请求测试", fun decode_request_test/0},
      {"类型检查测试", fun type_check_test/0},
      {"标准错误测试", fun standard_errors_test/0},
      {"MCP 特定错误测试", fun mcp_errors_test/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% 编码测试
%%====================================================================

encode_request_test() ->
    Encoded = beamai_mcp_jsonrpc:encode_request(1, <<"test/method">>, #{<<"key">> => <<"value">>}),
    ?assert(is_binary(Encoded)),

    {ok, Decoded} = beamai_mcp_jsonrpc:decode(Encoded),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(1, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"test/method">>, maps:get(<<"method">>, Decoded)),
    ?assertEqual(#{<<"key">> => <<"value">>}, maps:get(<<"params">>, Decoded)).

encode_notification_test() ->
    Encoded = beamai_mcp_jsonrpc:encode_notification(<<"notifications/test">>, #{<<"data">> => 123}),
    ?assert(is_binary(Encoded)),

    {ok, Decoded} = beamai_mcp_jsonrpc:decode(Encoded),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(false, maps:is_key(<<"id">>, Decoded)),
    ?assertEqual(<<"notifications/test">>, maps:get(<<"method">>, Decoded)).

encode_response_test() ->
    Encoded = beamai_mcp_jsonrpc:encode_response(1, #{<<"result">> => <<"success">>}),
    ?assert(is_binary(Encoded)),

    {ok, Decoded} = beamai_mcp_jsonrpc:decode(Encoded),
    ?assertEqual(1, maps:get(<<"id">>, Decoded)),
    ?assertEqual(#{<<"result">> => <<"success">>}, maps:get(<<"result">>, Decoded)).

encode_error_test() ->
    %% 基本错误
    Encoded1 = beamai_mcp_jsonrpc:encode_error(1, -32600, <<"Invalid Request">>),
    {ok, Decoded1} = beamai_mcp_jsonrpc:decode(Encoded1),
    Error1 = maps:get(<<"error">>, Decoded1),
    ?assertEqual(-32600, maps:get(<<"code">>, Error1)),
    ?assertEqual(<<"Invalid Request">>, maps:get(<<"message">>, Error1)),

    %% 带数据的错误
    Encoded2 = beamai_mcp_jsonrpc:encode_error(2, -32602, <<"Invalid params">>, #{<<"field">> => <<"name">>}),
    {ok, Decoded2} = beamai_mcp_jsonrpc:decode(Encoded2),
    Error2 = maps:get(<<"error">>, Decoded2),
    ?assertEqual(#{<<"field">> => <<"name">>}, maps:get(<<"data">>, Error2)).

%%====================================================================
%% 解码测试
%%====================================================================

decode_test() ->
    %% 有效 JSON
    {ok, _} = beamai_mcp_jsonrpc:decode(<<"{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}">>),

    %% 无效 JSON
    {error, parse_error} = beamai_mcp_jsonrpc:decode(<<"not json">>),

    %% 批处理
    BatchJson = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"},{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"test2\"}]">>,
    {ok, {batch, Batch}} = beamai_mcp_jsonrpc:decode(BatchJson),
    ?assertEqual(2, length(Batch)).

decode_request_test() ->
    %% 带参数的请求
    Json1 = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"test\"}}">>,
    {ok, {1, <<"tools/call">>, #{<<"name">> := <<"test">>}}} = beamai_mcp_jsonrpc:decode_request(Json1),

    %% 不带参数的请求
    Json2 = <<"{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}">>,
    {ok, {2, <<"ping">>, #{}}} = beamai_mcp_jsonrpc:decode_request(Json2),

    %% 通知（无 id）
    Json3 = <<"{\"jsonrpc\":\"2.0\",\"method\":\"notify\",\"params\":{}}">>,
    {ok, {null, <<"notify">>, #{}}} = beamai_mcp_jsonrpc:decode_request(Json3).

%%====================================================================
%% 类型检查测试
%%====================================================================

type_check_test() ->
    %% 请求
    Request = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test">>},
    ?assertEqual(true, beamai_mcp_jsonrpc:is_request(Request)),
    ?assertEqual(false, beamai_mcp_jsonrpc:is_notification(Request)),
    ?assertEqual(false, beamai_mcp_jsonrpc:is_response(Request)),
    ?assertEqual(false, beamai_mcp_jsonrpc:is_error(Request)),

    %% 通知
    Notification = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"notify">>},
    ?assertEqual(false, beamai_mcp_jsonrpc:is_request(Notification)),
    ?assertEqual(true, beamai_mcp_jsonrpc:is_notification(Notification)),

    %% 响应
    Response = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => #{}},
    ?assertEqual(true, beamai_mcp_jsonrpc:is_response(Response)),
    ?assertEqual(false, beamai_mcp_jsonrpc:is_error(Response)),

    %% 错误
    Error = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"error">> => #{<<"code">> => -32600}},
    ?assertEqual(false, beamai_mcp_jsonrpc:is_response(Error)),
    ?assertEqual(true, beamai_mcp_jsonrpc:is_error(Error)),

    %% 批处理
    ?assertEqual(true, beamai_mcp_jsonrpc:is_batch({batch, []})),
    ?assertEqual(false, beamai_mcp_jsonrpc:is_batch(#{})).

%%====================================================================
%% 标准错误测试
%%====================================================================

standard_errors_test() ->
    %% 解析错误
    ParseError = beamai_mcp_jsonrpc:parse_error(null),
    ?assert(is_binary(ParseError)),
    {ok, ParseDecoded} = beamai_mcp_jsonrpc:decode(ParseError),
    ?assertEqual(?MCP_ERROR_PARSE, maps:get(<<"code">>, maps:get(<<"error">>, ParseDecoded))),

    %% 无效请求
    InvalidReq = beamai_mcp_jsonrpc:invalid_request(1),
    {ok, InvalidDecoded} = beamai_mcp_jsonrpc:decode(InvalidReq),
    ?assertEqual(?MCP_ERROR_INVALID_REQUEST, maps:get(<<"code">>, maps:get(<<"error">>, InvalidDecoded))),

    %% 方法未找到
    MethodNotFound = beamai_mcp_jsonrpc:method_not_found(1, <<"unknown/method">>),
    {ok, MnfDecoded} = beamai_mcp_jsonrpc:decode(MethodNotFound),
    ?assertEqual(?MCP_ERROR_METHOD_NOT_FOUND, maps:get(<<"code">>, maps:get(<<"error">>, MnfDecoded))),

    %% 无效参数
    InvalidParams = beamai_mcp_jsonrpc:invalid_params(1, <<"Missing required field">>),
    {ok, IpDecoded} = beamai_mcp_jsonrpc:decode(InvalidParams),
    ?assertEqual(?MCP_ERROR_INVALID_PARAMS, maps:get(<<"code">>, maps:get(<<"error">>, IpDecoded))),

    %% 内部错误
    InternalError = beamai_mcp_jsonrpc:internal_error(1),
    {ok, IeDecoded} = beamai_mcp_jsonrpc:decode(InternalError),
    ?assertEqual(?MCP_ERROR_INTERNAL, maps:get(<<"code">>, maps:get(<<"error">>, IeDecoded))).

%%====================================================================
%% MCP 特定错误测试
%%====================================================================

mcp_errors_test() ->
    %% 资源未找到
    ResNotFound = beamai_mcp_jsonrpc:resource_not_found(1, <<"file:///not/exist">>),
    {ok, RnfDecoded} = beamai_mcp_jsonrpc:decode(ResNotFound),
    Error1 = maps:get(<<"error">>, RnfDecoded),
    ?assertEqual(?MCP_ERROR_RESOURCE_NOT_FOUND, maps:get(<<"code">>, Error1)),
    ?assertEqual(<<"file:///not/exist">>, maps:get(<<"uri">>, maps:get(<<"data">>, Error1))),

    %% 工具未找到
    ToolNotFound = beamai_mcp_jsonrpc:tool_not_found(2, <<"unknown_tool">>),
    {ok, TnfDecoded} = beamai_mcp_jsonrpc:decode(ToolNotFound),
    Error2 = maps:get(<<"error">>, TnfDecoded),
    ?assertEqual(?MCP_ERROR_TOOL_NOT_FOUND, maps:get(<<"code">>, Error2)),
    ?assertEqual(<<"unknown_tool">>, maps:get(<<"tool">>, maps:get(<<"data">>, Error2))),

    %% 提示未找到
    PromptNotFound = beamai_mcp_jsonrpc:prompt_not_found(3, <<"unknown_prompt">>),
    {ok, PnfDecoded} = beamai_mcp_jsonrpc:decode(PromptNotFound),
    Error3 = maps:get(<<"error">>, PnfDecoded),
    ?assertEqual(?MCP_ERROR_PROMPT_NOT_FOUND, maps:get(<<"code">>, Error3)),
    ?assertEqual(<<"unknown_prompt">>, maps:get(<<"prompt">>, maps:get(<<"data">>, Error3))).
