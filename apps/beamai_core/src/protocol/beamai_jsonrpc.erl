%%%-------------------------------------------------------------------
%%% @doc JSON-RPC 2.0 通用编解码模块
%%%
%%% 实现 JSON-RPC 2.0 协议的编码和解码功能。
%%% 此模块为通用实现，可被 A2A、MCP 等协议复用。
%%%
%%% == JSON-RPC 2.0 规范 ==
%%%
%%% 请求格式：
%%% ```json
%%% {
%%%     "jsonrpc": "2.0",
%%%     "id": "request-id",
%%%     "method": "method/name",
%%%     "params": { ... }
%%% }
%%% ```
%%%
%%% 响应格式（成功）：
%%% ```json
%%% {
%%%     "jsonrpc": "2.0",
%%%     "id": "request-id",
%%%     "result": { ... }
%%% }
%%% ```
%%%
%%% 响应格式（错误）：
%%% ```json
%%% {
%%%     "jsonrpc": "2.0",
%%%     "id": "request-id",
%%%     "error": {
%%%         "code": -32600,
%%%         "message": "Invalid Request",
%%%         "data": { ... }
%%%     }
%%% }
%%% ```
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 编码请求
%%% ReqJson = beamai_jsonrpc:encode_request(1, <<"method/name">>, #{key => value}).
%%%
%%% %% 解码响应
%%% {ok, Response} = beamai_jsonrpc:decode(JsonBin).
%%%
%%% %% 构建响应（不编码）
%%% RespMap = beamai_jsonrpc:response(1, #{result => <<"ok">>}).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_jsonrpc).

%% JSON-RPC 2.0 版本
-define(JSONRPC_VERSION, <<"2.0">>).

%% 标准错误码
-define(PARSE_ERROR, -32700).
-define(INVALID_REQUEST, -32600).
-define(METHOD_NOT_FOUND, -32601).
-define(INVALID_PARAMS, -32602).
-define(INTERNAL_ERROR, -32603).

%%====================================================================
%% API 导出
%%====================================================================

%% 编码 API（返回 binary）
-export([
    encode_request/3,
    encode_notification/2,
    encode_response/2,
    encode_error/3,
    encode_error/4
]).

%% 消息构建 API（返回 map）
-export([
    request/3,
    notification/2,
    response/2,
    error_response/3,
    error_response/4
]).

%% 解码 API
-export([
    decode/1,
    decode_request/1
]).

%% 类型检查
-export([
    is_request/1,
    is_notification/1,
    is_response/1,
    is_error/1,
    is_batch/1
]).

%% 标准错误响应
-export([
    parse_error/1,
    invalid_request/1,
    method_not_found/2,
    invalid_params/2,
    internal_error/1
]).

%% 自定义错误响应
-export([
    custom_error/4,
    custom_error/5
]).

%% ID 生成
-export([
    generate_id/0
]).

%% 常量导出
-export([
    version/0,
    error_code_parse_error/0,
    error_code_invalid_request/0,
    error_code_method_not_found/0,
    error_code_invalid_params/0,
    error_code_internal_error/0
]).

%%====================================================================
%% 常量
%%====================================================================

%% @doc 获取 JSON-RPC 版本
-spec version() -> binary().
version() -> ?JSONRPC_VERSION.

%% @doc 标准错误码
error_code_parse_error() -> ?PARSE_ERROR.
error_code_invalid_request() -> ?INVALID_REQUEST.
error_code_method_not_found() -> ?METHOD_NOT_FOUND.
error_code_invalid_params() -> ?INVALID_PARAMS.
error_code_internal_error() -> ?INTERNAL_ERROR.

%%====================================================================
%% 编码 API
%%====================================================================

%% @doc 编码 JSON-RPC 请求
%%
%% @param Id 请求 ID（可以是 binary、integer 或 null）
%% @param Method 方法名
%% @param Params 参数 map
%% @returns JSON 二进制字符串
-spec encode_request(term(), binary(), map()) -> binary().
encode_request(Id, Method, Params) ->
    jsx:encode(request(Id, Method, Params), []).

%% @doc 编码 JSON-RPC 通知（无 id 的请求）
%%
%% 通知不需要响应。
%%
%% @param Method 方法名
%% @param Params 参数 map
%% @returns JSON 二进制字符串
-spec encode_notification(binary(), map()) -> binary().
encode_notification(Method, Params) ->
    jsx:encode(notification(Method, Params), []).

%% @doc 编码 JSON-RPC 成功响应
%%
%% @param Id 请求 ID
%% @param Result 结果数据
%% @returns JSON 二进制字符串
-spec encode_response(term(), term()) -> binary().
encode_response(Id, Result) ->
    jsx:encode(response(Id, Result), []).

%% @doc 编码 JSON-RPC 错误响应
%%
%% @param Id 请求 ID
%% @param Code 错误码
%% @param Message 错误消息
%% @returns JSON 二进制字符串
-spec encode_error(term(), integer(), binary()) -> binary().
encode_error(Id, Code, Message) ->
    jsx:encode(error_response(Id, Code, Message), []).

%% @doc 编码 JSON-RPC 错误响应（带额外数据）
%%
%% @param Id 请求 ID
%% @param Code 错误码
%% @param Message 错误消息
%% @param Data 额外错误数据
%% @returns JSON 二进制字符串
-spec encode_error(term(), integer(), binary(), term()) -> binary().
encode_error(Id, Code, Message, Data) ->
    jsx:encode(error_response(Id, Code, Message, Data), []).

%%====================================================================
%% 消息构建 API（返回 map）
%%====================================================================

%% @doc 构建 JSON-RPC 请求 map
%%
%% @param Id 请求 ID
%% @param Method 方法名
%% @param Params 参数 map
%% @returns 请求 map
-spec request(term(), binary(), map()) -> map().
request(Id, Method, Params) ->
    #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"id">> => Id,
        <<"method">> => Method,
        <<"params">> => Params
    }.

%% @doc 构建 JSON-RPC 通知 map
%%
%% @param Method 方法名
%% @param Params 参数 map
%% @returns 通知 map
-spec notification(binary(), map()) -> map().
notification(Method, Params) ->
    #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"method">> => Method,
        <<"params">> => Params
    }.

%% @doc 构建 JSON-RPC 成功响应 map
%%
%% @param Id 请求 ID
%% @param Result 结果数据
%% @returns 响应 map
-spec response(term(), term()) -> map().
response(Id, Result) ->
    #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"id">> => Id,
        <<"result">> => Result
    }.

%% @doc 构建 JSON-RPC 错误响应 map
%%
%% @param Id 请求 ID
%% @param Code 错误码
%% @param Message 错误消息
%% @returns 错误响应 map
-spec error_response(term(), integer(), binary()) -> map().
error_response(Id, Code, Message) ->
    #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message
        }
    }.

%% @doc 构建 JSON-RPC 错误响应 map（带额外数据）
%%
%% @param Id 请求 ID
%% @param Code 错误码
%% @param Message 错误消息
%% @param Data 额外错误数据
%% @returns 错误响应 map
-spec error_response(term(), integer(), binary(), term()) -> map().
error_response(Id, Code, Message, null) ->
    error_response(Id, Code, Message);
error_response(Id, Code, Message, Data) ->
    #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message,
            <<"data">> => Data
        }
    }.

%%====================================================================
%% 解码 API
%%====================================================================

%% @doc 解码 JSON-RPC 消息
%%
%% 自动检测消息类型（请求、响应、通知、批处理）。
%%
%% @param JsonBin JSON 二进制字符串
%% @returns {ok, DecodedMessage} | {error, Reason}
-spec decode(binary()) -> {ok, map() | {batch, [map()]}} | {error, term()}.
decode(JsonBin) ->
    try
        case jsx:decode(JsonBin, [return_maps]) of
            List when is_list(List) ->
                %% 批处理请求
                {ok, {batch, [validate_message(M) || M <- List]}};
            Map when is_map(Map) ->
                {ok, validate_message(Map)};
            _ ->
                {error, invalid_json}
        end
    catch
        error:badarg ->
            {error, parse_error};
        _:Reason ->
            {error, {decode_error, Reason}}
    end.

%% @doc 解码 JSON-RPC 请求
%%
%% 专门用于解码请求，提取方法和参数。
%%
%% @param JsonBin JSON 二进制字符串
%% @returns {ok, {Id, Method, Params}} | {error, Reason}
-spec decode_request(binary()) -> {ok, {term(), binary(), map()}} | {error, term()}.
decode_request(JsonBin) ->
    case decode(JsonBin) of
        {ok, #{<<"method">> := Method, <<"params">> := Params} = Msg} ->
            Id = maps:get(<<"id">>, Msg, null),
            {ok, {Id, Method, Params}};
        {ok, #{<<"method">> := Method} = Msg} ->
            Id = maps:get(<<"id">>, Msg, null),
            {ok, {Id, Method, #{}}};
        {ok, _} ->
            {error, not_a_request};
        Error ->
            Error
    end.

%%====================================================================
%% 类型检查
%%====================================================================

%% @doc 检查是否为请求（有 id 和 method）
-spec is_request(map()) -> boolean().
is_request(Msg) when is_map(Msg) ->
    maps:is_key(<<"method">>, Msg) andalso maps:is_key(<<"id">>, Msg);
is_request(_) ->
    false.

%% @doc 检查是否为通知（有 method 但无 id）
-spec is_notification(map()) -> boolean().
is_notification(Msg) when is_map(Msg) ->
    maps:is_key(<<"method">>, Msg) andalso not maps:is_key(<<"id">>, Msg);
is_notification(_) ->
    false.

%% @doc 检查是否为成功响应
-spec is_response(map()) -> boolean().
is_response(Msg) when is_map(Msg) ->
    maps:is_key(<<"result">>, Msg) andalso maps:is_key(<<"id">>, Msg);
is_response(_) ->
    false.

%% @doc 检查是否为错误响应
-spec is_error(map()) -> boolean().
is_error(Msg) when is_map(Msg) ->
    maps:is_key(<<"error">>, Msg) andalso maps:is_key(<<"id">>, Msg);
is_error(_) ->
    false.

%% @doc 检查是否为批处理
-spec is_batch(term()) -> boolean().
is_batch({batch, _}) -> true;
is_batch(_) -> false.

%%====================================================================
%% 标准错误响应
%%====================================================================

%% @doc 构造解析错误响应 map
-spec parse_error(term()) -> map().
parse_error(Id) ->
    error_response(Id, ?PARSE_ERROR, <<"Parse error">>).

%% @doc 构造无效请求错误响应 map
-spec invalid_request(term()) -> map().
invalid_request(Id) ->
    error_response(Id, ?INVALID_REQUEST, <<"Invalid Request">>).

%% @doc 构造方法未找到错误响应 map
-spec method_not_found(term(), binary()) -> map().
method_not_found(Id, Method) ->
    error_response(Id, ?METHOD_NOT_FOUND, <<"Method not found">>, #{<<"method">> => Method}).

%% @doc 构造无效参数错误响应 map
-spec invalid_params(term(), binary()) -> map().
invalid_params(Id, Details) ->
    error_response(Id, ?INVALID_PARAMS, <<"Invalid params">>, #{<<"details">> => Details}).

%% @doc 构造内部错误响应 map
-spec internal_error(term()) -> map().
internal_error(Id) ->
    error_response(Id, ?INTERNAL_ERROR, <<"Internal error">>).

%%====================================================================
%% 自定义错误响应
%%====================================================================

%% @doc 构造自定义错误响应 map
%%
%% 用于协议特定的错误码（如 A2A 的 task_not_found）
%%
%% @param Id 请求 ID
%% @param Code 自定义错误码（建议使用 -32000 到 -32099）
%% @param Message 错误消息
%% @param Data 额外数据
%% @returns 错误响应 map
-spec custom_error(term(), integer(), binary(), term()) -> map().
custom_error(Id, Code, Message, Data) ->
    error_response(Id, Code, Message, Data).

%% @doc 构造自定义错误响应并编码为 JSON
-spec custom_error(term(), integer(), binary(), term(), encode) -> binary().
custom_error(Id, Code, Message, Data, encode) ->
    jsx:encode(custom_error(Id, Code, Message, Data), []).

%%====================================================================
%% ID 生成
%%====================================================================

%% @doc 生成唯一的请求 ID
%%
%% 格式: req-<timestamp_hex>-<random_hex>
%%
%% @returns 请求 ID 二进制字符串
-spec generate_id() -> binary().
generate_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFF),
    iolist_to_binary(io_lib:format("req-~.16b-~.4b", [Timestamp, Random])).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 验证消息格式
-spec validate_message(term()) -> map().
validate_message(#{<<"jsonrpc">> := <<"2.0">>} = Msg) ->
    Msg;
validate_message(Msg) when is_map(Msg) ->
    %% 允许没有 jsonrpc 字段的消息（宽松模式）
    Msg#{<<"jsonrpc">> => <<"2.0">>};
validate_message(_) ->
    #{<<"jsonrpc">> => <<"2.0">>, <<"error">> => <<"invalid_message">>}.
