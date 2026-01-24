%%%-------------------------------------------------------------------
%%% @doc JSON-RPC 2.0 通用工具模块
%%%
%%% 提供标准 JSON-RPC 2.0 协议的编解码和类型检查功能。
%%% 被 beamai_a2a_jsonrpc 和 beamai_mcp_jsonrpc 共同使用。
%%%
%%% == 编码函数 ==
%%% 将 Erlang 数据结构编码为 JSON binary。
%%%
%%% == 解码函数 ==
%%% 将 JSON binary 解码为 Erlang map。
%%%
%%% == 错误构造函数 ==
%%% 返回 JSON-RPC 错误响应 map（不编码为 binary）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_jsonrpc).

%% 编码 API
-export([
    encode_request/3,
    encode_notification/2,
    encode_response/2,
    encode_error/3,
    encode_error/4
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

%% 标准错误构造（返回 map）
-export([
    parse_error/1,
    invalid_request/1,
    method_not_found/2,
    invalid_params/2,
    internal_error/1,
    custom_error/4
]).

%%====================================================================
%% 编码 API
%%====================================================================

%% @doc 编码 JSON-RPC 请求
-spec encode_request(term(), binary(), map()) -> binary().
encode_request(Id, Method, Params) ->
    Msg = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => Method,
        <<"params">> => Params
    },
    jsx:encode(Msg, []).

%% @doc 编码 JSON-RPC 通知（无 id 字段）
-spec encode_notification(binary(), map()) -> binary().
encode_notification(Method, Params) ->
    Msg = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params
    },
    jsx:encode(Msg, []).

%% @doc 编码 JSON-RPC 成功响应
-spec encode_response(term(), term()) -> binary().
encode_response(Id, Result) ->
    Msg = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    },
    jsx:encode(Msg, []).

%% @doc 编码 JSON-RPC 错误响应
-spec encode_error(term(), integer(), binary()) -> binary().
encode_error(Id, Code, Message) ->
    Msg = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message
        }
    },
    jsx:encode(Msg, []).

%% @doc 编码 JSON-RPC 错误响应（带额外数据）
-spec encode_error(term(), integer(), binary(), term()) -> binary().
encode_error(Id, Code, Message, Data) ->
    Error = case Data of
        null -> #{<<"code">> => Code, <<"message">> => Message};
        undefined -> #{<<"code">> => Code, <<"message">> => Message};
        _ -> #{<<"code">> => Code, <<"message">> => Message, <<"data">> => Data}
    end,
    Msg = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => Error
    },
    jsx:encode(Msg, []).

%%====================================================================
%% 解码 API
%%====================================================================

%% @doc 解码 JSON-RPC 消息
%%
%% 支持单个消息和批处理。
%% 返回解码后的 map 或批处理列表。
-spec decode(binary()) -> {ok, map() | {batch, [map()]}} | {error, term()}.
decode(JsonBin) ->
    try
        case jsx:decode(JsonBin, [return_maps]) of
            Msgs when is_list(Msgs) ->
                {ok, {batch, Msgs}};
            Msg when is_map(Msg) ->
                {ok, Msg};
            _ ->
                {error, invalid_json}
        end
    catch
        _:_ ->
            {error, parse_error}
    end.

%% @doc 解码 JSON-RPC 请求，提取 id, method, params
-spec decode_request(binary()) ->
    {ok, {term(), binary(), map()}} | {error, term()}.
decode_request(JsonBin) ->
    case decode(JsonBin) of
        {ok, #{<<"method">> := Method} = Msg} ->
            Id = maps:get(<<"id">>, Msg, null),
            Params = maps:get(<<"params">>, Msg, #{}),
            {ok, {Id, Method, Params}};
        {ok, _} ->
            {error, not_a_request};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 类型检查
%%====================================================================

%% @doc 检查是否为 JSON-RPC 请求（有 method 和 id）
-spec is_request(map()) -> boolean().
is_request(#{<<"method">> := _, <<"id">> := _}) -> true;
is_request(_) -> false.

%% @doc 检查是否为 JSON-RPC 通知（有 method，无 id）
-spec is_notification(map()) -> boolean().
is_notification(Msg) when is_map(Msg) ->
    maps:is_key(<<"method">>, Msg) andalso not maps:is_key(<<"id">>, Msg);
is_notification(_) -> false.

%% @doc 检查是否为成功响应（有 result）
-spec is_response(map()) -> boolean().
is_response(#{<<"result">> := _, <<"id">> := _}) -> true;
is_response(_) -> false.

%% @doc 检查是否为错误响应（有 error）
-spec is_error(map()) -> boolean().
is_error(#{<<"error">> := _, <<"id">> := _}) -> true;
is_error(_) -> false.

%% @doc 检查是否为批处理
-spec is_batch(term()) -> boolean().
is_batch({batch, _}) -> true;
is_batch(Msgs) when is_list(Msgs) -> true;
is_batch(_) -> false.

%%====================================================================
%% 标准错误构造（返回 map，不编码为 binary）
%%====================================================================

%% @doc 解析错误 (-32700)
-spec parse_error(term()) -> map().
parse_error(Id) ->
    error_response(Id, -32700, <<"Parse error">>).

%% @doc 无效请求 (-32600)
-spec invalid_request(term()) -> map().
invalid_request(Id) ->
    error_response(Id, -32600, <<"Invalid Request">>).

%% @doc 方法未找到 (-32601)
-spec method_not_found(term(), binary()) -> map().
method_not_found(Id, Method) ->
    error_response_with_data(Id, -32601, <<"Method not found">>,
                             #{<<"method">> => Method}).

%% @doc 无效参数 (-32602)
-spec invalid_params(term(), binary()) -> map().
invalid_params(Id, Details) ->
    error_response_with_data(Id, -32602, <<"Invalid params">>,
                             #{<<"details">> => Details}).

%% @doc 内部错误 (-32603)
-spec internal_error(term()) -> map().
internal_error(Id) ->
    error_response(Id, -32603, <<"Internal error">>).

%% @doc 自定义错误
-spec custom_error(term(), integer(), binary(), term()) -> map().
custom_error(Id, Code, Message, null) ->
    error_response(Id, Code, Message);
custom_error(Id, Code, Message, Data) ->
    error_response_with_data(Id, Code, Message, Data).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 构造错误响应 map
-spec error_response(term(), integer(), binary()) -> map().
error_response(Id, Code, Message) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message
        }
    }.

%% @private 构造带数据的错误响应 map
-spec error_response_with_data(term(), integer(), binary(), term()) -> map().
error_response_with_data(Id, Code, Message, Data) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message,
            <<"data">> => Data
        }
    }.
