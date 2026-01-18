%%%-------------------------------------------------------------------
%%% @doc MCP JSON-RPC 模块
%%%
%%% 为 MCP 协议提供 JSON-RPC 2.0 支持。
%%% 基于 beamai_jsonrpc 通用模块，添加 MCP 特定的错误类型。
%%%
%%% == 通用功能 ==
%%%
%%% 编解码功能委托给 beamai_jsonrpc 模块：
%%% - encode_request/3, encode_notification/2, encode_response/2
%%% - encode_error/3, encode_error/4
%%% - decode/1, decode_request/1
%%% - is_request/1, is_notification/1, is_response/1, is_error/1
%%%
%%% == MCP 特定错误 ==
%%%
%%% MCP 定义的自定义错误码（-32000 到 -32099）：
%%% - resource_not_found (-32002)
%%% - tool_not_found (-32003)
%%% - prompt_not_found (-32004)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_jsonrpc).

-include("beamai_mcp.hrl").

%%====================================================================
%% API 导出 - 委托给 beamai_jsonrpc
%%====================================================================

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

%% 标准错误
-export([
    parse_error/1,
    invalid_request/1,
    method_not_found/2,
    invalid_params/2,
    internal_error/1
]).

%%====================================================================
%% API 导出 - MCP 特定
%%====================================================================

%% MCP 特定错误构造函数
-export([
    resource_not_found/2,
    tool_not_found/2,
    prompt_not_found/2
]).

%%====================================================================
%% 委托实现 - 编码 API
%%====================================================================

%% @doc 编码 JSON-RPC 请求
-spec encode_request(term(), binary(), map()) -> binary().
encode_request(Id, Method, Params) ->
    beamai_jsonrpc:encode_request(Id, Method, Params).

%% @doc 编码 JSON-RPC 通知
-spec encode_notification(binary(), map()) -> binary().
encode_notification(Method, Params) ->
    beamai_jsonrpc:encode_notification(Method, Params).

%% @doc 编码 JSON-RPC 成功响应
-spec encode_response(term(), term()) -> binary().
encode_response(Id, Result) ->
    beamai_jsonrpc:encode_response(Id, Result).

%% @doc 编码 JSON-RPC 错误响应
-spec encode_error(term(), integer(), binary()) -> binary().
encode_error(Id, Code, Message) ->
    beamai_jsonrpc:encode_error(Id, Code, Message).

%% @doc 编码 JSON-RPC 错误响应（带额外数据）
-spec encode_error(term(), integer(), binary(), term()) -> binary().
encode_error(Id, Code, Message, Data) ->
    beamai_jsonrpc:encode_error(Id, Code, Message, Data).

%%====================================================================
%% 委托实现 - 解码 API
%%====================================================================

%% @doc 解码 JSON-RPC 消息
-spec decode(binary()) -> {ok, map() | {batch, [map()]}} | {error, term()}.
decode(JsonBin) ->
    beamai_jsonrpc:decode(JsonBin).

%% @doc 解码 JSON-RPC 请求
-spec decode_request(binary()) -> {ok, {term(), binary(), map()}} | {error, term()}.
decode_request(JsonBin) ->
    beamai_jsonrpc:decode_request(JsonBin).

%%====================================================================
%% 委托实现 - 类型检查
%%====================================================================

%% @doc 检查是否为请求
-spec is_request(map()) -> boolean().
is_request(Msg) ->
    beamai_jsonrpc:is_request(Msg).

%% @doc 检查是否为通知
-spec is_notification(map()) -> boolean().
is_notification(Msg) ->
    beamai_jsonrpc:is_notification(Msg).

%% @doc 检查是否为成功响应
-spec is_response(map()) -> boolean().
is_response(Msg) ->
    beamai_jsonrpc:is_response(Msg).

%% @doc 检查是否为错误响应
-spec is_error(map()) -> boolean().
is_error(Msg) ->
    beamai_jsonrpc:is_error(Msg).

%% @doc 检查是否为批处理
-spec is_batch(term()) -> boolean().
is_batch(Msg) ->
    beamai_jsonrpc:is_batch(Msg).

%%====================================================================
%% 委托实现 - 标准错误（返回编码后的 binary）
%%
%% 这些函数将 beamai_jsonrpc 返回的 map 编码为 JSON binary，
%% 方便直接作为 HTTP 响应体使用。
%%====================================================================

%% @doc 构造解析错误
%%
%% 错误码: -32700
%% 当 JSON 解析失败时使用。
-spec parse_error(term()) -> binary().
parse_error(Id) ->
    encode_map(beamai_jsonrpc:parse_error(Id)).

%% @doc 构造无效请求错误
%%
%% 错误码: -32600
%% 当请求格式不符合 JSON-RPC 规范时使用。
-spec invalid_request(term()) -> binary().
invalid_request(Id) ->
    encode_map(beamai_jsonrpc:invalid_request(Id)).

%% @doc 构造方法未找到错误
%%
%% 错误码: -32601
%% 当请求的方法不存在时使用。
-spec method_not_found(term(), binary()) -> binary().
method_not_found(Id, Method) ->
    encode_map(beamai_jsonrpc:method_not_found(Id, Method)).

%% @doc 构造无效参数错误
%%
%% 错误码: -32602
%% 当方法参数无效时使用。
-spec invalid_params(term(), binary()) -> binary().
invalid_params(Id, Details) ->
    encode_map(beamai_jsonrpc:invalid_params(Id, Details)).

%% @doc 构造内部错误
%%
%% 错误码: -32603
%% 当发生内部错误时使用。
-spec internal_error(term()) -> binary().
internal_error(Id) ->
    encode_map(beamai_jsonrpc:internal_error(Id)).

%%====================================================================
%% MCP 特定错误构造函数（返回编码后的 binary）
%%
%% MCP 协议定义了自定义错误码范围 -32000 到 -32099。
%% 这里实现 MCP 规范中定义的特定错误类型。
%%====================================================================

%% @doc 构造资源未找到错误
%%
%% 错误码: -32002
%% 当请求的资源 URI 不存在时使用。
%%
%% @param Id 请求 ID
%% @param Uri 未找到的资源 URI
%% @returns JSON 编码的错误响应
-spec resource_not_found(term(), binary()) -> binary().
resource_not_found(Id, Uri) ->
    encode_map(beamai_jsonrpc:custom_error(Id, ?MCP_ERROR_RESOURCE_NOT_FOUND,
                                          <<"Resource not found">>,
                                          #{<<"uri">> => Uri})).

%% @doc 构造工具未找到错误
%%
%% 错误码: -32003
%% 当请求的工具名称不存在时使用。
%%
%% @param Id 请求 ID
%% @param ToolName 未找到的工具名称
%% @returns JSON 编码的错误响应
-spec tool_not_found(term(), binary()) -> binary().
tool_not_found(Id, ToolName) ->
    encode_map(beamai_jsonrpc:custom_error(Id, ?MCP_ERROR_TOOL_NOT_FOUND,
                                          <<"Tool not found">>,
                                          #{<<"tool">> => ToolName})).

%% @doc 构造提示未找到错误
%%
%% 错误码: -32004
%% 当请求的提示名称不存在时使用。
%%
%% @param Id 请求 ID
%% @param PromptName 未找到的提示名称
%% @returns JSON 编码的错误响应
-spec prompt_not_found(term(), binary()) -> binary().
prompt_not_found(Id, PromptName) ->
    encode_map(beamai_jsonrpc:custom_error(Id, ?MCP_ERROR_PROMPT_NOT_FOUND,
                                          <<"Prompt not found">>,
                                          #{<<"prompt">> => PromptName})).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 将 map 编码为 JSON binary
%%
%% 统一的 JSON 编码函数，减少重复的 jsx:encode 调用。
-spec encode_map(map()) -> binary().
encode_map(Map) ->
    jsx:encode(Map, []).
