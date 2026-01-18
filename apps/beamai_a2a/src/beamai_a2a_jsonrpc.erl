%%%-------------------------------------------------------------------
%%% @doc A2A JSON-RPC 模块
%%%
%%% 为 A2A 协议提供 JSON-RPC 2.0 支持。
%%% 基于 beamai_jsonrpc 通用模块，添加 A2A 特定的错误类型。
%%%
%%% == 通用功能 ==
%%%
%%% 编解码功能委托给 beamai_jsonrpc 模块：
%%% - encode_request/3, encode_notification/2, encode_response/2
%%% - encode_error/3, encode_error/4
%%% - decode/1, decode_request/1
%%% - is_request/1, is_notification/1, is_response/1, is_error/1
%%%
%%% == A2A 特定错误 ==
%%%
%%% A2A 定义的自定义错误码（-32000 到 -32099）：
%%% - task_not_found (-32001)
%%% - task_already_completed (-32002)
%%% - invalid_state_transition (-32003)
%%% - authentication_required (-32004)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_jsonrpc).

%% A2A 自定义错误码 (从 -32000 到 -32099)
-define(TASK_NOT_FOUND, -32001).
-define(TASK_ALREADY_COMPLETED, -32002).
-define(INVALID_STATE_TRANSITION, -32003).
-define(AUTHENTICATION_REQUIRED, -32004).

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
%% API 导出 - A2A 特定
%%====================================================================

%% A2A 特定错误构造函数
-export([
    task_not_found/2,
    task_already_completed/2,
    invalid_state_transition/3,
    authentication_required/1
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
%%====================================================================

%% @doc 构造解析错误
-spec parse_error(term()) -> binary().
parse_error(Id) ->
    jsx:encode(beamai_jsonrpc:parse_error(Id), []).

%% @doc 构造无效请求错误
-spec invalid_request(term()) -> binary().
invalid_request(Id) ->
    jsx:encode(beamai_jsonrpc:invalid_request(Id), []).

%% @doc 构造方法未找到错误
-spec method_not_found(term(), binary()) -> binary().
method_not_found(Id, Method) ->
    jsx:encode(beamai_jsonrpc:method_not_found(Id, Method), []).

%% @doc 构造无效参数错误
-spec invalid_params(term(), binary()) -> binary().
invalid_params(Id, Details) ->
    jsx:encode(beamai_jsonrpc:invalid_params(Id, Details), []).

%% @doc 构造内部错误
-spec internal_error(term()) -> binary().
internal_error(Id) ->
    jsx:encode(beamai_jsonrpc:internal_error(Id), []).

%%====================================================================
%% A2A 特定错误构造函数（返回编码后的 binary）
%%====================================================================

%% @doc 构造任务未找到错误
%%
%% 错误码: -32001
%%
%% @param Id 请求 ID
%% @param TaskId 未找到的任务 ID
%% @returns JSON 编码的错误响应
-spec task_not_found(term(), binary()) -> binary().
task_not_found(Id, TaskId) ->
    jsx:encode(beamai_jsonrpc:custom_error(Id, ?TASK_NOT_FOUND, <<"Task not found">>,
                                          #{<<"taskId">> => TaskId}), []).

%% @doc 构造任务已完成错误
%%
%% 错误码: -32002
%%
%% @param Id 请求 ID
%% @param TaskId 已完成的任务 ID
%% @returns JSON 编码的错误响应
-spec task_already_completed(term(), binary()) -> binary().
task_already_completed(Id, TaskId) ->
    jsx:encode(beamai_jsonrpc:custom_error(Id, ?TASK_ALREADY_COMPLETED, <<"Task already completed">>,
                                          #{<<"taskId">> => TaskId}), []).

%% @doc 构造无效状态转换错误
%%
%% 错误码: -32003
%%
%% @param Id 请求 ID
%% @param FromState 原状态
%% @param ToState 目标状态
%% @returns JSON 编码的错误响应
-spec invalid_state_transition(term(), atom(), atom()) -> binary().
invalid_state_transition(Id, FromState, ToState) ->
    jsx:encode(beamai_jsonrpc:custom_error(Id, ?INVALID_STATE_TRANSITION, <<"Invalid state transition">>,
                                          #{
                                              <<"from">> => beamai_a2a_types:task_state_to_binary(FromState),
                                              <<"to">> => beamai_a2a_types:task_state_to_binary(ToState)
                                          }), []).

%% @doc 构造需要认证错误
%%
%% 错误码: -32004
%%
%% @param Id 请求 ID
%% @returns JSON 编码的错误响应
-spec authentication_required(term()) -> binary().
authentication_required(Id) ->
    jsx:encode(beamai_jsonrpc:custom_error(Id, ?AUTHENTICATION_REQUIRED, <<"Authentication required">>, null), []).
