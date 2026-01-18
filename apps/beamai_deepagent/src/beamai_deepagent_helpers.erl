%%%-------------------------------------------------------------------
%%% @doc Agent Deep 公共工具函数模块
%%%
%%% 提供跨模块复用的工具函数，避免代码重复。
%%%
%%% 核心功能：
%%% - 状态操作：简化 graph_state 操作
%%% - 错误处理：统一的错误处理模式
%%% - 结果构建：标准化的结果构建
%%% - 类型转换：安全的数据类型转换
%%%
%%% 设计原则：
%%% - 纯函数：无副作用，易于测试
%%% - 幂等性：多次调用结果一致
%%% - 可组合：支持管道式调用
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_helpers).

%% 导出公共工具函数
-export([
    state_get/2, state_get/3,
    state_set/3, state_update/3,
    state_set_many/2,
    format_error/1,
    wrap_success/1, wrap_error/1,
    is_success/1, is_error/1,
    build_tool_result/3,
    extract_response/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type state() :: graph_state:state().
-type key() :: atom().
-type value() :: term().
-type result(T) :: {ok, T} | {error, term()}.

%%====================================================================
%% 状态操作
%%====================================================================

%% @doc 获取状态值（简化 graph_state 操作）
%%
%% 提供更简洁的调用方式，避免重复的 graph_state 前缀。
-spec state_get(state(), key()) -> value() | undefined.
state_get(State, Key) ->
    graph_state:get(State, Key).

%% @doc 获取状态值（带默认值）
-spec state_get(state(), key(), value()) -> value().
state_get(State, Key, Default) ->
    graph_state:get(State, Key, Default).

%% @doc 设置状态值
%%
%% 返回新的状态（不可变操作）。
-spec state_set(state(), key(), value()) -> state().
state_set(State, Key, Value) ->
    graph_state:set(State, Key, Value).

%% @doc 更新状态值
%%
%% 使用函数更新状态值，函数接收旧值返回新值。
-spec state_update(state(), key(), fun((value() | undefined) -> value())) -> state().
state_update(State, Key, UpdateFun) ->
    OldValue = state_get(State, Key),
    NewValue = UpdateFun(OldValue),
    state_set(State, Key, NewValue).

%% @doc 批量设置状态值
%%
%% 接受键值对列表，批量更新状态。
-spec state_set_many(state(), [{key(), value()}]) -> state().
state_set_many(State, Pairs) ->
    lists:foldl(
        fun({Key, Value}, AccState) ->
            state_set(AccState, Key, Value)
        end,
        State,
        Pairs
    ).

%%====================================================================
%% 错误处理
%%====================================================================

%% @doc 格式化错误信息
%%
%% 统一的错误格式化函数，提供友好的错误信息。
-spec format_error(term()) -> binary().
format_error({error, Reason}) ->
    format_error(Reason);
format_error({Class, Reason}) ->
    list_to_binary(
        io_lib:format("~p: ~p", [Class, Reason])
    );
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).

%% @doc 包装成功结果
%%
%% 标准化成功结果的包装。
-spec wrap_success(term()) -> {ok, term()}.
wrap_success(Value) ->
    {ok, Value}.

%% @doc 包装错误结果
%%
%% 标准化错误结果的包装，支持错误类型标记。
-spec wrap_error(term()) -> {error, term()}.
wrap_error(Reason) ->
    {error, Reason}.

%% @doc 判断是否为成功结果
-spec is_success(result(_)) -> boolean().
is_success({ok, _}) -> true;
is_success(_) -> false.

%% @doc 判断是否为错误结果
-spec is_error(result(_)) -> boolean().
is_error({error, _}) -> true;
is_error(_) -> false.

%%====================================================================
%% 结果构建
%%====================================================================

%% @doc 构建工具执行结果（成功）
%%
%% 标准化成功结果的构建。
-spec build_tool_result(binary(), binary(), term()) -> map().
build_tool_result(ToolCallId, ToolName, Result) ->
    #{
        tool_call_id => ToolCallId,
        name => ToolName,
        success => true,
        result => Result
    }.

%% @doc 构建工具执行结果（失败）
-spec build_tool_result(binary(), binary(), term(), term()) -> map().
build_tool_result(ToolCallId, ToolName, Error, _ErrorInfo) ->
    #{
        tool_call_id => ToolCallId,
        name => ToolName,
        success => false,
        error => Error
    }.

%% @doc 从状态中提取响应内容
%%
%% 优先使用 final_response，否则从消息历史中提取。
-spec extract_response(state()) -> binary().
extract_response(State) ->
    case state_get(State, final_response, undefined) of
        undefined ->
            Messages = state_get(State, messages, []),
            extract_last_assistant_response(Messages);
        Response ->
            Response
    end.

%%====================================================================
%% 私有函数
%%====================================================================

%% @private 提取最后的助手响应
%%
%% 从消息列表中提取最后一条助手消息的内容。
-spec extract_last_assistant_response([map()]) -> binary().
extract_last_assistant_response(Messages) ->
    case lists:reverse(Messages) of
        [#{role := assistant, content := Content} | _] ->
            Content;
        _ ->
            <<>>
    end.
