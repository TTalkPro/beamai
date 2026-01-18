%%%-------------------------------------------------------------------
%%% @doc 回调工具模块
%%%
%%% 提供统一的回调调用机制，用于 Agent 生命周期事件通知：
%%% - LLM 调用回调 (on_llm_start/end/error)
%%% - 工具执行回调 (on_tool_start/end/error)
%%% - Agent 动作回调 (on_agent_action/finish)
%%% - RAG 检索回调 (on_retriever_start/end/error)
%%% - 链式调用回调 (on_chain_start/end/error)
%%%
%%% 设计原则：
%%% - 安全执行：回调失败不影响主流程，仅记录警告日志
%%% - 统一接口：所有回调使用相同的调用模式
%%% - 元数据追加：自动将上下文信息追加到回调参数末尾
%%%
%%% 使用示例：
%%% <pre>
%%% %% 从图状态调用回调
%%% beamai_callback_utils:invoke_from_state(on_llm_start, [Messages], State).
%%%
%%% %% 直接调用回调
%%% beamai_callback_utils:invoke(on_tool_end, [Name, Result], Callbacks, Meta).
%%% </pre>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_callback_utils).

%% API 导出
-export([invoke/3, invoke/4]).
-export([invoke_from_state/3]).
-export([maybe_invoke/4]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 调用回调函数（不带元数据）
%% 等价于 invoke(CallbackName, Args, Callbacks, #{})
-spec invoke(atom(), list(), map()) -> ok.
invoke(CallbackName, Args, Callbacks) ->
    invoke(CallbackName, Args, Callbacks, #{}).

%% @doc 调用回调函数（带元数据）
%%
%% 参数：
%% - CallbackName: 回调名称，如 on_llm_start, on_tool_end
%% - Args: 回调参数列表（不含元数据）
%% - Callbacks: 回调函数映射 #{CallbackName => Handler}
%% - Meta: 元数据，将追加到 Args 末尾
%%
%% 如果回调未定义或执行失败，返回 ok 但不影响调用方
-spec invoke(atom(), list(), map(), map()) -> ok.
invoke(CallbackName, Args, Callbacks, Meta) ->
    case maps:get(CallbackName, Callbacks, undefined) of
        undefined ->
            ok;
        Handler when is_function(Handler) ->
            safe_apply(CallbackName, Handler, Args, Meta);
        _InvalidHandler ->
            ok
    end.

%% @doc 从图状态获取回调并执行
%%
%% 从 State 中提取 callbacks 和 callback_meta 字段，
%% 然后调用 invoke/4
%%
%% 状态要求：
%% - callbacks: 回调函数映射
%% - callback_meta: 元数据映射（可选，默认 #{}）
-spec invoke_from_state(atom(), list(), map()) -> ok.
invoke_from_state(CallbackName, Args, State) ->
    Callbacks = graph:get(State, callbacks, #{}),
    Meta = graph:get(State, callback_meta, #{}),
    invoke(CallbackName, Args, Callbacks, Meta).

%% @doc 条件调用回调
%%
%% 当 Condition 为 true 时调用回调，否则跳过
-spec maybe_invoke(boolean(), atom(), list(), map()) -> ok.
maybe_invoke(false, _CallbackName, _Args, _State) ->
    ok;
maybe_invoke(true, CallbackName, Args, State) ->
    invoke_from_state(CallbackName, Args, State).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 安全执行回调函数
%%
%% 捕获所有异常，记录警告日志但不抛出
-spec safe_apply(atom(), function(), list(), map()) -> ok.
safe_apply(CallbackName, Handler, Args, Meta) ->
    FullArgs = Args ++ [Meta],
    try
        erlang:apply(Handler, FullArgs)
    catch
        Class:Reason:Stack ->
            logger:warning("回调 ~p 执行失败: ~p:~p~n堆栈: ~p",
                           [CallbackName, Class, Reason, Stack])
    end,
    ok.
