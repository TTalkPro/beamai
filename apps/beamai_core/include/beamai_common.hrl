%%%-------------------------------------------------------------------
%%% @doc 公共宏定义和常量
%%%
%%% 提供全项目共享的：
%%% - 默认超时配置
%%% - Agent 默认配置
%%% - 存储默认配置
%%% - 安全执行宏
%%% - 日志宏
%%%
%%% 使用方式：
%%% -include_lib("beamai_core/include/beamai_common.hrl").
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AGENT_COMMON_HRL).
-define(AGENT_COMMON_HRL, true).

%%====================================================================
%% 默认超时配置（毫秒）
%%====================================================================

%% 通用超时
-define(DEFAULT_TIMEOUT, 60000).

%% HTTP 连接超时
-define(DEFAULT_CONNECT_TIMEOUT, 10000).

%% LLM 调用超时（较长，因为 LLM 响应可能较慢）
-define(DEFAULT_LLM_TIMEOUT, 120000).

%% 重试配置
-define(DEFAULT_MAX_RETRIES, 3).
-define(DEFAULT_RETRY_DELAY, 1000).

%%====================================================================
%% Agent 默认配置
%%====================================================================

%% 最大迭代次数（防止无限循环）
-define(DEFAULT_MAX_ITERATIONS, 10).

%% 最大深度（用于递归 Agent）
-define(DEFAULT_MAX_DEPTH, 3).

%%====================================================================
%% 存储默认配置
%%====================================================================

%% 最大消息数量
-define(DEFAULT_MAX_MESSAGES, 1000).

%% 最大检查点数量
-define(DEFAULT_MAX_CHECKPOINTS, 100).

%% 上下文存储键
-define(CONTEXT_KEY, <<"__context__">>).

%%====================================================================
%% 安全执行宏
%%====================================================================

%% @doc 安全执行表达式，捕获异常
%% 返回 {ok, Result} | {error, {Class, Reason}}
-define(SAFE_EXEC(Expr),
    try
        {ok, Expr}
    catch
        __Class__:__Reason__:_ ->
            {error, {__Class__, __Reason__}}
    end
).

%% @doc 安全执行表达式，异常时返回默认值
-define(SAFE_EXEC_DEFAULT(Expr, Default),
    try Expr
    catch _:_ -> Default
    end
).

%% @doc 安全执行表达式，异常时调用错误处理函数
%% ErrorFun 接收 {Class, Reason} 参数
-define(SAFE_EXEC_WITH(Expr, ErrorFun),
    try
        {ok, Expr}
    catch
        __Class__:__Reason__:_ ->
            ErrorFun({__Class__, __Reason__})
    end
).

%%====================================================================
%% 日志宏（基于 logger）
%%====================================================================

-define(LOG_DEBUG(Msg), logger:debug(Msg)).
-define(LOG_DEBUG(Fmt, Args), logger:debug(Fmt, Args)).
-define(LOG_INFO(Msg), logger:info(Msg)).
-define(LOG_INFO(Fmt, Args), logger:info(Fmt, Args)).
-define(LOG_WARNING(Msg), logger:warning(Msg)).
-define(LOG_WARNING(Fmt, Args), logger:warning(Fmt, Args)).
-define(LOG_ERROR(Msg), logger:error(Msg)).
-define(LOG_ERROR(Fmt, Args), logger:error(Fmt, Args)).

%%====================================================================
%% 断言宏（用于调试）
%%====================================================================

-ifdef(DEBUG).
-define(ASSERT(Cond, Msg),
    case Cond of
        true -> ok;
        false -> error({assertion_failed, Msg})
    end
).
-else.
-define(ASSERT(_Cond, _Msg), ok).
-endif.

%%====================================================================
%% 回调调用宏
%%====================================================================

%% @doc 调用回调函数（带回调映射和元数据）
%% 使用示例：?INVOKE_CALLBACK(on_tool_start, [Name, Args], Callbacks, Meta)
-define(INVOKE_CALLBACK(Name, Args, Callbacks, Meta),
    beamai_callback_utils:invoke(Name, Args, Callbacks, Meta)
).

%% @doc 从图状态调用回调
%% 使用示例：?INVOKE_CALLBACK_FROM_STATE(on_llm_start, [Messages], State)
-define(INVOKE_CALLBACK_FROM_STATE(Name, Args, State),
    beamai_callback_utils:invoke_from_state(Name, Args, State)
).

%%====================================================================
%% 状态操作宏
%%====================================================================

%% @doc 批量设置图状态
%% 使用示例：
%%   NewState = ?SET_STATE_MANY(State, [{key1, val1}, {key2, val2}])
-define(SET_STATE_MANY(State, Pairs),
    beamai_state_utils:set_many(State, Pairs)
).

%% @doc 管道式状态更新
%% 使用示例：
%%   NewState = ?SET_STATE_PIPELINE(State, [
%%       fun(S) -> graph:set(S, key1, val1) end,
%%       fun(S) -> maybe_update(S) end
%%   ])
-define(SET_STATE_PIPELINE(State, Funs),
    beamai_state_utils:set_pipeline(State, Funs)
).

%%====================================================================
%% 请求构建宏
%%====================================================================

%% @doc 构建请求体管道
%% 用于 LLM Provider 的请求体构建
%% 使用示例：
%%   Body = ?BUILD_BODY_PIPELINE(Base, [
%%       fun(B) -> maybe_add_system(B, Prompt) end,
%%       fun(B) -> maybe_add_tools(B, Tools) end
%%   ])
-define(BUILD_BODY_PIPELINE(Base, Funs),
    lists:foldl(fun(F, Acc) -> F(Acc) end, Base, Funs)
).

-endif. %% AGENT_COMMON_HRL
