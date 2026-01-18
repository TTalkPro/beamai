%%%-------------------------------------------------------------------
%%% @doc Model Retry Middleware - 模型调用重试
%%%
%%% 在模型调用失败时自动重试，支持指数退避策略。
%%%
%%% == 功能特性 ==
%%%
%%% - 可配置最大重试次数
%%% - 支持指数退避、线性退避、固定延迟
%%% - 可配置可重试的错误类型
%%% - 支持自定义重试判断函数
%%% - 支持重试回调通知
%%%
%%% == 配置示例 ==
%%%
%%% ```erlang
%%% {middleware_model_retry, #{
%%%     %% 最大重试次数（默认 3）
%%%     max_retries => 3,
%%%
%%%     %% 退避策略
%%%     backoff => #{
%%%         type => exponential,      %% exponential | linear | constant
%%%         initial_delay => 1000,    %% 初始延迟（毫秒）
%%%         max_delay => 30000,       %% 最大延迟
%%%         multiplier => 2,          %% 指数退避乘数
%%%         jitter => true            %% 添加随机抖动
%%%     },
%%%
%%%     %% 可重试的错误类型
%%%     retryable_errors => [timeout, connection_error, rate_limit, server_error],
%%%
%%%     %% 自定义重试判断函数
%%%     retry_fn => fun(Error, RetryCount) -> boolean(),
%%%
%%%     %% 重试回调
%%%     on_retry => fun(Error, RetryCount, Delay) -> ok
%%% }}
%%% ```
%%%
%%% == 工作流程 ==
%%%
%%% 1. after_model 钩子检测到错误
%%% 2. 判断是否可重试
%%% 3. 计算退避延迟
%%% 4. 等待延迟时间
%%% 5. 返回 {goto, model} 重新调用模型
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_model_retry).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, after_model/2]).

%% 工具函数
-export([
    is_retryable/2,
    calculate_delay/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type backoff_type() :: exponential | linear | constant.

-type backoff_config() :: #{
    type := backoff_type(),
    initial_delay := pos_integer(),
    max_delay := pos_integer(),
    multiplier => number(),
    jitter => boolean()
}.

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
%%
%% 配置选项：
%%   - max_retries: 最大重试次数（默认 3）
%%   - backoff: 退避配置
%%   - retryable_errors: 可重试的错误类型列表
%%   - retry_fn: 自定义重试判断函数
%%   - on_retry: 重试回调函数
%%   - debug: 调试模式
-spec init(map()) -> map().
init(Opts) ->
    DefaultBackoff = #{
        type => exponential,
        initial_delay => 1000,
        max_delay => 30000,
        multiplier => 2,
        jitter => true
    },

    #{
        max_retries => maps:get(max_retries, Opts, 3),
        backoff => maps:merge(DefaultBackoff, maps:get(backoff, Opts, #{})),
        retryable_errors => maps:get(retryable_errors, Opts, default_retryable_errors()),
        retry_fn => maps:get(retry_fn, Opts, undefined),
        on_retry => maps:get(on_retry, Opts, undefined),
        debug => maps:get(debug, Opts, false)
    }.

%% @doc 模型调用后检查是否需要重试
%%
%% 检查 llm_error 字段，如果存在且可重试，执行重试逻辑。
-spec after_model(map(), map()) -> beamai_middleware:middleware_result().
after_model(State, MwState) ->
    case graph:get(State, llm_error, undefined) of
        undefined ->
            %% 没有错误，清理重试计数器
            {update, #{mw_model_retry_count => 0}};
        Error ->
            handle_error(Error, State, MwState)
    end.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 判断错误是否可重试
-spec is_retryable(term(), map()) -> boolean().
is_retryable(Error, MwState) ->
    #{retryable_errors := RetryableErrors, retry_fn := RetryFn} = MwState,

    case RetryFn of
        undefined ->
            is_retryable_default(Error, RetryableErrors);
        RetryFn when is_function(RetryFn, 2) ->
            RetryCount = maps:get(mw_model_retry_count, MwState, 0),
            try
                RetryFn(Error, RetryCount)
            catch
                _:_ -> false
            end
    end.

%% @doc 计算退避延迟
-spec calculate_delay(pos_integer(), backoff_config()) -> pos_integer().
calculate_delay(RetryCount, #{type := Type,
                               initial_delay := InitialDelay,
                               max_delay := MaxDelay} = Config) ->
    BaseDelay = case Type of
        constant ->
            InitialDelay;
        linear ->
            InitialDelay * RetryCount;
        exponential ->
            Multiplier = maps:get(multiplier, Config, 2),
            round(InitialDelay * math:pow(Multiplier, RetryCount - 1))
    end,

    %% 添加抖动
    DelayWithJitter = case maps:get(jitter, Config, false) of
        true ->
            %% 添加 ±25% 的随机抖动
            Jitter = BaseDelay * 0.25,
            round(BaseDelay + (rand:uniform() * 2 - 1) * Jitter);
        false ->
            BaseDelay
    end,

    min(DelayWithJitter, MaxDelay).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 默认可重试的错误类型
-spec default_retryable_errors() -> [atom()].
default_retryable_errors() ->
    [
        timeout,
        connection_error,
        connection_closed,
        rate_limit,
        rate_limited,
        server_error,
        service_unavailable,
        internal_server_error,
        bad_gateway,
        gateway_timeout,
        econnrefused,
        econnreset,
        etimedout
    ].

%% @private 处理错误
-spec handle_error(term(), map(), map()) -> beamai_middleware:middleware_result().
handle_error(Error, State, MwState) ->
    #{max_retries := MaxRetries,
      backoff := BackoffConfig,
      on_retry := OnRetry,
      debug := Debug} = MwState,

    RetryCount = graph:get(State, mw_model_retry_count, 0),
    NewRetryCount = RetryCount + 1,

    CanRetry = NewRetryCount =< MaxRetries andalso is_retryable(Error, MwState),

    case CanRetry of
        true ->
            %% 计算延迟
            Delay = calculate_delay(NewRetryCount, BackoffConfig),

            %% 调用重试回调
            maybe_call_on_retry(OnRetry, Error, NewRetryCount, Delay),

            %% 调试日志
            case Debug of
                true ->
                    logger:info("[ModelRetry] 重试第 ~p 次，延迟 ~p ms，错误: ~p",
                               [NewRetryCount, Delay, Error]);
                false ->
                    ok
            end,

            %% 等待延迟
            timer:sleep(Delay),

            %% 清除错误并重试
            {update_goto, #{
                mw_model_retry_count => NewRetryCount,
                llm_error => undefined
            }, model};
        false ->
            %% 不可重试或超过重试次数
            case Debug of
                true ->
                    logger:warning("[ModelRetry] 放弃重试，已重试 ~p 次，错误: ~p",
                                  [RetryCount, Error]);
                false ->
                    ok
            end,
            ok
    end.

%% @private 默认重试判断
-spec is_retryable_default(term(), [atom()]) -> boolean().
is_retryable_default({ErrorType, _}, RetryableErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, RetryableErrors);
is_retryable_default(ErrorType, RetryableErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, RetryableErrors);
is_retryable_default(ErrorBin, _RetryableErrors) when is_binary(ErrorBin) ->
    %% 检查常见的可重试错误消息
    LowerError = string:lowercase(binary_to_list(ErrorBin)),
    lists:any(fun(Pattern) ->
        string:find(LowerError, Pattern) =/= nomatch
    end, ["timeout", "rate limit", "connection", "server error", "503", "502", "504"]);
is_retryable_default(_, _) ->
    false.

%% @private 调用重试回调
-spec maybe_call_on_retry(function() | undefined, term(),
                          pos_integer(), pos_integer()) -> ok.
maybe_call_on_retry(undefined, _, _, _) ->
    ok;
maybe_call_on_retry(OnRetry, Error, RetryCount, Delay) when is_function(OnRetry, 3) ->
    try
        OnRetry(Error, RetryCount, Delay)
    catch
        _:Reason ->
            logger:warning("[ModelRetry] 重试回调异常: ~p", [Reason])
    end,
    ok.
