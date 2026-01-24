%%%-------------------------------------------------------------------
%%% @doc 模型重试中间件
%%%
%%% 在 LLM 调用失败时自动进行重试，支持退避策略和随机抖动：
%%% - 可配置最大重试次数
%%% - 支持指数退避、线性退避和固定延迟三种策略
%%% - 支持随机抖动（jitter），避免多个客户端同时重试造成雷群效应
%%% - 内置常见 LLM 可重试错误类型列表
%%% - 支持自定义重试判断函数和重试回调
%%%
%%% 使用的钩子：
%%% - post_chat: 检测 LLM 响应中的错误并触发重试
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_model_retry).

-behaviour(beamai_middleware).

-export([init/1, post_chat/2]).
-export([is_retryable/2, calculate_delay/2]).

%%====================================================================
%% 类型定义
%%====================================================================

%% @type backoff_config() :: map().
%% 退避策略配置类型（含抖动支持）：
%% - type: 退避类型（exponential 指数 | linear 线性 | constant 固定）
%% - initial_delay: 初始延迟时间（毫秒），正整数
%% - max_delay: 最大延迟时间（毫秒），正整数
%% - multiplier: 退避倍数（仅 exponential 和 linear 使用），可选
%% - jitter: 是否启用随机抖动（布尔值），可选
-type backoff_config() :: #{
    type := exponential | linear | constant,
    initial_delay := pos_integer(),
    max_delay := pos_integer(),
    multiplier => number(),
    jitter => boolean()
}.

%%====================================================================
%% 中间件回调函数
%%====================================================================

%% @doc 初始化 LLM 重试配置。
%%
%% 根据传入的配置选项设置 LLM 重试参数，包括最大重试次数、
%% 退避策略（含抖动）、可重试错误类型列表等。
%%
%% 配置参数说明：
%% - max_retries: 最大重试次数（默认 3）
%% - backoff: 退避策略配置映射
%%   - type: exponential（默认指数退避）
%%   - initial_delay: 1000ms（初始延迟）
%%   - max_delay: 30000ms（最大延迟）
%%   - multiplier: 2（退避倍数）
%%   - jitter: true（默认启用抖动）
%% - retryable_errors: 可重试的错误类型列表（默认使用内置列表）
%% - retry_fn: 自定义重试判断函数 fun(Error, RetryCount)，可选
%% - on_retry: 重试回调函数 fun(Error, RetryCount, Delay)，可选
%%
%% @param Opts 配置选项映射
%% @returns 包含重试配置和初始重试计数器的状态映射
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
        retry_count => 0
    }.

%% @doc 检测 LLM 错误并触发重试。
%%
%% 在收到 LLM 响应后检查结果，如果结果为 {error, Error} 形式，
%% 则调用 handle_error 判断是否需要重试。如果结果正常则直接通过。
%%
%% @param FilterCtx 过滤器上下文，包含 result 键表示 LLM 响应结果
%% @param MwState 当前中间件状态，包含重试配置和计数器
%% @returns ok（不需要重试或结果正常时）或 {skip, RetryInfo}（触发重试时）
-spec post_chat(map(), map()) -> beamai_middleware:middleware_result().
post_chat(FilterCtx, MwState) ->
    case maps:get(result, FilterCtx, undefined) of
        {error, Error} ->
            handle_error(Error, FilterCtx, MwState);
        _ ->
            ok
    end.

%%====================================================================
%% 公共辅助函数
%%====================================================================

%% @doc 判断 LLM 错误是否可重试。
%%
%% 根据中间件配置判断给定的 LLM 错误是否允许重试：
%% - 如果配置了自定义 retry_fn，则调用该函数判断
%% - 否则使用默认的错误类型匹配逻辑（支持原子、元组和二进制字符串匹配）
%%
%% @param Error 错误信息，可以是原子、元组或二进制字符串
%% @param MwState 中间件状态（需包含 retryable_errors 和 retry_fn）
%% @returns true 表示可以重试，false 表示不可重试
-spec is_retryable(term(), map()) -> boolean().
is_retryable(Error, #{retryable_errors := RetryableErrors, retry_fn := RetryFn}) ->
    case RetryFn of
        undefined -> is_retryable_default(Error, RetryableErrors);
        RetryFn when is_function(RetryFn, 2) ->
            try RetryFn(Error, 0) catch _:_ -> false end
    end.

%% @doc 计算退避延迟时间（含抖动）。
%%
%% 根据当前重试次数和退避策略配置，计算本次重试应等待的延迟时间。
%% 支持三种基础计算方式：
%% - constant: 固定延迟，始终为 initial_delay
%% - linear: 线性增长，delay = initial_delay * retry_count
%% - exponential: 指数增长，delay = initial_delay * multiplier^(retry_count-1)
%%
%% 如果启用了 jitter（抖动），会在基础延迟上添加 +/-25% 的随机偏移，
%% 用于避免多个客户端同时重试造成的雷群效应。
%%
%% 计算结果不会小于 0 且不会超过 max_delay 配置的上限。
%%
%% @param RetryCount 当前重试次数（从 1 开始）
%% @param Config 退避策略配置映射（含可选的 jitter 字段）
%% @returns 延迟时间（毫秒），正整数
-spec calculate_delay(pos_integer(), backoff_config()) -> pos_integer().
calculate_delay(RetryCount, #{type := Type,
                               initial_delay := InitialDelay,
                               max_delay := MaxDelay} = Config) ->
    BaseDelay = case Type of
        constant -> InitialDelay;
        linear -> InitialDelay * RetryCount;
        exponential ->
            Multiplier = maps:get(multiplier, Config, 2),
            round(InitialDelay * math:pow(Multiplier, RetryCount - 1))
    end,
    DelayWithJitter = case maps:get(jitter, Config, false) of
        true ->
            %% 添加 +/-25% 的随机抖动
            Jitter = BaseDelay * 0.25,
            round(BaseDelay + (rand:uniform() * 2 - 1) * Jitter);
        false ->
            BaseDelay
    end,
    min(max(DelayWithJitter, 0), MaxDelay).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 默认的可重试 LLM 错误类型列表
%% 包含常见的网络错误、超时、限流和服务端错误类型
default_retryable_errors() ->
    [timeout, connection_error, connection_closed, rate_limit,
     rate_limited, server_error, service_unavailable,
     internal_server_error, bad_gateway, gateway_timeout,
     econnrefused, econnreset, etimedout].

%% @private 处理 LLM 错误，判断并执行重试
%% 检查重试计数器是否未超过上限，且错误类型是否可重试。
%% 如果满足重试条件：
%% 1. 计算退避延迟时间（含抖动）
%% 2. 调用重试回调函数（如已配置）
%% 3. 等待退避延迟
%% 4. 返回 {skip, RetryInfo} 信号通知上层进行模型重试
handle_error(Error, _FilterCtx, MwState) ->
    #{max_retries := MaxRetries,
      backoff := BackoffConfig,
      on_retry := OnRetry,
      retry_count := RetryCount} = MwState,

    NewRetryCount = RetryCount + 1,
    CanRetry = NewRetryCount =< MaxRetries andalso is_retryable(Error, MwState),

    case CanRetry of
        true ->
            Delay = calculate_delay(NewRetryCount, BackoffConfig),
            maybe_call_on_retry(OnRetry, Error, NewRetryCount, Delay),
            timer:sleep(Delay),
            %% 通过返回 skip 信号通知上层执行模型重试
            {skip, #{pending => model_retry,
                     retry_count => NewRetryCount,
                     error => Error}};
        false ->
            %% 不可重试或已达最大重试次数，放行错误
            ok
    end.

%% @private 默认的可重试错误判断逻辑
%% - 当错误为 {ErrorType, _} 元组时，检查 ErrorType 是否在可重试列表中
%% - 当错误为单个原子时，直接检查是否在可重试列表中
%% - 当错误为二进制字符串时，通过模式匹配检查是否包含可重试的关键词
%%   （timeout、rate limit、connection、server error、503、502、504）
%% - 其他格式的错误不可重试
is_retryable_default({ErrorType, _}, RetryableErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, RetryableErrors);
is_retryable_default(ErrorType, RetryableErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, RetryableErrors);
is_retryable_default(ErrorBin, _) when is_binary(ErrorBin) ->
    LowerError = string:lowercase(binary_to_list(ErrorBin)),
    lists:any(fun(Pattern) ->
        string:find(LowerError, Pattern) =/= nomatch
    end, ["timeout", "rate limit", "connection", "server error", "503", "502", "504"]);
is_retryable_default(_, _) -> false.

%% @private 安全调用重试回调函数
%% 如果 OnRetry 未定义则跳过，否则调用回调并捕获可能的异常。
%% 回调异常不会影响重试流程本身的执行。
maybe_call_on_retry(undefined, _, _, _) -> ok;
maybe_call_on_retry(OnRetry, Error, RetryCount, Delay) when is_function(OnRetry, 3) ->
    try OnRetry(Error, RetryCount, Delay) catch _:R -> logger:warning("Retry callback error: ~p", [R]) end,
    ok.
