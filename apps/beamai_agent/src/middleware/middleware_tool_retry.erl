%%%-------------------------------------------------------------------
%%% @doc Tool Retry Middleware - 工具重试
%%%
%%% 在工具执行失败时自动重试：
%%% - 支持指数退避
%%% - 支持自定义重试条件
%%% - 支持重试次数限制
%%% - 支持重试回调通知
%%%
%%% == 配置选项 ==
%%%
%%% ```erlang
%%% {middleware_tool_retry, #{
%%%     %% 最大重试次数
%%%     max_retries => 3,
%%%
%%%     %% 退避策略
%%%     backoff => #{
%%%         type => exponential | linear | constant,
%%%         initial_delay => 1000,    %% 初始延迟（毫秒）
%%%         max_delay => 30000,       %% 最大延迟
%%%         multiplier => 2           %% 指数退避乘数
%%%     },
%%%
%%%     %% 可重试的错误类型（默认重试所有错误）
%%%     retryable_errors => [timeout, connection_error, rate_limit],
%%%
%%%     %% 自定义重试判断函数
%%%     retry_fn => fun(Error, ToolName, RetryCount) -> boolean(),
%%%
%%%     %% 重试时的回调
%%%     on_retry => fun(ToolName, Error, RetryCount, Delay) -> ok
%%% }}
%%% ```
%%%
%%% == 工作流程 ==
%%%
%%% 1. after_tools 钩子触发
%%% 2. 检查工具执行结果
%%% 3. 如果有失败且可重试：
%%%    a. 计算退避延迟
%%%    b. 等待延迟时间
%%%    c. 将失败的工具调用重新加入队列
%%%    d. 返回 {goto, tools} 重新执行
%%% 4. 如果超过重试限制或不可重试，继续正常流程
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_tool_retry).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, before_tools/2, after_tools/2]).

%% 工具函数
-export([
    is_retryable/3,
    calculate_delay/2,
    get_retry_stats/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type backoff_type() :: exponential | linear | constant.

-type backoff_config() :: #{
    type := backoff_type(),
    initial_delay := pos_integer(),
    max_delay := pos_integer(),
    multiplier => number()
}.

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
%%
%% 配置选项：
%%   - max_retries: 最大重试次数
%%   - backoff: 退避配置（type, initial_delay, max_delay, multiplier）
%%   - retryable_errors: 可重试的错误类型（all | [atom()]）
%%   - retry_fn: 自定义重试判断函数
%%   - on_retry: 重试回调函数
%%   - enable_delay: 是否启用退避延迟
%%   - debug: 调试模式
-spec init(map()) -> map().
init(Opts) ->
    DefaultBackoff = #{
        type => exponential,
        initial_delay => 1000,
        max_delay => 30000,
        multiplier => 2
    },

    #{
        max_retries => maps:get(max_retries, Opts, 3),
        backoff => maps:merge(DefaultBackoff, maps:get(backoff, Opts, #{})),
        retryable_errors => maps:get(retryable_errors, Opts, all),
        retry_fn => maps:get(retry_fn, Opts, undefined),
        on_retry => maps:get(on_retry, Opts, undefined),
        enable_delay => maps:get(enable_delay, Opts, true),
        debug => maps:get(debug, Opts, false)
    }.

%% @doc 工具执行前初始化重试状态
%%
%% 确保重试追踪器存在于状态中。
-spec before_tools(map(), map()) -> beamai_middleware:middleware_result().
before_tools(State, _MwState) ->
    case graph:get(State, mw_tool_retry_tracker, undefined) of
        undefined ->
            {update, #{mw_tool_retry_tracker => #{}}};
        _ ->
            ok
    end.

%% @doc 工具执行后检查失败并重试
%%
%% 流程：
%%   1. 获取工具执行结果
%%   2. 找出失败的工具
%%   3. 如果有可重试的失败，执行重试
%%   4. 否则清理状态继续
-spec after_tools(map(), map()) -> beamai_middleware:middleware_result().
after_tools(State, MwState) ->
    ToolResults = graph:get(State, tool_results, []),
    OriginalToolCalls = graph:get(State, mw_original_tool_calls,
                                   graph:get(State, tool_calls, [])),

    FailedTools = find_failed_tools(OriginalToolCalls, ToolResults, State, MwState),

    case FailedTools of
        [] ->
            %% 没有失败的工具，清理状态
            {update, #{
                mw_tool_retry_tracker => #{},
                mw_original_tool_calls => undefined
            }};
        _ ->
            %% 有失败的工具，尝试重试
            handle_failed_tools(FailedTools, OriginalToolCalls, ToolResults, State, MwState)
    end.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 判断错误是否可重试
%%
%% 使用自定义函数（如果提供）或默认判断逻辑。
-spec is_retryable(term(), binary(), map()) -> boolean().
is_retryable(Error, ToolName, MwState) ->
    #{retryable_errors := RetryableErrors, retry_fn := RetryFn} = MwState,

    case RetryFn of
        undefined ->
            is_retryable_default(Error, RetryableErrors);
        RetryFn when is_function(RetryFn, 3) ->
            try
                RetryFn(Error, ToolName, MwState)
            catch
                _:_ -> false
            end
    end.

%% @doc 计算退避延迟
%%
%% 根据退避类型计算延迟时间：
%%   - constant: 固定延迟
%%   - linear: 线性递增
%%   - exponential: 指数递增
-spec calculate_delay(pos_integer(), backoff_config()) -> pos_integer().
calculate_delay(RetryCount, #{type := Type,
                               initial_delay := InitialDelay,
                               max_delay := MaxDelay} = Config) ->
    Delay = case Type of
        constant ->
            InitialDelay;
        linear ->
            InitialDelay * RetryCount;
        exponential ->
            Multiplier = maps:get(multiplier, Config, 2),
            round(InitialDelay * math:pow(Multiplier, RetryCount - 1))
    end,
    min(Delay, MaxDelay).

%% @doc 获取重试统计信息
-spec get_retry_stats(map()) -> map().
get_retry_stats(State) ->
    Tracker = graph:get(State, mw_tool_retry_tracker, #{}),
    TotalRetries = maps:fold(fun(_K, V, Acc) -> Acc + V end, 0, Tracker),
    #{
        retry_tracker => Tracker,
        total_retries => TotalRetries
    }.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 找出失败的工具调用
%%
%% 筛选条件：
%%   1. 执行结果为 {error, _}
%%   2. 未超过最大重试次数
%%   3. 错误类型可重试
-spec find_failed_tools([map()], [term()], map(), map()) -> [{map(), term(), pos_integer()}].
find_failed_tools(ToolCalls, Results, State, MwState) ->
    #{max_retries := MaxRetries} = MwState,
    Tracker = graph:get(State, mw_tool_retry_tracker, #{}),

    Pairs = lists:zip(ToolCalls, Results),

    lists:filtermap(fun({TC, Result}) ->
        case Result of
            {error, Error} ->
                ToolName = beamai_agent_utils:extract_tool_name(TC),
                RetryCount = maps:get(ToolName, Tracker, 0),

                CanRetry = RetryCount < MaxRetries andalso
                           is_retryable(Error, ToolName, MwState),

                case CanRetry of
                    true -> {true, {TC, Error, RetryCount + 1}};
                    false -> false
                end;
            _ ->
                false
        end
    end, Pairs).

%% @private 处理失败的工具
%%
%% 流程：
%%   1. 计算退避延迟
%%   2. 更新重试追踪器
%%   3. 调用重试回调
%%   4. 等待退避延迟
%%   5. 返回重试指令
-spec handle_failed_tools([{map(), term(), pos_integer()}], [map()], [term()], map(), map()) ->
    beamai_middleware:middleware_result().
handle_failed_tools(FailedTools, OriginalToolCalls, ToolResults, State, MwState) ->
    #{backoff := BackoffConfig,
      on_retry := OnRetry,
      enable_delay := EnableDelay,
      debug := Debug} = MwState,

    Tracker = graph:get(State, mw_tool_retry_tracker, #{}),

    %% 步骤 1：计算最大延迟（基于最高重试次数）
    MaxRetryCount = lists:max([Count || {_, _, Count} <- FailedTools]),
    Delay = calculate_delay(MaxRetryCount, BackoffConfig),

    %% 步骤 2：更新重试追踪器
    NewTracker = lists:foldl(fun({TC, _Error, Count}, Acc) ->
        ToolName = beamai_agent_utils:extract_tool_name(TC),
        Acc#{ToolName => Count}
    end, Tracker, FailedTools),

    %% 步骤 3：提取需要重试的工具调用
    RetryToolCalls = [TC || {TC, _, _} <- FailedTools],

    %% 步骤 4：调用重试回调并记录日志
    lists:foreach(fun({TC, Error, Count}) ->
        ToolName = beamai_agent_utils:extract_tool_name(TC),
        maybe_call_on_retry(OnRetry, ToolName, Error, Count, Delay),
        case Debug of
            true ->
                logger:info("[ToolRetry] 重试 ~s (第 ~p 次, 延迟 ~p ms): ~p",
                           [ToolName, Count, Delay, Error]);
            false ->
                ok
        end
    end, FailedTools),

    %% 步骤 5：等待退避延迟
    case EnableDelay andalso Delay > 0 of
        true -> timer:sleep(Delay);
        false -> ok
    end,

    %% 步骤 6：构建成功的结果（保留到消息中）
    SuccessResults = build_success_results(OriginalToolCalls, ToolResults, FailedTools),

    %% 步骤 7：返回重试指令
    {update_goto, #{
        tool_calls => RetryToolCalls,
        mw_original_tool_calls => OriginalToolCalls,
        mw_tool_retry_tracker => NewTracker,
        mw_partial_tool_results => SuccessResults
    }, tools}.

%% @private 构建成功的结果
%%
%% 过滤掉失败的结果，保留成功的。
-spec build_success_results([map()], [term()], [{map(), term(), pos_integer()}]) -> [term()].
build_success_results(ToolCalls, Results, FailedTools) ->
    FailedIds = [beamai_agent_utils:extract_tool_id(TC) || {TC, _, _} <- FailedTools],
    Pairs = lists:zip(ToolCalls, Results),
    [{TC, R} || {TC, R} <- Pairs,
                not lists:member(beamai_agent_utils:extract_tool_id(TC), FailedIds),
                not is_error_result(R)].

%% @private 检查是否为错误结果
-spec is_error_result(term()) -> boolean().
is_error_result({error, _}) -> true;
is_error_result(_) -> false.

%% @private 默认重试判断
-spec is_retryable_default(term(), all | [atom()]) -> boolean().
is_retryable_default(_Error, all) ->
    true;
is_retryable_default({ErrorType, _}, RetryableErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, RetryableErrors);
is_retryable_default(ErrorType, RetryableErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, RetryableErrors);
is_retryable_default(_, _) ->
    false.

%% @private 调用重试回调
-spec maybe_call_on_retry(function() | undefined, binary(), term(),
                          pos_integer(), pos_integer()) -> ok.
maybe_call_on_retry(undefined, _, _, _, _) ->
    ok;
maybe_call_on_retry(OnRetry, ToolName, Error, RetryCount, Delay) when is_function(OnRetry, 4) ->
    try
        OnRetry(ToolName, Error, RetryCount, Delay)
    catch
        _:Reason ->
            logger:warning("重试回调异常: ~p", [Reason])
    end,
    ok.
