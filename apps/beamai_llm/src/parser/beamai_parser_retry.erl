%%%-------------------------------------------------------------------
%%% @doc Output Parser 重试机制
%%%
%%% 提供解析失败时的自动重试功能。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_parser_retry).

%% 重试解析 API
-export([parse/4, parse_with_backoff/5]).

%%====================================================================
%% 类型定义
%%====================================================================

-type retry_options() :: #{
    on_retry => fun((term(), non_neg_integer()) -> ok),
    backoff => fun((non_neg_integer()) -> non_neg_integer()),
    max_delay => pos_integer()
}.

-type parse_result() :: {ok, term()} | {error, term()}.

%%====================================================================
%% 主解析函数
%%====================================================================

%% @doc 带重试的解析
-spec parse(beamai_output_parser:parser(), binary(), non_neg_integer(), map()) -> parse_result().
parse(Parser, Text, MaxRetries, Opts) ->
    do_parse(Parser, Text, MaxRetries, Opts, 0, []).

%% @private 执行带重试的解析
do_parse(_Parser, _Text, MaxRetries, _Opts, Attempt, Errors) when Attempt >= MaxRetries ->
    {error, {max_retries_exceeded, lists:reverse(Errors)}};

do_parse(Parser, Text, MaxRetries, Opts, Attempt, Errors) ->
    case beamai_output_parser:parse(Parser, Text) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} = Error ->
            %% 调用重试回调
            invoke_retry_callback(Opts, Reason, Attempt + 1),

            %% 检查是否可重试
            case beamai_output_parser:is_retryable_error(Reason) of
                true ->
                    %% 延迟后重试
                    BackoffFn = maps:get(backoff, Opts, fun linear_backoff/1),
                    Delay = BackoffFn(Attempt + 1),
                    MaxDelay = maps:get(max_delay, Opts, 5000),
                    ActualDelay = min(Delay, MaxDelay),
                    timer:sleep(ActualDelay),

                    do_parse(Parser, Text, MaxRetries, Opts, Attempt + 1, [Reason | Errors]);
                false ->
                    Error
            end
    end.

%% @private 调用重试回调
invoke_retry_callback(#{on_retry := Callback}, Error, Attempt) when is_function(Callback) ->
    try Callback(Error, Attempt) of
        _ -> ok
    catch
        _:_ -> ok
    end;
invoke_retry_callback(_, _, _) ->
    ok.

%%====================================================================
%% 带退避的重试
%%====================================================================

%% @doc 带退避策略的重试解析
%%
%% BackoffType: linear | exponential
-spec parse_with_backoff(beamai_output_parser:parser(), binary(), non_neg_integer(),
                        atom(), retry_options()) -> parse_result().
parse_with_backoff(Parser, Text, MaxRetries, BackoffType, Opts) ->
    BackoffFn = backoff_function(BackoffType),
    parse(Parser, Text, MaxRetries, Opts#{backoff => BackoffFn}).

%% @private 获取退避函数
-spec backoff_function(atom()) -> fun((non_neg_integer()) -> pos_integer()).
backoff_function(linear) ->
    fun linear_backoff/1;
backoff_function(exponential) ->
    fun exponential_backoff/1;
backoff_function(fibonacci) ->
    fun fibonacci_backoff/1;
backoff_function(_) ->
    fun linear_backoff/1.

%%====================================================================
%% 退避策略
%%====================================================================

%% @doc 线性退避
%% 每次等待: attempt * 100ms
-spec linear_backoff(non_neg_integer()) -> pos_integer().
linear_backoff(Attempt) ->
    Attempt * 100.

%% @doc 指数退避
%% 每次等待: 100ms * 2^(attempt-1)，但不超过 5 秒
-spec exponential_backoff(non_neg_integer()) -> pos_integer().
exponential_backoff(Attempt) ->
    BaseDelay = 100 bsl (Attempt - 1),
    min(BaseDelay, 5000).

%% @doc 斐波那契退避
%% 使用斐波那契数列计算延迟
-spec fibonacci_backoff(non_neg_integer()) -> pos_integer().
fibonacci_backoff(Attempt) ->
    fibonacci(Attempt) * 100.

%% @private 斐波那契数列
fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) -> fibonacci(N - 1) + fibonacci(N - 2).

