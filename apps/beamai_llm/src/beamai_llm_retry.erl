%%%-------------------------------------------------------------------
%%% @doc LLM 请求重试模块
%%%
%%% 把「按错误分类重试 + 退避」这一段逻辑从各调用方抽出，
%%% 供 chat（beamai_chat_model）与 embedding（beamai_embedding）共用。
%%%
%%% 重试判定统一委托 beamai_llm_error（单一事实源）：
%%%   - 瞬态错误（429 / 5xx / 网络超时）重试
%%%   - 语义错误（4xx 参数、鉴权）不重试
%%%
%%% 退避策略：
%%%   - 服务端给出 Retry-After 时按其建议（上限 60s）
%%%   - 否则线性退避 retry_delay * (Attempt + 1)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_retry).

-include_lib("beamai_core/include/beamai_common.hrl").

-export([run/2, opts/1, opts/2]).
-export([is_retryable/1, compute_delay/3]).

%% 重试上限退避（避免 Retry-After 过大时长时间阻塞）
-define(MAX_RETRY_DELAY, 60000).

-type retry_opts() :: #{
    max_retries := non_neg_integer(),
    retry_delay := pos_integer(),
    on_retry := fun((map()) -> any()) | undefined
}.

-export_type([retry_opts/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 从调用选项中提取重试配置（缺省用框架默认值）
-spec opts(map()) -> retry_opts().
opts(Opts) ->
    opts(#{}, Opts).

%% @doc 重试配置的三级取值：单次 Opts > provider Config > 框架默认
%%
%% 于是「这个 provider 一律重试 5 次」写在 create/2 的 Config 里一次即可，
%% 单次调用仍可覆盖（`max_retries => 0` 即本次不重试）。
-spec opts(map(), map()) -> retry_opts().
opts(Config, Opts) ->
    Get = fun(K, Default) ->
        case maps:get(K, Opts, undefined) of
            undefined -> maps:get(K, Config, Default);
            V -> V
        end
    end,
    #{
        max_retries => Get(max_retries, ?DEFAULT_MAX_RETRIES),
        retry_delay => Get(retry_delay, ?DEFAULT_RETRY_DELAY),
        on_retry => Get(on_retry, undefined)
    }.

%% @doc 执行请求函数，失败且可重试时按退避策略重试
%%
%% 共尝试 max_retries + 1 次；最后一次的结果（无论成败）直接返回。
-spec run(fun(() -> {ok, term()} | {error, term()}), retry_opts()) ->
    {ok, term()} | {error, term()}.
run(Fun, RetryOpts) ->
    run(Fun, RetryOpts, 0).

run(Fun, #{max_retries := Max}, Attempt) when Attempt >= Max ->
    Fun();
run(Fun, RetryOpts, Attempt) ->
    case Fun() of
        {ok, _} = Success ->
            Success;
        {error, Reason} = Error ->
            case is_retryable(Reason) of
                true ->
                    Delay = compute_delay(RetryOpts, Attempt, Reason),
                    invoke_retry_callback(RetryOpts, #{
                        attempt => Attempt + 1,
                        max_retries => maps:get(max_retries, RetryOpts),
                        error => Reason,
                        delay => Delay
                    }),
                    timer:sleep(Delay),
                    run(Fun, RetryOpts, Attempt + 1);
                false ->
                    Error
            end
    end.

%% @doc 是否可重试（统一委托给 beamai_llm_error 分类）
-spec is_retryable(term()) -> boolean().
is_retryable(Reason) ->
    beamai_llm_error:retryable(beamai_llm_error:from_reason(Reason)).

%% @doc 计算退避时长
%% 错误携带 Retry-After（服务端建议）时按其退避（上限 ?MAX_RETRY_DELAY），
%% 否则使用线性退避 retry_delay * (Attempt + 1)。
-spec compute_delay(map(), non_neg_integer(), term()) -> non_neg_integer().
compute_delay(RetryOpts, Attempt, Reason) ->
    case beamai_llm_error:retry_after_ms(beamai_llm_error:from_reason(Reason)) of
        Ms when is_integer(Ms), Ms > 0 ->
            min(Ms, ?MAX_RETRY_DELAY);
        _ ->
            maps:get(retry_delay, RetryOpts) * (Attempt + 1)
    end.

%%====================================================================
%% 内部函数
%%====================================================================

invoke_retry_callback(#{on_retry := undefined}, _) -> ok;
invoke_retry_callback(#{on_retry := Callback}, RetryState) when is_function(Callback) ->
    try Callback(RetryState)
    catch _:_ -> ok
    end;
invoke_retry_callback(_, _) -> ok.
