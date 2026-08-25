%%%-------------------------------------------------------------------
%%% @doc LLM 层内置 filter 集（llm 链，around_llm）
%%%
%%% around_llm 是包裹**一次真实 LLM 请求**的那层洋葱（嵌在 chat 链之内，见
%%% beamai_filter 的层次图）。「一轮内可能发生多次」的逻辑归这层：重试、
%%% fallback 换模型、限流、mock。「每轮只该一次」的逻辑（记忆、记账、审计）
%%% 归外面的 around_chat。
%%%
%%% 本模块放在 beamai_llm 而非 beamai_core：重试判定依赖 beamai_llm_error 的
%%% 错误分类，而 core 不能反向依赖 llm（会成环）。kernel 以运行时探测的方式
%%% 缺省注入 retry_filter（见 beamai_kernel:default_llm_filters/1）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_filters).

-export([retry_filter/0, retry_filter/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 重试 filter（around_llm；框架默认重试参数）
-spec retry_filter() -> beamai_filter:filter().
retry_filter() ->
    retry_filter(#{}).

%% @doc 重试 filter（around_llm；指定默认重试参数）
%%
%% 失败且可重试（429 / 5xx / 网络超时，判定见 beamai_llm_error）时按退避重入
%% `Next`——**每次重入都是一次真实请求**，故层序上它内层的 around_llm filter
%% 会被跑 N 次、外层的只跑一次。这正是把重试从 chat 链下沉到 llm 链的目的：
%% 记忆/记账那些 around_chat filter 不会跟着重跑。
%%
%% 重试参数（max_retries / retry_delay / on_retry）取自单次 chat 的 Opts，缺失
%% 项用本 filter 构造时给的 Defaults 兜底，再缺用框架默认值（见 beamai_llm_retry）。
%%
%% **流式路径不介入**：Req 带 `stream => true` 时直接透传——token 已经投递给
%% sink 了，重跑会让下游看到重复内容。流式要容错请在 around_chat 层做（那层
%% 可控地重跑整轮），或由使用方自己写一个懂 sink 语义的 around_llm filter。
-spec retry_filter(map()) -> beamai_filter:filter().
retry_filter(Defaults) when is_map(Defaults) ->
    beamai_filter:new(<<"llm_retry">>, #{
        around_llm => fun(Req, _FCtx, Next) ->
            case maps:get(stream, Req, false) of
                true ->
                    Next(Req);
                false ->
                    RetryOpts = beamai_llm_retry:opts(
                                  maps:merge(Defaults, maps:get(opts, Req, #{}))),
                    run_with_retry(Req, Next, RetryOpts)
            end
        end
    }).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 在 throw 契约与 beamai_llm_retry 的 {ok,_}|{error,_} 契约之间转换
%%
%% 链的 terminal 用 throw 报错（beamai_filter_chain 在最外层统一捕获），而
%% beamai_llm_retry:run/2 要的是返回值形态的错误，故此处就地互转：内层 throw
%% 转成 {error,Reason} 交给重试判定，重试耗尽后再原样 throw 出去。
run_with_retry(Req, Next, RetryOpts) ->
    Attempt = fun() ->
        try {ok, Next(Req)}
        catch throw:Reason -> {error, Reason}
        end
    end,
    case beamai_llm_retry:run(Attempt, RetryOpts) of
        {ok, Response} -> Response;
        {error, Reason} -> throw(Reason)
    end.
