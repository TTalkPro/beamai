%%%-------------------------------------------------------------------
%%% @doc HTTP provider 的统一调用实现
%%%
%%% provider 模块只**声明**自己的底层信息（base_url / endpoint / headers / body /
%%% parser / 流式累加器与 finalizer），"怎么把请求发出去"这件事全部收在这里：
%%% 超时取值、连接池路由、rate-limit 响应头解析、同步与流式两条路径的拼装。
%%%
%%% 于是每个 provider 的 `chat/2`、`stream_chat/3` 都退化成一行委托，新增一个
%%% provider 只需填那 7 个声明式回调（见 beamai_llm_provider_behaviour 的
%%% optional_callbacks）。
%%%
%%% 层次：重试等"更重的任务"在再上一层的 beamai_chat_model（filter 栈之下、
%%% 本模块之上）；本模块只负责一次请求怎么发。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_http_provider).

-export([chat/3, stream_chat/4, url/3]).

%%====================================================================
%% API
%%====================================================================

%% @doc 同步调用：拼 URL/headers/body → 发请求 → 用 provider 的 parser 归一
-spec chat(module(), map(), beamai_chat_request:t()) -> {ok, map()} | {error, term()}.
chat(Module, Config, Request) ->
    beamai_llm_http_client:request(
        url(Module, Config, Request),
        Module:headers(Config, Request),
        Module:body(Config, Request),
        transport_opts(Config),
        Module:parser(Config)).

%% @doc 流式调用：stream 标记写进请求选项，累加器与 finalizer 由 provider 给出
-spec stream_chat(module(), map(), beamai_chat_request:t(), fun((term()) -> ok)) ->
    {ok, map()} | {error, term()}.
stream_chat(Module, Config, Request0, Callback) ->
    Request = beamai_chat_request:put_option(Request0, stream, true),
    Opts = (transport_opts(Config))#{finalizer => Module:stream_finalizer(Config)},
    beamai_llm_http_client:stream_request(
        url(Module, Config, Request),
        Module:headers(Config, Request),
        Module:body(Config, Request),
        Opts,
        Callback,
        Module:stream_accumulator(Config)).

%% @doc 完整 URL：Config 的 base_url 优先，否则用 provider 声明的默认值
-spec url(module(), map(), beamai_chat_request:t()) -> binary().
url(Module, Config, Request) ->
    beamai_llm_provider_common:build_url(
        Config, Module:endpoint(Config, Request), Module:base_url(Config)).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 传输层选项：超时（Config 优先，否则 provider 默认）+ 连接池路由 +
%% rate-limit 响应头解析。这三件事对所有 HTTP provider 一致，不该各写一遍。
transport_opts(Config) ->
    beamai_llm_provider_common:with_pool_opt(#{
        timeout => beamai_llm_provider_common:request_timeout(Config, provider_of(Config)),
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    }, Config).

%% @private 取 provider 原子（{custom, Mod} 走通用默认超时）
provider_of(Config) ->
    case maps:get(provider, Config, undefined) of
        P when is_atom(P) -> P;
        _ -> undefined
    end.
