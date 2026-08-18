%%%-------------------------------------------------------------------
%%% @doc Voyage AI Rerank Provider 实现
%%%
%%% API 文档: https://docs.voyageai.com/reference/reranker-api
%%%
%%% 模型：
%%%   - rerank-2.5（默认）
%%%   - rerank-2.5-lite
%%%
%%% 注意：Voyage 的截断参数名为 `top_k'（其余厂商为 `top_n'），
%%% 结果数组名为 `data'（其余厂商为 `results'）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rerank_provider_voyage).
-behaviour(beamai_rerank_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([rerank/2, max_documents/0]).

-define(BASE_URL, <<"https://api.voyageai.com">>).
-define(ENDPOINT, <<"/v1/rerank">>).
-define(MODEL, <<"rerank-2.5">>).

name() -> <<"Voyage Rerank">>.

default_config() ->
    #{
        base_url => ?BASE_URL,
        model => ?MODEL,
        timeout => beamai_llm_provider_common:default_timeout(voyage)
    }.

validate_config(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_config(_) ->
    {error, missing_api_key}.

max_documents() -> 1000.

%% @doc 执行重排序请求
rerank(Config, Request) ->
    Url = beamai_llm_provider_common:build_url(Config, ?ENDPOINT, ?BASE_URL),
    Headers = beamai_rerank_common:build_headers(Config),
    Body = beamai_rerank_common:build_body(Config, Request, ?MODEL, <<"top_k">>),
    Opts = beamai_llm_provider_common:with_pool_opt(
        #{timeout => beamai_llm_provider_common:request_timeout(Config, voyage)}, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_rerank_common:parser_data(voyage)).
