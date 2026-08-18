%%%-------------------------------------------------------------------
%%% @doc Jina AI Rerank Provider 实现
%%%
%%% API 文档: https://jina.ai/reranker
%%%
%%% 模型：
%%%   - jina-reranker-v2-base-multilingual（默认，多语种）
%%%   - jina-colbert-v2
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rerank_provider_jina).
-behaviour(beamai_rerank_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([rerank/2, max_documents/0]).

-define(BASE_URL, <<"https://api.jina.ai">>).
-define(ENDPOINT, <<"/v1/rerank">>).
-define(MODEL, <<"jina-reranker-v2-base-multilingual">>).

name() -> <<"Jina Rerank">>.

default_config() ->
    #{
        base_url => ?BASE_URL,
        model => ?MODEL,
        timeout => beamai_llm_provider_common:default_timeout(jina)
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
    Body = beamai_rerank_common:build_body(Config, Request, ?MODEL),
    Opts = beamai_llm_provider_common:with_pool_opt(
        #{timeout => beamai_llm_provider_common:request_timeout(Config, jina)}, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_rerank_common:parser_results(jina)).
