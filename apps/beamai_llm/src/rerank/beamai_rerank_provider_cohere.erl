%%%-------------------------------------------------------------------
%%% @doc Cohere Rerank Provider 实现
%%%
%%% 使用 Cohere v2 重排序接口：POST /v2/rerank
%%% API 文档: https://docs.cohere.com/reference/rerank
%%%
%%% 模型：
%%%   - rerank-v3.5（默认，多语种）
%%%   - rerank-english-v3.0
%%%
%%% 说明：Cohere 不返回 token 统计，计费单元在
%%% `meta.billed_units.search_units'，解析后落在 usage.details.search_units。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rerank_provider_cohere).
-behaviour(beamai_rerank_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([rerank/2, max_documents/0]).

-define(BASE_URL, <<"https://api.cohere.com">>).
-define(ENDPOINT, <<"/v2/rerank">>).
-define(MODEL, <<"rerank-v3.5">>).

name() -> <<"Cohere Rerank">>.

default_config() ->
    #{
        base_url => ?BASE_URL,
        model => ?MODEL,
        timeout => beamai_llm_provider_common:default_timeout(cohere)
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
        #{timeout => beamai_llm_provider_common:request_timeout(Config, cohere)}, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_rerank_common:parser_results(cohere)).
