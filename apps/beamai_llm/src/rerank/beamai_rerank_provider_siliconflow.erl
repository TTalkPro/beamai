%%%-------------------------------------------------------------------
%%% @doc SiliconFlow（硅基流动）Rerank Provider 实现
%%%
%%% API 文档: https://docs.siliconflow.cn/api-reference/rerank/create-rerank
%%%
%%% 常用模型：
%%%   - BAAI/bge-reranker-v2-m3（默认，中英文通用）
%%%   - Qwen/Qwen3-Reranker-8B
%%%
%%% 特有参数：max_chunks_per_doc / overlap_tokens（长文档切块策略）
%%% 站点区域同对话 Provider：`cn'（默认）/ `global'。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rerank_provider_siliconflow).
-behaviour(beamai_rerank_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([rerank/2, max_documents/0]).

-define(BASE_URL_CN, <<"https://api.siliconflow.cn">>).
-define(BASE_URL_GLOBAL, <<"https://api.siliconflow.com">>).
-define(ENDPOINT, <<"/v1/rerank">>).
-define(MODEL, <<"BAAI/bge-reranker-v2-m3">>).

name() -> <<"SiliconFlow Rerank">>.

default_config() ->
    #{
        region => cn,
        model => ?MODEL,
        timeout => beamai_llm_provider_common:default_timeout(siliconflow)
    }.

validate_config(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_config(_) ->
    {error, missing_api_key}.

max_documents() -> 1000.

%% @doc 执行重排序请求
rerank(Config, Request) ->
    Default = region_base_url(maps:get(region, Config, cn)),
    Url = beamai_llm_provider_common:build_url(Config, ?ENDPOINT, Default),
    Headers = beamai_rerank_common:build_headers(Config),
    Body = beamai_rerank_common:build_body(Config, Request, ?MODEL),
    Opts = beamai_llm_provider_common:with_pool_opt(
        #{timeout => beamai_llm_provider_common:request_timeout(Config, siliconflow)}, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_rerank_common:parser_results(siliconflow)).

%% @private 区域对应的站点
region_base_url(global) -> ?BASE_URL_GLOBAL;
region_base_url(_) -> ?BASE_URL_CN.
