%%%-------------------------------------------------------------------
%%% @doc SiliconFlow（硅基流动）Embedding Provider 实现
%%%
%%% OpenAI 兼容的 /v1/embeddings 接口，模型名带厂商前缀。
%%% API 文档: https://docs.siliconflow.cn/api-reference/embeddings
%%%
%%% 常用模型：
%%%   - BAAI/bge-m3（默认，1024 维，中英文通用）
%%%   - BAAI/bge-large-zh-v1.5（1024 维，中文）
%%%   - Qwen/Qwen3-Embedding-8B（支持指定维度）
%%%
%%% 站点区域同对话 Provider：`cn'（默认）/ `global'。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embedding_provider_siliconflow).
-behaviour(beamai_embedding_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([embed/2, max_batch_size/0, supports_dimensions/0]).

-define(BASE_URL_CN, <<"https://api.siliconflow.cn">>).
-define(BASE_URL_GLOBAL, <<"https://api.siliconflow.com">>).
-define(ENDPOINT, <<"/v1/embeddings">>).
-define(MODEL, <<"BAAI/bge-m3">>).

name() -> <<"SiliconFlow Embeddings">>.

default_config() ->
    #{
        region => cn,
        model => ?MODEL,
        encoding_format => <<"float">>,
        timeout => beamai_llm_provider_common:default_timeout(siliconflow)
    }.

validate_config(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_config(_) ->
    {error, missing_api_key}.

%% 硅基流动单次请求上限 32 条
max_batch_size() -> 32.

supports_dimensions() -> true.

%% @doc 执行向量化请求
embed(Config, Request) ->
    Default = region_base_url(maps:get(region, Config, cn)),
    Url = beamai_llm_provider_common:build_url(Config, ?ENDPOINT, Default),
    Headers = beamai_embedding_common:build_headers(Config),
    Body = beamai_embedding_common:build_openai_body(Config, Request, ?MODEL),
    Opts = beamai_llm_provider_common:with_pool_opt(
        #{timeout => beamai_llm_provider_common:request_timeout(Config, siliconflow)}, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_embedding_common:parser_openai(siliconflow)).

%% @private 区域对应的站点
region_base_url(global) -> ?BASE_URL_GLOBAL;
region_base_url(_) -> ?BASE_URL_CN.
