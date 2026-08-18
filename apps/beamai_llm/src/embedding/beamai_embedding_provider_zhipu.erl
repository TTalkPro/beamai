%%%-------------------------------------------------------------------
%%% @doc 智谱 AI (Zhipu/BigModel) Embedding Provider 实现
%%%
%%% OpenAI 兼容的向量化接口。
%%% API 文档: https://docs.bigmodel.cn/api-reference/
%%%
%%% 模型：
%%%   - embedding-3（默认，2048 维，支持 dimensions 降维）
%%%   - embedding-2（1024 维）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embedding_provider_zhipu).
-behaviour(beamai_embedding_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([embed/2, max_batch_size/0, supports_dimensions/0]).

-define(BASE_URL, <<"https://open.bigmodel.cn">>).
-define(ENDPOINT, <<"/api/paas/v4/embeddings">>).
-define(MODEL, <<"embedding-3">>).

name() -> <<"Zhipu Embeddings">>.

default_config() ->
    #{
        base_url => ?BASE_URL,
        model => ?MODEL,
        timeout => beamai_llm_provider_common:default_timeout(zhipu)
    }.

validate_config(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_config(_) ->
    {error, missing_api_key}.

%% 智谱单次请求上限 64 条
max_batch_size() -> 64.

supports_dimensions() -> true.

%% @doc 执行向量化请求
embed(Config, Request) ->
    Url = beamai_llm_provider_common:build_url(Config, ?ENDPOINT, ?BASE_URL),
    Headers = beamai_embedding_common:build_headers(Config),
    Body = beamai_embedding_common:build_openai_body(Config, Request, ?MODEL),
    Opts = beamai_llm_provider_common:with_pool_opt(
        #{timeout => beamai_llm_provider_common:request_timeout(Config, zhipu)}, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_embedding_common:parser_openai(zhipu)).
