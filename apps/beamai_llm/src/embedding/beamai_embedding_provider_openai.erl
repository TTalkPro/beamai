%%%-------------------------------------------------------------------
%%% @doc OpenAI Embedding Provider 实现
%%%
%%% 支持 OpenAI Embeddings API 及兼容接口（Azure OpenAI、one-api 等）。
%%% API 文档: https://platform.openai.com/docs/api-reference/embeddings
%%%
%%% 模型：
%%%   - text-embedding-3-small（默认，1536 维，支持降维）
%%%   - text-embedding-3-large（3072 维，支持降维）
%%%   - text-embedding-ada-002（1536 维，不支持降维）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embedding_provider_openai).
-behaviour(beamai_embedding_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([embed/2, max_batch_size/0, supports_dimensions/0]).

-define(BASE_URL, <<"https://api.openai.com">>).
-define(ENDPOINT, <<"/v1/embeddings">>).
-define(MODEL, <<"text-embedding-3-small">>).

name() -> <<"OpenAI Embeddings">>.

default_config() ->
    #{
        base_url => ?BASE_URL,
        model => ?MODEL,
        encoding_format => <<"float">>,
        timeout => beamai_llm_provider_common:default_timeout(openai)
    }.

validate_config(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_config(_) ->
    {error, missing_api_key}.

%% OpenAI 单次请求上限 2048 条
max_batch_size() -> 2048.

supports_dimensions() -> true.

%% @doc 执行向量化请求
embed(Config, Request) ->
    Url = beamai_llm_provider_common:build_url(Config, ?ENDPOINT, ?BASE_URL),
    Headers = beamai_embedding_common:build_headers(Config),
    Body = beamai_embedding_common:build_openai_body(Config, Request, ?MODEL),
    Opts = beamai_llm_provider_common:with_pool_opt(
        #{timeout => beamai_llm_provider_common:request_timeout(Config, openai)}, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_embedding_common:parser_openai(openai)).
