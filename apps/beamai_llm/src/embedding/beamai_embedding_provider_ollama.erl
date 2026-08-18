%%%-------------------------------------------------------------------
%%% @doc Ollama 本地 Embedding Provider 实现
%%%
%%% 使用 Ollama 原生 /api/embed 接口（批量），无需 API Key。
%%% API 文档: https://github.com/ollama/ollama/blob/main/docs/api.md
%%%
%%% 常用模型：
%%%   - nomic-embed-text（默认，768 维）
%%%   - bge-m3 / mxbai-embed-large / all-minilm
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embedding_provider_ollama).
-behaviour(beamai_embedding_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([embed/2, max_batch_size/0, supports_dimensions/0]).

-define(BASE_URL, <<"http://localhost:11434">>).
-define(ENDPOINT, <<"/api/embed">>).
-define(MODEL, <<"nomic-embed-text">>).

name() -> <<"Ollama Embeddings">>.

default_config() ->
    #{
        base_url => ?BASE_URL,
        model => ?MODEL,
        timeout => beamai_llm_provider_common:default_timeout(ollama)
    }.

validate_config(Config) ->
    case maps:get(model, Config, undefined) of
        undefined -> {error, missing_model};
        _ -> ok
    end.

%% 本地推理无硬性上限，取保守值以控制单次请求体积
max_batch_size() -> 64.

%% Ollama 输出维度由模型决定，不支持 dimensions 参数
supports_dimensions() -> false.

%% @doc 执行向量化请求
embed(Config, Request) ->
    Url = beamai_llm_provider_common:build_url(Config, ?ENDPOINT, ?BASE_URL),
    Headers = beamai_embedding_common:build_headers(Config),
    Body = build_body(Config, Request),
    Opts = beamai_llm_provider_common:with_pool_opt(
        #{timeout => beamai_llm_provider_common:request_timeout(Config, ollama)}, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_embedding_common:parser_ollama(ollama)).

%% @private 构建请求体（truncate / keep_alive 可选透传）
build_body(Config, Request) ->
    Base = #{
        <<"model">> => maps:get(model, Config, ?MODEL),
        <<"input">> => maps:get(input, Request, [])
    },
    lists:foldl(fun({Key, JsonKey}, Acc) ->
        case maps:get(Key, Request, maps:get(Key, Config, undefined)) of
            undefined -> Acc;
            Value -> Acc#{JsonKey => Value}
        end
    end, Base, [{truncate, <<"truncate">>}, {keep_alive, <<"keep_alive">>},
                {options, <<"options">>}]).
