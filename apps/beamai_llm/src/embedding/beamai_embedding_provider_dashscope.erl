%%%-------------------------------------------------------------------
%%% @doc 阿里云 DashScope（通义千问）Embedding Provider 实现
%%%
%%% 支持两种 API 模式（与对话 Provider 的取舍一致，默认走原生）：
%%%
%%%   - `native'（默认）: /api/v1/services/embeddings/text-embedding/text-embedding
%%%     请求体为 `#{input => #{texts => [...]}, parameters => #{...}}'
%%%   - `compatible': OpenAI 兼容模式 /compatible-mode/v1/embeddings
%%%
%%% 模型：
%%%   - text-embedding-v4（默认，支持 dimension 降维与多语言）
%%%   - text-embedding-v3
%%%
%%% 特有参数：
%%%   - text_type: `<<"query">>' | `<<"document">>'（检索场景区分查询与文档）
%%%
%%% API 文档: https://help.aliyun.com/zh/model-studio/text-embedding-synchronous-api
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embedding_provider_dashscope).
-behaviour(beamai_embedding_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([embed/2, max_batch_size/0, supports_dimensions/0]).

-ifdef(TEST).
-export([build_native_body/2]).
-endif.

-define(BASE_URL, <<"https://dashscope.aliyuncs.com">>).
-define(NATIVE_ENDPOINT, <<"/api/v1/services/embeddings/text-embedding/text-embedding">>).
-define(COMPATIBLE_ENDPOINT, <<"/compatible-mode/v1/embeddings">>).
-define(MODEL, <<"text-embedding-v4">>).

name() -> <<"DashScope Embeddings">>.

default_config() ->
    #{
        base_url => ?BASE_URL,
        model => ?MODEL,
        api_mode => native,
        timeout => beamai_llm_provider_common:default_timeout(dashscope)
    }.

validate_config(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_config(_) ->
    {error, missing_api_key}.

%% DashScope 单次请求上限 10 条
max_batch_size() -> 10.

supports_dimensions() -> true.

%% @doc 执行向量化请求
embed(Config, Request) ->
    Headers = beamai_embedding_common:build_headers(Config),
    Opts = beamai_llm_provider_common:with_pool_opt(
        #{timeout => beamai_llm_provider_common:request_timeout(Config, dashscope)}, Config),
    case maps:get(api_mode, Config, native) of
        compatible ->
            Url = beamai_llm_provider_common:build_url(Config, ?COMPATIBLE_ENDPOINT, ?BASE_URL),
            Body = beamai_embedding_common:build_openai_body(Config, Request, ?MODEL),
            beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                           beamai_embedding_common:parser_openai(dashscope));
        _ ->
            Url = beamai_llm_provider_common:build_url(Config, ?NATIVE_ENDPOINT, ?BASE_URL),
            Body = build_native_body(Config, Request),
            beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                           beamai_embedding_common:parser_dashscope(dashscope))
    end.

%% @private 构建原生模式请求体
%% 原生接口的维度参数为 dimension（单数），与 OpenAI 的 dimensions 不同。
build_native_body(Config, Request) ->
    Base = #{
        <<"model">> => maps:get(model, Config, ?MODEL),
        <<"input">> => #{<<"texts">> => maps:get(input, Request, [])}
    },
    Params = lists:foldl(fun({Key, JsonKey}, Acc) ->
        case maps:get(Key, Request, maps:get(Key, Config, undefined)) of
            undefined -> Acc;
            Value -> Acc#{JsonKey => Value}
        end
    end, #{}, [{dimensions, <<"dimension">>}, {text_type, <<"text_type">>}]),
    case map_size(Params) of
        0 -> Base;
        _ -> Base#{<<"parameters">> => Params}
    end.
