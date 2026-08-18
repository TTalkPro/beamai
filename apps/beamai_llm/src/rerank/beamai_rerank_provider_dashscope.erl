%%%-------------------------------------------------------------------
%%% @doc 阿里云 DashScope（通义千问）Rerank Provider 实现
%%%
%%% 使用 DashScope 原生接口（与对话 / 向量化 Provider 的取舍一致）：
%%% /api/v1/services/rerank/text-rerank/text-rerank
%%%
%%% 请求体为 `#{input => #{query => ..., documents => [...]},
%%%             parameters => #{top_n => ..., return_documents => ...}}'。
%%%
%%% 模型：gte-rerank-v2（默认）
%%%
%%% API 文档: https://help.aliyun.com/zh/model-studio/text-rerank-api
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rerank_provider_dashscope).
-behaviour(beamai_rerank_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([rerank/2, max_documents/0]).

-ifdef(TEST).
-export([build_native_body/2]).
-endif.

-define(BASE_URL, <<"https://dashscope.aliyuncs.com">>).
-define(ENDPOINT, <<"/api/v1/services/rerank/text-rerank/text-rerank">>).
-define(MODEL, <<"gte-rerank-v2">>).

name() -> <<"DashScope Rerank">>.

default_config() ->
    #{
        base_url => ?BASE_URL,
        model => ?MODEL,
        timeout => beamai_llm_provider_common:default_timeout(dashscope)
    }.

validate_config(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_config(_) ->
    {error, missing_api_key}.

max_documents() -> 500.

%% @doc 执行重排序请求
rerank(Config, Request) ->
    Url = beamai_llm_provider_common:build_url(Config, ?ENDPOINT, ?BASE_URL),
    Headers = beamai_rerank_common:build_headers(Config),
    Body = build_native_body(Config, Request),
    Opts = beamai_llm_provider_common:with_pool_opt(
        #{timeout => beamai_llm_provider_common:request_timeout(Config, dashscope)}, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_rerank_common:parser_dashscope(dashscope)).

%% @private 构建原生模式请求体
build_native_body(Config, Request) ->
    Base = #{
        <<"model">> => maps:get(model, Config, ?MODEL),
        <<"input">> => #{
            <<"query">> => maps:get(query, Request, <<>>),
            <<"documents">> => maps:get(documents, Request, [])
        }
    },
    Params = lists:foldl(fun({Key, JsonKey}, Acc) ->
        case maps:get(Key, Request, maps:get(Key, Config, undefined)) of
            undefined -> Acc;
            Value -> Acc#{JsonKey => Value}
        end
    end, #{}, [{top_n, <<"top_n">>}, {return_documents, <<"return_documents">>}]),
    case map_size(Params) of
        0 -> Base;
        _ -> Base#{<<"parameters">> => Params}
    end.
