%%%-------------------------------------------------------------------
%%% @doc Rerank 子系统单元测试
%%%
%%% 覆盖：
%%%   - 门面 API（create / rerank / rerank_full / 结果工具）
%%%   - 文档回填、条数上限、空输入短路
%%%   - 请求体构建（通用 top_n / Voyage top_k / DashScope 原生结构）
%%%   - 三类响应解析（results / data / DashScope）与 usage 归一
%%%
%%% 测试模块自身实现 Rerank Provider 回调，作为可观测的假 Provider
%%% （经 {custom, Module} 注入），无需网络即可验证参数构建与回填。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rerank_tests).

-include_lib("eunit/include/eunit.hrl").

%% 假 Provider 回调（供 {custom, ?MODULE} 使用）
-export([name/0, default_config/0, validate_config/1]).
-export([rerank/2, max_documents/0]).

-define(DOCS, [<<"红烧肉的做法"/utf8>>, <<"Erlang 进程模型"/utf8>>, <<"OTP 监督树"/utf8>>]).

%%====================================================================
%% 门面 API（mock provider）
%%====================================================================

mock_rerank_test() ->
    Config = beamai_rerank:create(mock, #{}),
    Docs = [<<"cats and dogs">>, <<"erlang otp supervision">>, <<"otp release">>],
    {ok, Results} = beamai_rerank:rerank(Config, <<"erlang otp">>, Docs),
    ?assertEqual(3, length(Results)),
    %% 词重叠最高的文档排第一
    [#{index := First} | _] = Results,
    ?assertEqual(1, First),
    %% 降序
    Scores = beamai_rerank:scores(Results),
    ?assertEqual(lists:reverse(lists:sort(Scores)), Scores).

mock_top_n_test() ->
    Config = beamai_rerank:create(mock, #{}),
    {ok, Results} = beamai_rerank:rerank(Config, <<"otp">>,
                                         [<<"a otp">>, <<"b">>, <<"c otp">>], #{top_n => 2}),
    ?assertEqual(2, length(Results)).

documents_backfilled_test() ->
    Config = beamai_rerank:create(mock, #{}),
    {ok, Results} = beamai_rerank:rerank(Config, <<"OTP">>, ?DOCS),
    %% mock Provider 不回传原文，门面按 index 回填
    ?assert(lists:all(fun(R) -> maps:is_key(document, R) end, Results)),
    ?assertEqual(lists:sort(?DOCS), lists:sort(beamai_rerank:documents(Results))).

indices_and_scores_test() ->
    Config = beamai_rerank:create(mock, #{}),
    {ok, Results} = beamai_rerank:rerank(Config, <<"otp">>, [<<"otp">>, <<"x">>]),
    ?assertEqual([0, 1], lists:sort(beamai_rerank:indices(Results))),
    ?assertEqual(2, length(beamai_rerank:scores(Results))).

rerank_full_usage_test() ->
    Config = beamai_rerank:create(mock, #{}),
    {ok, Response} = beamai_rerank:rerank_full(Config, <<"a b">>, [<<"a">>]),
    ?assertEqual(mock, maps:get(provider, Response)),
    ?assertMatch(#{input_tokens := _, total_tokens := _}, maps:get(usage, Response)).

empty_documents_makes_no_request_test() ->
    reset_fake(),
    Config = beamai_rerank:create({custom, ?MODULE}, #{}),
    ?assertEqual({ok, []}, beamai_rerank:rerank(Config, <<"q">>, [])),
    ?assertEqual(undefined, get(fake_calls)).

too_many_documents_test() ->
    reset_fake(),
    put(fake_max_documents, 2),
    Config = beamai_rerank:create({custom, ?MODULE}, #{}),
    ?assertEqual({error, {too_many_documents, 3, 2}},
                 beamai_rerank:rerank(Config, <<"q">>, ?DOCS)),
    %% 超限时不发请求
    ?assertEqual(undefined, get(fake_calls)).

provider_routing_test() ->
    ?assertEqual(beamai_rerank_provider_siliconflow, beamai_rerank:provider_module(siliconflow)),
    ?assertEqual(beamai_rerank_provider_dashscope, beamai_rerank:provider_module(dashscope)),
    ?assertEqual(beamai_rerank_provider_jina, beamai_rerank:provider_module(jina)),
    ?assertEqual(beamai_rerank_provider_cohere, beamai_rerank:provider_module(cohere)),
    ?assertEqual(beamai_rerank_provider_voyage, beamai_rerank:provider_module(voyage)),
    ?assertEqual(?MODULE, beamai_rerank:provider_module({custom, ?MODULE})).

default_models_test() ->
    Model = fun(P) -> maps:get(model, beamai_rerank:create(P, #{api_key => <<"k">>})) end,
    ?assertEqual(<<"BAAI/bge-reranker-v2-m3">>, Model(siliconflow)),
    ?assertEqual(<<"gte-rerank-v2">>, Model(dashscope)),
    ?assertEqual(<<"jina-reranker-v2-base-multilingual">>, Model(jina)),
    ?assertEqual(<<"rerank-v3.5">>, Model(cohere)),
    ?assertEqual(<<"rerank-2.5">>, Model(voyage)).

validate_config_test() ->
    ?assertEqual({error, missing_api_key},
                 beamai_rerank:validate(beamai_rerank:create(siliconflow, #{}))),
    ?assertEqual(ok,
                 beamai_rerank:validate(beamai_rerank:create(cohere, #{api_key => <<"k">>}))).

%%====================================================================
%% 请求参数（假 Provider）
%%====================================================================

request_params_passthrough_test() ->
    reset_fake(),
    Config = beamai_rerank:create({custom, ?MODULE}, #{return_documents => true}),
    {ok, _} = beamai_rerank:rerank(Config, <<"q">>, ?DOCS, #{top_n => 2}),
    [Request] = get(fake_calls),
    ?assertEqual(<<"q">>, maps:get(query, Request)),
    ?assertEqual(?DOCS, maps:get(documents, Request)),
    ?assertEqual(2, maps:get(top_n, Request)),
    ?assert(maps:get(return_documents, Request)).

provider_documents_not_overwritten_test() ->
    reset_fake(),
    put(fake_return_documents, true),
    Config = beamai_rerank:create({custom, ?MODULE}, #{}),
    {ok, [First | _]} = beamai_rerank:rerank(Config, <<"q">>, ?DOCS),
    %% Provider 已回传原文时保持其内容（带标记后缀）
    ?assertMatch(<<_/binary>>, maps:get(document, First)),
    ?assert(binary:match(maps:get(document, First), <<"[srv]">>) =/= nomatch).

error_propagates_test() ->
    reset_fake(),
    put(fake_error, {http_error, 401, <<"unauthorized">>}),
    Config = beamai_rerank:create({custom, ?MODULE}, #{}),
    ?assertEqual({error, {http_error, 401, <<"unauthorized">>}},
                 beamai_rerank:rerank(Config, <<"q">>, ?DOCS, #{max_retries => 0})).

%%====================================================================
%% 请求体构建
%%====================================================================

common_body_test() ->
    Config = beamai_rerank:create(siliconflow, #{api_key => <<"k">>}),
    Body = beamai_rerank_common:build_body(
        Config, #{query => <<"q">>, documents => [<<"a">>], top_n => 3}, <<"m">>),
    ?assertEqual(<<"BAAI/bge-reranker-v2-m3">>, maps:get(<<"model">>, Body)),
    ?assertEqual(<<"q">>, maps:get(<<"query">>, Body)),
    ?assertEqual([<<"a">>], maps:get(<<"documents">>, Body)),
    ?assertEqual(3, maps:get(<<"top_n">>, Body)),
    ?assertNot(maps:is_key(<<"return_documents">>, Body)).

voyage_uses_top_k_test() ->
    Config = beamai_rerank:create(voyage, #{api_key => <<"k">>}),
    Body = beamai_rerank_common:build_body(
        Config, #{query => <<"q">>, documents => [<<"a">>], top_n => 3},
        <<"rerank-2.5">>, <<"top_k">>),
    ?assertEqual(3, maps:get(<<"top_k">>, Body)),
    ?assertNot(maps:is_key(<<"top_n">>, Body)).

dashscope_native_body_test() ->
    Config = beamai_rerank:create(dashscope, #{api_key => <<"k">>}),
    Body = beamai_rerank_provider_dashscope:build_native_body(
        Config, #{query => <<"q">>, documents => [<<"a">>, <<"b">>], top_n => 1}),
    ?assertEqual(<<"gte-rerank-v2">>, maps:get(<<"model">>, Body)),
    ?assertEqual(#{<<"query">> => <<"q">>, <<"documents">> => [<<"a">>, <<"b">>]},
                 maps:get(<<"input">>, Body)),
    ?assertEqual(#{<<"top_n">> => 1}, maps:get(<<"parameters">>, Body)).

headers_test() ->
    Headers = beamai_rerank_common:build_headers(#{api_key => <<"k">>}),
    ?assertEqual(<<"Bearer k">>, proplists:get_value(<<"Authorization">>, Headers)),
    NoKey = beamai_rerank_common:build_headers(#{}),
    ?assertEqual(undefined, proplists:get_value(<<"Authorization">>, NoKey)).

%%====================================================================
%% 响应解析
%%====================================================================

parse_results_style_test() ->
    Raw = #{
        <<"id">> => <<"r-1">>,
        <<"model">> => <<"BAAI/bge-reranker-v2-m3">>,
        <<"results">> => [
            #{<<"index">> => 2, <<"relevance_score">> => 0.2},
            #{<<"index">> => 0, <<"relevance_score">> => 0.9,
              <<"document">> => #{<<"text">> => <<"doc0">>}}
        ],
        <<"tokens">> => #{<<"input_tokens">> => 30, <<"output_tokens">> => 0}
    },
    {ok, Response} = beamai_rerank_common:from_results(Raw, siliconflow),
    %% 服务端顺序不可信，统一按得分降序
    ?assertEqual([0, 2], beamai_rerank:indices(maps:get(results, Response))),
    [First | _] = maps:get(results, Response),
    ?assertEqual(<<"doc0">>, maps:get(document, First)),
    ?assertEqual(30, maps:get(input_tokens, maps:get(usage, Response))),
    ?assertEqual(<<"r-1">>, maps:get(id, maps:get(metadata, Response))).

parse_data_style_test() ->
    Raw = #{
        <<"data">> => [
            #{<<"index">> => 0, <<"relevance_score">> => 0.4},
            #{<<"index">> => 1, <<"relevance_score">> => 0.8}
        ],
        <<"usage">> => #{<<"total_tokens">> => 26}
    },
    {ok, Response} = beamai_rerank_common:from_data(Raw, voyage),
    ?assertEqual([1, 0], beamai_rerank:indices(maps:get(results, Response))),
    ?assertEqual(26, maps:get(total_tokens, maps:get(usage, Response))).

parse_dashscope_test() ->
    Raw = #{
        <<"request_id">> => <<"req-9">>,
        <<"output">> => #{<<"results">> => [
            #{<<"index">> => 1, <<"relevance_score">> => 0.7,
              <<"document">> => #{<<"text">> => <<"doc1">>}}
        ]},
        <<"usage">> => #{<<"total_tokens">> => 12}
    },
    {ok, Response} = beamai_rerank_common:from_dashscope(Raw, dashscope),
    ?assertEqual([1], beamai_rerank:indices(maps:get(results, Response))),
    ?assertEqual(12, maps:get(total_tokens, maps:get(usage, Response))),
    ?assertEqual(<<"req-9">>, maps:get(request_id, maps:get(metadata, Response))).

parse_cohere_search_units_test() ->
    Raw = #{
        <<"results">> => [#{<<"index">> => 0, <<"relevance_score">> => 1}],
        <<"meta">> => #{<<"billed_units">> => #{<<"search_units">> => 1}}
    },
    {ok, Response} = beamai_rerank_common:from_results(Raw, cohere),
    %% 整数得分归一为 float
    ?assertEqual([1.0], beamai_rerank:scores(maps:get(results, Response))),
    ?assertEqual(1, maps:get(search_units, maps:get(details, maps:get(usage, Response)))).

parse_error_test() ->
    ?assertMatch({error, {api_error, _}},
                 beamai_rerank_common:from_results(#{<<"error">> => #{}}, jina)),
    ?assertMatch({error, {invalid_response, _}},
                 beamai_rerank_common:from_results(#{<<"junk">> => 1}, jina)),
    ?assertMatch({error, {api_error, _}},
                 beamai_rerank_common:from_dashscope(
                     #{<<"code">> => <<"InvalidApiKey">>, <<"message">> => <<"bad">>}, dashscope)).

out_of_range_index_kept_as_is_test() ->
    %% 服务端返回越界 index 时不猜测文档，只保留 index/score
    reset_fake(),
    put(fake_index_override, 99),
    Config = beamai_rerank:create({custom, ?MODULE}, #{}),
    {ok, [Result]} = beamai_rerank:rerank(Config, <<"q">>, [<<"only">>]),
    ?assertEqual(99, maps:get(index, Result)),
    ?assertNot(maps:is_key(document, Result)).

%%====================================================================
%% 假 Provider 实现
%%====================================================================

%% @private 重置假 Provider 的记录状态
reset_fake() ->
    erase(fake_calls),
    erase(fake_max_documents),
    erase(fake_error),
    erase(fake_return_documents),
    erase(fake_index_override),
    ok.

name() -> <<"Fake Rerank">>.

default_config() -> #{model => <<"fake">>}.

validate_config(_Config) -> ok.

max_documents() ->
    case get(fake_max_documents) of
        undefined -> 100;
        N -> N
    end.

%% @private 记录请求并按输入顺序打递减分
rerank(Config, Request) ->
    put(fake_calls, existing_calls() ++ [Request]),
    case get(fake_error) of
        undefined ->
            Documents = maps:get(documents, Request, []),
            Results = [make_result(Index, Doc)
                       || {Index, Doc} <- lists:zip(
                              lists:seq(0, length(Documents) - 1), Documents)],
            {ok, beamai_rerank_common:new_response(#{
                model => maps:get(model, Config, <<"fake">>),
                provider => fake,
                results => Results,
                usage => #{input_tokens => 1, total_tokens => 1},
                raw => #{}
            })};
        Error ->
            {error, Error}
    end.

make_result(Index, Doc) ->
    Base = #{index => index_of(Index), score => 1.0 / (Index + 1)},
    case get(fake_return_documents) of
        true -> Base#{document => <<Doc/binary, "[srv]">>};
        _ -> Base
    end.

index_of(Index) ->
    case get(fake_index_override) of
        undefined -> Index;
        Override -> Override
    end.

existing_calls() ->
    case get(fake_calls) of
        undefined -> [];
        Calls -> Calls
    end.
