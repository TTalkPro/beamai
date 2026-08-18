%%%-------------------------------------------------------------------
%%% @doc Embedding 子系统单元测试
%%%
%%% 覆盖：
%%%   - 门面 API（create / embed / embed_many / embed_full）
%%%   - 自动分批与响应合并（顺序、usage 累加）
%%%   - 请求参数透传与 dimensions 能力门控
%%%   - 各响应格式解析（OpenAI 兼容 / Ollama / DashScope / base64 向量）
%%%   - 向量工具（余弦相似度、归一化）
%%%
%%% 测试模块自身实现 Embedding Provider 回调，作为可观测的假 Provider
%%% （经 {custom, Module} 注入），无需网络即可验证分批与参数构建。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embedding_tests).

-include_lib("eunit/include/eunit.hrl").

%% 假 Provider 回调（供 {custom, ?MODULE} 使用）
-export([name/0, default_config/0, validate_config/1]).
-export([embed/2, max_batch_size/0, supports_dimensions/0]).

%%====================================================================
%% 门面 API（mock provider）
%%====================================================================

mock_embed_single_test() ->
    Config = beamai_embedding:create(mock, #{dimensions => 16}),
    {ok, Vector} = beamai_embedding:embed(Config, <<"你好"/utf8>>),
    ?assertEqual(16, length(Vector)),
    ?assert(lists:all(fun(X) -> is_float(X) end, Vector)).

mock_embed_deterministic_test() ->
    Config = beamai_embedding:create(mock, #{}),
    {ok, V1} = beamai_embedding:embed(Config, <<"same">>),
    {ok, V2} = beamai_embedding:embed(Config, <<"same">>),
    {ok, V3} = beamai_embedding:embed(Config, <<"other">>),
    ?assertEqual(V1, V2),
    ?assertNotEqual(V1, V3).

mock_embed_many_test() ->
    Config = beamai_embedding:create(mock, #{dimensions => 4}),
    {ok, Vectors} = beamai_embedding:embed_many(Config, [<<"a">>, <<"b">>, <<"c">>]),
    ?assertEqual(3, length(Vectors)),
    ?assert(lists:all(fun(V) -> length(V) =:= 4 end, Vectors)).

embed_full_usage_test() ->
    Config = beamai_embedding:create(mock, #{}),
    {ok, Response} = beamai_embedding:embed_full(Config, [<<"hello world">>]),
    ?assertEqual(mock, maps:get(provider, Response)),
    ?assertMatch(#{input_tokens := _, total_tokens := _}, maps:get(usage, Response)).

empty_input_makes_no_request_test() ->
    %% 空输入直接返回空响应，不触发 Provider 调用
    reset_fake(),
    Config = beamai_embedding:create({custom, ?MODULE}, #{model => <<"fake">>}),
    ?assertEqual({ok, []}, beamai_embedding:embed_many(Config, [])),
    ?assertEqual(undefined, get(fake_calls)).

provider_routing_test() ->
    ?assertEqual(beamai_embedding_provider_openai, beamai_embedding:provider_module(openai)),
    ?assertEqual(beamai_embedding_provider_dashscope, beamai_embedding:provider_module(dashscope)),
    ?assertEqual(beamai_embedding_provider_zhipu, beamai_embedding:provider_module(zhipu)),
    ?assertEqual(beamai_embedding_provider_siliconflow,
                 beamai_embedding:provider_module(siliconflow)),
    ?assertEqual(beamai_embedding_provider_ollama, beamai_embedding:provider_module(ollama)),
    ?assertEqual(?MODULE, beamai_embedding:provider_module({custom, ?MODULE})).

validate_config_test() ->
    ?assertEqual({error, missing_api_key},
                 beamai_embedding:validate(beamai_embedding:create(openai, #{}))),
    ?assertEqual(ok,
                 beamai_embedding:validate(beamai_embedding:create(openai, #{api_key => <<"k">>}))),
    %% Ollama 无需 API Key
    ?assertEqual(ok, beamai_embedding:validate(beamai_embedding:create(ollama, #{}))).

%%====================================================================
%% 分批与合并（假 Provider）
%%====================================================================

auto_batching_preserves_order_test() ->
    reset_fake(),
    put(fake_max_batch, 2),
    Config = beamai_embedding:create({custom, ?MODULE}, #{}),
    Texts = [<<"a">>, <<"bb">>, <<"ccc">>, <<"dddd">>, <<"eeeee">>],
    {ok, Response} = beamai_embedding:embed_full(Config, Texts),
    %% 假 Provider 用文本长度作为向量值，可直接验证顺序
    ?assertEqual([[1.0], [2.0], [3.0], [4.0], [5.0]], maps:get(embeddings, Response)),
    %% 5 条 / 每批 2 条 = 3 批
    ?assertEqual(3, length(get(fake_calls))),
    ?assertEqual(3, maps:get(batches, maps:get(metadata, Response))),
    %% usage 累加
    ?assertEqual(5, maps:get(total_tokens, maps:get(usage, Response))).

batch_size_option_capped_by_provider_test() ->
    reset_fake(),
    put(fake_max_batch, 2),
    Config = beamai_embedding:create({custom, ?MODULE}, #{}),
    %% 请求 10 条/批，但 Provider 上限为 2
    {ok, _} = beamai_embedding:embed_many(Config, [<<"a">>, <<"b">>, <<"c">>], #{batch_size => 10}),
    ?assertEqual(2, length(get(fake_calls))).

single_batch_no_metadata_noise_test() ->
    reset_fake(),
    put(fake_max_batch, 10),
    Config = beamai_embedding:create({custom, ?MODULE}, #{}),
    {ok, Response} = beamai_embedding:embed_full(Config, [<<"a">>, <<"b">>]),
    ?assertEqual(1, length(get(fake_calls))),
    ?assertNot(maps:is_key(batches, maps:get(metadata, Response))).

request_params_passthrough_test() ->
    reset_fake(),
    Config = beamai_embedding:create({custom, ?MODULE}, #{dimensions => 128}),
    {ok, _} = beamai_embedding:embed_many(Config, [<<"a">>], #{text_type => <<"query">>}),
    [Request] = get(fake_calls),
    ?assertEqual(128, maps:get(dimensions, Request)),
    ?assertEqual(<<"query">>, maps:get(text_type, Request)).

opts_override_config_test() ->
    reset_fake(),
    Config = beamai_embedding:create({custom, ?MODULE}, #{dimensions => 128}),
    {ok, _} = beamai_embedding:embed_many(Config, [<<"a">>], #{dimensions => 64}),
    [Request] = get(fake_calls),
    ?assertEqual(64, maps:get(dimensions, Request)).

dimensions_dropped_when_unsupported_test() ->
    reset_fake(),
    put(fake_supports_dims, false),
    Config = beamai_embedding:create({custom, ?MODULE}, #{dimensions => 128}),
    {ok, _} = beamai_embedding:embed_many(Config, [<<"a">>]),
    [Request] = get(fake_calls),
    ?assertNot(maps:is_key(dimensions, Request)).

error_propagates_test() ->
    reset_fake(),
    put(fake_error, {http_error, 400, <<"bad request">>}),
    Config = beamai_embedding:create({custom, ?MODULE}, #{}),
    ?assertEqual({error, {http_error, 400, <<"bad request">>}},
                 beamai_embedding:embed_many(Config, [<<"a">>], #{max_retries => 0})).

%%====================================================================
%% 请求体构建
%%====================================================================

openai_body_test() ->
    Config = beamai_embedding:create(openai, #{api_key => <<"k">>}),
    Body = beamai_embedding_common:build_openai_body(
        Config, #{input => [<<"a">>, <<"b">>]}, <<"text-embedding-3-small">>),
    ?assertEqual(<<"text-embedding-3-small">>, maps:get(<<"model">>, Body)),
    ?assertEqual([<<"a">>, <<"b">>], maps:get(<<"input">>, Body)),
    ?assertEqual(<<"float">>, maps:get(<<"encoding_format">>, Body)),
    ?assertNot(maps:is_key(<<"dimensions">>, Body)).

openai_body_dimensions_test() ->
    Config = beamai_embedding:create(openai, #{api_key => <<"k">>, dimensions => 512}),
    Body = beamai_embedding_common:build_openai_body(Config, #{input => [<<"a">>]}, <<"m">>),
    ?assertEqual(512, maps:get(<<"dimensions">>, Body)).

dashscope_native_body_test() ->
    Config = beamai_embedding:create(dashscope, #{api_key => <<"k">>, dimensions => 1024}),
    Body = beamai_embedding_provider_dashscope:build_native_body(
        Config, #{input => [<<"a">>], text_type => <<"document">>}),
    ?assertEqual(<<"text-embedding-v4">>, maps:get(<<"model">>, Body)),
    ?assertEqual(#{<<"texts">> => [<<"a">>]}, maps:get(<<"input">>, Body)),
    Params = maps:get(<<"parameters">>, Body),
    %% 原生接口的维度字段为单数 dimension
    ?assertEqual(1024, maps:get(<<"dimension">>, Params)),
    ?assertEqual(<<"document">>, maps:get(<<"text_type">>, Params)).

headers_without_api_key_test() ->
    Headers = beamai_embedding_common:build_headers(#{}),
    ?assertEqual(undefined, proplists:get_value(<<"Authorization">>, Headers)),
    ?assertEqual(<<"application/json">>, proplists:get_value(<<"Content-Type">>, Headers)).

%%====================================================================
%% 响应解析
%%====================================================================

parse_openai_response_test() ->
    Raw = #{
        <<"model">> => <<"text-embedding-3-small">>,
        <<"data">> => [
            #{<<"index">> => 1, <<"embedding">> => [0.3, 0.4]},
            #{<<"index">> => 0, <<"embedding">> => [0.1, 0.2]}
        ],
        <<"usage">> => #{<<"prompt_tokens">> => 6, <<"total_tokens">> => 6}
    },
    {ok, Response} = beamai_embedding_common:from_openai(Raw, openai),
    %% 按 index 排序，与输入顺序一致
    ?assertEqual([[0.1, 0.2], [0.3, 0.4]], maps:get(embeddings, Response)),
    ?assertEqual(#{input_tokens => 6, total_tokens => 6}, maps:get(usage, Response)).

parse_openai_integer_values_test() ->
    Raw = #{<<"data">> => [#{<<"index">> => 0, <<"embedding">> => [1, 2]}]},
    {ok, Response} = beamai_embedding_common:from_openai(Raw, openai),
    ?assertEqual([[1.0, 2.0]], maps:get(embeddings, Response)).

parse_openai_base64_embedding_test() ->
    Encoded = base64:encode(<<1.5:32/float-little, (-2.0):32/float-little>>),
    Raw = #{<<"data">> => [#{<<"index">> => 0, <<"embedding">> => Encoded}]},
    {ok, Response} = beamai_embedding_common:from_openai(Raw, openai),
    ?assertEqual([[1.5, -2.0]], maps:get(embeddings, Response)).

parse_openai_error_test() ->
    ?assertMatch({error, {api_error, _}},
                 beamai_embedding_common:from_openai(#{<<"error">> => #{}}, openai)),
    ?assertMatch({error, {invalid_response, _}},
                 beamai_embedding_common:from_openai(#{<<"junk">> => 1}, openai)).

parse_ollama_response_test() ->
    Raw = #{
        <<"model">> => <<"nomic-embed-text">>,
        <<"embeddings">> => [[0.1, 0.2], [0.3, 0.4]],
        <<"prompt_eval_count">> => 8
    },
    {ok, Response} = beamai_embedding_common:from_ollama(Raw, ollama),
    ?assertEqual([[0.1, 0.2], [0.3, 0.4]], maps:get(embeddings, Response)),
    ?assertEqual(8, maps:get(total_tokens, maps:get(usage, Response))).

parse_ollama_legacy_single_test() ->
    Raw = #{<<"embedding">> => [0.5, 0.6]},
    {ok, Response} = beamai_embedding_common:from_ollama(Raw, ollama),
    ?assertEqual([[0.5, 0.6]], maps:get(embeddings, Response)).

parse_dashscope_response_test() ->
    Raw = #{
        <<"request_id">> => <<"req-1">>,
        <<"output">> => #{<<"embeddings">> => [
            #{<<"text_index">> => 1, <<"embedding">> => [0.3]},
            #{<<"text_index">> => 0, <<"embedding">> => [0.1]}
        ]},
        <<"usage">> => #{<<"total_tokens">> => 4}
    },
    {ok, Response} = beamai_embedding_common:from_dashscope(Raw, dashscope),
    ?assertEqual([[0.1], [0.3]], maps:get(embeddings, Response)),
    ?assertEqual(4, maps:get(total_tokens, maps:get(usage, Response))),
    ?assertEqual(<<"req-1">>, maps:get(request_id, maps:get(metadata, Response))).

parse_dashscope_error_test() ->
    Raw = #{<<"code">> => <<"InvalidApiKey">>, <<"message">> => <<"bad">>},
    ?assertMatch({error, {api_error, _}},
                 beamai_embedding_common:from_dashscope(Raw, dashscope)).

merge_responses_test() ->
    R1 = beamai_embedding_common:new_response(#{
        model => <<"m">>, provider => openai, embeddings => [[1.0]],
        usage => #{input_tokens => 1, total_tokens => 1}}),
    R2 = beamai_embedding_common:new_response(#{
        model => <<"m">>, provider => openai, embeddings => [[2.0], [3.0]],
        usage => #{input_tokens => 2, total_tokens => 2}}),
    Merged = beamai_embedding_common:merge_responses([R1, R2]),
    ?assertEqual([[1.0], [2.0], [3.0]], maps:get(embeddings, Merged)),
    ?assertEqual(#{input_tokens => 3, total_tokens => 3}, maps:get(usage, Merged)).

%%====================================================================
%% 向量工具
%%====================================================================

cosine_similarity_test() ->
    ?assertEqual(1.0, beamai_embedding:cosine_similarity([1.0, 0.0], [1.0, 0.0])),
    ?assertEqual(0.0, beamai_embedding:cosine_similarity([1.0, 0.0], [0.0, 1.0])),
    ?assertEqual(-1.0, beamai_embedding:cosine_similarity([1.0, 0.0], [-1.0, 0.0])),
    %% 维度不一致 / 零向量返回 0.0
    ?assertEqual(0.0, beamai_embedding:cosine_similarity([1.0], [1.0, 2.0])),
    ?assertEqual(0.0, beamai_embedding:cosine_similarity([0.0, 0.0], [1.0, 1.0])).

normalize_test() ->
    [X, Y] = beamai_embedding:normalize([3.0, 4.0]),
    ?assert(abs(X - 0.6) < 1.0e-9),
    ?assert(abs(Y - 0.8) < 1.0e-9),
    ?assertEqual([0.0, 0.0], beamai_embedding:normalize([0.0, 0.0])).

dimension_test() ->
    ?assertEqual(3, beamai_embedding:dimension([1.0, 2.0, 3.0])).

%%====================================================================
%% 假 Provider 实现
%%====================================================================

%% @private 重置假 Provider 的记录状态
reset_fake() ->
    erase(fake_calls),
    erase(fake_max_batch),
    erase(fake_supports_dims),
    erase(fake_error),
    ok.

name() -> <<"Fake Embeddings">>.

default_config() -> #{model => <<"fake">>}.

validate_config(_Config) -> ok.

max_batch_size() ->
    case get(fake_max_batch) of
        undefined -> 8;
        N -> N
    end.

supports_dimensions() ->
    get(fake_supports_dims) =/= false.

%% @private 记录请求并按文本长度生成可预期的向量
embed(Config, Request) ->
    put(fake_calls, lists:append(existing_calls(), [Request])),
    case get(fake_error) of
        undefined ->
            Input = maps:get(input, Request, []),
            {ok, beamai_embedding_common:new_response(#{
                model => maps:get(model, Config, <<"fake">>),
                provider => fake,
                embeddings => [[float(byte_size(T))] || T <- Input],
                usage => #{input_tokens => length(Input), total_tokens => length(Input)},
                raw => #{}
            })};
        Error ->
            {error, Error}
    end.

existing_calls() ->
    case get(fake_calls) of
        undefined -> [];
        Calls -> Calls
    end.
