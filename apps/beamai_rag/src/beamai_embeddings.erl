%%%-------------------------------------------------------------------
%%% @doc 文本嵌入模块
%%%
%%% 提供文本向量化功能，支持多种嵌入模型：
%%% - OpenAI Embeddings API (text-embedding-3-small 等)
%%% - 本地嵌入模型（用于测试）
%%%
%%% 主要功能：
%%% - 单文本嵌入：将文本转换为向量
%%% - 批量嵌入：高效处理多个文本
%%% - 向量运算：相似度计算、归一化
%%%
%%% 使用示例：
%%% <pre>
%%% %% 创建嵌入模型
%%% Model = beamai_embeddings:new_openai(#{api_key => &lt;&lt;"sk-..."&gt;&gt;}),
%%%
%%% %% 嵌入文本
%%% {ok, Vector} = beamai_embeddings:embed_text(Model, &lt;&lt;"Hello"&gt;&gt;),
%%%
%%% %% 计算相似度
%%% Score = beamai_embeddings:cosine_similarity(Vec1, Vec2).
%%% </pre>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embeddings).

%% 导入公共工具函数
-import(beamai_rag_utils, [
    dot_product/2,
    vector_norm/1,
    safe_divide/2
]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 模型创建
-export([
    new_openai/0, new_openai/1,
    new_local/0, new_local/1
]).

%% 嵌入操作
-export([
    embed_text/2,
    embed_batch/2
]).

%% 向量运算
-export([
    cosine_similarity/2,
    euclidean_distance/2,
    normalize/1
]).

%% 便捷函数
-export([
    embed/1,
    embed_all/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type embedding_model() :: #{
    type := openai | local,
    api_key => binary(),
    model => binary(),
    base_url => binary(),
    dimension => pos_integer()
}.

-type vector() :: [float()].

-export_type([embedding_model/0, vector/0]).

%%====================================================================
%% 常量定义
%%====================================================================

-define(OPENAI_MODEL, <<"text-embedding-3-small">>).
-define(OPENAI_URL, <<"https://api.openai.com/v1/embeddings">>).
-define(OPENAI_DIMENSION, 1536).
-define(LOCAL_DIMENSION, 384).
-define(HTTP_TIMEOUT, 60000).

%%====================================================================
%% 模型创建 API
%%====================================================================

%% @doc 创建默认 OpenAI 嵌入模型
%%
%% 从环境变量 OPENAI_API_KEY 读取 API Key。
-spec new_openai() -> embedding_model().
new_openai() ->
    new_openai(#{}).

%% @doc 创建自定义 OpenAI 嵌入模型
%%
%% 选项：
%% - api_key: API 密钥
%% - model: 模型名称
%% - base_url: API 端点
%% - dimension: 向量维度
-spec new_openai(map()) -> embedding_model().
new_openai(Opts) ->
    #{
        type => openai,
        api_key => maps:get(api_key, Opts, get_env_api_key()),
        model => maps:get(model, Opts, ?OPENAI_MODEL),
        base_url => maps:get(base_url, Opts, ?OPENAI_URL),
        dimension => maps:get(dimension, Opts, ?OPENAI_DIMENSION)
    }.

%% @doc 创建默认本地嵌入模型
%%
%% 本地模型使用哈希生成伪嵌入，仅用于测试。
-spec new_local() -> embedding_model().
new_local() ->
    new_local(#{}).

%% @doc 创建自定义本地嵌入模型
%%
%% 选项：
%% - dimension: 向量维度（默认 384）
-spec new_local(map()) -> embedding_model().
new_local(Opts) ->
    #{
        type => local,
        dimension => maps:get(dimension, Opts, ?LOCAL_DIMENSION)
    }.

%%====================================================================
%% 嵌入操作 API
%%====================================================================

%% @doc 嵌入单个文本
%%
%% 根据模型类型分派到对应实现。
-spec embed_text(embedding_model(), binary()) -> {ok, vector()} | {error, term()}.
embed_text(#{type := openai} = Model, Text) ->
    embed_single_openai(Model, Text);
embed_text(#{type := local} = Model, Text) ->
    embed_single_local(Model, Text).

%% @doc 批量嵌入文本
%%
%% OpenAI 使用批量 API 提高效率，本地模型逐个处理。
-spec embed_batch(embedding_model(), [binary()]) -> {ok, [vector()]} | {error, term()}.
embed_batch(#{type := openai} = Model, Texts) ->
    embed_batch_openai(Model, Texts);
embed_batch(#{type := local} = Model, Texts) ->
    embed_batch_local(Model, Texts).

%%====================================================================
%% 向量运算 API
%%====================================================================

%% @doc 计算余弦相似度
%%
%% 公式：cos(θ) = (A·B) / (|A| × |B|)
%% 返回值范围 [-1, 1]，1 表示完全相同。
-spec cosine_similarity(vector(), vector()) -> float().
cosine_similarity(Vec1, Vec2) when length(Vec1) =:= length(Vec2) ->
    DotProd = dot_product(Vec1, Vec2),
    Denominator = vector_norm(Vec1) * vector_norm(Vec2),
    safe_divide(DotProd, Denominator).

%% @doc 计算欧几里得距离
%%
%% 公式：d = √Σ(ai - bi)²
%% 返回值 >= 0，0 表示完全相同。
-spec euclidean_distance(vector(), vector()) -> float().
euclidean_distance(Vec1, Vec2) when length(Vec1) =:= length(Vec2) ->
    DiffSquares = [math:pow(A - B, 2) || {A, B} <- lists:zip(Vec1, Vec2)],
    math:sqrt(lists:sum(DiffSquares)).

%% @doc 归一化向量
%%
%% 将向量转换为单位向量（模为 1）。
-spec normalize(vector()) -> vector().
normalize(Vec) ->
    Norm = vector_norm(Vec),
    scale_vector(Vec, Norm).

%%====================================================================
%% 便捷函数
%%====================================================================

%% @doc 使用默认模型嵌入文本
-spec embed(binary()) -> {ok, vector()} | {error, term()}.
embed(Text) ->
    embed_text(get_default_model(), Text).

%% @doc 使用默认模型批量嵌入
-spec embed_all([binary()]) -> {ok, [vector()]} | {error, term()}.
embed_all(Texts) ->
    embed_batch(get_default_model(), Texts).

%%====================================================================
%% OpenAI 实现（私有）
%%====================================================================

%% @private 嵌入单个文本（OpenAI）
-spec embed_single_openai(embedding_model(), binary()) -> {ok, vector()} | {error, term()}.
embed_single_openai(Model, Text) ->
    case embed_batch_openai(Model, [Text]) of
        {ok, [Vector]} -> {ok, Vector};
        {error, Reason} -> {error, Reason}
    end.

%% @private 批量嵌入（OpenAI）
-spec embed_batch_openai(embedding_model(), [binary()]) -> {ok, [vector()]} | {error, term()}.
embed_batch_openai(Model, Texts) ->
    Request = build_openai_request(Model, Texts),
    execute_openai_request(Request).

%% @private 构建 OpenAI 请求
-spec build_openai_request(embedding_model(), [binary()]) -> map().
build_openai_request(#{api_key := ApiKey, model := ModelName, base_url := BaseUrl}, Texts) ->
    #{
        url => BaseUrl,
        headers => build_auth_headers(ApiKey),
        body => jsx:encode(#{model => ModelName, input => Texts})
    }.

%% @private 构建认证请求头
-spec build_auth_headers(binary()) -> [{binary(), binary()}].
build_auth_headers(ApiKey) ->
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Authorization">>, <<"Bearer ", ApiKey/binary>>}
    ].

%% @private 执行 OpenAI HTTP 请求
%% 使用 beamai_http 作为底层 HTTP 客户端
-spec execute_openai_request(map()) -> {ok, [vector()]} | {error, term()}.
execute_openai_request(#{url := Url, headers := Headers, body := Body}) ->
    HttpOpts = #{
        timeout => ?HTTP_TIMEOUT,
        connect_timeout => ?HTTP_TIMEOUT,
        headers => Headers
    },
    case beamai_http:post(Url, <<"application/json">>, Body, HttpOpts) of
        {ok, Response} ->
            parse_openai_response(Response);
        {error, {http_error, StatusCode, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 解析 OpenAI 响应
%% 支持已解码的 map 和原始 binary 两种输入
-spec parse_openai_response(map() | binary()) -> {ok, [vector()]} | {error, term()}.
parse_openai_response(#{<<"data">> := Data}) ->
    try
        Vectors = [maps:get(<<"embedding">>, Item) || Item <- Data],
        {ok, Vectors}
    catch
        _:Error -> {error, {parse_error, Error}}
    end;
parse_openai_response(ResponseBody) when is_binary(ResponseBody) ->
    try
        Decoded = jsx:decode(ResponseBody, [return_maps]),
        parse_openai_response(Decoded)
    catch
        _:Error -> {error, {parse_error, Error}}
    end;
parse_openai_response(_) ->
    {error, {parse_error, invalid_response}}.

%%====================================================================
%% 本地嵌入实现（私有）
%%====================================================================

%% @private 嵌入单个文本（本地）
-spec embed_single_local(embedding_model(), binary()) -> {ok, vector()}.
embed_single_local(#{dimension := Dim}, Text) ->
    Vector = generate_hash_embedding(Text, Dim),
    {ok, Vector}.

%% @private 批量嵌入（本地）
-spec embed_batch_local(embedding_model(), [binary()]) -> {ok, [vector()]} | {error, term()}.
embed_batch_local(Model, Texts) ->
    Results = [embed_single_local(Model, T) || T <- Texts],
    collect_results(Results, length(Texts)).

%% @private 收集批量结果
-spec collect_results([{ok, vector()}], non_neg_integer()) -> {ok, [vector()]} | {error, term()}.
collect_results(Results, ExpectedCount) ->
    Vectors = [V || {ok, V} <- Results],
    case length(Vectors) =:= ExpectedCount of
        true -> {ok, Vectors};
        false -> {error, some_embeddings_failed}
    end.

%% @private 生成哈希嵌入
%%
%% 使用文本哈希生成确定性伪随机向量。
-spec generate_hash_embedding(binary(), pos_integer()) -> vector().
generate_hash_embedding(Text, Dimension) ->
    Seed = hash_to_seed(Text),
    rand:seed(exsss, {Seed, Seed * 2, Seed * 3}),
    RawVector = [rand:uniform() * 2 - 1 || _ <- lists:seq(1, Dimension)],
    normalize(RawVector).

%% @private 将文本哈希转换为种子
-spec hash_to_seed(binary()) -> integer().
hash_to_seed(Text) ->
    Hash = crypto:hash(md5, Text),
    <<Seed:128/unsigned-integer>> = Hash,
    Seed.

%%====================================================================
%% 辅助函数（私有）
%%====================================================================

%% @private 缩放向量
-spec scale_vector(vector(), float()) -> vector().
scale_vector(Vec, Norm) when Norm == 0 -> Vec;
scale_vector(Vec, Norm) -> [X / Norm || X <- Vec].

%% @private 从环境变量获取 API Key
-spec get_env_api_key() -> binary().
get_env_api_key() ->
    case os:getenv("OPENAI_API_KEY") of
        false -> <<>>;
        Key -> list_to_binary(Key)
    end.

%% @private 获取默认模型
-spec get_default_model() -> embedding_model().
get_default_model() ->
    case get_env_api_key() of
        <<>> -> new_local();
        _ -> new_openai()
    end.
