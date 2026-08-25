%%%-------------------------------------------------------------------
%%% @doc 文本向量化（Embedding）统一入口
%%%
%%% 与 beamai_chat_model（对话）对等的门面模块：
%%%   - 多 Provider 路由（openai / dashscope / zhipu / siliconflow / ollama / mock）
%%%   - 自动分批（按 Provider 声明的 max_batch_size 切分并按序拼接）
%%%   - 失败重试（复用 beamai_llm_retry：瞬态错误退避重试）
%%%   - 向量工具（余弦相似度、归一化）
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% Config = beamai_embedding:create(openai, #{
%%%     api_key => <<"sk-...">>,
%%%     model => <<"text-embedding-3-small">>,
%%%     dimensions => 512
%%% }),
%%%
%%% %% 单条文本
%%% {ok, Vector} = beamai_embedding:embed(Config, <<"你好"/utf8>>),
%%%
%%% %% 批量文本（超过 Provider 上限时自动分批）
%%% {ok, Vectors} = beamai_embedding:embed_many(Config, [<<"a">>, <<"b">>]),
%%%
%%% %% 需要 usage 统计时取完整响应
%%% {ok, #{usage := Usage}} = beamai_embedding:embed_full(Config, [<<"a">>]),
%%%
%%% Score = beamai_embedding:cosine_similarity(V1, V2).
%%% ```
%%%
%%% == 检索场景提示 ==
%%%
%%% DashScope 等 Provider 支持 `text_type'（`<<"query">>' / `<<"document">>'），
%%% 建索引与查询时分别传入可提升召回：
%%%
%%% ```erlang
%%% {ok, Vs} = beamai_embedding:embed_many(Config, Docs, #{text_type => <<"document">>}).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embedding).

%% 配置 API
-export([create/2, validate/1, provider_module/1]).

%% 向量化 API
-export([embed/2, embed/3]).
-export([embed_many/2, embed_many/3]).
-export([embed_full/2, embed_full/3]).

%% 向量工具
-export([cosine_similarity/2, normalize/1, dimension/1]).

-export_type([config/0, provider/0, vector/0, response/0]).

-type provider() :: openai | dashscope | zhipu | siliconflow | ollama | mock |
                    {custom, module()}.
-type vector() :: beamai_embedding_provider_behaviour:vector().
-type response() :: beamai_embedding_provider_behaviour:response().
-type config() :: #{
    provider := provider(),
    '__embedding_config__' := true,
    atom() => term()
}.

%% 请求级可透传参数（Opts -> Request）
-define(REQUEST_FIELDS, [dimensions, encoding_format, text_type, user, truncate]).

%%====================================================================
%% 配置 API
%%====================================================================

%% @doc 创建向量化配置
%%
%% 合并 Provider 默认配置与用户配置，用法与
%% beamai_chat_model:create/2 一致。
-spec create(provider(), map()) -> config().
create(Provider, Opts) ->
    Module = provider_module(Provider),
    Base = #{provider => Provider, '__embedding_config__' => true},
    maps:merge(maps:merge(Module:default_config(), Base), Opts).

%% @doc 校验配置（委托给 Provider）
-spec validate(config()) -> ok | {error, term()}.
validate(Config) ->
    Module = provider_module(maps:get(provider, Config, openai)),
    Module:validate_config(Config).

%% @doc Provider 路由
-spec provider_module(provider()) -> module().
provider_module(openai) -> beamai_embedding_provider_openai;
provider_module(dashscope) -> beamai_embedding_provider_dashscope;
provider_module(zhipu) -> beamai_embedding_provider_zhipu;
provider_module(siliconflow) -> beamai_embedding_provider_siliconflow;
provider_module(ollama) -> beamai_embedding_provider_ollama;
provider_module(mock) -> beamai_embedding_provider_mock;
provider_module({custom, Module}) -> Module.

%%====================================================================
%% 向量化 API
%%====================================================================

%% @doc 单条文本向量化
-spec embed(config(), binary()) -> {ok, vector()} | {error, term()}.
embed(Config, Text) ->
    embed(Config, Text, #{}).

%% @doc 单条文本向量化（带选项）
%%
%% 选项：
%%   dimensions / encoding_format / text_type / user - 透传给 Provider
%%   max_retries / retry_delay / on_retry            - 重试策略
-spec embed(config(), binary(), map()) -> {ok, vector()} | {error, term()}.
embed(Config, Text, Opts) when is_binary(Text) ->
    case embed_full(Config, [Text], Opts) of
        {ok, #{embeddings := [Vector | _]}} -> {ok, Vector};
        {ok, #{embeddings := []}} -> {error, empty_embedding_response};
        {error, _} = Error -> Error
    end.

%% @doc 批量文本向量化（返回顺序与输入一致）
-spec embed_many(config(), [binary()]) -> {ok, [vector()]} | {error, term()}.
embed_many(Config, Texts) ->
    embed_many(Config, Texts, #{}).

%% @doc 批量文本向量化（带选项）
-spec embed_many(config(), [binary()], map()) -> {ok, [vector()]} | {error, term()}.
embed_many(Config, Texts, Opts) ->
    case embed_full(Config, Texts, Opts) of
        {ok, #{embeddings := Vectors}} -> {ok, Vectors};
        {error, _} = Error -> Error
    end.

%% @doc 批量文本向量化，返回含 usage / raw 的完整响应
-spec embed_full(config(), [binary()]) -> {ok, response()} | {error, term()}.
embed_full(Config, Texts) ->
    embed_full(Config, Texts, #{}).

%% @doc 批量文本向量化，返回完整响应（带选项）
-spec embed_full(config(), [binary()], map()) -> {ok, response()} | {error, term()}.
embed_full(Config, [], _Opts) ->
    Provider = maps:get(provider, Config, unknown),
    {ok, beamai_embedding_common:empty_response(maps:get(model, Config, <<>>), Provider)};
embed_full(Config, Texts, Opts) when is_list(Texts) ->
    Module = provider_module(maps:get(provider, Config, openai)),
    Request = build_request(Module, Config, Opts),
    RetryOpts = beamai_llm_retry:opts(Config, Opts),
    Batches = chunk(Texts, batch_size(Module, Opts)),
    embed_batches(Module, Config, Request, RetryOpts, Batches, []).

%%====================================================================
%% 向量工具
%%====================================================================

%% @doc 余弦相似度（取值 -1.0 ~ 1.0）
%% 维度不一致或存在零向量时返回 0.0。
-spec cosine_similarity(vector(), vector()) -> float().
cosine_similarity(V1, V2) when length(V1) =:= length(V2) ->
    Dot = dot(V1, V2),
    Norm1 = math:sqrt(dot(V1, V1)),
    Norm2 = math:sqrt(dot(V2, V2)),
    case Norm1 * Norm2 of
        +0.0 -> 0.0;
        Denominator -> Dot / Denominator
    end;
cosine_similarity(_, _) ->
    0.0.

%% @doc 向量归一化（模长为 1）
%% 零向量原样返回。
-spec normalize(vector()) -> vector().
normalize(Vector) ->
    case math:sqrt(dot(Vector, Vector)) of
        +0.0 -> Vector;
        Norm -> [X / Norm || X <- Vector]
    end.

%% @doc 向量维度
-spec dimension(vector()) -> non_neg_integer().
dimension(Vector) when is_list(Vector) -> length(Vector).

%%====================================================================
%% 内部函数 - 请求构建
%%====================================================================

%% @private 构建 Provider 请求参数
%% Opts 优先于 Config；Provider 不支持维度参数时剔除 dimensions，避免 400。
build_request(Module, Config, Opts) ->
    Request = lists:foldl(fun(Key, Acc) ->
        case maps:get(Key, Opts, maps:get(Key, Config, undefined)) of
            undefined -> Acc;
            Value -> Acc#{Key => Value}
        end
    end, #{}, ?REQUEST_FIELDS),
    case Module:supports_dimensions() of
        true -> Request;
        false -> maps:remove(dimensions, Request)
    end.

%% @private 单批条数：Opts 可调小，但不超过 Provider 上限
batch_size(Module, Opts) ->
    Max = Module:max_batch_size(),
    case maps:get(batch_size, Opts, Max) of
        N when is_integer(N), N > 0 -> min(N, Max);
        _ -> Max
    end.

%%====================================================================
%% 内部函数 - 分批执行
%%====================================================================

%% @private 逐批请求并合并结果，任一批失败即整体失败
embed_batches(_Module, _Config, _Request, _RetryOpts, [], Acc) ->
    {ok, beamai_embedding_common:merge_responses(lists:reverse(Acc))};
embed_batches(Module, Config, Request, RetryOpts, [Batch | Rest], Acc) ->
    Call = fun() -> Module:embed(Config, Request#{input => Batch}) end,
    case beamai_llm_retry:run(Call, RetryOpts) of
        {ok, Response} ->
            embed_batches(Module, Config, Request, RetryOpts, Rest, [Response | Acc]);
        {error, _} = Error ->
            Error
    end.

%% @private 按长度切分列表
chunk(List, Size) when is_integer(Size), Size > 0 ->
    chunk(List, Size, []).

chunk([], _Size, Acc) ->
    lists:reverse(Acc);
chunk(List, Size, Acc) when length(List) =< Size ->
    lists:reverse([List | Acc]);
chunk(List, Size, Acc) ->
    {Batch, Rest} = lists:split(Size, List),
    chunk(Rest, Size, [Batch | Acc]).

%%====================================================================
%% 内部函数 - 向量运算
%%====================================================================

%% @private 点积
dot(V1, V2) ->
    lists:sum([X * Y || {X, Y} <- lists:zip(V1, V2)]).
