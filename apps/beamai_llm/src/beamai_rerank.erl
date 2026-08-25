%%%-------------------------------------------------------------------
%%% @doc 文档重排序（Rerank）统一入口
%%%
%%% 与 beamai_chat_model（对话）、beamai_embedding（向量化）对等的门面：
%%%   - 多 Provider 路由（siliconflow / dashscope / jina / cohere / voyage / mock）
%%%   - 结果归一（按相关性降序，index 指回原始文档下标，回填文档原文）
%%%   - 条数上限校验（重排序是全局比较，不能分批，超限直接报错而非静默截断）
%%%   - 失败重试（复用 beamai_llm_retry）
%%%
%%% == 典型用途（RAG 两阶段检索）==
%%%
%%% ```erlang
%%% %% 一阶段：向量召回候选
%%% {ok, Candidates} = my_store:search(QueryVector, 50),
%%%
%%% %% 二阶段：Cross-Encoder 重排序，取 top 5 入上下文
%%% Config = beamai_rerank:create(siliconflow, #{api_key => ApiKey}),
%%% {ok, Results} = beamai_rerank:rerank(Config, Query, Candidates, #{top_n => 5}),
%%% TopDocs = beamai_rerank:documents(Results).
%%% ```
%%%
%%% 每条结果形如：
%%%
%%% ```erlang
%%% #{index => 7, score => 0.93, document => <<"...">>}
%%% ```
%%%
%%% `index' 是文档在入参 Documents 中的下标（从 0 开始），
%%% 便于把得分挂回上层自己的元数据结构。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rerank).

%% 配置 API
-export([create/2, validate/1, provider_module/1]).

%% 重排序 API
-export([rerank/3, rerank/4]).
-export([rerank_full/3, rerank_full/4]).

%% 结果工具
-export([documents/1, indices/1, scores/1]).

-export_type([config/0, provider/0, result/0, response/0]).

-type provider() :: siliconflow | dashscope | jina | cohere | voyage | mock |
                    {custom, module()}.
-type result() :: beamai_rerank_provider_behaviour:result().
-type response() :: beamai_rerank_provider_behaviour:response().
-type config() :: #{
    provider := provider(),
    '__rerank_config__' := true,
    atom() => term()
}.

%% 请求级可透传参数（Opts -> Request）
-define(REQUEST_FIELDS, [top_n, return_documents, max_chunks_per_doc, overlap_tokens]).

%%====================================================================
%% 配置 API
%%====================================================================

%% @doc 创建重排序配置
-spec create(provider(), map()) -> config().
create(Provider, Opts) ->
    Module = provider_module(Provider),
    Base = #{provider => Provider, '__rerank_config__' => true},
    maps:merge(maps:merge(Module:default_config(), Base), Opts).

%% @doc 校验配置（委托给 Provider）
-spec validate(config()) -> ok | {error, term()}.
validate(Config) ->
    Module = provider_module(maps:get(provider, Config, siliconflow)),
    Module:validate_config(Config).

%% @doc Provider 路由
-spec provider_module(provider()) -> module().
provider_module(siliconflow) -> beamai_rerank_provider_siliconflow;
provider_module(dashscope) -> beamai_rerank_provider_dashscope;
provider_module(jina) -> beamai_rerank_provider_jina;
provider_module(cohere) -> beamai_rerank_provider_cohere;
provider_module(voyage) -> beamai_rerank_provider_voyage;
provider_module(mock) -> beamai_rerank_provider_mock;
provider_module({custom, Module}) -> Module.

%%====================================================================
%% 重排序 API
%%====================================================================

%% @doc 按 query 对文档重排序，返回降序结果列表
-spec rerank(config(), binary(), [binary()]) -> {ok, [result()]} | {error, term()}.
rerank(Config, Query, Documents) ->
    rerank(Config, Query, Documents, #{}).

%% @doc 按 query 对文档重排序（带选项）
%%
%% 选项：
%%   top_n              - 只返回前 N 条
%%   return_documents   - 是否让服务端回传文档原文（本模块总会回填，一般无需设置）
%%   max_retries / retry_delay / on_retry - 重试策略
-spec rerank(config(), binary(), [binary()], map()) -> {ok, [result()]} | {error, term()}.
rerank(Config, Query, Documents, Opts) ->
    case rerank_full(Config, Query, Documents, Opts) of
        {ok, #{results := Results}} -> {ok, Results};
        {error, _} = Error -> Error
    end.

%% @doc 重排序并返回含 usage / raw 的完整响应
-spec rerank_full(config(), binary(), [binary()]) -> {ok, response()} | {error, term()}.
rerank_full(Config, Query, Documents) ->
    rerank_full(Config, Query, Documents, #{}).

%% @doc 重排序并返回完整响应（带选项）
-spec rerank_full(config(), binary(), [binary()], map()) -> {ok, response()} | {error, term()}.
rerank_full(Config, _Query, [], _Opts) ->
    Provider = maps:get(provider, Config, unknown),
    {ok, beamai_rerank_common:empty_response(maps:get(model, Config, <<>>), Provider)};
rerank_full(Config, Query, Documents, Opts) when is_list(Documents) ->
    Module = provider_module(maps:get(provider, Config, siliconflow)),
    case check_limit(Module, Documents) of
        ok ->
            Request = build_request(Config, Query, Documents, Opts),
            RetryOpts = beamai_llm_retry:opts(Opts),
            case beamai_llm_retry:run(fun() -> Module:rerank(Config, Request) end, RetryOpts) of
                {ok, Response} -> {ok, fill_documents(Response, Documents)};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 结果工具
%%====================================================================

%% @doc 取重排序后的文档列表（已按相关性降序）
-spec documents([result()]) -> [binary()].
documents(Results) ->
    [D || #{document := D} <- Results].

%% @doc 取重排序后的原始下标列表
-spec indices([result()]) -> [non_neg_integer()].
indices(Results) ->
    [I || #{index := I} <- Results].

%% @doc 取相关性得分列表
-spec scores([result()]) -> [float()].
scores(Results) ->
    [S || #{score := S} <- Results].

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 文档条数上限校验
%% 重排序需要在同一次请求内做全局比较，超限时不能像向量化那样分批，
%% 因此显式报错，由上层决定先粗筛还是换模型。
check_limit(Module, Documents) ->
    Max = Module:max_documents(),
    case length(Documents) of
        Count when Count > Max -> {error, {too_many_documents, Count, Max}};
        _ -> ok
    end.

%% @private 构建 Provider 请求参数（Opts 优先于 Config）
build_request(Config, Query, Documents, Opts) ->
    Base = #{query => Query, documents => Documents},
    lists:foldl(fun(Key, Acc) ->
        case maps:get(Key, Opts, maps:get(Key, Config, undefined)) of
            undefined -> Acc;
            Value -> Acc#{Key => Value}
        end
    end, Base, ?REQUEST_FIELDS).

%% @private 按 index 回填文档原文
%% 未开启 return_documents 时服务端只回 index + score，
%% 统一回填成完整结果，调用方无需再对着原列表取值。
fill_documents(#{results := Results} = Response, Documents) ->
    Indexed = list_to_tuple(Documents),
    Size = tuple_size(Indexed),
    Filled = [fill_one(R, Indexed, Size) || R <- Results],
    Response#{results => Filled}.

fill_one(#{document := _} = Result, _Indexed, _Size) ->
    Result;
fill_one(#{index := Index} = Result, Indexed, Size) when Index >= 0, Index < Size ->
    Result#{document => element(Index + 1, Indexed)};
fill_one(Result, _Indexed, _Size) ->
    %% 服务端返回了越界 index（异常响应），保持原样不猜测
    Result.
