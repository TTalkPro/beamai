%%%-------------------------------------------------------------------
%%% @doc 向量存储模块
%%%
%%% 提供内存向量数据库功能：
%%% - 文档存储：添加、获取、删除文档
%%% - 向量搜索：基于余弦相似度的语义搜索
%%% - 批量操作：高效处理多文档
%%%
%%% 使用示例：
%%% <pre>
%%% %% 创建存储
%%% Store = beamai_vector_store:new(),
%%%
%%% %% 添加文档
%%% {ok, Id, Store1} = beamai_vector_store:add_document(Store, #{
%%%     content => &lt;&lt;"Hello"&gt;&gt;,
%%%     embedding => [0.1, 0.2, ...],
%%%     metadata => #{source => &lt;&lt;"test"&gt;&gt;}
%%% }),
%%%
%%% %% 搜索
%%% {ok, Results} = beamai_vector_store:search(Store1, QueryVec, #{top_k => 5}).
%%% </pre>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_vector_store).

%% 导入公共工具函数
-import(beamai_rag_utils, [
    generate_uuid/0,
    take/2
]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 创建与销毁
-export([new/0, new/1, clear/1]).

%% 文档操作
-export([
    add_document/2,
    add_documents/2,
    get_document/2,
    delete_document/2,
    count/1
]).

%% 搜索操作
-export([search/2, search/3]).

%% 批量操作
-export([
    get_all_documents/1,
    get_documents_by_ids/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type document_id() :: binary().

-type document() :: #{
    id := document_id(),
    content := binary(),
    embedding := beamai_embeddings:vector(),
    metadata => map()
}.

-type vector_store() :: #{
    documents := [document()],
    index := #{document_id() => document()}
}.

-type search_opts() :: #{
    top_k => pos_integer(),
    min_score => float(),
    filter => fun((document()) -> boolean())
}.

-type search_result() :: #{
    document := document(),
    score := float()
}.

-export_type([document_id/0, document/0, vector_store/0, search_opts/0, search_result/0]).

%%====================================================================
%% 常量定义
%%====================================================================

-define(DEFAULT_TOP_K, 5).
-define(DEFAULT_MIN_SCORE, 0.0).

%%====================================================================
%% 创建与销毁 API
%%====================================================================

%% @doc 创建空的向量存储
-spec new() -> vector_store().
new() ->
    new(#{}).

%% @doc 创建带配置的向量存储
-spec new(map()) -> vector_store().
new(_Opts) ->
    #{documents => [], index => #{}}.

%% @doc 清空向量存储
-spec clear(vector_store()) -> vector_store().
clear(_Store) ->
    new().

%%====================================================================
%% 文档操作 API
%%====================================================================

%% @doc 添加单个文档
%%
%% 输入需包含 content 和 embedding 字段。
%% 返回 {ok, 文档ID, 更新后的存储}。
-spec add_document(vector_store(), map()) -> {ok, document_id(), vector_store()}.
add_document(Store, DocInput) ->
    DocId = generate_uuid(),
    Document = build_document(DocId, DocInput),
    NewStore = store_document(Store, Document),
    {ok, DocId, NewStore}.

%% @doc 批量添加文档
%%
%% 使用 fold 累积添加，保持顺序。
-spec add_documents(vector_store(), [map()]) -> {ok, [document_id()], vector_store()}.
add_documents(Store, DocInputs) ->
    {DocIds, FinalStore} = lists:foldl(fun add_one_document/2, {[], Store}, DocInputs),
    {ok, lists:reverse(DocIds), FinalStore}.

%% @doc 获取单个文档
-spec get_document(vector_store(), document_id()) -> {ok, document()} | {error, not_found}.
get_document(#{index := Index}, DocId) ->
    case maps:find(DocId, Index) of
        {ok, Doc} -> {ok, Doc};
        error -> {error, not_found}
    end.

%% @doc 删除文档
-spec delete_document(vector_store(), document_id()) -> {ok, vector_store()} | {error, not_found}.
delete_document(Store, DocId) ->
    #{documents := Docs, index := Index} = Store,
    case maps:is_key(DocId, Index) of
        true -> {ok, remove_document(Store, DocId, Docs, Index)};
        false -> {error, not_found}
    end.

%% @doc 获取文档数量
-spec count(vector_store()) -> non_neg_integer().
count(#{documents := Docs}) ->
    length(Docs).

%%====================================================================
%% 搜索操作 API
%%====================================================================

%% @doc 搜索相似文档（默认选项）
-spec search(vector_store(), beamai_embeddings:vector()) -> {ok, [search_result()]}.
search(Store, QueryVector) ->
    search(Store, QueryVector, #{}).

%% @doc 搜索相似文档
%%
%% 选项：
%% - top_k: 返回数量（默认 5）
%% - min_score: 最小分数阈值（默认 0.0）
%% - filter: 自定义过滤函数
-spec search(vector_store(), beamai_embeddings:vector(), search_opts()) -> {ok, [search_result()]}.
search(#{documents := Docs}, QueryVector, Opts) ->
    TopK = maps:get(top_k, Opts, ?DEFAULT_TOP_K),
    MinScore = maps:get(min_score, Opts, ?DEFAULT_MIN_SCORE),
    Filter = maps:get(filter, Opts, fun(_) -> true end),

    %% 管道式处理：计分 → 过滤 → 排序 → 截取
    Scored = score_documents(Docs, QueryVector, Filter),
    Filtered = filter_by_min_score(Scored, MinScore),
    Sorted = sort_by_score_desc(Filtered),
    Results = take(Sorted, TopK),

    {ok, Results}.

%%====================================================================
%% 批量操作 API
%%====================================================================

%% @doc 获取所有文档
-spec get_all_documents(vector_store()) -> [document()].
get_all_documents(#{documents := Docs}) ->
    Docs.

%% @doc 根据 ID 列表获取文档
-spec get_documents_by_ids(vector_store(), [document_id()]) -> [document()].
get_documents_by_ids(#{index := Index}, DocIds) ->
    lists:filtermap(fun(Id) -> find_in_index(Index, Id) end, DocIds).

%%====================================================================
%% 私有函数 - 文档操作
%%====================================================================

%% @private 构建文档记录
-spec build_document(document_id(), map()) -> document().
build_document(DocId, Input) ->
    #{
        id => DocId,
        content => maps:get(content, Input, <<>>),
        embedding => maps:get(embedding, Input, []),
        metadata => maps:get(metadata, Input, #{})
    }.

%% @private 存储文档到 Store
-spec store_document(vector_store(), document()) -> vector_store().
store_document(#{documents := Docs, index := Index}, #{id := DocId} = Doc) ->
    #{
        documents => [Doc | Docs],
        index => Index#{DocId => Doc}
    }.

%% @private 添加单个文档（用于 fold）
-spec add_one_document(map(), {[document_id()], vector_store()}) -> {[document_id()], vector_store()}.
add_one_document(DocInput, {Ids, AccStore}) ->
    {ok, DocId, NewStore} = add_document(AccStore, DocInput),
    {[DocId | Ids], NewStore}.

%% @private 从存储中移除文档
-spec remove_document(vector_store(), document_id(), [document()], map()) -> vector_store().
remove_document(_Store, DocId, Docs, Index) ->
    #{
        documents => lists:filter(fun(#{id := Id}) -> Id =/= DocId end, Docs),
        index => maps:remove(DocId, Index)
    }.

%% @private 在索引中查找文档
-spec find_in_index(map(), document_id()) -> {true, document()} | false.
find_in_index(Index, DocId) ->
    case maps:find(DocId, Index) of
        {ok, Doc} -> {true, Doc};
        error -> false
    end.

%%====================================================================
%% 私有函数 - 搜索操作
%%====================================================================

%% @private 为文档计算相似度分数
-spec score_documents([document()], beamai_embeddings:vector(), fun()) -> [search_result()].
score_documents(Docs, QueryVector, Filter) ->
    lists:filtermap(
        fun(Doc) -> score_single_document(Doc, QueryVector, Filter) end,
        Docs
    ).

%% @private 为单个文档计算分数
-spec score_single_document(document(), beamai_embeddings:vector(), fun()) ->
    {true, search_result()} | false.
score_single_document(Doc, QueryVector, Filter) ->
    case Filter(Doc) of
        true ->
            Score = compute_similarity(Doc, QueryVector),
            {true, #{document => Doc, score => Score}};
        false ->
            false
    end.

%% @private 计算文档与查询的相似度
-spec compute_similarity(document(), beamai_embeddings:vector()) -> float().
compute_similarity(#{embedding := Embedding}, QueryVector) ->
    beamai_embeddings:cosine_similarity(QueryVector, Embedding).

%% @private 过滤低于阈值的结果
-spec filter_by_min_score([search_result()], float()) -> [search_result()].
filter_by_min_score(Results, MinScore) ->
    lists:filter(fun(#{score := S}) -> S >= MinScore end, Results).

%% @private 按分数降序排序
-spec sort_by_score_desc([search_result()]) -> [search_result()].
sort_by_score_desc(Results) ->
    lists:sort(fun(#{score := S1}, #{score := S2}) -> S1 > S2 end, Results).
