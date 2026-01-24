%%%-------------------------------------------------------------------
%%% @doc RAG（检索增强生成）管道模块
%%%
%%% 提供完整的 RAG 工作流：
%%% - 文本分割：将长文本切分为可嵌入的片段
%%% - 文档索引：嵌入并存储到向量数据库
%%% - 语义检索：基于相似度查找相关文档
%%% - 上下文生成：结合检索内容调用 LLM 生成回答
%%%
%%% 使用示例：
%%% <pre>
%%% %% 创建管道
%%% Pipeline = beamai_rag:new(#{
%%%     embeddings => beamai_embeddings:new_openai(),
%%%     llm => #{provider => openai, model => &lt;&lt;"gpt-4"&gt;&gt;}
%%% }),
%%%
%%% %% 索引文档
%%% {ok, DocId, Pipeline1} = beamai_rag:index_document(Pipeline,
%%%     &lt;&lt;"Erlang is..."&gt;&gt;, #{source => &lt;&lt;"wiki"&gt;&gt;}),
%%%
%%% %% RAG 查询
%%% {ok, Answer, Pipeline2} = beamai_rag:query(Pipeline1, &lt;&lt;"What is Erlang?"&gt;&gt;).
%%% </pre>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rag).

%% 导入公共工具函数
-import(beamai_rag_utils, [
    ensure_binary/1
]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 管道创建
-export([new/0, new/1]).

%% 文本分割
-export([split_text/2, split_text/3]).

%% 文档索引
-export([
    index_document/3, index_document/4,
    index_documents/3,
    index_file/3
]).

%% 检索操作
-export([retrieve/2, retrieve/3]).

%% RAG 生成
-export([query/2, query/3]).

%% 管道状态
-export([get_store/1, get_document_count/1]).

%%====================================================================
%% 类型定义
%%====================================================================

-type rag_pipeline() :: #{
    embeddings := beamai_embeddings:embedding_model(),
    vector_store := beamai_vector_store:vector_store(),
    text_splitter := beamai_rag_splitter:splitter(),
    llm := map()
}.

%% 复用 beamai_rag_splitter 的类型
-type text_splitter() :: beamai_rag_splitter:splitter().
-type chunk() :: beamai_rag_splitter:chunk().

-export_type([text_splitter/0, rag_pipeline/0, chunk/0]).

%%====================================================================
%% 常量定义
%%====================================================================

-define(CHUNK_SIZE, 1000).
-define(CHUNK_OVERLAP, 200).
-define(SEPARATOR, <<"\n">>).
-define(TOP_K, 5).
-define(MAX_CONTEXT, 2000).

%%====================================================================
%% 管道创建 API
%%====================================================================

%% @doc 创建默认 RAG 管道
-spec new() -> rag_pipeline().
new() ->
    new(#{}).

%% @doc 创建自定义 RAG 管道
%%
%% 选项：
%% - embeddings: 嵌入模型
%% - vector_store: 向量存储
%% - text_splitter: 分割器配置
%% - llm: LLM 配置
-spec new(map()) -> rag_pipeline().
new(Opts) ->
    #{
        embeddings => maps:get(embeddings, Opts, beamai_embeddings:new_local()),
        vector_store => maps:get(vector_store, Opts, beamai_vector_store:new()),
        text_splitter => beamai_rag_splitter:new(maps:get(text_splitter, Opts, #{})),
        llm => maps:get(llm, Opts, default_llm_config())
    }.

%%====================================================================
%% 文本分割 API（委托给 beamai_rag_splitter）
%%====================================================================

%% @doc 分割文本（使用管道配置）
-spec split_text(rag_pipeline(), binary()) -> [chunk()].
split_text(#{text_splitter := Splitter}, Text) ->
    beamai_rag_splitter:split(Splitter, Text).

%% @doc 分割文本（自定义配置）
-spec split_text(rag_pipeline(), binary(), map()) -> [chunk()].
split_text(_Pipeline, Text, SplitterOpts) ->
    Splitter = beamai_rag_splitter:new(SplitterOpts),
    beamai_rag_splitter:split(Splitter, Text).

%%====================================================================
%% 文档索引 API
%%====================================================================

%% @doc 索引单个文档
%%
%% 流程：分割 → 嵌入 → 存储
-spec index_document(rag_pipeline(), binary(), map()) ->
    {ok, binary(), rag_pipeline()} | {error, term()}.
index_document(Pipeline, Content, Metadata) ->
    index_document(Pipeline, Content, Metadata, #{}).

%% @doc 索引单个文档（带选项）
-spec index_document(rag_pipeline(), binary(), map(), map()) ->
    {ok, binary(), rag_pipeline()} | {error, term()}.
index_document(Pipeline, Content, Metadata, _Opts) ->
    ParentId = beamai_id:gen_id(<<"parent">>),
    Chunks = split_text(Pipeline, Content),
    store_chunks(Pipeline, Chunks, ParentId, Metadata).

%% @doc 批量索引文档
-spec index_documents(rag_pipeline(), [{binary(), map()}], map()) ->
    {ok, [binary()], rag_pipeline()} | {error, term()}.
index_documents(Pipeline, Documents, Opts) ->
    Folder = fun({Content, Meta}, {Ids, Acc}) ->
        case index_document(Acc, Content, Meta, Opts) of
            {ok, DocId, NewPipe} -> {[DocId | Ids], NewPipe};
            {error, _} -> {Ids, Acc}
        end
    end,
    {DocIds, FinalPipeline} = lists:foldl(Folder, {[], Pipeline}, Documents),
    {ok, lists:reverse(DocIds), FinalPipeline}.

%% @doc 索引文件
-spec index_file(rag_pipeline(), binary() | string(), map()) ->
    {ok, binary(), rag_pipeline()} | {error, term()}.
index_file(Pipeline, FilePath, Metadata) ->
    Path = ensure_binary(FilePath),
    case file:read_file(Path) of
        {ok, Content} ->
            FileMeta = Metadata#{source => Path, filename => filename:basename(Path)},
            index_document(Pipeline, Content, FileMeta);
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

%%====================================================================
%% 检索操作 API
%%====================================================================

%% @doc 检索相关文档（默认选项）
-spec retrieve(rag_pipeline(), binary()) -> {ok, [beamai_vector_store:search_result()]}.
retrieve(Pipeline, Query) ->
    retrieve(Pipeline, Query, #{}).

%% @doc 检索相关文档
%%
%% 选项：
%% - top_k: 返回结果数量（默认 5）
%% - min_score: 最低相似度分数（默认 0.0）
%% - callbacks: 回调函数映射
%%   - on_retriever_start: fun(Query, Metadata) -> ok
%%   - on_retriever_end: fun(Documents, Metadata) -> ok
%%   - on_retriever_error: fun(Error, Metadata) -> ok
%% - callback_meta: 回调元数据（默认 #{}）
-spec retrieve(rag_pipeline(), binary(), map()) ->
    {ok, [beamai_vector_store:search_result()]} | {error, term()}.
retrieve(Pipeline, Query, Opts) ->
    #{embeddings := Model, vector_store := Store} = Pipeline,
    SearchOpts = #{
        top_k => maps:get(top_k, Opts, ?TOP_K),
        min_score => maps:get(min_score, Opts, 0.0)
    },

    %% 获取回调配置
    Callbacks = maps:get(callbacks, Opts, #{}),
    Meta = maps:get(callback_meta, Opts, #{}),

    %% 调用 on_retriever_start 回调
    invoke_callback(on_retriever_start, [Query], Callbacks, Meta),

    case embed_and_search(Model, Store, Query, SearchOpts) of
        {ok, Results} ->
            %% 调用 on_retriever_end 回调
            invoke_callback(on_retriever_end, [Results], Callbacks, Meta),
            {ok, Results};
        {error, Reason} ->
            %% 调用 on_retriever_error 回调
            invoke_callback(on_retriever_error, [Reason], Callbacks, Meta),
            {error, Reason}
    end.

%%====================================================================
%% RAG 生成 API
%%====================================================================

%% @doc RAG 查询（默认选项）
-spec query(rag_pipeline(), binary()) -> {ok, binary(), rag_pipeline()} | {error, term()}.
query(Pipeline, Question) ->
    query(Pipeline, Question, #{}).

%% @doc RAG 查询
%%
%% 流程：检索 → 构建上下文 → LLM 生成
%%
%% 选项：
%% - top_k: 检索结果数量
%% - max_context_length: 最大上下文长度
%% - callbacks: 回调函数映射（传递给 retrieve）
%% - callback_meta: 回调元数据
-spec query(rag_pipeline(), binary(), map()) ->
    {ok, binary(), rag_pipeline()} | {error, term()}.
query(Pipeline, Question, Opts) ->
    TopK = maps:get(top_k, Opts, ?TOP_K),
    MaxContext = maps:get(max_context_length, Opts, ?MAX_CONTEXT),

    %% 构建 retrieve 选项（包含回调）
    RetrieveOpts = #{
        top_k => TopK,
        callbacks => maps:get(callbacks, Opts, #{}),
        callback_meta => maps:get(callback_meta, Opts, #{})
    },

    case retrieve(Pipeline, Question, RetrieveOpts) of
        {ok, Results} ->
            Context = build_context(Results, MaxContext),
            generate_with_context(Pipeline, Question, Context);
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 管道状态 API
%%====================================================================

%% @doc 获取向量存储
-spec get_store(rag_pipeline()) -> beamai_vector_store:vector_store().
get_store(#{vector_store := Store}) ->
    Store.

%% @doc 获取已索引文档数量
-spec get_document_count(rag_pipeline()) -> non_neg_integer().
get_document_count(#{vector_store := Store}) ->
    beamai_vector_store:count(Store).

%%====================================================================
%% 私有函数 - 索引操作
%%====================================================================

%% @private 存储所有分块
-spec store_chunks(rag_pipeline(), [chunk()], binary(), map()) ->
    {ok, binary(), rag_pipeline()} | {error, term()}.
store_chunks(Pipeline, Chunks, ParentId, Metadata) ->
    #{embeddings := Model, vector_store := Store} = Pipeline,
    Texts = [maps:get(content, C) || C <- Chunks],

    case beamai_embeddings:embed_batch(Model, Texts) of
        {ok, Embeddings} ->
            Documents = build_documents(Chunks, Embeddings, ParentId, Metadata),
            {ok, _Ids, NewStore} = beamai_vector_store:add_documents(Store, Documents),
            {ok, ParentId, Pipeline#{vector_store := NewStore}};
        {error, Reason} ->
            {error, {embedding_error, Reason}}
    end.

%% @private 构建文档列表
-spec build_documents([chunk()], [beamai_embeddings:vector()], binary(), map()) -> [map()].
build_documents(Chunks, Embeddings, ParentId, BaseMeta) ->
    lists:zipwith(
        fun(#{content := C, chunk_id := CId, chunk_index := Idx}, Emb) ->
            #{
                content => C,
                embedding => Emb,
                metadata => BaseMeta#{parent_id => ParentId, chunk_id => CId, chunk_index => Idx}
            }
        end,
        Chunks, Embeddings
    ).

%%====================================================================
%% 私有函数 - 检索操作
%%====================================================================

%% @private 嵌入查询并搜索
-spec embed_and_search(beamai_embeddings:embedding_model(), beamai_vector_store:vector_store(), binary(), map()) ->
    {ok, [beamai_vector_store:search_result()]} | {error, term()}.
embed_and_search(Model, Store, Query, SearchOpts) ->
    case beamai_embeddings:embed_text(Model, Query) of
        {ok, QueryVector} ->
            beamai_vector_store:search(Store, QueryVector, SearchOpts);
        {error, Reason} ->
            {error, {embedding_error, Reason}}
    end.

%%====================================================================
%% 私有函数 - 上下文构建
%%====================================================================

%% @private 构建 RAG 上下文
-spec build_context([beamai_vector_store:search_result()], pos_integer()) -> binary().
build_context(Results, MaxLength) ->
    Contents = [format_result(R) || R <- Results],
    Context = iolist_to_binary(lists:join(<<"\n\n---\n\n">>, Contents)),
    truncate_binary(Context, MaxLength).

%% @private 格式化搜索结果
-spec format_result(beamai_vector_store:search_result()) -> binary().
format_result(#{document := Doc, score := Score}) ->
    Content = maps:get(content, Doc, <<>>),
    ScoreStr = float_to_binary(Score, [{decimals, 3}]),
    <<"[Score: ", ScoreStr/binary, "]\n", Content/binary>>.

%% @private 截断二进制
-spec truncate_binary(binary(), pos_integer()) -> binary().
truncate_binary(Bin, MaxLen) when byte_size(Bin) > MaxLen ->
    binary:part(Bin, 0, MaxLen);
truncate_binary(Bin, _MaxLen) ->
    Bin.

%%====================================================================
%% 私有函数 - LLM 生成
%%====================================================================

%% @private 使用上下文生成回答
-spec generate_with_context(rag_pipeline(), binary(), binary()) ->
    {ok, binary(), rag_pipeline()} | {error, term()}.
generate_with_context(Pipeline, Question, Context) ->
    Prompt = build_rag_prompt(Question, Context),
    case call_llm(Pipeline, Prompt) of
        {ok, Answer} -> {ok, Answer, Pipeline};
        {error, Reason} -> {error, Reason}
    end.

%% @private 构建 RAG 提示词
-spec build_rag_prompt(binary(), binary()) -> binary().
build_rag_prompt(Question, Context) ->
    <<"Based on the following context, please answer the question.\n\n",
      "Context:\n", Context/binary, "\n\n",
      "Question: ", Question/binary, "\n\n",
      "Answer:">>.

%% @private 调用 LLM
-spec call_llm(rag_pipeline(), binary()) -> {ok, binary()} | {error, term()}.
call_llm(#{llm := Config}, Prompt) ->
    try
        %% 如果已经是有效配置（beamai_chat_completion 格式），直接使用；否则创建
        LLMConfig = case maps:get('__llm_config__', Config, false) of
            true -> Config;
            false ->
                Provider = maps:get(provider, Config, openai),
                beamai_chat_completion:create(Provider, Config)
        end,
        Messages = [#{role => user, content => Prompt}],
        case beamai_chat_completion:chat(LLMConfig, Messages) of
            {ok, Response} -> {ok, extract_llm_content(Response)};
            {error, Reason} -> {error, {llm_error, Reason}}
        end
    catch
        _:_ -> {ok, <<"[LLM unavailable] Context provided for: ", Prompt/binary>>}
    end.

%% @private 提取 LLM 响应内容
%% beamai_chat_completion:chat/2 返回标准化响应，content 是 null | binary
-spec extract_llm_content(map()) -> binary().
extract_llm_content(#{content := null}) -> <<>>;
extract_llm_content(#{content := C}) when is_binary(C) -> C.

%%====================================================================
%% 私有函数 - 默认配置
%%====================================================================

%% @private 默认 LLM 配置
-spec default_llm_config() -> map().
default_llm_config() ->
    #{provider => openai, model => <<"gpt-4">>}.

%%====================================================================
%% 私有函数 - 回调系统
%%====================================================================

%% @private 调用回调函数
-spec invoke_callback(atom(), list(), map(), map()) -> ok.
invoke_callback(CallbackName, Args, Callbacks, Meta) ->
    case maps:get(CallbackName, Callbacks, undefined) of
        undefined ->
            ok;
        Handler when is_function(Handler) ->
            FullArgs = Args ++ [Meta],
            try
                erlang:apply(Handler, FullArgs)
            catch
                Class:Reason:Stack ->
                    logger:warning("RAG callback ~p failed: ~p:~p~n~p",
                                   [CallbackName, Class, Reason, Stack])
            end,
            ok
    end.
