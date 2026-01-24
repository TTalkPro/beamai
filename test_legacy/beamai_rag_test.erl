%%%-------------------------------------------------------------------
%%% @doc RAG 模块测试
%%%
%%% 测试覆盖：
%%% - beamai_embeddings: 嵌入操作和向量运算
%%% - beamai_vector_store: 文档存储和搜索
%%% - beamai_rag: RAG 管道完整流程
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rag_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试生成器
%%====================================================================

beamai_embeddings_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"创建本地嵌入模型", fun test_create_local_model/0},
      {"创建 OpenAI 嵌入模型", fun test_create_openai_model/0},
      {"本地嵌入单个文本", fun test_embed_text_local/0},
      {"批量本地嵌入", fun test_embed_batch_local/0},
      {"余弦相似度计算", fun test_cosine_similarity/0},
      {"欧几里得距离计算", fun test_euclidean_distance/0},
      {"向量归一化", fun test_normalize/0},
      {"相同文本嵌入一致性", fun test_embedding_consistency/0}
     ]}.

beamai_vector_store_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"创建向量存储", fun test_create_vector_store/0},
      {"添加单个文档", fun test_add_document/0},
      {"批量添加文档", fun test_add_documents/0},
      {"获取文档", fun test_get_document/0},
      {"删除文档", fun test_delete_document/0},
      {"搜索相似文档", fun test_search_documents/0},
      {"搜索带阈值过滤", fun test_search_with_min_score/0},
      {"清空存储", fun test_clear_store/0}
     ]}.

beamai_rag_pipeline_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"创建 RAG 管道", fun test_create_rag_pipeline/0},
      {"文本分割", fun test_split_text/0},
      {"文本分割重叠", fun test_split_text_overlap/0},
      {"索引单个文档", fun test_index_document/0},
      {"批量索引文档", fun test_index_documents/0},
      {"检索相关文档", fun test_retrieve_documents/0},
      {"RAG 完整查询", fun test_rag_query/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% beamai_embeddings 测试
%%====================================================================

test_create_local_model() ->
    Model = beamai_embeddings:new_local(),
    ?assertEqual(local, maps:get(type, Model)),
    ?assertEqual(384, maps:get(dimension, Model)).

test_create_openai_model() ->
    Model = beamai_embeddings:new_openai(#{api_key => <<"test-key">>}),
    ?assertEqual(openai, maps:get(type, Model)),
    ?assertEqual(<<"test-key">>, maps:get(api_key, Model)),
    ?assertEqual(<<"text-embedding-3-small">>, maps:get(model, Model)).

test_embed_text_local() ->
    Model = beamai_embeddings:new_local(#{dimension => 128}),
    {ok, Vector} = beamai_embeddings:embed_text(Model, <<"Hello world">>),
    ?assertEqual(128, length(Vector)),
    %% 验证向量元素为浮点数
    ?assert(lists:all(fun is_float/1, Vector)).

test_embed_batch_local() ->
    Model = beamai_embeddings:new_local(#{dimension => 64}),
    Texts = [<<"Text 1">>, <<"Text 2">>, <<"Text 3">>],
    {ok, Vectors} = beamai_embeddings:embed_batch(Model, Texts),
    ?assertEqual(3, length(Vectors)),
    ?assert(lists:all(fun(V) -> length(V) =:= 64 end, Vectors)).

test_cosine_similarity() ->
    %% 相同向量的相似度应为 1
    Vec1 = [1.0, 0.0, 0.0],
    Sim1 = beamai_embeddings:cosine_similarity(Vec1, Vec1),
    ?assert(abs(Sim1 - 1.0) < 0.0001),

    %% 正交向量的相似度应为 0
    Vec2 = [0.0, 1.0, 0.0],
    Sim2 = beamai_embeddings:cosine_similarity(Vec1, Vec2),
    ?assert(abs(Sim2) < 0.0001),

    %% 反向向量的相似度应为 -1
    Vec3 = [-1.0, 0.0, 0.0],
    Sim3 = beamai_embeddings:cosine_similarity(Vec1, Vec3),
    ?assert(abs(Sim3 - (-1.0)) < 0.0001).

test_euclidean_distance() ->
    Vec1 = [0.0, 0.0],
    Vec2 = [3.0, 4.0],
    Dist = beamai_embeddings:euclidean_distance(Vec1, Vec2),
    ?assert(abs(Dist - 5.0) < 0.0001).

test_normalize() ->
    Vec = [3.0, 4.0],
    Normalized = beamai_embeddings:normalize(Vec),
    ?assertEqual(2, length(Normalized)),
    %% 归一化后模应为 1
    Norm = math:sqrt(lists:sum([X * X || X <- Normalized])),
    ?assert(abs(Norm - 1.0) < 0.0001).

test_embedding_consistency() ->
    %% 相同文本应生成相同的嵌入
    Model = beamai_embeddings:new_local(),
    Text = <<"Test text for consistency">>,
    {ok, Vec1} = beamai_embeddings:embed_text(Model, Text),
    {ok, Vec2} = beamai_embeddings:embed_text(Model, Text),
    ?assertEqual(Vec1, Vec2).

%%====================================================================
%% beamai_vector_store 测试
%%====================================================================

test_create_vector_store() ->
    Store = beamai_vector_store:new(),
    ?assertEqual(0, beamai_vector_store:count(Store)).

test_add_document() ->
    Store = beamai_vector_store:new(),
    Doc = #{
        content => <<"Hello world">>,
        embedding => [0.1, 0.2, 0.3],
        metadata => #{source => <<"test">>}
    },
    {ok, DocId, NewStore} = beamai_vector_store:add_document(Store, Doc),
    ?assert(is_binary(DocId)),
    ?assertEqual(1, beamai_vector_store:count(NewStore)).

test_add_documents() ->
    Store = beamai_vector_store:new(),
    Docs = [
        #{content => <<"Doc 1">>, embedding => [0.1, 0.2]},
        #{content => <<"Doc 2">>, embedding => [0.3, 0.4]},
        #{content => <<"Doc 3">>, embedding => [0.5, 0.6]}
    ],
    {ok, DocIds, NewStore} = beamai_vector_store:add_documents(Store, Docs),
    ?assertEqual(3, length(DocIds)),
    ?assertEqual(3, beamai_vector_store:count(NewStore)).

test_get_document() ->
    Store = beamai_vector_store:new(),
    Doc = #{content => <<"Test content">>, embedding => [0.1, 0.2]},
    {ok, DocId, NewStore} = beamai_vector_store:add_document(Store, Doc),

    {ok, RetrievedDoc} = beamai_vector_store:get_document(NewStore, DocId),
    ?assertEqual(<<"Test content">>, maps:get(content, RetrievedDoc)),

    ?assertEqual({error, not_found}, beamai_vector_store:get_document(NewStore, <<"invalid-id">>)).

test_delete_document() ->
    Store = beamai_vector_store:new(),
    Doc = #{content => <<"To be deleted">>, embedding => [0.1, 0.2]},
    {ok, DocId, Store1} = beamai_vector_store:add_document(Store, Doc),
    ?assertEqual(1, beamai_vector_store:count(Store1)),

    {ok, Store2} = beamai_vector_store:delete_document(Store1, DocId),
    ?assertEqual(0, beamai_vector_store:count(Store2)),

    ?assertEqual({error, not_found}, beamai_vector_store:delete_document(Store2, DocId)).

test_search_documents() ->
    Store = beamai_vector_store:new(),
    %% 添加一些文档
    Docs = [
        #{content => <<"Apple">>, embedding => [1.0, 0.0, 0.0]},
        #{content => <<"Orange">>, embedding => [0.0, 1.0, 0.0]},
        #{content => <<"Banana">>, embedding => [0.0, 0.0, 1.0]}
    ],
    {ok, _DocIds, Store1} = beamai_vector_store:add_documents(Store, Docs),

    %% 搜索与 Apple 相似的文档
    QueryVec = [0.9, 0.1, 0.0],
    {ok, Results} = beamai_vector_store:search(Store1, QueryVec, #{top_k => 2}),

    ?assertEqual(2, length(Results)),
    %% 第一个结果应该是 Apple（最相似）
    FirstResult = hd(Results),
    ?assertEqual(<<"Apple">>, maps:get(content, maps:get(document, FirstResult))).

test_search_with_min_score() ->
    Store = beamai_vector_store:new(),
    Docs = [
        #{content => <<"High similarity">>, embedding => [1.0, 0.0]},
        #{content => <<"Low similarity">>, embedding => [0.0, 1.0]}
    ],
    {ok, _DocIds, Store1} = beamai_vector_store:add_documents(Store, Docs),

    %% 使用高阈值搜索
    QueryVec = [1.0, 0.0],
    {ok, Results} = beamai_vector_store:search(Store1, QueryVec, #{
        top_k => 10,
        min_score => 0.9
    }),

    %% 只有高相似度的文档应该返回
    ?assertEqual(1, length(Results)).

test_clear_store() ->
    Store = beamai_vector_store:new(),
    Docs = [
        #{content => <<"Doc 1">>, embedding => [0.1, 0.2]},
        #{content => <<"Doc 2">>, embedding => [0.3, 0.4]}
    ],
    {ok, _DocIds, Store1} = beamai_vector_store:add_documents(Store, Docs),
    ?assertEqual(2, beamai_vector_store:count(Store1)),

    Store2 = beamai_vector_store:clear(Store1),
    ?assertEqual(0, beamai_vector_store:count(Store2)).

%%====================================================================
%% beamai_rag 管道测试
%%====================================================================

test_create_rag_pipeline() ->
    Pipeline = beamai_rag:new(),
    ?assertEqual(0, beamai_rag:get_document_count(Pipeline)),

    %% 自定义配置
    EmbeddingsModel = beamai_embeddings:new_local(#{dimension => 256}),
    Pipeline2 = beamai_rag:new(#{embeddings => EmbeddingsModel}),
    ?assertEqual(256, maps:get(dimension, maps:get(embeddings, Pipeline2))).

test_split_text() ->
    Pipeline = beamai_rag:new(#{
        text_splitter => #{chunk_size => 20, chunk_overlap => 5}
    }),

    Text = <<"Line 1\nLine 2\nLine 3\nLine 4\nLine 5">>,
    Chunks = beamai_rag:split_text(Pipeline, Text),

    ?assert(length(Chunks) >= 1),
    %% 每个 chunk 应有 content 和 chunk_id
    FirstChunk = hd(Chunks),
    ?assert(maps:is_key(content, FirstChunk)),
    ?assert(maps:is_key(chunk_id, FirstChunk)).

test_split_text_overlap() ->
    Pipeline = beamai_rag:new(#{
        text_splitter => #{chunk_size => 50, chunk_overlap => 10, separator => <<" ">>}
    }),

    Text = <<"This is a long text that should be split into multiple chunks with overlap">>,
    Chunks = beamai_rag:split_text(Pipeline, Text),

    ?assert(length(Chunks) >= 1).

test_index_document() ->
    Pipeline = beamai_rag:new(),

    Content = <<"Erlang is a programming language used to build massively scalable soft real-time systems.">>,
    Metadata = #{source => <<"wiki">>, topic => <<"programming">>},

    {ok, DocId, Pipeline1} = beamai_rag:index_document(Pipeline, Content, Metadata),

    ?assert(is_binary(DocId)),
    ?assert(beamai_rag:get_document_count(Pipeline1) > 0).

test_index_documents() ->
    Pipeline = beamai_rag:new(),

    Documents = [
        {<<"Erlang was designed for telecom systems.">>, #{topic => <<"erlang">>}},
        {<<"Elixir runs on the Erlang VM.">>, #{topic => <<"elixir">>}},
        {<<"OTP is a collection of middleware for Erlang.">>, #{topic => <<"otp">>}}
    ],

    {ok, DocIds, Pipeline1} = beamai_rag:index_documents(Pipeline, Documents, #{}),

    ?assertEqual(3, length(DocIds)),
    ?assert(beamai_rag:get_document_count(Pipeline1) >= 3).

test_retrieve_documents() ->
    Pipeline = beamai_rag:new(),

    %% 索引一些文档
    Documents = [
        {<<"Apples are red fruits.">>, #{category => <<"fruits">>}},
        {<<"Oranges are citrus fruits.">>, #{category => <<"fruits">>}},
        {<<"Cars are vehicles with wheels.">>, #{category => <<"vehicles">>}}
    ],

    {ok, _DocIds, Pipeline1} = beamai_rag:index_documents(Pipeline, Documents, #{}),

    %% 检索与水果相关的文档
    {ok, Results} = beamai_rag:retrieve(Pipeline1, <<"What are some fruits?">>, #{top_k => 2}),

    ?assert(length(Results) =< 2),
    ?assert(length(Results) >= 1).

test_rag_query() ->
    Pipeline = beamai_rag:new(),

    %% 索引文档
    Content = <<"Erlang is a general-purpose, concurrent, functional programming language. "
                "It was designed for building massively scalable soft real-time systems "
                "with requirements on high availability.">>,

    {ok, _DocId, Pipeline1} = beamai_rag:index_document(Pipeline, Content, #{source => <<"test">>}),

    %% 执行 RAG 查询
    {ok, Answer, _Pipeline2} = beamai_rag:query(Pipeline1, <<"What is Erlang?">>, #{top_k => 3}),

    ?assert(is_binary(Answer)),
    ?assert(byte_size(Answer) > 0).
