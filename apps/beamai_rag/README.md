# Agent RAG

[English](README_EN.md) | 中文

检索增强生成（Retrieval-Augmented Generation）模块，提供向量存储和文档检索功能。

## 特性

- 文档分割
- 向量嵌入
- 向量存储
- 相似度搜索
- 与 LLM 集成

## 模块概览

- **beamai_rag** - RAG 主模块
- **beamai_embeddings** - 嵌入向量生成
- **beamai_vector_store** - 向量存储
- **beamai_rag_splitter** - 文档分割器
- **beamai_rag_utils** - 工具函数

## API 文档

### beamai_rag

```erlang
%% 创建 RAG 实例
beamai_rag:new(Config) -> {ok, Rag} | {error, Reason}.

%% 添加文档
beamai_rag:add_documents(Rag, Documents) -> ok | {error, Reason}.

%% 检索相关文档
beamai_rag:retrieve(Rag, Query, Options) -> {ok, Results} | {error, Reason}.

%% 检索并生成回答
beamai_rag:query(Rag, Question, Options) -> {ok, Answer} | {error, Reason}.
```

### beamai_embeddings

```erlang
%% 生成嵌入向量
beamai_embeddings:embed(Text, Config) -> {ok, Vector} | {error, Reason}.

%% 批量生成嵌入
beamai_embeddings:embed_batch(Texts, Config) -> {ok, Vectors} | {error, Reason}.
```

### beamai_vector_store

```erlang
%% 创建向量存储
beamai_vector_store:new(Config) -> {ok, Store} | {error, Reason}.

%% 添加向量
beamai_vector_store:add(Store, Id, Vector, Metadata) -> ok | {error, Reason}.

%% 相似度搜索
beamai_vector_store:search(Store, QueryVector, K) -> {ok, Results} | {error, Reason}.

%% 删除向量
beamai_vector_store:delete(Store, Id) -> ok | {error, Reason}.
```

### beamai_rag_splitter

```erlang
%% 分割文本
beamai_rag_splitter:split(Text, Options) -> [Chunk].

%% 分割选项
Options = #{
    chunk_size => 1000,      %% 每块大小
    chunk_overlap => 200,    %% 重叠大小
    separator => <<"\n\n">>  %% 分隔符
}.
```

## 使用示例

### 基本 RAG 流程

```erlang
%% 配置
Config = #{
    %% 嵌入模型配置
    embeddings => #{
        provider => openai,
        model => <<"text-embedding-ada-002">>,
        api_key => os:getenv("OPENAI_API_KEY")
    },
    %% LLM 配置（用于生成回答）
    llm => #{
        provider => openai,
        model => <<"gpt-4">>,
        api_key => os:getenv("OPENAI_API_KEY")
    }
},

%% 创建 RAG 实例
{ok, Rag} = beamai_rag:new(Config),

%% 添加文档
Documents = [
    #{content => <<"Erlang is a programming language...">>, metadata => #{source => <<"wiki">>}},
    #{content => <<"OTP is a set of libraries...">>, metadata => #{source => <<"docs">>}}
],
ok = beamai_rag:add_documents(Rag, Documents),

%% 检索并回答问题
{ok, Answer} = beamai_rag:query(Rag, <<"What is Erlang?">>, #{top_k => 3}).
```

### 自定义文档分割

```erlang
%% 长文本
LongText = <<"...very long document...">>,

%% 分割为小块
Chunks = beamai_rag_splitter:split(LongText, #{
    chunk_size => 500,
    chunk_overlap => 100
}),

%% 添加分割后的文档
Documents = [#{content => Chunk, metadata => #{index => I}}
             || {I, Chunk} <- lists:zip(lists:seq(1, length(Chunks)), Chunks)],
ok = beamai_rag:add_documents(Rag, Documents).
```

### 仅检索（不生成）

```erlang
%% 只检索相关文档，不调用 LLM
{ok, Results} = beamai_rag:retrieve(Rag, <<"Erlang OTP">>, #{top_k => 5}),

%% Results 包含相关文档和相似度分数
lists:foreach(fun(#{content := Content, score := Score}) ->
    io:format("Score: ~.3f~nContent: ~s~n~n", [Score, Content])
end, Results).
```

## 配置选项

### 嵌入模型

```erlang
#{
    embeddings => #{
        provider => openai | anthropic | ollama,
        model => <<"text-embedding-ada-002">>,
        api_key => <<"...">>,
        base_url => <<"...">>  %% 可选
    }
}
```

### 向量存储

```erlang
#{
    vector_store => #{
        backend => memory | sqlite,  %% 存储后端
        path => <<"/tmp/vectors.db">>  %% SQLite 路径
    }
}
```

## 依赖

- beamai_core
- beamai_llm（可选，用于生成回答）
- jsx

## 许可证

Apache-2.0
