# Agent RAG

English | [中文](README.md)

Retrieval-Augmented Generation module, providing vector storage and document retrieval functionality.

## Features

- Document splitting
- Vector embedding
- Vector storage
- Similarity search
- Integration with LLM

## Module Overview

- **beamai_rag** - Main RAG module
- **beamai_embeddings** - Embedding vector generation
- **beamai_vector_store** - Vector storage
- **beamai_rag_splitter** - Document splitter
- **beamai_rag_utils** - Utility functions

## API Documentation

### beamai_rag

```erlang
%% Create RAG instance
beamai_rag:new(Config) -> {ok, Rag} | {error, Reason}.

%% Add documents
beamai_rag:add_documents(Rag, Documents) -> ok | {error, Reason}.

%% Retrieve relevant documents
beamai_rag:retrieve(Rag, Query, Options) -> {ok, Results} | {error, Reason}.

%% Retrieve and generate answer
beamai_rag:query(Rag, Question, Options) -> {ok, Answer} | {error, Reason}.
```

### beamai_embeddings

```erlang
%% Generate embedding vector
beamai_embeddings:embed(Text, Config) -> {ok, Vector} | {error, Reason}.

%% Batch generate embeddings
beamai_embeddings:embed_batch(Texts, Config) -> {ok, Vectors} | {error, Reason}.
```

### beamai_vector_store

```erlang
%% Create vector store
beamai_vector_store:new(Config) -> {ok, Store} | {error, Reason}.

%% Add vector
beamai_vector_store:add(Store, Id, Vector, Metadata) -> ok | {error, Reason}.

%% Similarity search
beamai_vector_store:search(Store, QueryVector, K) -> {ok, Results} | {error, Reason}.

%% Delete vector
beamai_vector_store:delete(Store, Id) -> ok | {error, Reason}.
```

### beamai_rag_splitter

```erlang
%% Split text
beamai_rag_splitter:split(Text, Options) -> [Chunk].

%% Split options
Options = #{
    chunk_size => 1000,      %% Size of each chunk
    chunk_overlap => 200,    %% Overlap size
    separator => <<"\n\n">>  %% Separator
}.
```

## Usage Examples

### Basic RAG Flow

```erlang
%% Configuration
Config = #{
    %% Embedding model configuration
    embeddings => #{
        provider => openai,
        model => <<"text-embedding-ada-002">>,
        api_key => os:getenv("OPENAI_API_KEY")
    },
    %% LLM configuration (for generating answers)
    llm => #{
        provider => openai,
        model => <<"gpt-4">>,
        api_key => os:getenv("OPENAI_API_KEY")
    }
},

%% Create RAG instance
{ok, Rag} = beamai_rag:new(Config),

%% Add documents
Documents = [
    #{content => <<"Erlang is a programming language...">>, metadata => #{source => <<"wiki">>}},
    #{content => <<"OTP is a set of libraries...">>, metadata => #{source => <<"docs">>}}
],
ok = beamai_rag:add_documents(Rag, Documents),

%% Retrieve and answer question
{ok, Answer} = beamai_rag:query(Rag, <<"What is Erlang?">>, #{top_k => 3}).
```

### Custom Document Splitting

```erlang
%% Long text
LongText = <<"...very long document...">>,

%% Split into smaller chunks
Chunks = beamai_rag_splitter:split(LongText, #{
    chunk_size => 500,
    chunk_overlap => 100
}),

%% Add split documents
Documents = [#{content => Chunk, metadata => #{index => I}}
             || {I, Chunk} <- lists:zip(lists:seq(1, length(Chunks)), Chunks)],
ok = beamai_rag:add_documents(Rag, Documents).
```

### Retrieval Only (Without Generation)

```erlang
%% Only retrieve relevant documents, without calling LLM
{ok, Results} = beamai_rag:retrieve(Rag, <<"Erlang OTP">>, #{top_k => 5}),

%% Results contain relevant documents and similarity scores
lists:foreach(fun(#{content := Content, score := Score}) ->
    io:format("Score: ~.3f~nContent: ~s~n~n", [Score, Content])
end, Results).
```

## Configuration Options

### Embedding Model

```erlang
#{
    embeddings => #{
        provider => openai | anthropic | ollama,
        model => <<"text-embedding-ada-002">>,
        api_key => <<"...">>,
        base_url => <<"...">>  %% Optional
    }
}
```

### Vector Store

```erlang
#{
    vector_store => #{
        backend => memory | sqlite,  %% Storage backend
        path => <<"/tmp/vectors.db">>  %% SQLite path
    }
}
```

## Dependencies

- beamai_core
- beamai_llm (optional, for generating answers)
- jsx

## License

Apache-2.0
