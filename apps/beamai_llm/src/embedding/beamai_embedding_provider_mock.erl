%%%-------------------------------------------------------------------
%%% @doc Mock Embedding Provider（测试用）
%%%
%%% 由文本哈希生成确定性伪向量，不产生任何网络请求：
%%% 相同文本恒得到相同向量，不同文本几乎必然不同，
%%% 便于在离线环境下测试向量检索与分批逻辑。
%%%
%%% 配置：
%%%   - dimensions: 向量维度（默认 8）
%%%
%%% 注意：伪向量不具备语义相似性，勿用于生产检索质量评估。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embedding_provider_mock).
-behaviour(beamai_embedding_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([embed/2, max_batch_size/0, supports_dimensions/0]).

-define(MODEL, <<"mock-embedding">>).
-define(DIMENSIONS, 8).

name() -> <<"Mock Embeddings">>.

default_config() ->
    #{model => ?MODEL, dimensions => ?DIMENSIONS}.

validate_config(_Config) -> ok.

max_batch_size() -> 1000.

supports_dimensions() -> true.

%% @doc 生成确定性伪向量
embed(Config, Request) ->
    Input = maps:get(input, Request, []),
    Dim = maps:get(dimensions, Request, maps:get(dimensions, Config, ?DIMENSIONS)),
    Model = maps:get(model, Config, ?MODEL),
    Embeddings = [hash_vector(Text, Dim) || Text <- Input],
    Tokens = lists:sum([byte_size(T) div 4 || T <- Input, is_binary(T)]),
    {ok, beamai_embedding_common:new_response(#{
        model => Model,
        provider => mock,
        embeddings => Embeddings,
        usage => #{input_tokens => Tokens, total_tokens => Tokens},
        raw => #{<<"mock">> => true}
    })}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 由 SHA-256 摘要展开出定长向量，元素落在 [-1, 1)
hash_vector(Text, Dim) when is_binary(Text), is_integer(Dim), Dim > 0 ->
    Digest = crypto:hash(sha256, Text),
    Size = byte_size(Digest),
    [begin
         Byte = binary:at(Digest, I rem Size),
         %% 高位轮次再混入索引，避免维度大于摘要长度时出现周期
         Mixed = (Byte + (I div Size) * 31) rem 256,
         (Mixed - 128) / 128.0
     end || I <- lists:seq(0, Dim - 1)];
hash_vector(_, Dim) ->
    lists:duplicate(Dim, 0.0).
