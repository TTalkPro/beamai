%%%-------------------------------------------------------------------
%%% @doc Mock Rerank Provider（测试用）
%%%
%%% 按 query 与文档的词重叠率打分，不产生任何网络请求：
%%% 相同输入恒得到相同排序，便于离线测试 RAG 的重排序链路。
%%%
%%% 注意：词重叠不是语义相关性，勿用于生产检索质量评估。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rerank_provider_mock).
-behaviour(beamai_rerank_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([rerank/2, max_documents/0]).

-define(MODEL, <<"mock-rerank">>).

name() -> <<"Mock Rerank">>.

default_config() -> #{model => ?MODEL}.

validate_config(_Config) -> ok.

max_documents() -> 10000.

%% @doc 按词重叠率打分并降序返回
rerank(Config, Request) ->
    Query = maps:get(query, Request, <<>>),
    Documents = maps:get(documents, Request, []),
    QueryTokens = tokens(Query),
    Scored = [#{index => Index, score => overlap_score(QueryTokens, tokens(Doc))}
              || {Index, Doc} <- lists:zip(lists:seq(0, length(Documents) - 1), Documents)],
    Sorted = lists:sort(fun(#{score := S1, index := I1}, #{score := S2, index := I2}) ->
        {-S1, I1} =< {-S2, I2}
    end, Scored),
    Results = apply_top_n(Sorted, maps:get(top_n, Request, undefined)),
    {ok, beamai_rerank_common:new_response(#{
        model => maps:get(model, Config, ?MODEL),
        provider => mock,
        results => Results,
        usage => #{input_tokens => length(QueryTokens), total_tokens => length(QueryTokens)},
        raw => #{<<"mock">> => true}
    })}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 简易分词：按空白切分并小写化
tokens(Text) when is_binary(Text) ->
    [T || T <- binary:split(string:lowercase(Text), [<<" ">>, <<"\n">>, <<"\t">>], [global]),
          T =/= <<>>];
tokens(_) ->
    [].

%% @private 重叠词数 / query 词数
overlap_score(_QueryTokens = [], _DocTokens) -> 0.0;
overlap_score(QueryTokens, DocTokens) ->
    Matched = length([T || T <- QueryTokens, lists:member(T, DocTokens)]),
    Matched / length(QueryTokens).

%% @private 截断到前 N 条
apply_top_n(Results, N) when is_integer(N), N > 0, N < length(Results) ->
    lists:sublist(Results, N);
apply_top_n(Results, _) ->
    Results.
