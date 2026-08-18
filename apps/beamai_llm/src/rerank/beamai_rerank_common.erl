%%%-------------------------------------------------------------------
%%% @doc Rerank Provider 公共模块
%%%
%%% 抽取各重排序 Provider 共用的请求体构建与响应解析逻辑，
%%% 与 beamai_llm_provider_common / beamai_embedding_common 职责对等。
%%%
%%% 覆盖三类响应形态：
%%%   - results 风格：`results[].{index, relevance_score}'
%%%     （SiliconFlow / Jina / Cohere）
%%%   - data 风格：`data[].{index, relevance_score}'（Voyage）
%%%   - DashScope 原生：`output.results[].{index, relevance_score}'
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rerank_common).

%% 请求构建
-export([build_body/3, build_body/4, build_headers/1]).

%% 响应解析
-export([parser_results/1, parser_data/1, parser_dashscope/1]).
-export([from_results/2, from_data/2, from_dashscope/2]).

%% 响应构造
-export([new_response/1, empty_response/2]).

-type response() :: beamai_rerank_provider_behaviour:response().

%%====================================================================
%% 请求构建
%%====================================================================

%% @doc 构建通用重排序请求体（top_n 字段名为 `<<"top_n">>'）
-spec build_body(map(), map(), binary()) -> map().
build_body(Config, Request, DefaultModel) ->
    build_body(Config, Request, DefaultModel, <<"top_n">>).

%% @doc 构建通用重排序请求体，指定 top_n 的字段名
%%
%% Voyage 用 `top_k'，其余厂商用 `top_n'。
%% Request 的字段优先于 Config，未设置的可选字段不发送。
-spec build_body(map(), map(), binary(), binary()) -> map().
build_body(Config, Request, DefaultModel, TopNKey) ->
    Base = #{
        <<"model">> => maps:get(model, Config, DefaultModel),
        <<"query">> => maps:get(query, Request, <<>>),
        <<"documents">> => maps:get(documents, Request, [])
    },
    Fields = [
        {top_n, TopNKey},
        {return_documents, <<"return_documents">>},
        {max_chunks_per_doc, <<"max_chunks_per_doc">>},
        {overlap_tokens, <<"overlap_tokens">>}
    ],
    lists:foldl(fun({Key, JsonKey}, Acc) ->
        case maps:get(Key, Request, maps:get(Key, Config, undefined)) of
            undefined -> Acc;
            Value -> Acc#{JsonKey => Value}
        end
    end, Base, Fields).

%% @doc 构建请求头（有 api_key 时用 Bearer，否则仅 Content-Type）
-spec build_headers(map()) -> [{binary(), binary()}].
build_headers(#{api_key := ApiKey}) when is_binary(ApiKey), byte_size(ApiKey) > 0 ->
    beamai_llm_provider_common:build_bearer_auth_headers(#{api_key => ApiKey});
build_headers(_Config) ->
    [{<<"Content-Type">>, <<"application/json">>}].

%%====================================================================
%% 响应解析
%%====================================================================

%% @doc results 风格响应解析器（SiliconFlow / Jina / Cohere）
-spec parser_results(atom()) -> fun((map()) -> {ok, response()} | {error, term()}).
parser_results(Provider) ->
    fun(Raw) -> from_results(Raw, Provider) end.

%% @doc data 风格响应解析器（Voyage）
-spec parser_data(atom()) -> fun((map()) -> {ok, response()} | {error, term()}).
parser_data(Provider) ->
    fun(Raw) -> from_data(Raw, Provider) end.

%% @doc DashScope 原生响应解析器
-spec parser_dashscope(atom()) -> fun((map()) -> {ok, response()} | {error, term()}).
parser_dashscope(Provider) ->
    fun(Raw) -> from_dashscope(Raw, Provider) end.

%% @doc 解析 results 风格响应
-spec from_results(map(), atom()) -> {ok, response()} | {error, term()}.
from_results(#{<<"results">> := Items} = Raw, Provider) when is_list(Items) ->
    {ok, build_response(Raw, Provider, Items)};
from_results(#{<<"error">> := Error}, _Provider) ->
    {error, {api_error, Error}};
from_results(Raw, _Provider) ->
    {error, {invalid_response, Raw}}.

%% @doc 解析 data 风格响应
-spec from_data(map(), atom()) -> {ok, response()} | {error, term()}.
from_data(#{<<"data">> := Items} = Raw, Provider) when is_list(Items) ->
    {ok, build_response(Raw, Provider, Items)};
from_data(#{<<"error">> := Error}, _Provider) ->
    {error, {api_error, Error}};
from_data(Raw, _Provider) ->
    {error, {invalid_response, Raw}}.

%% @doc 解析 DashScope 原生响应
-spec from_dashscope(map(), atom()) -> {ok, response()} | {error, term()}.
from_dashscope(#{<<"output">> := #{<<"results">> := Items}} = Raw, Provider)
  when is_list(Items) ->
    Response = build_response(Raw, Provider, Items),
    Metadata = maps:get(metadata, Response),
    {ok, Response#{metadata => Metadata#{request_id => maps:get(<<"request_id">>, Raw, undefined)}}};
from_dashscope(#{<<"code">> := Code} = Raw, _Provider) when Code =/= null, Code =/= <<>> ->
    {error, {api_error, #{code => Code, message => maps:get(<<"message">>, Raw, <<>>)}}};
from_dashscope(Raw, _Provider) ->
    {error, {invalid_response, Raw}}.

%%====================================================================
%% 响应构造
%%====================================================================

%% @doc 构造统一重排序响应（补齐缺省字段）
-spec new_response(map()) -> response().
new_response(Fields) ->
    #{
        model => maps:get(model, Fields, <<>>),
        provider => maps:get(provider, Fields, unknown),
        results => maps:get(results, Fields, []),
        usage => maps:get(usage, Fields, #{input_tokens => 0, total_tokens => 0}),
        raw => maps:get(raw, Fields, #{}),
        metadata => maps:get(metadata, Fields, #{})
    }.

%% @doc 空输入的零请求响应
-spec empty_response(binary(), atom()) -> response().
empty_response(Model, Provider) ->
    new_response(#{model => Model, provider => Provider}).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 由结果条目列表构造统一响应（按得分降序）
build_response(Raw, Provider, Items) ->
    new_response(#{
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => Provider,
        results => sort_by_score([parse_item(I) || I <- Items, is_map(I)]),
        usage => parse_usage(Raw),
        raw => Raw,
        metadata => #{id => maps:get(<<"id">>, Raw, undefined)}
    }).

%% @private 解析单条结果
%% 文档原文各家形态不一：`document' 可能是 `#{<<"text">> => ...}' 或直接字符串，
%% 未开启 return_documents 时缺省，由 beamai_rerank 按 index 回填。
parse_item(Item) ->
    Base = #{
        index => to_index(maps:get(<<"index">>, Item, 0)),
        score => to_float(maps:get(<<"relevance_score">>,
                                   Item, maps:get(<<"score">>, Item, 0)))
    },
    case document_text(maps:get(<<"document">>, Item, undefined)) of
        undefined -> Base;
        Text -> Base#{document => Text}
    end.

document_text(#{<<"text">> := Text}) when is_binary(Text) -> Text;
document_text(Text) when is_binary(Text) -> Text;
document_text(_) -> undefined.

%% @private 按得分降序，得分相同保持原索引顺序
sort_by_score(Results) ->
    lists:sort(fun(#{score := S1, index := I1}, #{score := S2, index := I2}) ->
        {-S1, I1} =< {-S2, I2}
    end, Results).

%% @private 解析 token 统计
%% SiliconFlow 用 tokens.{input,output}_tokens，Jina / Voyage 用 usage.total_tokens，
%% Cohere 用 meta.billed_units.search_units（计费单元，非 token）。
parse_usage(Raw) ->
    Usage = maps:get(<<"usage">>, Raw, #{}),
    Tokens = maps:get(<<"tokens">>, Raw, #{}),
    Input = first_number([
        maps:get(<<"input_tokens">>, Tokens, undefined),
        maps:get(<<"prompt_tokens">>, Usage, undefined),
        maps:get(<<"total_tokens">>, Usage, undefined)
    ]),
    Total = first_number([
        maps:get(<<"total_tokens">>, Usage, undefined),
        sum_tokens(Tokens),
        maps:get(<<"total_tokens">>, Tokens, undefined)
    ]),
    Base = #{input_tokens => Input, total_tokens => max(Total, Input)},
    case search_units(Raw) of
        undefined -> Base;
        Units -> Base#{details => #{search_units => Units}}
    end.

%% @private SiliconFlow 的 tokens 结构累加
sum_tokens(Tokens) when is_map(Tokens), map_size(Tokens) > 0 ->
    case {maps:get(<<"input_tokens">>, Tokens, undefined),
          maps:get(<<"output_tokens">>, Tokens, undefined)} of
        {undefined, undefined} -> undefined;
        {In, Out} -> zero_if_undefined(In) + zero_if_undefined(Out)
    end;
sum_tokens(_) ->
    undefined.

%% @private Cohere 计费单元
search_units(#{<<"meta">> := #{<<"billed_units">> := #{<<"search_units">> := Units}}}) ->
    Units;
search_units(_) ->
    undefined.

first_number([]) -> 0;
first_number([N | _]) when is_number(N) -> N;
first_number([_ | Rest]) -> first_number(Rest).

zero_if_undefined(N) when is_number(N) -> N;
zero_if_undefined(_) -> 0.

to_float(V) when is_float(V) -> V;
to_float(V) when is_integer(V) -> float(V);
to_float(_) -> 0.0.

to_index(V) when is_integer(V), V >= 0 -> V;
to_index(_) -> 0.
