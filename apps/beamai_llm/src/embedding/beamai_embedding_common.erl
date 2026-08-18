%%%-------------------------------------------------------------------
%%% @doc Embedding Provider 公共模块
%%%
%%% 抽取各 Embedding Provider 共用的请求体构建与响应解析逻辑，
%%% 与 beamai_llm_provider_common（对话）职责对等。
%%%
%%% 覆盖两类响应形态：
%%%   - OpenAI 兼容：data[].embedding + usage.prompt_tokens
%%%   - Ollama 原生：embeddings[] + prompt_eval_count
%%%   - DashScope 原生：output.embeddings[].embedding + usage.total_tokens
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embedding_common).

%% 请求构建
-export([build_openai_body/3, build_headers/1]).

%% 响应解析
-export([parser_openai/1, parser_ollama/1, parser_dashscope/1]).
-export([from_openai/2, from_ollama/2, from_dashscope/2]).

%% 响应构造与合并
-export([new_response/1, merge_responses/1, empty_response/2]).

%% 向量工具
-export([decode_embedding/1]).

-type response() :: beamai_embedding_provider_behaviour:response().

%%====================================================================
%% 请求构建
%%====================================================================

%% @doc 构建 OpenAI 兼容的向量化请求体
%%
%% Request 的字段优先于 Config，未设置的可选字段不发送。
-spec build_openai_body(map(), map(), binary()) -> map().
build_openai_body(Config, Request, DefaultModel) ->
    Base = #{
        <<"model">> => maps:get(model, Config, DefaultModel),
        <<"input">> => maps:get(input, Request, [])
    },
    Fields = [
        {encoding_format, <<"encoding_format">>},
        {dimensions, <<"dimensions">>},
        {user, <<"user">>}
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

%% @doc OpenAI 兼容响应解析器
-spec parser_openai(atom()) -> fun((map()) -> {ok, response()} | {error, term()}).
parser_openai(Provider) ->
    fun(Raw) -> from_openai(Raw, Provider) end.

%% @doc Ollama 原生响应解析器
-spec parser_ollama(atom()) -> fun((map()) -> {ok, response()} | {error, term()}).
parser_ollama(Provider) ->
    fun(Raw) -> from_ollama(Raw, Provider) end.

%% @doc DashScope 原生响应解析器
-spec parser_dashscope(atom()) -> fun((map()) -> {ok, response()} | {error, term()}).
parser_dashscope(Provider) ->
    fun(Raw) -> from_dashscope(Raw, Provider) end.

%% @doc 解析 OpenAI 兼容响应
%% data 元素带 index 时按 index 排序，保证与 input 顺序一致。
-spec from_openai(map(), atom()) -> {ok, response()} | {error, term()}.
from_openai(#{<<"data">> := Data} = Raw, Provider) when is_list(Data) ->
    Usage = maps:get(<<"usage">>, Raw, #{}),
    InputTokens = maps:get(<<"prompt_tokens">>, Usage, 0),
    {ok, new_response(#{
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => Provider,
        embeddings => [decode_embedding(maps:get(<<"embedding">>, D, []))
                       || D <- sort_by_index(Data, <<"index">>)],
        usage => #{
            input_tokens => InputTokens,
            total_tokens => maps:get(<<"total_tokens">>, Usage, InputTokens)
        },
        raw => Raw
    })};
from_openai(#{<<"error">> := Error}, _Provider) ->
    {error, {api_error, Error}};
from_openai(Raw, _Provider) ->
    {error, {invalid_response, Raw}}.

%% @doc 解析 Ollama 原生响应
%% /api/embed 返回 embeddings（二维数组），token 统计在 prompt_eval_count。
-spec from_ollama(map(), atom()) -> {ok, response()} | {error, term()}.
from_ollama(#{<<"embeddings">> := Embeddings} = Raw, Provider) when is_list(Embeddings) ->
    Tokens = maps:get(<<"prompt_eval_count">>, Raw, 0),
    {ok, new_response(#{
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => Provider,
        embeddings => [decode_embedding(E) || E <- Embeddings],
        usage => #{input_tokens => Tokens, total_tokens => Tokens},
        raw => Raw
    })};
from_ollama(#{<<"embedding">> := Embedding} = Raw, Provider) when is_list(Embedding) ->
    %% 兼容旧的 /api/embeddings 单条形态
    {ok, new_response(#{
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => Provider,
        embeddings => [decode_embedding(Embedding)],
        usage => #{input_tokens => 0, total_tokens => 0},
        raw => Raw
    })};
from_ollama(#{<<"error">> := Error}, _Provider) ->
    {error, {api_error, Error}};
from_ollama(Raw, _Provider) ->
    {error, {invalid_response, Raw}}.

%% @doc 解析 DashScope 原生响应
-spec from_dashscope(map(), atom()) -> {ok, response()} | {error, term()}.
from_dashscope(#{<<"output">> := #{<<"embeddings">> := Items}} = Raw, Provider)
  when is_list(Items) ->
    Usage = maps:get(<<"usage">>, Raw, #{}),
    Tokens = maps:get(<<"total_tokens">>, Usage, 0),
    {ok, new_response(#{
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => Provider,
        embeddings => [decode_embedding(maps:get(<<"embedding">>, I, []))
                       || I <- sort_by_index(Items, <<"text_index">>)],
        usage => #{input_tokens => Tokens, total_tokens => Tokens},
        raw => Raw,
        metadata => #{request_id => maps:get(<<"request_id">>, Raw, undefined)}
    })};
from_dashscope(#{<<"code">> := Code} = Raw, _Provider) when Code =/= null, Code =/= <<>> ->
    {error, {api_error, #{code => Code, message => maps:get(<<"message">>, Raw, <<>>)}}};
from_dashscope(Raw, _Provider) ->
    {error, {invalid_response, Raw}}.

%%====================================================================
%% 响应构造与合并
%%====================================================================

%% @doc 构造统一向量化响应（补齐缺省字段）
-spec new_response(map()) -> response().
new_response(Fields) ->
    #{
        model => maps:get(model, Fields, <<>>),
        provider => maps:get(provider, Fields, unknown),
        embeddings => maps:get(embeddings, Fields, []),
        usage => maps:get(usage, Fields, #{input_tokens => 0, total_tokens => 0}),
        raw => maps:get(raw, Fields, #{}),
        metadata => maps:get(metadata, Fields, #{})
    }.

%% @doc 空输入的零请求响应
-spec empty_response(binary(), atom()) -> response().
empty_response(Model, Provider) ->
    new_response(#{model => Model, provider => Provider}).

%% @doc 合并分批响应（向量按批次顺序拼接，usage 累加）
-spec merge_responses([response()]) -> response().
merge_responses([]) ->
    new_response(#{});
merge_responses([Single]) ->
    Single;
merge_responses([First | _] = Responses) ->
    Embeddings = lists:append([maps:get(embeddings, R, []) || R <- Responses]),
    Usage = lists:foldl(fun(R, Acc) ->
        U = maps:get(usage, R, #{}),
        #{
            input_tokens => maps:get(input_tokens, Acc, 0) + maps:get(input_tokens, U, 0),
            total_tokens => maps:get(total_tokens, Acc, 0) + maps:get(total_tokens, U, 0)
        }
    end, #{input_tokens => 0, total_tokens => 0}, Responses),
    First#{
        embeddings => Embeddings,
        usage => Usage,
        %% 分批时原始响应体只保留首批，避免无界增长
        metadata => maps:put(batches, length(Responses), maps:get(metadata, First, #{}))
    }.

%%====================================================================
%% 向量工具
%%====================================================================

%% @doc 归一化向量元素为 float
%% 兼容三种形态：数字列表、base64 编码的 float32 序列、异常值。
-spec decode_embedding(term()) -> [float()].
decode_embedding(Values) when is_list(Values) ->
    [to_float(V) || V <- Values, is_number(V)];
decode_embedding(Base64) when is_binary(Base64) ->
    %% encoding_format = base64 时为小端 float32 序列
    try decode_float32_le(base64:decode(Base64))
    catch _:_ -> []
    end;
decode_embedding(_) ->
    [].

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 按索引字段排序，缺失索引时保持原顺序
sort_by_index(Items, IndexKey) ->
    Indexed = lists:zip(lists:seq(1, length(Items)), Items),
    Sorted = lists:sort(fun({Pos1, I1}, {Pos2, I2}) ->
        {index_of(I1, IndexKey, Pos1), Pos1} =< {index_of(I2, IndexKey, Pos2), Pos2}
    end, Indexed),
    [Item || {_, Item} <- Sorted].

%% @private 取索引值，缺失时用位置兜底
index_of(Item, IndexKey, Pos) when is_map(Item) ->
    case maps:get(IndexKey, Item, undefined) of
        Index when is_integer(Index) -> Index;
        _ -> Pos
    end;
index_of(_, _, Pos) ->
    Pos.

to_float(V) when is_float(V) -> V;
to_float(V) when is_integer(V) -> float(V).

%% @private 解析小端 float32 序列
decode_float32_le(<<>>) -> [];
decode_float32_le(<<F:32/float-little, Rest/binary>>) -> [F | decode_float32_le(Rest)];
decode_float32_le(_) -> [].
