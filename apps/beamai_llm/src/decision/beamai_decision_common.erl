%%%-------------------------------------------------------------------
%%% @doc 决策 Provider 公共模块（System One 协议的请求构建 / 响应解析 / HTTP 调用）
%%%
%%% 与 beamai_embedding_common / beamai_rerank_common 职责对等。所有走 HTTP 的
%%% System One Provider 都只是「base_url + 鉴权 + 默认模型」不同，请求与响应形状
%%% 完全一致，故发请求的整条路径收在 decide_http/5 里。
%%%
%%% == 线上格式 ==
%%%
%%% 请求 `POST {base_url}/systemone'：
%%% ```json
%%% {"model": "decision-model-preview",
%%%  "state": {"message": "我的账单被扣了两次，要退款！"},
%%%  "questions": {
%%%    "department": {"type": "choice", "instructions": "该由哪个部门处理？",
%%%                   "criteria": {"billing": "付款、账单、退款", "tech": "故障"}},
%%%    "escalate":   {"type": "noul",   "instructions": "是否需要转人工？"},
%%%    "severity":   {"type": "score",  "instructions": "问题多严重？",
%%%                   "criteria": ["轻微", "一般", "严重", "紧急"]}}}
%%% '''
%%% 响应：
%%% ```json
%%% {"answers": {
%%%    "department": {"type": "choice", "choice": "billing", "confidence": 0.97,
%%%                   "probabilities": {"billing": 0.97, "tech": 0.03}},
%%%    "escalate":   {"type": "noul", "noul": 0.81},
%%%    "severity":   {"type": "score", "score": 2.65, "confidence": 0.9,
%%%                   "probabilities": {"0": 0.0, "1": 0.1, "2": 0.15, "3": 0.75},
%%%                   "legend": {"0": "轻微", "1": "一般", "2": "严重", "3": "紧急"}}},
%%%  "model": "decision-model-preview", "request_id": "...", "latency_ms": 50.5,
%%%  "usage": {"input_tokens": 100}}
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_decision_common).

%% 请求构建
-export([build_body/3, build_headers/1]).
-export([normalize_state/1, normalize_questions/1, validate_questions/1]).

%% 响应解析
-export([parser/1, from_systemone/2, normalize_answer/1, new_response/1]).

%% HTTP 调用（各 HTTP provider 的 decide/2 一行委托）
-export([decide_http/5]).

-define(ENDPOINT, <<"/systemone">>).

-type response() :: beamai_decision_provider_behaviour:response().
-type answer() :: beamai_decision_provider_behaviour:answer().

%%====================================================================
%% HTTP 调用
%%====================================================================

%% @doc 发一次 System One 请求并解析为统一响应
%%
%% Config 的 base_url 覆盖 DefaultBaseUrl、endpoint 覆盖 `/systemone'；
%% 超时与连接池取值与对话 provider 一致（request_timeout / with_pool_opt）。
-spec decide_http(atom(), map(), map(), binary(), binary()) -> {ok, response()} | {error, term()}.
decide_http(Provider, Config, Request, DefaultBaseUrl, DefaultModel) ->
    Url = beamai_llm_provider_common:build_url(Config, ?ENDPOINT, DefaultBaseUrl),
    Headers = build_headers(Config),
    Body = build_body(Config, Request, DefaultModel),
    Opts = beamai_llm_provider_common:with_pool_opt(
        #{timeout => beamai_llm_provider_common:request_timeout(Config, Provider)}, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts, parser(Provider)).

%%====================================================================
%% 请求构建
%%====================================================================

%% @doc 构建 System One 请求体（Request 的 extra 作为顶层附加字段合入，不覆盖主字段）
-spec build_body(map(), map(), binary()) -> map().
build_body(Config, Request, DefaultModel) ->
    Extra = binary_keys(maps:get(extra, Request, #{})),
    maps:merge(Extra, #{
        <<"model">> => maps:get(model, Config, DefaultModel),
        <<"state">> => maps:get(state, Request, #{}),
        <<"questions">> => maps:get(questions, Request, #{})
    }).

%% @doc 构建请求头（有 api_key 时用 Bearer，否则仅 Content-Type——本地 Laya 等无鉴权）
-spec build_headers(map()) -> [{binary(), binary()}].
build_headers(#{api_key := ApiKey}) when is_binary(ApiKey), byte_size(ApiKey) > 0 ->
    beamai_llm_provider_common:build_bearer_auth_headers(#{api_key => ApiKey});
build_headers(_Config) ->
    [{<<"Content-Type">>, <<"application/json">>}].

%% @doc state 归一：纯文本视为 `message' 字段；map 的键转 binary
-spec normalize_state(binary() | map()) -> map().
normalize_state(Text) when is_binary(Text) ->
    #{<<"message">> => Text};
normalize_state(State) when is_map(State) ->
    binary_keys(State).

%% @doc questions 归一为 wire 结构（id / type / criteria 的键全部 binary）
-spec normalize_questions(map()) -> #{binary() => map()}.
normalize_questions(Questions) when is_map(Questions) ->
    maps:fold(fun(Id, Q, Acc) -> Acc#{to_bin(Id) => normalize_question(Q)} end, #{}, Questions).

normalize_question(#{type := Type, instructions := Instructions} = Q) ->
    Base = #{<<"type">> => to_bin(Type), <<"instructions">> => Instructions},
    case maps:get(criteria, Q, undefined) of
        undefined -> Base;
        Criteria -> Base#{<<"criteria">> => normalize_criteria(Type, Criteria)}
    end;
normalize_question(#{<<"type">> := _} = Wire) ->
    %% 已是 wire 格式，原样
    Wire.

%% choice 的 criteria 可给选项名列表（说明为 null）；score 的 criteria 恒为有序列表
normalize_criteria(choice, Names) when is_list(Names) ->
    maps:from_list([{to_bin(N), null} || N <- Names]);
normalize_criteria(choice, Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) -> Acc#{to_bin(K) => criterion_desc(V)} end, #{}, Map);
normalize_criteria(score, Levels) when is_list(Levels) ->
    [to_bin(L) || L <- Levels];
normalize_criteria(_, Other) ->
    Other.

criterion_desc(null) -> null;
criterion_desc(undefined) -> null;
criterion_desc(V) -> to_bin(V).

%% @doc 校验题目集合：非空、题型合法、choice / score 带正确形状的 criteria
%%
%% 请求发出前就把形状错误拦下，避免拿到上游 400 再猜是哪道题写错。
-spec validate_questions(map()) -> ok | {error, term()}.
validate_questions(Questions) when is_map(Questions), map_size(Questions) =:= 0 ->
    {error, {invalid_questions, empty}};
validate_questions(Questions) when is_map(Questions) ->
    Errors = maps:fold(fun(Id, Q, Acc) ->
        case validate_question(Q) of
            ok -> Acc;
            {error, Reason} -> [{to_bin(Id), Reason} | Acc]
        end
    end, [], Questions),
    case Errors of
        [] -> ok;
        [{Id, Reason} | _] -> {error, {invalid_question, Id, Reason}}
    end;
validate_questions(_) ->
    {error, {invalid_questions, not_a_map}}.

validate_question(#{<<"type">> := _}) ->
    %% wire 格式交给服务端校验
    ok;
validate_question(#{type := Type} = Q) when Type =:= noul; Type =:= choice; Type =:= score ->
    case maps:get(instructions, Q, undefined) of
        I when is_binary(I), I =/= <<>> -> validate_criteria(Type, maps:get(criteria, Q, undefined));
        _ -> {error, missing_instructions}
    end;
validate_question(#{type := Type}) ->
    {error, {unknown_type, Type}};
validate_question(_) ->
    {error, missing_type}.

validate_criteria(noul, _) -> ok;
validate_criteria(choice, Map) when is_map(Map), map_size(Map) >= 2 -> ok;
validate_criteria(choice, List) when is_list(List), length(List) >= 2 -> ok;
validate_criteria(choice, _) -> {error, choice_needs_two_or_more_criteria};
validate_criteria(score, List) when is_list(List), length(List) >= 2 -> ok;
validate_criteria(score, _) -> {error, score_needs_ordered_levels}.

%%====================================================================
%% 响应解析
%%====================================================================

%% @doc System One 响应解析器（可直接传给 beamai_llm_http_client:request/5）
-spec parser(atom()) -> fun((map()) -> {ok, response()} | {error, term()}).
parser(Provider) ->
    fun(Raw) -> from_systemone(Raw, Provider) end.

%% @doc 解析 System One 响应
-spec from_systemone(map(), atom()) -> {ok, response()} | {error, term()}.
from_systemone(#{<<"answers">> := Answers} = Raw, Provider) when is_map(Answers) ->
    Usage = case maps:get(<<"usage">>, Raw, #{}) of U when is_map(U) -> U; _ -> #{} end,
    InputTokens = int_or_zero(maps:get(<<"input_tokens">>, Usage, 0)),
    OutputTokens = int_or_zero(maps:get(<<"output_tokens">>, Usage, 0)),
    {ok, new_response(#{
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => Provider,
        answers => maps:map(fun(_, A) -> normalize_answer(A) end, Answers),
        usage => #{input_tokens => InputTokens, output_tokens => OutputTokens,
                   total_tokens => int_or_zero(maps:get(<<"total_tokens">>, Usage, InputTokens + OutputTokens))},
        raw => Raw,
        metadata => #{request_id => maps:get(<<"request_id">>, Raw, undefined),
                      latency_ms => maps:get(<<"latency_ms">>, Raw, undefined)}
    })};
from_systemone(#{<<"error">> := Error}, _Provider) when is_map(Error) ->
    {error, {api_error, Error}};
from_systemone(#{<<"code">> := Code, <<"message">> := Message}, _Provider) ->
    %% 百炼风格的顶层错误
    {error, {api_error, #{code => Code, message => Message}}};
from_systemone(Raw, _Provider) ->
    {error, {invalid_response, Raw}}.

%% @doc 单道题回答 → 统一格式
%%
%% choice 的 confidence：服务端给了用它，否则取选中项概率，再否则 1.0。
-spec normalize_answer(map()) -> answer().
normalize_answer(#{<<"choice">> := Choice} = A) when is_binary(Choice) ->
    Probs = probabilities(A),
    Confidence = case maps:get(<<"confidence">>, A, undefined) of
        C when is_number(C) -> float(C);
        _ -> maps:get(Choice, Probs, 1.0)
    end,
    #{type => choice, choice => Choice, confidence => Confidence, probabilities => Probs};
normalize_answer(#{<<"noul">> := P}) when is_number(P) ->
    #{type => noul, probability => float(P)};
normalize_answer(#{<<"score">> := S} = A) when is_number(S) ->
    Legend = case maps:get(<<"legend">>, A, #{}) of
        L when is_map(L) -> maps:filter(fun(_, V) -> is_binary(V) end, L);
        _ -> #{}
    end,
    Confidence = case maps:get(<<"confidence">>, A, undefined) of
        C when is_number(C) -> float(C);
        _ -> undefined
    end,
    #{type => score, score => float(S), confidence => Confidence,
      probabilities => probabilities(A), legend => Legend};
normalize_answer(Other) ->
    #{type => unknown, raw => Other}.

probabilities(A) ->
    case maps:get(<<"probabilities">>, A, #{}) of
        M when is_map(M) -> maps:map(fun(_, V) -> float_or_zero(V) end, M);
        _ -> #{}
    end.

%% @doc 构造统一决策响应（补齐缺省字段）
-spec new_response(map()) -> response().
new_response(Fields) ->
    #{
        model => maps:get(model, Fields, <<>>),
        provider => maps:get(provider, Fields, unknown),
        answers => maps:get(answers, Fields, #{}),
        usage => maps:get(usage, Fields, #{input_tokens => 0, output_tokens => 0, total_tokens => 0}),
        raw => maps:get(raw, Fields, #{}),
        metadata => maps:get(metadata, Fields, #{})
    }.

%%====================================================================
%% 内部函数
%%====================================================================

binary_keys(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) -> Acc#{to_bin(K) => V} end, #{}, Map).

to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(B) when is_binary(B) -> B;
to_bin(Other) -> beamai_utils:to_binary(Other).

int_or_zero(N) when is_integer(N) -> N;
int_or_zero(_) -> 0.

float_or_zero(N) when is_number(N) -> float(N);
float_or_zero(_) -> 0.0.
