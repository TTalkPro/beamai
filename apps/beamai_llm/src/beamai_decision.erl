%%%-------------------------------------------------------------------
%%% @doc 结构化决策（Decision）统一入口 —— TypeSafe System One 协议
%%%
%%% 与 beamai_chat_model（对话）、beamai_embedding（向量化）、beamai_rerank
%%% （重排序）对等的门面：
%%%   - 多 Provider 路由（typesafe / dashscope / systemone / mock）
%%%   - 题目校验与归一（发请求前拦下形状错误）
%%%   - 失败重试（复用 beamai_llm_retry）
%%%   - 决策支持便捷接口：classify（意图分类）/ judge（是否判断）/ rate（等级打分）
%%%
%%% 决策模型不生成文本：给它一段 `state'（待判断的消息及上下文）和几道带 id 的题，
%%% 几十毫秒内返回每道题**带概率**的结构化回答。典型用途是 agent 的前置路由
%%% （这轮该走哪条规则 / 该用多强的模型）、HITL 判定（是否需要人工审批）、
%%% 内容分级等——用得起、答得快、结果可直接比阈值。
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 经 LiteGate 网关问百炼的 decision-model-preview
%%% Config = beamai_decision:create(systemone, #{
%%%     base_url => <<"http://127.0.0.1:4000/v1">>,
%%%     api_key => <<"lgk-...">>,
%%%     model => <<"decision-model-preview">>
%%% }),
%%%
%%% %% 一次问三道题
%%% Questions = #{
%%%     department => beamai_decision:choice_question(
%%%         <<"该由哪个部门处理？"/utf8>>,
%%%         #{billing => <<"付款、账单、退款"/utf8>>, tech => <<"故障、报错"/utf8>>}),
%%%     escalate => beamai_decision:noul_question(<<"是否需要转人工？"/utf8>>),
%%%     severity => beamai_decision:score_question(
%%%         <<"问题多严重？"/utf8>>, [<<"轻微"/utf8>>, <<"一般"/utf8>>, <<"紧急"/utf8>>])
%%% },
%%% {ok, Resp} = beamai_decision:decide(Config, <<"账单扣了两次，要退款！"/utf8>>, Questions),
%%% {<<"billing">>, 0.97} = beamai_decision:choice(Resp, department),
%%% 0.81 = beamai_decision:probability(Resp, escalate),
%%% 1.9 = beamai_decision:score(Resp, severity),
%%%
%%% %% 便捷接口：意图分类（自动加「都不是」选项，按阈值兜底为 none）
%%% {ok, #{intent := Intent}} = beamai_decision:classify(Config, Text,
%%%     [<<"写测试"/utf8>>, <<"改界面"/utf8>>, <<"查资料"/utf8>>], #{min_confidence => 0.6}).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_decision).

%% 配置 API
-export([create/2, validate/1, provider_module/1]).

%% 决策 API
-export([decide/3, decide/4]).

%% 决策支持便捷接口
-export([classify/3, classify/4]).
-export([judge/3, judge/4]).
-export([rate/4, rate/5]).

%% 题目构造
-export([choice_question/2, noul_question/1, score_question/2]).

%% 回答访问
-export([answers/1, answer/2, choice/2, confidence/2, probability/2, score/2,
         probabilities/2, legend/2]).

-export_type([config/0, provider/0, question/0, questions/0, answer/0, response/0]).

-type provider() :: typesafe | dashscope | systemone | mock | {custom, module()}.
-type question() :: beamai_decision_provider_behaviour:question().
-type questions() :: beamai_decision_provider_behaviour:questions().
-type answer() :: beamai_decision_provider_behaviour:answer().
-type response() :: beamai_decision_provider_behaviour:response().
-type config() :: #{
    provider := provider(),
    '__decision_config__' := true,
    atom() => term()
}.

%% classify 的「都不是」选项 id（ASCII，不与业务意图名撞）
-define(NONE_ID, <<"none">>).
%% classify 内部用的意图 id 前缀：选项名用 ASCII 的 iN，回答只看 id 与概率，
%% 不依赖模型回显中文 / 带空格的意图名
-define(INTENT_ID_PREFIX, "i").

-define(DEFAULT_CLASSIFY_INSTRUCTIONS,
        <<"Which kind of request is the `message`? Pick the criterion that fits it best.">>).
-define(DEFAULT_STATE_KEY, <<"message">>).

%%====================================================================
%% 配置 API
%%====================================================================

%% @doc 创建决策配置（合并 Provider 默认配置与用户配置）
-spec create(provider(), map()) -> config().
create(Provider, Opts) ->
    Module = provider_module(Provider),
    Base = #{provider => Provider, '__decision_config__' => true},
    maps:merge(maps:merge(Module:default_config(), Base), Opts).

%% @doc 校验配置（委托给 Provider）
-spec validate(config()) -> ok | {error, term()}.
validate(Config) ->
    Module = provider_module(maps:get(provider, Config, systemone)),
    Module:validate_config(Config).

%% @doc Provider 路由
-spec provider_module(provider()) -> module().
provider_module(typesafe) -> beamai_decision_provider_typesafe;
provider_module(dashscope) -> beamai_decision_provider_dashscope;
provider_module(systemone) -> beamai_decision_provider_systemone;
provider_module(mock) -> beamai_decision_provider_mock;
provider_module({custom, Module}) -> Module.

%%====================================================================
%% 决策 API
%%====================================================================

%% @doc 对 State 问一组题
%%
%% State 为纯文本时作为 `message' 字段；为 map 时原样作为 state（键转 binary），
%% 可携带 `previous_message_kind' 之类的上下文字段供题目引用。
-spec decide(config(), binary() | map(), questions()) -> {ok, response()} | {error, term()}.
decide(Config, State, Questions) ->
    decide(Config, State, Questions, #{}).

%% @doc 对 State 问一组题（带选项）
%%
%% 选项：
%%   extra                         - 合入请求体顶层的附加字段（provider 特有参数）
%%   max_retries / retry_delay / on_retry - 重试策略（同 beamai_chat_model）
-spec decide(config(), binary() | map(), questions(), map()) -> {ok, response()} | {error, term()}.
decide(Config, State, Questions, Opts) when is_map(Questions) ->
    case beamai_decision_common:validate_questions(Questions) of
        ok ->
            Module = provider_module(maps:get(provider, Config, systemone)),
            Request0 = #{
                state => beamai_decision_common:normalize_state(State),
                questions => beamai_decision_common:normalize_questions(Questions)
            },
            Request = case maps:get(extra, Opts, undefined) of
                Extra when is_map(Extra) -> Request0#{extra => Extra};
                _ -> Request0
            end,
            RetryOpts = beamai_llm_retry:opts(Config, Opts),
            beamai_llm_retry:run(fun() -> Module:decide(Config, Request) end, RetryOpts);
        Error ->
            Error
    end;
decide(_Config, _State, _Questions, _Opts) ->
    {error, {invalid_questions, not_a_map}}.

%%====================================================================
%% 决策支持便捷接口
%%====================================================================

%% @doc 意图分类：Text 属于 Intents 中的哪一类
-spec classify(config(), binary() | map(), [binary()] | #{binary() => binary()}) ->
    {ok, #{intent := binary() | none, confidence := float(), probabilities := #{binary() | none => float()},
           response := response()}} | {error, term()}.
classify(Config, Text, Intents) ->
    classify(Config, Text, Intents, #{}).

%% @doc 意图分类（带选项）
%%
%% Intents 为意图名列表，或 意图名 → 说明 的 map（说明会随选项一起给模型）。
%% 自动追加「都不是」选项；选中它、或选中项置信度低于 `min_confidence'（默认 0.0）
%% 时 intent 为 `none'。
%%
%% 选项：
%%   instructions   - 题干（默认英文通用题干；可换成业务语言）
%%   min_confidence - 置信度阈值
%%   none           - false 时不加「都不是」选项（每条消息必属某类，如等级判断）
%%   none_text      - 「都不是」选项的说明
%%   其余选项透传给 decide/4
-spec classify(config(), binary() | map(), [binary()] | #{binary() => binary()}, map()) ->
    {ok, #{intent := binary() | none, confidence := float(), probabilities := #{binary() | none => float()},
           response := response()}} | {error, term()}.
classify(Config, Text, Intents, Opts) ->
    Pairs = intent_pairs(Intents),
    case Pairs of
        [] ->
            {error, {invalid_questions, no_intents}};
        _ ->
            Ids = [{intent_id(N), Name, Desc} || {N, {Name, Desc}} <- lists:enumerate(Pairs)],
            Criteria0 = maps:from_list([{Id, criterion_text(Name, Desc)} || {Id, Name, Desc} <- Ids]),
            WithNone = maps:get(none, Opts, true) =/= false,
            Criteria = case WithNone of
                true -> Criteria0#{?NONE_ID => maps:get(none_text, Opts, <<"None of the other kinds">>)};
                false -> Criteria0
            end,
            Instructions = maps:get(instructions, Opts, ?DEFAULT_CLASSIFY_INSTRUCTIONS),
            Question = choice_question(Instructions, Criteria),
            case decide(Config, Text, #{intent => Question}, decide_opts(Opts)) of
                {ok, Resp} ->
                    {ok, classify_result(Resp, Ids, maps:get(min_confidence, Opts, 0.0))};
                Error ->
                    Error
            end
    end.

%% @doc 是 / 否判断：Text 是否满足 Instructions 描述的条件，返回「是」的概率
-spec judge(config(), binary() | map(), binary()) -> {ok, float()} | {error, term()}.
judge(Config, Text, Instructions) ->
    judge(Config, Text, Instructions, #{}).

-spec judge(config(), binary() | map(), binary(), map()) -> {ok, float()} | {error, term()}.
judge(Config, Text, Instructions, Opts) ->
    case decide(Config, Text, #{verdict => noul_question(Instructions)}, decide_opts(Opts)) of
        {ok, Resp} ->
            case answer(Resp, verdict) of
                #{type := noul, probability := P} -> {ok, P};
                Other -> {error, {unexpected_answer, Other}}
            end;
        Error ->
            Error
    end.

%% @doc 等级打分：按 Levels（由低到高的等级说明）给 Text 打分
%%
%% 返回 `score'（0 ~ N-1 的连续值）、四舍五入落到的 `level'（等级说明原文）、
%% `confidence' 与各等级概率（键为等级说明原文）。
-spec rate(config(), binary() | map(), binary(), [binary()]) ->
    {ok, #{score := float(), level := binary(), confidence := float() | undefined,
           probabilities := #{binary() => float()}, response := response()}} | {error, term()}.
rate(Config, Text, Instructions, Levels) ->
    rate(Config, Text, Instructions, Levels, #{}).

-spec rate(config(), binary() | map(), binary(), [binary()], map()) ->
    {ok, #{score := float(), level := binary(), confidence := float() | undefined,
           probabilities := #{binary() => float()}, response := response()}} | {error, term()}.
rate(Config, Text, Instructions, Levels, Opts) when is_list(Levels), length(Levels) >= 2 ->
    case decide(Config, Text, #{level => score_question(Instructions, Levels)}, decide_opts(Opts)) of
        {ok, Resp} ->
            case answer(Resp, level) of
                #{type := score, score := S, confidence := C, probabilities := Probs} ->
                    Index = max(0, min(round(S), length(Levels) - 1)),
                    {ok, #{score => S,
                           level => lists:nth(Index + 1, Levels),
                           confidence => C,
                           probabilities => level_probabilities(Probs, Levels),
                           response => Resp}};
                Other ->
                    {error, {unexpected_answer, Other}}
            end;
        Error ->
            Error
    end;
rate(_Config, _Text, _Instructions, _Levels, _Opts) ->
    {error, {invalid_question, <<"level">>, score_needs_ordered_levels}}.

%%====================================================================
%% 题目构造
%%====================================================================

%% @doc 多选一题：Criteria 为 选项名 → 说明（可为 null）的 map，或选项名列表
-spec choice_question(binary(), #{atom() | binary() => binary() | null} | [binary()]) -> question().
choice_question(Instructions, Criteria) ->
    #{type => choice, instructions => Instructions, criteria => Criteria}.

%% @doc 是 / 否题
-spec noul_question(binary()) -> question().
noul_question(Instructions) ->
    #{type => noul, instructions => Instructions}.

%% @doc 等级打分题：Levels 为由低到高的等级说明列表
-spec score_question(binary(), [binary()]) -> question().
score_question(Instructions, Levels) ->
    #{type => score, instructions => Instructions, criteria => Levels}.

%%====================================================================
%% 回答访问
%%====================================================================

%% @doc 全部回答（题目 id → 统一格式回答）
-spec answers(response()) -> #{binary() => answer()}.
answers(#{answers := Answers}) -> Answers.

%% @doc 某道题的回答（不存在返回 undefined）
-spec answer(response(), atom() | binary()) -> answer() | undefined.
answer(#{answers := Answers}, Id) ->
    maps:get(to_bin(Id), Answers, undefined).

%% @doc choice 题：{选中项, 置信度}
-spec choice(response(), atom() | binary()) -> {binary(), float()} | undefined.
choice(Resp, Id) ->
    case answer(Resp, Id) of
        #{type := choice, choice := C, confidence := Conf} -> {C, Conf};
        _ -> undefined
    end.

%% @doc choice / score 题的置信度
-spec confidence(response(), atom() | binary()) -> float() | undefined.
confidence(Resp, Id) ->
    case answer(Resp, Id) of
        #{confidence := C} -> C;
        _ -> undefined
    end.

%% @doc noul 题：「是」的概率
-spec probability(response(), atom() | binary()) -> float() | undefined.
probability(Resp, Id) ->
    case answer(Resp, Id) of
        #{type := noul, probability := P} -> P;
        _ -> undefined
    end.

%% @doc score 题：分值
-spec score(response(), atom() | binary()) -> float() | undefined.
score(Resp, Id) ->
    case answer(Resp, Id) of
        #{type := score, score := S} -> S;
        _ -> undefined
    end.

%% @doc choice / score 题的各选项概率
-spec probabilities(response(), atom() | binary()) -> #{binary() => float()}.
probabilities(Resp, Id) ->
    case answer(Resp, Id) of
        #{probabilities := P} -> P;
        _ -> #{}
    end.

%% @doc score 题的等级说明表（"0" → 最低等级说明 …）
-spec legend(response(), atom() | binary()) -> #{binary() => binary()}.
legend(Resp, Id) ->
    case answer(Resp, Id) of
        #{type := score, legend := L} -> L;
        _ -> #{}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 只把 decide 认识的选项透传
decide_opts(Opts) ->
    maps:with([extra, max_retries, retry_delay, on_retry], Opts).

intent_pairs(Intents) when is_list(Intents) ->
    [{to_bin(I), undefined} || I <- Intents];
intent_pairs(Intents) when is_map(Intents) ->
    %% map 无序，按名排序使 iN 编号稳定
    lists:sort([{to_bin(Name), Desc} || {Name, Desc} <- maps:to_list(Intents)]).

intent_id(N) -> iolist_to_binary([?INTENT_ID_PREFIX, integer_to_binary(N)]).

criterion_text(Name, undefined) -> Name;
criterion_text(Name, null) -> Name;
criterion_text(Name, Desc) -> <<Name/binary, ": ", (to_bin(Desc))/binary>>.

classify_result(Resp, Ids, MinConfidence) ->
    IdToName = maps:from_list([{Id, Name} || {Id, Name, _} <- Ids]),
    Probs = maps:fold(fun(Id, P, Acc) ->
        case Id of
            ?NONE_ID -> Acc#{none => P};
            _ -> case maps:get(Id, IdToName, undefined) of
                     undefined -> Acc;
                     Name -> Acc#{Name => P}
                 end
        end
    end, #{}, probabilities(Resp, intent)),
    {Intent, Confidence} = case choice(Resp, intent) of
        {?NONE_ID, Conf} -> {none, Conf};
        {Id, Conf} when Conf >= MinConfidence ->
            {maps:get(Id, IdToName, none), Conf};
        {_, Conf} -> {none, Conf};
        undefined -> {none, 0.0}
    end,
    #{intent => Intent, confidence => Confidence, probabilities => Probs, response => Resp}.

%% score 的 probabilities 键是 "0".."N-1"，映射回等级说明
level_probabilities(Probs, Levels) ->
    Indexed = lists:zip(lists:seq(0, length(Levels) - 1), Levels),
    maps:from_list([{L, maps:get(integer_to_binary(I), Probs, 0.0)} || {I, L} <- Indexed]).

to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(B) when is_binary(B) -> B.
