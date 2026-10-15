%%%-------------------------------------------------------------------
%%% @doc 决策（System One）子系统单元测试
%%%
%%% 覆盖：
%%%   - 门面 API（create / validate / decide / 回答访问器）
%%%   - 题目归一与校验（发请求前拦下形状错误）
%%%   - 请求体与响应解析（三种题型、confidence 兜底、错误形状）
%%%   - 决策支持接口 classify / judge / rate（经 mock provider 固定回答）
%%%   - HTTP provider 经 beamai_llm_fake_backend 端到端
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_decision_tests).

-include_lib("eunit/include/eunit.hrl").

%% 假 Provider 回调（供 {custom, ?MODULE} 使用）
-export([name/0, default_config/0, validate_config/1, decide/2]).

-define(TEXT, <<"账单扣了两次，要退款"/utf8>>).

questions() ->
    #{department => beamai_decision:choice_question(<<"哪个部门？"/utf8>>,
                        #{billing => <<"退款"/utf8>>, tech => null}),
      escalate => beamai_decision:noul_question(<<"转人工？"/utf8>>),
      severity => beamai_decision:score_question(<<"多严重？"/utf8>>, [<<"low">>, <<"mid">>, <<"high">>])}.

%%====================================================================
%% 配置
%%====================================================================

create_and_validate_test() ->
    T = beamai_decision:create(typesafe, #{}),
    ?assertEqual(<<"jev-latest">>, maps:get(model, T)),
    ?assertEqual({error, missing_api_key}, beamai_decision:validate(T)),
    ?assertEqual(ok, beamai_decision:validate(T#{api_key => <<"k">>})),
    D = beamai_decision:create(dashscope, #{api_key => <<"k">>}),
    ?assertEqual(<<"decision-model-preview">>, maps:get(model, D)),
    ?assertEqual(<<"https://token-plan.cn-beijing.maas.aliyuncs.com/compatible-mode/v1">>, maps:get(base_url, D)),
    ?assertEqual(<<"https://ws-1.cn-beijing.maas.aliyuncs.com/compatible-mode/v1">>,
                 beamai_decision_provider_dashscope:workspace_base_url(<<"ws-1">>)),
    S = beamai_decision:create(systemone, #{}),
    ?assertEqual({error, missing_base_url}, beamai_decision:validate(S)),
    ?assertEqual({error, missing_model}, beamai_decision:validate(S#{base_url => <<"http://h/v1">>})),
    ?assertEqual(ok, beamai_decision:validate(S#{base_url => <<"http://h/v1">>, model => <<"m">>})),
    ?assertEqual(ok, beamai_decision:validate(beamai_decision:create(mock, #{}))).

%%====================================================================
%% 题目归一与校验
%%====================================================================

normalize_questions_test() ->
    Wire = beamai_decision_common:normalize_questions(questions()),
    ?assertEqual([<<"department">>, <<"escalate">>, <<"severity">>], lists:sort(maps:keys(Wire))),
    ?assertEqual(#{<<"type">> => <<"choice">>, <<"instructions">> => <<"哪个部门？"/utf8>>,
                   <<"criteria">> => #{<<"billing">> => <<"退款"/utf8>>, <<"tech">> => null}},
                 maps:get(<<"department">>, Wire)),
    ?assertEqual(#{<<"type">> => <<"noul">>, <<"instructions">> => <<"转人工？"/utf8>>},
                 maps:get(<<"escalate">>, Wire)),
    ?assertEqual([<<"low">>, <<"mid">>, <<"high">>], maps:get(<<"criteria">>, maps:get(<<"severity">>, Wire))),
    %% choice 的 criteria 给名字列表 → 说明为 null 的 map
    #{<<"q">> := #{<<"criteria">> := C}} =
        beamai_decision_common:normalize_questions(#{q => beamai_decision:choice_question(<<"?">>, [a, <<"b">>])}),
    ?assertEqual(#{<<"a">> => null, <<"b">> => null}, C).

normalize_state_test() ->
    ?assertEqual(#{<<"message">> => ?TEXT}, beamai_decision_common:normalize_state(?TEXT)),
    ?assertEqual(#{<<"message">> => ?TEXT, <<"previous_message_kind">> => <<"billing">>},
                 beamai_decision_common:normalize_state(#{message => ?TEXT, <<"previous_message_kind">> => <<"billing">>})).

validate_questions_test() ->
    ?assertEqual(ok, beamai_decision_common:validate_questions(questions())),
    ?assertEqual({error, {invalid_questions, empty}}, beamai_decision_common:validate_questions(#{})),
    ?assertEqual({error, {invalid_question, <<"q">>, {unknown_type, rank}}},
                 beamai_decision_common:validate_questions(#{q => #{type => rank, instructions => <<"x">>}})),
    ?assertEqual({error, {invalid_question, <<"q">>, missing_instructions}},
                 beamai_decision_common:validate_questions(#{q => #{type => noul}})),
    ?assertEqual({error, {invalid_question, <<"q">>, choice_needs_two_or_more_criteria}},
                 beamai_decision_common:validate_questions(#{q => beamai_decision:choice_question(<<"x">>, [<<"one">>])})),
    ?assertEqual({error, {invalid_question, <<"q">>, score_needs_ordered_levels}},
                 beamai_decision_common:validate_questions(#{q => #{type => score, instructions => <<"x">>, criteria => #{}}})),
    %% 校验失败不会调 provider
    ?assertEqual({error, {invalid_questions, empty}}, beamai_decision:decide(beamai_decision:create(mock, #{}), ?TEXT, #{})).

%%====================================================================
%% 请求体与响应解析
%%====================================================================

build_body_test() ->
    Request = #{state => #{<<"message">> => ?TEXT},
                questions => beamai_decision_common:normalize_questions(questions()),
                extra => #{temperature => 0}},
    Body = beamai_decision_common:build_body(#{}, Request, <<"jev-latest">>),
    ?assertEqual(<<"jev-latest">>, maps:get(<<"model">>, Body)),
    ?assertEqual(#{<<"message">> => ?TEXT}, maps:get(<<"state">>, Body)),
    ?assertEqual(3, map_size(maps:get(<<"questions">>, Body))),
    ?assertEqual(0, maps:get(<<"temperature">>, Body)),
    ?assertEqual(<<"m">>, maps:get(<<"model">>, beamai_decision_common:build_body(#{model => <<"m">>}, Request, <<"x">>))),
    ?assertEqual([{<<"Content-Type">>, <<"application/json">>}], beamai_decision_common:build_headers(#{})),
    ?assert(lists:keymember(<<"Authorization">>, 1, beamai_decision_common:build_headers(#{api_key => <<"k">>}))).

raw_response() ->
    #{<<"answers">> => #{
          <<"department">> => #{<<"type">> => <<"choice">>, <<"choice">> => <<"billing">>, <<"confidence">> => 0.97,
                                <<"probabilities">> => #{<<"billing">> => 0.97, <<"tech">> => 0.03}},
          <<"escalate">> => #{<<"type">> => <<"noul">>, <<"noul">> => 0.81},
          <<"severity">> => #{<<"type">> => <<"score">>, <<"score">> => 1.6, <<"confidence">> => 0.9,
                              <<"probabilities">> => #{<<"0">> => 0.1, <<"1">> => 0.2, <<"2">> => 0.7},
                              <<"legend">> => #{<<"0">> => <<"low">>, <<"1">> => <<"mid">>, <<"2">> => <<"high">>}}},
      <<"model">> => <<"decision-model-preview">>, <<"request_id">> => <<"r1">>, <<"latency_ms">> => 50.5,
      <<"usage">> => #{<<"input_tokens">> => 100}}.

parse_response_test() ->
    {ok, Resp} = beamai_decision_common:from_systemone(raw_response(), systemone),
    ?assertEqual(systemone, maps:get(provider, Resp)),
    ?assertEqual(<<"decision-model-preview">>, maps:get(model, Resp)),
    ?assertEqual(#{input_tokens => 100, output_tokens => 0, total_tokens => 100}, maps:get(usage, Resp)),
    ?assertEqual(#{request_id => <<"r1">>, latency_ms => 50.5}, maps:get(metadata, Resp)),
    ?assertEqual({<<"billing">>, 0.97}, beamai_decision:choice(Resp, department)),
    ?assertEqual(0.97, beamai_decision:confidence(Resp, <<"department">>)),
    ?assertEqual(#{<<"billing">> => 0.97, <<"tech">> => 0.03}, beamai_decision:probabilities(Resp, department)),
    ?assertEqual(0.81, beamai_decision:probability(Resp, escalate)),
    ?assertEqual(1.6, beamai_decision:score(Resp, severity)),
    ?assertEqual(#{<<"0">> => <<"low">>, <<"1">> => <<"mid">>, <<"2">> => <<"high">>}, beamai_decision:legend(Resp, severity)),
    ?assertEqual(undefined, beamai_decision:answer(Resp, nope)),
    ?assertEqual(undefined, beamai_decision:choice(Resp, escalate)),
    ?assertEqual(3, map_size(beamai_decision:answers(Resp))).

normalize_answer_fallbacks_test() ->
    %% 没有 confidence：取选中项概率；再没有：1.0
    ?assertMatch(#{type := choice, confidence := 0.8},
                 beamai_decision_common:normalize_answer(#{<<"choice">> => <<"a">>, <<"probabilities">> => #{<<"a">> => 0.8}})),
    ?assertMatch(#{type := choice, confidence := 1.0, probabilities := #{}},
                 beamai_decision_common:normalize_answer(#{<<"choice">> => <<"a">>})),
    ?assertMatch(#{type := score, confidence := undefined, legend := #{}},
                 beamai_decision_common:normalize_answer(#{<<"score">> => 2})),
    ?assertMatch(#{type := noul, probability := 1.0}, beamai_decision_common:normalize_answer(#{<<"noul">> => 1})),
    ?assertMatch(#{type := unknown}, beamai_decision_common:normalize_answer(#{<<"weird">> => 1})).

parse_errors_test() ->
    ?assertEqual({error, {api_error, #{<<"message">> => <<"no key">>}}},
                 beamai_decision_common:from_systemone(#{<<"error">> => #{<<"message">> => <<"no key">>}}, typesafe)),
    ?assertEqual({error, {api_error, #{code => <<"InvalidParameter">>, message => <<"bad">>}}},
                 beamai_decision_common:from_systemone(#{<<"code">> => <<"InvalidParameter">>, <<"message">> => <<"bad">>}, dashscope)),
    ?assertMatch({error, {invalid_response, _}}, beamai_decision_common:from_systemone(#{<<"foo">> => 1}, mock)).

%%====================================================================
%% 门面（mock provider）
%%====================================================================

mock_decide_test() ->
    Config = beamai_decision:create(mock, #{}),
    {ok, Resp} = beamai_decision:decide(Config, ?TEXT, questions()),
    ?assertEqual(mock, maps:get(provider, Resp)),
    ?assertEqual({<<"billing">>, 1.0}, beamai_decision:choice(Resp, department)),
    ?assertEqual(0.5, beamai_decision:probability(Resp, escalate)),
    ?assertEqual(0.0, beamai_decision:score(Resp, severity)),
    ?assertEqual(#{<<"0">> => <<"low">>, <<"1">> => <<"mid">>, <<"2">> => <<"high">>}, beamai_decision:legend(Resp, severity)).

mock_overrides_and_failure_test() ->
    Config = beamai_decision:create(mock, #{answers => #{<<"escalate">> => #{<<"noul">> => 0.99}}}),
    {ok, Resp} = beamai_decision:decide(Config, ?TEXT, questions()),
    ?assertEqual(0.99, beamai_decision:probability(Resp, escalate)),
    Failing = beamai_decision:create(mock, #{fail => {http_error, 401, <<"nope">>}}),
    ?assertEqual({error, {http_error, 401, <<"nope">>}},
                 beamai_decision:decide(Failing, ?TEXT, questions(), #{max_retries => 0})).

classify_test() ->
    Intents = [<<"写测试"/utf8>>, <<"改界面"/utf8>>, <<"查资料"/utf8>>],
    Pick = fun(Id, Conf, Probs) ->
        beamai_decision:create(mock, #{answers => #{<<"intent">> => #{<<"choice">> => Id, <<"confidence">> => Conf,
                                                                       <<"probabilities">> => Probs}}})
    end,
    Probs = #{<<"i1">> => 0.1, <<"i2">> => 0.85, <<"i3">> => 0.0, <<"none">> => 0.05},
    {ok, R1} = beamai_decision:classify(Pick(<<"i2">>, 0.85, Probs), ?TEXT, Intents),
    ?assertEqual(<<"改界面"/utf8>>, maps:get(intent, R1)),
    ?assertEqual(0.85, maps:get(confidence, R1)),
    ?assertEqual(#{<<"写测试"/utf8>> => 0.1, <<"改界面"/utf8>> => 0.85, <<"查资料"/utf8>> => 0.0, none => 0.05},
                 maps:get(probabilities, R1)),
    %% 低于阈值 / 选中「都不是」→ none
    {ok, R2} = beamai_decision:classify(Pick(<<"i2">>, 0.55, Probs), ?TEXT, Intents, #{min_confidence => 0.7}),
    ?assertEqual(none, maps:get(intent, R2)),
    {ok, R3} = beamai_decision:classify(Pick(<<"none">>, 0.9, #{}), ?TEXT, Intents),
    ?assertEqual(none, maps:get(intent, R3)),
    %% map 形态的意图带说明；题目里的 criteria 是 iN → "名: 说明"，默认附带 none
    {ok, R4} = beamai_decision:classify(Pick(<<"i1">>, 0.9, #{}), ?TEXT, #{<<"test">> => <<"写测试"/utf8>>, <<"ui">> => <<"改界面"/utf8>>}),
    ?assertEqual(<<"test">>, maps:get(intent, R4)),
    Raw = maps:get(raw, maps:get(response, R4)),
    ?assertEqual(#{<<"mock">> => true}, Raw),
    ?assertEqual({error, {invalid_questions, no_intents}}, beamai_decision:classify(Pick(<<"i1">>, 1.0, #{}), ?TEXT, [])).

classify_question_shape_test() ->
    %% 用自定义 provider 捕获真正发出的题目
    Config = beamai_decision:create({custom, ?MODULE}, #{}),
    put(captured, undefined),
    {ok, _} = beamai_decision:classify(Config, ?TEXT, #{<<"ui">> => <<"改界面"/utf8>>, <<"test">> => <<"写测试"/utf8>>},
                                       #{instructions => <<"Which?">>, none_text => <<"nothing">>}),
    #{questions := #{<<"intent">> := Q}} = get(captured),
    ?assertEqual(<<"Which?">>, maps:get(<<"instructions">>, Q)),
    %% map 按名排序编号：test → i1，ui → i2
    ?assertEqual(#{<<"i1">> => <<"test: 写测试"/utf8>>, <<"i2">> => <<"ui: 改界面"/utf8>>, <<"none">> => <<"nothing">>},
                 maps:get(<<"criteria">>, Q)),
    {ok, _} = beamai_decision:classify(Config, ?TEXT, [<<"a">>, <<"b">>], #{none => false}),
    #{questions := #{<<"intent">> := Q2}} = get(captured),
    ?assertEqual(#{<<"i1">> => <<"a">>, <<"i2">> => <<"b">>}, maps:get(<<"criteria">>, Q2)).

judge_test() ->
    Config = beamai_decision:create(mock, #{answers => #{<<"verdict">> => #{<<"noul">> => 0.93}}}),
    ?assertEqual({ok, 0.93}, beamai_decision:judge(Config, ?TEXT, <<"危险？"/utf8>>)),
    ?assertMatch({error, {unexpected_answer, _}},
                 beamai_decision:judge(beamai_decision:create(mock, #{answers => #{<<"verdict">> => #{<<"score">> => 1}}}),
                                       ?TEXT, <<"?">>)).

rate_test() ->
    Levels = [<<"low">>, <<"medium">>, <<"high">>, <<"xhigh">>],
    Config = beamai_decision:create(mock, #{answers => #{<<"level">> => #{
        <<"score">> => 2.6, <<"confidence">> => 0.8,
        <<"probabilities">> => #{<<"0">> => 0.0, <<"1">> => 0.1, <<"2">> => 0.3, <<"3">> => 0.6}}}}),
    {ok, R} = beamai_decision:rate(Config, ?TEXT, <<"多少推理？"/utf8>>, Levels),
    ?assertEqual(2.6, maps:get(score, R)),
    ?assertEqual(<<"xhigh">>, maps:get(level, R)),
    ?assertEqual(0.8, maps:get(confidence, R)),
    ?assertEqual(#{<<"low">> => 0.0, <<"medium">> => 0.1, <<"high">> => 0.3, <<"xhigh">> => 0.6}, maps:get(probabilities, R)),
    %% 越界分值夹到最高等级
    Config2 = beamai_decision:create(mock, #{answers => #{<<"level">> => #{<<"score">> => 9.0}}}),
    {ok, R2} = beamai_decision:rate(Config2, ?TEXT, <<"?">>, Levels),
    ?assertEqual(<<"xhigh">>, maps:get(level, R2)),
    ?assertMatch({error, {invalid_question, <<"level">>, _}}, beamai_decision:rate(Config, ?TEXT, <<"?">>, [<<"only">>])).

%%====================================================================
%% HTTP provider（fake backend）
%%====================================================================

http_fixture_test_() ->
    {setup,
     fun() -> beamai_llm_fake_backend:install() end,
     fun(Prev) -> beamai_llm_fake_backend:uninstall(Prev) end,
     fun(_) -> [?_test(systemone_http_round_trip()), ?_test(typesafe_http_round_trip())] end}.

systemone_http_round_trip() ->
    beamai_llm_fake_backend:set_response(raw_response(), []),
    Config = beamai_decision:create(systemone, #{base_url => <<"http://127.0.0.1:4000/v1">>,
                                                 api_key => <<"lgk">>, model => <<"decision-model-preview">>,
                                                 timeout => 1234}),
    {ok, Resp} = beamai_decision:decide(Config, ?TEXT, questions(), #{max_retries => 0}),
    ?assertEqual(systemone, maps:get(provider, Resp)),
    ?assertEqual({<<"billing">>, 0.97}, beamai_decision:choice(Resp, department)),
    ?assertEqual(1234, maps:get(timeout, beamai_llm_fake_backend:last_opts())),
    %% 缺 base_url 时不发请求
    ?assertEqual({error, missing_base_url},
                 beamai_decision:decide(beamai_decision:create(systemone, #{model => <<"m">>}), ?TEXT, questions(), #{max_retries => 0})).

typesafe_http_round_trip() ->
    beamai_llm_fake_backend:set_response(raw_response(), []),
    Config = beamai_decision:create(typesafe, #{api_key => <<"k">>}),
    {ok, Resp} = beamai_decision:decide(Config, ?TEXT, questions(), #{max_retries => 0}),
    ?assertEqual(typesafe, maps:get(provider, Resp)),
    ?assertEqual(0.81, beamai_decision:probability(Resp, escalate)).

%%====================================================================
%% 假 Provider（{custom, ?MODULE}）：记录请求并返回固定回答
%%====================================================================

name() -> <<"capture">>.
default_config() -> #{model => <<"cap">>}.
validate_config(_) -> ok.
decide(_Config, Request) ->
    put(captured, Request),
    {ok, beamai_decision_common:new_response(#{
        provider => custom,
        answers => #{<<"intent">> => #{type => choice, choice => <<"i1">>, confidence => 1.0, probabilities => #{}}}})}.
