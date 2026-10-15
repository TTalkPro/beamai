%%%-------------------------------------------------------------------
%%% @doc Mock 决策 Provider（测试用）
%%%
%%% 不发网络请求，按题型给出确定性回答：
%%%   - choice：选 criteria 中按字典序最小的选项，confidence 1.0
%%%   - noul：概率 0.5
%%%   - score：0.0（最低等级），legend 按 criteria 生成
%%%
%%% Config 的 `answers'（#{题目 id => wire 格式回答}）可逐题覆盖，便于在测试里
%%% 固定某道题的结果；`fail => Reason' 则一律返回 {error, Reason}。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_decision_provider_mock).
-behaviour(beamai_decision_provider_behaviour).

-export([name/0, default_config/0, validate_config/1, decide/2]).

-define(MODEL, <<"mock-decision">>).

name() -> <<"Mock Decision">>.

default_config() ->
    #{model => ?MODEL}.

validate_config(_Config) -> ok.

decide(#{fail := Reason}, _Request) ->
    {error, Reason};
decide(Config, #{questions := Questions}) ->
    Overrides = maps:get(answers, Config, #{}),
    Answers = maps:map(fun(Id, Q) ->
        Wire = case maps:get(Id, Overrides, undefined) of
            undefined -> mock_answer(Q);
            Given -> Given
        end,
        beamai_decision_common:normalize_answer(Wire)
    end, Questions),
    Tokens = byte_size(beamai_utils:encode_json(Questions)) div 4,
    {ok, beamai_decision_common:new_response(#{
        model => maps:get(model, Config, ?MODEL),
        provider => mock,
        answers => Answers,
        usage => #{input_tokens => Tokens, output_tokens => 0, total_tokens => Tokens},
        raw => #{<<"mock">> => true},
        metadata => #{request_id => <<"mock">>}
    })}.

mock_answer(#{<<"type">> := <<"choice">>, <<"criteria">> := Criteria}) when is_map(Criteria) ->
    Names = lists:sort(maps:keys(Criteria)),
    [First | _] = Names,
    Probs = maps:from_list([{N, case N of First -> 1.0; _ -> 0.0 end} || N <- Names]),
    #{<<"type">> => <<"choice">>, <<"choice">> => First, <<"confidence">> => 1.0,
      <<"probabilities">> => Probs};
mock_answer(#{<<"type">> := <<"noul">>}) ->
    #{<<"type">> => <<"noul">>, <<"noul">> => 0.5};
mock_answer(#{<<"type">> := <<"score">>, <<"criteria">> := Levels}) when is_list(Levels) ->
    Indexed = lists:zip(lists:seq(0, length(Levels) - 1), Levels),
    #{<<"type">> => <<"score">>, <<"score">> => 0.0, <<"confidence">> => 1.0,
      <<"probabilities">> => maps:from_list([{integer_to_binary(I), case I of 0 -> 1.0; _ -> 0.0 end}
                                             || {I, _} <- Indexed]),
      <<"legend">> => maps:from_list([{integer_to_binary(I), L} || {I, L} <- Indexed])};
mock_answer(_) ->
    #{<<"type">> => <<"unsupported">>}.
