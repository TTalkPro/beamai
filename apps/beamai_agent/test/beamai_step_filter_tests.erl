%%%-------------------------------------------------------------------
%%% @doc around_step 链 + 循环 filter 测试
%%%
%%% 锚点：循环是 turn 链上的一环（terminal 是 step 链）；around_step 每轮迭代一次
%%% 且**包住该轮的工具执行**（这正是 around_chat 做不到的）；step filter 可短路
%%% 一轮迭代；整个循环策略可被替换。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_step_filter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助
%%====================================================================

%% 第一轮要求调工具，第二轮给终答（两轮迭代）
mock_two_rounds() ->
    Ctr = counters:new(1, []),
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        case counters:get(Ctr, 1) of
            0 ->
                counters:add(Ctr, 1, 1),
                {ok, #{content => null, tool_calls => [tool_call()],
                       finish_reason => <<"tool_calls">>}};
            _ ->
                {ok, #{content => <<"done">>, tool_calls => [],
                       finish_reason => <<"stop">>}}
        end
    end).

%% 每轮都要求调工具，永不收尾（用于验证循环可被替换/刹住）
mock_looping() ->
    meck:new(beamai_chat_model, [passthrough]),
    meck:expect(beamai_chat_model, chat, fun(_C, _M, _O) ->
        {ok, #{content => null, tool_calls => [tool_call()],
               finish_reason => <<"tool_calls">>}}
    end).

tool_call() ->
    #{id => <<"call_1">>, type => <<"function">>,
      function => #{name => <<"plugin_tool">>, arguments => <<"{}">>}}.

kernel(Filters) ->
    K0 = beamai_kernel:new(#{}, Filters),
    K1 = beamai_kernel:add_tool_module(K0, beamai_agent_test_plugin),
    beamai_kernel:add_chat_model(K1, beamai_chat_model:create(mock, #{})).

agent(Filters) -> agent(Filters, #{}).
agent(Filters, Extra) ->
    beamai_agent:new(maps:merge(#{kernel => kernel(Filters), memory => false}, Extra)).

%% 四链都留痕的探针 filter
probe() ->
    Self = self(),
    beamai_filter:new(<<"probe">>, #{
        around_turn => fun(Req, _F, Next) ->
            Self ! {trace, turn_in}, R = Next(Req), Self ! {trace, turn_out}, R
        end,
        around_step => fun(Req, _F, Next) ->
            Self ! {trace, {step_in, maps:get(iteration, Req),
                            length(maps:get(tool_calls_made, Req))}},
            R = Next(Req),
            Self ! {trace, {step_out, maps:get(status, R)}},
            R
        end,
        around_chat => fun(Req, _F, Next) ->
            Self ! {trace, chat_in}, R = Next(Req), Self ! {trace, chat_out}, R
        end,
        around_tool => fun(Req, _F, Next) ->
            Self ! {trace, tool_in}, R = Next(Req), Self ! {trace, tool_out}, R
        end
    }).

drain() -> drain([]).
drain(Acc) ->
    receive {trace, T} -> drain([T | Acc])
    after 50 -> lists:reverse(Acc)
    end.

flush() -> receive {trace, _} -> flush() after 0 -> ok end.

count(P, L) when is_function(P, 1) -> length([E || E <- L, P(E)]);
count(X, L) -> length([E || E <- L, E =:= X]).

with_meck(Fun) ->
    flush(),
    try Fun() after meck:unload(beamai_chat_model) end.

%%====================================================================
%% 粒度：turn 一次、step 每轮一次、tool 每次一次
%%====================================================================

step_runs_once_per_iteration_test() ->
    with_meck(fun() ->
        mock_two_rounds(),
        {ok, A} = agent([probe()]),
        ?assertMatch({ok, #{content := <<"done">>}, _}, beamai_agent:run(A, <<"go">>)),
        T = drain(),
        ?assertEqual(1, count(turn_in, T)),
        ?assertEqual(2, count(fun({step_in, _, _}) -> true; (_) -> false end, T)),
        ?assertEqual(2, count(chat_in, T)),
        ?assertEqual(1, count(tool_in, T))
    end).

%% step 请求带迭代序与已发生的调用数：第 1 轮 (0, 0)，第 2 轮 (1, 1)
step_request_carries_progress_test() ->
    with_meck(fun() ->
        mock_two_rounds(),
        {ok, A} = agent([probe()]),
        {ok, _, _} = beamai_agent:run(A, <<"go">>),
        T = drain(),
        ?assertEqual([{step_in, 0, 0}, {step_in, 1, 1}],
                     [E || {step_in, _, _} = E <- T])
    end).

%% step 的 status：第一轮 continue、第二轮 final
step_response_status_test() ->
    with_meck(fun() ->
        mock_two_rounds(),
        {ok, A} = agent([probe()]),
        {ok, _, _} = beamai_agent:run(A, <<"go">>),
        ?assertEqual([{step_out, continue}, {step_out, final}],
                     [E || {step_out, _} = E <- drain()])
    end).

%%====================================================================
%% 层序：step 包住该轮的 chat **与工具执行**
%%====================================================================

step_wraps_chat_and_tools_test() ->
    with_meck(fun() ->
        mock_two_rounds(),
        {ok, A} = agent([probe()]),
        {ok, _, _} = beamai_agent:run(A, <<"go">>),
        ?assertEqual([turn_in,
                      {step_in, 0, 0}, chat_in, chat_out, tool_in, tool_out,
                      {step_out, continue},
                      {step_in, 1, 1}, chat_in, chat_out,
                      {step_out, final},
                      turn_out],
                     drain())
    end).

%%====================================================================
%% step filter 短路一轮迭代（不调 Next，自己合成 status）
%%====================================================================

step_filter_can_short_circuit_test() ->
    with_meck(fun() ->
        mock_two_rounds(),
        Short = beamai_filter:new(<<"short">>, #{
            around_step => fun(#{context := Ctx}, _F, _Next) ->
                #{status => final,
                  response => beamai_llm_response:new(
                                #{content => <<"canned">>, finish_reason => stop}),
                  messages => [], context => Ctx, tool_calls_made => []}
            end
        }),
        {ok, A} = agent([Short]),
        ?assertMatch({ok, #{content := <<"canned">>}, _}, beamai_agent:run(A, <<"go">>)),
        %% 短路即不调 LLM
        ?assertEqual(0, meck:num_calls(beamai_chat_model, chat, '_'))
    end).

%%====================================================================
%% 循环本身可替换（换掉链上那一环）
%%====================================================================

%% 只跑一轮就收尾的“循环”：LLM 一直要工具，但循环策略说了算
custom_loop_filter_test() ->
    with_meck(fun() ->
        mock_looping(),
        OneShot = fun(_LoopOpts) ->
            beamai_filter:new(<<"one_shot_loop">>, #{
                around_turn => fun(Req, _F, Next) ->
                    StepReq = #{messages => maps:get(messages, Req),
                                context => maps:get(context, Req),
                                iteration => 0, tool_calls_made => []},
                    #{messages := Msgs, tool_calls_made := Made} = Next(StepReq),
                    Resp = beamai_llm_response:new(
                             #{content => <<"stopped-after-one-step">>,
                               finish_reason => stop}),
                    {ok, Resp, Made, 1, Msgs}
                end
            })
        end,
        {ok, A} = agent([probe()], #{loop_filter => OneShot,
                                     max_tool_iterations => 100}),
        ?assertMatch({ok, #{content := <<"stopped-after-one-step">>}, _},
                     beamai_agent:run(A, <<"go">>)),
        T = drain(),
        %% 自定义循环只驱动了一轮：step 链照样生效（它是 turn 链的 terminal）
        ?assertEqual(1, count(fun({step_in, _, _}) -> true; (_) -> false end, T)),
        ?assertEqual(1, count(chat_in, T)),
        ?assertEqual(1, count(tool_in, T))
    end).

%% 缺省循环在同样的 LLM 下会一直转到迭代上限——对照组，证明上面是循环策略起的作用
default_loop_runs_to_limit_test() ->
    with_meck(fun() ->
        mock_looping(),
        {ok, A} = agent([], #{max_tool_iterations => 3}),
        ?assertMatch({error, {max_tool_iterations, _}}, beamai_agent:run(A, <<"go">>))
    end).
