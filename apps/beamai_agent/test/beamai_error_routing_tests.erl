%%%-------------------------------------------------------------------
%%% @doc 工具错误分层路由：环境类暂停 + resume retry / proceed 缺省
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_error_routing_tests).

-include_lib("eunit/include/eunit.hrl").

tc(Id, Name) ->
    #{id => Id, type => <<"function">>,
      function => #{name => Name, arguments => <<"{}">>}}.

mock_llm(Fun) ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) -> Fun() end).

kernel_with(Tools) ->
    K0 = beamai_kernel:add_service(beamai_kernel:new(),
                                   beamai_chat_completion:create(mock, #{})),
    lists:foldl(fun({Name, H}, K) ->
        beamai_kernel:add_tool(K, #{name => Name, parameters => #{}, handler => H})
    end, K0, Tools).

%%====================================================================
%% 环境类失败 → 暂停（on_env_error=pause）→ resume retry 修复
%%====================================================================

env_pause_and_resume_retry_test() ->
    CC = counters:new(1, []),
    mock_llm(fun() ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [tc(<<"c1">>, <<"env_tool">>)]}};
            _ -> {ok, #{content => <<"done">>, finish_reason => <<"stop">>}}
        end
    end),
    TCtr = counters:new(1, []),
    EnvTool = fun(_, _) ->
        N = counters:get(TCtr, 1) + 1,
        counters:add(TCtr, 1, 1),
        case N of
            1 -> error(#{error_class => environment, message => <<"auth down">>});
            _ -> {ok, <<"recovered">>}
        end
    end,
    try
        {ok, Agent} = beamai_agent:new(#{
            kernel => kernel_with([{<<"env_tool">>, EnvTool}]),
            on_env_error => pause
        }),
        %% 第一次执行环境失败 → 暂停
        {interrupt, Info, A1} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(env_retry, maps:get(interrupt_type, Info)),
        ?assert(beamai_agent:is_interrupted(A1)),
        %% resume retry：重跑失败调用（这次成功），续跑到最终响应
        {ok, Result, A2} = beamai_agent:resume(A1, <<"retry">>),
        ?assertEqual(<<"done">>, maps:get(content, Result)),
        ?assertNot(beamai_agent:is_interrupted(A2)),
        ?assertEqual(2, counters:get(TCtr, 1))  %% 初次失败 + retry 成功
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 缺省 proceed：环境失败也走 errors-are-data，不暂停
%%====================================================================

env_error_proceed_default_test() ->
    CC = counters:new(1, []),
    mock_llm(fun() ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [tc(<<"c1">>, <<"env_tool">>)]}};
            _ -> {ok, #{content => <<"handled">>, finish_reason => <<"stop">>}}
        end
    end),
    EnvTool = fun(_, _) -> error(#{error_class => environment}) end,
    try
        %% 无 HITL、无显式 on_env_error → 缺省 proceed
        {ok, Agent} = beamai_agent:new(#{
            kernel => kernel_with([{<<"env_tool">>, EnvTool}])
        }),
        {ok, Result, A1} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(<<"handled">>, maps:get(content, Result)),
        ?assertNot(beamai_agent:is_interrupted(A1))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 语义类失败：照旧 errors-are-data 回模型（无暂停，无重试）
%%====================================================================

semantic_error_goes_to_model_test() ->
    CC = counters:new(1, []),
    mock_llm(fun() ->
        counters:add(CC, 1, 1),
        case counters:get(CC, 1) of
            1 -> {ok, #{content => null, finish_reason => <<"tool_calls">>,
                        tool_calls => [tc(<<"c1">>, <<"sem_tool">>)]}};
            _ -> {ok, #{content => <<"ok">>, finish_reason => <<"stop">>}}
        end
    end),
    SemTool = fun(_, _) -> {error, not_found} end,   %% semantic
    try
        {ok, Agent} = beamai_agent:new(#{
            kernel => kernel_with([{<<"sem_tool">>, SemTool}]),
            on_env_error => pause    %% 即便 pause 策略，语义类不触发暂停
        }),
        {ok, Result, A1} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(<<"ok">>, maps:get(content, Result)),
        ?assertNot(beamai_agent:is_interrupted(A1))
    after
        meck:unload(beamai_chat_completion)
    end.
