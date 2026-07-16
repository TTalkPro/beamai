%%%-------------------------------------------------------------------
%%% @doc max_tool_calls（整轮 run 的工具调用总数上限）测试
%%%
%%% 为什么限额在 tool loop 而不在 around_tool filter：
%%% filter 拿到的 context 是每轮初的**只读快照**（execute_sequential/4 把同一份
%%% Context 交给批内每个工具；design/context_split_parallel_tools.md §4.1 明确
%%% 「快照 + 屏障折叠，不引入批内穿线」）。批内每个工具读到的计数都一样，
%%% filter 里的计数器天生累加不起来——一批并发 5 个工具，各自算出 count+1，
%%% 折叠完还是 +1。循环这一层是串行的，length(ToolCallsMade) 无歧义。
%%%
%%% 与 max_tool_iterations 正交：后者限"来回几轮"，前者限"总共调了多少次工具"。
%%% 一轮可以并发调 20 个工具，迭代上限拦不住。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_max_tool_calls_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 配置
%%====================================================================

%% 缺省不设限：框架不替调用者判断"调几次算太多"
default_is_infinity_test() ->
    {ok, State} = beamai_agent_state:create(#{llm => {mock, #{}}}),
    ?assertEqual(infinity, maps:get(max_tool_calls, State)).

explicit_config_is_kept_test() ->
    {ok, State} = beamai_agent_state:create(#{llm => {mock, #{}}, max_tool_calls => 3}),
    ?assertEqual(3, maps:get(max_tool_calls, State)).

%%====================================================================
%% 限额行为（集成）
%%====================================================================

%% 每轮 LLM 都要求调一次工具 → 无限循环；max_tool_calls=3 必须刹住
limit_stops_the_loop_test() ->
    with_looping_llm(1, fun() ->
        {ok, Agent} = agent(#{max_tool_calls => 3, max_tool_iterations => 100}),
        ?assertMatch({error, {max_tool_calls, _}}, beamai_agent:run(Agent, <<"go">>))
    end).

%% 报错要带上已发生的调用，便于排查
error_carries_calls_made_test() ->
    with_looping_llm(1, fun() ->
        {ok, Agent} = agent(#{max_tool_calls => 3, max_tool_iterations => 100}),
        {error, {max_tool_calls, Calls}} = beamai_agent:run(Agent, <<"go">>),
        ?assert(is_list(Calls)),
        ?assert(length(Calls) >= 3)
    end).

%% infinity（缺省）下不该被这个限额拦住——拦住它的应该是 max_tool_iterations
default_does_not_limit_test() ->
    with_looping_llm(1, fun() ->
        {ok, Agent} = agent(#{max_tool_iterations => 3}),
        ?assertMatch({error, {max_tool_iterations, _}}, beamai_agent:run(Agent, <<"go">>))
    end).

%% 未触及上限时正常收尾
under_limit_completes_normally_test() ->
    %% 第一轮调工具，第二轮给终答
    with_finishing_llm(fun() ->
        {ok, Agent} = agent(#{max_tool_calls => 10}),
        ?assertMatch({ok, #{content := <<"done">>}, _}, beamai_agent:run(Agent, <<"go">>))
    end).

%% 关键：一轮并发多个 tool_call 也要被计入总数。
%% 这正是 filter 方案做不到的地方——批内快照让计数只前进 1。
%% max_tool_calls=3、每轮 2 个调用：第 1 轮后 2 次，第 2 轮后 4 次 >= 3 → 停。
parallel_batch_calls_all_counted_test() ->
    with_looping_llm(2, fun() ->
        {ok, Agent} = agent(#{max_tool_calls => 3, max_tool_iterations => 100}),
        {error, {max_tool_calls, Calls}} = beamai_agent:run(Agent, <<"go">>),
        %% 两轮各 2 个 = 4 次，说明并发批内的每一次都被算进去了
        ?assertEqual(4, length(Calls))
    end).

%% 上限为 1：第一批跑完就该停
limit_one_stops_after_first_batch_test() ->
    with_looping_llm(1, fun() ->
        {ok, Agent} = agent(#{max_tool_calls => 1, max_tool_iterations => 100}),
        {error, {max_tool_calls, Calls}} = beamai_agent:run(Agent, <<"go">>),
        ?assertEqual(1, length(Calls))
    end).

%%====================================================================
%% 辅助
%%====================================================================

agent(Extra) ->
    beamai_agent:new(maps:merge(
        #{llm => {mock, #{}}, memory => false, plugins => [beamai_agent_test_plugin]},
        Extra)).

%% LLM 每轮都要求调 N 个工具，永不收尾
with_looping_llm(N, Fun) ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) ->
        {ok, #{content => null,
               tool_calls => [tool_call(I) || I <- lists:seq(1, N)],
               finish_reason => <<"tool_calls">>}}
    end),
    try Fun() after meck:unload(beamai_chat_completion) end.

%% 第一轮调工具，之后收尾
with_finishing_llm(Fun) ->
    Ctr = counters:new(1, []),
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, _O) ->
        case counters:get(Ctr, 1) of
            0 ->
                counters:add(Ctr, 1, 1),
                {ok, #{content => null, tool_calls => [tool_call(1)],
                       finish_reason => <<"tool_calls">>}};
            _ ->
                {ok, #{content => <<"done">>, tool_calls => [], finish_reason => <<"stop">>}}
        end
    end),
    try Fun() after meck:unload(beamai_chat_completion) end.

%% 用 test_plugin 真实提供的工具：要验证的是**成功执行**的调用被计入，
%% 而不是 tool_not_found 的错误路径也被计入。
tool_call(I) ->
    Id = list_to_binary("call_" ++ integer_to_list(I)),
    #{id => Id, type => <<"function">>,
      function => #{name => <<"plugin_tool">>, arguments => <<"{}">>}}.
