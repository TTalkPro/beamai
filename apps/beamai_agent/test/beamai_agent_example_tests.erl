%%%-------------------------------------------------------------------
%%% @doc beamai_agent 端到端示例测试
%%%
%%% 一个完整示例：组装带「自定义 chat filter + tool filter + 真实工具 +
%%% 窗口记忆 + 回调」的 Agent，跑一轮 ReAct（工具调用 → 最终回复），
%%% 验证各部件协同：
%%%   - 预构建 kernel：new(Settings, Filters) 一次性给 filter + add_service(LLM) + 工具
%%%   - memory => {window, N}
%%%   - 回调 on_tool_result
%%%   - filter / 工具 / 回调均按预期触发，历史经 filter-memory 落库
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_example_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 示例工具模块（plugin 风格：tools/0）
%%====================================================================

%% 注：这里用内联 tool spec 而非独立模块，便于单文件演示。

%%====================================================================
%% 端到端示例
%%====================================================================

end_to_end_example_test() ->
    %% --- 1. Mock LLM：第一轮请求工具，第二轮给最终答复 ---
    meck:new(beamai_chat_completion, [passthrough]),
    CallCount = counters:new(1, []),
    meck:expect(beamai_chat_completion, chat, fun(_Config, _Messages, _Opts) ->
        counters:add(CallCount, 1, 1),
        case counters:get(CallCount, 1) of
            1 ->
                {ok, #{
                    content => null,
                    tool_calls => [#{
                        id => <<"call_1">>,
                        type => <<"function">>,
                        function => #{name => <<"get_weather">>,
                                      arguments => <<"{\"city\":\"Shanghai\"}">>}
                    }],
                    finish_reason => <<"tool_calls">>
                }};
            _ ->
                {ok, #{content => <<"上海 22℃，晴。"/utf8>>, finish_reason => <<"stop">>}}
        end
    end),

    Self = self(),
    try
        %% --- 2. 自定义 filter（chat + tool）---
        ChatFilter = beamai_filter:new(<<"ex_chat">>,
            #{around_chat => fun(#{messages := Msgs} = Req, _F, Next) ->
                Self ! {chat_filter, length(Msgs)},
                Next(Req)
            end}),
        ToolFilter = beamai_filter:new(<<"ex_tool">>,
            #{around_tool => fun(#{tool := Spec} = Req, _F, Next) ->
                Self ! {tool_filter, maps:get(name, Spec, <<>>)},
                Next(Req)
            end}),

        %% --- 3. 预构建 kernel：filter 一次性给出 + LLM 服务 + 工具 ---
        K0 = beamai_kernel:new(#{}, [ChatFilter, ToolFilter]),
        K1 = beamai_kernel:add_service(K0, beamai_chat_completion:create(mock, #{})),
        Kernel = beamai_kernel:add_tools(K1, [
            #{name => <<"get_weather">>,
              description => <<"查询城市天气"/utf8>>,
              parameters => #{city => #{type => string,
                                        description => <<"城市名"/utf8>>,
                                        required => true}},
              handler => fun(#{<<"city">> := City}, _Ctx) ->
                              {ok, #{city => City, temp => 22, sky => <<"sunny">>}}
                         end}
        ]),

        %% --- 4. 创建 Agent：窗口记忆 + 回调 ---
        {ok, Agent} = beamai_agent:new(#{
            kernel => Kernel,
            memory => {window, 50},
            conversation_id => <<"demo-conv">>,
            callbacks => #{
                on_tool_result => fun(Name, Result) -> Self ! {tool_result, Name, Result} end
            }
        }),

        %% --- 5. 跑一轮 ---
        {ok, Result, Agent1} = beamai_agent:run(Agent, <<"上海天气怎么样？"/utf8>>),

        %% --- 6. 断言：最终回复 + 工具调用记录 ---
        ?assertEqual(<<"上海 22℃，晴。"/utf8>>, maps:get(content, Result)),
        ?assertEqual(1, length(maps:get(tool_calls_made, Result, []))),
        ?assertEqual(1, beamai_agent:turn_count(Agent1)),

        %% chat filter 触发两次（工具轮 + 最终轮）；首轮看到 1 条消息（user）
        ?assertEqual([1, 3], collect(chat_filter, 2)),
        %% tool filter 拦截到 get_weather
        ?assertEqual([<<"get_weather">>], collect(tool_filter, 1)),
        %% on_tool_result 回调收到工具名 + 编码后结果
        receive {tool_result, <<"get_weather">>, R} ->
            ?assert(is_binary(R)),
            ?assert(binary:match(R, <<"sunny">>) =/= nomatch)
        after 1000 -> ?assert(false)
        end,

        %% --- 7. 历史经 memory provider 落库（默认 provider 带窗口 50）---
        ?assertMatch({beamai_memory_provider_default, {_, 50}},
                     beamai_agent_state:memory(Agent1)),
        Msgs = beamai_agent:messages(Agent1),
        %% user + assistant(tool_calls) + tool 结果 + assistant(final)
        ?assertEqual(4, length(Msgs)),
        ?assertEqual(<<"上海 22℃，晴。"/utf8>>, beamai_agent:last_response(Agent1))
    after
        meck:unload(beamai_chat_completion)
    end.

%%====================================================================
%% 辅助
%%====================================================================

%% @private 按序收集 N 个标签消息的载荷
collect(_Tag, 0) -> [];
collect(Tag, N) ->
    receive {Tag, Payload} -> [Payload | collect(Tag, N - 1)]
    after 1000 -> []
    end.
