%%%-------------------------------------------------------------------
%%% @doc 工具检索测试（对标 Spring AI ToolSearchToolCallingAdvisor）
%%%
%%% 核心断言对象是**每轮实际广播给 LLM 的 tools 列表**——检索的全部价值就在
%%% 于这个列表短了多少，故各用例都把 mock LLM 收到的 opts.tools 捞回来比对。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_search_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 辅助
%%====================================================================

tool(Name, Desc) ->
    #{name => Name, description => Desc, parameters => #{},
      handler => fun(_) -> {ok, <<"ran">>} end}.

%% 一组彼此无关的工具，让检索有得挑
tools() ->
    [tool(<<"get_weather">>, <<"查询指定城市的天气预报"/utf8>>),
     tool(<<"send_email">>, <<"给收件人发送一封电子邮件"/utf8>>),
     tool(<<"create_invoice">>, <<"生成一张发票"/utf8>>),
     tool(<<"list_files">>, <<"列出目录下的文件"/utf8>>),
     tool(<<"run_sql">>, <<"在数据库上执行 SQL 查询"/utf8>>)].

%% mock LLM：把每次收到的 opts.tools 里的工具名送回 Parent，恒返回一句话
mock_llm(Parent) ->
    meck:new(beamai_chat_completion, [passthrough]),
    meck:expect(beamai_chat_completion, chat, fun(_C, _M, O) ->
        Parent ! {advertised, [N || #{name := N} <- maps:get(tools, O, [])]},
        {ok, #{content => <<"ok">>, finish_reason => <<"stop">>}}
    end).

%% 建带检索的 kernel：全量注册，filter 负责裁剪广播
search_kernel(Opts) ->
    Tools = tools(),
    {SearchTool, Filter} = beamai_tool_search:new(Tools, Opts),
    K0 = beamai_kernel:new(#{}, [Filter]),
    K = beamai_kernel:add_tools(K0, [SearchTool | Tools]),
    beamai_kernel:add_service(K, beamai_chat_completion:create(mock, #{})).

%% 走一次 invoke_chat，返回该轮广播的工具名（已排序）
advertised(K, Messages) ->
    ToolSpecs = beamai_kernel:get_tool_specs(K),
    {ok, _, _} = beamai_kernel:invoke_chat(K, Messages, #{tools => ToolSpecs}),
    receive {advertised, Names} -> lists:sort(Names)
    after 200 -> erlang:error(no_llm_call)
    end.

%% tool_search 的一次调用 + 结果，拼成历史
search_round(Id, Names) ->
    [#{role => assistant, content => null,
       tool_calls => [#{id => Id, type => <<"function">>,
                        function => #{name => <<"tool_search">>,
                                      arguments => <<"{}">>}}]},
     #{role => tool, tool_call_id => Id, content => jsx:encode(Names)}].

user(Text) -> [#{role => user, content => Text}].

%% 直接跑一次检索工具，返回工具名列表
search(Opts, Query) ->
    {SearchTool, _} = beamai_tool_search:new(tools(), Opts),
    K = beamai_kernel:add_tool(beamai_kernel:new(), SearchTool),
    {ok, Names, _} = beamai_kernel:invoke_tool(
        K, <<"tool_search">>, #{<<"query">> => Query}, beamai_context:new()),
    Names.

flush() ->
    receive {advertised, _} -> flush() after 0 -> ok end.

with_mock(Fun) ->
    flush(),
    mock_llm(self()),
    try Fun() after meck:unload(beamai_chat_completion) end.

%%====================================================================
%% 裁剪行为
%%====================================================================

%% 首轮：没检索过 → 只广播检索工具本身（5 个业务工具一个不给）
first_round_only_search_tool_test() ->
    with_mock(fun() ->
        K = search_kernel(#{}),
        ?assertEqual([<<"tool_search">>], advertised(K, user(<<"今天天气"/utf8>>)))
    end).

%% 检索点过名的工具进入广播列表
selected_tools_advertised_test() ->
    with_mock(fun() ->
        K = search_kernel(#{}),
        Msgs = user(<<"q">>) ++ search_round(<<"s1">>, [<<"get_weather">>]),
        ?assertEqual([<<"get_weather">>, <<"tool_search">>], advertised(K, Msgs))
    end).

%% accumulate=true（缺省）：历次检索取并集
accumulate_unions_searches_test() ->
    with_mock(fun() ->
        K = search_kernel(#{}),
        Msgs = user(<<"q">>)
            ++ search_round(<<"s1">>, [<<"get_weather">>])
            ++ search_round(<<"s2">>, [<<"send_email">>]),
        ?assertEqual([<<"get_weather">>, <<"send_email">>, <<"tool_search">>],
                     advertised(K, Msgs))
    end).

%% accumulate=false：只认最近一次检索
no_accumulate_uses_last_only_test() ->
    with_mock(fun() ->
        K = search_kernel(#{accumulate => false}),
        Msgs = user(<<"q">>)
            ++ search_round(<<"s1">>, [<<"get_weather">>])
            ++ search_round(<<"s2">>, [<<"send_email">>]),
        ?assertEqual([<<"send_email">>, <<"tool_search">>], advertised(K, Msgs))
    end).

%% 检索工具永远广播——否则一轮不中就再没机会检索，彻底卡死
search_tool_always_present_test() ->
    with_mock(fun() ->
        K = search_kernel(#{}),
        Msgs = user(<<"q">>) ++ search_round(<<"s1">>, []),
        ?assertEqual([<<"tool_search">>], advertised(K, Msgs))
    end).

%% 未索引的工具（如运行时追加的中断工具）原样透传，不被静默吃掉
unindexed_tools_pass_through_test() ->
    with_mock(fun() ->
        K = search_kernel(#{}),
        Extra = #{name => <<"ask_human">>, description => <<"问人">>, parameters => #{}},
        Specs = beamai_kernel:get_tool_specs(K) ++ [Extra],
        {ok, _, _} = beamai_kernel:invoke_chat(K, user(<<"q">>), #{tools => Specs}),
        receive {advertised, Names} ->
            ?assertEqual([<<"ask_human">>, <<"tool_search">>], lists:sort(Names))
        after 200 -> erlang:error(no_llm_call)
        end
    end).

%% 检索结果里出现未注册的名字：忽略（模型幻觉/索引与注册不一致时不炸）
unknown_selected_name_ignored_test() ->
    with_mock(fun() ->
        K = search_kernel(#{}),
        Msgs = user(<<"q">>) ++ search_round(<<"s1">>, [<<"ghost_tool">>]),
        ?assertEqual([<<"tool_search">>], advertised(K, Msgs))
    end).

%% 结果消息内容不是合法 JSON：当没检索过，不崩
malformed_search_result_test() ->
    with_mock(fun() ->
        K = search_kernel(#{}),
        Msgs = user(<<"q">>) ++
            [#{role => assistant, content => null,
               tool_calls => [#{id => <<"s1">>, type => <<"function">>,
                                function => #{name => <<"tool_search">>,
                                              arguments => <<"{}">>}}]},
             #{role => tool, tool_call_id => <<"s1">>, content => <<"not json{">>}],
        ?assertEqual([<<"tool_search">>], advertised(K, Msgs))
    end).

%% 别的工具的结果消息不会被误认成检索结果（靠 assistant 回合的 id 认领）
other_tool_result_not_claimed_test() ->
    with_mock(fun() ->
        K = search_kernel(#{}),
        Msgs = user(<<"q">>) ++
            [#{role => assistant, content => null,
               tool_calls => [#{id => <<"x1">>, type => <<"function">>,
                                function => #{name => <<"get_weather">>,
                                              arguments => <<"{}">>}}]},
             #{role => tool, tool_call_id => <<"x1">>,
               content => jsx:encode([<<"send_email">>])}],
        %% get_weather 的结果碰巧长得像检索结果，但不是 tool_search 发起的，不认
        ?assertEqual([<<"tool_search">>], advertised(K, Msgs))
    end).

%% opts 无 tools 字段（无工具可用）：原样不动，不凭空造 tools 键
no_tools_key_untouched_test() ->
    with_mock(fun() ->
        K = search_kernel(#{}),
        {ok, _, _} = beamai_kernel:invoke_chat(K, user(<<"q">>), #{}),
        receive {advertised, Names} -> ?assertEqual([], Names)
        after 200 -> erlang:error(no_llm_call)
        end
    end).

%%====================================================================
%% 检索工具本身
%%====================================================================

%% 检索工具可执行，返回相关工具名
%%
%% invoke_tool 给出的是 handler 的**原始**返回（工具名列表）；转 JSON 是工具
%% 结果**成为消息时**才由 beamai_tool:encode_result 做的（见 encoded_result_round_trip_test）。
search_tool_finds_relevant_test() ->
    ?assert(lists:member(<<"get_weather">>, search(#{}, <<"weather forecast 天气"/utf8>>))).

%% max_results 截断
search_tool_respects_max_results_test() ->
    Names = search(#{max_results => 2}, <<"email file sql weather invoice">>),
    ?assert(length(Names) =< 2).

%% query 缺失：返回空，不崩（模型给的参数不可信）
search_tool_missing_query_test() ->
    {SearchTool, _} = beamai_tool_search:new(tools(), #{}),
    K = beamai_kernel:add_tool(beamai_kernel:new(), SearchTool),
    ?assertMatch({ok, [], _},
                 beamai_kernel:invoke_tool(K, <<"tool_search">>, #{},
                                           beamai_context:new())).

%% 空 query 在**任何**后端上都返回空
%%
%% 尤其针对 regex：空模式是合法正则、匹配一切，不收口就会把全量工具泄回提示词，
%% 检索的意义尽失。这条锁的是「没说要什么就什么都不给」跨后端一致。
search_tool_empty_query_all_backends_test() ->
    [?assertEqual([], search(#{index_module => M}, <<>>))
     || M <- [beamai_tool_index_keyword, beamai_tool_index_regex]].

%% 中文查询能召回中文描述的工具（本项目描述以中文为主，这条是刚需）
search_tool_chinese_query_test() ->
    ?assert(lists:member(<<"get_weather">>, search(#{}, <<"查询天气"/utf8>>))).

%% 工具返回经 encode_result 编码后，正是 filter 认得的形态（闭合两端的契约）
encoded_result_round_trip_test() ->
    Encoded = beamai_tool:encode_result(search(#{}, <<"weather">>)),
    ?assertEqual([<<"get_weather">>], jsx:decode(Encoded, [return_maps])).

%% 自定义工具名贯穿工具与 filter
custom_tool_name_test() ->
    with_mock(fun() ->
        Tools = tools(),
        {SearchTool, Filter} = beamai_tool_search:new(Tools, #{tool_name => <<"find_tool">>}),
        ?assertEqual(<<"find_tool">>, beamai_tool:get_name(SearchTool)),
        K0 = beamai_kernel:new(#{}, [Filter]),
        K1 = beamai_kernel:add_tools(K0, [SearchTool | Tools]),
        K = beamai_kernel:add_service(K1, beamai_chat_completion:create(mock, #{})),
        ?assertEqual([<<"find_tool">>], advertised(K, user(<<"q">>)))
    end).

%% regex 后端亦可用（behaviour 可换）
regex_backend_test() ->
    ?assertEqual([<<"get_weather">>],
                 search(#{index_module => beamai_tool_index_regex}, <<"weather">>)).
