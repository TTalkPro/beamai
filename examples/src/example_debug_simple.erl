%%%-------------------------------------------------------------------
%%% @doc Simple Checkpoint Debug Test
%%%
%%% 用于调试 checkpoint 保存流程，输出到标准输出
%%% @end
%%%-------------------------------------------------------------------
-module(example_debug_simple).

-export([run/0]).

run() ->
    io:format("=== Starting Simple Checkpoint Debug ===~n~n"),

    %% 1. 创建存储后端
    StoreName = simple_debug_store,
    {ok, _} = beamai_store_ets:start_link(StoreName, #{max_items => 1000}),

    %% 2. 创建 Memory 实例
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"simple_debug_thread">>
    }),
    ThreadId = beamai_memory:get_thread_id(Memory),
    io:format("thread_id: ~s~n~n", [ThreadId]),

    %% 3. 获取 LLM 配置
    case example_utils:get_llm_config() of
        {ok, LLMConfig} ->
            run_with_llm(LLMConfig, Memory, StoreName);
        {error, Reason} ->
            io:format("错误: 无法获取 LLM 配置: ~p~n", [Reason]),
            beamai_store_ets:stop(StoreName),
            {error, Reason}
    end.

run_with_llm(LLMConfig, Memory, StoreName) ->
    ThreadId = beamai_memory:get_thread_id(Memory),

    %% 打印 LLM 配置
    io:format("LLM Config:~n"),
    io:format("  provider: ~p~n", [maps:get(provider, LLMConfig, undefined)]),
    io:format("  model: ~p~n", [maps:get(model, LLMConfig, undefined)]),
    io:format("  base_url: ~p~n", [maps:get(base_url, LLMConfig, undefined)]),
    io:format("  api_key set: ~p~n", [maps:get(api_key, LLMConfig, undefined) =/= undefined]),
    io:format("~n"),

    %% 测试 LLM 直接调用
    io:format("Testing direct LLM call...~n"),
    TestMsgs = [#{role => user, content => <<"test"/utf8>>}],
    case llm_client:chat(LLMConfig, TestMsgs, #{}) of
        {ok, TestResp} ->
            io:format("  Direct LLM response: ~p~n", [maps:get(content, TestResp, null)]),
            io:format("  finish_reason: ~p~n", [maps:get(finish_reason, TestResp, undefined)]);
        {error, TestErr} ->
            io:format("  Direct LLM error: ~p~n", [TestErr])
    end,
    io:format("~n"),

    %% 4. 创建 Agent
    io:format("Creating Agent...~n"),
    {ok, Agent} = beamai_agent:start_link(<<"debug_agent">>, #{
        system_prompt => <<"你是一个有帮助的助手。请用一句话简短回答。"/utf8>>,
        llm => LLMConfig,
        storage => Memory
    }),

    %% 5. 运行对话
    io:format("~nSending message to Agent...~n"),
    Msg = <<"你好"/utf8>>,
    io:format("Message: ~ts~n", [Msg]),

    case beamai_agent:run(Agent, Msg) of
        {ok, Result} ->
            io:format("~n=== Agent Result ===~n"),
            io:format("status: ~p~n", [maps:get(status, Result, undefined)]),
            io:format("finish_reason: ~p~n", [maps:get(finish_reason, Result, undefined)]),
            io:format("iterations: ~p~n", [maps:get(iterations, Result, undefined)]),

            %% 检查错误字段
            case maps:get(warning, Result, undefined) of
                undefined -> ok;
                W -> io:format("warning: ~p~n", [W])
            end,

            %% 提取消息
            Messages = maps:get(messages, Result, []),
            io:format("~nMessages in result (~p):~n", [length(Messages)]),
            lists:foreach(fun(M) ->
                Role = get_role(M),
                Content = get_content(M),
                ToolCalls = maps:get(tool_calls, M, maps:get(<<"tool_calls">>, M, [])),
                io:format("  [~p] ~ts~n", [Role, truncate(Content, 100)]),
                case ToolCalls of
                    [] -> ok;
                    _ -> io:format("    tool_calls: ~p~n", [ToolCalls])
                end
            end, Messages),

            %% 最终响应
            FinalResponse = maps:get(final_response, Result, <<>>),
            io:format("~nFinal response: ~ts~n", [truncate(FinalResponse, 200)]),

            %% 打印完整结果用于调试
            io:format("~n=== Full Result Keys ===~n~p~n", [maps:keys(Result)]);
        {error, Reason} ->
            io:format("Agent run failed: ~p~n", [Reason])
    end,

    %% 6. 列出所有 checkpoints
    io:format("~n=== Checkpoints ===~n"),
    case beamai_memory:list_checkpoints(Memory, #{thread_id => ThreadId}) of
        {ok, Checkpoints} ->
            io:format("Total checkpoints: ~p~n~n", [length(Checkpoints)]),

            %% 按时间戳排序
            Sorted = lists:sort(
                fun({CpA, _, _}, {CpB, _, _}) ->
                    element(6, CpA) =< element(6, CpB)
                end,
                Checkpoints
            ),

            lists:foreach(fun({Cp, Meta, _}) ->
                CpId = element(2, Cp),
                Values = element(5, Cp),
                Timestamp = element(6, Cp),
                CheckpointType = element(2, Meta),
                Superstep = element(3, Meta),

                io:format("--- Checkpoint: ~s ---~n", [CpId]),
                io:format("  type: ~p, superstep: ~p, timestamp: ~p~n",
                         [CheckpointType, Superstep, Timestamp]),

                %% 检查 messages
                Msgs = get_messages_from_values(Values),
                io:format("  messages (~p):~n", [length(Msgs)]),
                lists:foreach(fun(M) ->
                    Role = get_role(M),
                    Content = get_content(M),
                    io:format("    [~p] ~ts~n", [Role, truncate(Content, 80)])
                end, Msgs),
                io:format("~n")
            end, Sorted);
        {error, Err} ->
            io:format("Failed to list checkpoints: ~p~n", [Err])
    end,

    %% 清理
    beamai_agent:stop(Agent),
    beamai_store_ets:stop(StoreName),
    io:format("=== Done ===~n"),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_messages_from_values(Values) when is_map(Values) ->
    case maps:get(<<"messages">>, Values, undefined) of
        undefined -> maps:get(messages, Values, []);
        Msgs -> Msgs
    end;
get_messages_from_values(_) ->
    [].

get_role(M) when is_map(M) ->
    case maps:get(role, M, undefined) of
        undefined -> maps:get(<<"role">>, M, unknown);
        R -> R
    end;
get_role(_) -> unknown.

get_content(M) when is_map(M) ->
    case maps:get(content, M, undefined) of
        undefined -> maps:get(<<"content">>, M, <<>>);
        C when is_binary(C) -> C;
        C when is_list(C) -> iolist_to_binary(io_lib:format("~p", [C]));
        C -> iolist_to_binary(io_lib:format("~p", [C]))
    end;
get_content(_) -> <<>>.

truncate(Content, MaxLen) when is_binary(Content) ->
    case byte_size(Content) > MaxLen of
        true -> <<(binary:part(Content, 0, MaxLen))/binary, "..."/utf8>>;
        false -> Content
    end;
truncate(null, _) -> <<"(null)">>;
truncate(Other, _) -> iolist_to_binary(io_lib:format("~p", [Other])).
