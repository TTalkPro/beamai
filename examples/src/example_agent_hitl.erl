%%%-------------------------------------------------------------------
%%% @doc Human-in-the-Loop Agent 示例
%%%
%%% 演示三种中断触发方式和 resume 流程：
%%%   1. interrupt_tools 触发（LLM 调用 ask_human tool）
%%%   2. callback 触发（on_tool_call 返回 {interrupt, Reason}）
%%%   3. 从 memory 恢复中断状态
%%%
%%% 使用方法：
%%%   make shell 后执行：
%%%   example_agent_hitl:demo_interrupt_tool().
%%%   example_agent_hitl:demo_callback_interrupt().
%%%   example_agent_hitl:demo_memory_resume().
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agent_hitl).

-export([
    demo_interrupt_tool/0,
    demo_callback_interrupt/0,
    demo_memory_resume/0
]).

%%====================================================================
%% 示例 1: Interrupt Tool 触发
%%====================================================================

%% @doc 演示 LLM 调用 ask_human tool 触发中断
%%
%% 场景：Agent 需要人类确认后才能执行危险操作。
%% LLM 被提示在执行删除操作前调用 ask_human 工具征求确认。
demo_interrupt_tool() ->
    io:format("~n=== Demo: Interrupt Tool ===~n"),

    %% 创建带 interrupt tool 的 agent
    {ok, Agent} = beamai_agent:new(#{
        llm => {mock, #{}},  %% 实际使用时替换为 {zhipu, #{model => <<"glm-4">>}}
        system_prompt => <<"You are a file management assistant. "
                          "Before performing any destructive operation, "
                          "always call ask_human to confirm with the user.">>,
        interrupt_tools => [#{
            name => <<"ask_human">>,
            description => <<"Ask the human operator for input or approval before proceeding">>,
            parameters => #{
                type => object,
                properties => #{
                    question => #{
                        type => string,
                        description => <<"The question to ask the human">>
                    }
                },
                required => [<<"question">>]
            }
        }]
    }),

    io:format("Agent created with interrupt tool: ask_human~n"),
    io:format("Running agent with message: 'Please delete the temp files'~n~n"),

    %% 执行 - 当 LLM 调用 ask_human 时会中断
    case beamai_agent:run(Agent, <<"Please delete the temp files">>) of
        {ok, Result, _Agent1} ->
            io:format("Agent completed without interrupt: ~s~n",
                     [maps:get(content, Result)]);
        {interrupt, Info, Agent1} ->
            io:format("Agent interrupted!~n"),
            io:format("  Type: ~p~n", [maps:get(interrupt_type, Info)]),
            io:format("  Reason: ~p~n", [maps:get(reason, Info)]),
            io:format("  Is interrupted: ~p~n~n", [beamai_agent:is_interrupted(Agent1)]),

            %% 人类确认后恢复
            io:format("Resuming with human input: 'Yes, approved'~n"),
            case beamai_agent:resume(Agent1, <<"Yes, approved. Go ahead.">>) of
                {ok, Result2, Agent2} ->
                    io:format("Agent completed: ~s~n", [maps:get(content, Result2)]),
                    io:format("Turn count: ~p~n", [beamai_agent:turn_count(Agent2)]);
                {interrupt, Info2, _} ->
                    io:format("Agent interrupted again: ~p~n", [Info2]);
                {error, Reason} ->
                    io:format("Resume error: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% 示例 2: Callback 触发中断
%%====================================================================

%% @doc 演示 on_tool_call callback 触发中断
%%
%% 场景：Agent 有 execute_sql tool，当检测到 DELETE/DROP 语句时
%% 通过 callback 中断执行，要求人类确认。
demo_callback_interrupt() ->
    io:format("~n=== Demo: Callback Interrupt ===~n"),

    %% 创建 kernel 并注册 SQL tool
    Kernel0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{}),
    K1 = beamai_kernel:add_service(Kernel0, LlmConfig),
    K2 = beamai_kernel:add_plugin(K1, <<"database">>, [
        #{name => <<"execute_sql">>,
          description => <<"Execute SQL query on the database">>,
          parameters => #{
              type => object,
              properties => #{
                  sql => #{type => string, description => <<"SQL statement">>}
              }
          },
          handler => fun(Args, _Ctx) ->
              SQL = maps:get(sql, Args, maps:get(<<"sql">>, Args, <<>>)),
              io:format("  [DB] Executing: ~s~n", [SQL]),
              {ok, <<"Query executed successfully, 42 rows affected">>}
          end}
    ]),

    %% Callback: 拦截危险 SQL
    Callbacks = #{
        on_tool_call => fun(Name, Args) ->
            case Name of
                <<"execute_sql">> ->
                    SQL = case maps:get(sql, Args, undefined) of
                        undefined -> maps:get(<<"sql">>, Args, <<>>);
                        V -> V
                    end,
                    IsDangerous = lists:any(fun(Keyword) ->
                        binary:match(SQL, Keyword) =/= nomatch
                    end, [<<"DELETE">>, <<"DROP">>, <<"TRUNCATE">>]),
                    case IsDangerous of
                        true ->
                            {interrupt, #{
                                reason => dangerous_sql,
                                sql => SQL,
                                warning => <<"This SQL modifies/deletes data">>
                            }};
                        false ->
                            ok
                    end;
                _ -> ok
            end
        end,
        on_interrupt => fun(IntState, _Meta) ->
            io:format("  [Callback] Agent entering interrupt state~n"),
            io:format("  [Callback] Reason: ~p~n", [maps:get(reason, IntState)])
        end
    },

    {ok, Agent} = beamai_agent:new(#{kernel => K2, callbacks => Callbacks}),

    io:format("Agent created with SQL tool and safety callback~n"),
    io:format("Running agent...~n~n"),

    case beamai_agent:run(Agent, <<"Delete all expired sessions from the database">>) of
        {interrupt, Info, Agent1} ->
            io:format("~nAgent interrupted by callback!~n"),
            io:format("  Type: ~p~n", [maps:get(interrupt_type, Info)]),
            io:format("  Reason: ~p~n~n", [maps:get(reason, Info)]),

            io:format("Resuming with approval...~n"),
            case beamai_agent:resume(Agent1, <<"Confirmed, execute the DELETE">>) of
                {ok, Result, _} ->
                    io:format("Agent completed: ~s~n", [maps:get(content, Result)]);
                Other ->
                    io:format("Resume result: ~p~n", [Other])
            end;
        {ok, Result, _} ->
            io:format("Completed (no interrupt): ~s~n", [maps:get(content, Result)]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% 示例 3: Memory 持久化恢复
%%====================================================================

%% @doc 演示从 memory 加载中断状态并恢复
%%
%% 场景：Agent 被中断后保存到 memory，
%% 稍后（可能在另一个进程中）从 memory 加载并恢复执行。
demo_memory_resume() ->
    io:format("~n=== Demo: Memory Resume ===~n"),

    %% 创建 memory store
    StoreName = demo_hitl_store,
    case beamai_store_ets:start_link(StoreName, #{}) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"demo-hitl-thread">>
    }),

    AgentConfig = #{
        llm => {mock, #{}},
        memory => Memory,
        auto_save => true,  %% 中断时自动保存
        interrupt_tools => [#{
            name => <<"ask_human">>,
            description => <<"Ask human for approval">>,
            parameters => #{type => object, properties => #{
                question => #{type => string}
            }}
        }]
    },

    %% Step 1: 创建 agent 并运行直到中断
    io:format("Step 1: Running agent (will be interrupted)~n"),
    {ok, Agent} = beamai_agent:new(AgentConfig),
    case beamai_agent:run(Agent, <<"Deploy the new version to production">>) of
        {interrupt, Info, _Agent1} ->
            io:format("  Agent interrupted: ~p~n", [maps:get(reason, Info)]),
            io:format("  State auto-saved to memory~n~n");
        Other ->
            io:format("  Unexpected: ~p~n", [Other])
    end,

    %% Step 2: 检查 memory 中的中断状态
    io:format("Step 2: Checking memory for pending interrupt~n"),
    ThreadConfig = #{thread_id => <<"demo-hitl-thread">>},
    HasInterrupt = beamai_memory:has_pending_interrupt(Memory, ThreadConfig),
    io:format("  Has pending interrupt: ~p~n", [HasInterrupt]),

    case beamai_memory:get_interrupt_context(Memory, ThreadConfig) of
        {ok, Ctx} ->
            io:format("  Interrupt context: ~p~n~n", [Ctx]);
        {error, not_interrupted} ->
            io:format("  No interrupt context found~n~n")
    end,

    %% Step 3: 从 memory 恢复并继续执行
    io:format("Step 3: Resuming from memory~n"),
    case beamai_agent:resume_from_memory(AgentConfig, Memory, <<"Yes, deploy it">>) of
        {ok, Result, Agent2} ->
            io:format("  Agent completed: ~s~n", [maps:get(content, Result)]),
            io:format("  Turn count: ~p~n", [beamai_agent:turn_count(Agent2)]);
        {interrupt, Info2, _} ->
            io:format("  Interrupted again: ~p~n", [Info2]);
        {error, Reason} ->
            io:format("  Error: ~p~n", [Reason])
    end,

    %% Cleanup
    beamai_store_ets:stop(StoreName),
    io:format("~nDemo complete!~n"),
    ok.
