%%%-------------------------------------------------------------------
%%% @doc Process Framework 简单示例
%%%
%%% 演示 Process Framework 的基础用法：
%%%   1. 简单 LLM 对话管线（翻译 -> 总结）
%%%   2. 多轮对话步骤（上下文保持）
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_process_simple:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_process_simple).

-export([run/0, run/1]).
-export([run_multiturn/0, run_multiturn/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 运行简单管线示例（翻译 -> 总结）
-spec run() -> ok.
run() ->
    run(example_llm_config:anthropic()).

-spec run(beamai_chat_completion:config()) -> ok.
run(LLMConfig) ->
    io:format("~n=== Process Framework: 简单管线示例 ===~n~n"),
    io:format("管线: 用户输入 -> 翻译(中->英) -> 总结 -> 输出~n~n"),

    ensure_started(),

    %% 构建 Kernel（包含 LLM 服务）
    Kernel = beamai:add_llm(beamai:kernel(), LLMConfig),
    Context = beamai_context:with_kernel(beamai_context:new(), Kernel),

    %% 构建 Process: translate -> summarize
    P0 = beamai_process:builder(translate_summarize),

    P1 = beamai_process:add_step(P0, translator, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"你是一个翻译助手。请将用户输入的中文翻译成英文。只输出翻译结果，不要解释。"/utf8>>,
        output_event => translated
    }),

    P2 = beamai_process:add_step(P1, summarizer, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"You are a summarization assistant. Summarize the input text in one brief sentence.">>,
        output_event => summarized
    }),

    %% 事件绑定: start -> translator, translated -> summarizer
    P3 = beamai_process:on_event(P2, start, translator, input),
    P4 = beamai_process:on_event(P3, translated, summarizer, input,
        fun(#{response := Response}) -> #{user_message => Response} end),

    %% 初始事件
    UserInput = <<"Erlang是一种通用的并发编程语言，最初由爱立信开发，用于构建大规模可靠的分布式系统。它具有轻量级进程、消息传递和容错机制等特性。"/utf8>>,
    io:format("用户输入: ~ts~n~n", [UserInput]),
    P5 = beamai_process:set_initial_event(P4, start, #{user_message => UserInput}),
    P6 = beamai_process:set_execution_mode(P5, sequential),

    {ok, Def} = beamai_process:build(P6),
    case beamai_process:run_sync(Def, #{timeout => 60000, context => Context}) of
        {ok, Result} ->
            #{translator := TransState, summarizer := SumState} = Result,
            io:format("翻译结果:~n"),
            print_step_response(TransState),
            io:format("~n总结结果:~n"),
            print_step_response(SumState),
            io:format("~n管线完成!~n~n");
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,
    ok.

%% @doc 运行多轮对话示例
-spec run_multiturn() -> ok.
run_multiturn() ->
    run_multiturn(example_llm_config:anthropic()).

-spec run_multiturn(beamai_chat_completion:config()) -> ok.
run_multiturn(LLMConfig) ->
    io:format("~n=== Process Framework: 多轮对话示例 ===~n~n"),

    ensure_started(),

    Kernel = beamai:add_llm(beamai:kernel(), LLMConfig),
    Context = beamai_context:with_kernel(beamai_context:new(), Kernel),

    %% 构建多轮对话 Process: 3轮自动对话
    P0 = beamai_process:builder(multiturn_chat),

    P1 = beamai_process:add_step(P0, chatter, example_process_steps, #{
        type => llm_multiturn,
        system_prompt => <<"你是一个Erlang编程专家。请用简短的语句回答问题。每次回答不超过2句话。"/utf8>>,
        output_event => chat_done
    }),

    %% 自循环: 每轮结束后生成下一轮输入
    P2 = beamai_process:add_step(P1, followup_gen, example_process_steps, #{
        type => transform,
        transform_fn => fun(#{response := _Resp, turn := Turn}) ->
            case Turn of
                1 -> #{user_message => <<"它的进程模型和操作系统进程有什么区别？"/utf8>>};
                2 -> #{user_message => <<"OTP中的supervisor树是怎么工作的？"/utf8>>};
                _ -> done
            end
        end,
        output_event => next_question
    }),

    %% 绑定
    P3 = beamai_process:on_event(P2, start, chatter, input),
    P4 = beamai_process:on_event(P3, chat_done, followup_gen, input),
    P5 = beamai_process:on_event(P4, next_question, chatter, input,
        fun(done) -> done;  %% 不会匹配因为 done 不是 map
           (Data) -> Data
        end),

    %% 初始问题
    io:format("问题1: 什么是Erlang的轻量级进程？~n~n"),
    P6 = beamai_process:set_initial_event(P5, start,
        #{user_message => <<"什么是Erlang的轻量级进程？"/utf8>>}),
    P7 = beamai_process:set_execution_mode(P6, sequential),

    {ok, Def} = beamai_process:build(P7),
    case beamai_process:run_sync(Def, #{timeout => 120000, context => Context}) of
        {ok, Result} ->
            #{chatter := ChatState} = Result,
            #{state := #{messages := History}} = ChatState,
            io:format("~n--- 完整对话历史 ---~n"),
            print_conversation(History),
            io:format("~n多轮对话完成! (共 ~p 轮)~n~n",
                     [maps:get(activation_count, ChatState)]);
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% Internal
%%====================================================================

ensure_started() ->
    case whereis(beamai_core_sup) of
        undefined ->
            {ok, _} = beamai_core_sup:start_link(),
            ok;
        _ ->
            ok
    end.

print_step_response(StepState) ->
    case StepState of
        #{state := #{type := llm_chat}} ->
            %% 从最后一次激活的事件中无法直接获取响应
            %% 但状态中保存了信息
            io:format("  (步骤已执行 ~p 次)~n", [maps:get(activation_count, StepState)]);
        #{state := #{type := llm_multiturn, messages := Msgs}} ->
            case lists:reverse(Msgs) of
                [#{role := assistant, content := Last} | _] ->
                    io:format("  ~ts~n", [Last]);
                _ ->
                    io:format("  (无响应)~n")
            end;
        _ ->
            io:format("  (步骤已执行 ~p 次)~n", [maps:get(activation_count, StepState)])
    end.

print_conversation([]) -> ok;
print_conversation([#{role := system} | Rest]) ->
    print_conversation(Rest);
print_conversation([#{role := user, content := Msg} | Rest]) ->
    io:format("  用户: ~ts~n", [Msg]),
    print_conversation(Rest);
print_conversation([#{role := assistant, content := Msg} | Rest]) ->
    io:format("  助手: ~ts~n~n", [Msg]),
    print_conversation(Rest);
print_conversation([_ | Rest]) ->
    print_conversation(Rest).
