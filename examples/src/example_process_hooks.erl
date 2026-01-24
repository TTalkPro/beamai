%%%-------------------------------------------------------------------
%%% @doc Process Framework Hooks 示例
%%%
%%% 演示 Process Framework 中各阶段的钩子机制：
%%%   1. Pre-activation hook: 输入验证/变换
%%%   2. Post-activation hook: 输出格式化/审计
%%%   3. Error hook: 错误处理和恢复
%%%   4. Logging hook: 全流程日志记录
%%%
%%% 使用方法:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_process_hooks:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_process_hooks).

-export([run/0, run/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 运行 Hooks 示例
-spec run() -> ok.
run() ->
    run(example_llm_config:anthropic()).

-spec run(beamai_chat_completion:config()) -> ok.
run(LLMConfig) ->
    io:format("~n=== Process Framework: Hooks 示例 ===~n~n"),
    io:format("演示: 输入验证 -> 日志 -> LLM处理 -> 日志 -> 输出格式化~n~n"),

    ensure_started(),

    Kernel = beamai:add_llm(beamai:kernel(), LLMConfig),
    Context = beamai_context:with_kernel(beamai_context:new(), Kernel),

    P0 = beamai_process:builder(hooks_demo),

    %% Hook 1: Pre-activation - 输入验证和清理
    P1 = beamai_process:add_step(P0, input_validator, example_process_steps, #{
        type => hook,
        phase => pre,
        hook_fn => fun validate_input/2,
        output_event => validated
    }),

    %% Hook 2: Pre-logging - 记录输入
    P2 = beamai_process:add_step(P1, pre_log, example_process_steps, #{
        type => log,
        label => <<"PRE-LLM">>,
        output_event => pre_logged
    }),

    %% 主步骤: LLM 对话
    P3 = beamai_process:add_step(P2, llm_step, example_process_steps, #{
        type => llm_chat,
        system_prompt => <<"你是一个简洁的问答助手。用一句话回答问题。"/utf8>>,
        output_event => llm_done
    }),

    %% Hook 3: Post-logging - 记录输出
    P4 = beamai_process:add_step(P3, post_log, example_process_steps, #{
        type => log,
        label => <<"POST-LLM">>,
        output_event => post_logged
    }),

    %% Hook 4: Post-activation - 输出格式化
    P5 = beamai_process:add_step(P4, output_formatter, example_process_steps, #{
        type => hook,
        phase => post,
        hook_fn => fun format_output/2,
        output_event => formatted
    }),

    %% Hook 5: 审计记录
    P6 = beamai_process:add_step(P5, audit, example_process_steps, #{
        type => hook,
        phase => post,
        hook_fn => fun audit_log/2,
        output_event => audit_done
    }),

    %% 事件绑定: 线性管线
    P7 = beamai_process:on_event(P6, start, input_validator, input),
    P8 = beamai_process:on_event(P7, validated, pre_log, input),
    P9 = beamai_process:on_event(P8, pre_logged, llm_step, input,
        fun(Data) -> #{user_message => maps:get(user_message, Data, <<"?">>)} end),
    P10 = beamai_process:on_event(P9, llm_done, post_log, input),
    P11 = beamai_process:on_event(P10, post_logged, output_formatter, input),
    P12 = beamai_process:on_event(P11, formatted, audit, input),

    %% 初始事件 - 带有需要清理的输入
    UserInput = <<"   Erlang 的 OTP 是什么？  "/utf8>>,
    io:format("原始输入: \"~ts\"~n~n", [UserInput]),
    P13 = beamai_process:set_initial_event(P12, start, #{
        user_message => UserInput,
        timestamp => erlang:system_time(millisecond),
        source => <<"user_cli">>
    }),
    P14 = beamai_process:set_execution_mode(P13, sequential),

    {ok, Def} = beamai_process:build(P14),
    case beamai_process:run_sync(Def, #{timeout => 60000, context => Context}) of
        {ok, _Result} ->
            io:format("~n所有 Hooks 执行完成!~n~n");
        {error, Reason} ->
            io:format("错误: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% Hook Functions
%%====================================================================

%% @doc 输入验证 Hook
%% - 去除首尾空白
%% - 检查消息长度
%% - 添加元数据
validate_input(Data, _State) ->
    UserMsg = maps:get(user_message, Data, <<>>),
    %% 去除首尾空白
    Trimmed = string:trim(UserMsg),
    TrimmedBin = unicode:characters_to_binary(Trimmed),
    io:format("    验证: 原始长度=~p, 清理后长度=~p~n",
             [byte_size(UserMsg), byte_size(TrimmedBin)]),
    case byte_size(TrimmedBin) of
        0 ->
            {error, empty_message};
        Len when Len > 1000 ->
            {error, message_too_long};
        _ ->
            {ok, Data#{
                user_message => TrimmedBin,
                validated_at => erlang:system_time(millisecond),
                original_length => byte_size(UserMsg)
            }}
    end.

%% @doc 输出格式化 Hook
%% - 添加格式化标签
%% - 记录处理时间
format_output(Data, _State) ->
    Response = maps:get(response, Data, <<>>),
    io:format("    格式化: 响应长度=~p bytes~n", [byte_size(Response)]),
    {ok, #{
        formatted_response => <<"[AI回答] ", Response/binary>>,
        response_length => byte_size(Response),
        formatted_at => erlang:system_time(millisecond)
    }}.

%% @doc 审计日志 Hook
%% - 记录完整处理链路
audit_log(Data, _State) ->
    io:format("    审计: 处理完成 - 响应长度=~p, 时间=~p~n",
             [maps:get(response_length, Data, 0),
              maps:get(formatted_at, Data, 0)]),
    {ok, Data#{audited => true}}.

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
