%%%-------------------------------------------------------------------
%%% @doc Agent 使用示例
%%%
%%% 演示 beamai_agent 的三种核心用法：
%%%   - run_basic/0: 基础单轮对话（mock LLM）
%%%   - run_basic_live/0: 基础单轮对话（真实 LLM）
%%%   - run_multi_turn/0: 多轮对话，展示消息历史自动累积
%%%   - run_multi_turn_live/0: 多轮对话（真实 LLM）
%%%   - run_with_plugin/0: 使用自定义 Plugin（天气查询），展示 tool calling
%%%   - run_with_plugin_live/0: 使用 Plugin（真实 LLM）
%%%   - run_with_file_plugin/0: 使用内置 file plugin（真实 LLM）
%%%
%%% 使用方法:
%%% ```
%%% %% Mock 版本（无需 API Key）
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_agent:run_basic().
%%% example_agent:run_multi_turn().
%%% example_agent:run_with_plugin().
%%%
%%% %% 真实 LLM 版本
%%% export ZHIPU_API_KEY=your-api-key
%%% example_agent:run_basic_live().
%%% example_agent:run_multi_turn_live().
%%% example_agent:run_with_plugin_live().
%%% example_agent:run_with_file_plugin().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agent).

-export([
    run_basic/0, run_basic_live/0,
    run_multi_turn/0, run_multi_turn_live/0,
    run_with_plugin/0, run_with_plugin_live/0,
    run_with_file_plugin/0
]).

%%====================================================================
%% 示例 1：基础对话
%%====================================================================

%% @doc 基础单轮对话（使用 mock LLM，无需 API Key）
%%
%% 展示 Agent 最简单的用法：
%%   1. 用 new/1 创建 agent
%%   2. 用 run/2 发送用户消息
%%   3. 从 Result 中取出 content
-spec run_basic() -> ok.
run_basic() ->
    io:format("=== BeamAI Agent: Basic Conversation (Mock) ===~n~n"),

    %% 1. 创建 Agent
    {ok, Agent0} = beamai_agent:new(#{
        llm => {mock, #{}},
        system_prompt => <<"你是一个有帮助的助手。"/utf8>>,
        callbacks => #{
            on_turn_start => fun(Meta) ->
                io:format("  [callback] turn_start: turn_count=~p~n",
                          [maps:get(turn_count, Meta)])
            end,
            on_turn_end => fun(Meta) ->
                io:format("  [callback] turn_end: turn_count=~p~n",
                          [maps:get(turn_count, Meta)])
            end
        }
    }),

    %% 2. 发送消息
    io:format("User: Hello!~n"),
    {ok, Result, Agent1} = beamai_agent:run(Agent0, <<"Hello!">>),

    %% 3. 输出结果
    io:format("Assistant: ~ts~n~n", [maps:get(content, Result)]),
    io:format("Turn count: ~p~n", [beamai_agent:turn_count(Agent1)]),
    io:format("Messages in history: ~p~n", [length(beamai_agent:messages(Agent1))]),
    ok.

%% @doc 基础单轮对话（使用真实 LLM）
-spec run_basic_live() -> ok.
run_basic_live() ->
    io:format("=== BeamAI Agent: Basic Conversation (Live) ===~n~n"),

    LLMConfig = example_llm_config:anthropic(),
    {ok, Agent0} = beamai_agent:new(#{
        llm => LLMConfig,
        system_prompt => <<"你是一个有帮助的助手。请用简短的一两句话回答。"/utf8>>
    }),

    UserMsg = <<"你好！请介绍一下你自己。"/utf8>>,
    io:format("User: ~ts~n", [UserMsg]),

    case beamai_agent:run(Agent0, UserMsg) of
        {ok, Result, Agent1} ->
            io:format("Assistant: ~ts~n~n", [maps:get(content, Result)]),
            io:format("Turn count: ~p~n", [beamai_agent:turn_count(Agent1)]),
            io:format("Finish reason: ~ts~n", [maps:get(finish_reason, Result, <<>>)]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% 示例 2：多轮对话
%%====================================================================

%% @doc 多轮对话（使用 mock LLM）
%%
%% 展示 Agent 如何自动维护对话历史：
%%   - 每次 run/2 后消息自动累积
%%   - 新一轮对话自动带上历史消息
%%   - turn_count 自动递增
-spec run_multi_turn() -> ok.
run_multi_turn() ->
    io:format("=== BeamAI Agent: Multi-Turn Conversation (Mock) ===~n~n"),

    {ok, Agent0} = beamai_agent:new(#{
        llm => {mock, #{}},
        system_prompt => <<"你是一个知识丰富的助手。"/utf8>>
    }),

    %% 定义多轮问题
    Questions = [
        <<"Erlang 是什么？"/utf8>>,
        <<"它的主要优势是什么？"/utf8>>,
        <<"OTP 又是什么？"/utf8>>
    ],

    %% 逐轮对话
    FinalAgent = lists:foldl(fun(Question, AgentAcc) ->
        Turn = beamai_agent:turn_count(AgentAcc) + 1,
        io:format("--- Turn ~B ---~n", [Turn]),
        io:format("User: ~ts~n", [Question]),

        {ok, Result, NewAgent} = beamai_agent:run(AgentAcc, Question),
        io:format("Assistant: ~ts~n", [maps:get(content, Result)]),
        io:format("  (history: ~B messages)~n~n", [length(beamai_agent:messages(NewAgent))]),
        NewAgent
    end, Agent0, Questions),

    io:format("=== Summary ===~n"),
    io:format("Total turns: ~p~n", [beamai_agent:turn_count(FinalAgent)]),
    io:format("Total messages in history: ~p~n", [length(beamai_agent:messages(FinalAgent))]),
    io:format("Last response: ~ts~n", [beamai_agent:last_response(FinalAgent)]),
    ok.

%% @doc 多轮对话（使用真实 LLM）
%%
%% 演示真实 LLM 下的多轮对话效果，LLM 会根据历史上下文给出连贯回答。
-spec run_multi_turn_live() -> ok.
run_multi_turn_live() ->
    io:format("=== BeamAI Agent: Multi-Turn Conversation (Live) ===~n~n"),

    LLMConfig = example_llm_config:anthropic(),
    {ok, Agent0} = beamai_agent:new(#{
        llm => LLMConfig,
        system_prompt => <<"你是一个知识丰富的编程助手。请简洁回答，每次不超过两句话。"/utf8>>
    }),

    Questions = [
        <<"Erlang 是什么语言？"/utf8>>,
        <<"它跟 Elixir 是什么关系？"/utf8>>,
        <<"它们各自适合什么场景？"/utf8>>
    ],

    lists:foldl(fun(Question, AgentAcc) ->
        Turn = beamai_agent:turn_count(AgentAcc) + 1,
        io:format("--- Turn ~B ---~n", [Turn]),
        io:format("User: ~ts~n", [Question]),

        case beamai_agent:run(AgentAcc, Question) of
            {ok, Result, NewAgent} ->
                io:format("Assistant: ~ts~n~n", [maps:get(content, Result)]),
                NewAgent;
            {error, Reason} ->
                io:format("Error: ~p~n~n", [Reason]),
                AgentAcc
        end
    end, Agent0, Questions),

    io:format("=== Conversation ended ===~n"),
    ok.

%%====================================================================
%% 示例 3：使用 Plugin（工具调用）
%%====================================================================

%% @doc 使用自定义天气 Plugin（mock LLM + mock 工具调用）
%%
%% 展示 Agent + Plugin 的集成方式：
%%   - 创建自定义 Plugin（天气查询）
%%   - Agent 通过 kernel 自动获取 tool specs
%%   - LLM 返回 tool_calls 时 Agent 自动执行并循环
%%   - on_tool_call 回调监控工具调用过程
%%
%% 注意：mock LLM 不会实际产生 tool_calls，此示例展示的是配置方式。
%% 使用 run_with_plugin_live/0 可看到真实的 tool calling 效果。
-spec run_with_plugin() -> ok.
run_with_plugin() ->
    io:format("=== BeamAI Agent: Plugin Usage (Mock) ===~n~n"),

    %% 1. 构建带自定义 Tool 的 Kernel
    Kernel0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{}),
    K1 = beamai_kernel:add_service(Kernel0, LlmConfig),
    K2 = beamai_kernel:add_tool(K1,
        #{name => <<"get_weather">>,
          description => <<"Get current weather for a city">>,
          tag => <<"weather">>,
          parameters => #{
              <<"city">> => #{type => string,
                            description => <<"City name">>,
                            required => true}
          },
          handler => fun(Args, _Ctx) ->
              City = maps:get(<<"city">>, Args, maps:get(city, Args, <<"unknown">>)),
              io:format("  [weather tool] Querying weather for: ~ts~n", [City]),
              {ok, #{city => City, temp => 25, condition => <<"Sunny">>}}
          end}),

    %% 2. 使用预构建 Kernel 创建 Agent
    {ok, Agent0} = beamai_agent:new(#{
        kernel => K2,
        system_prompt => <<"You are a helpful weather assistant.">>,
        callbacks => #{
            on_tool_call => fun(Name, Args) ->
                io:format("  [callback] on_tool_call: ~s(~p)~n", [Name, Args])
            end,
            on_llm_call => fun(Msgs, _Meta) ->
                io:format("  [callback] on_llm_call: ~B messages~n", [length(Msgs)])
            end
        }
    }),

    %% 3. 查看注册的工具
    RegisteredTools = beamai_kernel:get_tool_specs(beamai_agent:kernel(Agent0)),
    io:format("Registered tools: ~p~n~n", [[maps:get(name, T) || T <- RegisteredTools]]),

    %% 4. 发送对话（mock LLM 会返回固定文本，不会触发 tool call）
    io:format("User: What's the weather in Beijing?~n"),
    {ok, Result, _Agent1} = beamai_agent:run(Agent0, <<"What's the weather in Beijing?">>),
    io:format("Assistant: ~ts~n~n", [maps:get(content, Result)]),
    io:format("Tool calls made: ~p~n", [maps:get(tool_calls_made, Result, [])]),
    ok.

%% @doc 使用自定义天气 Plugin（真实 LLM，会产生 tool calling）
%%
%% 真实 LLM 会识别出需要调用天气工具，返回 tool_calls。
%% Agent 自动执行工具、拼接结果并再次请求 LLM，直到得到最终文本回复。
-spec run_with_plugin_live() -> ok.
run_with_plugin_live() ->
    io:format("=== BeamAI Agent: Plugin Usage (Live) ===~n~n"),

    %% 1. 构建 Kernel：LLM + 自定义天气 Tool
    LLMConfig = example_llm_config:anthropic(),
    Kernel0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_service(Kernel0, LLMConfig),
    K2 = beamai_kernel:add_tool(K1,
        #{name => <<"get_current_weather">>,
          description => <<"Get the current weather for a given city. Returns temperature, condition, and humidity.">>,
          tag => <<"weather">>,
          parameters => #{
              <<"city">> => #{type => string,
                            description => <<"City name, e.g. Beijing, Tokyo, New York">>,
                            required => true}
          },
          handler => fun(Args, _Ctx) ->
              City = maps:get(<<"city">>, Args, maps:get(city, Args, <<"unknown">>)),
              io:format("  [tool executed] get_current_weather(~ts)~n", [City]),
              Weather = mock_weather(City),
              {ok, Weather}
          end}),

    %% 2. 创建 Agent
    {ok, Agent0} = beamai_agent:new(#{
        kernel => K2,
        system_prompt => <<"You are a helpful weather assistant. "
                          "You MUST use the get_current_weather tool to answer weather questions. "
                          "Never guess the weather.">>,
        callbacks => #{
            on_tool_call => fun(Name, Args) ->
                io:format("  [callback] tool_call: ~s ~p~n", [Name, Args])
            end,
            on_llm_call => fun(_Msgs, _Meta) ->
                io:format("  [callback] calling LLM...~n")
            end
        }
    }),

    %% 3. 单城市查询
    io:format("--- Query 1: Single city ---~n"),
    Q1 = <<"What's the weather like in Beijing today?">>,
    io:format("User: ~s~n", [Q1]),
    case beamai_agent:run(Agent0, Q1) of
        {ok, R1, Agent1} ->
            io:format("Assistant: ~ts~n", [maps:get(content, R1)]),
            io:format("  (iterations: ~p, tool_calls: ~p)~n~n",
                      [maps:get(iterations, R1, 0), length(maps:get(tool_calls_made, R1, []))]),

            %% 4. 多城市查询（第二轮，带上历史）
            io:format("--- Query 2: Compare cities (with history) ---~n"),
            Q2 = <<"How about Tokyo? Is it warmer or cooler than Beijing?">>,
            io:format("User: ~s~n", [Q2]),
            case beamai_agent:run(Agent1, Q2) of
                {ok, R2, Agent2} ->
                    io:format("Assistant: ~ts~n", [maps:get(content, R2)]),
                    io:format("~n=== Summary ===~n"),
                    io:format("Total turns: ~p~n", [beamai_agent:turn_count(Agent2)]),
                    io:format("Messages in history: ~p~n", [length(beamai_agent:messages(Agent2))]);
                {error, Reason2} ->
                    io:format("Error: ~p~n", [Reason2])
            end;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%% @doc 使用内置 file 工具模块（真实 LLM）
%%
%% 展示如何通过 plugins 选项直接加载内置工具模块。
%% beamai_tool_file 提供 file_read, file_write, file_glob, file_list 等工具。
-spec run_with_file_plugin() -> ok.
run_with_file_plugin() ->
    io:format("=== BeamAI Agent: Built-in File Tools (Live) ===~n~n"),

    LLMConfig = example_llm_config:anthropic(),
    {ok, Agent0} = beamai_agent:new(#{
        llm => LLMConfig,
        plugins => [beamai_tool_file],
        system_prompt => <<"You are a helpful assistant with file system access. "
                          "Use the file tools to help users explore files. "
                          "Be concise in your responses.">>,
        max_tool_iterations => 5,
        callbacks => #{
            on_tool_call => fun(Name, Args) ->
                io:format("  [tool] ~s(~s)~n", [Name, format_args(Args)])
            end
        }
    }),

    %% 列出注册的工具
    Tools = beamai_kernel:get_tool_specs(beamai_agent:kernel(Agent0)),
    io:format("Available tools: ~p~n~n", [[maps:get(name, T) || T <- Tools]]),

    %% 让 Agent 查看当前目录
    Q = <<"List the .erl files in the current directory (src/ folder).">>,
    io:format("User: ~s~n", [Q]),
    case beamai_agent:run(Agent0, Q) of
        {ok, Result, _Agent1} ->
            io:format("Assistant: ~ts~n~n", [maps:get(content, Result)]),
            io:format("Tool calls made: ~p~n", [length(maps:get(tool_calls_made, Result, []))]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 模拟天气数据
mock_weather(<<"Beijing">>) ->
    #{city => <<"Beijing">>, temperature => 28, condition => <<"Sunny">>, humidity => 45};
mock_weather(<<"北京"/utf8>>) ->
    #{city => <<"Beijing">>, temperature => 28, condition => <<"Sunny">>, humidity => 45};
mock_weather(<<"Tokyo">>) ->
    #{city => <<"Tokyo">>, temperature => 22, condition => <<"Rainy">>, humidity => 80};
mock_weather(<<"东京"/utf8>>) ->
    #{city => <<"Tokyo">>, temperature => 22, condition => <<"Rainy">>, humidity => 80};
mock_weather(<<"Shanghai">>) ->
    #{city => <<"Shanghai">>, temperature => 32, condition => <<"Cloudy">>, humidity => 75};
mock_weather(<<"New York">>) ->
    #{city => <<"New York">>, temperature => 18, condition => <<"Partly Cloudy">>, humidity => 60};
mock_weather(City) ->
    #{city => City, temperature => 20, condition => <<"Unknown">>, humidity => 50}.

%% @private 格式化 Args 用于日志
format_args(Args) when is_map(Args) ->
    Pairs = maps:to_list(Args),
    Formatted = lists:map(fun({K, V}) ->
        io_lib:format("~s=~ts", [K, to_string(V)])
    end, Pairs),
    lists:flatten(lists:join(", ", Formatted));
format_args(_) ->
    "".

to_string(V) when is_binary(V) -> V;
to_string(V) when is_integer(V) -> integer_to_binary(V);
to_string(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_string(V) -> iolist_to_binary(io_lib:format("~p", [V])).
