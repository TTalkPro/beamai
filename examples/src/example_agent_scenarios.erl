%%%-------------------------------------------------------------------
%%% @doc Agent 四种典型场景示例
%%%
%%% 按 Memory × Plugin 两个维度组合，演示 beamai_agent 四种用法：
%%%
%%%   1. 无 Memory + 无 Plugin — 最简对话
%%%   2. 无 Memory + 有 Plugin — 带工具调用的对话
%%%   3. 有 Memory + 无 Plugin — 可持久化/恢复的对话
%%%   4. 有 Memory + 有 Plugin — 完整功能（持久化 + 工具调用）
%%%
%%% 使用方法：
%%% ```
%%% cd examples
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%%
%%% %% Mock 版本（无需 API Key）
%%% example_agent_scenarios:no_memory_no_plugin().
%%% example_agent_scenarios:no_memory_with_plugin().
%%% example_agent_scenarios:with_memory_no_plugin().
%%% example_agent_scenarios:with_memory_with_plugin().
%%%
%%% %% 真实 LLM 版本（需设置 API Key）
%%% example_agent_scenarios:no_memory_no_plugin_live().
%%% example_agent_scenarios:no_memory_with_plugin_live().
%%% example_agent_scenarios:with_memory_no_plugin_live().
%%% example_agent_scenarios:with_memory_with_plugin_live().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_agent_scenarios).

-export([
    %% Mock 版本
    no_memory_no_plugin/0,
    no_memory_with_plugin/0,
    with_memory_no_plugin/0,
    with_memory_with_plugin/0,
    %% Live 版本
    no_memory_no_plugin_live/0,
    no_memory_with_plugin_live/0,
    with_memory_no_plugin_live/0,
    with_memory_with_plugin_live/0
]).

%%====================================================================
%% 场景 1：无 Memory + 无 Plugin
%%
%% 最简用法：创建 agent → 对话 → 获取结果
%% 状态仅在内存中，进程结束后丢失
%%====================================================================

%% @doc 无 Memory 无 Plugin（Mock）
-spec no_memory_no_plugin() -> ok.
no_memory_no_plugin() ->
    io:format("~n=== 场景 1: 无 Memory + 无 Plugin (Mock) ===~n~n"),

    %% 创建 agent：只需指定 LLM
    {ok, Agent0} = beamai_agent:new(#{
        llm => {mock, #{}},
        system_prompt => <<"你是一个友好的助手，请用简短的话回答。"/utf8>>
    }),

    %% 第一轮对话
    io:format("User: 你好，我是小明~n"),
    {ok, R1, Agent1} = beamai_agent:run(Agent0, <<"你好，我是小明"/utf8>>),
    io:format("Assistant: ~ts~n", [maps:get(content, R1)]),
    io:format("  turn_count=~p, messages=~p~n~n",
              [beamai_agent:turn_count(Agent1), length(beamai_agent:messages(Agent1))]),

    %% 第二轮对话（Agent1 已包含历史消息）
    io:format("User: 我叫什么名字？~n"),
    {ok, R2, Agent2} = beamai_agent:run(Agent1, <<"我叫什么名字？"/utf8>>),
    io:format("Assistant: ~ts~n", [maps:get(content, R2)]),
    io:format("  turn_count=~p, messages=~p~n~n",
              [beamai_agent:turn_count(Agent2), length(beamai_agent:messages(Agent2))]),

    %% 第三轮
    io:format("User: 总结一下我们的对话~n"),
    {ok, R3, Agent3} = beamai_agent:run(Agent2, <<"总结一下我们的对话"/utf8>>),
    io:format("Assistant: ~ts~n", [maps:get(content, R3)]),
    io:format("  turn_count=~p, messages=~p~n",
              [beamai_agent:turn_count(Agent3), length(beamai_agent:messages(Agent3))]),

    io:format("~n注意：进程结束后所有对话状态丢失~n"),
    ok.

%% @doc 无 Memory 无 Plugin（Live LLM）
-spec no_memory_no_plugin_live() -> ok.
no_memory_no_plugin_live() ->
    io:format("~n=== 场景 1: 无 Memory + 无 Plugin (Live) ===~n~n"),

    {ok, Agent0} = beamai_agent:new(#{
        llm => example_llm_config:anthropic(),
        system_prompt => <<"你是一个友好的助手，请用简短的一两句话回答。"/utf8>>
    }),

    Turns = [
        <<"你好，我叫小明，我是一个 Erlang 程序员"/utf8>>,
        <<"请问我刚才说我做什么工作？"/utf8>>,
        <<"给我推荐一本适合我的书"/utf8>>
    ],

    lists:foldl(fun(Msg, Acc) ->
        io:format("User: ~ts~n", [Msg]),
        case beamai_agent:run(Acc, Msg) of
            {ok, Result, NewAgent} ->
                io:format("Assistant: ~ts~n~n", [maps:get(content, Result)]),
                NewAgent;
            {error, Reason} ->
                io:format("Error: ~p~n~n", [Reason]),
                Acc
        end
    end, Agent0, Turns),
    ok.

%%====================================================================
%% 场景 2：无 Memory + 有 Plugin
%%
%% Agent 可以调用工具完成任务
%% 适合一次性任务（如查询天气、执行计算等）
%%====================================================================

%% @doc 无 Memory 有 Plugin（Mock）
-spec no_memory_with_plugin() -> ok.
no_memory_with_plugin() ->
    io:format("~n=== 场景 2: 无 Memory + 有 Plugin (Mock) ===~n~n"),

    %% 方式一：通过 kernel 注册 plugin
    Kernel0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{}),
    K1 = beamai_kernel:add_service(Kernel0, LlmConfig),

    %% 注册计算器工具
    K2 = beamai_kernel:add_plugin(K1, <<"calculator">>, [
        #{name => <<"add">>,
          description => <<"Add two numbers">>,
          parameters => #{
              type => object,
              properties => #{
                  a => #{type => number, description => <<"First number">>},
                  b => #{type => number, description => <<"Second number">>}
              },
              required => [<<"a">>, <<"b">>]
          },
          handler => fun(Args, _Ctx) ->
              A = maps:get(a, Args, maps:get(<<"a">>, Args, 0)),
              B = maps:get(b, Args, maps:get(<<"b">>, Args, 0)),
              io:format("  [calculator] ~p + ~p = ~p~n", [A, B, A + B]),
              {ok, #{result => A + B}}
          end},
        #{name => <<"multiply">>,
          description => <<"Multiply two numbers">>,
          parameters => #{
              type => object,
              properties => #{
                  a => #{type => number, description => <<"First number">>},
                  b => #{type => number, description => <<"Second number">>}
              },
              required => [<<"a">>, <<"b">>]
          },
          handler => fun(Args, _Ctx) ->
              A = maps:get(a, Args, maps:get(<<"a">>, Args, 0)),
              B = maps:get(b, Args, maps:get(<<"b">>, Args, 0)),
              io:format("  [calculator] ~p * ~p = ~p~n", [A, B, A * B]),
              {ok, #{result => A * B}}
          end}
    ]),

    %% 创建 agent（传入预构建的 kernel）
    {ok, Agent} = beamai_agent:new(#{
        kernel => K2,
        system_prompt => <<"You are a calculator assistant. Use tools to compute.">>,
        callbacks => #{
            on_tool_call => fun(Name, Args) ->
                io:format("  [callback] tool invoked: ~s~n", [Name]),
                io:format("  [callback] args: ~p~n", [Args])
            end
        }
    }),

    %% 查看可用工具
    Tools = beamai_kernel:get_tool_specs(beamai_agent:kernel(Agent)),
    io:format("Available tools: ~p~n~n", [[maps:get(name, T) || T <- Tools]]),

    %% 执行对话（mock LLM 不会触发 tool call，仅展示配置方式）
    io:format("User: What is 42 + 58?~n"),
    {ok, Result, _} = beamai_agent:run(Agent, <<"What is 42 + 58?">>),
    io:format("Assistant: ~ts~n", [maps:get(content, Result)]),
    io:format("Tool calls: ~p~n~n", [maps:get(tool_calls_made, Result, [])]),

    io:format("注意：Mock LLM 不会触发 tool call，使用 live 版本可看到完整效果~n"),
    ok.

%% @doc 无 Memory 有 Plugin（Live LLM）
-spec no_memory_with_plugin_live() -> ok.
no_memory_with_plugin_live() ->
    io:format("~n=== 场景 2: 无 Memory + 有 Plugin (Live) ===~n~n"),

    %% 构建带天气和计算工具的 kernel
    Kernel0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_service(Kernel0, example_llm_config:anthropic()),
    K2 = beamai_kernel:add_plugin(K1, <<"tools">>, [
        #{name => <<"get_weather">>,
          description => <<"Get current weather for a city">>,
          parameters => #{
              type => object,
              properties => #{
                  city => #{type => string, description => <<"City name">>}
              },
              required => [<<"city">>]
          },
          handler => fun(Args, _Ctx) ->
              City = maps:get(city, Args, maps:get(<<"city">>, Args, <<"unknown">>)),
              io:format("  [tool] get_weather(~ts)~n", [City]),
              {ok, mock_weather(City)}
          end},
        #{name => <<"calculate">>,
          description => <<"Evaluate a math expression">>,
          parameters => #{
              type => object,
              properties => #{
                  expression => #{type => string, description => <<"Math expression">>}
              },
              required => [<<"expression">>]
          },
          handler => fun(Args, _Ctx) ->
              Expr = maps:get(expression, Args, maps:get(<<"expression">>, Args, <<"0">>)),
              io:format("  [tool] calculate(~s)~n", [Expr]),
              {ok, #{expression => Expr, result => <<"42">>}}
          end}
    ]),

    {ok, Agent} = beamai_agent:new(#{
        kernel => K2,
        system_prompt => <<"You are a helpful assistant with weather and math tools. "
                          "Always use tools when appropriate. Be concise.">>
    }),

    %% 第一轮：触发天气工具
    Q1 = <<"What's the weather in Tokyo?">>,
    io:format("User: ~s~n", [Q1]),
    case beamai_agent:run(Agent, Q1) of
        {ok, R1, Agent1} ->
            io:format("Assistant: ~ts~n", [maps:get(content, R1)]),
            io:format("  tool_calls: ~p, iterations: ~p~n~n",
                      [length(maps:get(tool_calls_made, R1, [])),
                       maps:get(iterations, R1, 0)]),

            %% 第二轮：触发计算工具
            Q2 = <<"If the temperature is 22°C, what is it in Fahrenheit? Use the calculate tool.">>,
            io:format("User: ~s~n", [Q2]),
            case beamai_agent:run(Agent1, Q2) of
                {ok, R2, _} ->
                    io:format("Assistant: ~ts~n", [maps:get(content, R2)]),
                    io:format("  tool_calls: ~p~n",
                              [length(maps:get(tool_calls_made, R2, []))]);
                {error, Reason2} ->
                    io:format("Error: ~p~n", [Reason2])
            end;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% 场景 3：有 Memory + 无 Plugin
%%
%% 对话可持久化到 store，进程重启后可恢复继续对话
%% 适合需要长期维护上下文的对话场景
%%====================================================================

%% @doc 有 Memory 无 Plugin（Mock）
-spec with_memory_no_plugin() -> ok.
with_memory_no_plugin() ->
    io:format("~n=== 场景 3: 有 Memory + 无 Plugin (Mock) ===~n~n"),

    %% Step 1: 创建 Store 和 Memory
    StoreName = example_mem_store,
    case beamai_store_ets:start_link(StoreName, #{}) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"example-session-001">>
    }),

    %% Agent 配置（save 和 restore 都使用此配置）
    AgentConfig = #{
        llm => {mock, #{}},
        system_prompt => <<"你是一个有记忆的助手。"/utf8>>,
        memory => Memory,
        auto_save => true  %% 每轮结束自动保存
    },

    io:format("Step 1: 创建 Agent 并进行两轮对话~n"),
    {ok, Agent0} = beamai_agent:new(AgentConfig),
    {ok, R1, Agent1} = beamai_agent:run(Agent0, <<"Hello, I'm Alice">>),
    io:format("  Turn 1 - Assistant: ~ts~n", [maps:get(content, R1)]),
    {ok, R2, Agent2} = beamai_agent:run(Agent1, <<"Remember my name">>),
    io:format("  Turn 2 - Assistant: ~ts~n", [maps:get(content, R2)]),
    io:format("  State: turn_count=~p, messages=~p~n~n",
              [beamai_agent:turn_count(Agent2), length(beamai_agent:messages(Agent2))]),

    %% Step 2: 模拟进程重启 —— 丢弃变量，从 memory 恢复
    io:format("Step 2: 模拟重启 — 从 Memory 恢复 Agent~n"),
    {ok, RestoredAgent} = beamai_agent:restore(AgentConfig, Memory),
    io:format("  恢复成功！~n"),
    io:format("  turn_count=~p, messages=~p~n",
              [beamai_agent:turn_count(RestoredAgent),
               length(beamai_agent:messages(RestoredAgent))]),

    %% Step 3: 在恢复的 agent 上继续对话
    io:format("~nStep 3: 恢复后继续对话~n"),
    {ok, R3, Agent3} = beamai_agent:run(RestoredAgent, <<"What was my name?">>),
    io:format("  Turn 3 - Assistant: ~ts~n", [maps:get(content, R3)]),
    io:format("  turn_count=~p, messages=~p~n~n",
              [beamai_agent:turn_count(Agent3), length(beamai_agent:messages(Agent3))]),

    %% Step 4: 手动保存（auto_save=false 时使用）
    io:format("Step 4: 手动保存~n"),
    ok = beamai_agent:save(Agent3),
    io:format("  保存成功！~n"),

    %% Cleanup
    beamai_store_ets:stop(StoreName),
    io:format("~nDemo 完成。数据已从内存清除。~n"),
    io:format("提示：使用 beamai_store_sqlite 可实现磁盘持久化~n"),
    ok.

%% @doc 有 Memory 无 Plugin（Live LLM）
-spec with_memory_no_plugin_live() -> ok.
with_memory_no_plugin_live() ->
    io:format("~n=== 场景 3: 有 Memory + 无 Plugin (Live) ===~n~n"),

    StoreName = example_mem_live_store,
    case beamai_store_ets:start_link(StoreName, #{}) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"live-session-001">>
    }),

    AgentConfig = #{
        llm => example_llm_config:anthropic(),
        system_prompt => <<"你是一个有记忆的助手。请简洁回答。"/utf8>>,
        memory => Memory,
        auto_save => true
    },

    io:format("--- 第一段对话 ---~n"),
    {ok, Agent0} = beamai_agent:new(AgentConfig),
    Msgs1 = [
        <<"我叫小明，今年25岁，在北京工作"/utf8>>,
        <<"我的爱好是写 Erlang 代码"/utf8>>
    ],
    _Agent1 = lists:foldl(fun(Msg, Acc) ->
        io:format("User: ~ts~n", [Msg]),
        case beamai_agent:run(Acc, Msg) of
            {ok, R, NewA} ->
                io:format("Assistant: ~ts~n~n", [maps:get(content, R)]),
                NewA;
            {error, Err} ->
                io:format("Error: ~p~n", [Err]),
                Acc
        end
    end, Agent0, Msgs1),

    io:format("--- 模拟重启，从 Memory 恢复 ---~n~n"),
    {ok, RestoredAgent} = beamai_agent:restore(AgentConfig, Memory),
    io:format("恢复成功: turn_count=~p~n~n", [beamai_agent:turn_count(RestoredAgent)]),

    %% 测试 LLM 是否记得之前的上下文
    Q = <<"请总结一下你知道的关于我的信息"/utf8>>,
    io:format("User: ~ts~n", [Q]),
    case beamai_agent:run(RestoredAgent, Q) of
        {ok, Result, _} ->
            io:format("Assistant: ~ts~n", [maps:get(content, Result)]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,

    beamai_store_ets:stop(StoreName),
    ok.

%%====================================================================
%% 场景 4：有 Memory + 有 Plugin
%%
%% 完整功能：可持久化 + 工具调用
%% 适合需要长期运行且具备工具能力的 Agent
%%====================================================================

%% @doc 有 Memory 有 Plugin（Mock）
-spec with_memory_with_plugin() -> ok.
with_memory_with_plugin() ->
    io:format("~n=== 场景 4: 有 Memory + 有 Plugin (Mock) ===~n~n"),

    %% 创建 Store 和 Memory
    StoreName = example_full_store,
    case beamai_store_ets:start_link(StoreName, #{}) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"full-agent-session">>
    }),

    %% 构建带工具的 kernel
    Kernel0 = beamai_kernel:new(),
    LlmConfig = beamai_chat_completion:create(mock, #{}),
    K1 = beamai_kernel:add_service(Kernel0, LlmConfig),
    K2 = beamai_kernel:add_plugin(K1, <<"notes">>, [
        #{name => <<"save_note">>,
          description => <<"Save a note with a title">>,
          parameters => #{
              type => object,
              properties => #{
                  title => #{type => string, description => <<"Note title">>},
                  content => #{type => string, description => <<"Note content">>}
              },
              required => [<<"title">>, <<"content">>]
          },
          handler => fun(Args, _Ctx) ->
              Title = maps:get(title, Args, maps:get(<<"title">>, Args, <<"untitled">>)),
              Content = maps:get(content, Args, maps:get(<<"content">>, Args, <<>>)),
              io:format("  [notes] Saved: ~ts -> ~ts~n", [Title, Content]),
              {ok, #{status => saved, title => Title}}
          end},
        #{name => <<"list_notes">>,
          description => <<"List all saved notes">>,
          parameters => #{type => object, properties => #{}},
          handler => fun(_Args, _Ctx) ->
              io:format("  [notes] Listing notes~n"),
              {ok, #{notes => [<<"Meeting notes">>, <<"TODO list">>]}}
          end}
    ]),

    %% Agent 配置
    AgentConfig = #{
        kernel => K2,
        memory => Memory,
        auto_save => true,
        system_prompt => <<"You are a note-taking assistant with save and list tools.">>,
        callbacks => #{
            on_tool_call => fun(Name, _Args) ->
                io:format("  [callback] tool: ~s~n", [Name])
            end,
            on_turn_end => fun(Meta) ->
                io:format("  [callback] turn ~p completed~n",
                          [maps:get(turn_count, Meta)])
            end
        }
    },

    %% 第一段对话
    io:format("--- Session 1: 创建 Agent 并对话 ---~n"),
    {ok, Agent0} = beamai_agent:new(AgentConfig),
    Tools = beamai_kernel:get_tool_specs(beamai_agent:kernel(Agent0)),
    io:format("Tools: ~p~n~n", [[maps:get(name, T) || T <- Tools]]),

    io:format("User: Save a note about our meeting~n"),
    {ok, R1, Agent1} = beamai_agent:run(Agent0, <<"Save a note about our meeting">>),
    io:format("Assistant: ~ts~n~n", [maps:get(content, R1)]),

    io:format("User: List my notes~n"),
    {ok, R2, _Agent2} = beamai_agent:run(Agent1, <<"List my notes">>),
    io:format("Assistant: ~ts~n~n", [maps:get(content, R2)]),

    %% 恢复并继续
    io:format("--- Session 2: 从 Memory 恢复 ---~n"),
    {ok, RestoredAgent} = beamai_agent:restore(AgentConfig, Memory),
    io:format("恢复成功: turn_count=~p~n~n", [beamai_agent:turn_count(RestoredAgent)]),

    io:format("User: What did we do earlier?~n"),
    {ok, R3, _} = beamai_agent:run(RestoredAgent, <<"What did we do earlier?">>),
    io:format("Assistant: ~ts~n", [maps:get(content, R3)]),

    beamai_store_ets:stop(StoreName),
    io:format("~nDemo 完成~n"),
    ok.

%% @doc 有 Memory 有 Plugin（Live LLM）
-spec with_memory_with_plugin_live() -> ok.
with_memory_with_plugin_live() ->
    io:format("~n=== 场景 4: 有 Memory + 有 Plugin (Live) ===~n~n"),

    %% 创建 Store 和 Memory
    StoreName = example_full_live_store,
    case beamai_store_ets:start_link(StoreName, #{}) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    {ok, Memory} = beamai_memory:new(#{
        context_store => {beamai_store_ets, StoreName},
        thread_id => <<"full-live-session">>
    }),

    %% 构建 kernel
    Kernel0 = beamai_kernel:new(),
    K1 = beamai_kernel:add_service(Kernel0, example_llm_config:anthropic()),
    K2 = beamai_kernel:add_plugin(K1, <<"tools">>, [
        #{name => <<"get_weather">>,
          description => <<"Get current weather for a city. Returns temperature and condition.">>,
          parameters => #{
              type => object,
              properties => #{
                  city => #{type => string, description => <<"City name (e.g. Beijing, Tokyo)">>}
              },
              required => [<<"city">>]
          },
          handler => fun(Args, _Ctx) ->
              City = maps:get(city, Args, maps:get(<<"city">>, Args, <<"unknown">>)),
              io:format("  [tool] get_weather(~ts)~n", [City]),
              {ok, mock_weather(City)}
          end},
        #{name => <<"remember_fact">>,
          description => <<"Store a fact for later recall. Use this when the user asks you to remember something.">>,
          parameters => #{
              type => object,
              properties => #{
                  fact => #{type => string, description => <<"The fact to remember">>}
              },
              required => [<<"fact">>]
          },
          handler => fun(Args, _Ctx) ->
              Fact = maps:get(fact, Args, maps:get(<<"fact">>, Args, <<>>)),
              io:format("  [tool] remember_fact: ~ts~n", [Fact]),
              {ok, #{stored => true, fact => Fact}}
          end}
    ]),

    AgentConfig = #{
        kernel => K2,
        memory => Memory,
        auto_save => true,
        system_prompt => <<"You are a helpful assistant with weather lookup and memory. "
                          "When asked to remember something, use the remember_fact tool. "
                          "Be concise in responses.">>
    },

    %% Session 1
    io:format("--- Session 1: 首次对话 ---~n~n"),
    {ok, Agent0} = beamai_agent:new(AgentConfig),

    Msgs1 = [
        <<"What's the weather in Beijing?">>,
        <<"Please remember that my favorite city is Beijing">>
    ],
    _Agent1 = lists:foldl(fun(Msg, Acc) ->
        io:format("User: ~s~n", [Msg]),
        case beamai_agent:run(Acc, Msg) of
            {ok, R, NewA} ->
                io:format("Assistant: ~ts~n", [maps:get(content, R)]),
                io:format("  (tools: ~p, iterations: ~p)~n~n",
                          [length(maps:get(tool_calls_made, R, [])),
                           maps:get(iterations, R, 0)]),
                NewA;
            {error, Err} ->
                io:format("Error: ~p~n", [Err]),
                Acc
        end
    end, Agent0, Msgs1),

    io:format("Session 1 完成, auto-saved~n~n"),

    %% Session 2: 恢复
    io:format("--- Session 2: 从 Memory 恢复并继续 ---~n~n"),
    {ok, RestoredAgent} = beamai_agent:restore(AgentConfig, Memory),
    io:format("恢复成功: turn_count=~p, messages=~p~n~n",
              [beamai_agent:turn_count(RestoredAgent),
               length(beamai_agent:messages(RestoredAgent))]),

    Msgs2 = [
        <<"What's my favorite city? And what's the weather there now?">>,
        <<"Compare it with Tokyo's weather">>
    ],
    lists:foldl(fun(Msg, Acc) ->
        io:format("User: ~s~n", [Msg]),
        case beamai_agent:run(Acc, Msg) of
            {ok, R, NewA} ->
                io:format("Assistant: ~ts~n", [maps:get(content, R)]),
                io:format("  (tools: ~p)~n~n",
                          [length(maps:get(tool_calls_made, R, []))]),
                NewA;
            {error, Err} ->
                io:format("Error: ~p~n", [Err]),
                Acc
        end
    end, RestoredAgent, Msgs2),

    beamai_store_ets:stop(StoreName),
    io:format("Demo 完成~n"),
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 模拟天气数据
mock_weather(<<"Beijing">>) ->
    #{city => <<"Beijing">>, temperature => 28, condition => <<"Sunny">>, humidity => 45};
mock_weather(<<"Tokyo">>) ->
    #{city => <<"Tokyo">>, temperature => 22, condition => <<"Rainy">>, humidity => 80};
mock_weather(<<"Shanghai">>) ->
    #{city => <<"Shanghai">>, temperature => 32, condition => <<"Cloudy">>, humidity => 75};
mock_weather(<<"New York">>) ->
    #{city => <<"New York">>, temperature => 18, condition => <<"Partly Cloudy">>, humidity => 60};
mock_weather(City) ->
    #{city => City, temperature => 20, condition => <<"Clear">>, humidity => 50}.
