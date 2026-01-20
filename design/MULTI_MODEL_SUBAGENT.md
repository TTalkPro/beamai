# 多模型 SubAgent 协作设计

English | [中文](#中文版本)

## Overview

This design explores how to use multiple different LLM models within the SubAgent architecture to accomplish specialized tasks. Each SubAgent can be configured with the most suitable model for its specific task type.

## Status

- **Status**: Proposed
- **Created**: 2026-01-20
- **Author**: Design Discussion

---

## Core Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                Multi-Model SubAgent Architecture             │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   ┌─────────────────────────────────────────────────────┐  │
│   │              Agent Registry                          │  │
│   │                                                     │  │
│   │  ┌─────────────┬─────────────┬─────────────┐       │  │
│   │  │ code_agent  │ write_agent │ data_agent  │       │  │
│   │  │ Claude      │ GPT-4       │ DeepSeek    │       │  │
│   │  │ Code Expert │ Writing     │ Data        │       │  │
│   │  └─────────────┴─────────────┴─────────────┘       │  │
│   │                                                     │  │
│   │  ┌─────────────┬─────────────┬─────────────┐       │  │
│   │  │ trans_agent │ fast_agent  │reason_agent │       │  │
│   │  │ Qwen        │ GPT-4o-mini │ DeepSeek-R1 │       │  │
│   │  │ Translation │ Quick Tasks │ Reasoning   │       │  │
│   │  └─────────────┴─────────────┴─────────────┘       │  │
│   └─────────────────────────────────────────────────────┘  │
│                          │                                  │
│                          ▼                                  │
│   ┌─────────────────────────────────────────────────────┐  │
│   │              Orchestrator                            │  │
│   │                                                     │  │
│   │  Planner LLM: "Analyze task, select suitable Agent" │  │
│   │       │                                              │  │
│   │       ▼                                              │  │
│   │  Task Router: Route to corresponding Agent          │  │
│   └───────────────────┬─────────────────────────────────┘  │
│                       │                                     │
│        ┌──────────────┼──────────────┐                     │
│        ▼              ▼              ▼                     │
│   [code_agent]   [write_agent]  [data_agent]               │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## Approach 1: Predefined Agent Pool

The simplest approach - preconfigure a set of specialized Agents.

### Configuration

```erlang
Config = #{
    %% Orchestrator uses high-end model for decisions
    orchestrator_llm => llm_client:create(anthropic, #{
        model => <<"claude-sonnet-4-20250514">>
    }),

    %% Predefined specialized Agent pool
    agent_pool => #{
        %% Code Expert - Claude excels at code
        code => #{
            llm => llm_client:create(anthropic, #{
                model => <<"claude-sonnet-4-20250514">>
            }),
            tools => [read_file, write_file, run_tests],
            system_prompt => <<"You are a code expert...">>
        },

        %% Writing Expert - GPT excels at creative writing
        writing => #{
            llm => llm_client:create(openai, #{
                model => <<"gpt-4o">>
            }),
            tools => [search_web, read_file],
            system_prompt => <<"You are a writing expert...">>
        },

        %% Data Analysis - DeepSeek cost-effective
        data => #{
            llm => llm_client:create(deepseek, #{
                model => <<"deepseek-chat">>
            }),
            tools => [query_db, create_chart],
            system_prompt => <<"You are a data analysis expert...">>
        },

        %% Quick Tasks - cheap and fast model
        fast => #{
            llm => llm_client:create(openai, #{
                model => <<"gpt-4o-mini">>
            }),
            tools => [],
            system_prompt => <<"Complete simple tasks quickly...">>
        }
    }
}.
```

### Implementation

```erlang
%% beamai_agent_pool.erl
-module(beamai_agent_pool).

-export([create/1, get_agent/2, run_task/3, list_agents/1]).

-type pool() :: #{atom() => agent_state()}.
-type agent_state() :: map().

%% Create Agent pool
-spec create(map()) -> {ok, pool()} | {error, term()}.
create(Config) ->
    AgentConfigs = maps:get(agent_pool, Config, #{}),
    Pool = maps:map(fun(_Name, AgentConfig) ->
        %% Pre-create Agent state (don't start process)
        {ok, State} = beamai_agent:create_state(AgentConfig),
        State
    end, AgentConfigs),
    {ok, Pool}.

%% Get Agent by type
-spec get_agent(pool(), atom()) -> {ok, agent_state()} | {error, not_found}.
get_agent(Pool, Type) ->
    case maps:get(Type, Pool, undefined) of
        undefined -> {error, not_found};
        State -> {ok, State}
    end.

%% Execute task with specified Agent type
-spec run_task(pool(), atom(), binary()) -> {ok, map()} | {error, term()}.
run_task(Pool, AgentType, Task) ->
    case get_agent(Pool, AgentType) of
        {ok, AgentState} ->
            beamai_agent:run_with_state(AgentState, Task, #{});
        Error ->
            Error
    end.

%% List available Agents
-spec list_agents(pool()) -> [atom()].
list_agents(Pool) ->
    maps:keys(Pool).
```

### Advantages

- Simple to implement
- Predictable behavior
- Easy to configure

### Disadvantages

- Static configuration
- Manual selection required
- Limited flexibility

---

## Approach 2: Dynamic Agent Selection (LLM Decision)

Let the Orchestrator LLM decide which Agent to use.

### Orchestrator Configuration

```erlang
OrchestratorPrompt = <<"
You are a task dispatcher. Based on user tasks, select the most suitable
specialized Agent for execution.

Available Agents:
1. code_agent - Code writing, review, debugging (uses Claude)
2. writing_agent - Article writing, content creation (uses GPT-4)
3. data_agent - Data analysis, report generation (uses DeepSeek)
4. translation_agent - Multi-language translation (uses Qwen)
5. fast_agent - Simple quick tasks (uses GPT-4o-mini)
6. reasoning_agent - Complex reasoning problems (uses DeepSeek-R1)

Use the delegate_to_agent tool to assign tasks.
">>.
```

### Delegation Tool

```erlang
delegate_tool() -> #{
    name => <<"delegate_to_agent">>,
    description => <<"Delegate task to specialized Agent for execution">>,
    parameters => #{
        type => object,
        properties => #{
            <<"agent_type">> => #{
                type => string,
                enum => [<<"code">>, <<"writing">>, <<"data">>,
                        <<"translation">>, <<"fast">>, <<"reasoning">>],
                description => <<"Selected Agent type">>
            },
            <<"task">> => #{
                type => string,
                description => <<"Specific task description to execute">>
            },
            <<"context">> => #{
                type => string,
                description => <<"Background information for the task (optional)">>
            }
        },
        required => [<<"agent_type">>, <<"task">>]
    },
    handler => fun handle_delegate/2
}.

handle_delegate(Args, State) ->
    AgentType = maps:get(<<"agent_type">>, Args),
    Task = maps:get(<<"task">>, Args),
    Context = maps:get(<<"context">>, Args, <<>>),

    Pool = get_agent_pool(State),
    FullTask = format_task_with_context(Task, Context),

    case beamai_agent_pool:run_task(Pool, binary_to_atom(AgentType), FullTask) of
        {ok, Result} ->
            #{
                status => success,
                agent => AgentType,
                result => maps:get(final_response, Result, <<>>)
            };
        {error, Reason} ->
            #{status => error, reason => Reason}
    end.
```

### Advantages

- Intelligent selection
- Adapts to task requirements
- Single entry point

### Disadvantages

- Extra LLM call for routing
- May make suboptimal choices
- Harder to debug

---

## Approach 3: Capability-Based Matching (Recommended)

More intelligent approach - automatic matching based on capability tags.

### Data Structures

```erlang
%% Capability types
-type capability() ::
    code | writing | translation | data_analysis |
    reasoning | fast | creative | technical | math.

%% Agent entry record
-record(agent_entry, {
    name :: binary(),
    llm_config :: map(),
    capabilities :: [capability()],
    cost_per_1k :: float(),       %% Cost per 1K tokens
    avg_latency :: integer(),     %% Average latency in ms
    quality_score :: float(),     %% Quality score 0-1
    tools :: [atom()],
    system_prompt :: binary()
}).
```

### Agent Registry

```erlang
%% beamai_agent_registry.erl
-module(beamai_agent_registry).

-export([register/1, unregister/1, find_by_capability/2, select_best/2]).

%% Example agent registrations
default_agents() -> [
    #agent_entry{
        name = <<"claude_coder">>,
        llm_config = llm_client:create(anthropic, #{
            model => <<"claude-sonnet-4-20250514">>
        }),
        capabilities = [code, technical, reasoning],
        cost_per_1k = 0.003,
        avg_latency = 2000,
        quality_score = 0.95,
        tools = [read_file, write_file, run_command],
        system_prompt = <<"You are an expert programmer...">>
    },
    #agent_entry{
        name = <<"gpt_writer">>,
        llm_config = llm_client:create(openai, #{
            model => <<"gpt-4o">>
        }),
        capabilities = [writing, creative],
        cost_per_1k = 0.005,
        avg_latency = 1500,
        quality_score = 0.92,
        tools = [search_web],
        system_prompt = <<"You are a creative writer...">>
    },
    #agent_entry{
        name = <<"deepseek_analyst">>,
        llm_config = llm_client:create(deepseek, #{
            model => <<"deepseek-chat">>
        }),
        capabilities = [data_analysis, reasoning, code, math],
        cost_per_1k = 0.0001,  %% Very cheap
        avg_latency = 3000,
        quality_score = 0.88,
        tools = [query_db, create_chart, run_python],
        system_prompt = <<"You are a data analyst...">>
    },
    #agent_entry{
        name = <<"deepseek_reasoner">>,
        llm_config = llm_client:create(deepseek, #{
            model => <<"deepseek-reasoner">>
        }),
        capabilities = [reasoning, math, code],
        cost_per_1k = 0.0005,
        avg_latency = 5000,  %% Slower but more thorough
        quality_score = 0.96,
        tools = [],
        system_prompt = <<"You are a reasoning expert...">>
    },
    #agent_entry{
        name = <<"qwen_translator">>,
        llm_config = llm_client:create(ollama, #{
            model => <<"qwen2.5:14b">>
        }),
        capabilities = [translation, writing],
        cost_per_1k = 0.0,  %% Local, free
        avg_latency = 1000,
        quality_score = 0.85,
        tools = [],
        system_prompt = <<"You are a multilingual translator...">>
    },
    #agent_entry{
        name = <<"fast_helper">>,
        llm_config = llm_client:create(openai, #{
            model => <<"gpt-4o-mini">>
        }),
        capabilities = [fast],
        cost_per_1k = 0.00015,
        avg_latency = 500,
        quality_score = 0.75,
        tools = [],
        system_prompt = <<"Complete tasks quickly and concisely...">>
    }
].

%% Find agents with required capabilities
-spec find_by_capability([capability()], [#agent_entry{}]) -> [#agent_entry{}].
find_by_capability(RequiredCaps, Agents) ->
    lists:filter(fun(#agent_entry{capabilities = Caps}) ->
        lists:all(fun(C) -> lists:member(C, Caps) end, RequiredCaps)
    end, Agents).

%% Select best agent based on preferences
-spec select_best([#agent_entry{}], map()) -> #agent_entry{} | undefined.
select_best([], _Prefs) -> undefined;
select_best(Agents, Prefs) ->
    Sorted = case maps:get(optimize_for, Prefs, balanced) of
        cost -> lists:sort(fun(A, B) ->
            A#agent_entry.cost_per_1k =< B#agent_entry.cost_per_1k
        end, Agents);
        speed -> lists:sort(fun(A, B) ->
            A#agent_entry.avg_latency =< B#agent_entry.avg_latency
        end, Agents);
        quality -> lists:sort(fun(A, B) ->
            A#agent_entry.quality_score >= B#agent_entry.quality_score
        end, Agents);
        balanced -> sort_balanced(Agents)
    end,
    hd(Sorted).

%% Balanced sorting considering all factors
sort_balanced(Agents) ->
    lists:sort(fun(A, B) ->
        score(A) >= score(B)
    end, Agents).

score(#agent_entry{cost_per_1k = Cost, avg_latency = Latency, quality_score = Quality}) ->
    %% Higher is better
    Quality * 100 - (Cost * 1000) - (Latency / 100).
```

### Smart Task Routing

```erlang
%% beamai_task_router.erl
-module(beamai_task_router).

-export([route/2, analyze_task/1]).

%% Route task to best Agent
-spec route(binary(), map()) -> {ok, #agent_entry{}, binary()} | {error, term()}.
route(Task, Opts) ->
    %% Analyze task to determine required capabilities
    RequiredCaps = analyze_task(Task),

    %% Find matching agents
    AllAgents = beamai_agent_registry:default_agents(),
    Candidates = beamai_agent_registry:find_by_capability(RequiredCaps, AllAgents),

    case Candidates of
        [] ->
            %% Fallback to fast agent for unknown tasks
            FastAgent = find_agent_by_name(<<"fast_helper">>, AllAgents),
            {ok, FastAgent, Task};
        _ ->
            %% Select best based on preferences
            Prefs = maps:get(preferences, Opts, #{optimize_for => balanced}),
            Best = beamai_agent_registry:select_best(Candidates, Prefs),
            {ok, Best, Task}
    end.

%% Analyze task to determine capabilities needed
-spec analyze_task(binary()) -> [capability()].
analyze_task(Task) ->
    TaskLower = string:lowercase(Task),
    Caps = [],

    %% Simple keyword matching (can be enhanced with LLM)
    Caps1 = case contains_any(TaskLower, [<<"code">>, <<"function">>, <<"bug">>,
                                          <<"implement">>, <<"program">>]) of
        true -> [code | Caps];
        false -> Caps
    end,

    Caps2 = case contains_any(TaskLower, [<<"write">>, <<"article">>, <<"blog">>,
                                          <<"story">>, <<"content">>]) of
        true -> [writing | Caps1];
        false -> Caps1
    end,

    Caps3 = case contains_any(TaskLower, [<<"translate">>, <<"翻译">>, <<"translation">>]) of
        true -> [translation | Caps2];
        false -> Caps2
    end,

    Caps4 = case contains_any(TaskLower, [<<"data">>, <<"analyze">>, <<"chart">>,
                                          <<"report">>, <<"statistics">>]) of
        true -> [data_analysis | Caps3];
        false -> Caps3
    end,

    Caps5 = case contains_any(TaskLower, [<<"reason">>, <<"think">>, <<"why">>,
                                          <<"explain">>, <<"prove">>]) of
        true -> [reasoning | Caps4];
        false -> Caps4
    end,

    %% Default to fast if no specific capability detected
    case Caps5 of
        [] -> [fast];
        _ -> Caps5
    end.

contains_any(Text, Keywords) ->
    lists:any(fun(K) -> binary:match(Text, K) =/= nomatch end, Keywords).
```

### Advantages

- Automatic intelligent matching
- Considers cost/speed/quality tradeoffs
- Extensible capability system
- No extra LLM call for routing

### Disadvantages

- More complex implementation
- Keyword matching may be imprecise
- Needs maintenance of capability tags

---

## Approach 4: Hierarchical Agent Architecture

For complex tasks, use multiple levels of Agents.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                Hierarchical Agent Architecture               │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   Level 0: Orchestrator (Claude Opus - Strongest Reasoning) │
│   ┌─────────────────────────────────────────────────────┐  │
│   │  • Understand complex tasks                          │  │
│   │  • Create execution strategy                         │  │
│   │  • Assign to Level 1 Agents                         │  │
│   └───────────────────┬─────────────────────────────────┘  │
│                       │                                     │
│   Level 1: Specialist Agents (Claude Sonnet / GPT-4)       │
│   ┌───────────┬───────────┬───────────┐                    │
│   │  Project  │  Code     │  Doc      │                    │
│   │  Manager  │  Lead     │  Writer   │                    │
│   │           │           │           │                    │
│   │  Decompose│  Code     │  Doc      │                    │
│   │  tasks    │  architect│  planning │                    │
│   └─────┬─────┴─────┬─────┴─────┬─────┘                    │
│         │           │           │                          │
│   Level 2: Worker Agents (GPT-4o-mini / DeepSeek)          │
│   ┌─────┴─────┬─────┴─────┬─────┴─────┐                    │
│   │  Task 1   │  Task 2   │  Task 3   │                    │
│   │  Worker   │  Worker   │  Worker   │                    │
│   │  (cheap)  │  (cheap)  │  (cheap)  │                    │
│   └───────────┴───────────┴───────────┘                    │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Configuration

```erlang
%% Hierarchical configuration
HierarchyConfig = #{
    levels => [
        %% Level 0: Orchestrator
        #{
            level => 0,
            role => orchestrator,
            llm => llm_client:create(anthropic, #{
                model => <<"claude-opus-4-20250514">>  %% Strongest model
            }),
            system_prompt => <<"You are a strategic planner...">>,
            can_delegate_to => [1],  %% Can delegate to level 1
            max_agents => 1
        },

        %% Level 1: Specialists
        #{
            level => 1,
            role => specialist,
            agents => #{
                project_manager => #{
                    llm => llm_client:create(anthropic, #{
                        model => <<"claude-sonnet-4-20250514">>
                    }),
                    system_prompt => <<"You manage project execution...">>,
                    tools => [create_plan, track_progress]
                },
                code_lead => #{
                    llm => llm_client:create(anthropic, #{
                        model => <<"claude-sonnet-4-20250514">>
                    }),
                    system_prompt => <<"You design code architecture...">>,
                    tools => [read_file, design_api]
                },
                doc_writer => #{
                    llm => llm_client:create(openai, #{
                        model => <<"gpt-4o">>
                    }),
                    system_prompt => <<"You write documentation...">>,
                    tools => [read_file, write_file]
                }
            },
            can_delegate_to => [2]  %% Can delegate to level 2
        },

        %% Level 2: Workers
        #{
            level => 2,
            role => worker,
            default_llm => llm_client:create(openai, #{
                model => <<"gpt-4o-mini">>  %% Cheap and fast
            }),
            system_prompt => <<"You execute specific tasks efficiently...">>,
            max_agents => 10,
            can_delegate_to => []  %% Cannot delegate further
        }
    ]
}.
```

### Execution Flow

```erlang
%% beamai_hierarchy.erl
-module(beamai_hierarchy).

-export([execute/2]).

execute(Task, Config) ->
    %% Start with orchestrator
    Orchestrator = get_level_agent(0, Config),
    execute_with_agent(Orchestrator, Task, Config, 0).

execute_with_agent(Agent, Task, Config, CurrentLevel) ->
    case beamai_agent:run_with_state(Agent, Task, #{}) of
        {ok, #{delegate := Delegations}} ->
            %% Agent wants to delegate subtasks
            Results = execute_delegations(Delegations, Config, CurrentLevel),
            %% Return results to current agent for synthesis
            synthesize_results(Agent, Results, Config);
        {ok, Result} ->
            %% Direct result
            {ok, Result};
        Error ->
            Error
    end.

execute_delegations(Delegations, Config, CurrentLevel) ->
    NextLevel = CurrentLevel + 1,
    lists:map(fun(#{agent := AgentType, task := SubTask}) ->
        Agent = get_agent_at_level(NextLevel, AgentType, Config),
        execute_with_agent(Agent, SubTask, Config, NextLevel)
    end, Delegations).
```

### Advantages

- Clear responsibility separation
- Cost optimization (expensive models for planning, cheap for execution)
- Scales well for complex tasks
- Natural decomposition

### Disadvantages

- Higher latency (multiple levels)
- Complex coordination
- May over-engineer simple tasks

---

## Recommended Implementation Strategy

### Phase 1: Simple Pool (Short-term)

```erlang
%% Simple but practical configuration
Config = #{
    %% Decision layer
    orchestrator => #{
        llm => llm_client:create(anthropic, #{
            model => <<"claude-sonnet-4-20250514">>
        }),
        prompt => <<"Select appropriate Agent for task execution...">>
    },

    %% Execution layer - predefined Agent pool
    agents => #{
        code => #{
            llm => llm_client:create(anthropic, #{model => <<"claude-sonnet">>}),
            tools => [read_file, write_file, run_command]
        },
        write => #{
            llm => llm_client:create(openai, #{model => <<"gpt-4o">>}),
            tools => [search_web]
        },
        analyze => #{
            llm => llm_client:create(deepseek, #{model => <<"deepseek-chat">>}),
            tools => [query_db, create_chart]
        },
        fast => #{
            llm => llm_client:create(openai, #{model => <<"gpt-4o-mini">>}),
            tools => []
        }
    }
}.
```

### Phase 2: Capability Matching (Medium-term)

- Implement capability tag system
- Add automatic task analysis
- Support cost/speed/quality preferences

### Phase 3: Full Hierarchy (Long-term)

- Implement hierarchical architecture
- Support dynamic agent registration
- Add distributed execution support

---

## Model Selection Guide

| Task Type | Recommended Model | Reason |
|-----------|------------------|--------|
| Code Writing/Review | Claude Sonnet | Best code understanding |
| Creative Writing | GPT-4o | Strong creative ability |
| Data Analysis | DeepSeek Chat | Cost-effective, good at structured data |
| Complex Reasoning | DeepSeek-R1 | Specialized for reasoning |
| Translation | Qwen (local) | Good multilingual, privacy |
| Simple Tasks | GPT-4o-mini | Fast and cheap |
| Strategic Planning | Claude Opus | Strongest reasoning |

---

## Cost Optimization Example

```
Task: Build a web application with documentation

Without Multi-Model (Single Claude Opus):
- Planning: 2K tokens × $0.015 = $0.03
- Code (10 files): 50K tokens × $0.015 = $0.75
- Documentation: 10K tokens × $0.015 = $0.15
- Total: $0.93

With Multi-Model:
- Planning (Opus): 2K tokens × $0.015 = $0.03
- Code (Sonnet): 50K tokens × $0.003 = $0.15
- Documentation (GPT-4o-mini): 10K tokens × $0.00015 = $0.0015
- Total: $0.18

Savings: ~80%
```

---

## Files to Create/Modify

```
apps/beamai_deepagent/src/
├── pool/
│   ├── beamai_agent_pool.erl        (new)
│   └── beamai_agent_registry.erl    (new)
├── routing/
│   └── beamai_task_router.erl       (new)
├── hierarchy/
│   └── beamai_hierarchy.erl         (new)
└── tools/
    └── beamai_delegate_tool.erl     (new)
```

---

## Open Questions

1. How to handle cross-agent context sharing?
2. Should agents be able to request specific other agents?
3. How to handle agent failures in hierarchy?
4. Should we support agent "specialization" learning?
5. How to measure and improve agent selection accuracy?

---

## Related Designs

- [Dual LLM Architecture](DUAL_LLM_DEEPAGENT.md) - Planner/Executor separation
- [SubAgent Architecture](SUBAGENT_ARCHITECTURE.md) - Process isolation model

---

---

# 中文版本

## 概述

本设计探讨如何在 SubAgent 架构中使用多个不同的 LLM 模型来完成专业化任务。每个 SubAgent 可以配置最适合其特定任务类型的模型。

## 状态

- **状态**: 提议中
- **创建日期**: 2026-01-20
- **作者**: 设计讨论

---

## 核心架构

```
┌─────────────────────────────────────────────────────────────┐
│                    多模型 SubAgent 架构                      │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   ┌─────────────────────────────────────────────────────┐  │
│   │              Agent Registry (Agent 注册表)           │  │
│   │                                                     │  │
│   │  ┌─────────────┬─────────────┬─────────────┐       │  │
│   │  │ code_agent  │ write_agent │ data_agent  │       │  │
│   │  │ Claude      │ GPT-4       │ DeepSeek    │       │  │
│   │  │ 代码专家    │ 写作专家    │ 数据分析    │       │  │
│   │  └─────────────┴─────────────┴─────────────┘       │  │
│   │                                                     │  │
│   │  ┌─────────────┬─────────────┬─────────────┐       │  │
│   │  │ trans_agent │ fast_agent  │reason_agent │       │  │
│   │  │ Qwen        │ GPT-4o-mini │ DeepSeek-R1 │       │  │
│   │  │ 多语言翻译  │ 快速简单任务│ 深度推理    │       │  │
│   │  └─────────────┴─────────────┴─────────────┘       │  │
│   └─────────────────────────────────────────────────────┘  │
│                          │                                  │
│                          ▼                                  │
│   ┌─────────────────────────────────────────────────────┐  │
│   │              Orchestrator (调度器)                   │  │
│   │                                                     │  │
│   │  Planner LLM: "分析任务，选择合适的 Agent"          │  │
│   │       │                                              │  │
│   │       ▼                                              │  │
│   │  Task Router: 根据任务类型路由到对应 Agent          │  │
│   └───────────────────┬─────────────────────────────────┘  │
│                       │                                     │
│        ┌──────────────┼──────────────┐                     │
│        ▼              ▼              ▼                     │
│   [code_agent]   [write_agent]  [data_agent]               │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 方案一：预定义 Agent 池

最简单的方式 - 预先配置一组专业 Agent。

```erlang
Config = #{
    %% Orchestrator 使用高端模型做决策
    orchestrator_llm => llm_client:create(anthropic, #{
        model => <<"claude-sonnet-4-20250514">>
    }),

    %% 预定义的专业 Agent 池
    agent_pool => #{
        %% 代码专家 - Claude 擅长代码
        code => #{
            llm => llm_client:create(anthropic, #{
                model => <<"claude-sonnet-4-20250514">>
            }),
            tools => [read_file, write_file, run_tests],
            system_prompt => <<"你是代码专家...">>
        },

        %% 写作专家 - GPT 擅长创意写作
        writing => #{
            llm => llm_client:create(openai, #{
                model => <<"gpt-4o">>
            }),
            tools => [search_web, read_file],
            system_prompt => <<"你是写作专家...">>
        },

        %% 数据分析 - DeepSeek 性价比高
        data => #{
            llm => llm_client:create(deepseek, #{
                model => <<"deepseek-chat">>
            }),
            tools => [query_db, create_chart],
            system_prompt => <<"你是数据分析专家...">>
        },

        %% 快速任务 - 便宜快速的模型
        fast => #{
            llm => llm_client:create(openai, #{
                model => <<"gpt-4o-mini">>
            }),
            tools => [],
            system_prompt => <<"快速完成简单任务...">>
        }
    }
}.
```

---

## 方案二：动态 Agent 选择（LLM 决策）

让 Orchestrator LLM 自己决定用哪个 Agent。

```erlang
%% Orchestrator 的系统提示
OrchestratorPrompt = <<"
你是一个任务调度器。根据用户任务，选择最合适的专业 Agent 执行。

可用的 Agent:
1. code_agent - 代码编写、审查、调试（使用 Claude）
2. writing_agent - 文章写作、内容创作（使用 GPT-4）
3. data_agent - 数据分析、报表生成（使用 DeepSeek）
4. translation_agent - 多语言翻译（使用 Qwen）
5. fast_agent - 简单快速任务（使用 GPT-4o-mini）
6. reasoning_agent - 复杂推理问题（使用 DeepSeek-R1）

使用 delegate_to_agent 工具来分配任务。
"/utf8>>.

%% delegate_to_agent 工具
delegate_tool() -> #{
    name => <<"delegate_to_agent">>,
    description => <<"将任务委托给专业 Agent 执行">>,
    parameters => #{
        type => object,
        properties => #{
            <<"agent_type">> => #{
                type => string,
                enum => [<<"code">>, <<"writing">>, <<"data">>,
                        <<"translation">>, <<"fast">>, <<"reasoning">>],
                description => <<"选择的 Agent 类型">>
            },
            <<"task">> => #{
                type => string,
                description => <<"要执行的具体任务描述">>
            }
        },
        required => [<<"agent_type">>, <<"task">>]
    },
    handler => fun handle_delegate/2
}.
```

---

## 方案三：能力标签匹配（推荐）

更智能的方式 - 基于能力标签自动匹配。

```erlang
%% Agent 能力定义
-type capability() ::
    code | writing | translation | data_analysis |
    reasoning | fast | creative | technical | math.

%% Agent 注册表
-record(agent_entry, {
    name :: binary(),
    llm_config :: map(),
    capabilities :: [capability()],
    cost_per_1k :: float(),      %% 每千 token 成本
    avg_latency :: integer(),     %% 平均延迟 ms
    quality_score :: float(),     %% 质量评分 0-1
    tools :: [atom()],
    system_prompt :: binary()
}).

%% 示例注册
default_agents() -> [
    #agent_entry{
        name = <<"claude_coder">>,
        llm_config = llm_client:create(anthropic, #{model => <<"claude-sonnet">>}),
        capabilities = [code, technical, reasoning],
        cost_per_1k = 0.003,
        avg_latency = 2000,
        quality_score = 0.95,
        tools = [read_file, write_file, run_command],
        system_prompt = <<"你是专业程序员...">>
    },
    #agent_entry{
        name = <<"deepseek_analyst">>,
        llm_config = llm_client:create(deepseek, #{model => <<"deepseek-chat">>}),
        capabilities = [data_analysis, reasoning, code, math],
        cost_per_1k = 0.0001,  %% 非常便宜
        avg_latency = 3000,
        quality_score = 0.88,
        tools = [query_db, create_chart],
        system_prompt = <<"你是数据分析师...">>
    },
    #agent_entry{
        name = <<"fast_helper">>,
        llm_config = llm_client:create(openai, #{model => <<"gpt-4o-mini">>}),
        capabilities = [fast],
        cost_per_1k = 0.00015,
        avg_latency = 500,
        quality_score = 0.75,
        tools = [],
        system_prompt = <<"快速完成任务...">>
    }
].

%% 智能选择 Agent
select_agent(Task, Preferences) ->
    RequiredCaps = analyze_task_capabilities(Task),
    Candidates = filter_by_capabilities(default_agents(), RequiredCaps),

    case maps:get(optimize_for, Preferences, balanced) of
        cost -> sort_by_cost(Candidates);
        speed -> sort_by_latency(Candidates);
        quality -> sort_by_quality(Candidates);
        balanced -> sort_balanced(Candidates)
    end.
```

---

## 方案四：层级 Agent 架构

复杂任务可以有多层 Agent。

```
┌─────────────────────────────────────────────────────────────┐
│                    层级 Agent 架构                           │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   Level 0: Orchestrator (Claude Opus - 最强推理)            │
│   ┌─────────────────────────────────────────────────────┐  │
│   │  • 理解复杂任务                                      │  │
│   │  • 制定执行策略                                      │  │
│   │  • 分配给 Level 1 Agent                             │  │
│   └───────────────────┬─────────────────────────────────┘  │
│                       │                                     │
│   Level 1: Specialist Agents (Claude Sonnet / GPT-4)       │
│   ┌───────────┬───────────┬───────────┐                    │
│   │  Project  │  Code     │  Doc      │                    │
│   │  Manager  │  Lead     │  Writer   │                    │
│   └─────┬─────┴─────┬─────┴─────┬─────┘                    │
│         │           │           │                          │
│   Level 2: Worker Agents (GPT-4o-mini / DeepSeek)          │
│   ┌─────┴─────┬─────┴─────┬─────┴─────┐                    │
│   │  Task 1   │  Task 2   │  Task 3   │                    │
│   │  Worker   │  Worker   │  Worker   │                    │
│   │  (便宜)   │  (便宜)   │  (便宜)   │                    │
│   └───────────┴───────────┴───────────┘                    │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 模型选择指南

| 任务类型 | 推荐模型 | 原因 |
|----------|----------|------|
| 代码编写/审查 | Claude Sonnet | 最佳代码理解能力 |
| 创意写作 | GPT-4o | 强创意能力 |
| 数据分析 | DeepSeek Chat | 性价比高，擅长结构化数据 |
| 复杂推理 | DeepSeek-R1 | 专门针对推理优化 |
| 翻译 | Qwen (本地) | 多语言能力强，保护隐私 |
| 简单任务 | GPT-4o-mini | 快速便宜 |
| 战略规划 | Claude Opus | 最强推理能力 |

---

## 成本优化示例

```
任务: 构建一个带文档的 Web 应用

不使用多模型（单一 Claude Opus）:
- 规划: 2K tokens × $0.015 = $0.03
- 代码 (10 文件): 50K tokens × $0.015 = $0.75
- 文档: 10K tokens × $0.015 = $0.15
- 总计: $0.93

使用多模型:
- 规划 (Opus): 2K tokens × $0.015 = $0.03
- 代码 (Sonnet): 50K tokens × $0.003 = $0.15
- 文档 (GPT-4o-mini): 10K tokens × $0.00015 = $0.0015
- 总计: $0.18

节省: ~80%
```

---

## 推荐实施策略

### 阶段 1: 简单 Agent 池（短期）

```erlang
%% 简单但实用的配置
#{
    orchestrator => #{
        llm => claude_sonnet,
        prompt => <<"选择合适的 Agent...">>
    },
    agents => #{
        code => #{llm => claude_sonnet, tools => [...]},
        write => #{llm => gpt4o, tools => [...]},
        analyze => #{llm => deepseek, tools => [...]},
        fast => #{llm => gpt4o_mini, tools => []}
    }
}
```

### 阶段 2: 能力匹配（中期）

- 实现能力标签系统
- 添加自动任务分析
- 支持成本/速度/质量偏好

### 阶段 3: 完整层级架构（长期）

- 实现层级架构
- 支持动态 Agent 注册
- 添加分布式执行支持

---

## 待解决问题

1. 如何处理跨 Agent 的上下文共享？
2. Agent 是否可以请求特定的其他 Agent？
3. 如何处理层级中的 Agent 失败？
4. 是否应该支持 Agent "专业化"学习？
5. 如何测量和改进 Agent 选择的准确性？

---

## 相关设计

- [双 LLM 架构设计](DUAL_LLM_DEEPAGENT.md) - Planner/Executor 分离
- [SubAgent 架构设计](SUBAGENT_ARCHITECTURE.md) - 进程隔离模型
