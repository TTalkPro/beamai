# DeepAgent 双 LLM 架构设计

English | [中文](#中文版本)

## Overview

This design proposes a dual-LLM architecture for DeepAgent, where two different LLM providers work together:
- **Planner LLM**: Responsible for planning, task decomposition, and reflection
- **Executor LLM**: Responsible for tool execution and generating responses

## Status

- **Status**: Proposed
- **Created**: 2026-01-20
- **Author**: Design Discussion

---

## Current Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    DeepAgent Execution Flow                  │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   User Input ──▶ LLM Node ──▶ Router ──▶ Tool Node          │
│                    │            │           │               │
│                    │            ├─▶ Create Plan             │
│                    │            ├─▶ Spawn Subtasks          │
│                    │            ├─▶ Reflect                 │
│                    │            └─▶ End                     │
│                    │                        │               │
│                    ◀────────────────────────┘               │
│                      (Single LLM)                           │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

**Current configuration supports only one LLM:**
```erlang
config() :: #{
    llm => llm_config(),  %% Single LLM configuration
    ...
}
```

---

## Proposed Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                  Dual LLM DeepAgent Architecture             │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   ┌─────────────────────┐    ┌─────────────────────┐       │
│   │   Planner LLM       │    │   Executor LLM      │       │
│   │  (Planner/Reflector)│    │   (Executor)        │       │
│   │                     │    │                     │       │
│   │  • Create plans     │    │  • Execute tools    │       │
│   │  • Decompose tasks  │    │  • Handle simple    │       │
│   │  • Reflect & adjust │    │    conversations    │       │
│   │  • High-level       │    │  • Generate final   │       │
│   │    decisions        │    │    responses        │       │
│   │                     │    │  • Parse tool args  │       │
│   │  Recommended:       │    │                     │       │
│   │  Claude/GPT-4       │    │  Recommended:       │       │
│   │  DeepSeek-R1        │    │  GPT-4o-mini        │       │
│   │                     │    │  Qwen/GLM-4-Flash   │       │
│   └─────────┬───────────┘    └──────────┬──────────┘       │
│             │                           │                   │
│             ▼                           ▼                   │
│   ┌─────────────────────────────────────────────────┐      │
│   │           Unified State Management (Graph)       │      │
│   └─────────────────────────────────────────────────┘      │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## Configuration Design

### New Configuration Structure

```erlang
%% Extended configuration
config() :: #{
    %% Option 1: Dual LLM configuration
    planner_llm => llm_config(),   %% Planner LLM
    executor_llm => llm_config(),  %% Executor LLM

    %% Option 2: Backward compatible - single LLM for both roles
    llm => llm_config(),

    %% Other existing options...
    tools => [tool_spec()],
    max_depth => pos_integer(),
    max_iterations => pos_integer(),
    planning_enabled => boolean(),
    reflection_enabled => boolean(),
    ...
}
```

### Example Configurations

```erlang
%% High cost-performance configuration
#{
    planner_llm => llm_client:create(anthropic, #{
        model => <<"claude-sonnet-4-20250514">>,
        api_key => PlannerKey
    }),
    executor_llm => llm_client:create(openai, #{
        model => <<"gpt-4o-mini">>,
        api_key => ExecutorKey
    })
}

%% Local-first configuration
#{
    planner_llm => llm_client:create(deepseek, #{
        model => <<"deepseek-reasoner">>,  %% Strong reasoning
        api_key => DeepSeekKey
    }),
    executor_llm => llm_client:create(ollama, #{
        model => <<"qwen2.5:7b">>  %% Fast local execution
    })
}

%% Backward compatible - single LLM
#{
    llm => llm_client:create(openai, #{
        model => <<"gpt-4o">>,
        api_key => ApiKey
    })
}
```

---

## Implementation Plan

### Phase 1: Configuration Extension (Low Risk)

**File: `beamai_deepagent.erl`**

```erlang
%% Configuration validation
validate_llm_config(#{planner_llm := P, executor_llm := E}) ->
    llm_client:is_valid_config(P) andalso llm_client:is_valid_config(E);
validate_llm_config(#{llm := L}) ->
    %% Backward compatible: single LLM for both roles
    llm_client:is_valid_config(L);
validate_llm_config(_) ->
    false.

%% Normalize config to always have both LLMs
normalize_llm_config(#{planner_llm := _, executor_llm := _} = Config) ->
    Config;
normalize_llm_config(#{llm := LLM} = Config) ->
    Config#{planner_llm => LLM, executor_llm => LLM};
normalize_llm_config(Config) ->
    Config.
```

### Phase 2: LLM Selection Logic

**File: `beamai_deepagent_llm_node.erl`**

```erlang
%% Get appropriate LLM based on operation type
get_llm_for_operation(State) ->
    ToolCalls = graph:get(State, pending_tool_calls, []),
    case classify_operation(ToolCalls) of
        planning ->
            graph:get(State, planner_llm, graph:get(State, llm));
        reflection ->
            graph:get(State, planner_llm, graph:get(State, llm));
        execution ->
            graph:get(State, executor_llm, graph:get(State, llm))
    end.

%% Classify current operation
classify_operation([]) ->
    planning;  %% No tool calls = planning phase
classify_operation(Calls) ->
    Names = [maps:get(name, C, <<>>) || C <- Calls],
    case lists:any(fun is_planning_tool/1, Names) of
        true -> planning;
        false -> execution
    end.

%% Planning tool detection
is_planning_tool(<<"create_plan">>) -> true;
is_planning_tool(<<"update_plan">>) -> true;
is_planning_tool(<<"spawn_subtask">>) -> true;
is_planning_tool(<<"reflect">>) -> true;
is_planning_tool(_) -> false.
```

### Phase 3: Cross-Model Context Optimization

```erlang
%% Prepare concise context for Executor
prepare_executor_context(State, ToolCall) ->
    #{
        task => extract_current_task(State),
        tool => ToolCall,
        constraints => extract_constraints(State),
        %% Don't pass full history, only necessary info
        recent_context => get_recent_context(State, 3)
    }.

%% Extract current task from plan
extract_current_task(State) ->
    case graph:get(State, current_plan, undefined) of
        undefined ->
            graph:get(State, original_input, <<>>);
        Plan ->
            beamai_deepagent_plan:get_current_step(Plan)
    end.
```

---

## Advantages

| Advantage | Description |
|-----------|-------------|
| **Cost Optimization** | Use expensive models (GPT-4/Claude) for planning, cheap models (GPT-4o-mini) for execution. Can reduce costs by 50-70% |
| **Specialization** | Models with strong reasoning (DeepSeek-R1) for planning, models with good instruction following for execution |
| **Speed Optimization** | Executor can use faster models, reducing overall latency |
| **Model Complementarity** | Leverage different model strengths: Claude excels at reasoning, GPT excels at tool calling |
| **Hybrid Local+Cloud** | Planner uses cloud high-end model, Executor uses local Ollama model for privacy |
| **Fault Tolerance** | If one provider is unavailable, the other may still work |
| **A/B Testing** | Easy to compare different models in different roles |

---

## Disadvantages & Mitigations

| Disadvantage | Description | Mitigation |
|--------------|-------------|------------|
| **Context Consistency** | Two models may understand the same task differently | Design clear state transfer protocol, use structured intermediate representations |
| **Debugging Complexity** | Hard to identify if Planner or Executor caused the error | Enhanced trace logging, mark which LLM each operation uses |
| **Configuration Complexity** | Users need to manage two sets of configurations | Provide sensible defaults, support single-LLM fallback |
| **Increased Latency** | May need more API calls for coordination | Optimize batching, cache common decisions |
| **Token Waste** | Context may need to be passed repeatedly | Design concise cross-model context format |
| **Style Inconsistency** | Final output may have inconsistent style | Executor follows Planner's style instructions |

---

## Files to Modify

1. **`apps/beamai_deepagent/src/beamai_deepagent.erl`**
   - Add dual LLM configuration validation
   - Normalize configuration to support both patterns

2. **`apps/beamai_deepagent/src/nodes/beamai_deepagent_llm_node.erl`**
   - Add LLM selection logic based on operation type
   - Implement `get_llm_for_operation/1`

3. **`apps/beamai_deepagent/src/core/beamai_deepagent_state.erl`** (if exists)
   - Add helpers for cross-model context preparation

4. **Documentation**
   - Update README with dual LLM examples
   - Add configuration guide

---

## Testing Strategy

1. **Unit Tests**
   - Test configuration validation with various input patterns
   - Test LLM selection logic
   - Test backward compatibility

2. **Integration Tests**
   - Test with actual dual LLM configuration
   - Test fallback behavior
   - Test error handling when one LLM fails

3. **Performance Tests**
   - Compare cost between single vs dual LLM
   - Measure latency differences
   - Token usage analysis

---

## Open Questions

1. Should we support more than two LLMs (e.g., separate reflection LLM)?
2. How to handle context window differences between models?
3. Should we add automatic model selection based on task complexity?
4. How to handle streaming responses across different models?

---

---

# 中文版本

## 概述

本设计提出 DeepAgent 的双 LLM 架构，两个不同的 LLM Provider 协同工作：
- **Planner LLM（规划者）**: 负责计划制定、任务分解和反思
- **Executor LLM（执行者）**: 负责工具执行和生成响应

## 状态

- **状态**: 提议中
- **创建日期**: 2026-01-20
- **作者**: 设计讨论

---

## 当前架构

```
┌─────────────────────────────────────────────────────────────┐
│                    DeepAgent 执行流程                        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   用户输入 ──▶ LLM Node ──▶ 路由决策 ──▶ Tool Node          │
│                  │              │            │              │
│                  │              ├─▶ 创建计划  │              │
│                  │              ├─▶ 生成子任务 │              │
│                  │              ├─▶ 反思分析  │              │
│                  │              └─▶ 结束      │              │
│                  │                           │              │
│                  ◀───────────────────────────┘              │
│                      (同一个 LLM)                           │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 提议的架构

```
┌─────────────────────────────────────────────────────────────┐
│                  双 LLM DeepAgent 架构                       │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   ┌─────────────────────┐    ┌─────────────────────┐       │
│   │   Planner LLM       │    │   Executor LLM      │       │
│   │  (规划者/反思者)     │    │   (执行者)          │       │
│   │                     │    │                     │       │
│   │  • 制定计划         │    │  • 执行具体工具     │       │
│   │  • 分解任务         │    │  • 处理简单对话     │       │
│   │  • 反思和调整       │    │  • 生成最终回复     │       │
│   │  • 高层决策         │    │  • 解析工具参数     │       │
│   │                     │    │                     │       │
│   │  推荐: Claude/GPT-4 │    │  推荐: GPT-4o-mini │       │
│   │  DeepSeek-R1        │    │  Qwen/GLM-4-Flash  │       │
│   └─────────┬───────────┘    └──────────┬──────────┘       │
│             │                           │                   │
│             ▼                           ▼                   │
│   ┌─────────────────────────────────────────────────┐      │
│   │              统一状态管理 (Graph State)          │      │
│   └─────────────────────────────────────────────────┘      │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## 配置设计

### 新配置结构

```erlang
%% 扩展配置
config() :: #{
    %% 方式 1: 双 LLM 配置
    planner_llm => llm_config(),   %% 规划者 LLM
    executor_llm => llm_config(),  %% 执行者 LLM

    %% 方式 2: 向后兼容 - 单 LLM 同时担任两个角色
    llm => llm_config(),

    %% 其他现有选项...
    tools => [tool_spec()],
    max_depth => pos_integer(),
    max_iterations => pos_integer(),
    planning_enabled => boolean(),
    reflection_enabled => boolean(),
    ...
}
```

### 配置示例

```erlang
%% 高性价比配置
#{
    planner_llm => llm_client:create(anthropic, #{
        model => <<"claude-sonnet-4-20250514">>,
        api_key => PlannerKey
    }),
    executor_llm => llm_client:create(openai, #{
        model => <<"gpt-4o-mini">>,
        api_key => ExecutorKey
    })
}

%% 本地优先配置
#{
    planner_llm => llm_client:create(deepseek, #{
        model => <<"deepseek-reasoner">>,  %% 强推理能力
        api_key => DeepSeekKey
    }),
    executor_llm => llm_client:create(ollama, #{
        model => <<"qwen2.5:7b">>  %% 本地快速执行
    })
}

%% 向后兼容 - 单 LLM
#{
    llm => llm_client:create(openai, #{
        model => <<"gpt-4o">>,
        api_key => ApiKey
    })
}
```

---

## 优势分析

| 优势 | 说明 |
|------|------|
| **成本优化** | Planner 用高端模型（如 GPT-4/Claude），Executor 用便宜模型（如 GPT-4o-mini），可降低 50-70% 成本 |
| **专业化分工** | 推理能力强的模型（DeepSeek-R1）做规划，指令遵循好的模型做执行 |
| **速度优化** | Executor 可用更快的模型，减少整体延迟 |
| **模型互补** | 利用不同模型的优势：Claude 擅长推理，GPT 擅长工具调用 |
| **本地+云端混合** | Planner 用云端高端模型，Executor 用本地 Ollama 模型，兼顾能力和隐私 |
| **容错能力** | 一个 Provider 不可用时，另一个可能仍可工作 |
| **A/B 测试** | 方便对比不同模型在不同角色的表现 |

---

## 劣势与缓解措施

| 劣势 | 说明 | 缓解措施 |
|------|------|----------|
| **上下文一致性** | 两个模型对同一任务理解可能不同 | 设计清晰的状态传递协议，使用结构化中间表示 |
| **调试复杂** | 出错时难以定位是 Planner 还是 Executor 的问题 | 增强 trace 日志，标记每个操作使用的 LLM |
| **配置复杂** | 用户需要管理两套配置 | 提供合理默认值，支持单 LLM 回退 |
| **延迟增加** | 可能需要更多 API 调用来协调 | 优化批处理，缓存常用决策 |
| **Token 浪费** | 上下文可能需要重复传递 | 设计精简的跨模型上下文格式 |
| **风格不一致** | 最终输出可能风格不统一 | Executor 遵循 Planner 的风格指令 |

---

## 实现阶段

### 阶段 1: 配置扩展（低风险）
- 扩展配置验证逻辑
- 支持向后兼容

### 阶段 2: LLM 选择逻辑
- 根据操作类型选择合适的 LLM
- 实现 `get_llm_for_operation/1`

### 阶段 3: 跨模型上下文优化
- 设计精简的跨模型上下文格式
- 优化 token 使用

---

## 待解决问题

1. 是否需要支持两个以上的 LLM（例如独立的反思 LLM）？
2. 如何处理不同模型之间的上下文窗口差异？
3. 是否需要根据任务复杂度自动选择模型？
4. 如何处理不同模型之间的流式响应？

---

## 相关文件

- `apps/beamai_deepagent/src/beamai_deepagent.erl` - 主模块
- `apps/beamai_deepagent/src/nodes/beamai_deepagent_llm_node.erl` - LLM 节点
- `apps/beamai_deepagent/src/core/beamai_deepagent_plan.erl` - 计划管理
