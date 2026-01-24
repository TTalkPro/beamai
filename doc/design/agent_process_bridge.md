# Agent vs Process Framework: 设计决策

## 背景

beamai 中存在两个编排系统：
- **beamai_agent**: 单智能体对话 (Chat + Tools + Memory)
- **beamai_process**: 多步骤工作流编排 (Steps + Events + Topology)

## 架构对比

| 维度 | beamai_agent | Process Framework |
|------|-------------|-------------------|
| 执行模型 | 函数式, 无常驻进程 | gen_statem, 有状态进程 |
| 核心抽象 | Turn (一轮对话) | Step (事件驱动步骤) |
| 编排方式 | 单一 tool-loop 循环 | 事件路由 + 拓扑图 |
| 并发方式 | 无 (同步顺序) | poolboy 工作池 |
| 状态管理 | Map 透传, 调用方持有 | gen_statem 内部持有 |
| HITL | 中断工具 / 回调中断 | Step-level pause/resume |
| 持久化 | Checkpoint (memory backend) | Snapshot/Restore |
| 流式 | 支持 (on_token callback) | 不支持 |

## 决策

**选择方案三: 保持 Agent 独立, 提供 Process-Agent 桥接**

### 理由

Agent 和 Process 解决的是不同层次的问题，是互补关系而非替代关系：
- Agent 擅长"单体对话智能"（对话历史、流式、精细中断）
- Process 擅长"多步骤编排"（拓扑、并行、事件驱动）

完全迁移 Agent 到 Process 是用高层抽象降级为低层实现，会丢失：
1. 流式输出支持
2. 对话历史自动管理
3. 精细的中断粒度（tool loop 中间暂停）
4. 8 种回调观测机制
5. 函数式设计的灵活性

### 实现方案

```
beamai_agent:           单智能体对话 (保持不变)
beamai_process:         工作流编排 (保持不变)
beamai_agent_step:      桥接层 (新增)
```

新增 `beamai_agent_step` 模块，实现 `beamai_step_behaviour`，将 Agent 包装为 Process 中的一个 step。

### 优势

1. 零破坏性: 现有 Agent 用法完全不变
2. 能力叠加: Agent 获得 Process 的并行、fan-out/fan-in、循环能力
3. 关注点分离: Agent 管对话, Process 管编排
4. 渐进式采用: 用户按需选择复杂度层级
5. 流式保留: Agent 内部流式不受影响
6. 中断保留: Agent 的精细中断机制不受影响

### 使用示例

```erlang
%% 简单场景: 直接用 Agent
{ok, Agent} = beamai_agent:new(Config),
{ok, Result, Agent1} = beamai_agent:run(Agent, <<"Hello">>).

%% 复杂编排: 在 Process 中组合多个 Agent steps
P0 = beamai_process:builder(multi_agent_workflow),
P1 = beamai_process:add_step(P0, researcher, beamai_agent_step, #{
    agent_config => #{system_prompt => <<"研究员"/utf8>>, ...},
    output_event => research_done
}),
P2 = beamai_process:add_step(P1, writer, beamai_agent_step, #{
    agent_config => #{system_prompt => <<"写手"/utf8>>, ...},
    output_event => draft_done
}),
...
```
