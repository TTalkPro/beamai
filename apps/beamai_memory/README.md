%%%-------------------------------------------------------------------
%%% Agent Memory - 分层架构 v4.0
%%%
%%% 统一记忆管理系统，职责清晰的双管理器分层设计。
%%%-------------------------------------------------------------------

# Agent Memory

统一记忆管理系统，提供短期记忆（Checkpointer）和长期记忆（Store）的统一接口。

## 架构概览

```
┌─────────────────────────────────────────────────────────────────────┐
│                    beamai_memory (API 层)                          │
│                      统一对外接口 - 协调层                        │
└─────────────────────────────────────────────────────────────────────┘
                            │
        ┌───────────────────┴───────────────────┐
        │                                       │
┌──────────────────────────┐        ┌──────────────────────────────┐
│  checkpoint_manager      │        │  store_manager               │
│  (检查点管理器)           │        │  (存储管理器)                │
│                          │        │                              │
│  - 检查点 CRUD           │        │  - context_store 管理        │
│  - 时间旅行             │        │  - persistent_store 管理     │
│  - 分支管理             │        │  - Store 选择逻辑            │
│                          │        │  - 归档功能                  │
└──────────────────────────┘        └──────────────────────────────┘
        │                                       │
        │                                       │
┌──────────────────────────┐        ┌──────────────────────────────┐
│  扩展模块                │        │  扩展模块                    │
│  ├── time_travel         │        │  ├── store_archiver          │
│  └── branch              │        │  └── ...                     │
└──────────────────────────┘        └──────────────────────────────┘
```

## 设计原则

1. **分层架构** - API 层协调，管理器层实现，扩展层增强
2. **职责分离** - checkpoint_manager 管检查点，store_manager 管存储
3. **双 Store 架构** - 支持热/冷数据分离，也可降级为单 Store
4. **可插拔后端** - Store 协议支持多种实现（ETS、SQLite 等）

## Checkpoint 数量限制

### 配置选项

checkpoint_manager 支持配置检查点数量限制，自动清理旧检查点：

```erlang
%% 创建时配置数量限制
{ok, ContextStore} = beamai_store_ets:start_link(my_context, #{}),
{ok, Memory} = beamai_memory:new(#{
    context_store => {beamai_store_ets, my_context},
    checkpoint_config => #{
        max_checkpoints => 100,    %% 最多保留 100 个检查点
        auto_prune => true           %% 自动清理旧的检查点
    }
}).
```

### 数量限制策略

| 配置 | 默认值 | 说明 |
|------|--------|------|
| `max_checkpoints` | **50** | 最大检查点数量（优化内存） |
| `auto_prune` | `true` | 超过限制时自动清理旧检查点 |
| ETS `max_items` | **1000** | ETS 存储容量限制（优化内存） |
| ETS `max_namespaces` | **1000** | 命名空间数量限制 |

**默认配置已优化以降低内存使用：**
- checkpoint 默认最多保留 **50 个** 检查点
- ETS Store 默认最多 **1000** 个数据项
- 超过限制时自动清理旧数据

### 清理策略

- **自动清理**：保存检查点时，如果数量超过 `max_checkpoints`，自动删除最旧的检查点
- **手动清理**：调用 `beamai_memory:prune_checkpoints/3` 手动清理
- **ETS Store**：使用 LRU 策略淘汰（自动触发，约删除 10% 的旧数据）

### 统计和监控

```erlang
%% 获取统计信息
Stats = beamai_memory:get_checkpoint_stats(Memory),
%% => #{
%%     total_checkpoints => 42,
%%     max_checkpoints => 50,
%%     branches => 1,
%%     usage_percentage => 84.0
%% }

%% 获取线程的检查点数量
Count = beamai_memory:get_checkpoint_count(Memory, <<"thread-1">>).

%% 手动清理
{ok, DeletedCount} = beamai_memory:prune_checkpoints(Memory, <<"thread-1">>, 50).

%% 清理所有线程
{ok, Result} = beamai_memory:prune_all_checkpoints(Memory, 100).
```

## 模块职责

### API 层

| 模块 | 职责 | 主要功能 |
|------|------|----------|
| `beamai_memory` | 统一 API | 协调两个管理器，提供便捷接口 |

### 管理器层

| 管理器 | 职责 | 文件 |
|--------|------|------|
| **checkpoint_manager** | 检查点管理 | `checkpoint/agent_checkpoint_manager.erl` |
| **store_manager** | 存储管理 | `store/beamai_store_manager.erl` |

### 扩展层

| 扩展 | 职责 | 文件 |
|------|------|------|
| **time_travel** | 时间旅行 | `checkpoint/agent_checkpoint_time_travel.erl` |
| **branch** | 分支管理 | `checkpoint/agent_checkpoint_branch.erl` |
| **archiver** | 归档功能 | `store/beamai_store_archiver.erl` |

## 双 Store 架构

### 设计动机

| 存储类型 | 用途 | 特点 |
|---------|------|------|
| context_store | 当前会话、Checkpointer | 快速读写、内存存储 |
| persistent_store | 历史归档、知识库 | 持久化、大容量 |

### 数据流向

```
用户对话 → context_store → [归档] → persistent_store
              ↑                           ↓
         checkpoint_manager          [加载历史]
         (时间旅行 + 分支)
```

## 使用示例

### 基本用法

```erlang
%% 创建 Memory（双 Store 模式）
{ok, ContextStore} = beamai_store_ets:start_link(my_context, #{}),
{ok, PersistStore} = beamai_store_sqlite:start_link(my_persist, #{
    db_path => "/data/agent-memory.db"
}),
{ok, Memory} = beamai_memory:new(#{
    context_store => {beamai_store_ets, my_context},
    persistent_store => {beamai_store_sqlite, my_persist}
}).

%% 保存检查点
Config = #{thread_id => <<"thread-1">>},
State = #{messages => [#{role => user, content => <<"Hello">>}]},
{ok, CpId} = beamai_memory:save_checkpoint(Memory, Config, State).

%% 加载检查点
{ok, LoadedState} = beamai_memory:load_checkpoint(Memory, Config).
```

### 时间旅行

```erlang
%% 回退 3 步
{ok, State} = beamai_memory:go_back(Memory, Config, 3).

%% 前进 1 步
{ok, State} = beamai_memory:go_forward(Memory, Config, 1).

%% 跳转到指定检查点
{ok, State} = beamai_memory:goto(Memory, Config, <<"cp_12345_6789">>).

%% 撤销（回退 1 步）
{ok, State} = beamai_memory:undo(Memory, Config).

%% 重做（前进 1 步）
{ok, State} = beamai_memory:redo(Memory, Config).

%% 查看历史
{ok, History} = beamai_memory:list_history(Memory, Config).
```

### 分支管理

```erlang
%% 创建新分支
{ok, BranchId} = beamai_memory:create_branch(Memory, Config, <<"experiment">>, #{}).

%% 切换分支
{ok, State} = beamai_memory:switch_branch(Memory, Config, <<"experiment">>).

%% 列出所有分支
{ok, Branches} = beamai_memory:list_branches(Memory).

%% 比较两个分支
{ok, Diff} = beamai_memory:compare_branches(Memory, <<"main">>, <<"experiment">>).
```

### 长期记忆

```erlang
%% 存储知识（到 persistent_store）
ok = beamai_memory:put(Memory,
    [<<"knowledge">>, <<"facts">>],
    <<"fact-1">>,
    #{content => <<"CL-Agent 是一个 Erlang AI 框架">>},
    #{persistent => true}  % 指定存储到 persistent_store
).

%% 搜索知识
{ok, Results} = beamai_memory:search(Memory, [<<"knowledge">>], #{}).

%% 获取知识
{ok, Item} = beamai_memory:get(Memory, [<<"knowledge">>, <<"facts">>], <<"fact-1">>).
```

### 归档功能

```erlang
%% 归档当前会话
{ok, SessionId} = beamai_memory:archive_session(Memory, <<"thread-1">>).

%% 归档并总结
SummarizeFn = fun(Checkpoints) ->
    <<"会话包含 5 条消息">>
end,
{ok, SessionId} = beamai_memory:archive_session(Memory, <<"thread-1">>, #{
    summarize_fn => SummarizeFn,
    tags => [<<"important">>]
}).

%% 列出已归档的会话
{ok, Archives} = beamai_memory:list_archived(Memory).

%% 按标签过滤归档
{ok, Archives} = beamai_memory:list_archived(Memory, #{
    tags => [<<"important">>]
}).

%% 加载已归档的会话（只读）
{ok, Checkpoints} = beamai_memory:load_archived(Memory, SessionId).

%% 恢复已归档的会话到 Checkpointer
{ok, ThreadId} = beamai_memory:restore_archived(Memory, SessionId).

%% 删除归档
ok = beamai_memory:delete_archived(Memory, SessionId).
```

### 便捷函数

```erlang
%% 添加消息
ok = beamai_memory:add_message(Memory, Config, #{
    role => user,
    content => <<"你好">>
}).

%% 获取消息历史
{ok, Messages} = beamai_memory:get_messages(Memory, Config).

%% 获取通道值
{ok, Value} = beamai_memory:get_channel(Memory, Config, messages).

%% 设置通道值
ok = beamai_memory:set_channel(Memory, Config, counter, 42).
```

## 高级用法

### 直接使用管理器

```erlang
%% 获取 Checkpointer 管理器
Checkpointer = beamai_memory:get_checkpointer(Memory),

%% 使用 manager 的所有功能
{ok, CpId} = agent_checkpoint_manager:save(Checkpointer, Checkpoint, Config),
{ok, Cp} = agent_checkpoint_manager:load(Checkpointer, latest, Config),

%% 使用时间旅行扩展
{ok, History} = agent_checkpoint_time_travel:list_history(Checkpointer, Config),

%% 使用分支管理扩展
{ok, BranchId} = agent_checkpoint_branch:create_branch(
    Checkpointer, Config, <<"feature-x">>, #{}
).

%% 获取 Store 管理器
StoreManager = beamai_memory:get_store_manager(Memory),

%% 使用 Store 管理器
ok = beamai_store_manager:put(StoreManager, Namespace, Key, Value, #{}),
{ok, Item} = beamai_store_manager:get(StoreManager, Namespace, Key).

%% 使用归档器
{ok, SessionId} = beamai_store_archiver:archive_session(
    StoreManager, Checkpointer, ThreadId
).
```

## 文件结构

```
beamai_memory/
├── README.md                              # 本文档
├── rebar.config                           # 编译配置
├── include/
│   ├── beamai_checkpointer.hrl             # Checkpoint 记录定义
│   ├── agent_episodic_memory.hrl          # 情景记忆记录
│   ├── agent_procedural_memory.hrl        # 程序记忆记录
│   ├── agent_semantic_memory.hrl          # 语义记忆记录
│   └── beamai_store.hrl                    # Store 记录定义
├── src/
│   ├── api/
│   │   └── beamai_memory.erl               # 统一 Memory API
│   ├── checkpoint/                        # 检查点管理模块
│   │   ├── agent_checkpoint_protocol.erl  # 协议定义
│   │   ├── agent_checkpoint_manager.erl   # 核心管理器
│   │   ├── agent_checkpoint_time_travel.erl # 时间旅行扩展
│   │   └── agent_checkpoint_branch.erl    # 分支管理扩展
│   ├── store/                             # 存储管理模块
│   │   ├── beamai_store.erl                # Store 协议
│   │   ├── beamai_store_ets.erl            # ETS 后端
│   │   ├── beamai_store_sqlite.erl         # SQLite 后端
│   │   ├── beamai_store_manager.erl        # 存储管理器
│   │   └── beamai_store_archiver.erl       # 归档器
│   ├── memory/
│   │   ├── agent_episodic_memory.erl      # 情景记忆实现
│   │   ├── agent_procedural_memory.erl    # 程序记忆实现
│   │   └── agent_semantic_memory.erl      # 语义记忆实现
│   ├── utils/
│   │   ├── beamai_memory_helpers.erl       # 辅助函数
│   │   ├── beamai_memory_types.erl         # 类型定义
│   │   └── beamai_memory_utils.erl         # 工具函数
│   └── buffer/
│       └── ...                             # 缓冲区模块
└── test/
    └── ...                                 # 测试文件
```

## 与旧版本对比

| 特性 | 旧架构 (v3.x) | 新架构 (v4.x) |
|------|--------------|--------------|
| 分层设计 | 无明确分层 | API + 管理器 + 扩展 |
| 代码组织 | 单一大文件 (900+ 行) | 职责分离，模块化 |
| Checkpointer 逻辑 | 混在 beamai_memory.erl | checkpoint_manager |
| Store 管理 | 直接调用 beamai_store | store_manager |
| 时间旅行 | 混在一起 | agent_checkpoint_time_travel |
| 分支管理 | 混在一起 | agent_checkpoint_branch |
| 归档功能 | 混在一起 | beamai_store_archiver |
| 可测试性 | 较低 | 每个模块独立测试 |
| 可扩展性 | 需修改大文件 | 添加新扩展模块 |

## 版本历史

- **4.0.0** - 分层架构重构，双管理器设计
- **3.0.0** - 双 Store 架构，Checkpointer 使用 Store 接口
- **2.0.0** - 分离 Checkpointer 和 Store
- **1.0.0** - 初始版本
