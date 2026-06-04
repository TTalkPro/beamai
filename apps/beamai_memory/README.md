# BeamAI Memory

[English](README_EN.md) | 中文

纯存储引擎，提供快照管理（Snapshot）、Store 后端和状态存储的统一接口。

> **注意**: 认知架构（语义/情景/程序记忆）和对话缓冲已迁移至 [beamai_extra](https://github.com/TTalkPro/beamai_extra) 扩展项目。

## 架构概览

```
┌─────────────────────────────────────────────────────────────────────┐
│                 beamai_snapshot (通用快照引擎/Behaviour)              │
│                   分支管理 · 时间旅行 · 历史查询                     │
└─────────────────────────────────────────────────────────────────────┘
                         │
            ┌─────────────────────────┐
            │ beamai_process_snapshot  │
            │ (Process 执行快照)       │
            │                          │
            │ - save_from_state/3,4    │
            │ - 步骤状态查询           │
            │ - 暂停/恢复信息          │
            └─────────────────────────┘
                         │
┌────────────────────────┴────────────────────────┐
│              beamai_state_store                   │
│            (状态存储抽象层)                        │
└────────────────────────┬────────────────────────┘
                         │
         ┌───────────────┴───────────────┐
         │                               │
┌────────────────┐            ┌──────────────────┐
│ beamai_store_ets│            │ beamai_store_sqlite│
│  (ETS 后端)     │            │  (SQLite 后端)     │
└────────────────┘            └──────────────────┘
```

## 设计原则

1. **Behaviour 模式** - `beamai_snapshot` 定义通用快照行为，Process 快照实现特化逻辑
2. **纯函数式管理器** - 快照管理器为不可变 Map，操作返回新管理器
3. **分支 + 时间旅行** - 内建分支管理和双向时间旅行
4. **可插拔后端** - Store 协议支持多种实现（ETS、SQLite 等）

## 模块职责

### 快照层

| 模块 | 职责 | 说明 |
|------|------|------|
| `beamai_snapshot` | 通用快照引擎 | Behaviour 定义 + 分支/时间旅行/历史查询通用实现 |
| `beamai_process_snapshot` | Process 快照 | 保存/恢复 Process 执行状态（步骤、事件队列、暂停信息） |

### 存储层

| 模块 | 职责 | 说明 |
|------|------|------|
| `beamai_state_store` | 状态存储抽象 | 命名空间化的 KV 存储接口 |
| `beamai_store` | Store 协议 | 存储后端行为定义 |
| `beamai_store_ets` | ETS 后端 | 高速内存存储，支持容量限制和 LRU 淘汰 |
| `beamai_store_sqlite` | SQLite 后端 | 持久化存储 |
| `beamai_store_manager` | 存储管理器 | 多 Store 管理和归档 |

### 集成层

| 模块 | 职责 | 说明 |
|------|------|------|
| `beamai_process_memory_store` | Process 存储适配 | 实现 `beamai_process_store_behaviour`，桥接 Process 和 Snapshot |

## 使用示例

### 基本用法：Process 快照

```erlang
%% 1. 创建存储后端
{ok, _} = beamai_store_ets:start_link(my_store, #{}),
Store = {beamai_store_ets, my_store},

%% 2. 创建状态存储
StateStore = beamai_state_store:new(Store),

%% 3. 创建 Process 快照管理器
Mgr = beamai_process_snapshot:new(StateStore),
%% 或带选项：
Mgr = beamai_process_snapshot:new(StateStore, #{max_entries => 100, auto_prune => true}),

%% 4. 保存快照
StateMap = #{
    fsm_state => completed,
    thread_id => <<"thread-1">>,
    steps => #{<<"step1">> => #{result => <<"done">>}}
},
{ok, Snapshot, Mgr1} = beamai_process_snapshot:save_from_state(Mgr, <<"thread-1">>, StateMap),

%% 5. 加载快照
SnapshotId = beamai_process_snapshot:get_id(Snapshot),
{ok, Loaded} = beamai_process_snapshot:load(Mgr1, SnapshotId).
```

### 时间旅行

```erlang
%% 回退 1 步
{ok, OlderSnapshot, Mgr2} = beamai_process_snapshot:go_back(Mgr1, <<"thread-1">>, 1),

%% 前进 1 步
{ok, NewerSnapshot, Mgr3} = beamai_process_snapshot:go_forward(Mgr2, <<"thread-1">>, 1),

%% 撤销（回退 1 步）
{ok, UndoSnapshot, Mgr4} = beamai_process_snapshot:undo(Mgr1, <<"thread-1">>),

%% 重做（前进 1 步）
{ok, RedoSnapshot, Mgr5} = beamai_process_snapshot:redo(Mgr4, <<"thread-1">>),

%% 跳转到指定快照
{ok, TargetSnapshot, Mgr6} = beamai_process_snapshot:goto(Mgr1, <<"thread-1">>, TargetSnapshotId),

%% 查看历史
{ok, History} = beamai_process_snapshot:get_history(Mgr1, <<"thread-1">>).
```

### 分支管理

```erlang
%% 从某个快照创建分支
{ok, BranchMgr} = beamai_process_snapshot:fork_from(
    Mgr1, SnapshotId, <<"experiment">>, #{}
),

%% 切换分支
{ok, Mgr2} = beamai_process_snapshot:switch_branch(Mgr1, <<"experiment">>),

%% 列出所有分支
Branches = beamai_process_snapshot:list_branches(Mgr1).
```

### Process 专用查询

```erlang
%% 获取步骤状态
{ok, StepState} = beamai_process_snapshot:get_step_state(Snapshot, <<"step1">>),

%% 获取所有步骤状态
StepsState = beamai_process_snapshot:get_steps_state(Snapshot),

%% 检查是否暂停
IsPaused = beamai_process_snapshot:is_paused(Snapshot),

%% 获取暂停信息
PauseInfo = beamai_process_snapshot:get_pause_info(Snapshot).
```

## 快照数量限制

### 配置选项

快照管理器支持配置数量限制，自动清理旧快照：

```erlang
%% 创建时配置数量限制
Mgr = beamai_process_snapshot:new(StateStore, #{
    max_entries => 100,    %% 每个分支最多保留 100 个快照
    auto_prune => true     %% 自动清理旧的快照
}).
```

### 数量限制策略

| 配置 | 默认值 | 说明 |
|------|--------|------|
| `max_entries` | **100** | 每个分支最大快照数量 |
| `auto_prune` | `true` | 超过限制时自动清理旧快照 |
| ETS `max_items` | **1000** | ETS 存储容量限制 |
| ETS `max_namespaces` | **1000** | 命名空间数量限制 |

### ETS Store 限制说明

```erlang
%% 创建 ETS Store 时配置
{ok, _} = beamai_store_ets:start_link(my_store, #{
    max_items => 1000,        %% 全局限制：所有数据总共 1000 条
    max_namespaces => 1000    %% 全局限制：所有命名空间总共 1000 个
}).
```

## 文件结构

```
beamai_memory/
├── README.md
├── rebar.config
├── include/
│   ├── beamai_process_snapshot.hrl    # Process 快照记录定义
│   ├── beamai_state_store.hrl         # 状态存储记录定义
│   └── beamai_store.hrl              # Store 记录定义
├── src/
│   ├── beamai_memory_app.erl         # OTP 应用回调
│   ├── beamai_memory_sup.erl         # 监督树
│   ├── snapshot/                     # 快照模块
│   │   ├── beamai_snapshot.erl       # 通用快照引擎 (Behaviour)
│   │   └── beamai_process_snapshot.erl # Process 快照实现
│   ├── store/                        # 存储模块
│   │   ├── beamai_store.erl          # Store 协议
│   │   ├── beamai_store_ets.erl      # ETS 后端
│   │   ├── beamai_store_sqlite.erl   # SQLite 后端
│   │   ├── beamai_state_store.erl    # 状态存储抽象
│   │   └── beamai_store_manager.erl  # 存储管理器
│   ├── process/
│   │   └── beamai_process_memory_store.erl  # Process 存储适配
│   └── utils/
│       ├── beamai_memory_helpers.erl  # 辅助函数
│       └── beamai_memory_utils.erl    # 工具函数
└── test/
    └── ...
```

## 版本历史

- **5.0.0** - 快照架构重构：`beamai_snapshot` Behaviour + Process 专用快照
- **4.0.0** - 分层架构重构，双管理器设计
- **3.0.0** - 双 Store 架构，Checkpointer 使用 Store 接口
- **2.0.0** - 分离 Checkpointer 和 Store
- **1.0.0** - 初始版本
