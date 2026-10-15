# Performance Benchmark Harness 设计

## 目标

提供可重复、离线、低依赖的性能基准入口。基准结果只用于比较同一环境下的
代码变更，不作为绝对性能承诺。

## 位置

`beamai_benchmark` 放在 `beamai_agent` 应用内。该应用已依赖 `beamai_core`
和 `beamai_llm`，可以覆盖 filter、tool dispatch、memory、stream fanout、
HTTP pool 和 provider adapter 场景，不需要引入新的根依赖。

## API

```erlang
beamai_benchmark:available/0.
beamai_benchmark:run/0.                         %% 全部场景
beamai_benchmark:run(Scenario).                 %% 单场景，默认参数
beamai_benchmark:run(Scenario, Opts).           %% 单场景，自定义 iterations / warmup
```

`run/1,2` 返回结构化 result map，并输出一行式文本表格；`run/0` 输出多行
文本表格。场景失败时立即返回 `{error, Reason}`，不吞异常。

## 执行模型

1. 场景 setup 只执行一次，用于构建固定输入和受控本地资源。
2. `Opts.warmup`（默认 10）次调用不计时。
3. `Opts.iterations`（默认 1000）次调用逐次测量 wall-clock。
4. 单次耗时以微秒记录。
5. 输出 p50、p95、max 和 mean。
6. 采样不做聚合执行；避免长时间循环掩盖尾部延迟。
7. 场景 cleanup 只执行一次，关闭 benchmark 自建的 ETS store、本地 TCP
   listener、HTTP pool 等资源。

时间源使用 `erlang:monotonic_time(nanosecond)`，不受系统时间跳变影响。

## 场景约束

- 不访问真实 LLM / 真实网络。
- 不使用随机大小或随机分支输入；同一场景使用固定输入。
- 工具使用纯内存 handler 或 mock provider。
- HTTP pool 场景只测本地 acquire / release，不发起外部请求。
- 场景名称是稳定 atom，结果可通过场景名和历史数据对比。

## 核心场景

批次 S 使用以下固定输入；除 HTTP listener 的 TCP 建连发生在 warmup 外，
所有 measured iteration 都只访问本地内存或已建立的池化连接。

| 场景 | 固定输入 |
|---|---|
| `noop` | 空操作冒烟路径 |
| `filter_chain_0` / `filter_chain_1` / `filter_chain_4` / `filter_chain_16` | around_chat 洋葱链层数 |
| `tool_dispatch_1` / `tool_dispatch_8` / `tool_dispatch_64` / `tool_dispatch_256` | 单批纯内存 tool call 数量，串行调度 |
| `tool_manager_concurrent_16` / `tool_manager_sequential_16` | 同一组 16 个纯内存 tool call，`parallel=true`，只切换 manager 策略 |
| `memory_history_append_read` | ETS memory provider：append 一条、读取、清空，保持每轮状态一致 |
| `agent_event_fanout_4` | 同一统一事件广播到 4 个无副作用 consumer |
| `http_pool_acquire_release` | 本地 listener + 1 连接 HTTP pool，warmup 建立连接后反复 acquire / release |
| `provider_openai_encode_decode` | 固定 OpenAI request 编码后再用固定 response parser 解码 |

## 环境记录

每次 result 包含：

- OTP release。
- CPU topology。
- logical processors。
- Erlang schedulers：configured / online。
- UTC timestamp。
- iterations 和 warmup。

文本输出包含同样的关键字段，便于直接粘贴到 issue 或 baseline 文档。

## 基线策略

批次 S 已在 [`benchmarks/BASELINE.md`](../benchmarks/BASELINE.md) 记录首份
本地基线。后续影响热点路径的 PR 应运行受影响场景，并在 PR 描述中对比
p50 / p95 / max。
