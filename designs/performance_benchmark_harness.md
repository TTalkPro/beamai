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

1. `Opts.warmup`（默认 10）次调用不计时。
2. `Opts.iterations`（默认 1000）次调用逐次测量 wall-clock。
3. 单次耗时以微秒记录。
4. 输出 p50、p95、max 和 mean。
5. 采样不做聚合执行；避免长时间循环掩盖尾部延迟。

时间源使用 `erlang:monotonic_time(nanosecond)`，不受系统时间跳变影响。

## 场景约束

- 不访问真实 LLM / 真实网络。
- 不使用随机大小或随机分支输入；同一场景使用固定输入。
- 工具使用纯内存 handler 或 mock provider。
- HTTP pool 场景只测本地 acquire / release，不发起外部请求。
- 场景名称是稳定 atom，结果可通过场景名和历史数据对比。

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

批次 S 落地核心场景后，将当时机器输出记录到 `benchmarks/BASELINE.md`。
后续影响热点路径的 PR 应运行受影响场景，并在 PR 描述中对比 p50 / p95 / max。
