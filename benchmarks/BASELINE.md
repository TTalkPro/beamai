# BeamAI Performance Baseline

这是批次 S 落地后的首份本地基线，用于同一台机器上的前后对比；不同机器或
container CPU / scheduler 限制下的数值不可直接比较。

- 采集时间：`2026-10-15T03:52:52Z`
- OTP：`28`
- Logical processors：`8`
- CPU topology：1 processor / 8 logical cores
- Erlang schedulers：configured `128` / online `8`
- Parameters：`iterations=1000`, `warmup=10`

采集命令：

```bash
erl -pa _build/default/lib/*/ebin -noshell \
  -eval 'beamai_benchmark:run(), halt(0).'
```

| Scenario | Iterations | p50 us | p95 us | Max us | Mean us |
|---|---:|---:|---:|---:|---:|
| agent_event_fanout_4 | 1000 | 4.00 | 4.00 | 424.00 | 5.30 |
| filter_chain_0 | 1000 | 4.00 | 4.00 | 1162.00 | 6.56 |
| filter_chain_1 | 1000 | 4.00 | 4.00 | 33.00 | 4.12 |
| filter_chain_16 | 1000 | 5.00 | 9.00 | 60.00 | 5.48 |
| filter_chain_4 | 1000 | 4.00 | 5.00 | 69.00 | 4.40 |
| http_pool_acquire_release | 1000 | 30.00 | 33.00 | 171.00 | 30.80 |
| memory_history_append_read | 1000 | 26.00 | 31.00 | 1329.00 | 29.69 |
| noop | 1000 | 4.00 | 4.00 | 65.00 | 4.24 |
| provider_openai_encode_decode | 1000 | 6.00 | 10.00 | 131.00 | 6.64 |
| tool_dispatch_1 | 1000 | 47.00 | 54.00 | 152.00 | 49.34 |
| tool_dispatch_256 | 1000 | 7824.00 | 8885.00 | 16705.00 | 8023.29 |
| tool_dispatch_64 | 1000 | 1910.00 | 2121.00 | 4141.00 | 1938.73 |
| tool_dispatch_8 | 1000 | 247.00 | 320.00 | 806.00 | 259.15 |
| tool_manager_concurrent_16 | 1000 | 392.00 | 511.00 | 817.00 | 406.31 |
| tool_manager_sequential_16 | 1000 | 480.00 | 582.00 | 814.00 | 496.95 |

后续影响这些热点路径的行为变更，应在 PR 描述中给出受影响场景的 p50 /
p95 / max 对比；仅更新文档、测试或非热点实现时可说明无需重新采集。
