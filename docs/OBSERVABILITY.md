# 可观测性

BeamAI 内置零依赖 telemetry dispatch。关键路径发布纯数据事件，业务代码不依赖
具体监控实现；用户可注入任意 handler（日志、metrics、tracing、测试收集器）。

## 事件结构

```erlang
#{
    name => [beamai, llm, request, started],
    level => info,
    timestamp => nanoseconds,
    data => #{},       %% 领域数据，仅包含安全的摘要字段
    metadata => #{}    %% run_id / request_id / conversation_id / batch_id 等
}
```

## 使用 handler

```erlang
Handler = fun(#{name := Name, data := Data, metadata := Meta}) ->
    collect(Name, Data, Meta)
end,
ok = beamai_telemetry:add_handler(app_trace, Handler).
```

同一 `id` 重复注入会覆盖旧 handler；`beamai_telemetry:remove_handler(app_trace)`
可移除。handler 抛出的异常会被 telemetry 层捕获并记录 warning，不会中断 LLM、
Tool 或 Agent 调用。

## 级别与默认日志

默认开启 `info`、`notice`、`warning`、`error`；`debug` 必须显式开启，适合高频
token、connection 或批处理事件。

```erlang
ok = beamai_telemetry:set_config(#{
    enabled => true,
    enabled_levels => [info, warning, error],
    handlers => [#{id => file_logger,
                   handler => fun beamai_telemetry:default_logger/1}]
}).
```

内置 `default_logger/1` 只记录事件名、级别和标准链路 ID，不格式化 `data`。
因此默认日志不会输出 prompt、tool result 全文或 API key。

## 命名约定

计划中的稳定事件命名如下；各业务模块接入时不得为同一语义另建名称：

| 路径 | 事件 |
| --- | --- |
| LLM | `beamai.llm.request.started/finished`, `beamai.llm.retry`, `beamai.llm.stream.started/finished` |
| Tool | `beamai.tool.queued/started/finished/timeout/crash` |
| Agent | `beamai.agent.run.started/finished`, `beamai.agent.turn.started/finished`, `beamai.agent.interrupted/resumed/state_changed` |
| HTTP Pool | `beamai.http_pool.acquired/reused/failed/wait_timeout/idle_reaped` |

所有事件都应在 `metadata` 中尽量携带 `request_id`、`run_id`、`conversation_id`
或 `batch_id`，用于还原一次请求或 Agent run 的完整时序。
